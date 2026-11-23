// Open a problem page in Chrome with the Competitive Companion extension
// and click the extension's toolbar button, so that it parses the page and
// sends the problem to Emacs.
//
// Usage: node driver.mjs --port PORT --url URL --wait-for SELECTOR
//
// The extension sends problems to PORT on localhost.  SELECTOR is a CSS
// selector present once the page has loaded; when it does not show up
// (Cloudflare challenge, site down), the page is reported as unavailable.
//
// Protocol with the caller, one line each on stdout:
//   TRIGGERED       the button was clicked
//   SKIP <reason>   the page could not be used; exit status 77
// Errors go to stderr, with exit status 1.  After TRIGGERED, the browser
// stays open (the extension may still be fetching pages, for contests)
// until stdin is closed.  Set CC_INTEGRATION_HEADFUL=1 to see the browser.

import path from 'node:path';
import { parseArgs } from 'node:util';
import puppeteer from 'puppeteer';

const SKIP_STATUS = 77;
const PAGE_TIMEOUT = 30_000;
const MAX_LIFETIME = 180_000;

const { values: args } = parseArgs({
  options: {
    port: { type: 'string' },
    url: { type: 'string' },
    'wait-for': { type: 'string' },
  },
});
for (const name of ['port', 'url', 'wait-for']) {
  if (!args[name]) {
    console.error(`driver: missing --${name}`);
    process.exit(2);
  }
}

const extensionDir = path.join(import.meta.dirname, '.cache', 'extension');
const headful = process.env.CC_INTEGRATION_HEADFUL === '1';

const stdinClosed = new Promise(resolve => {
  process.stdin.on('end', resolve);
  process.stdin.on('close', resolve);
  process.stdin.resume();
});

const browser = await puppeteer.launch({
  headless: !headful,
  // Extensions.triggerAction is only available over a pipe.
  pipe: true,
  enableExtensions: true,
  args: ['--enable-unsafe-extension-debugging'],
});

let status = 0;
try {
  const id = await browser.installExtension(extensionDir);
  const extension = (await browser.extensions()).get(id);

  // Send problems to the caller's port only, and give Emacs time to answer:
  // the default is 500 ms.
  const workerTarget = await browser.waitForTarget(
    target => target.type() === 'service_worker' && target.url().startsWith(`chrome-extension://${id}/`),
  );
  const worker = await workerTarget.worker();
  await worker.evaluate(
    port => chrome.storage.local.set({ customPorts: [port], requestTimeout: 5000 }),
    Number(args.port),
  );

  const page = await browser.newPage();
  page.on('console', message => console.error(`page: ${message.text()}`));

  let loaded = true;
  try {
    await page.goto(args.url, { waitUntil: 'domcontentloaded', timeout: PAGE_TIMEOUT });
    await page.waitForSelector(args['wait-for'], { timeout: PAGE_TIMEOUT });
  } catch (err) {
    loaded = false;
    const title = await page.title().catch(() => '');
    console.log(`SKIP ${args.url} did not load (title: ${JSON.stringify(title)}): ${err.message}`);
    status = SKIP_STATUS;
  }

  if (loaded) {
    await extension.triggerAction(page);
    console.log('TRIGGERED');
    await Promise.race([stdinClosed, new Promise(resolve => setTimeout(resolve, MAX_LIFETIME))]);
  }
} catch (err) {
  console.error(`driver: ${err.stack ?? err}`);
  status = 1;
} finally {
  await browser.close().catch(() => {});
}
process.exit(status);

// Keep the browser Puppeteer downloads next to the tests, not in ~/.cache.
const path = require('node:path');

module.exports = {
  cacheDirectory: path.join(__dirname, '.cache', 'puppeteer'),
};

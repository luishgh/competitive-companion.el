;; Dependencies of the browser integration tests (`make integration'):
;;
;;   guix shell -m integration-manifest.scm -- make integration
;;
;; Puppeteer and the Chrome for Testing build it drives are not Guix
;; packages: `make integration' installs them with npm into
;; test/integration/.

(specifications->manifest
 '(;; Test runner
   "make"
   "emacs"
   "emacs-magit"
   ;; Browser driver (test/integration/driver.mjs), npm included
   "node"
   ;; Compiling the solutions in test/integration/solutions/
   "gcc-toolchain"
   ;; test/integration/fetch-extension.sh
   "bash"
   "coreutils"
   "grep"
   "curl"
   "nss-certs"
   "unzip"))

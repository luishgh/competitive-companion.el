EMACS ?= emacs
NPM ?= npm

.PHONY: check test integration

# Guix's emacs-build-system runs `make check' in its check phase.
check:
	$(EMACS) -Q --batch -L . -L test \
	  -l competitive-companion-tests \
	  -f ert-run-tests-batch-and-exit

test: check

# Tests with a real browser and live judges: they need the network, Node.js
# and g++, so `check' does not run them.  `ulimit -c 0': the tests crash
# programs on purpose, keep their core dumps out.
integration:
	cd test/integration && $(NPM) ci --no-fund --no-audit && ./fetch-extension.sh
	ulimit -c 0; $(EMACS) -Q --batch -L . -L test \
	  -l competitive-companion-integration-tests \
	  -f ert-run-tests-batch-and-exit

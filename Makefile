EMACS ?= emacs

.PHONY: check test

# Guix's emacs-build-system runs `make check' in its check phase.
check:
	$(EMACS) -Q --batch -L . -L test \
	  -l competitive-companion-tests \
	  -f ert-run-tests-batch-and-exit

test: check

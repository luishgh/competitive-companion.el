(use-modules (guix packages)
             (guix download)
             (guix build-system emacs)
             (guix gexp)
             (gnu packages emacs-xyz)
             (guix licenses))

(package
  (name "emacs-competitive-companion")
  (version "0.0.1")
  (source (local-file "." "checkout"
                      #:recursive? #t))
  (build-system emacs-build-system)
  ;; NOTE: only magit-section would be enough, but currently on Guix it is not packaged separately
  (propagated-inputs (list emacs-magit))
  (synopsis "Competitive Companion Integration")
  (description
   "This plugin provides an Emacs interface to interact with the Competitive Companion browser extension.
Inspired by competitest.nvim and cphelper.nvim, it allows users to fetch competitive programming problem
data and manage test cases directly within Emacs.")
  (home-page "https://www.gnu.org/software/hello/")
  (license gpl3+))

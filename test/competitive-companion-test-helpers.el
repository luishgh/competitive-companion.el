;;; competitive-companion-test-helpers.el --- Fixtures for the test suite -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Fixtures shared by the ERT test suite.  Test programs are tiny shell
;; scripts, so the suite needs no compiler: any executable regular file
;; works as the COMMAND of `competitive-companion-run-tests'.

;;; Code:

(require 'cl-lib)
(require 'competitive-companion)

(defconst cc-test-programs
  '((cat    . "exec cat")
    (hang   . "exec sleep 100")
    (slow   . "sleep 0.3; exec cat")
    (segv   . "kill -SEGV $$")
    (exit3  . "exit 3")
    (stderr . "echo out; echo err >&2")
    (noread . "echo x")
    (bigout . "yes | head -c 2000000"))
  "Named bodies for test programs, see `cc-test-make-program'.")

(defun cc-test-make-program (directory body &optional name)
  "Write an executable shell script running BODY into DIRECTORY.
BODY is a string, or a symbol naming an entry of `cc-test-programs'.
NAME is the script's file name, defaulting to \"prog\".
Return the script's absolute file name."
  (let ((file (expand-file-name (or name "prog") directory))
        (body (if (symbolp body) (alist-get body cc-test-programs) body)))
    (unless body
      (error "Unknown test program"))
    ;; Guix build containers have no /bin/sh, so look it up.
    (with-temp-file file
      (insert "#!" (executable-find "sh") "\n" body "\n"))
    (set-file-modes file #o755)
    file))

(defmacro cc-test-capturing-messages (var &rest body)
  "Run BODY, collecting every string passed to `message' into VAR.
VAR is bound around BODY, oldest message first.  Messages are still
printed, which keeps batch logs useful."
  (declare (indent 1))
  `(let ((,var nil))
     (cl-letf* ((orig (symbol-function 'message))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (when format-string
                     (setq ,var (append ,var (list (apply #'format-message format-string args)))))
                   (apply orig format-string args))))
       ,@body)))

(cl-defun cc-test-run (&key tests program (separate-stderr nil))
  "Run PROGRAM against TESTS with `competitive-companion-run-tests'.

TESTS is a list of (INPUT . EXPECTED-OUTPUT) strings, written as
inputN.txt/outputN.txt in a fresh hidden task directory.  PROGRAM is
passed to `cc-test-make-program'.  SEPARATE-STDERR is bound as
`competitive-companion-separate-stderr'.

Everything is set up in a temporary directory and torn down afterwards.
Return a plist with :result (the command's return value), :output (the
output buffer's text) and :messages (strings passed to `message')."
  (let* ((contest-dir (file-name-as-directory (make-temp-file "cc-test-" t)))
         (task-dir (expand-file-name ".a" contest-dir))
         (command (cc-test-make-program contest-dir program))
         (competitive-companion-separate-stderr separate-stderr)
         output-buffer result output messages)
    (unwind-protect
        (progn
          (make-directory task-dir)
          (cl-loop for index from 1
                   for (input . expected) in tests do
                   (write-region input nil (expand-file-name (format "input%d.txt" index) task-dir))
                   (write-region expected nil (expand-file-name (format "output%d.txt" index) task-dir)))
          (with-temp-buffer
            (setq-local competitive-companion--current-task task-dir)
            (setq-local competitive-companion--current-task-name "A. Test")
            (setq-local competitive-companion--original-test-count (length tests))
            (cc-test-capturing-messages captured
              (setq result (competitive-companion-run-tests command nil))
              (setq messages captured))
            (setq output-buffer competitive-companion--output-buffer)
            (setq output (with-current-buffer output-buffer
                           (buffer-substring-no-properties (point-min) (point-max))))))
      (when (buffer-live-p output-buffer)
        (kill-buffer output-buffer))
      (delete-directory contest-dir t))
    (list :result result :output output :messages messages)))

(provide 'competitive-companion-test-helpers)

;; Local Variables:
;; read-symbol-shorthands: (("cc-" . "competitive-companion-"))
;; End:
;;; competitive-companion-test-helpers.el ends here

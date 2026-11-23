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

(cl-defun cc-test-run (&key tests program (separate-stderr nil) time-limit after quit-after)
  "Run PROGRAM against TESTS with `competitive-companion-run-tests'.

TESTS is a list of (INPUT . EXPECTED-OUTPUT) strings, written as
inputN.txt/outputN.txt in a fresh hidden task directory.  PROGRAM is
passed to `cc-test-make-program'.  SEPARATE-STDERR is bound as
`competitive-companion-separate-stderr'.  TIME-LIMIT is the judge's
time limit in milliseconds, as stored by `competitive-companion--process-data'.
AFTER, if non-nil, is called with no arguments in the output buffer
once the run is over.  When QUIT-AFTER is non-nil, simulate
\\[keyboard-quit] after that many seconds.

Everything is set up in a temporary directory and torn down afterwards.
Return a plist with :result (the command's return value), :output (the
output buffer's text), :messages (strings passed to `message') and
:after (what AFTER returned)."
  (let* ((contest-dir (file-name-as-directory (make-temp-file "cc-test-" t)))
         (task-dir (expand-file-name ".a" contest-dir))
         (command (cc-test-make-program contest-dir program))
         (competitive-companion-separate-stderr separate-stderr)
         (timer (when quit-after
                  (run-at-time quit-after nil (lambda () (setq quit-flag t)))))
         output-buffer result output messages after-value)
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
            (setq-local competitive-companion--current-time-limit time-limit)
            (cc-test-capturing-messages captured
              (setq result (competitive-companion-run-tests command nil))
              (setq messages captured))
            (setq output-buffer competitive-companion--output-buffer)
            (setq output (with-current-buffer output-buffer
                           (buffer-substring-no-properties (point-min) (point-max))))
            (when after
              (setq after-value (with-current-buffer output-buffer
                                  (funcall after))))))
      (when timer (cancel-timer timer))
      (when (buffer-live-p output-buffer)
        (kill-buffer output-buffer))
      (delete-directory contest-dir t))
    (list :result result :output output :messages messages :after after-value)))

(defun cc-test-message-matching (regexp run)
  "Return the first message of RUN matching REGEXP.
RUN is a plist with :messages, as returned by `cc-test-run'."
  (seq-find (lambda (msg) (string-match-p regexp msg))
            (plist-get run :messages)))

(defmacro cc-test-with-temp-dir (var &rest body)
  "Bind VAR to a fresh temporary directory around BODY, then delete it."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "cc-test-" t))))
     (unwind-protect (progn ,@body)
       (delete-directory ,var t))))

(cl-defun cc-test-execute (program input &key hard-limit warn-after merge-stderr quit-after)
  "Run PROGRAM on INPUT with `competitive-companion--execute'.
PROGRAM is passed to `cc-test-make-program'; INPUT is a string.
HARD-LIMIT, WARN-AFTER and MERGE-STDERR are passed along.  When
QUIT-AFTER is non-nil, simulate \\[keyboard-quit] after that many
seconds.  Return the plist from `competitive-companion--execute' with
:messages (strings passed to `message') added."
  (cc-test-with-temp-dir dir
    (let ((command (cc-test-make-program dir program))
          (input-file (expand-file-name "input1.txt" dir))
          (timer (when quit-after
                   (run-at-time quit-after nil (lambda () (setq quit-flag t))))))
      (write-region input nil input-file)
      (unwind-protect
          (cc-test-capturing-messages messages
            (append (competitive-companion--execute
                     command input-file
                     :index 1 :hard-limit hard-limit :warn-after warn-after
                     :merge-stderr merge-stderr)
                    (list :messages messages)))
        (when timer (cancel-timer timer))))))

(defun cc-test-leftovers ()
  "Return processes and buffers left behind by test runs."
  (append
   (seq-filter (lambda (process)
                 (string-match-p "\\`competitive-companion-\\(run\\|stderr\\)" (process-name process)))
               (process-list))
   (seq-filter (lambda (buffer)
                 (string-match-p "\\` \\*competitive-companion-\\(stdout\\|stderr\\)" (buffer-name buffer)))
               (buffer-list))))

(defun cc-test-counting-program (counter-file body)
  "Return a program body that appends a line to COUNTER-FILE, then runs BODY."
  (format "echo run >> %s\n%s" (shell-quote-argument counter-file) body))

(defun cc-test-dispatching-program (log-file hang-on)
  "Return a program body echoing its input, logged to LOG-FILE.
The program hangs instead when its input is the string HANG-ON."
  (format "input=$(cat)\necho \"$input\" >> %s\n[ \"$input\" = %s ] && exec sleep 100\necho \"$input\""
          (shell-quote-argument log-file) (shell-quote-argument hang-on)))

(defun cc-test-file-string (file)
  "Return the contents of FILE, or nil if it does not exist."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun cc-test-count-lines (file)
  "Return the number of lines in FILE, or 0 if it does not exist."
  (if (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (count-lines (point-min) (point-max)))
    0))

(provide 'competitive-companion-test-helpers)

;; Local Variables:
;; read-symbol-shorthands: (("cc-" . "competitive-companion-"))
;; End:
;;; competitive-companion-test-helpers.el ends here

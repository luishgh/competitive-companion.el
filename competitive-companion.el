;;; competitive-companion.el --- Competitive Companion Integration -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025, 2026 Luis Higino

;; Author: Luis Higino <luis.higino@dcc.ufmg.br>
;; URL: https://github.com/luishgh/competitive-companion.el
;; Created: 25 Dec 2024
;; Package-Version: 0.0.2
;; Package-Requires: ((emacs "30.1") (magit-section "4.0.0"))
;; Keywords: convenience

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This plugin provides an Emacs interface to interact with the Competitive Companion browser extension.
;; Inspired by competitest.nvim and cphelper.nvim, it allows users to fetch competitive programming problem
;; data and manage test cases directly within Emacs.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'server)
(require 'magit-section)

;; Chill compiler...
(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))

;;;; Variables

(defvar-local competitive-companion--current-task nil
  "Holds the current task's (i.e. problem), associated with its data dir.")

(defvar-local competitive-companion--current-task-name nil
  "Holds the current task's (i.e. problem) name.")

(defvar-local competitive-companion--test-count nil
  "Holds the number of test cases for output buffer's task.")

(defvar-local competitive-companion--original-test-count nil
  "Holds the original number of test cases for output buffer's task.")

(defvar-local competitive-companion--current-time-limit nil
  "Holds the current task's time limit on the judge, in milliseconds.
It is nil when the judge's time limit is unknown.")

(defvar-local competitive-companion--output-buffer nil
  "Holds the output buffer corresponding to this implementation file.")

(defvar-local competitive-companion--current-command nil
  "Holds last called command for output buffer.")

(defvar competitive-companion--contest-directory nil
  "Holds the current contest's directory.")

(defvar competitive-companion-last-problem-data nil
  "Holds the data alist for the last problem received from the browser extension.

This is an alist representing the JSON replied by the browser extension")

(defvar competitive-companion-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c .") #'competitive-companion-run-tests)
    map)
  "Keymap for `competitive-companion-mode'.")

(defvar competitive-companion-test-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "-") 'competitive-companion--remove-test-section-files)
    map)
  "Keymap for output buffer's file sections.")

;; comment: only RET works currently
(defvar competitive-companion-file-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'competitive-companion--open-section-file)
    (define-key map [mouse-2] 'competitive-companion--open-section-file)
    map)
  "Keymap for output buffer's file sections.")

(defvar competitive-companion-output-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "+") 'competitive-companion--add-test-case)
    map))

(defvar competitive-companion--prompt-task-filename nil
  "Holds value of `competitive-companion-prompt-task-filename' when mode turns on.")

(defvar competitive-companion--modeline-item nil
  "Competitive Companion item for the global-mode-line.")

;;;; Customization

(defgroup competitive-companion nil
  "Competitive Companion integration for Emacs."
  :group 'tools
  :prefix "competitive-companion-")

(defcustom competitive-companion-file-generation-hook
  '(competitive-companion-insert-default-header
    competitive-companion-insert-template-file)
  "Hook run when a new task file is created.
The functions in this hook are run in a temporary buffer visiting
the newly created file.  They can use
`competitive-companion-last-problem-data' to access problem
metadata.  See Info node `(competitive-companion) Language/file options'"
  :type 'hook
  :group 'competitive-companion)

;;;;; Options

(defcustom competitive-companion-task-major-mode 'c++-mode
  "Major mode for task's implementation file.
This value should be one of the mode names present in
`competitive-companion-languages'.
Make sure the mode defines comment syntax!"
  :type 'symbol
  :group 'competitive-companion)

(defcustom competitive-companion-task-template-file "~/Documents/Maratona/templ.cpp"
  "Path to a template file to use for task files.

If file is non-empty, its contents will be inserted verbatim below
the task file header.

If the user prefers using proper templating packages for this,
this variable should be set to nil."
  :type 'file
  :group 'competitive-companion)

(defcustom competitive-companion-server-port 10050
  "Port on which the Competitive Companion server listens."
  :type 'integer
  :group 'competitive-companion)

(defcustom competitive-companion-languages
  '((c-mode . ".c")
    (c++-mode . ".cpp")
    (python-mode . ".py")
    (java-mode . ".java")
    (ruby-mode . ".rb")
    (javascript-mode . ".js"))
  "List of available languages to choose when fetching a new task.

Each entry should be of the form `(MAJOR-MODE . EXTENSION)', where
`MAJOR-MODE' is the name of the major mode used for that programming
language and `EXTENSION' is a string indicating the file extension,
always preceded by a dot."
  :type '(repeat (cons symbol string))
  :group 'competitive-companion)

(defcustom competitive-companion-collapse-test-cases t
  "Controls whether test cases in the output buffer should be initially collapsed.

If it is set to nil, all test cases are initially
expanded."
  :type 'symbol
  :group 'competitive-companion)

(defcustom competitive-companion-prompt-task-filename nil
  "If non-nil, prompt for a task filename instead of automatically generating one.
This option must be set to the correct value BEFORE turning on the mode,
because it is only read when turning on.
This is to enable setting it as a directory local variable consistently."
  :type 'boolean
  :group 'competitive-companion)

(defcustom competitive-companion-separate-stderr nil
  "If non-nil, stdout and stderr are shown separately on output buffers."
  :type 'boolean
  :group 'competitive-companion)

(defcustom competitive-companion-insert-header t
  "If non-nil, insert the package's header on created files."
  :type 'boolean
  :group 'competitive-companion)

(defcustom competitive-companion-guess-program
  (lambda (filename)
    (let ((guess (file-name-sans-extension filename)))
      (when (file-regular-p guess)
        guess)))
  "Function that guesses the program's name based on FILENAME.
NOTE: the function should always check if the guessed path exists
before returning.
Set this to a function that always returns nil if you don't want any guessing."
  :type 'function
  :group 'competitive-companion)

(defcustom competitive-companion-time-limit '(factor . 3)
  "How long a program may run on a single test case before being killed.

The killed test case gets a TLE verdict.  The value can be:
- nil: never kill the program (abort it with \[keyboard-quit]);
- a number: a fixed limit in seconds;
- (factor . F): F times the judge's time limit, but never less than
  `competitive-companion-minimum-time-limit'.

Local builds (debug flags, sanitizers) are often much slower than on the
judge, so this limit is meant to catch infinite loops, not to tell how
close a solution is to the judge's time limit."
  :type '(choice (const :tag "No limit" nil)
                 (number :tag "Fixed limit (seconds)")
                 (cons :tag "Multiple of the judge's limit"
                       (const factor) (number :tag "Factor")))
  :group 'competitive-companion)

(defcustom competitive-companion-minimum-time-limit 5
  "Least time limit in seconds when `competitive-companion-time-limit' is a factor.
It is also the time limit when the judge's time limit is unknown."
  :type 'number
  :group 'competitive-companion)

(defcustom competitive-companion-slow-test-delay nil
  "Seconds before reporting that a test case is still running.
If nil, use the judge's time limit, or 2 seconds when it is unknown."
  :type '(choice (const :tag "Judge's time limit" nil)
                 (number :tag "Seconds"))
  :group 'competitive-companion)

(defun competitive-companion-default-task-filename-generator (problem-data)
  "Default task filename generator.
`PROBLEM-DATA' should be an alist representing a return from
the browser extension about a task.
The default behaviour extracts the first alphanumerical characters from
problem's name and uses them, lowercased, as filename.
This behaviour works on Codeforces and AtCoder,
it is recommended to try it for yourself
and customize this (possibly on a dir local basis) for other judges."
  (let ((name (alist-get 'name problem-data)))
    (when (string-match "\\`[[:alnum:]]+" name)
      (downcase (match-string 0 name)))))

(defcustom competitive-companion-task-filename-generator
  #'competitive-companion-default-task-filename-generator
  "Function that generates the task filename based on problem's data.
The problem's data is an alist representing the JSON replied by
the browser extension.

The default behaviour is defined at
`competitive-companion-default-task-filename-generator'.
This behaviour works on Codeforces and AtCoder,
it is recommended to try it for yourself
and customize this (possibly on a dir local basis) for other judges.

Unfornunately, an everywhere works, sensible solution seems impossible."
  :type 'function
  :group 'competitive-companion)

(defcustom competitive-companion-modeline-support t
  "Enable modeline icon to indicate mode is on.

This uses `global-mode-string', which means it should work on things
like `doom-modeline' and similars.
See `competitive-companion-modeline-face'."
  :type 'boolean
  :group 'competitive-companion)

;;;; Commands

;;;###autoload
(define-minor-mode competitive-companion-mode
  "Minor mode for Competitive Companion integration.
Turning this on sets the variable
`competitive-companion--contest-directory' to
`default-directory', so it should be done inside
the contest's dedicated folder."
  :global t
  :lighter nil
  :keymap competitive-companion-mode-map
  (if competitive-companion-mode
      (progn
        (setq competitive-companion--contest-directory default-directory)
        (setq competitive-companion--prompt-task-filename competitive-companion-prompt-task-filename)
        (competitive-companion--start-server)
        (when competitive-companion-modeline-support
          (setq competitive-companion--modeline-item
                '(:eval (when competitive-companion-mode
                          (if (require 'nerd-icons nil 'noerror)
                              (concat " " (nerd-icons-mdicon "nf-md-balloon" :face 'competitive-companion-modeline-face) " ")
                            (concat " " (propertize "CC" 'face 'competitive-companion-modeline-face) " ")))))
          (add-to-list 'global-mode-string competitive-companion--modeline-item))

        (message "Competitive Companion mode activated."))
    (setq global-mode-string
          (seq-remove (lambda (item) (equal item competitive-companion--modeline-item))
                      global-mode-string))
    (setq competitive-companion--contest-directory nil)
    (setq competitive-companion--current-task nil)
    (setq competitive-companion--prompt-task-filename nil)
    (delete-process "*competitive-companion-server*")
    (competitive-companion--kill-buffers)
    (message "Competitive Companion mode deactivated.")))

(defun competitive-companion-setup-evil ()
  "Set up default Evil bindings for `competitive-companion'.
See Info node `(competitive-companion) Evil integration'"
  (interactive)
  (when (featurep 'evil)
    (evil-set-initial-state 'competitive-companion-output-mode 'motion)
    (evil-define-key* 'motion competitive-companion-output-mode-map
             "+" #'competitive-companion--add-test-case)))

(define-derived-mode competitive-companion-output-mode
  magit-section-mode
  "Competitive Companion Output"
  :interactive nil
  (setq-local revert-buffer-function #'competitive-companion-output-revert))

;; TODO: rework those classes, they don't work exactly as
;; intended. Only the keymap field is used, the file is recovered by
;; the value field

(defclass competitive-companion-root-section (magit-section) ())
(defclass competitive-companion-test-section (magit-section)
  ((test-index :initarg :test-index :type string :documentation "Index of the test case.")
   (keymap :initform 'competitive-companion-test-section-map)))

(defclass competitive-companion-input-section (magit-section)
  ((file :initarg :file :type string :documentation "Path to the input file.")
   (keymap :initform 'competitive-companion-file-section-map)))

(defclass competitive-companion-expected-section (magit-section)
  ((keymap :initform 'competitive-companion-file-section-map)
   (file :initarg :file :type string :documentation "Path to the expected output file.")))

(defclass competitive-companion-actual-section (magit-section) ())

(defun competitive-companion-output-revert (_ignore-auto _noconfirm)
  "Revert current Competitive Companion output buffer, using last used command."
  (interactive)
  (unless competitive-companion--current-command
    (error "`competitive-companion--current-command' is not set! Cannot revert output buffer!"))
  (competitive-companion-run-tests competitive-companion--current-command (current-buffer)))

;; TODO: would be cool for this to close the current window
;; if current buffer is a competitive-companion-output buffer
(defun competitive-companion-toggle-output-buffer ()
  "Toggle the output buffer for the current Competitive Companion task.

If the output buffer does not exist or is dead, tries
to run `competitive-companion-run-tests'.

If the output buffer is visible, select its window.
If it is alive but not visible, display it and select its window."
  (interactive)

  (unless (buffer-file-name)
    (user-error "Current buffer is not visiting a file"))

  ;; Guard: minor mode
  (unless (bound-and-true-p competitive-companion-mode)
    (user-error "Competitive Companion mode is not enabled"))

  ;; Guard: current task
  (unless competitive-companion--current-task
    (user-error "No current Competitive Companion task"))

  ;; Ensure output buffer exists
  (if (and competitive-companion--output-buffer
           (buffer-live-p competitive-companion--output-buffer))
      (let ((buf competitive-companion--output-buffer))
        (cond
         ;; Visible: select window
         ((get-buffer-window buf 'visible)
          (select-window (get-buffer-window buf 'visible)))

         ;; Alive but not visible: display and select
         ((buffer-live-p buf)
          (pop-to-buffer buf))

         ;; Defensive fallback
         (t
          (error "Output buffer is not available"))))
    (let* ((file (buffer-file-name))
           (program
            (progn
              (unless file
                (error "Current buffer is not visiting a file"))
              (funcall competitive-companion-guess-program file)))

           (keys (where-is-internal #'competitive-companion-run-tests nil t))
           (key-desc (when keys (key-description keys))))
      (unless (and program
                   (file-regular-p program))
        (user-error "This task has not been run before! Try `%s'"
                    (if key-desc
                        key-desc
                      "`competitive-companion-run-tests'")))

      (setq-local competitive-companion--output-buffer
                  (generate-new-buffer "*competitive-companion-output*"))
      (competitive-companion-run-tests program competitive-companion--output-buffer))))

(defun competitive-companion-run-tests (&optional command output-buffer)
  "Run all test cases for current task using `COMMAND'.
Reports success if all tests pass,
show failed test cases outputs on `OUTPUT-BUFFER' otherwise.
Default value for `COMMAND' is the last used command for the current task,
if that exists.
Otherwise, we take the suggestion from `competitive-companion-guess-program'.
Non internal uses should set `OUTPUT-BUFFER' to nil.

The return value indicates if all test cases were successful."
  (interactive (progn
                 (unless (buffer-file-name)
                   (user-error "Current buffer is not visiting a file"))
                 (unless competitive-companion-mode
                   (user-error "Competitive Companion mode is not turned on"))
                 (unless competitive-companion--current-task
                   (user-error "The current task has not been fetched"))
                 (list (expand-file-name
                        (read-file-name
                         "Command to run: "
                         competitive-companion--contest-directory nil t
                         (if competitive-companion--current-command
                             (file-name-nondirectory competitive-companion--current-command)
                           (when-let* ((guess (funcall competitive-companion-guess-program (buffer-file-name))))
                             (file-name-nondirectory guess)))))
                       nil)))

  (unless output-buffer
    (setq output-buffer
          (if (buffer-live-p competitive-companion--output-buffer)
              competitive-companion--output-buffer
            (setq-local competitive-companion--output-buffer
                        (generate-new-buffer "*competitive-companion-output*"))
            competitive-companion--output-buffer)))

  (unless command
    (setq command
          (expand-file-name
           (if competitive-companion--current-command
               (file-name-nondirectory competitive-companion--current-command)
             (when-let* ((guess (funcall competitive-companion-guess-program (buffer-file-name))))
               (file-name-nondirectory guess))))))

  (unless command
    (user-error "Could not guess value of `COMMAND'"))

  (unless (file-regular-p command)
    (user-error "`%s' is not a regular file!" command))

  (let* ((task-directory competitive-companion--current-task)
         (default-directory task-directory)
         (task-hashname (file-name-nondirectory task-directory))
         (task (if competitive-companion--current-task-name
                   competitive-companion--current-task-name
                 (substring task-hashname 0 1)))
         (original-test-count competitive-companion--original-test-count)
         (time-limit competitive-companion--current-time-limit)
         (test-results (competitive-companion--run-test-cases command task-directory time-limit))
         (failed-test-results
          (seq-sort
           (lambda (result-a result-b)
             (< (competitive-companion-test-result-index result-a) (competitive-companion-test-result-index result-b)))
           (seq-filter
            (lambda (test-result)
              (not (equal (competitive-companion-test-result-verdict test-result)
                          'ac)))
            test-results)))
         (accepted-test-results
          (seq-sort
           (lambda (result-a result-b)
             (< (competitive-companion-test-result-index result-a) (competitive-companion-test-result-index result-b)))
           (seq-filter
            (lambda (test-result)
              (equal (competitive-companion-test-result-verdict test-result)
                     'ac))
            test-results)))
         (all-success (null failed-test-results))
         (empty-stderr t))

    ;; We first check if there was any output to stderr:
    (dolist (test-result test-results)
      (unless (or (null (competitive-companion-test-result-stderr test-result))
                  (string-empty-p (competitive-companion-test-result-stderr test-result)))
        (setq empty-stderr nil)))

    ;; NOTE: Currently we only displayed failed results, maybe this should be user defined
    ;; I do think failed tests should come first nonetheless
    (with-current-buffer output-buffer
      (let ((inhibit-read-only t)
            (default-directory task-directory))

        ;; we don't want the test count to reset if the user reverts the buffer
        (put 'competitive-companion--test-count 'permanent-local t)
        (put 'competitive-companion--original-test-count 'permanent-local t)

        (erase-buffer)
        (competitive-companion-output-mode)

        ;; save some metadata on output buffer:
        (setq-local competitive-companion--current-task task-directory)
        (setq-local competitive-companion--current-command command)
        (setq-local competitive-companion--current-task-name task)
        (setq-local competitive-companion--current-time-limit time-limit)
        (unless competitive-companion--original-test-count
          (setq-local competitive-companion--test-count original-test-count)
          (setq-local competitive-companion--original-test-count competitive-companion--test-count))

        (magit-insert-section (competitive-companion-root-section)
          (magit-insert-heading (format "Failed Test Results: (%s)" task))
          (insert "\n")
          (dolist (test-result failed-test-results)
            (let ((index (competitive-companion-test-result-index test-result))
                  (input-text (competitive-companion-test-result-input test-result))
                  (expected-output (competitive-companion-test-result-expected test-result))
                  (verdict (competitive-companion-test-result-verdict test-result))
                  (stdout (competitive-companion-test-result-stdout test-result))
                  (stderr (competitive-companion-test-result-stderr test-result))
                  (input-file (competitive-companion-test-result-input-file test-result))
                  (output-file (competitive-companion-test-result-output-file test-result)))
              (magit-insert-section (competitive-companion-test-section index competitive-companion-collapse-test-cases)
                (magit-insert-heading (propertize (format "Test %s " index) 'font-lock-face 'magit-section-heading)
                  (competitive-companion--verdict-string verdict))
                (magit-insert-section (competitive-companion-input-section input-file)
                  (magit-insert-heading "Input")
                  (insert (format "%s\n" input-text)))
                (magit-insert-section (competitive-companion-expected-section output-file)
                  (magit-insert-heading "Expected Output")
                  (insert (format "%s\n" expected-output)))
                (if (and competitive-companion-separate-stderr
                         (not (string-empty-p stderr)))
                    (progn
                      (magit-insert-section (competitive-companion-actual-section)
                        (magit-insert-heading "Actual Output")
                        (insert (format "%s\n" stdout)))
                      (magit-insert-section (competitive-companion-actual-section)
                        (magit-insert-heading "Actual Output [stderr]")
                        (insert (format "%s\n" stderr))))
                  (magit-insert-section (competitive-companion-actual-section)
                    (magit-insert-heading "Actual Output")
                    (insert (format "%s\n" stdout))))))))
        (goto-char (point-min))
        (if all-success
            (if empty-stderr
                (message "All tests passed!")
              (message "All tests passed! [stderr IS BEING USED]"))

          (message "Some tests failed.")
          (pop-to-buffer output-buffer))))
    all-success))

(defun competitive-companion--open-section-file ()
  "Open the file associated with the current section."
  (interactive)
  (let ((file
         (magit-section-value-if
          '(competitive-companion-input-section competitive-companion-expected-section))))
    (if file
        (find-file file)
      (message "No file associated with this section."))))

(defun competitive-companion--remove-test-section-files ()
  "Remove the files associated with the current section."
  (interactive)
  (let ((index
         (magit-section-value-if 'competitive-companion-test-section)))
    (when index
      (unless competitive-companion--original-test-count
        (error "`competitive-companion--original-test-count' is not set! Cannot remove test case!"))
      (if (<= index competitive-companion--original-test-count)
          (message "This is an original test case! You should not remove it ;)")
        (let ((default-directory competitive-companion--current-task))
          (delete-file (expand-file-name (format "input%s.txt" index)))
          (delete-file (expand-file-name (format "output%s.txt" index))))
        (revert-buffer)))))

(defun competitive-companion--add-test-case ()
  "Add test case to current output buffer task, and move point to it."
  (interactive)
  (unless competitive-companion--test-count
    (error "`competitive-companion--test-count' is not set! Cannot add test case!"))
  (unless competitive-companion--current-task
    (error "`competitive-companion--current-task' is not set! Cannot add test case!"))
  (setq-local competitive-companion--test-count (1+ competitive-companion--test-count))
  (let ((input-file (expand-file-name (format "input%d.txt" competitive-companion--test-count) competitive-companion--current-task))
        (output-file (expand-file-name (format "output%d.txt" competitive-companion--test-count) competitive-companion--current-task)))
    (write-region "" nil input-file)
    (write-region "" nil output-file)
    (revert-buffer)
    (goto-char (point-min))
    (when (search-forward (format "Test %d " competitive-companion--test-count) nil t)
      (beginning-of-line))))

;;;; Structs

(cl-defstruct competitive-companion-test-result
  index
  verdict ;; one of: 'ac 'wa 'tle 'mle 'rte
  time-ms
  memory-kb
  input
  stdout ;; contains stderr as well if `competitive-companion-separate-stderr' is nil
  stderr
  expected
  ;; file metadata for sections
  input-file
  output-file)

;;;; Functions

(defun competitive-companion--start-server ()
  "Start a Competitive Companion server to receive problem data.

If a server is already running, fails silently."
  (unless (get-process "competitive-companion-server")
    (make-network-process
     :name "competitive-companion-server"
     :buffer "*competitive-companion-server*"
     :host "127.0.0.1"
     :service competitive-companion-server-port
     :family 'ipv4
     :server t
     :filter 'competitive-companion--handle-request)))

(defun competitive-companion--handle-request (connection message)
  "Handle incoming MESSAGE from the Competitive Companion CONNECTION."
  (let* ((lines (split-string message "\r\n"))
         (body (car (last lines))))
    (let ((data (condition-case nil
                    (json-parse-string body :object-type 'alist)
                  (error nil))))
      (if (not (and data (listp data)))
          (message "Invalid data received from Competitive Companion: '%s'" body)
        (process-send-string connection "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n")
        (delete-process connection)
        (competitive-companion--process-data data)))))

(defun competitive-companion--hard-limit (judge-limit)
  "Return the seconds a test case may run, or nil for no limit.
JUDGE-LIMIT is the judge's time limit in milliseconds, or nil if unknown.
See `competitive-companion-time-limit'."
  (pcase competitive-companion-time-limit
    ('nil nil)
    ((pred numberp) competitive-companion-time-limit)
    (`(factor . ,factor)
     (if judge-limit
         (max (* factor (/ judge-limit 1000.0))
              competitive-companion-minimum-time-limit)
       competitive-companion-minimum-time-limit))
    (value (user-error "Invalid `competitive-companion-time-limit': %S" value))))

(defun competitive-companion--slow-test-delay (judge-limit)
  "Return the seconds before reporting that a test case is still running.
JUDGE-LIMIT is the judge's time limit in milliseconds, or nil if unknown.
See `competitive-companion-slow-test-delay'."
  (cond
   (competitive-companion-slow-test-delay)
   (judge-limit (/ judge-limit 1000.0))
   (t 2)))

(defun competitive-companion--default-task-extension ()
  "Get the default file extension based on `competitive-companion-task-major-mode'."
  (let ((mode-name competitive-companion-task-major-mode))
    (cdr (or (assoc mode-name competitive-companion-languages #'equal)
             '("_" . ".txt")))))

(defun competitive-companion--task-filename (problem-data)
  "Return the filename used for task.

If `competitive-companion-prompt-task-filename' is non-nil,
prompt for the filename.  Otherwise, generate it automatically passing
`PROBLEM-DATA' to `competitive-companion-task-filename-generator'."
  (let ((default-filename (concat (funcall competitive-companion-task-filename-generator problem-data)
                                  (competitive-companion--default-task-extension))))
    (if competitive-companion--prompt-task-filename
        (read-file-name "Task file: " competitive-companion--contest-directory
                        (expand-file-name default-filename competitive-companion--contest-directory))
      default-filename)))

(defun competitive-companion-insert-default-header ()
  "Insert the default header in the current buffer.
This function uses `competitive-companion-last-problem-data' to
get the problem metadata.

If `competitive-companion-insert-header' is nil, this is a no-op."
  (when competitive-companion-insert-header
    (let-alist competitive-companion-last-problem-data
      (funcall competitive-companion-task-major-mode)
      (unless comment-start
        (user-error "The major mode %s, set by `competitive-companion-task-major-mode', does not define comment syntax!" competitive-companion-task-major-mode))
      (let ((start (point)))
        (insert (format "Problem: '%s'
Contest: '%s'
Judge: %s
URL: '%s'
Memory Limit: %s MB
Time Limit: %s ms

Powered by competitive-companion.el (https://github.com/luishgh/competitive-companion.el)

" .name .contest .judge .url .memoryLimit .timeLimit))
        (comment-region start (point))))))

(defun competitive-companion-insert-template-file ()
  "Insert the contents of `competitive-companion-task-template-file'."
  (when (and competitive-companion-task-template-file
             (file-regular-p competitive-companion-task-template-file))
    (insert-file-contents competitive-companion-task-template-file)))

(defun competitive-companion--process-data (data)
  "Process problem DATA received from Competitive Companion."
  ;; Augment data
  (when-let* ((group-str (alist-get 'group data))
              (parts (split-string group-str " - " t)))
    (push `(judge . ,(string-trim (car parts))) data)
    (push `(contest . ,(string-trim (cadr parts))) data))

  (setq competitive-companion-last-problem-data data)

  (let* ((name (alist-get 'name data))
         (group (alist-get 'group data))
         (tests (alist-get 'tests data))
         (task-filename (expand-file-name (competitive-companion--task-filename data) competitive-companion--contest-directory))
         (test-dir (expand-file-name (concat "." (file-name-base task-filename))
                                      (file-name-directory task-filename))))

    (make-directory test-dir t)
    (competitive-companion--write-test-cases test-dir tests)
    (unless (file-exists-p task-filename)
      (with-temp-file task-filename
        (run-hooks 'competitive-companion-file-generation-hook)))
    (with-current-buffer (find-file-noselect task-filename)
      (funcall competitive-companion-task-major-mode)
      (setq-local competitive-companion--current-task test-dir)
      (setq-local competitive-companion--current-task-name name)
      (setq-local competitive-companion--original-test-count (length tests))
      (setq-local competitive-companion--current-time-limit (alist-get 'timeLimit data)))
    (message "Problem '%s' (%s) fetched and saved to %s" name group task-filename)))

(defun competitive-companion--write-test-cases (directory test-cases)
  "Write TEST-CASES to DIRECTORY.  Each test case is a pair of input and output."
  (cl-loop for index from 1
           for test across test-cases do
           (let ((input-file (expand-file-name (format "input%d.txt" index) directory))
                 (output-file (expand-file-name (format "output%d.txt" index) directory)))
             (write-region (alist-get 'input test) nil input-file)
             (write-region (alist-get 'output test) nil output-file))))

(defun competitive-companion--kill-buffers ()
  "Kill all buffers that start with '*competitive-companion'."
  (dolist (buffer (buffer-list))
    (let ((buffer-name (buffer-name buffer)))
      (when (and buffer-name (string-prefix-p "*competitive-companion" buffer-name))
        (kill-buffer buffer)))))

(defun competitive-companion--run-test-cases (command task-directory &optional judge-limit)
  "Run COMMAND against all test cases in TASK-DIRECTORY.

JUDGE-LIMIT is the judge's time limit in milliseconds, or nil if unknown.
It decides how long each test case may run, see
`competitive-companion-time-limit'.

Returns a list of `cc-test-result'.
The order of results is not guaranteed."

  (let* ((default-directory task-directory)
         (test-files (directory-files task-directory t "^input[0-9]+\.txt$"))
         (hard-limit (competitive-companion--hard-limit judge-limit))
         (warn-after (competitive-companion--slow-test-delay judge-limit))
         (test-results (list)))
    (dolist-with-progress-reporter (input-file test-files test-results)
        "Running test cases..."
      (let* ((match-input (file-name-nondirectory input-file))
             (_ (string-match "[0-9]+" match-input))
             (index (string-to-number (match-string 0 match-input)))
             (output-file (expand-file-name (format "output%d.txt" index))))
        (push (competitive-companion--run-program command index input-file output-file
                                                  hard-limit warn-after)
              test-results)))))

(cl-defun competitive-companion--execute (command input-file
                                                  &key index hard-limit warn-after merge-stderr)
  "Run COMMAND with the contents of INPUT-FILE on its stdin.

INDEX is the test case number, used to report a slow test case after
WARN-AFTER seconds.  The program is killed after HARD-LIMIT seconds;
if HARD-LIMIT is nil, it runs until it exits or the user quits.  If
MERGE-STDERR is non-nil, stderr is mixed into stdout, keeping the order
in which the program wrote them.

Return a plist with:
  :status   `exit', `signal', `timeout' or `aborted' (the user quit)
  :code     the exit status or signal number, if the program ended itself
  :stdout   the program's stdout (with stderr, if MERGE-STDERR)
  :stderr   the program's stderr, or nil if MERGE-STDERR
  :elapsed  wall-clock seconds the program ran"
  (let ((stdout-buffer (generate-new-buffer " *competitive-companion-stdout*"))
        (stderr-buffer (unless merge-stderr
                         (generate-new-buffer " *competitive-companion-stderr*")))
        (start (float-time))
        stderr-process process status elapsed)
    (unwind-protect
        (progn
          (when stderr-buffer
            (setq stderr-process
                  (make-pipe-process :name "competitive-companion-stderr"
                                     :buffer stderr-buffer
                                     :coding 'utf-8-unix
                                     :noquery t
                                     :sentinel #'ignore)))
          (setq process
                (make-process :name "competitive-companion-run"
                              :command (list command)
                              :buffer stdout-buffer
                              :stderr stderr-process
                              :coding 'utf-8-unix
                              :connection-type 'pipe
                              :noquery t
                              :sentinel #'ignore))
          (condition-case nil
              (progn
                ;; Programs that exit without reading all of their input
                ;; close the pipe early; that is not an error.
                (ignore-error error
                  (with-temp-buffer
                    (insert-file-contents input-file)
                    (process-send-region process (point-min) (point-max)))
                  (process-send-eof process))
                (let ((reported 0))
                  (while (and (process-live-p process) (not status))
                    (accept-process-output process 0.05)
                    (setq elapsed (- (float-time) start))
                    (when (and warn-after
                               (> elapsed warn-after)
                               (>= (floor elapsed) reported))
                      (setq reported (1+ (floor elapsed)))
                      (message "Test %d still running (%.1fs)... %s to abort"
                               index elapsed (substitute-command-keys "\\[keyboard-quit]")))
                    (when (and hard-limit (> elapsed hard-limit))
                      (setq status 'timeout)))))
            (quit (setq status 'aborted)))
          (setq elapsed (- (float-time) start))
          (when status
            (kill-process process))
          ;; Wait for the process to end and read whatever it wrote.
          (while (accept-process-output process 0.01))
          (while (process-live-p process)
            (accept-process-output process 0.01))
          (when stderr-process
            (while (accept-process-output stderr-process 0.01)))
          (list :status (or status (process-status process))
                :code (unless status (process-exit-status process))
                :stdout (with-current-buffer stdout-buffer (buffer-string))
                :stderr (when stderr-buffer
                          (with-current-buffer stderr-buffer (buffer-string)))
                :elapsed elapsed))
      (when process (delete-process process))
      (when stderr-process (delete-process stderr-process))
      (kill-buffer stdout-buffer)
      (when stderr-buffer (kill-buffer stderr-buffer)))))

(defun competitive-companion--normalize-output (output)
  "Return OUTPUT without blank lines and surrounding whitespace on each line."
  (concat (string-join (split-string output "\n" t "[ \t\r]+") "\n") "\n"))

(defun competitive-companion--run-program (command index input-file output-file
                                                   &optional hard-limit warn-after)
  "Run COMMAND with INPUT-FILE as input and return a test-result record.

INDEX should be the number identifing the test case and the contents of
OUTPUT-FILE are used to decide the verdict.  HARD-LIMIT and WARN-AFTER
are passed to `competitive-companion--execute'."
  (let* ((expected-output (with-temp-buffer
                            (insert-file-contents output-file)
                            (buffer-string)))
         (input-text (with-temp-buffer
                       (insert-file-contents input-file)
                       (buffer-string)))
         ;; The verdict is always decided on stdout alone.
         (run (competitive-companion--execute command input-file
                                              :index index
                                              :hard-limit hard-limit
                                              :warn-after warn-after))
         (stdout (competitive-companion--normalize-output (plist-get run :stdout)))
         (stderr (plist-get run :stderr))
         (verdict (pcase (plist-get run :status)
                    ((or 'timeout 'aborted) 'tle)
                    ('exit (cond ((/= (plist-get run :code) 0) 'rte)
                                 ((string= stdout expected-output) 'ac)
                                 (t 'wa)))
                    (_ 'rte)))
         (shown-output
          (cond
           ((or competitive-companion-separate-stderr (eq verdict 'ac)) stdout)
           ;; Running a timed out program again would mean waiting for it twice.
           ((eq verdict 'tle)
            (competitive-companion--normalize-output (concat (plist-get run :stdout) stderr)))
           ;; Run again with stderr mixed into stdout, keeping their order.
           (t
            (competitive-companion--normalize-output
             (plist-get (competitive-companion--execute command input-file
                                                        :index index
                                                        :hard-limit hard-limit
                                                        :warn-after warn-after
                                                        :merge-stderr t)
                        :stdout))))))
    (make-competitive-companion-test-result
     :index index
     :verdict verdict
     :time-ms (round (* 1000 (plist-get run :elapsed)))
     :input input-text
     :stdout shown-output
     :stderr stderr
     :expected expected-output
     :input-file input-file
     :output-file output-file)))

(defun competitive-companion--verdict-string (verdict)
  "Get VERDICT identifying string.
Returns a string propertized with a semantic font-locking."
  (pcase verdict
    ('wa (propertize " WA " 'font-lock-face 'competitive-companion-verdict-wa))
    ('tle (propertize " TLE " 'font-lock-face 'competitive-companion-verdict-tle))
    ('rte (propertize " RTE " 'font-lock-face 'competitive-companion-verdict-rte))
    ('ac (propertize " AC " 'font-lock-face 'competitive-companion-verdict-ac))
    (_ (error "Competitive Companion: not recognized verdict: %s" verdict))))

;;;; Faces

(defface competitive-companion-verdict-wa
  '((t :inherit error))
  "Face for the WA text in output buffer."
  :group 'competitive-companion)

(defface competitive-companion-verdict-tle
  '((t :inherit warning))
  "Face for the TLE text in output buffer."
  :group 'competitive-companion)

(defface competitive-companion-verdict-rte
  '((t :inherit warning))
  "Face for the RTE text in output buffer."
  :group 'competitive-companion)

(defface competitive-companion-verdict-ac
  '((t :inherit success))
  "Face for the AC text in output buffer."
  :group 'competitive-companion)

(defface competitive-companion-modeline-face
  '((t :foreground "#ff0000"))
  "Face for the balloon icon in modeline.
See `competitive-companion-modeline-support'."
  :group 'competitive-companion)

;;; _
;; Local Variables:
;; read-symbol-shorthands: (
;;   ("cc-"       . "competitive-companion-"))
;; End:
(provide 'competitive-companion)
;;; competitive-companion.el ends here

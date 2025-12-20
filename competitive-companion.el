;;; competitive-companion.el --- Competitive Companion Integration -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025 Luis Higino

;; Author: Luis Higino <luis.higino@dcc.ufmg.br>
;; URL: https://github.com/luishgh/competitive-companion.el
;; Created: 25 Dec 2024
;; Package-Requires: ((emacs "28.1"))
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
(require 'json)
(require 'server)

;;;; Variables

(defvar-local competitive-companion--current-task nil
  "Holds the current task's (i.e. problem), associated with its data dir.")

(defvar competitive-companion--contest-directory nil
  "Holds the current contest's directory.")

(defvar competitive-companion-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c .") #'competitive-companion-run-tests)
    map)
  "Keymap for `competitive-companion-mode'.")

;; comment: only RET works currently
(defvar competitive-companion-file-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'competitive-companion--open-section-file)
    (define-key map [mouse-2] 'competitive-companion--open-section-file)
    (define-key map [follow-link] 'code-review-utils--visit-author-at-point)
    map)
  "Keymap for output buffer's file sections.")

(defvar competitive-companion--prompt-task-filename nil
  "Holds value of `competitive-companion-prompt-task-filename' when mode turns on.")

;;;; Customization

(defgroup competitive-companion nil
  "Competitive Companion integration for Emacs."
  :group 'tools
  :prefix "competitive-companion-")

;;;;; Options

(defcustom competitive-companion-task-major-mode 'c++-mode
  "Major mode for task's implementation file."
  :type 'symbol
  :group 'competitive-companion)

(defcustom competitive-companion-task-template-file "~/Documents/Maratona/templ.cpp"
  "Path to a template file to use for task files.

If file is non-empty, its contents will be inserted verbatim below
the task file header.

If the user prefers using proper templating packages for this,
this variable should point to a non existing file."
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
  :type '(list (cons symbol string))
  :group 'competitive-companion)

(defcustom competitive-companion-collapse-test-cases 't
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

;;;; Commands

;;;###autoload
(define-minor-mode competitive-companion-mode
  "Minor mode for Competitive Companion integration.
Turning this on sets the variable
`competitive-companion--contest-directory' to
`default-directory', so it should be done inside
the contest's dedicated folder."
  :global t
  :lighter " CC"
  :keymap competitive-companion-mode-map
  (if competitive-companion-mode
      (progn
        (setq competitive-companion--contest-directory default-directory)
        (setq competitive-companion--prompt-task-filename competitive-companion-prompt-task-filename)
        (competitive-companion--start-server)
        (message "Competitive Companion mode activated."))
    (setq competitive-companion--contest-directory nil)
    (setq competitive-companion--current-task nil)
    (delete-process "*competitive-companion-server*")
    (competitive-companion--kill-buffers)
    (message "Competitive Companion mode deactivated.")))

;; TODO: rework those classes, they don't work exactly as
;; intended. Only the keymap field is used, the file is recovered by
;; the value field
(require 'magit-section)

(defclass competitive-companion-root-section (magit-section) ())
(defclass competitive-companion-test-section (magit-section)
  ((test-index :initarg :test-index  :type string :documentation "Index of the test case.")))

(defclass competitive-companion-input-section (magit-section)
  ((file :initarg :file :type string :documentation "Path to the input file.")
   (keymap :initform 'competitive-companion-file-section-map)))

(defclass competitive-companion-expected-section (magit-section)
  ((keymap :initform 'competitive-companion-file-section-map)
   (file :initarg :file :type string :documentation "Path to the expected output file.")))

(defclass competitive-companion-actual-section (magit-section) ())

(defun competitive-companion-run-tests (command)
  "Run all test cases for the current task using `COMMAND'.
Reports success if all tests pass, or failure otherwise."
  (interactive (progn
                 (unless competitive-companion-mode
                   (error "Competitive Companion mode is not turned on"))
                 (unless competitive-companion--current-task
                   (error "The current task has not been fetched"))
                 (list (expand-file-name
                        (read-shell-command "Command to run: " competitive-companion--contest-directory)))))

  (let* ((task-directory competitive-companion--current-task)
         (default-directory task-directory)
         (task-hashname (file-name-nondirectory task-directory)) ;; FIXME: better name or kill buffer if present
         (task (substring task-hashname 0 1))
         (test-files (directory-files task-directory t "^input[0-9]+\.txt$"))
         (all-success t)
         (empty-stderr t)
         (output-buffer (generate-new-buffer (format "*competitive-companion-output* [%s]" task-hashname))))
    (with-current-buffer output-buffer
      (let ((inhibit-read-only t)
            (default-directory task-directory))
        (erase-buffer)
        (magit-section-mode)
        (magit-insert-section (competitive-companion-root-section)
          (magit-insert-heading (format "Failed Test Results: (%s)" task))
          (insert "\n")
          (dolist (input-file test-files)
            (let* ((match-input (file-name-nondirectory input-file))
                   (_ (string-match "[0-9]+" match-input))
                   (index (match-string 0 match-input))
                   (output-file (format "output%s.txt" index))
                   (actual-output (competitive-companion--run-program command input-file))
                   (input-text (with-temp-buffer
                                 (insert-file-contents input-file)
                                 (buffer-string)))
                   (expected-output (with-temp-buffer
                                      (insert-file-contents output-file)
                                      (buffer-string))))
              (progn
                (unless (string-empty-p (cadr actual-output))
                  (setq empty-stderr nil))
                (unless (string= (car actual-output) expected-output)
                  (setq all-success nil)
                  (magit-insert-section (competitive-companion-test-section index competitive-companion-collapse-test-cases)
                    (magit-insert-heading (format "Test %s" index))
                    (magit-insert-section (competitive-companion-input-section input-file)
                      (magit-insert-heading "Input")
                      (insert (format "%s\n" input-text)))
                    (magit-insert-section (competitive-companion-expected-section output-file)
                      (magit-insert-heading "Expected Output")
                      (insert (format "%s\n" expected-output)))
                    (if (and competitive-companion-separate-stderr
                             (not (string-empty-p (cadr actual-output))))
                        (progn
                          (magit-insert-section (competitive-companion-actual-section)
                            (magit-insert-heading "Actual Output")
                            (insert (format "%s\n" (car actual-output))))
                          (magit-insert-section (competitive-companion-actual-section)
                            (magit-insert-heading "Actual Output [stderr]")
                            (insert (format "%s\n" (cadr actual-output)))))
                      (magit-insert-section (competitive-companion-actual-section)
                        (magit-insert-heading "Actual Output")
                        (insert (format "%s\n" (concat (caddr actual-output))))))))))))
        (if all-success
            (if empty-stderr
                (message "All tests passed!")
              (message "All tests passed! [stderr IS BEING USED]"))

          (message "Some tests failed.")
          (pop-to-buffer output-buffer))))))

(defun competitive-companion--open-section-file ()
  "Open the file associated with the current section."
  (interactive)
  (let ((file
         (magit-section-value-if
          '(competitive-companion-input-section competitive-companion-expected-section))))
    (if file
        (find-file file)
      (message "No file associated with this section."))))

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
                    (json-read-from-string body)
                  (error nil))))
      (if (not (and data (listp data)))
          (message "Invalid data received from Competitive Companion: '%s'" body)
        (process-send-string connection "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n")
        (delete-process connection)
        (competitive-companion--process-data data)))))

(defun competitive-companion--default-task-extension ()
  "Get the default file extension based on `competitive-companion-task-major-mode'."
  (let ((mode-name competitive-companion-task-major-mode))
    (cdr (or (assoc mode-name competitive-companion-languages #'equal)
             '("_" . ".txt")))))

(defun competitive-companion--task-filename (name)
  "Return the filename used for task named `NAME'.

If `competitive-companion-prompt-task-filename' is non-nil, prompt for
the filename.  Otherwise, generate it automatically based on `NAME'."
  (let ((default-filename (concat (substring name 0 1)
                                  (competitive-companion--default-task-extension))))
    (if competitive-companion--prompt-task-filename
        (read-file-name "Task file: " competitive-companion--contest-directory
                        (expand-file-name default-filename competitive-companion--contest-directory))
      default-filename)))

(defun competitive-companion--process-data (data)
  "Process problem DATA received from Competitive Companion."
  (let* ((name (alist-get 'name data))
         (group (alist-get 'group data))
         (url (alist-get 'url data))
         (tests (alist-get 'tests data))
         (memory-limit (alist-get 'memoryLimit data))
         (time-limit (alist-get 'timeLimit data))
         (slug (replace-regexp-in-string "[\\/:*?\"<>| ]" "_" name))
         (temp-dir (make-temp-file slug t))
         (task-filename (expand-file-name (competitive-companion--task-filename name) competitive-companion--contest-directory)))
    (competitive-companion--write-test-cases temp-dir tests)
    (unless (file-exists-p task-filename)
      (with-temp-file task-filename
		(insert (format "// Problem: '%s'
// Contest: '%s'
// URL: '%s'
// Memory Limit: %s MB
// Time Limit: %s ms
//
// Powered by competitive-companion.el (https://github.com/luishgh/competitive-companion.el)

" name group url memory-limit time-limit))
        (when competitive-companion-task-template-file
          (insert-file-contents competitive-companion-task-template-file))))
    (with-current-buffer (find-file-noselect task-filename)
      (funcall competitive-companion-task-major-mode)
      (setq-local competitive-companion--current-task temp-dir))
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

(defun competitive-companion--run-program (command input-file)
  "Run COMMAND with INPUT-FILE as input.

Trims trailing whitespace from each line of stdout.  Returns a list
containing three strings (stdout, stderr and both together)."
  (let ((out-buffer (generate-new-buffer " *cc-out*"))
        (stdout-buffer (generate-new-buffer " *cc-stdout*"))
        (stderr-file (make-temp-file "cc-stderr-")))
    (unwind-protect
        (progn

          (call-process command input-file (list out-buffer t))

          ;; Call the command, redirecting stdout to buffer, stderr to file
          (call-process command input-file (list stdout-buffer stderr-file))

          ;; Process stdout
          (let* ((out (with-current-buffer out-buffer
                           (split-string (buffer-string) "\n" t "[ \t\r]+")))
                 (stdout (with-current-buffer stdout-buffer
                           (split-string (buffer-string) "\n" t "[ \t\r]+")))
                 (joined-stdout (concat (string-join stdout "\n") "\n"))
                 (joined-out (concat (string-join out "\n") "\n"))
                 (stderr (with-temp-buffer
                           (insert-file-contents stderr-file)
                           (buffer-string))))
            (list joined-stdout stderr joined-out)))
      ;; Cleanup
      (kill-buffer stdout-buffer)
      (delete-file stderr-file))))

(provide 'competitive-companion)
;;; competitive-companion.el ends here

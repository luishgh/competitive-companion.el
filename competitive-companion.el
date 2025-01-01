;;; competitive-companion.el --- Competitive Companion Integration -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Luis Higino

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

Each entry should be of the form (MAJOR-MODE . EXTENSION), where
`MAJOR-MODE' is the name of the major mode used for that programming
language and `EXTENSION' is a string indicating the file extension,
always preceded by a dot."
  :type 'list
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
        (competitive-companion--start-server)
        (message "Competitive Companion mode activated."))
    (setq competitive-companion--contest-directory nil)
    (setq competitive-companion--current-task nil)
    (delete-process "*competitive-companion-server*")
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
                   (error "Competitive Companion mode is not turned on!"))
                 (unless competitive-companion--current-task
                   (error "The current task has not been fetched!"))
                 (list (read-shell-command "Command to run: " competitive-companion--contest-directory))))

  (let* ((default-directory competitive-companion--current-task)
         (task (substring (file-name-nondirectory default-directory) 0 1))
         (test-files (directory-files default-directory t "^input[0-9]+\.txt$"))
         (all-success t))
    (with-current-buffer (get-buffer-create (format "*competitive-companion-output* [%s]" task))
      (let ((inhibit-read-only t))
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
                   (actual-output (shell-command-to-string (format "%s < %s" command input-file)))
                   (input-text (with-temp-buffer
                                 (insert-file-contents input-file)
                                 (buffer-string)))
                   (expected-output (with-temp-buffer
                                      (insert-file-contents output-file)
                                      (buffer-string))))
              (unless (string= actual-output expected-output)
                (setq all-success nil)
                (magit-insert-section (competitive-companion-test-section :test-index index)
                  (magit-insert-heading (format "Test %s" index))
                  (magit-insert-section (competitive-companion-input-section input-file)
                    (magit-insert-heading "Input")
                    (insert (format "%s\n" input-text)))
                  (magit-insert-section (competitive-companion-expected-section output-file)
                    (magit-insert-heading "Expected Output")
                    (insert (format "%s\n" expected-output)))
                  (magit-insert-section (competitive-companion-actual-section)
                    (magit-insert-heading "Actual Output")
                    (insert (format "%s\n" actual-output))))))))
        (if all-success
            (message "All tests passed!")
          (message "Some tests failed.")
          (pop-to-buffer (format "*competitive-companion-output* [%s]" task)))))))

(defun competitive-companion--open-section-file ()
  "Open the file associated with the current section."
  (interactive)
  (let ((section (magit-current-section)))
    (when section
      (let ((file (cond
                   ((object-of-class-p section competitive-companion-input-section)
                    (oref section value))
                   ((object-of-class-p section competitive-companion-expected-section)
                    (oref section value)))))
        (if file
            (find-file file)
          (message "No file associated with this section."))))))

;;;; Functions

(defun competitive-companion--start-server ()
  "Start a Competitive Companion server to receive problem data."
  (make-network-process
   :name "competitive-companion-server"
   :buffer "*competitive-companion-server*"
   :host "127.0.0.1"
   :service competitive-companion-server-port
   :family 'ipv4
   :server t
   :filter 'competitive-companion--handle-request))

(defun competitive-companion--handle-request (connection message)
  "Handle incoming MESSAGE from the Competitive Companion CONNECTION."
  (let* ((lines (split-string message "\r\n"))
         (body (car (last lines))))
    (let ((data (condition-case nil
                    (json-read-from-string body)
                  (error nil))))
      (if (not (and data (listp data)))
          (message "Invalid data received from Competitive Companion: '%s'" body)
        (competitive-companion--process-data data))
      (process-send-string connection "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n")
      (delete-process connection))))

(defun competitive-companion--default-task-extension ()
  "Get the default file extension based on `competitive-companion-task-major-mode'."
  (let ((mode-name competitive-companion-task-major-mode))
    (cdr (or (assoc mode-name competitive-companion-languages #'equal)
             '("_" . ".txt")))))

(defun competitive-companion--task-filename (name)
  "Return the filename used for task named `NAME'.

`NAME' is expected to be the value from the name field of a response
from the Competitive Companion browser extension."
  (concat (substring name 0 1) (competitive-companion--default-task-extension)))

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

(provide 'competitive-companion)
;;; competitive-companion.el ends here

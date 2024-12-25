;;; competitive-companion.el --- Wrapper for Competitive Companion -*- lexical-binding: t; -*-

;;; Commentary:
;; This plugin provides an Emacs interface to interact with the Competitive Companion browser extension.
;; Inspired by competitest.nvim and cphelper.nvim, it allows users to fetch competitive programming problem
;; data and manage test cases directly within Emacs.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'server)

(defgroup competitive-companion nil
  "Competitive Companion integration for Emacs."
  :group 'tools
  :prefix "competitive-companion-")

(defcustom competitive-companion-task-major-mode 'c++-mode
  "Major mode for task's implementation file."
  :type 'symbol
  :group 'competitive-companion)

(defcustom competitive-companion-task-template-file "~/Documents/Maratona/templ.cpp"
  "Path to a template file to use for task files.
If non-nil, the contents of this file will be inserted verbatim below
the task file header."
  :type 'file
  :group 'competitive-companion)

(defun competitive-companion--default-task-extension ()
  "Get the default file extension based on `competitive-companion-task-major-mode'."
  (pcase competitive-companion-task-major-mode
    ('c-mode ".c")
    ('c++-mode ".cpp")
    ('python-mode ".py")
    ('java-mode ".java")
    ('ruby-mode ".rb")
    ('javascript-mode ".js")
    (_ ".txt")))

(defvar-local competitive-companion--current-task nil
  "Holds the current task's (i.e. problem), associated with its data dir.")

(defvar competitive-companion--contest-directory nil
  "Holds the current contest's directory.")

(define-minor-mode competitive-companion-mode
  "Minor mode for Competitive Companion integration."
  :lighter " CC"
  (if competitive-companion-mode
      (progn
        (setq competitive-companion--contest-directory default-directory)
        (competitive-companion--start-server)
        (message "Competitive Companion mode activated."))
    (setq competitive-companion--contest-directory nil)
    (setq competitive-companion--current-task nil)
    (delete-process "*competitive-companion-server*")
    (message "Competitive Companion mode deactivated.")))

(defcustom competitive-companion-server-port 10050
  "Port on which the Competitive Companion server listens."
  :type 'integer
  :group 'competitive-companion)

(defcustom competitive-companion-directory "~/competitive/"
  "Directory to store problem files and test cases."
  :type 'string
  :group 'competitive-companion)

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

(defun competitive-companion--process-data (data)
  "Process problem DATA received from Competitive Companion."
  (let* ((name (alist-get 'name data))
         (group (alist-get 'group data))
         (url (alist-get 'url data))
         (tests (alist-get 'tests data))
         (memory-limit (alist-get 'memoryLimit data))
         (time-limit (alist-get 'timeLimit data))
         (slug (replace-regexp-in-string "[\\/:*?\"<>| ]" "_" name))
         (dir (competitive-companion--create-problem-directory slug))
         (temp-dir (make-temp-file slug t))
         (extension (competitive-companion--default-task-extension))
         (task-filename (expand-file-name (concat (substring name 0 1) extension) competitive-companion--contest-directory)))
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
    (message "Problem '%s' (%s) fetched and saved to %s" name group dir)))

(defun competitive-companion--create-problem-directory (slug)
  "Create and return the problem directory using SLUG."
  (let ((dir (expand-file-name slug competitive-companion-directory)))
    (unless (file-exists-p dir)
      (make-directory dir t))
    dir))

(defun competitive-companion--write-test-cases (directory test-cases)
  "Write TEST-CASES to DIRECTORY.  Each test case is a pair of input and output."
  (cl-loop for index from 1
           for test across test-cases do
           (let ((input-file (expand-file-name (format "input%d.txt" index) directory))
                 (output-file (expand-file-name (format "output%d.txt" index) directory)))
             (write-region (alist-get 'input test) nil input-file)
             (write-region (alist-get 'output test) nil output-file))))

(defun competitive-companion-start ()
  "Start the Competitive Companion server."
  (interactive)
  (competitive-companion--start-server)
  (message "Competitive Companion server started on port %d" competitive-companion-server-port))

(defun competitive-companion-run-tests (command)
  "Run all test cases for the current task using COMMAND.
Reports success if all tests pass, or failure otherwise."
  (interactive (list (read-shell-command "Command to run: " (concat competitive-companion--contest-directory "/"))))
  (let* ((default-directory competitive-companion--current-task)
         (test-files (directory-files default-directory t "^input[0-9]+\.txt$"))
         (failed-outputs "")
         (all-success t))
    (dolist (input-file test-files)
      (let* ((match-input (file-name-nondirectory input-file))
             (match (string-match "[0-9]+" match-input))
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
          (setq failed-outputs
                (concat failed-outputs
                  (format "* Test %s failed.\n** Input:\n%s\n** Expected:\n%s\n** Got:\n%s\n" index input-text expected-output actual-output)))))
    (if all-success
        (message "All tests passed!")
      (with-help-window "*competitive-companion-output*"
        (outline-minor-mode)
        (message "Some tests failed.")
        (insert failed-outputs))))))

(provide 'competitive-companion)

;;; competitive-companion.el ends here

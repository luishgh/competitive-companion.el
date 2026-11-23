;;; competitive-companion-integration-tests.el --- Tests with a real browser -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; End-to-end tests: a real Chrome with the Competitive Companion
;; extension opens live Codeforces and AtCoder pages, its toolbar button is
;; clicked, and the problems it sends reach `competitive-companion-mode'.
;; Real C++ solutions are then compiled and judged.
;;
;; These tests need the network, Node.js and g++, so `make check' (and
;; thus `guix build') does not run them.  Run them with `make integration',
;; which installs the browser driver (test/integration/) first.
;;
;; Codeforces shows a Cloudflare challenge to headless browsers; its test
;; is then skipped.  Set CC_INTEGRATION_HEADFUL=1 to use a visible browser,
;; which gets through.

;;; Code:

(require 'ert)
(require 'competitive-companion)
(require 'competitive-companion-test-helpers)

(defconst cc-integration-directory
  (expand-file-name "integration" (file-name-directory (or load-file-name buffer-file-name)))
  "Directory of the browser driver, the extension and the solutions.")

(defconst cc-integration-fetch-timeout 90
  "Seconds to wait for the browser to send all expected problems.")

(defun cc-integration-missing ()
  "Return why the integration tests cannot run, or nil if they can."
  (cond
   ((not (executable-find "node")) "node is not installed")
   ((not (executable-find "g++")) "g++ is not installed")
   ((not (file-directory-p (expand-file-name "node_modules/puppeteer" cc-integration-directory)))
    "Puppeteer is not installed; run `make integration'")
   ((not (file-exists-p (expand-file-name ".cache/extension/manifest.json" cc-integration-directory)))
    "the extension is not downloaded; run `make integration'")))

(defmacro cc-integration-skip-unless-ready ()
  "Skip the current test if the integration tests cannot run."
  `(when-let* ((reason (cc-integration-missing)))
     (ert-skip reason)))

(defmacro cc-integration-with-contest (spec &rest body)
  "Run BODY with `competitive-companion-mode' on in a fresh contest directory.
SPEC is (DIR PORT): DIR is bound to the contest directory and PORT to
the port the task server listens on, a random free one.  Task files are
C++ files with the default header and no template.  Everything is torn
down afterwards."
  (declare (indent 1))
  (let ((dir (car spec)) (port (cadr spec)))
    `(let* ((,dir (file-name-as-directory (make-temp-file "cc-integration-" t)))
            (default-directory ,dir)
            (competitive-companion-server-port t)
            (competitive-companion-task-major-mode 'c++-mode)
            (competitive-companion-task-template-file nil)
            (competitive-companion-file-generation-hook
             '(competitive-companion-insert-default-header))
            (competitive-companion-insert-header t)
            (competitive-companion-collapse-test-cases nil)
            (competitive-companion-last-problem-data nil))
       (unwind-protect
           (progn
             (competitive-companion-mode 1)
             (let ((,port (process-contact (get-process "competitive-companion-server")
                                           :service)))
               ,@body))
         (competitive-companion-mode -1)
         (cc-test-stop-server ,dir)))))

(cl-defun cc-integration-fetch (port url wait-for until)
  "Have the browser parse URL and send the result to PORT.
WAIT-FOR is a CSS selector that shows the page has loaded.  Wait until
UNTIL, called with no arguments, returns non-nil, and return its value.
Skip the test if the page cannot be loaded, fail it if UNTIL does not
become non-nil in time."
  (let* ((stdout "")
         (stderr-buffer (generate-new-buffer " *cc-integration-driver-stderr*"))
         (default-directory cc-integration-directory)
         (driver (make-process
                  :name "cc-integration-driver"
                  :command (list "node" "driver.mjs"
                                 "--port" (number-to-string port)
                                 "--url" url
                                 "--wait-for" wait-for)
                  :connection-type 'pipe
                  :noquery t
                  :stderr stderr-buffer
                  :filter (lambda (_process string) (setq stdout (concat stdout string)))
                  :sentinel #'ignore))
         result)
    (unwind-protect
        (progn
          (setq result (cc-test-wait-for
                        (lambda () (or (funcall until)
                                       (not (process-live-p driver))))
                        cc-integration-fetch-timeout))
          (when (string-match "^SKIP \\(.*\\)$" stdout)
            (ert-skip (match-string 1 stdout)))
          ;; The driver may have ended right after the problems arrived.
          (setq result (funcall until))
          (unless result
            (ert-fail (list "The browser did not send the expected problems"
                            :url url :driver-stdout stdout
                            :driver-stderr (with-current-buffer stderr-buffer
                                             (buffer-string)))))
          result)
      (when (process-live-p driver)
        ;; Closing its stdin makes the driver close the browser.
        (ignore-errors (process-send-eof driver))
        (cc-test-wait-for (lambda () (not (process-live-p driver))) 10))
      (delete-process driver)
      (kill-buffer stderr-buffer))))

(defun cc-integration-task-buffers (dir)
  "Return the buffers of the tasks fetched into DIR."
  (seq-filter (lambda (buffer)
                (and (buffer-file-name buffer)
                     (file-in-directory-p (buffer-file-name buffer) dir)
                     (buffer-local-value 'competitive-companion--current-task buffer)))
              (buffer-list)))

(defun cc-integration-compile (solution dir name)
  "Compile SOLUTION, a file in test/integration/solutions, into DIR/NAME.
Return the program's file name."
  (let ((program (expand-file-name name dir)))
    (with-temp-buffer
      (unless (eql 0 (call-process "g++" nil t nil "-O2" "-o" program
                                   (expand-file-name solution
                                                     (expand-file-name "solutions"
                                                                       cc-integration-directory))))
        (error "Could not compile %s:\n%s" solution (buffer-string))))
    program))

(defun cc-integration-run (task-buffer &optional command)
  "Run the tests of TASK-BUFFER with COMMAND, as \\[competitive-companion-run-tests] would.
Without COMMAND, the program is guessed from the task file's name.
Return (RESULT . OUTPUT): what the command returned and the text of the
output buffer."
  (with-current-buffer task-buffer
    (let ((result (competitive-companion-run-tests command nil)))
      (cons result
            (with-current-buffer competitive-companion--output-buffer
              (buffer-substring-no-properties (point-min) (point-max)))))))

(defun cc-integration-header-p (task-buffer &rest lines)
  "Return non-nil if TASK-BUFFER's file header contains each of LINES."
  (let ((text (with-current-buffer task-buffer
                (buffer-substring-no-properties (point-min) (point-max)))))
    (seq-every-p (lambda (line) (string-search line text)) lines)))

;;;; Single problems

(ert-deftest cc-integration/codeforces-problem ()
  "A Codeforces problem is fetched, and a correct solution passes."
  (cc-integration-skip-unless-ready)
  (cc-integration-with-contest (dir port)
    (let ((buffer (cc-integration-fetch
                   port "https://codeforces.com/problemset/problem/4/A" ".problem-statement"
                   (lambda () (cc-test-task-buffer dir "a.cpp" 0)))))
      (with-current-buffer buffer
        (should (eq major-mode 'c++-mode))
        (should (equal competitive-companion--current-task-name "A. Watermelon"))
        (should (equal competitive-companion--current-time-limit 1000))
        (should (equal competitive-companion--original-test-count 1)))
      (should (cc-integration-header-p buffer "Problem: 'A. Watermelon'"
                                       "Judge: Codeforces" "Time Limit: 1000 ms"
                                       "Memory Limit: 64 MB"))
      (should (equal (cc-test-file-string (expand-file-name ".a/input1.txt" dir)) "8\n"))
      (should (equal (cc-test-file-string (expand-file-name ".a/output1.txt" dir)) "YES\n"))
      (cc-integration-compile "cf4a.cpp" dir "a")
      (should (eq (car (cc-integration-run buffer)) t)))))

(ert-deftest cc-integration/atcoder-problem ()
  "An AtCoder problem is fetched; real programs get the right verdicts."
  (cc-integration-skip-unless-ready)
  (cc-integration-with-contest (dir port)
    (let ((buffer (cc-integration-fetch
                   port "https://atcoder.jp/contests/abs/tasks/abc086_a" "#task-statement"
                   (lambda () (cc-test-task-buffer dir "abc086a.cpp" 0))))
          (competitive-companion-time-limit 1))
      (with-current-buffer buffer
        (should (equal competitive-companion--current-task-name "ABC086A - Product"))
        (should (equal competitive-companion--current-time-limit 2000))
        (should (equal competitive-companion--original-test-count 2)))
      (should (cc-integration-header-p buffer "Problem: 'ABC086A - Product'"
                                       "Judge: AtCoder" "Contest: 'AtCoder Beginners Selection'"))
      (should (equal (cc-test-file-string (expand-file-name ".abc086a/input2.txt" dir)) "1 21\n"))
      (should (equal (cc-test-file-string (expand-file-name ".abc086a/output2.txt" dir)) "Odd\n"))

      ;; Accepted, with the program guessed from the task file.
      (cc-integration-compile "abc086a.cpp" dir "abc086a")
      (should (eq (car (cc-integration-run buffer)) t))

      ;; Wrong answer on the second sample only.
      (let ((run (cc-integration-run buffer (cc-integration-compile "abc086a-wa.cpp" dir "wa"))))
        (should (null (car run)))
        (should-not (string-match-p "^Test 1 " (cdr run)))
        (should (string-match-p "^Test 2 +WA" (cdr run)))
        (should (string-search "Actual Output\nEven\n" (cdr run))))

      ;; Runtime error.
      (let ((run (cc-integration-run buffer (cc-integration-compile "abc086a-rte.cpp" dir "rte"))))
        (should (null (car run)))
        (should (string-match-p "^Test 1 +RTE" (cdr run)))
        (should (string-match-p "^Test 2 +RTE" (cdr run))))

      ;; Time limit exceeded: killed after `competitive-companion-time-limit'.
      (let ((run (cc-integration-run buffer (cc-integration-compile "abc086a-tle.cpp" dir "tle"))))
        (should (null (car run)))
        (should (string-match-p "^Test 1 +TLE +(killed after 1\\.[0-9]s)$" (cdr run))))

      ;; A program that never reads a large input is killed on time too.
      (write-region (make-string 1000000 ?1) nil (expand-file-name ".abc086a/input3.txt" dir))
      (write-region "Odd\n" nil (expand-file-name ".abc086a/output3.txt" dir))
      (let* ((program (cc-integration-compile "abc086a-noread.cpp" dir "noread"))
             (start (float-time))
             (run (cc-integration-run buffer program)))
        (should (string-match-p "^Test 3 +TLE +(killed after 1\\.[0-9]s)$" (cdr run)))
        (should (< (- (float-time) start) 6))))))

;;;; Contests

(ert-deftest cc-integration/atcoder-contest ()
  "All problems of a contest, sent at once, become tasks with their own data.
Filenames are prompted for while the other problems keep arriving."
  (cc-integration-skip-unless-ready)
  (let ((competitive-companion-prompt-task-filename t)
        (depth 0) (max-depth 0) (prompts 0))
    (cc-integration-with-contest (dir port)
      (cl-letf (((symbol-function 'read-file-name)
                 (lambda (_prompt _dir default &rest _)
                   ;; Take a while to answer, as a person would.
                   (cl-incf prompts)
                   (cl-incf depth)
                   (setq max-depth (max depth max-depth))
                   (unwind-protect (accept-process-output nil 0.1)
                     (cl-decf depth))
                   default)))
        (cc-test-capturing-messages messages
          (let ((buffers (cc-integration-fetch
                          port "https://atcoder.jp/contests/abs/tasks" "#main-container table"
                          (lambda ()
                            (let ((buffers (cc-integration-task-buffers dir)))
                              (and (= (length buffers) 11) buffers))))))
            (should (= prompts 11))
            (should (= max-depth 1))
            (should-not (seq-some (lambda (msg) (string-search "Invalid data" msg)) messages))
            (dolist (buffer buffers)
              (let ((name (buffer-local-value 'competitive-companion--current-task-name buffer))
                    (task (buffer-local-value 'competitive-companion--current-task buffer)))
                (should (cc-integration-header-p buffer (format "Problem: '%s'" name)))
                (should (file-exists-p (expand-file-name "input1.txt" task)))))))))))

(provide 'competitive-companion-integration-tests)

;; Local Variables:
;; read-symbol-shorthands: (("cc-" . "competitive-companion-"))
;; End:
;;; competitive-companion-integration-tests.el ends here

;;; competitive-companion-tests.el --- ERT suite for competitive-companion -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Run with `make check' (or `make test') from the repository root.

;;; Code:

(require 'ert)
(require 'competitive-companion)
(require 'competitive-companion-test-helpers)

;;;; Characterization: verdicts and output of `competitive-companion-run-tests'
;;
;; These pin down the behaviour before the test runner is rewritten.

(ert-deftest cc-test-run-tests/accepted ()
  "A program printing the expected output passes."
  (let ((run (cc-test-run :tests '(("1 2\n" . "1 2\n")) :program 'cat)))
    (should (eq (plist-get run :result) t))
    (should (cc-test-message-matching "\\`All tests passed! (slowest: [0-9.]+s)\\'" run))
    (should-not (string-search "Test 1" (plist-get run :output)))))

(ert-deftest cc-test-run-tests/accepted-ignores-surrounding-whitespace ()
  "Trailing spaces and blank lines in the program's output are ignored."
  (let ((run (cc-test-run :tests '(("x" . "1 2\n"))
                          :program "printf '1 2  \\n\\n'")))
    (should (eq (plist-get run :result) t))))

(ert-deftest cc-test-run-tests/wrong-answer ()
  "A wrong output fails and shows up in the output buffer."
  (let* ((run (cc-test-run :tests '(("1\n" . "2\n")) :program 'cat))
         (output (plist-get run :output)))
    (should (null (plist-get run :result)))
    (should (member "Some tests failed." (plist-get run :messages)))
    (should (string-search "Failed Test Results: (A. Test)" output))
    (should (string-match-p "Test 1 +WA" output))
    (should (string-search "Expected Output\n2\n" output))
    (should (string-search "Actual Output\n1\n" output))))

(ert-deftest cc-test-run-tests/only-failed-tests-are-shown ()
  "Accepted tests are left out of the output buffer."
  (let* ((run (cc-test-run :tests '(("1\n" . "1\n") ("2\n" . "3\n")) :program 'cat))
         (output (plist-get run :output)))
    (should-not (string-search "Test 1" output))
    (should (string-match-p "Test 2 +WA" output))))

(ert-deftest cc-test-run-tests/killed-by-signal-is-rte ()
  (let ((run (cc-test-run :tests '(("1\n" . "1\n")) :program 'segv)))
    (should (null (plist-get run :result)))
    (should (string-match-p "Test 1 +RTE" (plist-get run :output)))))

(ert-deftest cc-test-run-tests/nonzero-exit-is-rte ()
  (let ((run (cc-test-run :tests '(("1\n" . "1\n")) :program 'exit3)))
    (should (null (plist-get run :result)))
    (should (string-match-p "Test 1 +RTE" (plist-get run :output)))))

(ert-deftest cc-test-run-tests/stderr-merged-by-default ()
  "Without `competitive-companion-separate-stderr', stderr joins stdout."
  (let ((output (plist-get (cc-test-run :tests '(("" . "nope\n")) :program 'stderr)
                           :output)))
    (should (string-search "Actual Output\nout\nerr\n" output))
    (should-not (string-search "[stderr]" output))))

(ert-deftest cc-test-run-tests/stderr-separated ()
  "With `competitive-companion-separate-stderr', stderr has its own section."
  (let ((output (plist-get (cc-test-run :tests '(("" . "nope\n"))
                                        :program 'stderr
                                        :separate-stderr t)
                           :output)))
    (should (string-search "Actual Output\nout\n" output))
    (should (string-search "Actual Output [stderr]\nerr\n" output))))

(ert-deftest cc-test-run-tests/stderr-does-not-affect-verdict ()
  "Only stdout is judged, but the user is warned that stderr was used."
  (dolist (separate '(nil t))
    (let ((run (cc-test-run :tests '(("" . "out\n"))
                            :program 'stderr
                            :separate-stderr separate)))
      (should (eq (plist-get run :result) t))
      (should (cc-test-message-matching
               "\\`All tests passed! (slowest: [0-9.]+s) \\[stderr IS BEING USED\\]\\'"
               run)))))

(ert-deftest cc-test-run-tests/empty-test-case-is-shown ()
  "An empty test case, as `+' creates it, always shows up."
  (let ((run (cc-test-run :tests '(("" . "")) :program 'cat)))
    (should (null (plist-get run :result)))
    (should (string-match-p "Test 1 +WA" (plist-get run :output)))))

(ert-deftest cc-test-run-tests/input-is-fed-on-stdin ()
  "The whole input file reaches the program's stdin."
  (let* ((input (mapconcat #'number-to-string (number-sequence 1 5000) "\n"))
         (run (cc-test-run :tests `((,input . ,(concat input "\n"))) :program 'cat)))
    (should (eq (plist-get run :result) t))))

;;;; Time limits

(ert-deftest cc-test-hard-limit/disabled ()
  (let ((competitive-companion-time-limit nil))
    (should (null (competitive-companion--hard-limit 2000)))
    (should (null (competitive-companion--hard-limit nil)))))

(ert-deftest cc-test-hard-limit/fixed-seconds ()
  "A fixed limit ignores both the judge's limit and the minimum."
  (let ((competitive-companion-time-limit 7)
        (competitive-companion-minimum-time-limit 10))
    (should (= (competitive-companion--hard-limit 2000) 7))
    (should (= (competitive-companion--hard-limit nil) 7))))

(ert-deftest cc-test-hard-limit/factor-of-judge-limit ()
  (let ((competitive-companion-time-limit '(factor . 3))
        (competitive-companion-minimum-time-limit 5))
    (should (= (competitive-companion--hard-limit 2000) 6))
    (should (= (competitive-companion--hard-limit 2500) 7.5))))

(ert-deftest cc-test-hard-limit/factor-respects-minimum ()
  (let ((competitive-companion-time-limit '(factor . 3))
        (competitive-companion-minimum-time-limit 5))
    (should (= (competitive-companion--hard-limit 1000) 5))))

(ert-deftest cc-test-hard-limit/factor-without-judge-limit ()
  "When the judge's limit is unknown, the minimum is used."
  (let ((competitive-companion-time-limit '(factor . 3))
        (competitive-companion-minimum-time-limit 5))
    (should (= (competitive-companion--hard-limit nil) 5))))

(ert-deftest cc-test-slow-test-delay/defaults-to-judge-limit ()
  (let ((competitive-companion-slow-test-delay nil))
    (should (= (competitive-companion--slow-test-delay 1500) 1.5))
    (should (= (competitive-companion--slow-test-delay nil) 2))))

(ert-deftest cc-test-slow-test-delay/explicit ()
  (let ((competitive-companion-slow-test-delay 0.5))
    (should (= (competitive-companion--slow-test-delay 1500) 0.5))
    (should (= (competitive-companion--slow-test-delay nil) 0.5))))

(ert-deftest cc-test-process-data/stores-time-limit ()
  "Fetching a task keeps the judge's time limit in the task buffer."
  (let* ((dir (file-name-as-directory (make-temp-file "cc-test-" t)))
         (competitive-companion--contest-directory dir)
         (competitive-companion--prompt-task-filename nil)
         (competitive-companion-task-major-mode 'fundamental-mode)
         (competitive-companion-file-generation-hook nil)
         (competitive-companion-last-problem-data nil)
         buffer)
    (unwind-protect
        (progn
          (competitive-companion--process-data
           `((name . "A. Sum") (group . "Codeforces - Round 1")
             (timeLimit . 2000) (memoryLimit . 256)
             (tests . [((input . "1 2\n") (output . "3\n"))])))
          (setq buffer (get-file-buffer (expand-file-name "a.txt" dir)))
          (should buffer)
          (should (equal (buffer-local-value 'competitive-companion--current-time-limit buffer)
                         2000)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest cc-test-run-tests/output-buffer-keeps-time-limit ()
  "The time limit reaches the output buffer and survives a revert."
  (let ((run (cc-test-run
              :tests '(("1\n" . "2\n")) :program 'cat :time-limit 2000
              :after (lambda ()
                       (let ((before competitive-companion--current-time-limit))
                         (revert-buffer)
                         (list before competitive-companion--current-time-limit))))))
    (should (equal (plist-get run :after) '(2000 2000)))))

;;;; Running a single program: `competitive-companion--execute'

(ert-deftest cc-test-execute/normal-exit ()
  (let ((run (cc-test-execute 'cat "1 2\n")))
    (should (eq (plist-get run :status) 'exit))
    (should (eql (plist-get run :code) 0))
    (should (equal (plist-get run :stdout) "1 2\n"))
    (should (equal (plist-get run :stderr) ""))
    (should (< 0 (plist-get run :elapsed) 1))))

(ert-deftest cc-test-execute/exit-code ()
  (let ((run (cc-test-execute 'exit3 "")))
    (should (eq (plist-get run :status) 'exit))
    (should (eql (plist-get run :code) 3))))

(ert-deftest cc-test-execute/killed-by-signal ()
  (should (eq (plist-get (cc-test-execute 'segv "") :status) 'signal)))

(ert-deftest cc-test-execute/timeout ()
  (let ((run (cc-test-execute 'hang "" :hard-limit 0.3)))
    (should (eq (plist-get run :status) 'timeout))
    (should (<= 0.3 (plist-get run :elapsed) 1.0))
    (should-not (cc-test-leftovers))))

(ert-deftest cc-test-execute/no-limit-until-quit ()
  "Without a hard limit, \\[keyboard-quit] stops the program."
  (let ((run (cc-test-execute 'hang "" :quit-after 0.3)))
    (should (eq (plist-get run :status) 'aborted))
    (should (<= 0.3 (plist-get run :elapsed) 1.0))
    (should-not (cc-test-leftovers))))

(ert-deftest cc-test-execute/separate-stderr ()
  (let ((run (cc-test-execute 'stderr "")))
    (should (equal (plist-get run :stdout) "out\n"))
    (should (equal (plist-get run :stderr) "err\n"))))

(ert-deftest cc-test-execute/merged-stderr-keeps-order ()
  (let ((run (cc-test-execute 'stderr "" :merge-stderr t)))
    (should (equal (plist-get run :stdout) "out\nerr\n"))))

(ert-deftest cc-test-execute/large-output-is-complete ()
  (should (= (length (plist-get (cc-test-execute 'bigout "") :stdout)) 2000000)))

(ert-deftest cc-test-execute/program-not-reading-stdin ()
  (let ((run (cc-test-execute 'noread "1 2 3\n")))
    (should (eq (plist-get run :status) 'exit))
    (should (equal (plist-get run :stdout) "x\n"))))

(ert-deftest cc-test-execute/reports-slow-test ()
  (let ((run (cc-test-execute 'slow "" :warn-after 0.1)))
    (should (eq (plist-get run :status) 'exit))
    (should (seq-some (lambda (msg) (string-prefix-p "Test 1 still running" msg))
                      (plist-get run :messages)))))

(ert-deftest cc-test-execute/quiet-when-fast ()
  (let ((run (cc-test-execute 'cat "" :warn-after 0.5)))
    (should-not (seq-some (lambda (msg) (string-search "still running" msg))
                          (plist-get run :messages)))))

(ert-deftest cc-test-execute/cleans-up ()
  (dolist (program '(cat stderr exit3 segv))
    (cc-test-execute program "1\n")
    (should-not (cc-test-leftovers))))

;;;; Verdicts from `competitive-companion--execute' results

(ert-deftest cc-test-run-tests/timeout-is-tle ()
  (let* ((competitive-companion-time-limit 0.3)
         (run (cc-test-run :tests '(("1\n" . "1\n")) :program 'hang)))
    (should (null (plist-get run :result)))
    (should (string-match-p "Test 1 +TLE" (plist-get run :output)))))

(ert-deftest cc-test-run-tests/judge-limit-sets-hard-limit ()
  "With a factor, the judge's limit decides when the program is killed."
  (let* ((competitive-companion-time-limit '(factor . 1))
         (competitive-companion-minimum-time-limit 0)
         (start (float-time))
         (run (cc-test-run :tests '(("1\n" . "1\n")) :program 'hang :time-limit 300)))
    (should (string-match-p "Test 1 +TLE" (plist-get run :output)))
    (should (< (- (float-time) start) 1.5))))

(ert-deftest cc-test-run-program/records-time ()
  (cc-test-with-temp-dir dir
    (let ((command (cc-test-make-program dir 'cat))
          (input-file (expand-file-name "input1.txt" dir))
          (output-file (expand-file-name "output1.txt" dir)))
      (write-region "1\n" nil input-file)
      (write-region "1\n" nil output-file)
      (let ((result (competitive-companion--run-program command 1 input-file output-file)))
        (should (eq (competitive-companion-test-result-verdict result) 'ac))
        (should (natnump (competitive-companion-test-result-time-ms result)))))))

(ert-deftest cc-test-run-tests/runs-passing-test-once ()
  "Accepted tests run once, also when stderr is merged into the output."
  (cc-test-with-temp-dir dir
    (let ((counter (expand-file-name "count" dir)))
      (cc-test-run :tests '(("1\n" . "1\n"))
                   :program (cc-test-counting-program counter "exec cat"))
      (should (= (cc-test-count-lines counter) 1)))))

(ert-deftest cc-test-run-tests/runs-timed-out-test-once ()
  "A hanging program is not waited for twice."
  (cc-test-with-temp-dir dir
    (let ((counter (expand-file-name "count" dir))
          (competitive-companion-time-limit 0.3))
      (cc-test-run :tests '(("1\n" . "1\n"))
                   :program (cc-test-counting-program counter "exec sleep 100"))
      (should (= (cc-test-count-lines counter) 1)))))


;;;; Running all test cases

(defconst cc-test-three-tests '(("1\n" . "1\n") ("2\n" . "2\n") ("3\n" . "3\n")))

(ert-deftest cc-test-run-tests/hanging-test-does-not-stop-others ()
  (cc-test-with-temp-dir dir
    (let* ((log (expand-file-name "log" dir))
           (competitive-companion-time-limit 0.3)
           (run (cc-test-run :tests cc-test-three-tests
                             :program (cc-test-dispatching-program log "2")))
           (output (plist-get run :output)))
      (should (null (plist-get run :result)))
      (should (string-match-p "^Test 2 +TLE" output))
      (should-not (string-search "Test 1" output))
      (should-not (string-search "Test 3" output))
      (should (equal (cc-test-file-string log) "1\n2\n3\n")))))

(ert-deftest cc-test-run-tests/quit-stops-the-run ()
  "\\[keyboard-quit] aborts the running test and skips the rest."
  (cc-test-with-temp-dir dir
    (let* ((log (expand-file-name "log" dir))
           (competitive-companion-time-limit nil)
           (run (cc-test-run :tests cc-test-three-tests
                             :program (cc-test-dispatching-program log "2")
                             :quit-after 0.3))
           (output (plist-get run :output)))
      (should (null (plist-get run :result)))
      (should (string-search "aborted at test 2" output))
      (should (string-match-p "^Test 2 +TLE +(aborted after 0\\.[3-9]s)$" output))
      (should (cc-test-message-matching "\\`Aborted at test 2" run))
      (should (equal (cc-test-file-string log) "1\n2\n")))))

(ert-deftest cc-test-run-tests/reports-progress ()
  (let ((run (cc-test-run :tests cc-test-three-tests :program 'cat)))
    (should (cc-test-message-matching "\\`Running test 2/3" run))))

(ert-deftest cc-test-run-tests/numeric-order ()
  (cc-test-with-temp-dir dir
    (let* ((log (expand-file-name "log" dir))
           (tests (mapcar (lambda (i) (let ((s (format "%d\n" i))) (cons s s)))
                          (number-sequence 1 11))))
      (cc-test-run :tests tests :program (cc-test-dispatching-program log "none"))
      (should (equal (cc-test-file-string log)
                     (mapconcat (lambda (i) (format "%d\n" i)) (number-sequence 1 11) ""))))))

;;;; Display

(ert-deftest cc-test-display/failed-heading-shows-time ()
  (let ((run (cc-test-run :tests '(("1\n" . "2\n")) :program 'cat)))
    (should (string-match-p "^Test 1 +WA +([0-9]+\\.[0-9][0-9]s)$" (plist-get run :output)))))

(ert-deftest cc-test-display/tle-heading-shows-when-killed ()
  (let* ((competitive-companion-time-limit 0.3)
         (run (cc-test-run :tests '(("1\n" . "1\n")) :program 'hang)))
    (should (string-match-p "^Test 1 +TLE +(killed after 0\\.[3-9]s)$" (plist-get run :output)))))

(ert-deftest cc-test-display/success-reports-slowest-test ()
  (let ((run (cc-test-run :tests '(("1\n" . "1\n") ("2\n" . "2\n"))
                          :program "sleep 0.2; exec cat")))
    (should (cc-test-message-matching "slowest: 0\\.[2-9][0-9]s" run))))

(provide 'competitive-companion-tests)

;; Local Variables:
;; read-symbol-shorthands: (("cc-" . "competitive-companion-"))
;; End:
;;; competitive-companion-tests.el ends here

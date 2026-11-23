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
    (should (member "All tests passed!" (plist-get run :messages)))
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
      (should (member "All tests passed! [stderr IS BEING USED]"
                      (plist-get run :messages))))))

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

(provide 'competitive-companion-tests)

;; Local Variables:
;; read-symbol-shorthands: (("cc-" . "competitive-companion-"))
;; End:
;;; competitive-companion-tests.el ends here

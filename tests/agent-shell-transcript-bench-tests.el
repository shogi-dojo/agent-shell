;;; agent-shell-transcript-bench-tests.el --- Tests for the transcript benchmark -*- lexical-binding: t; -*-

(require 'ert)
(load (expand-file-name "agent-shell-transcript-bench.el"
                        (file-name-directory
                         (or load-file-name buffer-file-name)))
      nil t)

(declare-function agent-shell-bench-build "agent-shell-transcript-bench")
(declare-function agent-shell-bench-run "agent-shell-transcript-bench")
(declare-function agent-shell-bench-format "agent-shell-transcript-bench")
(declare-function agent-shell-bench--stats "agent-shell-transcript-bench")

;;; Code:

(ert-deftest agent-shell-transcript-bench-run-measures-every-phase-test ()
  "A small history still produces samples for each benchmark phase."
  (let ((result (agent-shell-bench-run 0.02 :max-seconds 30)))
    (should (>= (plist-get result :bytes) (floor (* 0.02 1024 1024))))
    (should (> (plist-get result :turns) 0))
    (should-not (plist-get result :truncated))
    (dolist (phase '(:parse :insert :stream :toggle :typing))
      (should (> (plist-get (plist-get result phase) :count) 0)))))

(ert-deftest agent-shell-transcript-bench-build-stops-at-time-cap-test ()
  "A history that cannot finish within the cap is reported as truncated."
  (with-temp-buffer
    (let ((result (agent-shell-bench-build (* 100 1024 1024)
                                           :max-seconds 0)))
      (should (plist-get result :truncated))
      (should (< (plist-get result :bytes) (* 100 1024 1024))))))

(ert-deftest agent-shell-transcript-bench-format-puts-header-before-rows-test ()
  "The markdown report starts with its header row, then the separator."
  (let* ((report (agent-shell-bench-format
                  (list (agent-shell-bench-run 0.02 :max-seconds 30))))
         (lines (split-string report "\n")))
    (should (string-prefix-p "| size |" (nth 0 lines)))
    (should (string-prefix-p "| --- |" (nth 1 lines)))
    (should (string-prefix-p "| 0.02 MB |" (nth 2 lines)))))

(ert-deftest agent-shell-transcript-bench-stats-summarizes-samples-test ()
  "Percentiles come from the sorted samples."
  (let ((stats (agent-shell-bench--stats '(5.0 1.0 3.0 2.0 4.0))))
    (should (= (plist-get stats :count) 5))
    (should (= (plist-get stats :p50) 3.0))
    (should (= (plist-get stats :max) 5.0))
    (should (= (plist-get stats :mean) 3.0)))
  (should (= (plist-get (agent-shell-bench--stats nil) :count) 0)))

(provide 'agent-shell-transcript-bench-tests)

;;; agent-shell-transcript-bench-tests.el ends here

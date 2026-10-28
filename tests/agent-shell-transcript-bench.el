;;; agent-shell-transcript-bench.el --- Transcript renderer benchmark -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Measures how the current transcript renderer behaves as a session grows,
;; so changes to it can be compared against a baseline.  It builds
;; deterministic synthetic histories of a target size (user prompts, agent
;; messages with markdown and fenced code, and large collapsed tool output)
;; and times each phase separately:
;;
;;   parse    `agent-shell-markdown-convert' on each agent message
;;   insert   `agent-shell-ui-update-fragment' creating each block
;;   stream   appending small chunks to the last block of a large buffer
;;   toggle   collapsing and expanding blocks in the middle of the buffer
;;   typing   self-inserting at the end of the buffer
;;
;; Typing also includes redisplay when run in a graphical frame; in batch
;; mode it measures insertion and its hooks only.
;;
;; Run it headless:
;;
;;   emacs --batch -Q -L . -L ../acp -L ../shell-maker -L ../compat \
;;     -l tests/agent-shell-transcript-bench.el -f agent-shell-bench-batch
;;
;; Environment variables:
;;
;;   AGENT_SHELL_BENCH_SIZES_MB     comma-separated sizes (default "0.5,10")
;;   AGENT_SHELL_BENCH_MAX_SECONDS  cap per size for building (default 120)
;;   AGENT_SHELL_BENCH_OUTPUT       also write the report to this file
;;
;; Or call `agent-shell-bench' from a graphical Emacs to include redisplay.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'agent-shell-ui)
(require 'agent-shell-markdown)

(defconst agent-shell-bench--prose
  (concat "The renderer should keep **typing** and scrolling responsive while "
          "a long session streams in.  Here is `inline code`, a "
          "[link](https://example.com/page), and some Unicode: "
          "こんにちは 🌍 — Ελληνικά.\n\n")
  "One paragraph of markdown used to fill agent messages.")

(defun agent-shell-bench--code-block (lines)
  "Return a fenced code block with LINES lines."
  (concat "```elisp\n"
          (mapconcat
           (lambda (index)
             (format "(defun bench-fn-%d (x) (let ((y (* x %d))) (message \"value %%d\" y) y))"
                     index index))
           (number-sequence 1 lines) "\n")
          "\n```\n\n"))

(defun agent-shell-bench--agent-body (index)
  "Return a markdown agent message for turn INDEX."
  (concat (format "## Step %d\n\n" index)
          (apply #'concat (make-list 6 agent-shell-bench--prose))
          (agent-shell-bench--code-block 20)
          "- first item\n- second item with `code`\n- third item\n"))

(defun agent-shell-bench--tool-body (index)
  "Return a large plain tool output for turn INDEX."
  (mapconcat
   (lambda (line)
     (format "line %d of tool output for call %d: %s" line index
             (make-string 60 ?x)))
   (number-sequence 1 80) "\n"))

(defmacro agent-shell-bench--measure (samples &rest body)
  "Run BODY, push its duration in milliseconds onto SAMPLES, return its value."
  (declare (indent 1))
  (let ((start (make-symbol "start"))
        (value (make-symbol "value")))
    `(let* ((,start (float-time))
            (,value (progn ,@body)))
       (push (* 1000.0 (- (float-time) ,start)) ,samples)
       ,value)))

(defun agent-shell-bench--stats (samples)
  "Return a plist summarizing SAMPLES, a list of durations in milliseconds."
  (if (null samples)
      (list :count 0)
    (let* ((sorted (vconcat (sort (copy-sequence samples) #'<)))
           (count (length sorted))
           (pick (lambda (fraction)
                   (aref sorted (min (1- count)
                                     (floor (* fraction count)))))))
      (list :count count
            :mean (/ (apply #'+ samples) count)
            :p50 (funcall pick 0.50)
            :p95 (funcall pick 0.95)
            :p99 (funcall pick 0.99)
            :max (aref sorted (1- count))))))

(cl-defun agent-shell-bench-build (target-bytes &key max-seconds)
  "Fill the current buffer with synthetic turns up to TARGET-BYTES characters.
Stop early after MAX-SECONDS and mark the result truncated.  Return a plist
with the turn count, final size, elapsed time, and parse and insert stats."
  (let ((parse-samples nil)
        (insert-samples nil)
        (turn 0)
        (truncated nil)
        (start (float-time))
        (gc-start (list gc-elapsed gcs-done)))
    (while (and (< (buffer-size) target-bytes) (not truncated))
      (cl-incf turn)
      (let* ((namespace (format "bench-%d" turn))
             (parsed (agent-shell-bench--measure parse-samples
                       (agent-shell-markdown-convert
                        (agent-shell-bench--agent-body turn)))))
        (goto-char (point-max))
        (agent-shell-bench--measure insert-samples
          (agent-shell-ui-update-fragment
           (agent-shell-ui-make-fragment-model
            :namespace-id namespace :block-id "user"
            :label-left "You" :body (format "Question number %d" turn))
           :create-new t :expanded t :no-undo t))
        (agent-shell-bench--measure insert-samples
          (agent-shell-ui-update-fragment
           (agent-shell-ui-make-fragment-model
            :namespace-id namespace :block-id "agent"
            :label-left "Agent" :body parsed)
           :create-new t :expanded t :no-undo t))
        (agent-shell-bench--measure insert-samples
          (agent-shell-ui-update-fragment
           (agent-shell-ui-make-fragment-model
            :namespace-id namespace :block-id "tool"
            :label-left "Tool" :label-right "completed"
            :body (agent-shell-bench--tool-body turn))
           :create-new t :no-undo t)))
      (when (and max-seconds (> (- (float-time) start) max-seconds))
        (setq truncated t)))
    (list :turns turn
          :bytes (buffer-size)
          :truncated truncated
          :build-seconds (- (float-time) start)
          :gc-seconds (- gc-elapsed (nth 0 gc-start))
          :gcs (- gcs-done (nth 1 gc-start))
          :parse (agent-shell-bench--stats parse-samples)
          :insert (agent-shell-bench--stats insert-samples))))

(defun agent-shell-bench-stream (chunk-count)
  "Append CHUNK-COUNT small chunks to a new block at the end of the buffer.
Return the per-chunk latency stats."
  (let ((samples nil))
    (goto-char (point-max))
    (dotimes (index chunk-count)
      (agent-shell-bench--measure samples
        (agent-shell-ui-update-fragment
         (agent-shell-ui-make-fragment-model
          :namespace-id "bench-stream" :block-id "live"
          :label-left "Agent"
          :body (format "streamed chunk %d with some words in it. " index))
         :append (> index 0) :create-new (= index 0)
         :expanded t :no-undo t)))
    (agent-shell-bench--stats samples)))

(defun agent-shell-bench--next-fragment (position)
  "Return the first position at or after POSITION that starts a fragment."
  (let ((found nil))
    (while (and (not found) position (< position (point-max)))
      (if (get-text-property position 'agent-shell-ui-state)
          (setq found position)
        (setq position (next-single-property-change
                        position 'agent-shell-ui-state nil (point-max)))))
    found))

(defun agent-shell-bench-toggle (count)
  "Collapse then expand COUNT blocks spread through the buffer.
Return the per-toggle latency stats."
  (let ((samples nil)
        (step (max 1 (/ (buffer-size) (1+ count)))))
    (dotimes (index count)
      (when-let* ((position (agent-shell-bench--next-fragment
                             (min (point-max) (* step (1+ index))))))
        (goto-char position)
        (dotimes (_ 2)
          (agent-shell-bench--measure samples
            (agent-shell-ui--toggle-fragment-at-point)))))
    (agent-shell-bench--stats samples)))

(defun agent-shell-bench-typing (count)
  "Self-insert COUNT characters at the end of the buffer.
Redisplay after each key in a graphical frame.  Return the per-key latency
stats, with the insertion and redisplay parts also reported separately under
`:insert' and `:redisplay'."
  (let ((totals nil)
        (inserts nil)
        (redisplays nil)
        (graphic (display-graphic-p)))
    (goto-char (point-max))
    ;; Blocks end read-only and sticky, so give the input a prompt of its own,
    ;; the way the shell does.
    (let ((inhibit-read-only t))
      (insert (propertize "\n> " 'read-only t
                          'rear-nonsticky '(read-only))))
    (dotimes (_ count)
      (let ((start (float-time))
            (insert-ms nil)
            (redisplay-ms 0.0))
        (let ((last-command-event ?x))
          (self-insert-command 1))
        (setq insert-ms (* 1000.0 (- (float-time) start)))
        (when graphic
          (let ((redisplay-start (float-time)))
            (redisplay t)
            (setq redisplay-ms (* 1000.0 (- (float-time) redisplay-start))))
          (push redisplay-ms redisplays))
        (push insert-ms inserts)
        (push (+ insert-ms redisplay-ms) totals)))
    (append (agent-shell-bench--stats totals)
            (list :insert (agent-shell-bench--stats inserts)
                  :redisplay (agent-shell-bench--stats redisplays)))))

(cl-defun agent-shell-bench-run (size-mb &key max-seconds)
  "Benchmark one history of SIZE-MB megabytes and return a result plist."
  (with-temp-buffer
    (let ((window (and (display-graphic-p)
                       (progn (switch-to-buffer (current-buffer))
                              (selected-window)))))
      (ignore window)
      (let* ((target (floor (* size-mb 1024 1024)))
             (build (agent-shell-bench-build target :max-seconds max-seconds)))
        (append (list :size-mb size-mb
                      :redisplay (and (display-graphic-p) t))
                build
                (list :stream (agent-shell-bench-stream 100)
                      :toggle (agent-shell-bench-toggle 10)
                      :typing (agent-shell-bench-typing 200)))))))

(defun agent-shell-bench--cell (stats key)
  "Return KEY from STATS formatted in milliseconds."
  (if-let* ((value (plist-get stats key)))
      (format "%.2f" value)
    "-"))

(defun agent-shell-bench-format (results)
  "Format RESULTS, a list of `agent-shell-bench-run' plists, as markdown."
  (let ((lines (list "| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |"
                     "| size | turns | built | build s | gc s | typing p95 | redisplay p95 | stream p95 | toggle p95 | insert p95 | parse p95 |")))
    (dolist (result results)
      (push (format "| %s MB | %d | %.2f MB%s | %.1f | %.1f | %s | %s | %s | %s | %s | %s |"
                    (plist-get result :size-mb)
                    (plist-get result :turns)
                    (/ (plist-get result :bytes) 1048576.0)
                    (if (plist-get result :truncated) " (capped)" "")
                    (plist-get result :build-seconds)
                    (plist-get result :gc-seconds)
                    (agent-shell-bench--cell (plist-get result :typing) :p95)
                    (agent-shell-bench--cell
                     (plist-get (plist-get result :typing) :redisplay) :p95)
                    (agent-shell-bench--cell (plist-get result :stream) :p95)
                    (agent-shell-bench--cell (plist-get result :toggle) :p95)
                    (agent-shell-bench--cell (plist-get result :insert) :p95)
                    (agent-shell-bench--cell (plist-get result :parse) :p95))
            lines))
    (concat (mapconcat #'identity (nreverse lines) "\n")
            "\n\nSizes count characters.  Latencies are milliseconds at the 95th percentile.  Typing "
            (if (and results (plist-get (car results) :redisplay))
                "includes redisplay.\n"
              "excludes redisplay (batch mode).\n"))))

(defun agent-shell-bench--sizes ()
  "Return the sizes in megabytes requested through the environment."
  (mapcar #'string-to-number
          (split-string (or (getenv "AGENT_SHELL_BENCH_SIZES_MB") "0.5,10")
                        "," t "[ \t]+")))

(defun agent-shell-bench--max-seconds ()
  "Return the build time cap in seconds requested through the environment."
  (string-to-number (or (getenv "AGENT_SHELL_BENCH_MAX_SECONDS") "120")))

(defun agent-shell-bench-batch ()
  "Run the benchmark for the configured sizes and print a markdown report."
  (let* ((results (mapcar (lambda (size)
                            (agent-shell-bench-run
                             size :max-seconds (agent-shell-bench--max-seconds)))
                          (agent-shell-bench--sizes)))
         (report (agent-shell-bench-format results)))
    (princ report)
    (when-let* ((output (getenv "AGENT_SHELL_BENCH_OUTPUT")))
      (with-temp-file output (insert report)))))

(defun agent-shell-bench ()
  "Run the benchmark from a graphical Emacs and show the report."
  (interactive)
  (let ((report (agent-shell-bench-format
                 (mapcar (lambda (size)
                           (agent-shell-bench-run
                            size :max-seconds (agent-shell-bench--max-seconds)))
                         (agent-shell-bench--sizes)))))
    (with-current-buffer (get-buffer-create "*agent-shell-bench*")
      (erase-buffer)
      (insert report)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(provide 'agent-shell-transcript-bench)

;;; agent-shell-transcript-bench.el ends here

(defvar erf-entries ()
  "List of the entries.")

(defvar erf-last-update 0
  "The last time the buffer was redrawn in epoch seconds.")

(defvar erf-feed-title-width 30 "Width of the title text.")

(defvar erf-count-width 5 "Width of the count.")


(defun erf-print-line-function (line)
  (let* ((feed (car line))
         (n (cadr line))
         (last-update (caddr line))
         (feed-column (elfeed-format-column
                       feed erf-feed-title-width :left))
         (n-column (elfeed-format-column (format "%s" n)
                                         erf-count-width :right))
         (last-update-column (erf-date last-update 11)))
    (insert (propertize feed-column 'face 'elfeed-search-feed-face) " ")
    (insert (propertize n-column 'face 'elfeed-search-tag-face) " ")
    (insert (propertize last-update-column
                          'face 'elfeed-search-date-face) " ")))
  
(defun erf-date (epoch-date date-width)
  (let* ((elfeed-search-date-format (list "%a %b-%d" date-width :left))
         (date       epoch-date)
         (date-str   (elfeed-search-format-date date))
         (delta      (time-subtract (float-time) date))
         ;; (delta-days (decode-time delta nil 'integer))
         (delta-days (- (time-to-days (float-time)) (time-to-days date)))
         (time-str   (cond ((equal delta-days 0) "Today")
                           ((equal delta-days 1) "Yesterday")
                           ((format "%2s days ago" delta-days)))))
    (concat (format "%11s" time-str) ", " date-str)))

(defun erf-get-feeds-titles (entries)
  (seq-uniq (seq-map
             (lambda (entry)
               (let ((feed (elfeed-entry-feed entry)))
               (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
             entries)))

(defun erf-feeds-occurrences-alist (entries)
  "Return alist (feed-title counter most-recent) for a list of ENTRIES.
Sort descending."
  (let ((occurrences))
    (dolist (occurrence-title (erf-get-feeds-titles entries))
      (push 
       (cl-loop for entry in entries
                for feed-title = (elfeed-meta (elfeed-entry-feed entry) :title)
                for date = (elfeed-entry-date entry)
                if (string= feed-title occurrence-title)
                  count t into p
                  and maximize date into latest-update
                finally return (list occurrence-title p latest-update))
       occurrences))
    (seq-sort (lambda (a b) (> (cadr a) (cadr b))) occurrences)))

;; (erf-feeds-occurrences-alist entries)

(defun erf-summary-buffer ()
  (get-buffer-create "*elfeed-erf-summary*"))

(defun erf-summary-update (&optional force)
  "Update the erf-summary buffer listing to match the database.
When FORCE is non-nil, redraw even when the database hasn't changed."
  (interactive)
  (erf-entries--update-list)
  (with-current-buffer (erf-summary-buffer)
    (when (or force (and (not elfeed-search-filter-active)
                         (< erf-last-update (elfeed-db-last-update))))
      (elfeed-save-excursion
        (let ((inhibit-read-only t)
              (standard-output (current-buffer))
              (occurrences (erf-feeds-occurrences-alist erf-entries)))
          (erase-buffer)
          (dolist (occurrence-line occurrences)
            (funcall #'erf-print-line-function occurrence-line)
            (insert "\n"))
          (setf erf-last-update (float-time))))
      (when (zerop (buffer-size))
        ;; If nothing changed, force a header line update
        (force-mode-line-update))
      )))

(erf-summary-update t)

(defun erf-entries--update-list ()
  "Update `erf-entries' list."
  (let* ((filter (elfeed-search-parse-filter elfeed-search-filter))
         (head (list nil))
         (tail head)
         (count 0))
    (if elfeed-search-compile-filter
        ;; Force lexical bindings regardless of the current
        ;; buffer-local value. Lexical scope uses the faster
        ;; stack-ref opcode instead of the traditional varref opcode.
        (let ((lexical-binding t)
              (func (byte-compile (elfeed-search-compile-filter filter))))
          (with-elfeed-db-visit (entry feed)
            (when (funcall func entry feed count)
              (setf (cdr tail) (list entry)
                    tail (cdr tail)
                    count (1+ count)))))
      (with-elfeed-db-visit (entry feed)
        (when (elfeed-search-filter filter entry feed count)
          (setf (cdr tail) (list entry)
                tail (cdr tail)
                count (1+ count)))))
    ;; Determine the final list order
    (let ((entries (cdr head)))
      (when elfeed-search-sort-function
        (setf entries (sort entries elfeed-search-sort-function)))
      (when (eq elfeed-sort-order 'ascending)
        (setf entries (nreverse entries)))
      (setf erf-entries
            entries))))

(defun erf-get-feeds-titles (entries)
  (seq-uniq (seq-map
             (lambda (entry)
               (let ((feed (elfeed-entry-feed entry)))
               (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
             entries)))

(defun erf-feeds-occurrences-alist (entries)
  "Return alist (feed-title counter) for a list of ENTRIES, sorted descending."
  (let ((occurrences))
    (dolist (occurrence-title (erf-get-feeds-titles entries))
      (push 
       (cl-loop for entry in entries
                for feed-title = (elfeed-meta (elfeed-entry-feed entry) :title)
                count (string= feed-title occurrence-title) into p
                finally return (list occurrence-title p)) occurrences))
    (seq-sort (lambda (a b) (> (cadr a) (cadr b))) occurrences)))

(defun erf-summary-buffer ()
  (get-buffer-create "*elfeed-erf-summary*"))

(defvar erf-feed-title-width 30 "Width of the title text.")

(defvar erf-count-width 5 "Width of the count.")

(defun erf-summary-update (&optional force)
  "Update the erf-summary buffer listing to match the database.
When FORCE is non-nil, redraw even when the database hasn't changed."
  (interactive)
  (with-current-buffer (erf-summary-buffer)
    (when (or force (and (not elfeed-search-filter-active)
                         (< erf-last-update (elfeed-db-last-update))))
      (elfeed-save-excursion
        (let ((inhibit-read-only t)
              (standard-output (current-buffer))
              (occurrences (erf-feeds-occurrences-alist entries))) ;;TODO get entries from database?
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

(defun erf-print-line-function (line)
  (let* ((feed (car line))
         (n (cadr line))
         (feed-column (elfeed-format-column
                       feed erf-feed-title-width :left))
         (n-column (elfeed-format-column (format "%s" n)
                                         erf-count-width :right)))
    (insert (propertize feed-column 'face 'elfeed-search-feed-face) " ")
    (insert (propertize n-column 'face 'elfeed-search-date-face) " ")))
  

(defvar erf-last-update 0
  "The last time the buffer was redrawn in epoch seconds.")

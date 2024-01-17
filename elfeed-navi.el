(require 'elfeed)

;;; source data from elfeed-search

(defvar elfeed-navi-entries ()
  "List of the entries displayed in `*elfeed-search*'.

It is a copy of `elfeed-search-entries' refreshed as needed.")

(defvar elfeed-navi-entries-last-update 0
  "The last time the entries list was updated in epoch seconds.")

(defvar elfeed-navi-search-filter-default "@4-days-ago +unread"
  "Default filter for the `*elfeed-navi*' buffer.")

(defvar elfeed-navi-search-filter elfeed-navi-search-filter-default
  "Copy of the active filter in the `*elfeed-search*' buffer.")

(defun elfeed-navi-store-filter ()
  (setq elfeed-navi-search-filter (substring elfeed-search-filter)))

(defun elfeed-navi-entries--update-list ()
  "Update `elfeed-navi-entries' list based on current filter."
  (let* ((filter (elfeed-search-parse-filter elfeed-navi-search-filter))
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
    (setf elfeed-navi-entries (cdr head))
    (setf elfeed-navi-entries-last-update (float-time))))

;;; create summary

(defvar elfeed-navi-aggregator 'feed-title
  "Symbol to specify the aggregation on with compute the summary.

Currently only implemented for default `'feed-title'.")

(defvar elfeed-navi-summary-entries-alist ()
  "List of the summary entries computed on `elfeed-navi-entries'.

The alist is scalable to allow different aggregations methods.
((:aggregator symbol :title str :count int :most-recent epoch))")

(defvar elfeed-navi-summary-entries-alist-last-update 0
  "Last update of the alist, in epoch.")

(defun elfeed-navi-list-aggregator-values (aggregator)
  "List values of AGGREGATOR in `elfeed-navi-entries'."
  (let* ((extract-func (pcase aggregator
                         ('feed-title (lambda (entry)
                                        (let ((feed (elfeed-entry-feed entry)))
                                          (or (elfeed-meta feed :title)
                                              (elfeed-feed-title feed)))))))
         (all-values (seq-map extract-func elfeed-navi-entries))
         (dedup-values (seq-uniq all-values)))
    dedup-values))
 
(defun elfeed-navi-create-summary (aggregator &optional force) 
  "Populate the `elfeed-navi-summary-entries-alist' with AGGREGATOR.

Sort descending for count.
When FORCE not-nil, update list even if the entries-list did not change."
  (when (or force (> elfeed-navi-entries-last-update
                     elfeed-navi-summary-entries-alist-last-update))
  (let ((unsorted))
    (dolist (title (elfeed-navi-list-aggregator-values aggregator))
      ;; TODO adapt for other aggregators like tags
      (push (cl-loop for entry in elfeed-navi-entries
                     for feed = (elfeed-entry-feed entry)
                     for feed-title = (or (elfeed-meta feed :title)
                                          (elfeed-feed-title feed))
                     for date = (elfeed-entry-date entry)
                     if (string= feed-title title)
                     count t into p
                     and maximize date into latest-update
                     finally return (list :aggregator aggregator
                                          :title title
                                          :count p
                                          :most-recent latest-update))
            unsorted))
    (setq elfeed-navi-summary-entries-alist
          (seq-sort (lambda (a b) (> (plist-get a :count)
                                     (plist-get b :count))) unsorted)))
  (setq elfeed-navi-summary-entries-alist-last-update (float-time))))

(defun elfeed-navi-summary-entry-get-title (summary-entry)
  "Get title from the plist SUMMARY-ENTRY."
  (plist-get summary-entry :title))

(defun elfeed-navi-summary-entry-get-count (summary-entry)
  "Get count from the plist SUMMARY-ENTRY and return a string."
  (format "%s" (plist-get summary-entry :count)))

(defun elfeed-navi-summary-entry-get-most-recent (summary-entry)
  "Return a meaningful string for date.

Returning `Today' for today's date, `Yesterday' for yesterday, and
`n days ago' for other dates."
  (let* ((date-epoch (plist-get summary-entry :most-recent))
         ;; (elfeed-search-date-format (list "%a %b-%d" date-width :left))
         (date-str   (elfeed-search-format-date date-epoch))
         (delta      (time-subtract (float-time) date-epoch))
         (delta-days (- (time-to-days (float-time)) (time-to-days date-epoch)))
         (time-str   (cond ((equal delta-days 0) "Today")
                           ((equal delta-days 1) "Yesterday")
                           ((format "%2s days ago" delta-days)))))
    (concat (format "%11s" time-str) ", " date-str)))

;;; rendering results

(defface elfeed-navi-today-face
  '((t :foreground "red1" :weight bold))
  "Marks today.")

(defface elfeed-navi-yesterday-face
  '((t :foreground "orange1"))
  "Marks yesterday.")

(defvar elfeed-navi-buffer-last-update 0
  "The last time the buffer was redrawn in epoch seconds.")

(defvar elfeed-navi-buffer-filter ""
  "Filter with which the buffer is built.")

;; (defvar elfeed-navi-title-width 30
;;   "Width of the title column.")

;; (defvar elfeed-navi-count-width 5
;;   "Width of the count column.")

(defun elfeed-navi-buffer ()
  "Get or create the summary buffer."
  (get-buffer-create "*elfeed-navi*"))

(defun elfeed-navi-line-format (aggregator)
  "Return format string for lines and header, appropriate for AGGREGATOR."
  (pcase aggregator
    ('feed-title "%30s %7s %26s")))

(defun elfeed-navi-title-face (summary-entry)
  "Return face depending on entry age."
  (let ((date (elfeed-navi-summary-entry-get-most-recent summary-entry)))
    (cond ((string-prefix-p "Yesterday" (string-clean-whitespace date))
           'elfeed-navi-yesterday-face)
          ((string-prefix-p "Today" (string-clean-whitespace date))
           'elfeed-navi-today-face)
          (t 'elfeed-search-date-face))))

(defun elfeed-navi-print-line-function (summary-entry aggregator)
  "Print SUMMARY-ENTRY line in the `*elfeed-navi-buffer*'."
  (let* ((title       (elfeed-navi-summary-entry-get-title summary-entry))
         (count   (elfeed-navi-summary-entry-get-count summary-entry))
         (most-recent  (elfeed-navi-summary-entry-get-most-recent summary-entry))
         (date-face (elfeed-navi-title-face summary-entry)))
    (insert
     (format (elfeed-navi-line-format aggregator)
              (propertize title       'face 'elfeed-search-feed-face)
              (propertize count       'face 'elfeed-search-tag-face)
              (propertize most-recent 'face date-face)))))

(defun elfeed-navi-header-line (aggregator)
  "Set the header line appropriate for AGGREGATOR."
  (setq header-line-format
        (format (elfeed-navi-line-format aggregator)
                "Feed" "Count" "Most recent")))

(defun elfeed-navi-buffer-update (&optional force)
  "Update the elfeed-navi buffer listing to match the database.
When FORCE is non-nil, redraw even when the database hasn't changed."
  (interactive)
  ;; update entries, considering filter if any
  (elfeed-navi-entries--update-list)
  ;; update summary entries
  ;; TODO consider other aggregators
  (let ((aggregator 'feed-title))
    (elfeed-navi-create-summary aggregator)
    (with-current-buffer (elfeed-navi-buffer)
      (when (or force
                (and (not elfeed-search-filter-active)
                     (< elfeed-navi-buffer-last-update (elfeed-db-last-update))))
        (elfeed-save-excursion
          (let ((inhibit-read-only t)
                (standard-output (current-buffer))
                (summary-entries elfeed-navi-summary-entries-alist))
            (erase-buffer)
            (setq elfeed-navi-buffer-filter elfeed-navi-search-filter)
            (elfeed-navi-header-line aggregator)
            ;; (insert (format "Current filter: %s\n" elfeed-navi-buffer-filter))
            ;; (insert (format-time-string "Buffer update: %H:%M:%S\n"
                                        ;; elfeed-navi-buffer-last-update))
            ;; (insert (format-time-string "Entries list update update: %H:%M:%S\n\n"
                                        ;; elfeed-navi-entries-last-update))
            (dolist (summary-entry summary-entries)
              (funcall #'elfeed-navi-print-line-function summary-entry aggregator)
              (insert "\n"))
            (setf elfeed-navi-buffer-last-update (float-time))))
        (when (zerop (buffer-size))
          ;; If nothing changed, force a header line update
          (force-mode-line-update))
        ))))

(defun elfeed-navi-buffer-update--force ()
  "Force update the `*elfeed-navi*' buffer to match the database."
  (interactive)
  (elfeed-navi-buffer-update t))

(defun elfeed-navi-buffer-update--force-reset ()
  "Update the `*elfeed-navi*' buffer using default filter."
  (interactive)
  (setq elfeed-navi-search-filter elfeed-navi-search-filter-default)
  (elfeed-navi-buffer-update--force))

(defvar elfeed-navi-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map "h" #'describe-mode)
      (define-key map "q" #'quit-window)
      (define-key map "e" #'elfeed)
      (define-key map "n" #'next-line)
      (define-key map "g" #'elfeed-navi-buffer-update--force)
      (define-key map "G" #'elfeed-navi-buffer-update--force-reset)
      (define-key map (kbd "RET") #'elfeed-navi-goto-feed)
      (define-key map "p" #'previous-line))) "Keymap for elfeed-navi-mode.")

(defun elfeed-navi-mode ()
  "Major mode for listing elfeed feed entries occurrences.
\\{elfeed-navi-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map elfeed-navi-mode-map)
  (setq major-mode 'elfeed-navi-mode
        mode-name "elfeed-navi"
        truncate-lines t
        buffer-read-only t)
  (buffer-disable-undo)
  (hl-line-mode)
  (run-mode-hooks 'elfeed-navi-mode-hook))

;;;###autoload
(defun elfeed-navi ()
  "Enter elfeed-navi."
  (interactive)
  (switch-to-buffer (elfeed-navi-buffer))
  (unless (eq major-mode 'elfeed-navi-mode)
    (elfeed-navi-mode)))

(defun elfeed-navi-selected-summary-entry ()
  (let* ((line (line-number-at-pos (point))))
    (nth (- line 1) elfeed-navi-summary-entries-alist)))

(defun elfeed-navi-sanitize-regexp-filter (str)
  "Replace spaces with [[:space:]] in STR.

See `https://github.com/skeeto/elfeed/issues/336'"
  (string-replace " " "[[:space:]]" str))

(defun elfeed-navi-goto-feed (summary-entry)
  "Display the elfeed-search buffer filtered by FEED-TITLE at point"
  (interactive (list (elfeed-navi-selected-summary-entry)))
  (let* ((feed-title (elfeed-navi-summary-entry-get-title summary-entry))
         (feed-title-sanitized (elfeed-navi-sanitize-regexp-filter feed-title)))
    (elfeed-search-set-filter
     (format "@4-days-ago +unread =%s" feed-title-sanitized)) ;; TODO review this filter
    (elfeed)))

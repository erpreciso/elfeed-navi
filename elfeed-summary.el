;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.


(nth 1 entries)

;; #s(elfeed-entry
;; ("www.politico.eu" . "https://www.politico.eu/?post_type=article&p=4117362")
;; "A big Chinese delegation unnerves US diplomats in Davos"
;; "https://www.politico.com/news/2024/01/15/davos-china-blinken-ukraine-00135530?utm_source=RSS_Feed&utm_medium=RSS&utm_campaign=RSS_Syndication"
;; 1705300124.0
;; #s(elfeed-ref "9a3c172820ff20907c9fe4bb4f6d6a2af9bf0adf")
;; html
;; nil
;; (english politics)
;; "https://www.politico.eu/feed/"
;; (:authors ((:name "Nahal Toosi")) :categories ("Business and competition" "Davos" "Elections" "Politics") :elfeed-score/score 500))

(type-of (nth 1 entries))

(length entries)

(defun erf-get-feeds-titles (entries)
  (seq-uniq
   (seq-map
    (lambda (e) (elfeed-meta (elfeed-entry-feed e) :title)) entries)))



(let ((feeds-alist
       (seq-map (lambda (f) (list f 0)) (erf-get-feeds-titles entries))))
  (assoc "Politico" feeds-alist #'string=))

(defun erf-feeds-occurrences-alist (entries)
  "Return alist (feed-title counter) for a list of ENTRIES."
  (let ((occurrences))
    (dolist (occurrence-title (erf-get-feeds-titles entries))
      (push 
       (cl-loop for entry in entries
                for feed-title = (elfeed-meta (elfeed-entry-feed entry) :title)
                count (string= feed-title occurrence-title) into p
                finally return (list occurrence-title p)) occurrences))
    occurrences))
         

#s(elfeed-feed "https://www.varesenews.it/?feed=news-news24" "https://www.varesenews.it/?feed=news-news24" "VareseNews" nil (:last-modified "Mon, 15 Jan 2024 08:40:09 GMT" :etag "W/\"817e9a40bf5b318537f9d90145ca19dc-gzip\"" :title "VareseNews" :failures 10))

(defun elfeed-search--count-unread ()
  (
    (cl-loop with feeds = (make-hash-table :test 'equal)
             for entry in (seq-take entries 100)
             for feed = (elfeed-entry-feed entry)
             for url = (elfeed-feed-url feed)
             count entry into entry-count
             count (elfeed-tagged-p 'unread entry) into unread-count
             do (puthash url t feeds)
             finally
             (cl-return
              (format "%d/%d:%d"
                      unread-count entry-count
                      (hash-table-count feeds)))))

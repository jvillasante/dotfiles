(defun elfeed-mark-all-as-read ()
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

;; functions to support syncing .elfeed between machines
;; makes sure elfeed reads index from disk before launching
(defun jv/elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force))

;; write to disk when quiting
(defun jv/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

;; code to add and remove a starred tag to elfeed article
;; based on http://matt.hackinghistory.ca/2015/11/22/elfeed/

;; add a star
(defun jv/elfeed-star ()
  "Apply starred to all selected entries."
  (interactive )
  (let* ((entries (elfeed-search-selected))
         (tag (intern "starred")))

    (cl-loop for entry in entries do (elfeed-tag entry tag))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

;; remove a start
(defun jv/elfeed-unstar ()
  "Remove starred tag from all selected entries."
  (interactive )
  (let* ((entries (elfeed-search-selected))
         (tag (intern "starred")))

    (cl-loop for entry in entries do (elfeed-untag entry tag))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

;; shortcut to jump to starred bookmark
(defun jv/elfeed-show-starred ()
  (interactive)
  (bookmark-jump "elfeed-starred"))

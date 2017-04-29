(defconst jv-elfeed-packages
  '(elfeed))

(defun jv-elfeed/post-init-elfeed ()
  (with-eval-after-load 'elfeed
    ;; elfeed db path
    (setq elfeed-db-directory (concat my-dropbox-path "/Personal/elfeed/elfeed_db"))

    ;; load and save db on open and quit
    (spacemacs/set-leader-keys "af" 'jv/elfeed-load-db-and-open)
    (evil-define-key 'evilified elfeed-search-mode-map "q" 'jv/elfeed-save-db-and-bury)))

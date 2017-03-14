(defconst jv-elfeed-packages
  '(elfeed))

(defun jv-elfeed/post-init-elfeed ()
  (with-eval-after-load 'elfeed
    (setq elfeed-db-directory (concat my-dropbox-path "/Personal/elfeed/elfeed_db"))

    (spacemacs/set-leader-keys "af" 'jv/elfeed-load-db-and-open)
    (evil-define-key 'evilified elfeed-search-mode-map "q" 'jv/elfeed-save-db-and-bury)
    (evil-define-key 'evilified elfeed-search-mode-map "Q" 'jv/elfeed-save-db-and-bury)))

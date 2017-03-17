(defconst jv-elfeed-packages
  '(elfeed))

(defun jv-elfeed/post-init-elfeed ()
  (with-eval-after-load 'elfeed
    ;; elfeed db path
    (setq elfeed-db-directory (concat my-dropbox-path "/Personal/elfeed/elfeed_db"))

    ;; load and save db on open and quit
    (spacemacs/set-leader-keys "af" 'jv/elfeed-load-db-and-open)
    (evil-define-key 'evilified elfeed-search-mode-map "q" 'jv/elfeed-save-db-and-bury)

    ;; face for starred articles
    (defface elfeed-search-starred-title-face
      '((t :foreground "#f77"))
      "Marks a starred Elfeed entry.")
    (push '(starred elfeed-search-starred-title-face) elfeed-search-face-alist)

    ;; starred articles
    (evil-define-key 'evilified elfeed-search-mode-map "*" 'jv/elfeed-star)
    (evil-define-key 'evilified elfeed-search-mode-map "8" 'jv/elfeed-unstar)
    (evil-define-key 'evilified elfeed-search-mode-map "S" 'jv/elfeed-show-starred)))

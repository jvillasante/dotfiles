;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq my-rss-packages
      '(
        elfeed
        ))

;; List of packages to exclude.
(setq my-rss-excluded-packages '())

(defun my-rss/init-elfeed ()
  (use-package elfeed
    ;; :defer t activates lazy loading which makes startup faster
    :defer t
    ;; The code in :init is always run, use it to set up config vars and key bindings
    :init
    (progn ; :init only takes one expression so use "progn" to combine multiple things
      (setq elfeed-feeds
            '(("http://nullprogram.com/feed/" emacs)
              ("http://emacs-fu.blogspot.com/feeds/posts/default" emacs)
              ("http://planet.emacsen.org/atom.xml" emacs)
              ("http://emacsrocks.com/atom.xml" emacs)
              ("http://nedroid.com/feed/" webcomic)
              ("https://blog.golang.org/feed.atom" golang)))

      (setq-default elfeed-search-filter "-junk @2-week-ago +unread")
      (setq elfeed-sort-order 'ascending)
      (setq elfeed-search-title-max-width 100)

      ;; elfeed-search
      (evil-define-key 'normal elfeed-search-mode-map (kbd "q") 'quit-window)
      (evil-define-key 'normal elfeed-search-mode-map (kbd "a") 'elfeed-search-update--force)
      (evil-define-key 'normal elfeed-search-mode-map (kbd "A") 'elfeed-update)
      (evil-define-key 'normal elfeed-search-mode-map (kbd "s") 'elfeed-search-live-filter)
      (evil-define-key 'normal elfeed-search-mode-map (kbd "RET") 'elfeed-search-show-entry)
      (evil-define-key 'normal elfeed-search-mode-map (kbd "o") 'elfeed-search-browse-url)
      (evil-define-key 'normal elfeed-search-mode-map (kbd "y") 'elfeed-search-yank)
      (evil-define-key 'normal elfeed-search-mode-map (kbd "r") 'elfeed-search-untag-all-unread)
      (evil-define-key 'normal elfeed-search-mode-map (kbd "u") 'elfeed-search-tag-all-unread)
      (evil-define-key 'normal elfeed-search-mode-map (kbd "+") 'elfeed-search-tag-all)
      (evil-define-key 'normal elfeed-search-mode-map (kbd "-") 'elfeed-search-untag-all)
      ;; (evil-define-key 'normal elfeed-search-mode-map (kbd "E") (lambda() (interactive)(find-file "~/.emacs.d/elfeed.el.gpg")))
      ;; elfeed-show
      (evil-define-key 'normal elfeed-show-mode-map (kbd "q") 'elfeed-kill-buffer)
      (evil-define-key 'normal elfeed-show-mode-map (kbd "g") 'elfeed-show-refresh)
      (evil-define-key 'normal elfeed-show-mode-map (kbd "n") 'elfeed-show-next)
      (evil-define-key 'normal elfeed-show-mode-map (kbd "p") 'elfeed-show-prev)
      (evil-define-key 'normal elfeed-show-mode-map (kbd "o") 'elfeed-show-visit)
      (evil-define-key 'normal elfeed-show-mode-map (kbd "y") 'elfeed-show-yank)
      (evil-define-key 'normal elfeed-show-mode-map (kbd "u") (elfeed-expose #'elfeed-show-tag 'unread))
      (evil-define-key 'normal elfeed-show-mode-map (kbd "+") 'elfeed-show-tag)
      (evil-define-key 'normal elfeed-show-mode-map (kbd "-") 'elfeed-show-untag)
      (evil-define-key 'normal elfeed-show-mode-map (kbd "SPC") 'scroll-up)
      (evil-define-key 'normal elfeed-show-mode-map (kbd "S-SPC") 'scroll-down)

      (evil-leader/set-key
        "of" 'elfeed))
    :config ; :config is called after the package is actually loaded with defer
    ;; You can put stuff that relies on the package like function calls here
    (message "zeal mode was actually loaded!")))

;;; packages.el --- mycontribs Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar mycontribs-packages
  '(
    ;; package mycontribss go here
    ;; evil-tabs
    password-store
    zeal-at-point
    define-word
    deft
    twittering-mode
    elfeed
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar mycontribs-excluded-packages '()
  "List of packages to exclude.")

;; (defun mycontribs/init-evil-tabs ()
;;   (use-package evil-tabs
;;     :init
;;     (global-evil-tabs-mode t)))

(defun mycontribs/init-password-store()
  (use-package password-store
    :ensure t))

(defun mycontribs/init-zeal-at-point ()
  (use-package zeal-at-point
    :ensure t
    :config
    (evil-leader/set-key
      "oz" 'zeal-at-point)))

(defun mycontribs/init-define-word ()
  (use-package define-word
    :ensure t
    :bind (("H-d" . define-word-at-point)
            ("H-D" . define-word))))

(defun mycontribs/init-deft ()
  (use-package deft
    :ensure t
    :config
    (setq deft-directory "~/Dropbox/Personal/Notes")
    (setq deft-extension "org")
    (setq deft-text-mode 'org-mode)
    (setq deft-use-filename-as-title t)
    (setq deft-auto-save-interval 0)
    (evil-leader/set-key
      "od" 'deft))
  )

(defun mycontribs/init-twittering-mode ()
  (use-package twittering-mode
    :ensure t
    :config
    (setq twittering-use-master-password t)
    (setq twittering-icon-mode t)                ; Show icons
    (setq twittering-use-icon-storage t)
    (setq twittering-timer-interval 300)         ; Update your timeline each 300 seconds (5 minutes)
    (setq twittering-url-show-status nil)        ; Keeps the echo area from showing all the http processes
    (twittering-enable-unread-status-notifier)
    (setq twittering-display-remaining t)
    (setq twittering-edit-skeleton 'inherit-any)
    ;; (setq twittering-initial-timeline-spec-string
    ;;       '(":home"
    ;;         ":replies"
    ;;         ":favorites"
    ;;         ":direct_messages"
    ;;         ":search/emacs/"
    ;;         "user_name/list_name"))
    (evil-leader/set-key
      "ot" 'twit))
  )

(defun mycontribs/init-elfeed ()
  (use-package elfeed
    :ensure t
    :config
    (evil-leader/set-key
      "of" 'elfeed)

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
    (evil-define-key 'normal elfeed-show-mode-map (kbd "S-SPC") 'scroll-down)))

;; For each package, define a function mycontribs/init-<package-mycontribs>
;;
;; (defun mycontribs/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

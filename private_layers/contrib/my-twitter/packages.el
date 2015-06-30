;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq my-twitter-packages
      '(
        twittering-mode
        ))

;; List of packages to exclude.
(setq my-twitter-excluded-packages '())

(defun my-twitter/init-twittering-mode ()
  (use-package twittering-mode
    ;; :defer t activates lazy loading which makes startup faster
    :defer t
    ;; The code in :init is always run, use it to set up config vars and key bindings
    :init
    (progn ; :init only takes one expression so use "progn" to combine multiple things
      (setq twittering-use-master-password t)
      (setq twittering-icon-mode t)                ; Show icons
      (setq twittering-use-icon-storage t)
      (setq twittering-timer-interval 300)         ; Update your timeline each 300 seconds (5 minutes)
      (setq twittering-url-show-status nil)        ; Keeps the echo area from showing all the http processes
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
    :config ; :config is called after the package is actually loaded with defer
    ;; You can put stuff that relies on the package like function calls here
    (twittering-enable-unread-status-notifier)
    (message "zeal mode was actually loaded!")))

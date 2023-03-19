;;; my-init-apps.el -*- lexical-binding: t; -*-

(straight-use-package 'elfeed)
(straight-use-package 'elfeed-org)
(straight-use-package 'pdf-tools)

(use-package eww
    :init
    ;; use brave as default engine to search keyword.
    (setq eww-search-prefix "https://search.brave.com/search?q=")

    :config
    (my/localleader
        :keymaps 'eww-mode-map
        "y" #'eww-copy-page-url
        ;; get the url of current visiting page
        "g" #'my/web-search-eww))

(use-package xwidget
    :init
    (setq my/xwidget-side-window-display
        `("\\*xwidget"
             (display-buffer-in-side-window display-buffer-reuse-window)
             (window-width . 0.33)
             (window-height . 0.5)
             (side . ,(alist-get 'xwidget-plot my/side-window-sides))
             (slot . ,(alist-get 'xwidget-plot my/side-window-slots))))

    :config
    (add-hook 'xwidget-webkit-mode-hook (display-line-numbers-mode nil)))

(use-package elfeed
    :init
    (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
        elfeed-enclosure-default-dir (expand-file-name "closures" elfeed-db-directory))

    (my/open-map
        :keymaps 'override
        "f" #'elfeed)

    :config
    (setq elfeed-search-title-min-width 60)
    (setq elfeed-search-title-max-width 100)
    (setq elfeed-search-trailing-width 0)
    (setq elfeed-search-filter "@6-months-ago +unread")
    (setq elfeed-db-directory (expand-file-name "Apps/elfeed/elfeed_db" +my/dropbox-path))
    (setq elfeed-show-entry-switch #'pop-to-buffer)
    (setq shr-max-image-proportion 0.7)
    (add-to-list 'display-buffer-alist
        '("\\*elfeed-entry"
             (display-buffer-below-selected)
             (window-height . 0.85)))

    (general-define-key
        :keymaps 'elfeed-show-mode-map
        "gw" #'my:elfeed-open-entry-via-eww
        "gW" #'my:elfeed-open-entry-via-xwidget)

    ;; `elfeed-kill-buffer' only kills the buffer, but won't delete
    ;; the window. This is not an ideal behavior since you typically
    ;; what to hit `q' to delete the window displaying the news after
    ;; you have finished reading.
    (advice-add #'elfeed-kill-buffer :after #'my:elfeed-delete-window-after-kill-buffer))

(use-package elfeed-org
    :after elfeed
    :demand t
    :init
    (setq rmh-elfeed-org-files `(,(expand-file-name "elfeed.org" org-directory)))

    :config
    (elfeed-org))

(use-package pdf-tools
    :mode ("\\.pdf\\'" . pdf-view-mode)
    :init
    (setq pdf-view-display-size 'fit-page
        ;; Enable hiDPI support, but at the cost of memory! See politza/pdf-tools#51
        pdf-view-use-scaling t
        pdf-view-use-imagemagick nil
        pdf-view-continuous nil)

    :config
    (add-to-list 'display-buffer-alist
        `("\\*[oO]utline.*pdf\\*"
             (display-buffer-in-side-window display-buffer-reuse-window)
             (side . ,(alist-get 'pdf-outline my/side-window-sides))
             (window-width . 0.3)))

    (add-hook 'pdf-outline-buffer-mode-hook #'my:font-set-small-variable-font)
    (add-hook 'pdf-outline-buffer-mode-hook (display-line-numbers-mode nil))
    (add-hook 'pdf-view-mode-hook #'my:pdf-midnight-mode-maybe)
    (add-hook 'pdf-view-mode-hook (display-line-numbers-mode nil)))

(provide 'my-init-apps)
;;; my-init-apps.el ends here

(evil-set-initial-state 'xwidget-webkit-mode 'emacs)
(add-hook 'xwidget-webkit-mode-hook
          (lambda ()
            (define-key xwidget-webkit-mode-map [mouse-4] 'xwidget-webkit-scroll-down)
            (define-key xwidget-webkit-mode-map [mouse-5] 'xwidget-webkit-scroll-up)
            (define-key xwidget-webkit-mode-map (kbd "<up>") 'xwidget-webkit-scroll-down)
            (define-key xwidget-webkit-mode-map (kbd "<down>") 'xwidget-webkit-scroll-up)
            (define-key xwidget-webkit-mode-map (kbd "M-w") 'xwidget-webkit-copy-selection-as-kill)
            (define-key xwidget-webkit-mode-map (kbd "C-c") 'xwidget-webkit-copy-selection-as-kill)

            ;; make xwidget default browser (not for now... maybe in the future!)
            ;; (setq browse-url-browser-function (lambda (url session)
            ;;                                     (other-window 1)
            ;;                                     (xwidget-browse-url-no-reuse url)))

            ;; adapt webkit according to window configuration change automatically
            ;; without this hook, every time you change your window configuration,
            ;; you must press 'a' to adapt webkit content to new window size
            (add-hook 'window-configuration-change-hook (lambda ()
                                                          (when (equal major-mode 'xwidget-webkit-mode)
                                                            (xwidget-webkit-adjust-size-dispatch))))

            ;; by default, xwidget reuses previous xwidget window,
            ;; thus overriding your current website, unless a prefix argument
            ;; is supplied
            ;;
            ;; This function always opens a new website in a new window
            (defun xwidget-browse-url-no-reuse (url &optional sessoin)
              (interactive (progn
                             (require 'browse-url)
                             (browse-url-interactive-arg "xwidget-webkit URL: "
                                                         )))
              (xwidget-webkit-browse-url url t))))

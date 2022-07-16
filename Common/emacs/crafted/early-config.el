;;; early-config.el --- Early Emacs configuration -*- lexical-binding: t; -*-

(if (daemonp)
    (add-hook 'after-make-frame-functions
        (lambda (frame)
            (with-selected-frame frame
                (progn
                    ;; Set the font faces early
                    (setq crafted-ui-default-font '(:font "Iosevka" :height 160))
                    (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :weight 'light)

                    ;; Line numbers
                    (setq crafted-ui-display-line-numbers t)

                    ;; Set frame transparency and maximize frame by default before the first frame loads
                    ;; (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
                    ;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))
                    (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
                    (add-to-list 'default-frame-alist '(fullscreen . maximized))))))
    (progn
        ;; Set the font faces early
        (setq crafted-ui-default-font '(:font "Iosevka" :height 160))
        (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :weight 'light)

        ;; Line numbers
        (setq crafted-ui-display-line-numbers t)

        ;; Set frame transparency and maximize frame by default before the first frame loads
        ;; (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
        ;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))
        (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
        (add-to-list 'default-frame-alist '(fullscreen . maximized))))

;;; early-config.el ends here

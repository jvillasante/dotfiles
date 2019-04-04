(defun update-solarize-dark ()
    (custom-theme-set-faces
        'solarized-dark

        ;; Makes matching parens obvious
        `(sp-show-pair-match-face ((t (:inherit sp-show-pair-match-face
                                          :background "#586e75"))))

        ;; active modeline has no colors
        ;;`(mode-line ((t (:inherit mode-line :background "#002b36"))))
        ;;`(mode-line-inactive ((t (:inherit mode-line :background "#002b36"))))
        ;;`(spaceline-highlight-face ((t (:inherit mode-line :background "#002b36"))))
        ;;`(powerline-active1 ((t (:inherit mode-line :background "#002b36"))))
        ;;`(powerline-active2 ((t (:inherit mode-line :background "#002b36"))))


        `(mode-line ((t (:inherit mode-line :height 1.0))))
        ;;`(spaceline-highlight-face ((t (:inherit mode-line :background "#ff2b36"
        ;;                                         :height 1.0))))

        ;; Inactive modeline has tint
        `(powerline-inactive2 ((t (:inherit powerline-inactive1))))

        ;; Org and outline header updates
        `(org-level-1 ((t (:height 1.25 :foreground ,my-black
                              :background "#268bd2"
                              :weight bold))))
        `(org-level-2 ((t (:height 1.15 :foreground ,my-black
                              :background "#2aa198"
                              :weight bold))))
        `(org-level-3 ((t (:height 1.05 :foreground ,my-black
                              :background "#b58900"
                              :weight bold))))

        '(outline-1 ((t (:inherit org-level-1))))
        '(outline-2 ((t (:inherit org-level-2))))
        '(outline-3 ((t (:inherit org-level-3))))
        '(outline-4 ((t (:inherit org-level-4))))
        ))

(setq my-black "#1b1b1e")

(defun update-solarize-light ()
    (custom-theme-set-faces
        'solarized-light

        ;; Makes matching parens obvious
        `(sp-show-pair-match-face ((t (:inherit sp-show-pair-match-face
                                          :background "light gray"))))

        ;; '(font-lock-constant-face nil :weight 'normal)
        ;; '(font-lock-function-name-face nil :weight 'bold)
        ;; '(which-key-key-face nil :foreground
        ;;      (face-attribute 'error :foreground))

        ;; active modeline has no colors
        ;; `(mode-line ((t (:inherit mode-line :background "#fdf6e3"))))
        ;; `(mode-line-inactive ((t (:inherit mode-line :background "#fdf6e3"))))
        ;; `(spaceline-highlight-face ((t (:inherit mode-line :background "#fdf6e3"))))
        ;; `(powerline-active1 ((t (:inherit mode-line :background "#fdf6e3"))))
        ;; `(powerline-active2 ((t (:inherit mode-line :background "#fdf6e3"))))

        ;; Inactive modeline has tint
        ;; `(powerline-inactive2 ((t (:inherit powerline-inactive1))))

        ;; Org and outline header updates
        ;; `(org-level-1 ((t (:height 1.25 :foreground ,my-black
        ;;                       :background "#C9DAEA"
        ;;                       :weight bold))))
        ;; `(org-level-2 ((t (:height 1.15 :foreground ,my-black
        ;;                       :background "#7CDF64"
        ;;                       :weight bold))))
        ;; `(org-level-3 ((t (:height 1.05 :foreground ,my-black
        ;;                       :background "#F8DE7E"
        ;;                       :weight bold))))

        ;; '(outline-1 ((t (:inherit org-level-1))))
        ;; '(outline-2 ((t (:inherit org-level-2))))
        ;; '(outline-3 ((t (:inherit org-level-3))))
        ;; '(outline-4 ((t (:inherit org-level-4))))

        ;; `(org-todo ((t (:foreground ,my-black :weight extra-bold
        ;;                    :background "light gray"))))
        ;; `(org-priority ((t (:foreground ,my-black :weight bold
        ;;                        :background "light gray"))))
        ))

(with-eval-after-load "solarized"
    (if (string= 'solarized-dark (car custom-enabled-themes))
        (update-solarize-dark)
        (update-solarize-light)))

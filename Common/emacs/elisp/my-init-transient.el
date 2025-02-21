;;; my-init-transient.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; disproject : dispatch various project-related commands via Transient menus
(use-package disproject
    :disabled t
    ;; Replace `project-prefix-map' with `disproject-dispatch'.
    :bind (:map ctl-x-map
                ("p" . disproject-dispatch)))

;; casual : https://github.com/kickingvegas/casual
;;          https://github.com/kickingvegas/casual-suite
(use-package casual-suite
    :config
    (keymap-set calc-mode-map           "C-o" #'casual-calc-tmenu)
    (keymap-set dired-mode-map          "C-o" #'casual-dired-tmenu)
    (keymap-set isearch-mode-map        "C-o" #'casual-isearch-tmenu)
    (keymap-set ibuffer-mode-map        "C-o" #'casual-ibuffer-tmenu)
    (keymap-set Info-mode-map           "C-o" #'casual-info-tmenu)
    (keymap-set reb-mode-map            "C-o" #'casual-re-builder-tmenu)
    (keymap-set reb-lisp-mode-map       "C-o" #'casual-re-builder-tmenu)
    (keymap-set bookmark-bmenu-mode-map "C-o" #'casual-bookmarks-tmenu)
    (keymap-set org-agenda-mode-map     "C-o" #'casual-agenda-tmenu)
    (keymap-set symbol-overlay-map      "C-o" #'casual-symbol-overlay-tmenu)
    ;; (keymap-global-set "M-g" #'casual-avy-tmenu)
    ;; (keymap-global-set "C-o" #'casual-editkit-main-tmenu)
    )

(provide 'my-init-transient)
;;; my-init-transient.el ends here

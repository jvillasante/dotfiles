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
    :defer t
    :bind (;; (:map global-map ("M-g" . casual-avy-tmenu))
           ;; (:map global-map ("C-o" . casual-editkit-main-tmenu))
           (:map calc-mode-map ("C-o" . casual-calc-tmenu))
           (:map dired-mode-map ("C-o" . casual-dired-tmenu))
           (:map isearch-mode-map ("C-o" . casual-isearch-tmenu))
           (:map ibuffer-mode-map ("C-o" . casual-ibuffer-tmenu))
           (:map Info-mode-map ("C-o" . casual-info-tmenu))
           (:map reb-mode-map ("C-o" . casual-re-builder-tmenu))
           (:map reb-lisp-mode-map ("C-o" . casual-re-builder-tmenu))
           (:map bookmark-bmenu-mode-map ("C-o" . casual-bookmarks-tmenu))
           (:map org-agenda-mode-map ("C-o" . casual-agenda-tmenu))
           (:map symbol-overlay-map ("C-o" . casual-symbol-overlay-tmenu))))

(provide 'my-init-transient)
;;; my-init-transient.el ends here

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
;; A collection of opinionated Transient-based keyboard-driven user interfaces
;; for various built-in Emacs modes: Agenda, Bookmarks, Calc, Calendar, Dired,
;; EditKit, IBuffer, Info, I-Search, Re-Builder
(use-package casual
    :disabled t
    :config
    ;; dired
    (keymap-set dired-mode-map "C-o" #'casual-dired-tmenu)
    (keymap-set dired-mode-map "s" #'casual-dired-sort-by-tmenu) ; optional
    (keymap-set dired-mode-map "/" #'casual-dired-search-replace-tmenu) ; optional

    ;; ibuffer
    (keymap-set ibuffer-mode-map "C-o" #'casual-ibuffer-tmenu)
    (keymap-set ibuffer-mode-map "F" #'casual-ibuffer-filter-tmenu)
    (keymap-set ibuffer-mode-map "s" #'casual-ibuffer-sortby-tmenu))

(provide 'my-init-transient)
;;; my-init-transient.el ends here

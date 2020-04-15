;;; +ui.el -*- lexical-binding: t; -*-

;; font
(setq doom-font (font-spec :family "Source Code Pro" :size 20))
(setq doom-variable-pitch-font (font-spec :family "Source Code Pro"))
(setq doom-unicode-font (font-spec :family "Source Code Pro"))
(setq doom-big-font (font-spec :family "Source Code Pro" :size 40))

;; Dash highlighting
(after! dash (dash-enable-font-lock))

;; theme
;; (setq doom-theme 'doom-solarized-light)
(setq doom-theme 'modus-operandi)

(after! doom-themes
    (setq
        doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t)   ; if nil, italics is universally disabled

    (setq
        doom-themes-neotree-enable-file-icons nil
        doom-themes-neotree-file-icons nil)

    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)

    ;; for neotree users
    (doom-themes-neotree-config)

    ;; for treemacs users
    ;; (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
    ;; (doom-themes-treemacs-config)

    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config))

;; modeline
(after! doom-modeline
    ;; show workspace name on the modeline
    (setq doom-modeline-persp-name t)

    ;; How tall the mode-line should be. It's only respected in GUI.
    ;; If the actual char height is larger, it respects the actual height.
    (setq doom-modeline-height 20)

    ;; Whether display icons in the mode-line. Respects `all-the-icons-color-icons'.
    ;; While using the server mode in GUI, should set the value explicitly.
    (setq doom-modeline-icon nil))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; popup rules
;; (set-popup-rules!
;;   '((\"^ \\*\" :slot 1 :vslot -1 :size #'+popup-shrink-to-fit)
;;     (\"^\\*\"  :slot 1 :vslot -1 :select t))
;;   '((\"^\\*Completions\" :slot -1 :vslot -2 :ttl 0)
;;     (\"^\\*Compil\\(?:ation\\|e-Log\\)\" :size 0.3 :ttl 0 :quit t)))"
(set-popup-rule! "^\\*Treemacs" :side 'left :size 0.30 :quit nil :ttl 0)
(set-popup-rule! "^\\*lsp" :size 0.4 :quit t :ttl 0)
(set-popup-rule! "^\\*lsp-help" :size 0.35 :quit t :select t)

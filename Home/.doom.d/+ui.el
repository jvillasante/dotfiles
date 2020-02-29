;;; +ui.el -*- lexical-binding: t; -*-


;; font
(setq doom-font (font-spec :family "Source Code Pro" :size 20))
(setq doom-variable-pitch-font (font-spec :family "Source Code Pro"))
(setq doom-unicode-font (font-spec :family "Source Code Pro"))
(setq doom-big-font (font-spec :family "Source Code Pro" :size 40))

;; theme
(setq doom-theme 'doom-solarized-light)

(after! doom-themes
    (setq
        doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t)   ; if nil, italics is universally disabled

    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)

    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config))

;; modeline
(after! doom-modeline
    ;; How tall the mode-line should be. It's only respected in GUI.
    ;; If the actual char height is larger, it respects the actual height.
    (setq doom-modeline-height 20)

    ;; Whether display icons in the mode-line. Respects `all-the-icons-color-icons'.
    ;; While using the server mode in GUI, should set the value explicitly.
    (setq doom-modeline-icon nil))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


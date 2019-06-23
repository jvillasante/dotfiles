;;; -*- lexical-binding: t; -*-

;; MAC setup
(when IS-MAC
    (setq ns-use-thin-smoothing t)
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-appearance . dark))
    (add-hook 'window-setup-hook #'toggle-frame-maximized))

(setq ns-use-proxy-icon nil)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq doom-font (font-spec :family "Source Code Pro" :size 22))
(setq doom-variable-pitch-font (font-spec :family "Source Code Pro"))
(setq doom-unicode-font (font-spec :family "Source Code Pro"))
(setq doom-big-font (font-spec :family "Source Code Pro" :size 40))

;; Dash highlighting
(after! dash (dash-enable-font-lock))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq solarized-use-variable-pitch nil
    solarized-use-less-bold t
    solarized-use-more-italic nil
    solarized-distinct-doc-face t
    solarized-high-contrast-mode-line nil
    ;; I find different font sizes irritating.
    solarized-height-minus-1 1.0
    solarized-height-plus-1 1.0
    solarized-height-plus-2 1.0
    solarized-height-plus-3 1.0
    solarized-height-plus-4 1.0)

(setq doom-theme (if (display-graphic-p) 'doom-solarized-light nil))

(after! doom-themes
    (custom-set-faces '(header-line ((t (:inherit neo-root-dir-face))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Resize window
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def-package! resize-window
    :commands  (resize-window))

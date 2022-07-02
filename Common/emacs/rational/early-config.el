;;; early-config.el --- Early Emacs configuration -*- lexical-binding: t; -*-

;; Set the font faces early
(custom-set-variables '(rational-ui-default-font '(:font "Iosevka" :height 160)))
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :weight 'light)

;; Set frame transparency and maximize frame by default before the first frame loads
;; (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

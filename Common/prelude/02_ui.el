;;; personal/02_ui.el --- Prelude personal config -*- lexical-binding: t; -*-
;;;

;;; theme
(prelude-require-package 'modus-themes)
(load-theme 'modus-operandi t)

;;; font
(set-face-attribute 'default nil
    :family "Iosevka"
    :height 160
    :weight 'normal
    :width 'normal)

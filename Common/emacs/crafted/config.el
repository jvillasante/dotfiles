;;; config.el --- Emacs Configuration -*- lexical-binding: t; -*-
;;
;;; https://github.com/SystemCrafters/crafted-emacs
;;; https://systemcrafters.net/live-streams/july-1-2022/

;;; Crafted Modules
(require 'crafted-defaults)    ; Sensible default settings for Emacs
(require 'crafted-updates)     ; Tools to upgrade Crafted Emacs
(require 'crafted-completion)  ; selection framework based on `vertico`
(require 'crafted-ui)          ; Better UI experience (modeline etc.)
(require 'crafted-windows)     ; Window management configuration
(require 'crafted-editing)     ; Whitspace trimming, auto parens etc.
;; (require 'crafted-evil)        ; An `evil-mode` configuration
(require 'crafted-org)         ; org-appear, clickable hyperlinks etc.
(require 'crafted-project)     ; built-in alternative to projectile
;; (require 'crafted-speedbar)    ; built-in file-tree
;; (require 'crafted-screencast)  ; show current command and binding in modeline
(require 'crafted-compile)       ; Set up automatic compilation for some emacs-lisp files)

;;; Further settings and customizations follow here...

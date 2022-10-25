;;; config.el --- Emacs Configuration -*- lexical-binding: t; -*-
;;;
;;; Links:
;;; https://github.com/SystemCrafters/crafted-emacs
;;; https://github.com/K8X1D/crafted-emacs

;;; Commentary:
;;; Emacs config based on crafted-emacs.

;;; Code:

;; Crafted Emacs Modules
(require 'crafted-compile)     ; Set up automatic compilation for some emacs-lisp files
(require 'crafted-completion)  ; A selection framework configuration based on Vertico etc.
(require 'crafted-defaults)    ; Lightly opinionated default settings for Emacs
(require 'crafted-editing)     ; Settings for the editing component (whitespace trimming etc.)
;; (require 'crafted-erlang)      ; A configuration for Erlang programming
;; (require 'crafted-evil)        ; An evil-mode configuration
(require 'crafted-ide)         ; A general configuration to make Emacs more like an IDE, uses eglot.
(require 'crafted-latex)       ; A configuration for creating documents using the LaTeX typesetting language
(require 'crafted-lisp)        ; A configuration for the Lisp family of languages (Clojure, Common Lisp, Scheme, Racket)
(require 'crafted-org)         ; A few tweaks to Org-mode (org-appear, clickable hyperlinks
;; (require 'crafted-osx)         ; Set up some conveniences to work in a Mac OS/OSX environment
;; (require 'crafted-pdf-reader)  ; Setup pdf-tools for reading PDF files in Emacs
(require 'crafted-project)     ; Built in project management alternative to projectile
(require 'crafted-python)      ; A configuration for programming in Python
(require 'crafted-screencast)  ; Tools for doing screencasts
(require 'crafted-speedbar)    ; A file-tree
(require 'crafted-ui)          ; Extra UI configuration for a better experience (mode line, etc)
(require 'crafted-updates)     ; Tools to upgrade Crafted Emacs
(require 'crafted-windows)     ; Window management configuration

;; Personal Modules
(require 'my-ui) ; custom ui configs

;;; To not load `custom.el' after `config.el', uncomment this line.
;;; (setq crafted-load-custom-file nil)

;;; config.el ends here

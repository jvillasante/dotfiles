;;; config.el --- Emacs Configuration -*- lexical-binding: t; -*-
;;
;;; https://github.com/SystemCrafters/crafted-emacs
;;; https://systemcrafters.net/live-streams/july-1-2022/

;;; Commentary:
;;
;; Crafted Emacs supports user customization through a `config.el' file
;; similar to this one.  You can copy this file as `config.el' to your
;; Crafted Emacs configuration directory as an example.
;;
;; In your configuration you can set any Emacs configuration variable, face
;; attributes, themes, etc as you normally would.
;;
;; See the README.org file in this repository for additional information.

;;; Code:
;; At the moment, Crafted Emacs offers the following modules.
;; Comment out everything you don't want to use.
;; At the very least, you should decide whether or not you want to use
;; evil-mode, as it will greatly change how you interact with Emacs.
;; So, if you prefer Vim-style keybindings over vanilla Emacs keybindings
;; remove the comment in the line about `crafted-evil' below.
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

;; Set the default face. The default face is the basis for most other
;; faces used in Emacs. A "face" is a configuration including font,
;; font size, foreground and background colors and other attributes.
;; The fixed-pitch and fixed-pitch-serif faces are monospace faces
;; generally used as the default face for code. The variable-pitch
;; face is used when `variable-pitch-mode' is turned on, generally
;; whenever a non-monospace face is preferred.
(add-hook 'emacs-startup-hook
    (lambda ()
        (custom-set-faces
            `(default ((t (:font "Iosevka 18"))))
            `(fixed-pitch ((t (:inherit (default)))))
            `(fixed-pitch-serif ((t (:inherit (default)))))
            `(variable-pitch ((t (:font "Iosevka Aile 18")))))))

;; line numbers on
(setq crafted-ui-display-line-numbers t)

;; Themes are color customization packages which coordinate the
;; various colors, and in some cases, font-sizes for various aspects
;; of text editing within Emacs, toolbars, tabbars and
;; modeline. Several themes are built-in to Emacs, by default,
;; Crafted Emacs uses the `deeper-blue' theme. Here is an example of
;; loading a different theme from the venerable Doom Emacs project.
(crafted-package-install-package 'doom-themes)
(progn
    (disable-theme 'deeper-blue)          ; first turn off the deeper-blue theme
    (load-theme 'doom-palenight t))       ; load the doom-palenight theme

;; To not load `custom.el' after `config.el', uncomment this line.
;; (setq crafted-load-custom-file nil)

;;; config.el ends here

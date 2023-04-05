;;; init.el --- -*- lexical-binding: t -*-

;; Author: Julio C. Villasante <jvillasantegomez@gmail.com>
;; URL: https://github.com/jvillasante/dotfiles
;; Keywords: dotfiles emacs

;;; Commentary:
;; This is my minimal Emacs configuration.

;;; Code:

;; custom variables
(custom-set-variables
    '(custom-enabled-themes '(modus-operandi))
    '(font-use-system-font t)
    '(menu-bar-mode nil)
    '(org-html-validation-link "" t)
    '(package-selected-packages
         '(sudo-edit undo-tree))
    '(show-paren-mode t)
    '(tool-bar-mode nil)
    '(delete-selection-mode t)
    '(undo-tree-visualizer-diff t)
    '(undo-tree-visualizer-relative-timestamps t))

;; melpa

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; install packages

(package-initialize)
(unless package-archive-contents
    (package-refresh-contents))
(package-install-selected-packages)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; UI settings
(setq default-cursor-type 'hbar) ;Use a minimal cursor
(setq initial-scratch-message "") ; Don't use messages that you don't read
(setq inhibit-startup-message t)
(setq visible-bell t) ; Don't let Emacs hurt your ears
(toggle-scroll-bar -1) ; no distractions
(toggle-frame-maximized) ; start maximized

;; parens pairs
(show-paren-mode 1)
(setq show-paren-delay 0)

;; windmove
(when (fboundp 'windmove-default-keybindings)
    (windmove-default-keybindings))

;; undo tree
(global-undo-tree-mode)

;; set custom face
(add-hook 'emacs-startup-hook
    (lambda ()
        (custom-set-faces
            `(default ((t (:font "Iosevka 16"))))
            `(fixed-pitch ((t (:inherit (default)))))
            `(fixed-pitch-serif ((t (:inherit (default)))))
            `(variable-pitch ((t (:font "Iosevka Aile")))))))

;; eww settings
(setq
    browse-url-browser-function 'eww-browse-url ; Use eww as the default browser
    shr-use-fonts  nil                          ; No special fonts
    shr-use-colors nil                          ; No colours
    shr-indentation 2                           ; Left-side margin
    shr-inhibit-images t
    eww-search-prefix "https://www.mojeek.com/search?hp=minimal&q=") ; Use Mojeek Minimal for search (default)

;; OTHER KEYBINDINGS
(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'init)
;;; init.el ends here

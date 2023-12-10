;;; Bare configuration -*- lexical-binding: t -*-

;; Add the NonGNU ELPA package archive
(require 'package)
(add-to-list 'package-archives
             '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(unless package-archive-contents  (package-refresh-contents))

;; Load a custom theme
(load-theme 'modus-operandi t)

;; Set default font face
(set-face-attribute 'default nil :font "Berkeley Mono 16")

;; Disable the menu bar
(menu-bar-mode -1)

;; Disable the tool bar
(tool-bar-mode -1)

;; Disable the scroll bars
(scroll-bar-mode -1)

;; Disable splash screen
(setq inhibit-startup-screen t)

;;; In-Emacs Terminal Emulation
(unless (package-installed-p 'eat)
    (package-install 'eat))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(eat)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el --- Init -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

;;; Set up the package manager

(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 29)
    (unless (package-installed-p 'use-package)
        (unless package-archive-contents
            (package-refresh-contents))
        (package-install 'use-package)))

(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

;;; Basic behaviour

(use-package delsel
    :ensure nil
    :hook (after-init . delete-selection-mode))

;;; Tweak the looks of Emacs

;; Those three belong in the early-init.el, but I am putting them here
;; for convenience.  If the early-init.el exists in the same directory
;; as the init.el, then Emacs will read+evaluate it before moving to
;; the init.el.
(menu-bar-mode 1)
(scroll-bar-mode 1)
(tool-bar-mode -1)

(let ((mono-spaced-font "Berkeley Mono")
      (proportionately-spaced-font "TX-02"))
    (set-face-attribute 'default nil :family mono-spaced-font :height 100)
    (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
    (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

(use-package modus-themes
    :ensure t
    :config
    (load-theme 'modus-operandi :no-confirm-loading))

(use-package savehist
    :ensure nil ; it is built-in
    :hook (after-init . savehist-mode))

(use-package activities
    :ensure t
    :init
    (activities-mode)
    (activities-tabs-mode)
    (setq edebug-inhibit-emacs-lisp-mode-bindings t)
    ;; :custom
    ;; (activities-bookmark-store nil)
    ;; (activities-always-persist nil)
    :bind
    (("C-x C-a C-n" . activities-new)
     ("C-x C-a C-d" . activities-define)
     ("C-x C-a C-a" . activities-resume)
     ("C-x C-a C-s" . activities-suspend)
     ("C-x C-a C-k" . activities-kill)
     ("C-x C-a RET" . activities-switch)
     ("C-x C-a b"   . activities-switch-buffer)
     ("C-x C-a g"   . activities-revert)
     ("C-x C-a l"   . activities-list)))

(provide 'init)
;;; init.el ends here

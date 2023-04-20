;;; init.el --- -*- lexical-binding: t -*-

;; Author: Julio C. Villasante <jvillasantegomez@gmail.com>
;; URL: https://github.com/jvillasante/dotfiles
;; Keywords: dotfiles emacs

;;; Commentary:
;; This is my personal Emacs configuration.

;;; Code:

;; increase gc threshold to speedup starting up
(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold most-positive-fixnum)

;; bootstrap package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package) (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t
      use-package-always-ensure t
      use-package-expand-minimally t)

;; no-littering needs to come first
(use-package no-littering
    :init
    ;; auto-save files
    (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

    ;; lock files
    (setq lock-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "lock-file/") t)))

    ;; backup all files but
    (setq backup-directory-alist
        `(("." . ,(no-littering-expand-var-file-name "backup/"))))

    ;; ... do not backup files from /tmp/
    (add-to-list 'backup-directory-alist
        (cons "^/tmp/" nil))

    ;; ... do not backup files from /dev/shm/
    (add-to-list 'backup-directory-alist
        (cons "^/dev/shm/" nil))

    ;; ... do not backup tramp files
    (with-eval-after-load 'tramp
        (add-to-list 'backup-directory-alist
            (cons tramp-file-name-regexp nil)))

    ;; custom.el into etc directory
    (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

;; exec-path-from-shell : Sane environment variables
(use-package exec-path-from-shell
    :init
    (when (daemonp)
        (exec-path-from-shell-initialize)))

;; add to load path and require
(push (expand-file-name "lisp" user-emacs-directory) load-path)
(require 'my-utils)
(require 'my-init-early)
(require 'my-init-ui)
(require 'my-init-completion)
(require 'my-init-vcs)
(require 'my-init-org)
(require 'my-init-langs)
(require 'my-init-langtools)
(require 'my-init-apps)
(require 'my-init-shell)
(require 'my-init-misc)
(require 'my-init-bindings)
(require 'my-hydras)

;; after started, stop debug on error
(setq debug-on-error nil)

;; after started up, reset GC threshold to normal.
(run-with-idle-timer 4 nil
    (lambda ()
        "Clean up gc."
        (setq gc-cons-threshold  67108864) ; 64M
        (setq gc-cons-percentage 0.1) ; original value
        (garbage-collect)))

(provide 'init)
;;; init.el ends here

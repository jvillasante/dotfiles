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
    ;; no-littering doesn't set this by default so we must place
    ;; auto save files in the same path as it uses for sessions
    (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; files and paths constants
(defconst my/home-path (expand-file-name "~/"))
(defconst my/dotfiles-path (expand-file-name "Workspace/Public/dotfiles/" my/home-path))
(defconst my/software-path (expand-file-name "Workspace/Software/" my/home-path))
(defconst my/dropbox-path (expand-file-name "Dropbox/" my/home-path))
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

;; use custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
    (load custom-file 'noerror))

(provide 'init)
;;; init.el ends here

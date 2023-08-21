;;; init.el --- -*- lexical-binding: t -*-

;; Author: Julio C. Villasante <jvillasantegomez@gmail.com>
;; URL: https://github.com/jvillasante/dotfiles
;; Keywords: dotfiles emacs

;;; Commentary:
;; This is my personal Emacs configuration.

;;; Code:

;; bootstrap package.el
(progn
    (require 'package)
    (when (version< emacs-version "28")
        (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")))
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
    (setq package-archive-priorities
          '(("gnu"    . 99)   ; prefer GNU packages
            ("nongnu" . 80)   ; use non-gnu packages if not found in GNU elpa
            ("melpa"  . 10))) ; if all else fails, get it from melpa
    (setq package-user-dir
          (expand-file-name "var/elpa" user-emacs-directory))
    (when (boundp 'package-gnupghome-dir)
        (setq package-gnupghome-dir
              (expand-file-name "var/gnupg" user-emacs-directory)))
    (setq package-install-upgrade-built-in t)
    (package-initialize)
    (unless package-archive-contents
        (package-refresh-contents)))

;; bootstrap use-package
(use-package use-package
    :ensure nil ;; emacs built-in
    :custom
    (use-package-verbose t)
    (use-package-always-ensure t)
    (use-package-expand-minimally t))

;; exec-path-from-shell : Sane environment variables
(use-package exec-path-from-shell
    :init (when (daemonp)
              (exec-path-from-shell-initialize)
              (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"
                             "CARGO_HOME" "GOPATH" "GOBIN" "NIX_SSL_CERT_FILE" "NIX_PATH"))
                  (exec-path-from-shell-copy-env var))))

;; no-littering needs to come first
(use-package no-littering)

;; config changes made through the customize UI will be stored here
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(when (file-exists-p custom-file)
    (load custom-file))

;; Paths
(defconst my/home-path (expand-file-name "~/"))
(defconst my/dotfiles-path (expand-file-name "Workspace/Public/dotfiles/" my/home-path))
(defconst my/software-path (expand-file-name "Workspace/Software/" my/home-path))
(defconst my/dropbox-path (expand-file-name "Dropbox/" my/home-path))
(push (expand-file-name "lisp" user-emacs-directory) load-path)

;; load config
(require 'my-utils)
(require 'my-init-early)
(require 'my-init-ui)
(require 'my-init-completion)
(require 'my-init-vcs)
(require 'my-init-org)
(require 'my-init-langs)
(require 'my-init-lang-tools)
(require 'my-init-apps)
(require 'my-init-shell)
(require 'my-init-misc)
(require 'my-init-filemanager)
(require 'my-init-workspaces)
(require 'my-init-modal)
(require 'my-init-bindings)
(require 'modus-themes-exporter)

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

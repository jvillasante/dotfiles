;;; init.el --- Init -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; set EDITOTR to `emacsclient' as early as possible
(setf (getenv "EDITOR") "emacsclient")

;; bootstrap package.el
(require 'package)
(when (version< emacs-version "28")
    (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(setq package-archive-priorities
      '(("gnu"           . 99)
        ("nongnu"        . 80)
        ("melpa-stable"  . 60)
        ("melpa"         . 40)))
(setq package-user-dir (expand-file-name "var/elpa" user-emacs-directory))
(when (boundp 'package-gnupghome-dir)
    (setq package-gnupghome-dir
          (expand-file-name "var/gnupg" user-emacs-directory)))
(setq package-install-upgrade-built-in t)
(when package-enable-at-startup (package-initialize))
(unless package-archive-contents (package-refresh-contents))

;; bootstrap use-package
(use-package use-package
    :ensure nil ;; emacs built-in
    :custom ((use-package-verbose t)
             (use-package-always-ensure t)
             (use-package-expand-minimally t)))

;; exec-path-from-shell : Sane environment variables
(use-package exec-path-from-shell
    :init
    (when (daemonp)
        (exec-path-from-shell-initialize)
        (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"
                       "CARGO_HOME" "GOPATH" "GOBIN" "NIX_SSL_CERT_FILE" "NIX_PATH" "VCPKG_ROOT"))
            (exec-path-from-shell-copy-env var))))

;; Idle garbage collection
(use-package gcmh
    :config
    ;; (setopt garbage-collection-messages t)
    ;; (setopt gcmh-high-cons-threshold (* 256 1000 1000))
    ;; (setopt gcmh-low-cons-threshold (* 16 1000 1000))
    ;; (setopt gcmh-idle-delay 3)
    ;; (setopt gcmh-verbose t)
    ;; (setopt gc-cons-percentage 0.2)
    (add-hook 'after-init-hook #'gcmh-mode))

;; no-littering needs to come first
(use-package no-littering
    :config
    (setq my/var-dir no-littering-var-directory)
    (setq my/etc-dir no-littering-etc-directory))

;; Paths
(defconst my/home-path (expand-file-name "~/"))
(defconst my/dotfiles-path (expand-file-name "Workspace/Public/dotfiles/" my/home-path))
(defconst my/software-path (expand-file-name "Workspace/Software/" my/home-path))
(defconst my/dropbox-path (expand-file-name "Dropbox/" my/home-path))

;; load config
(load (expand-file-name "Common/emacs/elisp/my-utils"              my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-early"         my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-completion"    my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-vcs"           my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-org"           my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-langs"         my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-lang-tools"    my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-docker"        my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-apps"          my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-shell"         my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-misc"          my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-filemanager"   my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-workspaces"    my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-modal"         my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-ui"            my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-ai"            my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-bindings"      my/dotfiles-path))
;; (load (expand-file-name "Common/emacs/elisp/my-init-transient"     my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/modus-themes-exporter" my/dotfiles-path))

(provide 'init)
;;; init.el ends here

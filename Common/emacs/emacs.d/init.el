;;; init.el --- Init -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; package.el
(require 'package)
(package-initialize)
(when (< emacs-major-version 29)
    (unless (package-installed-p 'use-package)
        (unless package-archive-contents
            (package-refresh-contents))
        (package-install 'use-package)))

;; use-package : macro that allows to isolate package configuration
(use-package use-package
    :ensure nil ;; emacs built-in
    :custom ((use-package-verbose t)
             (use-package-compute-statistics nil) ; use `use-package-report'
             (use-package-always-ensure t)
             (use-package-expand-minimally t)))

;; exec-path-from-shell : Sane environment variables
(use-package exec-path-from-shell
    :config
    (when (daemonp)
        (add-to-list 'exec-path-from-shell-variables "SSH_AUTH_SOCK")
        (add-to-list 'exec-path-from-shell-variables "SSH_AGENT_PID")
        (add-to-list 'exec-path-from-shell-variables "GPG_AGENT_INFO")
        (add-to-list 'exec-path-from-shell-variables "LANG")
        (add-to-list 'exec-path-from-shell-variables "LC_CTYPE")
        ;; (add-to-list 'exec-path-from-shell-variables "CARGO_HOME")
        ;; (add-to-list 'exec-path-from-shell-variables "GOPATH")
        ;; (add-to-list 'exec-path-from-shell-variables "GOBIN")
        ;; (add-to-list 'exec-path-from-shell-variables "NIX_SSL_CERT_FILE")
        ;; (add-to-list 'exec-path-from-shell-variables "NIX_PATH")
        (exec-path-from-shell-initialize)))

;; gcmh : to optimize gc
(use-package gcmh
    :custom (gcmh-high-cons-threshold (* 128 1024 1024))
    :hook (after-init . gcmh-mode))

;; Paths used throughout
(defconst my/home-path (expand-file-name "~/"))
(defconst my/dotfiles-path (expand-file-name "Workspace/Public/dotfiles/" my/home-path))
(defconst my/software-path (expand-file-name "Workspace/Software/" my/home-path))
(defconst my/dropbox-path (expand-file-name "Dropbox/" my/home-path))

;; Telling Emacs where the C source code is let's us jump all the way down into
;; primitive functions when exploring elisp functions.
(setq source-directory (expand-file-name "emacs/" my/software-path))
(setq find-function-C-source-directory (expand-file-name "emacs/src/" my/software-path))

;; load custom config
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
(load (expand-file-name "Common/emacs/elisp/my-init-transient"     my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-bindings"      my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/modus-themes-exporter" my/dotfiles-path))

(provide 'init)
;;; init.el ends here

;;; init.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
    (load custom-file nil 'nomessage))

;; set EDITOTR to `emacsclient' as early as possible
(setf (getenv "EDITOR") "emacsclient")

;; bootstrap package.el
(require 'package)
(when (version< emacs-version "28")
    (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/") t)
(setq package-archive-priorities
      '(("gnu"      . 99)   ; prefer GNU packages
        ("nongnu"   . 80)   ; use non-gnu packages if not found in GNU elpa
        ("melpa"    . 10)   ; if all else fails, get it from melpa
        ("jcs-elpa" . 0)))  ; if all else fails, get it from jcs-elpa
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
    :custom
    (use-package-verbose t)
    (use-package-always-ensure t)
    (use-package-expand-minimally t))

;; bootstrap use-package vc
(when (version<= "29.0" emacs-version)
    ;; will be able to remove the following package-vc-install as of emacs30
    ;; as this will be built-in
    (unless (package-installed-p 'vc-use-package)
        (package-vc-install "https://github.com/slotThe/vc-use-package"))

    ;; now use-package has the :vc keyword!
    ;; (use-package org-ql
    ;;     :defer t
    ;;     :vc (:fetcher github :repo "alphapapa/org-ql"))
    ;; (use-package ada-mode
    ;;     :vc (:fetcher github :repo "captainflasmr/old-ada-mode"))
    ;; (use-package kbd-mode
    ;;     :vc (:fetcher github :repo "kmonad/kbd-mode")
    ;;     :custom
    ;;     (kbd-mode-kill-kmonad "pkill -9 kmonad")
    ;;     (kbd-mode-start-kmonad "kmonad ~/.config/kmonad/keyboard.kbd"))
    )


;; exec-path-from-shell : Sane environment variables
(use-package exec-path-from-shell
    :functions exec-path-from-shell-initialize exec-path-from-shell-copy-env
    :init
    (when (daemonp)
        (exec-path-from-shell-initialize)
        (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"
                       "CARGO_HOME" "GOPATH" "GOBIN" "NIX_SSL_CERT_FILE" "NIX_PATH"))
            (exec-path-from-shell-copy-env var))))

;; no-littering needs to come first
(use-package no-littering)

;; Paths
(defconst my--home-path (expand-file-name "~/"))
(defconst my--dotfiles-path (expand-file-name "Workspace/Public/dotfiles/" my--home-path))
(defconst my--software-path (expand-file-name "Workspace/Software/" my--home-path))
(defconst my--dropbox-path (expand-file-name "Dropbox/" my--home-path))

;; load config
(load (expand-file-name "lisp/my-utils" user-emacs-directory))
(load (expand-file-name "lisp/my-init-early" user-emacs-directory))
(load (expand-file-name "lisp/my-init-completion" user-emacs-directory))
(load (expand-file-name "lisp/my-init-vcs" user-emacs-directory))
(load (expand-file-name "lisp/my-init-org" user-emacs-directory))
(load (expand-file-name "lisp/my-init-langs" user-emacs-directory))
(load (expand-file-name "lisp/my-init-lang-tools" user-emacs-directory))
(load (expand-file-name "lisp/my-init-apps" user-emacs-directory))
(load (expand-file-name "lisp/my-init-shell" user-emacs-directory))
(load (expand-file-name "lisp/my-init-misc" user-emacs-directory))
(load (expand-file-name "lisp/my-init-filemanager" user-emacs-directory))
(load (expand-file-name "lisp/my-init-workspaces" user-emacs-directory))
(load (expand-file-name "lisp/my-init-modal" user-emacs-directory))
(load (expand-file-name "lisp/my-init-ui" user-emacs-directory))
(load (expand-file-name "lisp/my-init-ai" user-emacs-directory))
(load (expand-file-name "lisp/my-init-bindings" user-emacs-directory))
(load (expand-file-name "lisp/my-init-transient" user-emacs-directory))
(load (expand-file-name "lisp/modus-themes-exporter" user-emacs-directory))

(provide 'init)
;;; init.el ends here

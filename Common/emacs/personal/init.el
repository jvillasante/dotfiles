;;; init.el --- Init -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; custom
;; (setq custom-file (expand-file-name "etc/custom.el" user-emacs-directory))
;; (when (and custom-file
;;            (file-exists-p custom-file))
;;     (load custom-file nil 'nomessage))
(setq custom-file null-device)

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
    :custom ((use-package-verbose t)
             (use-package-always-ensure t)
             (use-package-expand-minimally t)))

;; exec-path-from-shell : Sane environment variables
(use-package exec-path-from-shell
    :init
    (when (daemonp)
        (exec-path-from-shell-initialize)
        (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"
                       "CARGO_HOME" "GOPATH" "GOBIN" "NIX_SSL_CERT_FILE" "NIX_PATH"))
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
(use-package no-littering)

;; Paths
(defconst my--home-path (expand-file-name "~/"))
(defconst my--dotfiles-path (expand-file-name "Workspace/Public/dotfiles/" my--home-path))
(defconst my--software-path (expand-file-name "Workspace/Software/" my--home-path))
(defconst my--dropbox-path (expand-file-name "Dropbox/" my--home-path))

;; load config
(load (expand-file-name "elisp/my-utils"              no-littering-etc-directory))
(load (expand-file-name "elisp/my-init-early"         no-littering-etc-directory))
(load (expand-file-name "elisp/my-init-completion"    no-littering-etc-directory))
(load (expand-file-name "elisp/my-init-vcs"           no-littering-etc-directory))
(load (expand-file-name "elisp/my-init-org"           no-littering-etc-directory))
(load (expand-file-name "elisp/my-init-langs"         no-littering-etc-directory))
(load (expand-file-name "elisp/my-init-lang-tools"    no-littering-etc-directory))
(load (expand-file-name "elisp/my-init-apps"          no-littering-etc-directory))
(load (expand-file-name "elisp/my-init-shell"         no-littering-etc-directory))
(load (expand-file-name "elisp/my-init-misc"          no-littering-etc-directory))
(load (expand-file-name "elisp/my-init-filemanager"   no-littering-etc-directory))
(load (expand-file-name "elisp/my-init-workspaces"    no-littering-etc-directory))
(load (expand-file-name "elisp/my-init-modal"         no-littering-etc-directory))
(load (expand-file-name "elisp/my-init-ui"            no-littering-etc-directory))
(load (expand-file-name "elisp/my-init-ai"            no-littering-etc-directory))
(load (expand-file-name "elisp/my-init-bindings"      no-littering-etc-directory))
(load (expand-file-name "elisp/my-init-transient"     no-littering-etc-directory))
(load (expand-file-name "elisp/modus-themes-exporter" no-littering-etc-directory))

(provide 'init)
;;; init.el ends here

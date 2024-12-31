;;; init.el --- Init -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; package.el
(progn
    (require 'package)

    ;; Initialize and refresh package contents again if needed
    (package-initialize)
    (unless package-archive-contents
        (package-refresh-contents))

    ;; Install use-package if necessary
    (unless (package-installed-p 'use-package)
        (package-install 'use-package))

    ;; Ensure use-package is available at compile time
    (eval-when-compile
        (require 'use-package)
        (use-package use-package
            :ensure nil ;; emacs built-in
            :custom ((use-package-verbose t)
                     (use-package-always-ensure t)
                     (use-package-expand-minimally t)))))

(setq make-backup-files nil)

;; exec-path-from-shell : Sane environment variables
(use-package exec-path-from-shell
    :init
    (when (daemonp)
        (exec-path-from-shell-initialize)
        (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"
                       "CARGO_HOME" "GOPATH" "GOBIN" "NIX_SSL_CERT_FILE" "NIX_PATH" "VCPKG_ROOT"))
            (exec-path-from-shell-copy-env var))))

;; Paths
(defconst my/home-path (expand-file-name "~/"))
(defconst my/dotfiles-path (expand-file-name "Workspace/Public/dotfiles/" my/home-path))
(defconst my/software-path (expand-file-name "Workspace/Software/" my/home-path))
(defconst my/dropbox-path (expand-file-name "Dropbox/" my/home-path))

;; load config
(load (expand-file-name "Common/emacs/elisp/my-utils"              my/dotfiles-path))
;; (load (expand-file-name "Common/emacs/elisp/my-init-early"         my/dotfiles-path))
;; (load (expand-file-name "Common/emacs/elisp/my-init-completion"    my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-vcs"           my/dotfiles-path))
;; (load (expand-file-name "Common/emacs/elisp/my-init-org"           my/dotfiles-path))
;; (load (expand-file-name "Common/emacs/elisp/my-init-langs"         my/dotfiles-path))
;; (load (expand-file-name "Common/emacs/elisp/my-init-lang-tools"    my/dotfiles-path))
;; (load (expand-file-name "Common/emacs/elisp/my-init-docker"        my/dotfiles-path))
;; (load (expand-file-name "Common/emacs/elisp/my-init-apps"          my/dotfiles-path))
;; (load (expand-file-name "Common/emacs/elisp/my-init-shell"         my/dotfiles-path))
;; (load (expand-file-name "Common/emacs/elisp/my-init-misc"          my/dotfiles-path))
;; (load (expand-file-name "Common/emacs/elisp/my-init-filemanager"   my/dotfiles-path))
;; (load (expand-file-name "Common/emacs/elisp/my-init-workspaces"    my/dotfiles-path))
;; (load (expand-file-name "Common/emacs/elisp/my-init-modal"         my/dotfiles-path))
;; (load (expand-file-name "Common/emacs/elisp/my-init-ui"            my/dotfiles-path))
;; (load (expand-file-name "Common/emacs/elisp/my-init-ai"            my/dotfiles-path))
;; (load (expand-file-name "Common/emacs/elisp/my-init-transient"     my/dotfiles-path))
;; (load (expand-file-name "Common/emacs/elisp/my-init-bindings"      my/dotfiles-path))
;; (load (expand-file-name "Common/emacs/elisp/modus-themes-exporter" my/dotfiles-path))

(use-package activities
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

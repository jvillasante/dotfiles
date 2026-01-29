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
             (use-package-vc-prefer-newest t)
             (use-package-compute-statistics nil) ; use `use-package-report'
             (use-package-always-ensure t)
             (use-package-expand-minimally t)))

;; Paths used throughout
(defconst my-home-path         (expand-file-name "~/"))
(defconst my-dotfiles-path     (expand-file-name "Workspace/Public/dotfiles/" my-home-path))
(defconst my-software-path     (expand-file-name "Workspace/Software/"        my-home-path))
(defconst my-dropbox-path      (expand-file-name "Dropbox/"                   my-home-path))

;; Set environment variables - `exec-path-from-shell' is too slow!
(progn
    (setq my-exec-path-list
          (list (expand-file-name ".go/bin" my-home-path)
                (expand-file-name ".cargo/bin" my-home-path)
                (expand-file-name ".local/bin" my-home-path)
                "/usr/local/bin"
                "/usr/local/sbin"
                "/usr/bin"
                "/usr/sbin"
                "/bin"
                "/sbin"))

    ;; Set the `$PATH' environment variable
    (setenv "PATH" (mapconcat 'identity my-exec-path-list path-separator))

    ;; Set the Emacs-internal `exec-path'
    (setq exec-path (append (parse-colon-path (getenv "PATH")) (list exec-directory))))

;; Telling Emacs where the C source code is let's us jump all the way down into
;; primitive functions when exploring elisp functions.
(setq source-directory (expand-file-name "emacs/" my-software-path))
(setq find-function-C-source-directory (expand-file-name "emacs/src/" my-software-path))

;; load custom config
(load (expand-file-name "Common/emacs/elisp/my-utils"              my-dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-early"         my-dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-completion"    my-dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-vcs"           my-dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-org"           my-dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-langs"         my-dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-lang-tools"    my-dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-docker"        my-dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-apps"          my-dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-shell"         my-dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-misc"          my-dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-filemanager"   my-dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-workspaces"    my-dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-modal"         my-dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-ui"            my-dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-ai"            my-dotfiles-path))
;; (load (expand-file-name "Common/emacs/elisp/my-init-icons"         my-dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-transient"     my-dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-bindings"      my-dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/modus-themes-exporter" my-dotfiles-path))

(provide 'init)
;;; init.el ends here

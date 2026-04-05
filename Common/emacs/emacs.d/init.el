;;; init.el --- Init -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; package.el
(require 'package)
(package-initialize)

;; use-package : macro that allows to isolate package configuration
(use-package use-package
    :ensure nil ;; emacs built-in
    :custom ((use-package-vc-prefer-newest t)
             (use-package-compute-statistics nil) ; use `use-package-report'
             (use-package-always-ensure t)
             (use-package-expand-minimally t)))

;; Paths used throughout
(defconst my/home-path         (expand-file-name "~/"))
(defconst my/dotfiles-path     (expand-file-name "Workspace/Public/dotfiles/" my/home-path))
(defconst my/work-path         (expand-file-name "Workspace/Work/Omicron/"    my/home-path))
(defconst my/software-path     (expand-file-name "Workspace/Software/"        my/home-path))
(defconst my/dropbox-path      (expand-file-name "Dropbox/"                   my/home-path))

;; Set environment variables - `exec-path-from-shell' is too slow!
(setq my/exec-path-list
      (list (expand-file-name ".go/bin" my/home-path)
            (expand-file-name ".cargo/bin" my/home-path)
            (expand-file-name ".local/bin" my/home-path)
            "/usr/local/bin"
            "/usr/local/sbin"
            "/usr/bin"
            "/usr/sbin"
            "/bin"
            "/sbin"))

;; Set the `$PATH' environment variable
(setenv "PATH" (mapconcat #'identity my/exec-path-list path-separator))

;; Set the Emacs-internal `exec-path'
(setq exec-path (append (parse-colon-path (getenv "PATH")) (list exec-directory)))

;; Telling Emacs where the C source code is let's us jump all the way down into
;; primitive functions when exploring elisp functions.
(setq source-directory (expand-file-name "emacs/" my/software-path))
(setq find-function-C-source-directory (expand-file-name "emacs/src/" my/software-path))

;; load custom config
(load (expand-file-name "my-utils"              my/lisp-dir))
(load (expand-file-name "my-init-early"         my/lisp-dir))
(load (expand-file-name "my-init-completion"    my/lisp-dir))
(load (expand-file-name "my-init-vcs"           my/lisp-dir))
(load (expand-file-name "my-init-org"           my/lisp-dir))
(load (expand-file-name "my-init-langs"         my/lisp-dir))
(load (expand-file-name "my-init-lang-tools"    my/lisp-dir))
;; (load (expand-file-name "my-init-docker"        my/lisp-dir))
(load (expand-file-name "my-init-apps"          my/lisp-dir))
(load (expand-file-name "my-init-shell"         my/lisp-dir))
(load (expand-file-name "my-init-misc"          my/lisp-dir))
(load (expand-file-name "my-init-filemanager"   my/lisp-dir))
(load (expand-file-name "my-init-workspaces"    my/lisp-dir))
(load (expand-file-name "my-init-modal"         my/lisp-dir))
(load (expand-file-name "my-init-ui"            my/lisp-dir))
(load (expand-file-name "my-init-ai"            my/lisp-dir))
;; (load (expand-file-name "my-init-icons"         my/lisp-dir))
;; (load (expand-file-name "my-init-transient"     my/lisp-dir))
(load (expand-file-name "my-init-bindings"      my/lisp-dir))

(provide 'init)
;;; init.el ends here

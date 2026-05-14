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
(load (expand-file-name "my-init-mail"          my/lisp-dir))
(load (expand-file-name "my-init-bindings"      my/lisp-dir))

(provide 'init)
;;; init.el ends here

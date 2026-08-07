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

;; compile-angel : speed up Emacs by Byte-compiling and Native-compiling all Elisp files
(use-package compile-angel
    :config
    ;; The following disables compilation of packages during installation;
    ;; compile-angel will handle it.
    (setq package-native-compile nil)

    ;; Set `compile-angel-verbose' to nil to disable compile-angel messages.
    ;; (When set to nil, compile-angel won't show which file is being compiled.)
    (setq compile-angel-verbose t)

    ;; The following directive prevents compile-angel from compiling your init
    ;; files.
    (push "/init.el" compile-angel-excluded-path-suffixes)
    (push "/early-init.el" compile-angel-excluded-path-suffixes)
    (push "/custom.el" compile-angel-excluded-path-suffixes)

    ;; Uncomment the line below to compile automatically when an Elisp file is saved
    ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)

    ;; A global mode that compiles .el files when they are loaded
    ;; using `load' or `require'.
    (compile-angel-on-load-mode 1))

;; load custom config
(load (expand-file-name "my-utils"              my/lisp-dir))
(load (expand-file-name "my-init-early"         my/lisp-dir))
(load (expand-file-name "my-init-completion"    my/lisp-dir))
(load (expand-file-name "my-init-vcs"           my/lisp-dir))
(load (expand-file-name "my-init-org"           my/lisp-dir))
(load (expand-file-name "my-init-langs"         my/lisp-dir))
(load (expand-file-name "my-init-lang-tools"    my/lisp-dir))
(load (expand-file-name "my-init-apps"          my/lisp-dir))
(load (expand-file-name "my-init-shell"         my/lisp-dir))
(load (expand-file-name "my-init-misc"          my/lisp-dir))
(load (expand-file-name "my-init-filemanager"   my/lisp-dir))
(load (expand-file-name "my-init-filetree"      my/lisp-dir))
(load (expand-file-name "my-init-workspaces"    my/lisp-dir))
(load (expand-file-name "my-init-modal"         my/lisp-dir))
(load (expand-file-name "my-init-ui"            my/lisp-dir))
(load (expand-file-name "my-init-ai"            my/lisp-dir))
(load (expand-file-name "my-init-mail"          my/lisp-dir))
(load (expand-file-name "my-emacs-anywhere"     my/lisp-dir))
;; (load (expand-file-name "my-init-docker"        my/lisp-dir))
;; (load (expand-file-name "my-init-icons"         my/lisp-dir))
;; (load (expand-file-name "my-init-transient"     my/lisp-dir))
(load (expand-file-name "my-init-bindings"      my/lisp-dir))

(provide 'init)
;;; init.el ends here

;;; my-ide.el --- Custom IDE Configs -*- lexical-binding: t; -*-

;;; Commentary:
;;; IDE Customizations

;;; Code:

;; Project.el enhancements
(with-eval-after-load 'project
    (defcustom project-root-markers
        '("Cargo.toml" "compile_commands.json" "compile_flags.txt"
             "project.clj" ".git" "deps.edn" "shadow-cljs.edn")
        "Files or directories that indicate the root of a project."
        :type '(repeat string)
        :group 'project)

    (defun project-root-p (path)
        "Check if the current PATH has any of the project root markers."
        (catch 'found
            (dolist (marker project-root-markers)
                (when (file-exists-p (concat path marker))
                    (throw 'found marker)))))

    (defun project-find-root (path)
        "Search up the PATH for `project-root-markers'."
        (when-let ((root (locate-dominating-file path #'project-root-p)))
            (cons 'transient (expand-file-name root))))
    (add-to-list 'project-find-functions #'project-find-root)

    (defun project-save-some-buffers (&optional arg)
        "Save some modified file-visiting buffers in the current project.

Optional argument ARG (interactively, prefix argument) non-nil
means save all with no questions."
        (interactive "P")
        (let* ((project-buffers (project-buffers (project-current)))
                  (pred (lambda () (memq (current-buffer) project-buffers))))
            (funcall-interactively #'save-some-buffers arg pred)))

    (define-advice project-compile (:around (fn) save-project-buffers)
        "Only ask to save project-related buffers."
        (let* ((project-buffers (project-buffers (project-current)))
                  (compilation-save-buffers-predicate
                      (lambda () (memq (current-buffer) project-buffers))))
            (funcall fn)))

    (define-advice recompile (:around (fn &optional edit-command) save-project-buffers)
        "Only ask to save project-related buffers if inside a project."
        (if (project-current)
            (let* ((project-buffers (project-buffers (project-current)))
                      (compilation-save-buffers-predicate
                          (lambda () (memq (current-buffer) project-buffers))))
                (funcall fn edit-command))
            (funcall fn edit-command))))

;; Eglot
(setc eldoc-echo-area-use-multiline-p t)
(with-eval-after-load "eglot"
    (with-eval-after-load "eglot"
        (setc eglot-extend-to-xref t)
        (setc eglot-ignored-server-capabilities
            (quote (:documentFormattingProvider :documentRangeFormattingProvider)))
        (add-to-list 'eglot-server-programs
            `(rust-mode . ("rust-analyzer"))
            `(c-mode c++-mode
                 . ("/usr/bin/clangd"
                       "-j=4"
                       "--malloc-trim"
                       "--log=error"
                       "--background-index"
                       "--clang-tidy"
                       "--cross-file-rename"
                       "--completion-style=detailed"
                       "--pch-storage=memory"
                       "--header-insertion=never"
                       "--header-insertion-decorators=0")))))

;; C++
(progn
    (setc c-default-style "linux")
    (setc c-basic-offset 4)
    (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.C\\'" . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))
    (add-hook 'c-mode-common-hook
        (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
                (eglot-ensure)))))

;; rustic : rust mode
(crafted-package-install-package 'rustic)
(progn
    (setc rustic-format-on-save nil)
    (add-hook 'rustic-mode-hook #'eglot-ensure))

;; format-all : auto format source code
(crafted-package-install-package 'format-all)
(progn
    (add-hook 'c-mode-hook #'format-all-mode)
    (add-hook 'c++-mode-hook #'format-all-mode)
    (add-hook 'python-mode-hook #'format-all-mode)
    (add-hook 'rustic-mode-hook #'format-all-mode)
    (add-hook 'format-all-mode-hook #'format-all-ensure-formatter)
    (custom-set-variables
        '(format-all-formatters (quote (("C++" clang-format)
                                           ("Python" black))))))

(provide 'my-ide)
;;; my-ide.el ends here

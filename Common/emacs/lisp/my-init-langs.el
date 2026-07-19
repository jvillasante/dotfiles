;;; my-init-langs.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; treesit : tree-sitter utilities
(use-package treesit
    :ensure nil ;; emacs built-in
    :bind (:map prog-mode-map
              ("M-<up>" . treesit-beginning-of-defun)
              ("M-<down>" . treesit-end-of-defun))
    :custom
    (treesit-extra-load-path
        `(,(expand-file-name "tree-sitter" my/var-dir)))
    (treesit-auto-install-grammar 'ask)
    (treesit-enabled-modes t)
    (treesit-font-lock-level 4))

(use-package elisp-mode
    :ensure nil ;; emacs built-in
    :defer t
    :bind (("C-h ." . helpful-at-point))
    :custom (lisp-body-indent 4))

(use-package elisp-demos
    :after elisp-mode
    :init
    ;; (advice-add #'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
    (advice-add #'helpful-update :after #'elisp-demos-advice-helpful-update))

;; c/c++
(use-package c-ts-mode
    :ensure nil ;; emacs built-in
    :preface
    (defun my/c-ts-indent-style()
        "Override the built-in BSD indentation style with some additional rules.
         Docs: https://www.gnu.org/software/emacs/manual/html_node/elisp/Parser_002dbased-Indentation.html
         Notes: `treesit-explore-mode' can be very useful to see where you're at in the tree-sitter tree,
                especially paired with `(setq treesit--indent-verbose t)' to debug what rules is being
                applied at a given point."
        (let ((my/rules '(;; do not indent preprocessor statements
                             ((node-is "preproc") column-0 0)
                             ;; do not indent namespace children (C++ only, harmless in C)
                             ((n-p-gp nil "declaration_list" "namespace_definition") parent-bol 0))))
            (if (>= emacs-major-version 31)
                ;; Emacs 31+: rules are per-language; must cover both c and cpp
                (let ((c-bsd-rules   (cdar (c-ts-mode--simple-indent-rules 'c 'bsd)))
                         (cpp-bsd-rules (cdar (c-ts-mode--simple-indent-rules 'cpp 'bsd))))
                    `((c   ,@my/rules ,@c-bsd-rules)
                         (cpp ,@my/rules ,@cpp-bsd-rules)))
                ;; Emacs 30: flat rule list, same rules work for both C and C++
                (let ((bsd-rules (alist-get 'bsd (c-ts-mode--indent-styles 'cpp))))
                    `(,@my/rules ,@bsd-rules)))))
    :mode (("\\.c\\'"   . c-ts-mode)
              ("\\.C\\'"   . c-or-c++-ts-mode)
              ("\\.h\\'"   . c-or-c++-ts-mode)
              ("\\.H\\'"   . c-or-c++-ts-mode)
              ("\\.inc\\'" . c-or-c++-ts-mode)
              ("\\.hpp\\'" . c++-ts-mode)
              ("\\.HPP\\'" . c++-ts-mode)
              ("\\.cpp\\'" . c++-ts-mode)
              ("\\.CPP\\'" . c++-ts-mode))
    :bind (:map c-ts-base-mode-map
              ("C-x C-o" . my/eglot-clangd-find-other-file))
    :custom
    (c-ts-mode-indent-offset 4)
    (c-ts-mode-indent-style #'my/c-ts-indent-style))

;; cmake
(use-package cmake-ts-mode
    :ensure nil
    :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; adoc-mode : ascii docs
(use-package adoc-mode
    :defer t
    :mode "\\.adoc\\'")

;; csv-mode : Support for csv files (use csv-align-mode for alignment)
(use-package csv-mode
    :defer t
    :mode "\\.csv\\'"
    :hook ((csv-mode . csv-align-mode)
              (csv-mode . (lambda () (setq-local truncate-lines t)))))

;; yaml
(use-package yaml-ts-mode
    :ensure nil
    :mode ("\\.yml\\'" "\\.yaml\\'" "\\.clangd\\'"))

;; web-mode : Support various web files
(use-package web-mode
    :defer t
    :mode ("\\.html?\\'"
              "\\.html\\.twig\\'"
              "\\.phtml\\'"
              "\\.tpl\\.php\\'")
    :custom ((web-mode-script-padding 0) ; For vue.js SFC : no initial padding in the script section
                (web-mode-markup-indent-offset 2)
                (web-mode-css-indent-offset 2)
                (web-mode-code-indent-offset 2)))

;; php
(use-package php-ts-mode
    :ensure nil
    :mode "\\.php\\'")

(use-package python
    :defer t
    :custom
    (python-indent-offset 4)
    (python-shell-interpreter "ipython3")
    (python-shell-interpreter-args "-i --simple-prompt --no-color-info"))

(use-package markdown-mode
    :defer t
    :mode (("\\.[Rr]md\\'" . markdown-mode)
              ("\\.qmd\\'" . markdown-mode))
    :bind (:map markdown-mode-map
              ("TAB" . markdown-cycle))
    :custom
    (markdown-command "multimarkdown")
    (markdown-fontify-code-blocks-natively t)
    (markdown-fontify-whole-heading-line t)
    (markdown-enable-math t))

(use-package go-ts-mode
    :ensure nil
    :hook (go-ts-mode . (lambda () (setq-local tab-width 4))))

(use-package sql
    :defer t
    :hook (sql-mode . (lambda () (setq-local tab-width 4))))

;; zig
(use-package zig-mode :defer t)

;; TypeScript
(use-package typescript-ts-mode
    :ensure nil
    :mode ("\\.ts\\'" "\\.tsx\\'"))

;; js is everywhere
(use-package js
    :ensure nil
    :mode ("\\.js\\'" . js-ts-mode)
    :hook (js-ts-mode . (lambda () (push '("function" . ?ƒ) prettify-symbols-alist)))
    :custom (js-indent-level 2))

(provide 'my-init-langs)
;;; my-init-langs.el ends here

;;; my-init-langs.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;; Waiting until this gets more traction, *-mode-ts feels very weird to me!
;;
;; Install Language Grammars:
;;  - Call the command `M-x treesit-install-language-grammar' for each language.
;;  - Alternativelly, evaluate the following to install them all:
;;    (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
;; Check if grammar is working:
;;    (treesit-language-available-p 'python)
;;    (treesit-language-available-p 'cpp)
;; List all known tree-sitter major modes:
;;    C-h a -ts-mode$
;; Give `treesit-explore-mode' or `treesit-inspect-mode' a try!
(use-package treesit
    :ensure nil ;; emacs built-in
    :preface
    (defun my/setup-install-grammars ()
        "Install Tree-sitter grammars if they are absent."
        (interactive)
        (dolist (grammar
                 '((bash       "https://github.com/tree-sitter/tree-sitter-bash")
                   (c          "https://github.com/tree-sitter/tree-sitter-c/" "master" "src")
                   (clojure    "https://github.com/sogaiu/tree-sitter-clojure" "master" "src")
                   (cpp        "https://github.com/tree-sitter/tree-sitter-cpp/" "v0.22.0" "src")
                   (cmake      "https://github.com/uyha/tree-sitter-cmake")
                   (css        "https://github.com/tree-sitter/tree-sitter-css")
                   (dockerfile "file:///opt/src/github/tree-sitter-dockerfile" "main" "src")
                   (elisp      "https://github.com/Wilfred/tree-sitter-elisp")
                   (elixir     "https://github.com/elixir-lang/tree-sitter-elixir" "main" "src")
                   (erlang     "https://github.com/WhatsApp/tree-sitter-erlang" "main" "src")
                   (go         "https://github.com/tree-sitter/tree-sitter-go")
                   (haskell    "https://github.com/tree-sitter/tree-sitter-haskell" "master" "src")
                   (html       "https://github.com/tree-sitter/tree-sitter-html")
                   (java       "https://github.com/tree-sitter/tree-sitter-java" "master" "src")
                   (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
                   (json       "https://github.com/tree-sitter/tree-sitter-json")
                   (julia      "https://github.com/tree-sitter/tree-sitter-julia" "master" "src")
                   (lua        "https://github.com/MunifTanjim/tree-sitter-lua" "main" "src")
                   (make       "https://github.com/alemuller/tree-sitter-make")
                   (markdown   "https://github.com/ikatyang/tree-sitter-markdown")
                   (meson      "https://github.com/Decodetalkers/tree-sitter-meson" "master" "src")
                   (python     "https://github.com/tree-sitter/tree-sitter-python")
                   (ruby       "https://github.com/tree-sitter/tree-sitter-ruby" "master" "src")
                   (rust       "https://github.com/tree-sitter/tree-sitter-rust" "master" "src")
                   (toml       "https://github.com/tree-sitter/tree-sitter-toml")
                   (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
                   (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
                   (yaml       "https://github.com/ikatyang/tree-sitter-yaml")))
            (add-to-list 'treesit-language-source-alist grammar)
            ;; Only install `grammar' if we don't already have it
            ;; installed. However, if you want to *update* a grammar then
            ;; this obviously prevents that from happening.
            (unless (treesit-language-available-p (car grammar))
                (treesit-install-language-grammar (car grammar)))))
    ;; Optional, but recommended. Tree-sitter enabled major modes are
    ;; distinct from their ordinary counterparts.
    ;;
    ;; You can remap major modes with `major-mode-remap-alist'. Note
    ;; that this does *not* extend to hooks! Make sure you migrate them
    ;; also
    (dolist (mapping '((bash-mode . bash-ts-mode)
                       (python-mode . python-ts-mode)
                       (css-mode . css-ts-mode)
                       (javascript-mode . js-ts-mode)
                       (typescript-mode . tsx-ts-mode)
                       (js-mode . js-ts-mode)
                       (js-json-mode . json-ts-mode)
                       (css-mode . css-ts-mode)
                       (yaml-mode . yaml-ts-mode)
                       (c-mode . c-ts-mode)
                       (c++-mode . c++-ts-mode)
                       (c-or-c++-mode . c-or-c++-ts-mode)))
        (add-to-list 'major-mode-remap-alist mapping))
    :config
    (my/setup-install-grammars)
    (setq treesit-font-lock-level 4))

;; treesit : treesitter for Emacs
;; (use-package treesit
;;     :ensure nil ;; emacs built-in
;;     :custom (treesit-font-lock-level 4))

;; treesit-auto : Automatically install and use tree-sitter major modes in Emacs 29+.
;; (use-package treesit-auto
;;     :custom (treesit-auto-install 'prompt)
;;     :config
;;     (delete 'janet treesit-auto-langs)
;;     (treesit-auto-add-to-auto-mode-alist 'all)

;;     ;; Workaround for https://github.com/renzmann/treesit-auto/issues/76
;;     (setq major-mode-remap-alist (treesit-auto--build-major-mode-remap-alist))

;;     ;; enable globally
;;     (global-treesit-auto-mode))

;; elisp
(use-package elisp-mode
    :ensure nil ;; emacs built-in
    :config (setq lisp-body-indent 4))

(use-package elisp-demos
    :init
    (advice-add #'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
    (advice-add #'helpful-update :after #'elisp-demos-advice-helpful-update))

;; c++ treesiter
(use-package c-ts-mode
    :ensure nil ;; emacs built-in
    :preface
    (defun my--c-ts-indent-style()
        "Override the built-in BSD indentation style with some additional rules.
         Docs: https://www.gnu.org/software/emacs/manual/html_node/elisp/Parser_002dbased-Indentation.html
         Notes: `treesit-explore-mode' can be very useful to see where you're at in the tree-sitter tree,
                especially paired with `(setq treesit--indent-verbose t)' to debug what rules is being
                applied at a given point."
        `(;; do not indent preprocessor statements
          ((node-is "preproc") column-0 0)
          ;; do not indent namespace children
          ;; ((n-p-gp nil nil "namespace_definition") grand-parent 0)
          ((n-p-gp nil "declaration_list" "namespace_definition") parent-bol 0)
          ;; append to bsd style
          ,@(alist-get 'bsd (c-ts-mode--indent-styles 'cpp))))
    :config
    (setq c-ts-mode-indent-offset 4)
    (setq c-ts-mode-indent-style #'my--c-ts-indent-style))

;; cmake
(use-package cmake-mode)

;; adoc-mode : ascii docs
(use-package adoc-mode
    :mode "\\.adoc\\'")

;; csv-mode : Support for csv files (use csv-align-mode for alignment)
(use-package csv-mode
    :mode "\\.csv\\'")

;; yaml-mode : Support gitlab-ci.yml
(use-package yaml-mode
    :mode "\\.yml\\'")

;; web-mode : Support various web files
(use-package web-mode
    :disabled t
    :mode ("\\.css\\'" "\\.html\\'" "\\.ts\\'" "\\.js\\'" "\\.jsx?$" "\\.vue\\'")
    :custom
    (web-mode-script-padding 0) ; For vue.js SFC : no initial padding in the script section
    (web-mode-markup-indent-offset 2)
    (web-mode-css-indent-offset 2)
    (web-mode-code-indent-offset 2))

(use-package python
    :config
    (defvar my--python-enable-ipython t
        "use ipython as the embedded REPL.")
    (setq python-indent-offset 4)

    (when my--python-enable-ipython
        (setq python-shell-interpreter "ipython3")
        (setq python-shell-interpreter-args "-i --simple-prompt --no-color-info")))

(use-package markdown-mode
    :mode (("\\.[Rr]md\\'" . markdown-mode)
           ("\\.qmd\\'" . markdown-mode))
    ;; :init (add-hook 'markdown-mode-hook 'markdown-toggle-markup-hiding)
    :config (setq markdown-fontify-code-blocks-natively t
                  markdown-fontify-whole-heading-line t
                  markdown-enable-math t))

(use-package go-mode
    :preface
    (defun my--go-setup ()
        (setq-local c-basic-offset 4) ; Base indent size when indented automatically
        (setq-local tab-width 4)
        (setq-local indent-tabs-mode nil))
    :hook ((go-mode . my--go-setup)
           (go-ts-mode . my--go-setup)))

(use-package sql
    :config (add-hook 'sql-mode-hook (setq-local tab-width 4)))

;; rust-mode : blazingly fast
(use-package rust-mode
    :config (setq rust-format-on-save nil))

;; zig
(use-package zig-mode)

;; JSON Support
(use-package json-mode)

;; Lua Support
(use-package lua-mode)

;; js is everywhere
(use-package js2-mode
    :init
    (add-hook 'js2-mode-hook
              (lambda ()
                  (push '("function" . ?ƒ) prettify-symbols-alist)))
    (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
    :config
    (setq-default js2-basic-indent 2
                  js2-basic-offset 2
                  js2-auto-indent-p t
                  js2-cleanup-whitespace t
                  js2-enter-indents-newline t
                  js2-indent-on-enter-key t
                  js2-global-externs (list "window" "module" "require" "buster" "sinon" "assert" "refute"
                                           "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location"
                                           "__dirname" "console" "JSON" "jQuery" "$")))

(provide 'my-init-langs)
;;; my-init-langs.el ends here

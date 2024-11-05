;;; my-init-langs.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
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
    (defun my--treesit-install-language-grammars ()
        "Install Tree-sitter grammars if they are absent."
        (interactive)
        (dolist (grammar
                 '((awk        "https://github.com/Beaglefoot/tree-sitter-awk")
                   (bash       "https://github.com/tree-sitter/tree-sitter-bash")
                   ;; (bibtex     "https://github.com/latex-lsp/tree-sitter-bibtex")
                   ;; (blueprint  "https://github.com/huanie/tree-sitter-blueprint")
                   (c          "https://github.com/tree-sitter/tree-sitter-c/" "master" "src")
                   (c-sharp    "https://github.com/tree-sitter/tree-sitter-c-sharp")
                   (clojure    "https://github.com/sogaiu/tree-sitter-clojure" "master" "src")
                   (cmake      "https://github.com/uyha/tree-sitter-cmake")
                   (commonlisp "https://github.com/tree-sitter-grammars/tree-sitter-commonlisp")
                   (cpp        "https://github.com/tree-sitter/tree-sitter-cpp/" "v0.23.1" "src")
                   (css        "https://github.com/tree-sitter/tree-sitter-css")
                   (dart       "https://github.com/ast-grep/tree-sitter-dart")
                   (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "main" "src")
                   (elisp      "https://github.com/Wilfred/tree-sitter-elisp")
                   (elixir     "https://github.com/elixir-lang/tree-sitter-elixir" "main" "src")
                   (erlang     "https://github.com/WhatsApp/tree-sitter-erlang" "main" "src")
                   (glsl       "https://github.com/tree-sitter-grammars/tree-sitter-glsl")
                   (go         "https://github.com/tree-sitter/tree-sitter-go")
                   (gomod      "https://github.com/camdencheek/tree-sitter-go-mod")
                   ;; (heex       "https://github.com/phoenixframework/tree-sitter-heex")
                   ;; (haskell    "https://github.com/tree-sitter/tree-sitter-haskell" "master" "src")
                   (html       "https://github.com/tree-sitter/tree-sitter-html")
                   ;; (janet      "https://github.com/sogaiu/tree-sitter-janet-simple")
                   (java       "https://github.com/tree-sitter/tree-sitter-java" "master" "src")
                   (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
                   (json       "https://github.com/tree-sitter/tree-sitter-json")
                   (julia      "https://github.com/tree-sitter/tree-sitter-julia" "master" "src")
                   (kotlin     "https://github.com/fwcd/tree-sitter-kotlin")
                   ;; (latex      "https://github.com/latex-lsp/tree-sitter-latex")
                   (lua        "https://github.com/tree-sitter-grammars/tree-sitter-lua")
                   (make       "https://github.com/tree-sitter-grammars/tree-sitter-make")
                   ;; (markdown   "https://github.com/tree-sitter-grammars/tree-sitter-markdown")
                   (nix        "https://github.com/nix-community/tree-sitter-nix")
                   ;; (nushell    "https://github.com/nushell/tree-sitter-nu")
                   ;; (org        "https://github.com/milisims/tree-sitter-org")
                   (perl       "https://github.com/ganezdragon/tree-sitter-perl")
                   (proto      "https://github.com/mitchellh/tree-sitter-proto")
                   (python     "https://github.com/tree-sitter/tree-sitter-python")
                   (r          "https://github.com/r-lib/tree-sitter-r")
                   (ruby       "https://github.com/tree-sitter/tree-sitter-ruby" "master" "src")
                   (rust       "https://github.com/tree-sitter/tree-sitter-rust" "master" "src")
                   (scala      "https://github.com/tree-sitter/tree-sitter-scala")
                   ;; (sql        "https://github.com/DerekStride/tree-sitter-sql")
                   (toml       "https://github.com/tree-sitter/tree-sitter-toml")
                   (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
                   (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
                   ;; (typst      "https://github.com/uben0/tree-sitter-typst")
                   ;; (verilog    "https://github.com/gmlarumbe/tree-sitter-verilog")
                   ;; (vhdl       "https://github.com/alemuller/tree-sitter-vhdl")
                   (vue        "https://github.com/tree-sitter-grammars/tree-sitter-vue")
                   (yaml       "https://github.com/tree-sitter-grammars/tree-sitter-yaml")))
            (add-to-list 'treesit-language-source-alist grammar)
            ;; Only install `grammar' if we don't already have it
            ;; installed. However, if you want to *update* a grammar then
            ;; this obviously prevents that from happening.
            (unless (treesit-language-available-p (car grammar))
                (treesit-install-language-grammar (car grammar)
                                                  (no-littering-expand-var-file-name "tree-sitter")))))
    :config
    ;; Optional, but recommended. Tree-sitter enabled major modes are
    ;; distinct from their ordinary counterparts.
    ;;
    ;; You can remap major modes with `major-mode-remap-alist'. Note
    ;; that this does *not* extend to hooks! Make sure you migrate them
    ;; also
    (dolist (mapping '((awk-mode                                        . awk-ts-mode)
                       (bash-mode                                       . bash-ts-mode)
                       (bibtex-mode                                     . bibtext-ts-mode)
                       (blueprint-mode                                  . blueprint-ts-mode)
                       (c-mode                                          . c-ts-mode)
                       (c-sharp                                         . c-sharp-ts-mode)
                       ((clojure-mode clojurescript-mode clojurec-mode) . clojure-ts-mode)
                       (c-make-mode                                     . cmake-ts-mode)
                       (common-lisp-mode                                . common-lisp-ts-mode)
                       (c++-mode                                        . c++-ts-mode)
                       (css-mode                                        . css-ts-mode)
                       (dart-mode                                       . dart-ts-mode)
                       (dockerfile-mode                                 . dockerfile-ts-mode)
                       (elisp-mode                                      . elisp-ts-mode)
                       (elixir-mode                                     . elixir-ts-mode)
                       (erlang-mode                                     . erlang-ts-mode)
                       (glsl-mode                                       . glsl-ts-mode)
                       (go-mode                                         . go-ts-mode)
                       (go-mod-mode                                     . go-mod-ts-mode)
                       (heex-mode                                       . heex-ts-mode)
                       (haskell-mode                                    . haskel-ts-mode)
                       ((html-mode mhtml-mode sgml-mode)                . html-ts-mode)
                       ;; (janet-mode                                      . janet-ts-mode)
                       (java-mode                                       . java-ts-mode)
                       ((js-mode javascript-mode js2-mode)              . js-ts-mode)
                       (js-json-mode                                    . json-ts-mode)
                       (julia-mode                                      . julia-ts-mode)
                       (kotlin-mode                                     . kotlin-ts-mode)
                       (latex-mode                                      . latex-ts-mode)
                       (lua-mode                                        . lua-ts-mode)
                       (makefile-mode                                   . makefile-ts-mode)
                       ((poly-markdown-mode markdown-mode)              . markdown-ts-mode)
                       (nix-mode                                        . nix-ts-mode)
                       ;; (nushell-mode                                    . nushell-ts-mode)
                       ;; (org-mode                                        . org-ts-mode)
                       (perl-mode                                       . perl-ts-mode)
                       (protobuf-mode                                   . protobuf-ts-mode)
                       (python-mode                                     . python-ts-mode)
                       (ess-mode                                        . r-ts-mode)
                       (ruby-mode                                       . ruby-ts-mode)
                       (rust-mode                                       . rust-ts-mode)
                       (scala-mode                                      . scala-ts-mode)
                       (sql-mode                                        . sql-ts-mode)
                       ((conf-toml-mode toml-mode)                      . toml-ts-mode)
                       (typescript-tsx-mode                             . tsx-ts-mode)
                       (typescript-mode                                 . typescript-ts-mode)
                       (typst-mode                                      . typst-ts-mode)
                       (verilog-mode                                    . verilog-ts-mode)
                       (vhdl-mode                                       . vhdl-ts-mode)
                       (vue-mode                                        . vue-ts-mode)
                       (yaml-mode                                       . yaml-ts-mode)))
        (add-to-list 'major-mode-remap-alist mapping))
    (setq treesit-extra-load-path
          `(,(no-littering-expand-var-file-name "tree-sitter")))
    (setq treesit-font-lock-level 4))

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
    :mode "\\.yml\\'" "\\.yaml\\'")

;; web-mode : Support various web files
(use-package web-mode
    :mode ("\\.css\\'"
           "\\.html?\\'"
           "\\.html\\.twig\\'"
           "\\.phtml\\'"
           "\\.tpl\\.php\\'"
           "\\.ts\\'"
           "\\.js\\'"
           "\\.jsx?$"
           "\\.vue\\'")
    :custom ((web-mode-script-padding 0) ; For vue.js SFC : no initial padding in the script section
             (web-mode-markup-indent-offset 2)
             (web-mode-css-indent-offset 2)
             (web-mode-code-indent-offset 2)))

;; php-mode
(use-package php-mode
    :custom ((php-mode-coding-style 'psr2)
             (php-mode-template-compatibility nil)
             (php-imenu-generic-expression 'php-imenu-generic-expression-simple)))

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

(use-package csv-mode
    :ensure nil ;; emacs built-in
    :init
    (add-hook 'csv-mode-hook 'csv-align-mode)
    (add-hook 'csv-mode-hook (lambda () (interactive) (toggle-truncate-lines nil))))

(provide 'my-init-langs)
;;; my-init-langs.el ends here

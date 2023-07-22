;;; my-init-langs-treesitter.el -*- lexical-binding: t; -*-

;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;; Install Language Grammars:
;;  - Call the command `M-x treesit-install-language-grammar' for each language.
;;  - Alternativelly, evaluate the following to install them all:
;;    (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
;; Check if grammar is working:
;;    (treesit-language-available-p 'python)
;;    (treesit-language-available-p 'cpp)
;; List all known tree-sitter major modes:
;;    C-h a -ts-mode$

(use-package treesit
    :ensure nil ;; emacs built-in
    :disabled t
    :preface
    (defun my/setup-install-grammars ()
        "Install Tree-sitter grammars if they are absent."
        (interactive)
        (dolist (grammar
                    '((bash          "https://github.com/tree-sitter/tree-sitter-bash")
                         (c          "https://github.com/tree-sitter/tree-sitter-c/" "master" "src")
                         (clojure    "https://github.com/sogaiu/tree-sitter-clojure" "master" "src")
                         (cpp        "https://github.com/tree-sitter/tree-sitter-cpp/" "master" "src")
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
    (dolist (mapping '((python-mode . python-ts-mode)
                          (css-mode . css-ts-mode)
                          (typescript-mode . tsx-ts-mode)
                          (js-mode . js-ts-mode)
                          (css-mode . css-ts-mode)
                          (yaml-mode . yaml-ts-mode)
                          (c-mode . c--ts-mode)
                          (c++-mode . c++-ts-mode)))
        (add-to-list 'major-mode-remap-alist mapping))

    :config
    (my/setup-install-grammars)
    (setq treesit-font-lock-level 4))

(provide 'my-init-langs-treesitter)
;;; my-init-langs-misc.el ends here

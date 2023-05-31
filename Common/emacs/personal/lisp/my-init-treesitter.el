;;; my-init-treesitter.el -*- lexical-binding: t; -*-

;; https://github.com/tree-sitter
;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
(setq treesit-language-source-alist
    '((bash "https://github.com/tree-sitter/tree-sitter-bash")
         (c "https://github.com/tree-sitter/tree-sitter-c")
         (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
         (cmake "https://github.com/uyha/tree-sitter-cmake")
         (css "https://github.com/tree-sitter/tree-sitter-css")
         (elisp "https://github.com/Wilfred/tree-sitter-elisp")
         (go "https://github.com/tree-sitter/tree-sitter-go")
         (html "https://github.com/tree-sitter/tree-sitter-html")
         (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
         (json "https://github.com/tree-sitter/tree-sitter-json")
         (make "https://github.com/alemuller/tree-sitter-make")
         (markdown "https://github.com/ikatyang/tree-sitter-markdown")
         (python "https://github.com/tree-sitter/tree-sitter-python")
         (rust "https://github.com/tree-sitter/tree-sitter-rust")
         (toml "https://github.com/tree-sitter/tree-sitter-toml")
         (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
         (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
         (yaml "https://github.com/ikatyang/tree-sitter-yaml")
         (zig "https://github.com/maxxnino/tree-sitter-zig")))

(provide 'my-init-treesitter)
;;; my-init-misc.el ends here

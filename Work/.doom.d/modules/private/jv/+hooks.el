;;; private/jv/+hooks.el -*- lexical-binding: t; -*-

(add-hook! 'markdown-mode-hook
    (progn
        (toggle-word-wrap nil)
        (auto-fill-mode -1)))

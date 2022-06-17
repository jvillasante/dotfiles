;;; private/my-cc/init.el -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.C\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))

(c-set-offset 'innamespace 0)

(setq c-default-style "stroustrup"
    c-basic-offset 4
    indent-tabs-mode t)

(add-hook! c-mode-common-hook
    (c-toggle-auto-state 1))

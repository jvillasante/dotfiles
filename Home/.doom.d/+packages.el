;;; +packages.el -*- lexical-binding: t; -*-

(use-package! rg
    :defines projectile-command-map
    ;;:hook (after-init . rg-enable-default-bindings)
    :config
    (rg-enable-default-bindings "\C-cr")
    (setq rg-group-result t
        rg-show-columns t)

    (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases)

    (with-eval-after-load 'projectile
        (defalias 'projectile-ripgrep 'rg-project)
        (bind-key "s R" #'rg-project projectile-command-map))

    (with-eval-after-load 'counsel
        (bind-keys
            :map rg-global-map
            ("c r" . counsel-rg)
            ("c s" . counsel-ag)
            ("c p" . counsel-pt)
            ("c f" . counsel-fzf)))

    (defun my-swiper-toggle-rg-dwim ()
        "Toggle `rg-dwim' with current swiper input."
        (interactive)
        (ivy-quit-and-run (rg-dwim default-directory)))

    (remove-hook 'compilation-filter-hook #'doom-apply-ansi-color-to-compilation-buffer-h))

(use-package! sr-speedbar
    ;; :init
    ;; (spacemacs/set-leader-keys
    ;;     "sr" 'sr-speedbar-toggle)
    )

(use-package! flyspell-lazy
    :init
    (add-hook 'prog-mode-hook 'flyspell-lazy-mode))

(use-package! pinentry
    :defer t
    :init
    (setq epa-pinentry-mode 'loopback))

(use-package! crux
    :defer t
    :bind (("C-c o" . crux-open-with)))

(use-package! visual-regexp
    :commands (vr/query-replace vr/replace))

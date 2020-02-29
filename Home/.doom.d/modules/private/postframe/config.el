;;; private/postframe/config.el -*- lexical-binding: t; -*-

(use-package! counsel
    :diminish ivy-mode
    :config
    (map!
        :g "C-S-a" #'counsel-M-x
        :g "C-S-o" #'counsel-find-file))

(use-package! ivy-posframe
    :custom
    (ivy-posframe-parameters
        '((left-fringe . 5)
             (right-fringe . 5)))
    (ivy-posframe-border-width 2)
    (ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
    :custom-face
    (ivy-posframe ((t (:background "#282a36"))))
    (ivy-posframe-border ((t (:background "#6272a4")))) ; mac では反映されない
    (ivy-posframe-cursor ((t (:background "#61bfff")))) ; mac では反映されない
    :config
    (ivy-posframe-mode 1))

(use-package! hydra-posframe
    :config
    (hydra-posframe-enable))

(use-package! flymake-posframe
    :hook (flymake-mode . flymake-posframe-mode))

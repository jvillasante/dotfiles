;;; my-init-transient.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(use-package casual-suite)

(use-package casual-calc
    :requires casual-suite
    :preface (defvar calc-mode-map)
    :bind (:map calc-mode-map ("C-o" . casual-calc-tmenu)))

(use-package casual-info
    :requires casual-suite
    :preface (defvar Info-mode-map)
    :bind (:map Info-mode-map ("C-o" . casual-info-tmenu)))

(use-package casual-dired
    :requires casual-suite
    :preface (defvar dired-mode-map)
    :bind (:map dired-mode-map ("C-o" . casual-dired-tmenu)))

;; (use-package casual-avy
;;     :requires casual-suite
;;     :bind ("M-g" . casual-avy-tmenu))

(use-package casual-isearch
    :requires casual-suite
    :preface (defvar isearch-mode-map)
    :bind (:map isearch-mode-map ("<f2>" . casual-isearch-tmenu)))

(provide 'my-init-transient)
;;; my-init-transient.el ends here

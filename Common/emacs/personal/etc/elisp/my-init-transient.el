;;; my-init-transient.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; TODO: Install the new casual packages...

(use-package casual-suite)

(use-package casual-calc
    :requires casual-suite
    :bind (:map calc-mode-map ("C-o" . casual-calc-tmenu)))

(use-package casual-info
    :requires casual-suite
    :bind (:map Info-mode-map ("C-o" . casual-info-tmenu)))

(use-package casual-dired
    :requires casual-suite
    :bind (:map dired-mode-map ("C-o" . casual-dired-tmenu)))

;; (use-package casual-avy
;;     :requires casual-suite
;;     :bind ("M-g" . casual-avy-tmenu))

(use-package casual-isearch
    :requires casual-suite
    :bind (:map isearch-mode-map ("<f2>" . casual-isearch-tmenu)))

(use-package casual-ibuffer
    :requires casual-suite
    :bind (:map ibuffer-mode-map
                ("C-o" . casual-ibuffer-tmenu)
                ("F" . casual-ibuffer-filter-tmenu)
                ("s" . casual-ibuffer-sortby-tmenu)))

(use-package casual-re-builder
    :requires casual-suite
    :bind (:map reb-mode-map
                ("C-o" . casual-re-builder-tmenu)))

(use-package casual-bookmarks
    :requires casual-suite
    :bind (:map bookmark-bmenu-mode-map
                ("C-o" . casual-bookmarks-tmenu)))

(provide 'my-init-transient)
;;; my-init-transient.el ends here

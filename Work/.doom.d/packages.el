;; -*- no-byte-compile: t; -*-
;;; Hacking/workspace/dotfiles/Home/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; Disable Packages
(disable-packages! company-irony company-irony-c-headers flycheck-irony irony irony-eldoc ivy-rtags rtags)
(disable-packages! winum)

;; Load Packages
(package! ivy-prescient)
(package! crux)
(package! dired-quick-sort)

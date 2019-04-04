;;; packages.el --- description -*- no-byte-compile: t; -*-

(disable-packages! company-irony company-irony-c-headers flycheck-irony irony irony-eldoc ivy-rtags rtags)
(disable-packages! winum)

;; JS stuff
;; (package! prettier-js)
;; (package! pkgbuild-mode)

;; Filestuff
;; (package! ranger)

;; Reasonml stuff
;; (package! reason-mode)

;; Email stuff
;; (package! notmuch)
;; (if (featurep! :completion ivy)
;;     (package! counsel-notmuch)
;;   (package! helm-notmuch))
;; (package! org-mime)

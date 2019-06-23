;;; packages.el --- description -*- no-byte-compile: t; -*-

;; Disable Packages
(disable-packages! company-irony company-irony-c-headers flycheck-irony irony irony-eldoc ivy-rtags rtags)
(disable-packages! winum)

;; Load Packages
(package! ccls)
(package! ivy-prescient)
(package! crux)
(package! dired-quick-sort)

;; Github example
;; (package! tablature-mode :recipe (:fetcher github :repo "valrus/tablature-mode"))

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

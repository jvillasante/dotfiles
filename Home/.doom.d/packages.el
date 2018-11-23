;;;  -*- no-byte-compile: t; -*-

;; Ruby stuff
;; (package! enh-ruby-mode)
;; (package! rbenv) ; i use rbenv
;; (package! yard-mode)
;; (package! rinari)
;; (package! rubocop)

;; Misc stuff
;; Can highlight a region and hit M-x carbon-now-sh to get a https://carbon.now
;; of the region
;; (package! carbon-now-sh)

;; JS stuff
(package! prettier-js)
(package! pkgbuild-mode)

;; Fuzzy searching
;; (package! flx)

;; Music stuff
;; (package! google-play-music :recipe (:fetcher github :repo "merrickluo/google-play-music.el"))

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

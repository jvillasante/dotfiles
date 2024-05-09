;; -*- mode:emacs-lisp; coding: utf-8-emacs; -*-
;; Gnus startup file.
;; Never delete this file -- if you want to force Gnus to read the
;; .newsrc file (if you have one), touch .newsrc instead.
(setq gnus-newsrc-file-version "Gnus v5.13")

(setq gnus-newsrc-last-checked-date '"Wed, 08 May 2024 13:43:48 -0400")

(setq gnus-newsrc-alist '(("nndraft:drafts" 1 nil nil (nndraft "") ((gnus-dummy (gnus-draft-mode)))) ("nntp+news.eternal-september.org:alt.religion.emacs" 3 nil nil "nntp:news.eternal-september.org" nil) ("nntp+news.eternal-september.org:comp.emacs" 3 nil nil "nntp:news.eternal-september.org" nil) ("nntp+news.eternal-september.org:comp.lang.c" 3 ((1 . 43519) 309252 309254) nil "nntp:news.eternal-september.org" nil) ("nntp+news.eternal-september.org:comp.lang.c++" 3 ((1 . 29632) (150703 . 150704) 150709 150728 150777 150822 150845 150853 (150856 . 150857) (150863 . 150864) (150870 . 150871)) ((unexist) (seen (150677 . 150877))) "nntp:news.eternal-september.org") ("nntp+news.eternal-september.org:gnu.emacs.help" 3 nil nil "nntp:news.eternal-september.org" nil)))

(setq gnus-server-alist '(("archive" nnfolder "archive" (nnfolder-directory "~/Mail/archive") (nnfolder-active-file "~/Mail/archive/active") (nnfolder-get-new-mail nil) (nnfolder-inhibit-expiry t))))

(setq gnus-topic-topology '(("Gnus" visible) (("misc" visible)) (("emacs" visible nil nil)) (("comp.lang" visible nil nil))))

(setq gnus-topic-alist '(("comp.lang" "nntp+news.eternal-september.org:comp.lang.c++" "nntp+news.eternal-september.org:comp.lang.c") ("emacs" "nntp+news.eternal-september.org:alt.religion.emacs" "nntp+news.eternal-september.org:comp.emacs" "nntp+news.eternal-september.org:gnu.emacs.help") ("misc" "nndraft:drafts") ("Gnus")))

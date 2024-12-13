;;; my-init-mail.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; we need mu4e loaded
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)

;; mu4easy : A global minor mode that defines a full working setup for mu4e and mbsync
(use-package mu4easy
    :disabled t
    :demand t
    :config (mu4easy-mode)
    :custom (mu4easy-contexts
             '((mu4easy-context
                :c-name  "Google"
                :maildir "jvillasantegomez@gmail.com"
                :mail    "Gmail"
                :smtp    "smtp.gmail.com"
                :sent-action delete)
               (mu4easy-context
                :c-name  "Apple"
                :maildir "julio.villasante@icloud.com"
                :mail    "Apple"
                :smtp    "smtp.mail.me.com"))))

(provide 'my-init-mail)
;;; my-init-mail.el ends here

;;; my-init-mail.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; mu4e
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(use-package mu4e
    :ensure nil ;; built-in
    :defer t)

;; mu4easy : A global minor mode that defines a full working setup for mu4e and mbsync
(use-package mu4easy
    :defer t
    :hook (after-init . (lambda ()
                            (mu4easy-mode)
                            (mu4e-alert-disable-mode-line-display)))
    :bind ("C-c o u" . mu4e)
    :custom
    (mu4easy-signature "\n\n*Julio Villasante*")
    (mu4easy-headers
        '((:human-date . 18) (:flags . 6) (:maildir . 28) (:from-or-to . 22)
             (:mailing-list . 10) (:tags . 10) (:subject . 92)))
    (mu4easy-contexts
        '((mu4easy-context
              :c-name  "Google"
              :maildir "jvillasantegomez@gmail.com"
              :mail    "jvillasantegomez@gmail.com"
              :smtp    "smtp.gmail.com"
              :sent-action delete)
             (mu4easy-context
                 :c-name  "Apple"
                 :maildir "julio.villasante@icloud.com"
                 :mail    "julio.villasante@icloud.com"
                 :smtp    "smtp.mail.me.com"))))

(provide 'my-init-mail)
;;; my-init-mail.el ends here

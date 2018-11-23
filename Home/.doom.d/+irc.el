;;;  -*- lexical-binding: t; -*-

(after! irc
  (defun fetch-password (&rest params)
    "Fetch a password from auth-source"
    (require 'auth-source)
    (let ((match (car (apply 'auth-source-search params))))
      (if match
        (let ((secret (plist-get match :secret)))
          (if (functionp secret)
            (funcall secret)
            secret))
        (error "Password not found for %S" params))))

  (defun irc-password (server)
    (fetch-password :user "ar1a" :host server))

  ;; Freenode
  (set-irc-server! "irc.freenode.net"
    '(:use-tls
       t
       :port 6697
       :nick "ar1a"
       :sasl-username "ar1a"
       :sasl-password irc-password
       :channels ("#emacs"))))


(map! :leader
  (:prefix "o"
    (:nve "I" #'=irc)))

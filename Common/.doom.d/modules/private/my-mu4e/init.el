;; -*- no-byte-compile: t; -*-
;;; email/mu4e/init.el

(setq +my/downloads-path (expand-file-name "Downloads" "~/"))

(cond
    (IS-MAC
        (setq
            +my/mu-path "/usr/local/bin/mu"
            +my/msmtp-path "/usr/local/bin/msmtp"))
    (IS-LINUX
        (setq
            +my/mu-path "/usr/bin/mu"
            +my/msmtp-path "/usr/bin/msmtp")))

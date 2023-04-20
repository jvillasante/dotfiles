(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(connection-local-criteria-alist
      '(((:application tramp)
            tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)
           ((:application eshell)
               eshell-connection-default-profile)))
 '(connection-local-profile-alist
      '((tramp-connection-local-darwin-ps-profile
            (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
            (tramp-process-attributes-ps-format
                (pid . number)
                (euid . number)
                (user . string)
                (egid . number)
                (comm . 52)
                (state . 5)
                (ppid . number)
                (pgrp . number)
                (sess . number)
                (ttname . string)
                (tpgid . number)
                (minflt . number)
                (majflt . number)
                (time . tramp-ps-time)
                (pri . number)
                (nice . number)
                (vsize . number)
                (rss . number)
                (etime . tramp-ps-time)
                (pcpu . number)
                (pmem . number)
                (args)))
           (tramp-connection-local-busybox-ps-profile
               (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
               (tramp-process-attributes-ps-format
                   (pid . number)
                   (user . string)
                   (group . string)
                   (comm . 52)
                   (state . 5)
                   (ppid . number)
                   (pgrp . number)
                   (ttname . string)
                   (time . tramp-ps-time)
                   (nice . number)
                   (etime . tramp-ps-time)
                   (args)))
           (tramp-connection-local-bsd-ps-profile
               (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
               (tramp-process-attributes-ps-format
                   (pid . number)
                   (euid . number)
                   (user . string)
                   (egid . number)
                   (group . string)
                   (comm . 52)
                   (state . string)
                   (ppid . number)
                   (pgrp . number)
                   (sess . number)
                   (ttname . string)
                   (tpgid . number)
                   (minflt . number)
                   (majflt . number)
                   (time . tramp-ps-time)
                   (pri . number)
                   (nice . number)
                   (vsize . number)
                   (rss . number)
                   (etime . number)
                   (pcpu . number)
                   (pmem . number)
                   (args)))
           (tramp-connection-local-default-shell-profile
               (shell-file-name . "/bin/sh")
               (shell-command-switch . "-c"))
           (tramp-connection-local-default-system-profile
               (path-separator . ":")
               (null-device . "/dev/null"))
           (eshell-connection-default-profile
               (eshell-path-env-list))))
 '(package-selected-packages
      '(minions which-key anzu shackle popper vertico orderless all-the-icons-completion consult-dir embark-consult corfu-terminal yasnippet-snippets git-gutter hl-todo org-superstar deft elisp-demos csv-mode yaml-mode web-mode go-mode rustic js2-mode cmake-mode fancy-compilation elfeed pdf-tools eshell-prompt-extras rainbow-delimiters ibuffer-vc crux persistent-scratch exec-path-from-shell avy undo-tree hydra expand-region whole-line-or-region multiple-cursors engine-mode diredfl all-the-icons-dired dired-hide-dotfiles dired-sidebar ef-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fancy-compilation-default-face ((t (:inherit nil :background unspecified))))
 '(fixed-pitch ((t (:inherit (default)))))
 '(fixed-pitch-serif ((t (:inherit (default)))))
 '(variable-pitch ((t (:font "Iosevka Aile")))))

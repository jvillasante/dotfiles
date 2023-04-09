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
 '(help-at-pt-display-when-idle '(flymake-overlay) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.5)
 '(package-selected-packages
      '(dired-sidebar dired-hide-dotfiles all-the-icons-dired diredfl engine-mode multiple-cursors whole-line-or-region expand-region hydra undo-tree avy exec-path-from-shell editorconfig persistent-scratch crux ibuffer-vc vterm rainbow-delimiters helpful eshell-prompt-extras pdf-tools elfeed fancy-compilation apheleia cmake-mode js2-mode rustic go-mode markdown-mode web-mode yaml-mode csv-mode adoc-mode elisp-demos deft org-superstar hl-todo git-gutter magit yasnippet-snippets cape corfu-terminal corfu embark-consult embark consult-dir consult marginalia all-the-icons-completion orderless vertico popper shackle anzu which-key minions all-the-icons modus-themes no-littering)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fancy-compilation-default-face ((t (:inherit nil :background unspecified))))
 '(flymake-error ((((class color)) (:underline "red"))))
 '(flymake-warning ((((class color)) (:underline "yellow")))))

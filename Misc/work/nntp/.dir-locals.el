;;; Directory Local Variables --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; For more information see (info "(emacs) Directory Variables")

((nil .
      ((eval . (progn
                   ;; At work, we don't auto-format code :(
                   (setq-local my/format-on-save nil)

                   ;; Point compile-command at the build script using an absolute
                   ;; path derived from the .dir-locals.el location so it works
                   ;; regardless of which subdirectory the visited file is in.
                   (let ((root (locate-dominating-file default-directory ".dir-locals.el")))
                       (setq-local compile-command
                                   (concat "bash "
                                           (expand-file-name ".scripts/build.sh" root))))

                   ;; Open an interactive shell inside the running podman container.
                   ;; If the container is not running yet, start it first via the
                   ;; build script so the session is consistent.
                   (defun my/nntp-attach ()
                       "Attach an interactive bash shell to the nntp podman container in ghostel.
If the container is not running, starts it first via the build script.
Use `podman stop <name>' to end the session."
                       (interactive)
                       (let* ((root (locate-dominating-file default-directory ".dir-locals.el"))
                              (container (file-name-nondirectory (directory-file-name root)))
                              (cname (format "nntp-%s" container))
                              (buf-name (format "*ghostel-%s*" container))
                              (running-p (string= "true"
                                                  (string-trim
                                                   (shell-command-to-string
                                                    (format "podman inspect -f '{{.State.Running}}' %s 2>/dev/null"
                                                            (shell-quote-argument cname)))))))
                           ;; Start the container if not running
                           (unless running-p
                               (let ((default-directory root))
                                   (message "Starting container %s..." cname)
                                   (call-process "bash" nil nil nil
                                                 (expand-file-name ".scripts/ensure-container.sh" root))))
                           (let ((g-buf (get-buffer buf-name)))
                               (if g-buf
                                       (pop-to-buffer g-buf)
                                   ;; Create a new ghostel buffer in the other window
                                   (display-buffer-override-next-command
                                    (lambda (buffer alist)
                                        (cons (display-buffer-pop-up-window buffer alist) 'window)))
                                   (let ((ghostel-buffer-name buf-name))
                                       (ghostel))
                                   (setq g-buf (get-buffer buf-name)))
                               (with-current-buffer g-buf
                                   (process-send-string
                                    ghostel--process
                                    (format "podman exec -it --user nntpuser %s /bin/bash\n"
                                            cname))))))

                   ;; Kept for reference in case we switch back to vterm.
                   (defun my/nntp-attach-vterm ()
                       "Attach an interactive bash shell to the nntp podman container in vterm.
If the container is not running, starts it first via the build script.
Use `podman stop <name>' to end the session."
                       (interactive)
                       (let* ((root (locate-dominating-file default-directory ".dir-locals.el"))
                              (container (file-name-nondirectory (directory-file-name root)))
                              (cname (format "nntp-%s" container))
                              (buf-name (format "*vterm-%s*" container))
                              (running-p (string= "true"
                                                  (string-trim
                                                   (shell-command-to-string
                                                    (format "podman inspect -f '{{.State.Running}}' %s 2>/dev/null"
                                                            (shell-quote-argument cname)))))))
                           ;; Start the container if not running
                           (unless running-p
                               (let ((default-directory root))
                                   (message "Starting container %s..." cname)
                                   (call-process "bash" nil nil nil
                                                 (expand-file-name ".scripts/ensure-container.sh" root))))
                           (let ((v-buf (or (get-buffer buf-name)
                                            (vterm-other-window buf-name))))
                               (pop-to-buffer v-buf)
                               (with-current-buffer v-buf
                                   (setq-local vterm-scroll-to-bottom-on-output t)
                                   (vterm-send-string
                                    (format "podman exec -it --user nntpuser %s /bin/bash\n"
                                            cname))))))

                   ;; C-x p a  ("project attach") — consistent with C-x p c (compile)
                   (keymap-set project-prefix-map "a" #'my/nntp-attach))))))

;;; .dir-locals --- -*- no-byte-compile: t; lexical-binding: t; -*-
((nil .
      ((eval . (progn
                   ;; At work, we don't auto-format code :(
                   (apheleia-mode -1)

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
                   (defun my-nntp-attach ()
                       "Attach an interactive bash shell to the nntpcode podman container in vterm.
If the container is not running, starts it first (without building).
Use `podman stop nntpcode' to end the session."
                       (interactive)
                       (let* ((root (locate-dominating-file default-directory ".dir-locals.el"))
                              (container (file-name-nondirectory (directory-file-name root)))
                              (buf-name (format "*vterm-%s*" container))
                              (v-buf (or (get-buffer buf-name)
                                         (vterm-other-window buf-name))))
                           (pop-to-buffer v-buf)
                           (with-current-buffer v-buf
                               (setq-local vterm-scroll-to-bottom-on-output t)
                               (vterm-send-string
                                (format "podman exec -it --user nntpuser %s /bin/bash\n"
                                        container)))))

                   ;; C-x p a  ("project attach") — consistent with C-x p c (compile)
                   (keymap-set project-prefix-map "a" #'my/nntp-attach))))))

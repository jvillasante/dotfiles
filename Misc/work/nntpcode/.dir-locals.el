;;; .dir-locals --- -*- no-byte-compile: t; lexical-binding: t; -*-
((nil .
      ((eval . (progn
                 (defun my/nntp-compile-logic ()
                   "Ensures container is running and we are inside build dir before make."
                   (interactive)
                   (let* ((proj-dir (project-root (project-current t)))
                          (proj-name (file-name-nondirectory (directory-file-name proj-dir)))
                          (buf-name (format "*vterm-%s*" proj-name))
                          (v-buf (get-buffer buf-name))
                          (is-running (format "podman inspect -f '{{.State.Running}}' %s 2>/dev/null" proj-name))
                          (podman-run-cmd
                           (concat "podman run --user nntpuser --rm -it "
                                   (format "--volume %s:/tmp/nntpcode:rw,z " proj-dir)
                                   "--userns=keep-id -w /tmp/nntpcode "
                                   (format "--name %s " proj-name)
                                   "localhost/nntp:latest /bin/bash"))
                          (build-setup-cmd
                           (concat "if [ \"$(basename \"$PWD\")\" != \"build\" ]; then "
                                   "if [ ! -d \"build\" ]; then mkdir build && cd build && ln -s ../Makefile; "
                                   "else cd build; fi; "
                                   "fi")))

                     ;; Ensure vterm exists
                     (if (not v-buf)
                         (setq v-buf (vterm-other-window buf-name))
                       (pop-to-buffer v-buf))

                     ;; Local hook to scroll to bottom on output
                     (with-current-buffer v-buf
                       (setq-local vterm-scroll-to-bottom-on-output t))

                     ;; Ensure container is running
                     (unless (string-equal (string-trim is-running) "true")
                       (vterm-send-string (concat podman-run-cmd "\n"))
                       (sleep-for 0.6))

                     ;; Run setup and make
                     (vterm-send-string (concat "clear; " build-setup-cmd " && make\n"))))

                 ;; Intercept M-x compile
                 (defun my/vterm-compile-advice (orig-fun &rest args)
                   (if (string-equal (car args) "podman-make")
                       (my/nntp-compile-logic)
                     (apply orig-fun args)))
                 (advice-add 'compile :around #'my/vterm-compile-advice)

                 ;; Cleanup: Remove advice if this buffer is killed or we leave
                 (add-hook 'kill-buffer-hook
                           (lambda () (advice-remove 'compile #'my/vterm-compile-advice))
                           nil t)))

       ;; Change `compile' command
       (compile-command . "podman-make")

       ;; At work, we don't auto-format code :(
       (eval . (apheleia-mode -1)))))

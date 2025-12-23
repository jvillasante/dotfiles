;;; .dir-locals --- -*- no-byte-compile: t; lexical-binding: t; -*-
((nil .
      ((eval . (progn
                 ;; 1. The Core Logic
                 (defun my/nntp-compile-logic ()
                   "Ensures container is running and we are inside build dir before make."
                   (interactive)
                   (let* ((buf-name "*vterm-nntp*")
                          (v-buf (get-buffer buf-name))
                          (proj-dir (project-root (project-current t)))
                          (is-running (shell-command-to-string "podman inspect -f '{{.State.Running}}' nntp 2>/dev/null"))
                          (podman-run-cmd (format "podman run --user nntpuser --rm -it --volume %s:/tmp/nntpcode:rw,z --userns=keep-id -w /tmp/nntpcode --name nntp nntp:latest /bin/bash" proj-dir))
                          (build-setup-cmd "if [ \"$(basename \"$PWD\")\" != \"build\" ]; then if [ ! -d \"build\" ]; then mkdir build && cd build && ln -s ../Makefile; else cd build; fi; fi"))

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

                 ;; 2. Intercept M-x compile
                 (defun my/vterm-compile-advice (orig-fun &rest args)
                   (if (string-equal (car args) "podman-make")
                       (my/nntp-compile-logic)
                     (apply orig-fun args)))

                 (advice-add 'compile :around #'my/vterm-compile-advice)

                 ;; 3. Cleanup: Remove advice if this buffer is killed or we leave
                 (add-hook 'kill-buffer-hook
                           (lambda () (advice-remove 'compile #'my/vterm-compile-advice))
                           nil t)))

       (compile-command . "podman-make")
       (eval . (apheleia-mode -1)))))

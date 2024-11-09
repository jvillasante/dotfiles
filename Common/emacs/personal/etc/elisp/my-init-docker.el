;;; my-init-docker.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(defcustom my--docker-executable 'docker
    "The executable to be used with docker-mode."
    :type '(choice
            (const :tag "docker" docker)
            (const :tag "podman" podman))
    :group 'my)

(use-package docker
    :preface
    (defun my--docker-run-async-with-buffer-eat (program &rest args)
        "Execute \"PROGRAM ARGS\" and display output in a new `eat' buffer."
        (defvar eat-buffer-name)
        (defvar eat-kill-buffer-on-exit)
        (if (fboundp 'eat-other-window)
                (let* ((process-args (-remove 's-blank? (-flatten args)))
                       (eat-buffer-name (s-join " " (-insert-at 0 program process-args)))
                       (eat-kill-buffer-on-exit nil))
                    (eat-other-window eat-buffer-name args))
            (error "The eat package is not installed")))
    :config
    (when (eq my--docker-executable 'docker)
        (setq docker-command "docker")
        (setq docker-compose-command "docker-compose"))

    (when (eq my--docker-executable 'podman)
        (setq docker-command "podman")
        (setq docker-compose-command "podman-compose"))

    (when (package-installed-p 'eat)
        (setq docker-run-async-with-buffer-function #'my--docker-run-async-with-buffer-eat))

    ;; When docker run is called on an image whose repository name matches the regular expression "^postgres",
    ;; the option "-e POSTGRES_PASSWORD=postgres" will appear as set along with the defaults specified by `docker-image-run-default-args'.
    (add-to-list 'docker-image-run-custom-args
                 `("^postgres" ("-e POSTGRES_PASSWORD=postgres" . ,docker-image-run-default-args)))

    ;; docker run --user nntpuser --rm --interactive --tty --volume $selected:/tmp/nntpcode -w /tmp/nntpcode --name nntp nntp:latest /bin/bash
    (add-to-list 'docker-image-run-custom-args
                 `("^nntp" ("-u nntpuser"
                            "-v \"$HOME\"/Workspace/Work/Omicron/Projects/nntpcode:/tmp/nntpcode"
                            "-w /tmp/nntpcode"
                            "--name nntpcode" . ,docker-image-run-default-args))))

(use-package dockerfile-mode
    :mode "\\Dockerfile\\'"
    :config
    (when (eq my--docker-executable 'docker)
        (setq dockerfile-mode-command "docker"))

    (when (eq my--docker-executable 'podman)
        (setq dockerfile-mode-command "podman")))

(provide 'my-init-docker)
;;; my-init-docker.el ends here

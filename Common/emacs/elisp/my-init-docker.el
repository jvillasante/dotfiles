;;; my-init-docker.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(use-package docker
    :disabled t
    :defer t
    :preface
    (defun my/docker-run-async-with-buffer-eat (program &rest args)
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
    (pcase my/docker-executable
        ('docker
         (setf docker-command "docker"
               docker-compose-command "docker-compose"
               docker-container-tramp-method "docker"))
        ('podman
         (setf docker-command "podman"
               docker-compose-command "podman-compose"
               docker-container-tramp-method "podman")))

    ;; (when (package-installed-p 'eat)
    ;;     (setq docker-run-async-with-buffer-function #'my/docker-run-async-with-buffer-eat))
    (when (package-installed-p 'vterm)
        (setq docker-run-async-with-buffer-function #'docker-run-async-with-buffer-vterm))

    ;; When docker run is called on an image whose repository name matches the
    ;; regular expression "^postgres", the option "-e POSTGRES_PASSWORD=postgres"
    ;; will appear as set along with the defaults specified by `docker-image-run-default-args'.
    (add-to-list 'docker-image-run-custom-args
                 `("^postgres" ("-e POSTGRES_PASSWORD=postgres" . ,docker-image-run-default-args)))

    ;; podman run --user nntpuser --rm --interactive --tty --volume
    ;;    $selected:/tmp/nntpcode:rw,z --userns=keep-id
    ;;    -w /tmp/nntpcode --name nntp-$selected_name nntp:latest /bin/bash
    (add-to-list 'docker-image-run-custom-args
                 `("^localhost/nntp" ("-u nntpuser"
                                      "-v \"$HOME\"/Workspace/Work/Omicron/Projects/nntpcode:/tmp/nntpcode:rw,z --userns=keep-id"
                                      "-w /tmp/nntpcode"
                                      "--name nntp-emacs" . ,docker-image-run-default-args))))

;; (use-package devcontainer
;;     :vc (:url "git@github.com:johannes-mueller/devcontainer.el.git"
;;               :rev :newest)
;;     :config (setq devcontainer-engine 'podman))

(provide 'my-init-docker)
;;; my-init-docker.el ends here

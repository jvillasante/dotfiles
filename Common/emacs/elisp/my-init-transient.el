;;; my-init-transient.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; disproject : dispatch various project-related commands via Transient menus
(use-package disproject
    :disabled t
    ;; Replace `project-prefix-map' with `disproject-dispatch'.
    :bind (:map ctl-x-map
                ("p" . disproject-dispatch)))

;; TODO: Install the new casual packages...

(provide 'my-init-transient)
;;; my-init-transient.el ends here

;;; my-init-misc.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;;
;;; Code:

(use-package which-key
    :ensure t
    :config
    (which-key-mode))

(use-package uniquify
    :ensure nil
    :custom
    (uniquify-buffer-name-style 'reverse)
    (uniquify-separator "•")
    (uniquify-after-kill-buffer-p t)
    (uniquify-ignore-buffers-re "^\\*"))

(provide 'my-init-misc)
;;; my-init-misc.el ends here

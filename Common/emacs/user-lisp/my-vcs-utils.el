;;; my-vcs-utils.el --- Custom VCs utilities -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; Tell the compiler these functions exist so it doesn't panic.
(declare-function ansi-color-apply-on-region nil)
(declare-function vc-git-command nil)
(declare-function magit-mode-get-buffers nil)
(declare-function magit-restore-window-configuration nil)

;;;###autoload
(defun my/vc-git-reflog ()
    "Show git reflog in a new buffer with ANSI colors and custom keybindings."
    (interactive)
    (let* ((root (vc-root-dir))
              (buffer (get-buffer-create "*vc-git-reflog*")))
        (with-current-buffer buffer
            (setq-local vc-git-reflog-root root)
            (let ((inhibit-read-only t))
                (erase-buffer)
                (vc-git-command buffer nil nil
                    "reflog"
                    "--color=always"
                    "--pretty=format:%C(yellow)%h%Creset %C(auto)%d%Creset %Cgreen%gd%Creset %s %Cblue(%cr)%Creset")
                (goto-char (point-min))
                (ansi-color-apply-on-region (point-min) (point-max)))

            (let ((map (make-sparse-keymap)))
                (define-key map (kbd "/") #'isearch-forward)
                (define-key map (kbd "p") #'previous-line)
                (define-key map (kbd "n") #'next-line)
                (define-key map (kbd "q") #'kill-buffer-and-window)

                (use-local-map map))

            (setq buffer-read-only t)
            (setq mode-name "Git-Reflog")
            (setq major-mode 'special-mode))
        (pop-to-buffer buffer)))

;;;###autoload
(defun my/magit-kill-buffers ()
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
        (magit-restore-window-configuration)
        (mapc #'kill-buffer buffers)))

(provide 'my-vcs-utils)
;;; my-vcs-utils.el ends here

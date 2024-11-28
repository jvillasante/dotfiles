;;; my-init-early.el --- My Early Init -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;;
;;; Code:

;; Hide warnings and display only errors
(setq warning-minimum-level :error)

;; Configure Emacs to ask for confirmation before exiting
;; (setq confirm-kill-emacs 'y-or-n-p)

;; enable repeat-mode if available, see: `describe-repeat-maps'
(when (fboundp 'repeat-mode)
    (repeat-mode +1))

;; Emacs28 dictionary lookup
(when (fboundp 'dictionary-lookup-definition)
    (defvar dictionary-server)
    (setq dictionary-server "dict.org"))

;; disable risky local variables warning
(advice-add 'risky-local-variable-p :override #'ignore)

;; Prevent killing Messages buffer
(with-current-buffer "*Messages*" (emacs-lock-mode 'kill))

;; Prevent killing scratch buffer
(with-current-buffer "*scratch*" (emacs-lock-mode 'kill))

;; Printer
(setq printer-name "Brother_HL_L2370DW_series")
;; (setq lpr-command "lp")
;; (setq lpr-switches '("-d" "Brother_HL_L2370DW_series"))

;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(add-hook 'after-init-hook #'global-auto-revert-mode)

;; recentf is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(add-hook 'after-init-hook #'recentf-mode)

;; savehist is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(add-hook 'after-init-hook #'savehist-mode)

;; save-place-mode enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(add-hook 'after-init-hook #'save-place-mode)

(provide 'my-init-early)
;;; my-init-early.el ends here

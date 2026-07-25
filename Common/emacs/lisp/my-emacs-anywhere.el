;;; my-emacs-anywhere.el --- Edit in Emacs, paste back to any Wayland window  -*- lexical-binding: t -*-

;; Author: Julio C. Villasante <julio.villasante@omicronmedia.com>
;; Keywords: convenience, editing

;;; Commentary:
;;
;; A lightweight "Emacs anywhere" integration for KDE Plasma / Wayland.
;;
;; Workflow:
;;   1. A global shortcut triggers the `my-emacs-anywhere' shell script.
;;   2. The script pre-populates a temporary Markdown file with the current
;;      primary selection (highlighted text), if any, then opens it in a new
;;      Emacs frame via `emacsclient'.
;;   3. Write or edit your text, then:
;;        C-c C-c  copy to clipboard and paste into the previously focused window
;;        C-c C-k  copy to clipboard only, without pasting
;;
;; Signaling:
;;   The shell script blocks on a FIFO created alongside the temp file.  Emacs
;;   writes one of three signals to unblock it:
;;     "done"   (C-c C-c) -- copy buffer to clipboard and paste
;;     "copy"   (C-c C-k) -- copy buffer to clipboard, do not paste
;;     "cancel" (WM close) -- do nothing
;;   If you close the frame via the window manager instead of a keybinding,
;;   `my/emacs-anywhere--on-frame-delete' sends "cancel" automatically so the
;;   script never hangs.
;;
;; Requirements (outside Emacs):
;;   - my-emacs-anywhere  shell script (emacsclient + wl-copy + ydotool)
;;   - ydotoold           running as a user systemd service
;;   - xremap             configured to translate C-y -> C-v for non-Emacs apps

;;; Code:

(defvar-local my/emacs-anywhere--signaled nil
    "Non-nil once this session has been explicitly signaled via C-c C-c or C-c C-k.")

(defun my/emacs-anywhere-p ()
    "Return non-nil if the current buffer is an emacs-anywhere editing session."
    (and buffer-file-name
        (string-suffix-p ".md" buffer-file-name)
        (string-prefix-p temporary-file-directory buffer-file-name)))

(defun my/emacs-anywhere--write-signal (fifo msg)
    "Write MSG followed by a newline to FIFO to unblock the waiting shell script."
    (write-region (concat msg "\n") nil fifo nil 'quiet))

(defun my/emacs-anywhere--on-frame-delete (frame)
    "Send cancel if FRAME is closed without an explicit C-c C-c or C-c C-k.
Added to `delete-frame-functions' so that closing via the window manager X
button never leaves the shell script hanging on the FIFO."
    (dolist (win (window-list frame))
        (with-current-buffer (window-buffer win)
            (when (and (my/emacs-anywhere-p)
                      (not my/emacs-anywhere--signaled))
                (setq my/emacs-anywhere--signaled t)
                (my/emacs-anywhere--write-signal
                    (concat buffer-file-name ".fifo") "cancel")))))

(defun my/emacs-anywhere--signal (msg)
    "Mark session as signaled, close the frame, then write MSG to the FIFO.
Setting `my/emacs-anywhere--signaled' before calling `delete-frame' prevents
`my/emacs-anywhere--on-frame-delete' from sending a duplicate signal."
    (let ((fifo (concat buffer-file-name ".fifo")))
        (setq my/emacs-anywhere--signaled t)
        (delete-frame)
        (my/emacs-anywhere--write-signal fifo msg)))

;;;###autoload
(defun my/emacs-anywhere-done ()
    "Save the buffer and signal the shell script to paste its contents."
    (interactive)
    (save-buffer)
    (my/emacs-anywhere--signal "done"))

;;;###autoload
(defun my/emacs-anywhere-cancel ()
    "Save the buffer to the clipboard but close the frame without pasting."
    (interactive)
    (save-buffer)
    (my/emacs-anywhere--signal "copy"))

(defun my/emacs-anywhere-setup ()
    "Configure the current buffer for an emacs-anywhere editing session.
Activates `markdown-mode', suppresses the trailing-newline insertion that
would appear as a blank line at the paste target, and binds the finish and
cancel commands locally."
    (when (my/emacs-anywhere-p)
        (markdown-mode)
        (setq-local require-final-newline nil)
        (local-set-key (kbd "C-c C-c") #'my/emacs-anywhere-done)
        (local-set-key (kbd "C-c C-k") #'my/emacs-anywhere-cancel)))

(add-hook 'find-file-hook #'my/emacs-anywhere-setup)
(add-hook 'delete-frame-functions #'my/emacs-anywhere--on-frame-delete)

(provide 'my-emacs-anywhere)
;;; my-emacs-anywhere.el ends here

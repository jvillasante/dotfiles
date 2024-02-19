;;; my-utils.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(require 'cl-lib)

;;; Constants

(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-LINUX (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))


;;; Macros

(defmacro setq! (&rest settings)
    "A more sensible `setopt' for SETTINGS customizable variables.

This can be used as a drop-in replacement for `setq' and *should* be used
instead of `setopt'.  Unlike `setq', this triggers custom setters on variables.
Unlike `setopt', this won't needlessly pull in dependencies."
    (macroexp-progn
     (cl-loop for (var val) on settings by 'cddr
              collect `(funcall (or (get ',var 'custom-set) #'set-default-toplevel-value)
                                ',var ,val))))

(defmacro delq! (elt list &optional fetcher)
    "`delq' ELT from LIST in-place.

If FETCHER is a function, ELT is used as the key in LIST (an alist)."
    `(setq ,list (delq ,(if fetcher
                                `(funcall ,fetcher ,elt ,list)
                            elt)
                       ,list)))

;;; UI

(defun my--font-installed-p (font-name)
    "Check if a font with FONT-NAME is available."
    (find-font (font-spec :name font-name)))

(defun my--font-set-small-mono-font ()
    "Set the default font to a smaller sized font for current buffer."
    (face-remap-add-relative 'default :height 145))

(defun my--font-set-small-variable-font ()
    "Set the default font to a smaller sized font for current buffer."
    (face-remap-add-relative 'default :height 145 :family "Iosevka Aile"))

;;; VCS

(defun my--project-todos ()
    "Find `hl-todo--regex' items in project using `consult-ripgrep'."
    (interactive)
    (defvar hl-todo--regexp)
    (when (and (featurep 'consult) (fboundp 'consult-ripgrep))
        (consult-ripgrep nil hl-todo--regexp)))

;;; Elisp

(defun my--helpful-lookup-symbl-at-point ()
    "Look up for the symbol under point."
    (interactive)
    (when (and (featurep 'helpful) (fboundp 'helpful-symbol))
        (helpful-symbol (symbol-at-point))))

(defun my--elisp-look-up-symbol (beg end)
    "Look up for the symbol under point.
If region (BEG to END) is active, use the selected region as the symbol."
    (interactive "r")
    (when (and (featurep 'helpful) (fboundp 'helpful-symbol))
        (if (use-region-p)
                (helpful-symbol (intern (buffer-substring beg end)))
            (helpful-symbol (symbol-at-point)))))

;;; OS

(defun my--macos-cmd-w ()
    "If there is only one tab, close EMACS, otherwise close one tab."
    (interactive)
    (if (> (length (tab-bar-tabs)) 1)
            (tab-bar-close-tab)
        (kill-emacs)))

;;; Misc

(defun my--clone-buffer-in-new-window-readonly ()
    "Clone the current buffer in a new window.

Make it readonly, and set up a keybinding (q) to close the window."
    (interactive)
    (let ((clone-buffer (clone-indirect-buffer (buffer-name) t)))
        (with-current-buffer clone-buffer
            (read-only-mode t)
            (let ((map (make-sparse-keymap)))
                (define-key map (kbd "q") (lambda ()
                                              (interactive)
                                              (kill-buffer-and-window)))
                (use-local-map map)))
        (switch-to-buffer-other-window clone-buffer)))

(defun my--file-is-root-p (name)
    "Check whether tramp su/sudo method is used for opening filepath NAME."
    ;; Adopted from https://www.gnu.org/software/emacs/manual/html_node/tramp/Auto_002dsave-File-Lock-and-Backup.html
    (let ((method (file-remote-p name 'method)))
        (when (stringp method)
            (member method '("su" "sudo")))))

(defun my--file-is-not-root-p (name)
    "Check whether tramp su/sudo method is not used for opening filepath NAME."
    (not (my--file-is-root-p name)))

(defun my--project-root-or-default-dir ()
    "If a project root is found, return it. Otherwise return `default-directory'."
    (when (fboundp 'project-root)
        (if-let ((proj (project-current)))
                (project-root proj)
            default-directory)))

(defun my--switch-to-messages-buffer ()
    "Stolen from spacemacs."
    (interactive)
    (with-current-buffer (messages-buffer)
        (goto-char (point-max))
        (switch-to-buffer (current-buffer))))

(defun my--dos2unix ()
    "Replace DOS eolns CR LF with Unix eolns CR."
    (interactive)
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t) (replace-match "")))

(defun my--hide-dos-eol ()
    "Hide ^M in files containing mixed UNIX and DOS line endings."
    (interactive)
    (setq buffer-display-table (make-display-table))
    (aset buffer-display-table ?\^M []))

(defun my--show-dos-eol ()
    "Show ^M in files containing mixed UNIX and DOS line endings."
    (interactive)
    (setq buffer-display-table (make-display-table))
    (aset buffer-display-table ?\^M ?\^M))

(defun my--switch-theme (theme)
    "This interactive call is taken from `load-theme'.
Switch the current theme to THEME"
    (interactive
     (list
      (intern (completing-read "Load custom theme: "
                               (mapcar 'symbol-name
                                       (custom-available-themes))))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t))

(defun my--save-all ()
    "Save all buffers without prompt."
    (interactive)
    (let ((modified-count
           (length (cl-loop for buf in (buffer-list)
                            when (and (buffer-file-name buf) (buffer-modified-p buf))
                            collect buf))))
        (save-excursion
            (save-some-buffers t)
            (message "%d buffer(s) saved" modified-count))))

(defun my--fill-or-unfill ()
    "Like `fill-paragraph', but `unfill' if used twice.
Taken from: https://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html."
    (interactive)
    (let ((fill-column
           (if (eq last-command 'my--fill-or-unfill)
                   (progn (setq this-command nil)
                          (point-max))
               fill-column)))
        (call-interactively #'fill-paragraph)))

(defun my--org-fill-or-unfill ()
    "Like `org-fill-paragraph', but `unfill' if used twice.
Taken from: https://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html."
    (interactive)
    (when (fboundp 'org-fill-paragraph)
        (let ((fill-column
               (if (eq last-command 'my--org-fill-or-unfill)
                       (progn (setq this-command nil)
                              (point-max))
                   fill-column)))
            (call-interactively #'org-fill-paragraph))))

(defun my--fill-buffer ()
    "Fill the entire current buffer."
    (interactive)
    (save-excursion
        (save-restriction
            (widen)
            (fill-region (point-min) (point-max)))))

(defun my--unfill-paragraph (&optional region)
    "Takes a REGION and make it into a single line of text.
Taken from: https://www.emacswiki.org/emacs/UnfillParagraph."
    (interactive (progn (barf-if-buffer-read-only) '(t)))
    (let ((fill-column (point-max))
          ;; This would override `fill-column' if it's an integer.
          (emacs-lisp-docstring-fill-column t))
        (fill-paragraph nil region)))

(defun my--unfill-region (beg end)
    "Unfill the region (BEG - END).
Joining text paragraphs into a single
logical line.  This is useful, e.g., for use with `visual-line-mode'.
Taken from: https://www.emacswiki.org/emacs/UnfillRegion."
    (interactive "*r")
    (let ((fill-column (point-max)))
        (fill-region beg end)))

(defun my--unfill-buffer ()
    "Undo filling for all paragraphs.
Taken from: https://www.emacswiki.org/emacs/UndoFilling."
    (interactive)
    (goto-char (point-min))
    (let ((fill-column 99999))
        (fill-paragraph nil)
        (while (< (point) (point-max))
            (forward-paragraph)
            (fill-paragraph nil))))

(defun my--toggle-window-split ()
    "Toggle between vertical and horizontal split.
Vertical split shows more of each line, horizontal split shows
more lines. This code toggles between them. It only works for
frames with exactly two windows. Taken from:
https://www.emacswiki.org/emacs/ToggleWindowSplit"
    (interactive)
    (if (= (count-windows) 2)
            (let* ((this-win-buffer (window-buffer))
                   (next-win-buffer (window-buffer (next-window)))
                   (this-win-edges (window-edges (selected-window)))
                   (next-win-edges (window-edges (next-window)))
                   (this-win-2nd (not (and (<= (car this-win-edges)
                                               (car next-win-edges))
                                           (<= (cadr this-win-edges)
                                               (cadr next-win-edges)))))
                   (splitter
                    (if (= (car this-win-edges)
                           (car (window-edges (next-window))))
                            'split-window-horizontally
                        'split-window-vertically)))
                (delete-other-windows)
                (let ((first-win (selected-window)))
                    (funcall splitter)
                    (if this-win-2nd (other-window 1))
                    (set-window-buffer (selected-window) this-win-buffer)
                    (set-window-buffer (next-window) next-win-buffer)
                    (select-window first-win)
                    (if this-win-2nd (other-window 1))))))

(defun my--new-scratch-buffer-in-markdown ()
    "Make a temporary buffer in markdown mode and switch to it."
    (interactive)
    (when (fboundp 'markdown-mode)
        (switch-to-buffer (make-temp-name "scratch-"))
        (markdown-mode)))

(defun my--comment-or-uncomment ()
    "Comments or uncomments the current line or region."
    (interactive)
    (if (region-active-p)
            (comment-or-uncomment-region
             (region-beginning)(region-end))
        (comment-or-uncomment-region
         (line-beginning-position)(line-end-position))))

;; Behave like vi's o command
(defun my--open-next-line (arg)
    "Move to the next line and then opens ARG lines."
    (interactive "p")
    (end-of-line)
    (open-line arg)
    (forward-line 1)
    (when electric-indent-mode
        (indent-according-to-mode)))

;; Behave like vi's O command
(defun my--open-previous-line (arg)
    "Open ARG lines before the current one."
    (interactive "p")
    (beginning-of-line)
    (open-line arg)
    (when electric-indent-mode
        (indent-according-to-mode)))

;; Movement

(defun my--move-character-backward ()
    "Move character backward."
    (interactive)
    (transpose-chars 1)
    (backward-char 2))

(defun my--move-character-forward ()
    "Move character forward."
    (interactive)
    (forward-char 1)
    (transpose-chars 1)
    (backward-char))

(defun my--move-word-backwards ()
    "Move word backwards."
    (interactive)
    (backward-to-word 1)
    (transpose-words 1)
    (backward-word-strictly 2))

(defun my--move-word-forward ()
    "Move word forward."
    (interactive)
    (forward-to-word 1)
    (transpose-words 1)
    (backward-word))

(defun my--move-sentence-backward ()
    "Move sentence backward."
    (interactive)
    (transpose-sentences 1)
    (backward-sentence 2))

(defun my--move-sentence-forward ()
    "Move sentence forward."
    (interactive)
    (forward-sentence 1)
    (transpose-sentences 1)
    (backward-sentence))

(defun my--move-paragraph-backward ()
    "Move paragraph up."
    (interactive)
    (transpose-paragraphs -1)
    (backward-paragraph)
    (forward-line))

(defun my--move-paragraph-forward ()
    "Move paragraph down."
    (interactive)
    (transpose-paragraphs 1)
    (backward-paragraph)
    (forward-line))

(defun my--move-sexp-backward ()
    "Move sexp backward."
    (interactive)
    (transpose-sexps 1)
    (backward-sexp 2))

(defun my--move-sexp-forward ()
    "Move sexp forward."
    (interactive)
    (forward-sexp 1)
    (transpose-sexps 1)
    (backward-sexp))

(defun my--move-line-backward ()
    "Move line up."
    (interactive)
    (transpose-lines 1)
    (forward-line -2))

(defun my--move-line-forward ()
    "Move line down."
    (interactive)
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1))

(defun my--move-line-up ()
    "Move up the current line."
    (interactive)
    (transpose-lines 1)
    (forward-line -2)
    (when electric-indent-mode
        (indent-according-to-mode)))

(defun my--move-line-down ()
    "Move down the current line."
    (interactive)
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (when electric-indent-mode
        (indent-according-to-mode)))

(provide 'my-utils)
;;; my-utils.el ends here

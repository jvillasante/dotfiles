;;; my-bindings.el --- Custom Bindings -*- lexical-binding: t; -*-

;;; Commentary:
;;; Bindings Customizations

;;; Code:
(crafted-package-install-package 'general)
(progn
    ;; * Global Keybindings
    ;; `general-define-key' acts like `global-set-key' when :keymaps is not
    ;; specified (because ":keymaps 'global" is the default)
    ;; kbd is not necessary and arbitrary amount of key def pairs are allowed
    (general-define-key
        ;;; general keys
        "C-x C-m" 'execute-extended-command ; ALT may or may not be available
        "C-c C-m" 'execute-extended-command ; ALT may or may not be available
        "C-c u" 'browse-url-at-point ; simple browse url
        "C-x k" 'kill-this-buffer ; kill buffer without prompt
        "C-x K" 'kill-buffer ; prompt for buffer to kill
        "M-/" 'hippie-expand ; use hippie-expand instead of debbrev
        "C-x S" #'+my/save-all ; save some buffers without prompt
        "C-z" nil ; suspend frame should go away
        "C-x C-z" nil ; same
        ;; [remap list-buffers] 'ibuffer ; ibuffer is better than list-buffers

        ;;; upcase, downcase and capitalize
        "M-u" 'upcase-dwim
        "M-l" 'downcase-dwim
        "M-c" 'capitalize-dwim

        ;;; hippie-expand is a better dabbrev
        [remap dabbrev-expand] 'hippie-expand

        ;;; fill-unfill
        [remap fill-paragraph] #'+my/fill-or-unfill
        "M-Q"                  #'+my/unfill-paragraph
        "C-M-Q"                #'+my/unfill-region

        ;;; avy
        (when (crafted-package-installed-p 'avy)
            "C-." 'avy-goto-char-timer)

        ;; undo
        (when (crafted-package-installed-p 'undo-fu)
            "C-/" 'undo-fu-only-undo
            "C-?" 'undo-fu-only-redo)

        ;;;; ace-window
        [remap other-window] 'ace-window ; better other window

        ;;;; anzu
        "M-%" 'anzu-query-replace
        "C-M-%" 'anzu-query-replace-regexp

        ;;; expand-region
        (when (crafted-package-installed-p 'expand-region)
            "C-=" 'er/expand-region
            "C--" 'er/contract-region)

        ;;; magit
        (when (crafted-package-installed-p 'magit)
            "C-x g" 'magit-status)

        ;; crux
        ;; "C-c o" 'crux-open-with
        ;; "C-c u" 'crux-view-url
        "C-o" 'crux-smart-open-line
        "M-o" 'crux-smart-open-line-above
        "C-x C-r" 'crux-recentf-find-file
        "C-c f" 'crux-recentf-find-file
        "C-c F" 'crux-recentf-find-directory
        ;; "C-c n" 'crux-cleanup-buffer-or-region
        "C-M-z" 'crux-indent-defun
        "C-c e" 'crux-eval-and-replace
        "C-c w" 'crux-swap-windows
        "C-c D" 'crux-delete-file-and-buffer
        "C-c r" 'crux-rename-buffer-and-file
        "C-c t" 'crux-visit-term-buffer
        "C-c k" 'crux-kill-other-buffers
        "C-c TAB" 'crux-indent-rigidly-and-copy-to-clipboard
        "C-c I" 'crux-find-user-custom-file
        "C-c S" ' crux-find-shell-init-file
        "C-^" 'crux-top-join-line
        "C-c s" 'crux-ispell-word-then-abbrev
        "C-k" 'crux-smart-kill-line
        "C-<backspace>" 'crux-kill-line-backwards
        "C-x 4 t" 'crux-transpose-windows
        "C-x C-u" 'crux-upcase-region
        "C-x C-l" 'crux-downcase-region
        "C-x M-c" 'crux-capitalize-region
        [remap move-beginning-of-line] 'crux-move-beginning-of-line
        [shift return] 'crux-smart-open-line
        [control shift return] 'crux-smart-open-line-above
        [remap kill-whole-line] 'crux-kill-whole-line)

    ;; * Prefix Keybindings
    ;; :prefix can be used to prevent redundant specification of prefix keys
    (general-define-key :prefix "C-c c" ; code
        "f" 'format-all-buffer)

    (general-define-key :prefix "C-c n" ; notes
        "d" 'deft-find-file
        "D" 'deft)

    (general-define-key :prefix "C-c o" ; open
        "c" 'calc
        "C" 'quick-calc
        "e" 'elfeed
        "p" 'neotree-toggle
        "v" 'vterm)

    ;; * Mode Keybindings
    ;; `general-define-key' is comparable to `define-key' when :keymaps is specified
    (general-define-key :keymaps 'ctl-x-4-map
        "t" '+my/toggle-window-split)

    (general-define-key :keymaps 'org-mode-map
        [remap fill-paragraph] '+my/org-fill-or-unfill)

    (general-define-key :keymaps 'dired-mode-map
        "C-c o" 'crux-open-with)

    (general-define-key :keymaps 'isearch-mode-map
        ;; Prevents issue where you have to press backspace twice when
        ;; trying to remove the first character that fails a search
        [remap isearch-delete-char] #'isearch-del-char
        "C-n" 'isearch-repeat-forward ; better navigation
        "C-p" 'isearch-repeat-backward ; better navigation
        "M-e" 'consult-isearch-history ; orig. isearch-edit-string
        "M-s e" 'consult-isearch-history ; orig. isearch-edit-string
        "M-s l" 'consult-line ; needed by consult-line to detect isearch
        "M-s L" 'consult-line-multi) ; needed by consult-line to detect isearch

    (general-define-key :keymaps 'ibuffer-mode-map
        "q" 'kill-this-buffer)

    (general-define-key :keymaps 'smartparens-mode-map
        "C-M-<backspace>" 'sp-backward-kill-sexp)

    (general-define-key :keymaps 'vterm-mode-map
        "M-[" 'vterm-copy-mode
        "C-y" 'vterm-yank)

    (general-define-key :keymaps 'neotree-mode-map
        "TAB" 'neotree-stretch-toggle)

    (general-define-key :keymaps 'minibuffer-local-map
        "M-s" 'consult-history ; orig. next-matching-history-element
        "M-r" 'consult-history) ; orig. previous-matching-history-element
    )

(provide 'my-bindings)
;;; my-bindings.el ends here

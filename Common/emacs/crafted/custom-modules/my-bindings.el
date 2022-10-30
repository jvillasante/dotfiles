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

        ;;; for some reason, crafted-emacs remaps C-s to `consult-line'
        "C-s" 'isearch-forward

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
        "C-." 'avy-goto-char-timer

        ;;; ace-window
        ;; [remap other-window] 'ace-window ; better other window

        ;;; anzu
        "M-%" 'anzu-query-replace
        "C-M-%" 'anzu-query-replace-regexp

        ;;; expand-region
        "C-=" 'er/expand-region
        "C--" 'er/contract-region

        ;;; magit
        "C-x g" 'magit-status

        ;;; embark
        "C-;" 'embark-act        ; pick some comfortable binding
        "C-M-;" 'embark-dwim     ; good alternative: "M-."
        "C-h B" 'embark-bindings ; alternative for `describe-bindings'

        ;;; consult
        ;; C-c bindings (mode-specific-map)
        "C-c h" 'consult-history
        "C-c m" 'consult-mode-command
        ;; "C-c k" 'consult-kmacro
        ;; C-x bindings (Ctl-x-map)
        "C-x M-:" 'consult-complex-command     ; orig. repeat-complex-command
        "C-x b" 'consult-buffer                ; orig. switch-to-buffer
        "C-x 4 b" 'consult-buffer-other-window ; orig. switch-to-buffer-other-window
        "C-x 5 b" 'consult-buffer-other-frame  ; orig. switch-to-buffer-other-frame
        "C-x r b" 'consult-bookmark            ; orig. bookmark-jump
        "C-x p b" 'consult-project-buffer      ; orig. project-switch-to-buffer
        ;; Custom M-# bindings for fast register access
        "M-#" 'consult-register-load
        "M-'" 'consult-register-store          ; orig. abbrev-prefix-mark (unrelated)
        "C-M-#" 'consult-register
        ;; Other custom bindings
        "M-y" 'consult-yank-pop                ; orig. yank-pop
        "<help> a" 'consult-apropos            ; orig. apropos-command
        ;; M-g bindings (goto-map)
        "M-g e" 'consult-compile-error
        "M-g f" 'consult-flymake               ; Alternative: consult-flycheck
        "M-g g" 'consult-goto-line             ; orig. goto-line
        "M-g M-g" 'consult-goto-line           ; orig. goto-line
        "M-g o" 'consult-outline               ; Alternative: consult-org-heading
        "M-g m" 'consult-mark
        "M-g k" 'consult-global-mark
        "M-g i" 'consult-imenu
        "M-g I" 'consult-imenu-multi
        ;; M-s bindings (search-map)
        "M-s d" 'consult-find
        "M-s D" 'consult-locate
        "M-s g" 'consult-grep
        "M-s G" 'consult-git-grep
        "M-s r" 'consult-ripgrep
        "M-s l" 'consult-line
        "M-s L" 'consult-line-multi
        "M-s m" 'consult-multi-occur
        "M-s k" 'consult-keep-lines
        "M-s u" 'consult-focus-lines
        ;; Isearch integration
        "M-s e" 'consult-isearch-history

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
        ;; "C-<backspace>" 'crux-kill-line-backwards
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

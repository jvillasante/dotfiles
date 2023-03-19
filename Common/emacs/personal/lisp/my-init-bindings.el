;;; my-init-bindings.el --- -*- lexical-binding: t; -*-

(general-define-key
    :keymaps 'override
    ;; general keys
    "C-x C-m" 'execute-extended-command ; ALT may or may not be available
    "C-c C-m" 'execute-extended-command ; ALT may or may not be available
    "C-c u"   'browse-url-at-point ; simple browse url
    "C-x k"   'kill-this-buffer ; kill buffer without prompt
    "C-x K"   'kill-buffer ; prompt for buffer to kill
    "C-x S"   'my/save-all              ;; save some buffers without prompt
    "C-z"     nil                         ;; suspend frame should go away
    "C-x C-z" nil                         ;; same

    ;; M is a Ctrl on steroids
    "M-n" 'forward-paragraph
    "M-p" 'backward-paragraph

    ;; I use this all the time
    "C-<" 'beginning-of-buffer
    "C->" 'end-of-buffer

    ;; upcase, downcase and capitalize
    "M-u" 'upcase-dwim
    "M-l" 'downcase-dwim
    "M-c" 'capitalize-dwim

    ;; better comment/un-comment
    "M-;" 'my/comment-or-uncomment
    "C-x C-;" 'my/comment-or-uncomment

    ;; zap
    "M-S-z" 'zap-up-to-char ;; New in Emacs 28

    ;; ibuffer is better then list-buffers
    [remap list-buffers] 'ibuffer

    ;; hippie-expand is a better dabbrev
    [remap dabbrev-expand] 'hippie-expand

    ;;; easy-kill
    ;; [remap kill-ring-save] 'easy-kill

    ;; fill-unfill
    [remap fill-paragraph] 'my/fill-or-unfill
    "M-Q"                  'my/unfill-paragraph
    "C-M-Q"                'my/unfill-region

    ;; avy
    "M-j" 'avy-goto-char-timer ; most usefull avy function

    ;; ace-window
    ;; [remap other-window] 'ace-window ; better other window

    ;; anzu
    "M-%" 'anzu-query-replace
    "C-M-%" 'anzu-query-replace-regexp

    ;; expand-region
    "C-=" 'er/expand-region
    "C--" 'er/contract-region

    ;; magit
    "C-x g" 'magit-status

    ;; embark
    "C-." 'embark-act        ; pick some comfortable binding
    "C-;" 'embark-dwim       ; good alternative: "M-."
    "C-h B" 'embark-bindings ; alternative for `describe-bindings'

    ;; projectile
    ;; "C-c p" 'projectile-command-map

    ;; helpful
    ;; (define-key helpful-mode-map [remap revert-buffer] #'helpful-update)
    [remap describe-command] 'helpful-command
    [remap describe-function] 'helpful-callable
    [remap describe-key] 'helpful-key
    [remap describe-symbol] 'helpful-symbol
    [remap describe-variable] 'helpful-variable
    "C-h F" 'helpful-function
    "C-h K" 'describe-keymap

    ;; consult
    [remap apropos] #'consult-apropos
    [remap bookmark-jump] #'consult-bookmark
    [remap goto-line] #'consult-goto-line
    [remap imenu] #'consult-imenu
    [remap locate] #'consult-locate
    [remap load-theme] #'consult-theme
    [remap man] #'consult-man
    [remap recentf-open-files] #'consult-recent-file
    [remap switch-to-buffer] #'consult-buffer
    [remap switch-to-buffer-other-window] #'consult-buffer-other-window
    [remap switch-to-buffer-other-frame] #'consult-buffer-other-frame
    [remap yank-pop] #'consult-yank-pop
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
    "d" 'lsp-describe-thing-at-point
    "f" 'format-all-buffer)
(general-define-key :prefix "C-c n" ; notes
    "d" 'deft-find-file
    "D" 'deft)
(general-define-key :prefix "C-c o" ; open
    "c" 'calc
    "C" 'quick-calc
    "f" 'elfeed
    "p" 'dired-sidebar-toggle-sidebar
    "t" 'vterm
    "e" 'eshell)

;; * Mode Keybindings
;; `general-define-key' is comparable to `define-key' when :keymaps is specified
(general-define-key :keymaps 'org-mode-map
    [remap fill-paragraph] #'my/org-fill-or-unfill)
(general-define-key :keymaps 'ibuffer-mode-map
    "q" 'kill-this-buffer)
(general-define-key :keymaps 'dired-mode-map
    "C-u RET" 'crux-open-with
    "C-u return" 'crux-open-with)
(general-define-key :keymaps 'isearch-mode-map
    [remap isearch-delete-char] 'isearch-del-char
    "C-n" 'isearch-repeat-forward          ; move forward
    "C-p" 'isearch-repeat-backward         ; move backward
    "M-e" 'consult-isearch-history         ; orig. isearch-edit-string
    "M-s e" 'consult-isearch-history       ; orig. isearch-edit-string
    "M-s l" 'consult-line                  ; needed by consult-line to detect isearch
    "M-s L" 'consult-line-multi)           ; needed by consult-line to detect isearch
(general-define-key :keymaps 'vterm-mode-map
    "C-x [" 'vterm-copy-mode
    "M-[" 'vterm-copy-mode
    "C-y" 'vterm-yank
    "C-q" 'vterm-send-next-key)
(general-define-key :keymaps 'minibuffer-local-map
    "M-s" 'consult-history       ; orig. next-matching-history-element
    "M-r" 'consult-history)     ; orig. previous-matching-history-element

(provide 'my-init-bindings)
;;; my-init-bindings.el ends here
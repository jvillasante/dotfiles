;;; my-init-bindings.el --- -*- lexical-binding: t; -*-

(straight-use-package 'general)

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

    ;; I use this all the time
    ;; "C-<" 'beginning-of-buffer
    ;; "C->" 'end-of-buffer

    ;; Clone the current buffer in a new window with `q' to exit
    "C-x 9" 'my/clone-buffer-in-new-window-readonly

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

    ;; fill-unfill
    [remap fill-paragraph] 'my/fill-or-unfill
    "M-Q"                  'my/unfill-paragraph
    "C-M-Q"                'my/unfill-region

    ;; avy
    "M-j" 'avy-goto-char-timer ; most usefull avy function

    ;; anzu
    "M-%" 'anzu-query-replace
    "C-M-%" 'anzu-query-replace-regexp

    ;; expand-region
    "C-=" 'er/expand-region
    "C--" 'er/contract-region

    ;; magit
    "C-x g" 'magit-status

    ;; embark
    "C-;" 'embark-act        ; pick some comfortable binding
    ;; "C-;" 'embark-dwim       ; good alternative: "M-."
    "C-h B" 'embark-bindings ; alternative for `describe-bindings'

    ;; helpful
    ;; (define-key helpful-mode-map [remap revert-buffer] #'helpful-update)
    [remap describe-command] 'helpful-command
    [remap describe-function] 'helpful-callable
    [remap describe-key] 'helpful-key
    [remap describe-symbol] 'helpful-symbol
    [remap describe-variable] 'helpful-variable
    "C-h F" 'helpful-function
    "C-h K" 'describe-keymap

    ;; crux
    ;; "C-c o" 'crux-open-with
    ;; "C-c u" 'crux-view-url
    "C-o" 'crux-smart-open-line
    "M-o" 'crux-smart-open-line-above
    ;; "C-x C-r" 'crux-recentf-find-file
    ;; "C-c f" 'crux-recentf-find-file
    ;; "C-c F" 'crux-recentf-find-directory
    ;; "C-c n" 'crux-cleanup-buffer-or-region
    "C-M-z" 'crux-indent-defun
    "C-c e" 'crux-eval-and-replace
    "C-c w" 'crux-swap-windows
    "C-c D" 'crux-delete-file-and-buffer
    "C-c r" 'crux-rename-buffer-and-file
    ;; "C-c t" 'crux-visit-term-buffer
    "C-c k" 'crux-kill-other-buffers
    ;; "C-c TAB" 'crux-indent-rigidly-and-copy-to-clipboard
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
(general-define-key :prefix "M-s" ; search
    "M-x" 'my/web-search-xwidget
    "M-w" 'my/web-search-eww
    "M-b" 'my/web-search-browser)

(general-define-key :prefix "C-c f" ; find
    "f" #'project-find-file
    "F" #'find-file
    "D" #'project-dired
    "o" #'consult-recent-file
    "r" #'consult-yank-from-kill-ring
    "b" #'consult-buffer
    "p" #'project-switch-project
    "g" #'consult-ripgrep
    "a" #'embark-act
    "j" #'evil-collection-consult-jump-list
    "m" #'evil-collection-consult-mark
    "i" #'consult-imenu
    "I" #'consult-imenu-multi
    "l" #'consult-line
    "L" #'consult-line-multi)

(general-define-key :prefix "C-c c" ; code
    ;; "d" 'lsp-describe-thing-at-point
    "f" 'my/toggle-fold)

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

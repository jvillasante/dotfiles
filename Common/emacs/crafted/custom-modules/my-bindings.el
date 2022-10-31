;;; my-bindings.el --- Custom Bindings -*- lexical-binding: t; -*-
;;;
;;; Links:
;;; https://www.masteringemacs.org/article/mastering-key-bindings-emacs

;;; Commentary:
;;; Bindings Customizations

;;; Code:

;;; Some globals
(global-set-key (kbd "C-x C-m") 'execute-extended-command) ; ALT may or may not be available
(global-set-key (kbd "C-c C-m") 'execute-extended-command) ; ALT may or may not be available
(global-set-key (kbd "C-c u") 'browse-url-at-point) ; simple browse url
(global-set-key (kbd "C-x k") 'kill-this-buffer) ; kill buffer without prompt
(global-set-key (kbd "C-x K") 'kill-buffer) ; prompt for buffer to kill
(global-set-key (kbd "M-/") 'hippie-expand) ; use hippie-expand instead of debbrev
(global-set-key (kbd "C-x S") '+my/save-all) ; save some buffers without prompt
(global-set-key (kbd "C-z") nil) ; suspend frame should go away
(global-set-key (kbd "C-x C-z") nil) ; same

;; for some reason, crafted-emacs remaps C-s to `consult-line'
(global-set-key (kbd "C-s") 'isearch-forward)

;; upcase, downcase and capitalize
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

;; hippie-expand is a better dabbrev
(define-key global-map [remap dabbrev-expand] 'hippie-expand)

;; fill-unfill
(define-key global-map [remap fill-paragraph] '+my/fill-or-unfill)
(global-set-key (kbd "M-Q") '+my/unfill-paragraph)
(global-set-key (kbd "C-M-Q") '+my/unfill-region)

;; avy
(global-set-key (kbd "C-.") 'avy-goto-char-timer)

;; ace-window
;; (define-key global-map [remap other-window] 'ace-window)

;; anzu
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; embark
(global-set-key (kbd "C-;") 'embark-act)        ; pick some comfortable binding
(global-set-key (kbd "C-M-;") 'embark-dwim)     ; good alternative: "M-."
(global-set-key (kbd "C-h B") 'embark-bindings) ; alternative for `describe-bindings'

;; consult
;; C-c bindings (mode-specific-map)
(global-set-key (kbd "C-c h") 'consult-history)
(global-set-key (kbd "C-c m") 'consult-mode-command)
;; (global-set-key (kbd "C-c k") 'consult-kmacro)
;; C-x bindings (Ctl-x-map)
(global-set-key (kbd "C-x M-:") 'consult-complex-command)     ; orig. repeat-complex-command
(global-set-key (kbd "C-x b") 'consult-buffer)                ; orig. switch-to-buffer
(global-set-key (kbd "C-x 4 b") 'consult-buffer-other-window) ; orig. switch-to-buffer-other-window
(global-set-key (kbd "C-x 5 b") 'consult-buffer-other-frame)  ; orig. switch-to-buffer-other-frame
(global-set-key (kbd "C-x r b") 'consult-bookmark)            ; orig. bookmark-jump
(global-set-key (kbd "C-x p b") 'consult-project-buffer)      ; orig. project-switch-to-buffer
;; Custom M-# bindings for fast register access
(global-set-key (kbd "M-#") 'consult-register-load)
(global-set-key (kbd "M-'") 'consult-register-store)          ; orig. abbrev-prefix-mark (unrelated)
(global-set-key (kbd "C-M-#") 'consult-register)
;; Other custom bindings
(global-set-key (kbd "M-y") 'consult-yank-pop)                ; orig. yank-pop
(global-set-key (kbd "<help> a") 'consult-apropos)            ; orig. apropos-command
;; M-g bindings (goto-map)
(global-set-key (kbd "M-g e") 'consult-compile-error)
(global-set-key (kbd "M-g f") 'consult-flymake)               ; Alternative: consult-flycheck
(global-set-key (kbd "M-g g") 'consult-goto-line)             ; orig. goto-line
(global-set-key (kbd "M-g M-g") 'consult-goto-line)           ; orig. goto-line
(global-set-key (kbd "M-g o") 'consult-outline)               ; Alternative: consult-org-heading
(global-set-key (kbd "M-g m") 'consult-mark)
(global-set-key (kbd "M-g k") 'consult-global-mark)
(global-set-key (kbd "M-g i") 'consult-imenu)
(global-set-key (kbd "M-g I") 'consult-imenu-multi)
;; M-s bindings (search-map)
(global-set-key (kbd "M-s d") 'consult-find)
(global-set-key (kbd "M-s D") 'consult-locate)
(global-set-key (kbd "M-s g") 'consult-grep)
(global-set-key (kbd "M-s G") 'consult-git-grep)
(global-set-key (kbd "M-s r") 'consult-ripgrep)
(global-set-key (kbd "M-s l") 'consult-line)
(global-set-key (kbd "M-s L") 'consult-line-multi)
(global-set-key (kbd "M-s m") 'consult-multi-occur)
(global-set-key (kbd "M-s k") 'consult-keep-lines)
(global-set-key (kbd "M-s u") 'consult-focus-lines)
;; Isearch integration
(global-set-key (kbd "M-s e") 'consult-isearch-history)

;; crux
;; (global-set-key (kbd "C-c o") 'crux-open-with)
;; (global-set-key (kbd "C-c u") 'crux-view-url)
(global-set-key (kbd "C-o") 'crux-smart-open-line)
(global-set-key (kbd "M-o") 'crux-smart-open-line-above)
(global-set-key (kbd "C-x C-r") 'crux-recentf-find-file)
(global-set-key (kbd "C-c f") 'crux-recentf-find-file)
(global-set-key (kbd "C-c F") 'crux-recentf-find-directory)
;; (global-set-key (kbd "C-c n") 'crux-cleanup-buffer-or-region)
(global-set-key (kbd "C-M-z") 'crux-indent-defun)
(global-set-key (kbd "C-c e") 'crux-eval-and-replace)
(global-set-key (kbd "C-c w") 'crux-swap-windows)
(global-set-key (kbd "C-c D") 'crux-delete-file-and-buffer)
(global-set-key (kbd "C-c r") 'crux-rename-buffer-and-file)
(global-set-key (kbd "C-c t") 'crux-visit-term-buffer)
(global-set-key (kbd "C-c k") 'crux-kill-other-buffers)
(global-set-key (kbd "C-c TAB") 'crux-indent-rigidly-and-copy-to-clipboard)
(global-set-key (kbd "C-c I") 'crux-find-user-custom-file)
(global-set-key (kbd "C-c S") 'crux-find-shell-init-file)
(global-set-key (kbd "C-^") 'crux-top-join-line)
(global-set-key (kbd "C-c s") 'crux-ispell-word-then-abbrev)
(global-set-key (kbd "C-k") 'crux-smart-kill-line)
;; (global-set-key (kbd "C-<backspace>") 'crux-kill-line-backwards)
(global-set-key (kbd "C-x 4 t") 'crux-transpose-windows)
(global-set-key (kbd "C-x C-u") 'crux-upcase-region)
(global-set-key (kbd "C-x C-l") 'crux-downcase-region)
(global-set-key (kbd "C-x M-c") 'crux-capitalize-region)
(define-key global-map [remap move-beginning-of-line] 'crux-move-beginning-of-line)
(define-key global-map [remap kill-whole-line] 'crux-kill-whole-line)

;;; Prefix
;; C-c c
(global-set-key (kbd "C-c c f") 'format-all-buffer)
(global-set-key (kbd "C-c c h") 'eldoc)
(with-eval-after-load "eglot"
    (define-key eglot-mode-map (kbd "C-c c r") 'eglot-rename)
    (define-key eglot-mode-map (kbd "C-c c a") 'eglot-code-actions)
    (define-key eglot-mode-map (kbd "C-c c o") 'eglot-code-action-organize-imports))

;; C-c n
(global-set-key (kbd "C-c n d") 'deft-find-file)
(global-set-key (kbd "C-c n D") 'deft)

;; C-c o
(global-set-key (kbd "C-c o c") 'calc)
(global-set-key (kbd "C-c o C") 'quick-calc)
(global-set-key (kbd "C-c o f") 'elfeed)
(global-set-key (kbd "C-c o p") 'treemacs)
(global-set-key (kbd "C-c o t") 'vterm)
(global-set-key (kbd "C-c o e") 'eshell)

;;; Keymaps
;; ctl-x-4-map
(define-key ctl-x-4-map (kbd "t") '+my/toggle-window-split)

;; org-mode-map
(with-eval-after-load "org"
    (define-key org-mode-map [remap fill-paragraph] '+my/org-fill-or-unfill))

;; dired-mode-map
(define-key dired-mode-map (kbd "C-c o") 'crux-open-with)
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory

;; isearch-mode-map
;; Prevents issue where you have to press backspace twice when trying
;; to remove the first character that fails a search
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)
(define-key isearch-mode-map (kbd "C-n") 'isearch-repeat-forward) ; better navigation
(define-key isearch-mode-map (kbd "C-p") 'isearch-repeat-backward) ; better navigation
(define-key isearch-mode-map (kbd "M-e") 'consult-isearch-history) ; orig. isearch-edit-string
(define-key isearch-mode-map (kbd "M-s e") 'consult-isearch-history) ; orig. isearch-edit-string
(define-key isearch-mode-map (kbd "M-s l") 'consult-line) ; needed by consult-line to detect isearch
(define-key isearch-mode-map (kbd "M-s L") 'consult-line-multi) ; needed by consult-line to detect isearch

;; vterm-mode-map
(with-eval-after-load "vterm"
    (define-key vterm-mode-map (kbd "M-[") 'vterm-copy-mode)
    (define-key vterm-mode-map (kbd "C-y") 'vterm-yank))

;; neotree-mode-map
(with-eval-after-load "neotree"
    (define-key neotree-mode-map (kbd "TAB") 'neotree-stretch-toggle))

;; minibuffer-local-map
(define-key minibuffer-local-map (kbd "M-s") 'consult-history) ; orig. next-matching-history-element
(define-key minibuffer-local-map (kbd "M-r") 'consult-history) ; orig. previous-matching-history-element

;; prog-mode
(with-eval-after-load "prog-mode"
    (define-key prog-mode-map (kbd "C-c e n") #'flymake-goto-next-error)
    (define-key prog-mode-map (kbd "C-c e p") #'flymake-goto-prev-error))

(provide 'my-bindings)
;;; my-bindings.el ends here

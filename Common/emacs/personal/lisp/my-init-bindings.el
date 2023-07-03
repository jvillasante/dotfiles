;;; my-init-bindings.el --- -*- lexical-binding: t; -*-

;;; Links:
;;; https://www.masteringemacs.org/article/mastering-key-bindings-emacs

;; HACK
;; (defun c-col ()
;;     (interactive)
;;     (insert ":"))
;; (define-key c-mode-base-map ":" 'c-col)

;; General
(global-set-key (kbd "C-x C-m") 'execute-extended-command) ; ALT may or may not be available
(global-set-key (kbd "C-c C-m") 'execute-extended-command) ; ALT may or may not be available
(global-set-key (kbd "C-c u") 'browse-url-at-point) ; simple browse url
(global-set-key (kbd "C-x k") 'kill-this-buffer) ; kill buffer without prompt
(global-set-key (kbd "C-x K") 'kill-buffer) ; prompt for buffer to kill
(global-set-key (kbd "C-x S") 'my/save-all) ; save some buffers without prompt
(global-set-key (kbd "C-z") nil) ; suspend frame should go away
(global-set-key (kbd "C-x C-z") nil) ; same

;; really kill emacs even on `emacsclient'
(global-set-key (kbd "C-x C-S-c") 'save-buffers-kill-emacs)

;; Cone the current buffer in a new window with `q' to exit
(global-set-key (kbd "C-x 9") 'my/clone-buffer-in-new-window-readonly) ; same

;; ctl-x-4-map
(define-key ctl-x-4-map (kbd "t") 'my/toggle-window-split)

;; upcase, downcase and capitalize
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

;; better comment/un-comment
(global-set-key (kbd "M-;") 'my/comment-or-uncomment)
(global-set-key (kbd "C-x C-;") 'my/comment-or-uncomment)

;; hippie-expand is a better dabbrev
(define-key global-map [remap dabbrev-expand] 'hippie-expand)

;; fill-unfill
(define-key global-map [remap fill-paragraph] 'my/fill-or-unfill)
(global-set-key (kbd "M-Q") 'my/unfill-paragraph)
(global-set-key (kbd "C-M-Q") 'my/unfill-region)

;; avy
(global-set-key (kbd "M-j") 'avy-goto-char-timer)

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

;; helpful
;; (define-key helpful-mode-map [remap revert-buffer] #'helpful-update)
(define-key global-map [remap describe-command] 'helpful-command)
(define-key global-map [remap describe-function] 'helpful-callable)
(define-key global-map [remap describe-key] 'helpful-key)
(define-key global-map [remap describe-symbol] 'helpful-symbol)
(define-key global-map [remap describe-variable] 'helpful-variable)
(global-set-key (kbd "C-h F") 'helpful-function)
(global-set-key (kbd "C-h K") 'describe-keymap) ; alternative for `describe-bindings'

;; crux
;; (global-set-key (kbd "C-c o") 'crux-open-with)
;; (global-set-key (kbd "C-c u") 'crux-view-url)
(global-set-key (kbd "C-o") 'crux-smart-open-line)
(global-set-key (kbd "M-o") 'crux-smart-open-line-above)
;; (global-set-key (kbd "C-x C-r") 'crux-recentf-find-file)
;; (global-set-key (kbd "C-c f") 'crux-recentf-find-file)
;; (global-set-key (kbd "C-c F") 'crux-recentf-find-directory)
;; (global-set-key (kbd "C-c n") 'crux-cleanup-buffer-or-region)
(global-set-key (kbd "C-M-z") 'crux-indent-defun)
(global-set-key (kbd "C-c e") 'crux-eval-and-replace)
(global-set-key (kbd "C-c w") 'crux-swap-windows)
(global-set-key (kbd "C-c D") 'crux-delete-file-and-buffer)
(global-set-key (kbd "C-c r") 'crux-rename-buffer-and-file)
;; (global-set-key (kbd "C-c t") 'crux-visit-term-buffer)
(global-set-key (kbd "C-c k") 'crux-kill-other-buffers)
;; (global-set-key (kbd "C-c TAB") 'crux-indent-rigidly-and-copy-to-clipboard)
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

;; ibuffer
(define-key global-map [remap list-buffers] 'ibuffer)
(with-eval-after-load 'ibuffer
    (define-key ibuffer-mode-map (kbd "q") 'kill-this-buffer))

;; org
(with-eval-after-load 'org
    (define-key org-mode-map [remap fill-paragraph] 'my/org-fill-or-unfill))

;; dired
(with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "C-u C-o") 'crux-open-with))

;; isearch
(with-eval-after-load 'isearch
    ;; Prevents issue where you have to press backspace twice when trying to remove the first character that fails a search
    (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)
    (define-key isearch-mode-map (kbd "C-n") 'isearch-repeat-forward) ; better navigation
    (define-key isearch-mode-map (kbd "C-p") 'isearch-repeat-backward)) ; better navigation

;; vterm
(with-eval-after-load 'vterm
    (define-key vterm-mode-map (kbd "C-x [") 'vterm-copy-mode)
    (define-key vterm-mode-map (kbd "M-[") 'vterm-copy-mode)
    (define-key vterm-mode-map (kbd "C-y") 'vterm-yank)
    (define-key vterm-mode-map (kbd "C-c <escape>") 'vterm-send-escape))

;; neotree
(with-eval-after-load 'neotree
    (define-key neotree-mode-map (kbd "TAB") 'neotree-stretch-toggle))

;; prog-mode
(with-eval-after-load 'prog-mode
    (with-eval-after-load 'flymake
        (define-key prog-mode-map (kbd "M-n") 'flymake-goto-next-error)
        (define-key prog-mode-map (kbd "M-p") 'flymake-goto-prev-error))
    (with-eval-after-load 'flycheck
        (define-key prog-mode-map (kbd "M-n") 'flycheck-next-error)
        (define-key prog-mode-map (kbd "M-p") 'flycheck-previous-error)))

;; dwim-shell-command
(with-eval-after-load 'dwim-shell-command
    (define-key global-map [remap shell-command] 'dwim-shell-command)
    (define-key global-map [remap async-shell-command] 'dwim-shell-command)
    (define-key dired-mode-map [remap dired-do-shell-command] 'dwim-shell-command)
    (define-key dired-mode-map [remap dired-do-async-shell-command] 'dwim-shell-command)
    (define-key dired-mode-map [remap dired-smart-shell-command] 'dwim-shell-command))

;;; Prefix
;; C-c f : find
(global-set-key (kbd "C-c f f") 'project-find-file)
(global-set-key (kbd "C-c f F") 'find-file)
(global-set-key (kbd "C-c f D") 'project-dired)
(global-set-key (kbd "C-c f o") 'consult-recent-file)
(global-set-key (kbd "C-c f r") 'consult-yank-from-kill-ring)
(global-set-key (kbd "C-c f b") 'consult-buffer)
(global-set-key (kbd "C-c f p") 'project-switch-project)
(global-set-key (kbd "C-c f g") 'consult-ripgrep)
(global-set-key (kbd "C-c f a") 'embark-act)
(global-set-key (kbd "C-c f j") 'evil-collection-consult-jump-list)
(global-set-key (kbd "C-c f m") 'evil-collection-consult-mark)
(global-set-key (kbd "C-c f i") 'consult-imenu)
(global-set-key (kbd "C-c f I") 'consult-imenu-multi)
(global-set-key (kbd "C-c f l") 'consult-line)
(global-set-key (kbd "C-c f L") 'consult-line-multi)

;; C-c c : Code
(global-set-key (kbd "C-c c f") 'my/toggle-fold)
(global-set-key (kbd "C-c c h") 'eldoc)
(with-eval-after-load 'eglot
    (define-key eglot-mode-map (kbd "C-c c R") 'eglot-reconnect)
    (define-key eglot-mode-map (kbd "C-c c r") 'eglot-rename)
    (define-key eglot-mode-map (kbd "C-c c a") 'eglot-code-actions)
    (define-key eglot-mode-map (kbd "C-c c o") 'eglot-code-action-organize-imports))
(with-eval-after-load 'flymake
    (define-key flymake-mode-map (kbd "C-c c e") 'flymake-show-buffer-diagnostics)
    (define-key flymake-mode-map (kbd "C-c c E") 'flymake-show-project-diagnostics))

;; C-c n : Notes
(global-set-key (kbd "C-c n d") 'deft-find-file)
(global-set-key (kbd "C-c n D") 'deft)
(global-set-key (kbd "C-c n a") 'org-agenda)

;; C-c o : Open
(global-set-key (kbd "C-c o f") 'elfeed)
(global-set-key (kbd "C-c o p") 'dired-sidebar-toggle-sidebar)
(global-set-key (kbd "C-c o t") 'vterm)
(global-set-key (kbd "C-c o e") 'eshell)

;; C-c l : lookup
(global-set-key (kbd "C-c l d") 'dictionary-lookup-definition)

(provide 'my-init-bindings)
;;; my-init-bindings.el ends here

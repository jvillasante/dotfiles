;;; my-init-bindings.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;;; Links:
;; https://www.masteringemacs.org/article/mastering-key-bindings-emacs

;; General
(global-set-key (kbd "C-x C-m") 'execute-extended-command) ; ALT may or may not be available
(global-set-key (kbd "C-c C-m") 'execute-extended-command) ; ALT may or may not be available
(global-set-key (kbd "C-c u") 'browse-url-at-point) ; simple browse url
(global-set-key (kbd "C-x k") 'kill-this-buffer) ; kill buffer without prompt
(global-set-key (kbd "C-x K") 'kill-buffer) ; prompt for buffer to kill
(global-set-key (kbd "C-x S") 'my--save-all) ; save some buffers without prompt

;; C-z does not make sense on graphical environment
(when (display-graphic-p)
    (global-unset-key (kbd "C-z"))
    (global-unset-key (kbd "C-x C-z")))

;; really kill emacs even on `emacsclient'
(global-set-key (kbd "C-x C-S-c") 'save-buffers-kill-emacs)

;; Cone the current buffer in a new window with `q' to exit
(global-set-key (kbd "C-x 9") 'my--clone-buffer-in-new-window-readonly) ; same

;; ctl-x-4-map
(define-key ctl-x-4-map (kbd "t") 'my--toggle-window-split)

;; duplicate current line or region
(global-set-key (kbd "C-x j") #'duplicate-dwim)

;; upcase, downcase and capitalize
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

;; Open line(s) below/above current one
(global-set-key (kbd "C-o") 'my--open-next-line)
(global-set-key (kbd "M-o") 'my--open-previous-line)

;; better comment/un-comment
(global-set-key (kbd "M-;") 'my--comment-or-uncomment)
(global-set-key (kbd "C-x C-;") 'my--comment-or-uncomment)

;; fill-unfill
(define-key global-map [remap fill-paragraph] 'my--fill-or-unfill)
(global-set-key (kbd "M-Q") 'my--unfill-paragraph)
(global-set-key (kbd "C-M-Q") 'my--unfill-region)

;; isearch
(with-eval-after-load 'isearch
    ;; Prevents issue where you have to press backspace twice when trying to remove the first character that fails a search
    (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)
    (define-key isearch-mode-map (kbd "C-o") 'isearch-occur) ; occur
    (define-key isearch-mode-map (kbd "C-n") 'isearch-repeat-forward) ; better navigation
    (define-key isearch-mode-map (kbd "C-p") 'isearch-repeat-backward)) ; better navigation

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
(global-set-key (kbd "C-c c f") 'my--toggle-fold)
(global-set-key (kbd "C-c c h") 'eldoc)
(with-eval-after-load 'eglot
    (define-key eglot-mode-map (kbd "C-c c R") 'eglot-reconnect)
    (define-key eglot-mode-map (kbd "C-c c r") 'eglot-rename)
    (define-key eglot-mode-map (kbd "C-c c d") 'eglot-find-declaration)
    (define-key eglot-mode-map (kbd "C-c c .") 'eglot-find-typeDefinition)
    (define-key eglot-mode-map (kbd "C-c c i") 'eglot-find-implementation)
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
(global-set-key (kbd "C-c o c") 'calc)

;; C-c l : lookup
(global-set-key (kbd "C-c l d") 'dictionary-lookup-definition)

(provide 'my-init-bindings)
;;; my-init-bindings.el ends here

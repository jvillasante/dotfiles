;;; personal/100_bindings.el --- bindings customizations -*- lexical-binding: t; -*-
;;;

;; get rid of `find-file-read-only' and replace it with something more useful
(global-set-key (kbd "C-x C-r") 'crux-recentf-find-file)

;; remap "C-c f" to `crux-cleanup-buffer-or-region'
(define-key prelude-mode-map (kbd "C-c f") nil)
(global-set-key (kbd "C-c f") 'crux-cleanup-buffer-or-region)

;; remap "C-c n" to `neotree-toggle'
(define-key prelude-mode-map (kbd "C-c n") nil)
(global-set-key (kbd "C-c n") 'neotree-toggle)

;; "C-x K" `kill-this-buffer'
(global-set-key (kbd "C-x K") 'kill-this-buffer)

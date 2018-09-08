;;; ~/.doom.d/+magit.el -*- lexical-binding: t; -*-

;; (setq +magit-hub-features t ;; I want the PR/issue stuff too!
;;   +magit-hub-enable-by-default t)  ;; And I want it on by default!

;; (after! magit
;;   (magit-wip-after-save-mode t)
;;   (magit-wip-after-apply-mode t)
;;   (setq magit-save-repository-buffers 'dontask
;;     magit-repository-directories '(("/home/aria/projects/" . 3)
;;                                     ("/home/aria/dotfiles/" . 0))
;;     magit-popup-display-buffer-action nil ;; Not sure why this is here, wonder what it does
;;     magit-display-file-buffer-function #'switch-to-buffer-other-window
;;     magithub-clone-default-directory "~/projects")) ;; I want my stuff to clone to ~/projects

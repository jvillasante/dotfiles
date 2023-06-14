;;; my-init-filemanager.el -*- lexical-binding: t; -*-

;; Dired : built-in navigation of folders
(use-package dired
    :ensure nil ;; emacs built-in
    :init
    (setq dired-ls-F-marks-symlinks t) ;; mark symlinks
    (setq dired-recursive-copies 'always) ;; Never prompt for recursive copies of a directory
    (setq dired-recursive-deletes 'always) ;; Never prompt for recursive deletes of a directory
    (setq dired-dwim-target t) ;; makes dired guess the target directory
    (setq dired-auto-revert-buffer t) ;; auto-revert dired buffers if file changed on disk
    (setq dired-hide-details-hide-symlink-targets nil
        ;; Ask whether destination dirs should get created when copying/removing files.
        dired-create-destination-dirs 'ask
        ;; Disable the prompt about whether I want to kill the Dired buffer for a
        ;; deleted directory. Of course I do!
        dired-clean-confirm-killing-deleted-buffers nil)

    ;; Dired listing switches
    ;;  -a : Do not ignore entries starting with .
    ;;  -l : Use long listing format.
    ;;  -h : Human-readable sizes like 1K, 234M, ..
    ;;  -v : Do natural sort .. so the file names starting with . will show up first.
    ;;  -F : Classify filenames by appending '*' to executables, '/' to directories, etc.
    (setq dired-listing-switches (if (eq system-type 'windows-nt)
                                     "-alh"
                                     "-alhvF --group-directories-first"))

    ;; global auto-revert only applies to buffers associated with files on the disk
    (add-hook 'dired-mode-hook 'auto-revert-mode)

    ;; enable some really cool extensions like C-x C-j(dired-jump)
    (if (< emacs-major-version 28)
        (add-hook 'dired-load-hook (lambda ()
                                       (load "dired-x"))))

    ;; Make dired use the same buffer for viewing directory
    (if (< emacs-major-version 28)
        (progn
            (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
            (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))) ; was dired-up-directory
        (progn
            (setq dired-kill-when-opening-new-dired-buffer t))))

(use-package dired-x
    :ensure nil ;; emacs built-in
    :bind (:map dired-mode-map ("." . dired-omit-mode))
    :config
    ;; Make dired-omit-mode hide all "dotfiles"
    (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..*$")))

;; Addtional syntax highlighting for dired
(use-package diredfl
    :hook
    ((dired-mode . diredfl-mode)
        (dirvish-directory-view-mode . diredfl-mode))
    :config
    (set-face-attribute 'diredfl-dir-name nil :bold t))

(use-package all-the-icons-dired
    :init (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package dired-sidebar
    :init
    (add-hook 'dired-sidebar-mode-hook
        (lambda ()
            (my/font-set-small-mono-font)
            (display-line-numbers-mode 0)
            (unless (file-remote-p default-directory)
                (auto-revert-mode))))
    :config
    (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
    (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
    (setq dired-sidebar-use-term-integration t
        dired-sidebar-resize-on-open nil
        dired-sidebar-window-fixed nil
        dired-sidebar-theme 'all-the-icons
        dired-sidebar-use-custom-modeline nil))

(provide 'my-init-filemanager)
;;; my-init-filemanager.el ends here

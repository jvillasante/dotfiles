;;; my-init-filemanager.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; Dired : built-in navigation of folders
(use-package dired
    :ensure nil ;; emacs built-in
    :hook ((dired-mode . auto-revert-mode)
           (dired-mode . dired-hide-details-mode)
           (dired-mode . hl-line-mode))
    :bind ((:map dired-mode-map
                 ("C-<return>" . crux-open-with)))
    :init
    ;; (dired-async-mode 1)
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

    ;; Allow to change permissions with wdired
    (setq wdired-allow-to-change-permissions t)

    ;; Teach Dired to use a specific external program with either the
    ;; `dired-do-shell-command' or `dired-do-async-shell-command' command
    ;; (with the default keys, those are bound to `!' `&', respectively).
    ;; The first string is a pattern match against file names.  The
    ;; remaining strings are external programs that Dired will provide as
    ;; suggestions.  Of course, you can always type an arbitrary program
    ;; despite these defaults.
    (setq dired-guess-shell-alist-user
          (append '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open")
                    ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open")
                    ("\\.pdf\\'" "okular")
                    (".*" "xdg-open"))
                  dired-guess-shell-alist-user))

    ;; Dired listing switches
    ;;  -a : Do not ignore entries starting with .
    ;;  -l : Use long listing format.
    ;;  -h : Human-readable sizes like 1K, 234M, ..
    ;;  -v : Do natural sort .. so the file names starting with . will show up first.
    ;;  -F : Classify filenames by appending '*' to executables, '/' to directories, etc.
    (setq dired-listing-switches (if (eq system-type 'windows-nt)
                                         "-alh"
                                     "-alhvF --group-directories-first"))

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
    ;; hide all "dotfiles"
    ;; (setq dired-omit-files (concat dired-omit-files "\\|^\\..*$"))

    ;; vcs folders
    (setq dired-omit-files (concat dired-omit-files "^\\.\\(?:git\\|hg\\|svn\\)$"))

    ;; compiled files
    (setq dired-omit-files (concat dired-omit-files "\\.\\(?:pyc\\|o\\|elc\\|lock\\|css.map\\|class\\)$"))

    ;; generated files, caches or local pkgs
    (setq dired-omit-files (concat dired-omit-files "^\\(?:node_modules\\|vendor\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"))

    ;; org-mode folders
    (setq dired-omit-files (concat dired-omit-files "^\\.\\(?:sync\\|export\\|attach\\)$"))

    ;; temp files
    (setq dired-omit-files (concat dired-omit-files "~$"))
    (setq dired-omit-files (concat dired-omit-files "^#.*#$"))

    ;; others
    (setq dired-omit-files (concat dired-omit-files "^\\.\\(cache\\|tox\\|coverage\\)$"))
    (setq dired-omit-files (concat dired-omit-files "^\\.\\(DS_Store\\|python\\-version\\)"))
    (setq dired-omit-files (concat dired-omit-files "^\\(htmlcov\\)$"))
    (setq dired-omit-files (concat dired-omit-files "\\.elcs$"))
    (setq dired-omit-files (concat dired-omit-files "^\\.coverage\\..*"))
    (setq dired-omit-files (concat dired-omit-files "\\.ipynb.*$"))
    (setq dired-omit-files (concat dired-omit-files "\\.py[cod]$"))
    (setq dired-omit-files (concat dired-omit-files "^\\.#.*$"))
    (setq dired-omit-files (concat dired-omit-files "^__pycache__$"))
    (setq dired-omit-files (concat dired-omit-files "\\.gcda$"))
    (setq dired-omit-files (concat dired-omit-files "\\.gcov$"))
    (setq dired-omit-files (concat dired-omit-files "\\.gcno$"))
    (setq dired-omit-files (concat dired-omit-files "\\.lo$"))
    (setq dired-omit-files (concat dired-omit-files "\\.o$"))
    (setq dired-omit-files (concat dired-omit-files "\\.so$"))
    (setq dired-omit-files (concat dired-omit-files "^\\.cproject$"))
    (setq dired-omit-files (concat dired-omit-files "^\\.project$"))
    (setq dired-omit-files (concat dired-omit-files "^\\.projectile$"))
    (setq dired-omit-files (concat dired-omit-files "^\\.log$"))
    (setq dired-omit-files (concat dired-omit-files "\\.egg\-info$")))

;; Addtional syntax highlighting for dired
(use-package diredfl
    :hook
    ((dired-mode . diredfl-mode)
     (dirvish-directory-view-mode . diredfl-mode))
    :config
    (set-face-attribute 'diredfl-dir-name nil :bold t))

(use-package dired-sidebar
    :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
    :init (add-hook 'dired-sidebar-mode-hook
                    (lambda ()
                        (unless (file-remote-p default-directory)
                            (auto-revert-mode))))
    :config
    (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
    (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
    (setq dired-sidebar-theme 'ascii)
    (setq dired-sidebar-use-term-integration t)
    (setq dired-sidebar-resize-on-open nil)
    (setq dired-sidebar-window-fixed nil)
    (setq dired-sidebar-use-custom-modeline nil))

(provide 'my-init-filemanager)
;;; my-init-filemanager.el ends here

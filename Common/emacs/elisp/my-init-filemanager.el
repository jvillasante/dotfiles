;;; my-init-filemanager.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;;; Dired

;; dired : built-in navigation of folders
(use-package dired
    :ensure nil ;; emacs built-in
    :hook((dired-mode . (lambda ()
                            (auto-revert-mode)
                            (dired-hide-details-mode)
                            (hl-line-mode))))
    :config
    (setq dired-free-space nil
          dired-deletion-confirmer 'y-or-n-p
          dired-filter-verbose nil
          dired-recursive-deletes 'top
          dired-recursive-copies  'always
          dired-create-destination-dirs 'ask)
    (setq dired-ls-F-marks-symlinks t) ;; mark symlinks
    (setq dired-dwim-target t) ;; makes dired guess the target directory
    (setq dired-auto-revert-buffer t) ;; auto-revert dired buffers if file changed on disk
    (setq dired-hide-details-hide-symlink-targets nil
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
    (setq dired-listing-switches
          "-a -l -h -v --group-directories-first --color=never")

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

;; dired-x : dired eXtenxions
(use-package dired-x
    :ensure nil ;; emacs built-in
    :config
    (setq dired-omit-extensions
          (append dired-latex-unclean-extensions
                  dired-bibtex-unclean-extensions
                  dired-texinfo-unclean-extensions))
    (setq dired-omit-files
          (concat
           "\\(" "^\\.?#\\|^\\.$\\|^\\.\\.$"               "\\)" "\\|"
           "\\(" "^\\.\\(git\\|cache\\|tox\\|coverage\\)$" "\\)" "\\|"
           "\\(" "^\\.\\(DS_Store\\|python\\-version\\)"   "\\)" "\\|"
           "\\(" "^\\(htmlcov\\|node_modules\\)$"          "\\)" "\\|"
           "\\(" "^\\.\\(vscode\\|devcontainer\\)$"        "\\)" "\\|"
           "\\(" "\\.elcs$"                                "\\)" "\\|"
           "\\(" "^\\.coverage\\..*"                       "\\)" "\\|"
           "\\(" "\\.ipynb.*$"                             "\\)" "\\|"
           "\\(" "\\.py[cod]$"                             "\\)" "\\|"
           "\\(" "~$"                                      "\\)" "\\|"
           "\\(" "^#.*#$"                                  "\\)" "\\|"
           "\\(" "^\\.#.*$"                                "\\)" "\\|"
           "\\(" "^__pycache__$"                           "\\)" "\\|"
           "\\(" "\\.gcda$"                                "\\)" "\\|"
           "\\(" "\\.gcov$"                                "\\)" "\\|"
           "\\(" "\\.gcno$"                                "\\)" "\\|"
           "\\(" "\\.lo$"                                  "\\)" "\\|"
           "\\(" "\\.o$"                                   "\\)" "\\|"
           "\\(" "\\.so$"                                  "\\)" "\\|"
           "\\(" "^\\.cproject$"                           "\\)" "\\|"
           "\\(" "^\\.project$"                            "\\)" "\\|"
           "\\(" "^\\.projectile$"                         "\\)" "\\|"
           "\\(" "\\.egg\-info$"                           "\\)")))

;; Addtional syntax highlighting for dired
(use-package diredfl
    :hook
    ((dired-mode . diredfl-mode))
    :config
    (set-face-attribute 'diredfl-dir-name nil :bold t))

;; dired-sidebar : dired in the sidebar
(use-package dired-sidebar
    :init
    (add-hook 'dired-sidebar-mode-hook
              (lambda ()
                  (unless (file-remote-p default-directory)
                      (auto-revert-mode))))
    :config
    (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
    (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
    (setq dired-sidebar-theme 'ascii)
    (setq dired-sidebar-width 48)
    (setq dired-sidebar-window-fixed nil)
    (setq dired-sidebar-use-term-integration t))

(use-package neotree
    :disabled t
    :preface
    (defun my/neotree-project-dir ()
        "Open NeoTree using project root."
        (interactive)
        (let ((project-dir (or (when-let ((project (project-current))) (project-root project))
                               default-directory))
              (file-name (buffer-file-name)))
            (neotree-toggle)
            (if project-dir
                    (if (neo-global--window-exists-p)
                            (progn
                                (neotree-dir project-dir)
                                (neotree-find file-name)))
                (message "Could not find project root."))))
    :hook ((neotree-mode . hl-line-mode))
    :config
    (setq neo-theme 'ascii)
    (setq neo-window-width 48)
    (setq neo-smart-open t)
    (setq neo-create-file-auto-open nil)
    (setq neo-show-updir-line t)
    (setq neo-show-hidden-files t)
    (setq neo-auto-indent-point nil)
    (setq neo-vc-integration nil)
    (setq neo-autorefresh nil)
    (setq neo-mode-line-type 'neotree)
    (setq neo-banner-message nil)
    (setq neo-confirm-create-file #'off-p)
    (setq neo-confirm-create-directory #'off-p)
    (setq neo-keymap-style 'concise)
    (setq neo-hidden-regexp-list
          '(;; vcs folders
            "^\\.\\(?:git\\|hg\\|svn\\)$"
            ;; compiled files
            "\\.\\(?:pyc\\|o\\|elc\\|lock\\|css.map\\|class\\)$"
            ;; generated files, caches or local pkgs
            "^\\(?:node_modules\\|vendor\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"
            ;; org-mode folders
            "^\\.\\(?:sync\\|export\\|attach\\)$"
            ;; temp files
            "~$"
            "^#.*#$"
            ;; Others
            "^\\.\\(cache\\|tox\\|coverage\\)$"
            "^\\.\\(DS_Store\\|python\\-version\\)"
            "^\\(htmlcov\\)$" "\\.elcs$"
            "^\\.coverage\\..*" "\\.ipynb.*$" "\\.py[cod]$"
            "^\\.#.*$" "^__pycache__$"
            "\\.gcda$" "\\.gcov$" "\\.gcno$" "\\.lo$" "\\.o$" "\\.so$"
            "^\\.cproject$" "^\\.project$" "^\\.projectile$"
            "^\\.log$"
            "\\.egg\-info$")))

;; trashed : Viewing/editing system trash can in Emacs
(use-package trashed
    :commands (trashed)
    :config
    (setq trashed-action-confirmer 'y-or-n-p)
    (setq trashed-use-header-line t)
    (setq trashed-sort-key '("Date deleted" . t))
    (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(provide 'my-init-filemanager)
;;; my-init-filemanager.el ends here

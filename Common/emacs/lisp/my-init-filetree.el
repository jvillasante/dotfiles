;;; my-init-filetree.el --- File Explorer -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; Tell the compiler: Don't worry, this function will exist later
(declare-function project-root "project")

;; dired-sidebar : dired in the sidebar
(use-package dired-sidebar
    :after dired
    :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
    :custom
    (dired-sidebar-theme 'ascii)
    (dired-sidebar-width 48)
    (dired-sidebar-window-fixed nil)
    (dired-sidebar-use-term-integration t)
    :config
    (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
    (push 'rotate-windows dired-sidebar-toggle-hidden-commands))

;; speedbar : quick access to files and tags in a frame
;; NOTE: Heavily vibe-coded!
(use-package speedbar
    :disabled t
    :ensure nil ; emacs built-in
    :preface
    (defun my/speedbar-toggle-dotfiles ()
        "Toggle showing dotfiles and unknown-type files in speedbar."
        (interactive)
        (setq speedbar-show-unknown-files (not speedbar-show-unknown-files))
        (setq speedbar-directory-unshown-regexp
            (if speedbar-show-unknown-files
                "^$"                    ; reveal dot-directories (never matches a name)
                "^\\(\\..*\\)\\'"))      ; default: hide dot-directories
        (speedbar-refresh))
    (defun my/speedbar-allow-other-window (&rest _)
        "Strip `no-other-window' so `C-x o' can move into the speedbar."
        (when (window-live-p speedbar--window)
            (set-window-parameter speedbar--window 'no-other-window nil)))
    (defvar my/speedbar-display-action
        '((display-buffer-reuse-window display-buffer-use-some-window)
             (inhibit-same-window . t))
        "Display action `my/speedbar-open-window' uses to open files.
Defaults to reusing an existing window so `RET' does not split.")
    (defun my/speedbar-open-window (fn &rest args)
        "Open speedbar files via `my/speedbar-display-action' instead of splitting.
`speedbar-find-file-in-frame' visits files with `switch-to-buffer' from the
dedicated sidebar, which otherwise pops up (splits) a new window."
        (let ((display-buffer-overriding-action my/speedbar-display-action))
            (apply fn args)))
    (defun my/speedbar-open-in-other-window ()
        "Visit the file on the current line in a separate window and select it."
        (interactive)
        (let ((my/speedbar-display-action
                  '((display-buffer-pop-up-window) (inhibit-same-window . t))))
            (speedbar-edit-line)))
    (defun my/speedbar-close ()
        "Close the speedbar window, tearing down its buffer and timer."
        (interactive)
        (speedbar -1))
    (defvar my/speedbar-following nil
        "Reentrancy guard for `my/speedbar-follow'.")
    (defun my/speedbar-follow (&rest _)
        "Show the selected window's file in the speedbar and put point on it.
Speedbar's built-in file-following assumes a separate frame and is
unreliable in window mode, so track the current file explicitly on window
changes: re-root the tree to its directory, highlight it, and set the
speedbar window's point (a non-selected window keeps its own point, so
this is what makes the cursor actually follow)."
        (let ((buf (window-buffer (selected-window))))
            (when (and (not my/speedbar-following)
                      (not (minibufferp buf))
                      (bound-and-true-p speedbar--window)
                      (window-live-p speedbar--window)
                      (bound-and-true-p speedbar-buffer)
                      (not (eq buf speedbar-buffer))
                      (buffer-local-value 'buffer-file-name buf))
                (let ((my/speedbar-following t)
                         (file (buffer-local-value 'buffer-file-name buf))
                         (dir (expand-file-name
                                  (buffer-local-value 'default-directory buf))))
                    (with-current-buffer speedbar-buffer
                        (let ((default-directory dir))
                            ;; Re-root / rehighlight only when the file changed.
                            (unless (equal file speedbar-last-selected-file)
                                (unless (member dir speedbar-shown-directories)
                                    (speedbar-update-directory-contents))
                                (speedbar-clear-current-file)
                                (setq speedbar-last-selected-file file))
                            ;; Always park point (and the highlight) on the file.
                            (save-excursion
                                (when (speedbar-find-selected-file file)
                                    (speedbar-with-writable
                                        (put-text-property (match-beginning 1)
                                            (match-end 1)
                                            'face
                                            'speedbar-selected-face))
                                    (beginning-of-line)
                                    (set-window-point speedbar--window (point))))))))))
    (defun my/speedbar-toggle ()
        "Toggle the speedbar window, opening with point on the current file."
        (interactive)
        (if (and (bound-and-true-p speedbar--window)
                (window-live-p speedbar--window))
            (my/speedbar-close)
            (speedbar 1)                        ; open; focus stays in editing window
            (my/speedbar-follow)                ; select current file while it is known
            (when (window-live-p speedbar--window)
                (select-window speedbar--window))))
    :bind
    (("C-x C-n" . my/speedbar-toggle)
        :map speedbar-mode-map
        ("TAB" . speedbar-toggle-line-expansion)
        ("."   . my/speedbar-toggle-dotfiles)
        ("o"   . my/speedbar-open-in-other-window)
        ("^"   . speedbar-up-directory)
        ("q"   . my/speedbar-close))
    :custom
    (speedbar-prefer-window t)
    (speedbar-use-images nil)
    (speedbar-update-flag nil)
    (speedbar-hide-button-brackets-flag t)
    (speedbar-show-unknown-files t)   ; list all files, not just known extensions
    (speedbar-vc-do-check nil)        ; don't stat every file for VC state (slow over TRAMP)
    (speedbar-window-default-width 48)
    (speedbar-window-max-width 48)
    :config
    (advice-add 'speedbar-window-mode :after #'my/speedbar-allow-other-window)
    (advice-add 'speedbar-find-file-in-frame :around #'my/speedbar-open-window)
    (add-hook 'window-buffer-change-functions #'my/speedbar-follow)
    (add-hook 'window-selection-change-functions #'my/speedbar-follow))

;; neotree : A Emacs tree plugin like NerdTree for Vim
(use-package neotree
    :disabled t
    :preface
    (defun my-neotree-project-dir ()
        "Open NeoTree using project root."
        (interactive)
        (let ((project-dir (or (when-let ((project (project-current)))
                                   (project-root project))
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
    :bind (("C-x C-n" . my-neotree-project-dir)
              :map neotree-mode-map
              ("." . neotree-hidden-file-toggle))
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

(provide 'my-init-filetree)
;;; my-init-filetree.el ends here

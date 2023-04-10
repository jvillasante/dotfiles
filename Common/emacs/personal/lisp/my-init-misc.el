;;; my-init-misc.el -*- lexical-binding: t; -*-

;; encryption
;; https://orgmode.org/worg/org-tutorials/encrypting-files.html
(progn
    (setq auth-source-save-behavior nil)

    ;; setup epa
    (require 'epa-file)
    (epa-file-enable)
    (setq epa-file-encrypt-to user-mail-address
        epa-file-select-keys 'silent
        epa-file-cache-passphrase-for-symmetric-encryption nil)

    ;; setup org-crypt
    (require 'org-crypt)
    (org-crypt-use-before-save-magic)
    (setq org-crypt-disable-auto-save nil
        org-tags-exclude-from-inheritance (quote ("crypt"))
        org-crypt-key nil
        org-crypt-key user-mail-address))

(use-package electric
    :ensure nil ;; emacs built-in
    :preface
    (defun my/electric-indent-local-mode-maybe ()
        "Enable `electric-indent-local-mode' if appropriate."
        (unless (or (eq major-mode 'fundamental-mode)
                    (eq major-mode 'text-mode))
            (electric-indent-local-mode 1)))
    :init
    (setq-default electric-indent-chars '(?\n ?\^?))
    (electric-indent-mode 0) ;; disable by default
    (add-hook 'after-change-major-mode-hook #'my/electric-indent-local-mode-maybe))

(use-package isearch
    :ensure nil ;; emacs built-in
    :init
    (setq isearch-resume-in-command-history t) ; use history for isearch as well
    (setq search-whitespace-regexp ".*?") ; isearch convenience, space matches anything (non-greedy)
    (setq isearch-lax-whitespace t)
    (setq isearch-allow-motion t) ; enable Emacs28 isearch motions
    (defadvice isearch-search (after isearch-no-fail activate)
        (unless isearch-success
            (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
            (ad-activate 'isearch-search)
            (isearch-repeat (if isearch-forward 'forward))
            (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
            (ad-activate 'isearch-search))))

;; saveplace : remembers your location in a file when saving files
(use-package saveplace
    :ensure nil ;; emacs built-in
    :init
    (save-place-mode +1)
    :config
    (setq save-place-file (expand-file-name "saveplace" no-littering-var-directory)))

;; savehist : save minibuffer history
(use-package savehist
    :ensure nil ;; emacs built-in
    :init
    (savehist-mode +1)
    :config
    (setq history-length t
        history-delete-duplicates t
        savehist-save-minibuffer-history t
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring) ;; search entries
        savehist-file (expand-file-name "savehist" no-littering-var-directory)))

;; recentf : recent files
(use-package recentf
    :ensure nil ;; emacs built-in
    :config
    (setq recentf-save-file (expand-file-name "recentf" no-littering-var-directory)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)
    (add-to-list 'recentf-exclude (expand-file-name "Apps/elfeed/elfeed_db/" my/dropbox-path))
    (add-to-list 'recentf-exclude (expand-file-name ".password-store/" my/home-path))
    (add-to-list 'recentf-exclude (expand-file-name ".mail/" my/home-path))
    (add-to-list 'recentf-exclude "/dev/shm/")
    (add-to-list 'recentf-exclude "\\.git")
    (add-to-list 'recentf-exclude "/tmp/")
    (add-to-list 'recentf-exclude "/ssh:")
    (add-to-list 'recentf-exclude "/usr/")
    (add-to-list 'recentf-exclude "\\.?ido\\.last$")
    (add-to-list 'recentf-exclude "^/nix/store/")
    (add-to-list 'recentf-exclude ".+\\.mp3$")
    (recentf-mode +1))

;; project.el : default project manager
(use-package project
    :init
    (setq project-list-file (expand-file-name "projects" no-littering-var-directory))
    (add-to-list 'project-switch-commands
        '(project-dired "Dired at root")))

;; tramp : Transparent Remote Access, Multiple Protocols
(use-package tramp
    :ensure nil ;; emacs built-in
    :init
    (setq tramp-verbose 2)
    (setq tramp-default-method "ssh")    ; ssh is faster than scp and supports ports.
    (setq tramp-password-prompt-regexp   ; Add verification code support.
        (concat
            "^.*"
            (regexp-opt
                '("passphrase" "Passphrase"
                     "password" "Password"
                     "Verification code")
                t)
            ".*:\0? *")))

(use-package elec-pair
    :ensure nil ;; emacs built-in
    :init
    (add-hook 'after-init-hook 'electric-pair-mode)

    ;; more conservative on whether should also insert ) when typing
    ;; (, for example, prevent from inserting ) when point is on a
    ;; word.
    (setq electric-pair-inhibit-predicate #'electric-pair-conservative-inhibit))

(use-package autorevert
    :ensure nil ;; emacs built-in
    :init (global-auto-revert-mode +1))

(use-package helpful)

(use-package rainbow-delimiters
    :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package ibuffer-vc
    :init (add-hook 'ibuffer-hook #'my/ibuffer-vc-setup))

;; crux : A Collection of Ridiculously Useful eXtensions for Emacs
(use-package crux)

;; persistent-scratch : preserve scratch buffer across sessions
(use-package persistent-scratch
    :config
    (setq persistent-scratch-save-file (expand-file-name "persistent-scratch" no-littering-var-directory))
    (persistent-scratch-setup-default)
    (persistent-scratch-autosave-mode 1))

;; editorconfig : editorconfig for Emacs
(use-package editorconfig
    :config (editorconfig-mode 1))

;; avy : GNU Emacs package for jumping to visible text using a char-based decision tree
(use-package avy
    :init
    (setq avy-all-windows t)
    (setq avy-background t))

;; undo-tree : treat undo history as a tree
(use-package undo-tree
    :init (global-undo-tree-mode +1)
    :config
    ;; autosave the undo-tree history
    (setq undo-tree-history-directory-alist
        `((".*" . ,no-littering-var-directory)))
    (setq undo-tree-auto-save-history nil))

;; hydra : Keybindings combinations
(use-package hydra)

;; Expand Region : expand or contract selection
(use-package expand-region)

;; better C-w and M-w
(use-package whole-line-or-region
    :init (whole-line-or-region-global-mode))

(use-package multiple-cursors
    :bind (("H-SPC" . set-rectangular-region-anchor)
              ("C-M-SPC" . set-rectangular-region-anchor)
              ("C->" . mc/mark-next-like-this)
              ("C-<" . mc/mark-previous-like-this)
              ("C-c C->" . mc/mark-all-like-this)
              ("C-S-c C-S-c" . mc/edit-lines)))

(use-package engine-mode
    :ensure t
    :init
    (defengine duckduckgo
        "https://duckduckgo.com/?q=%s"
        :keybinding "d")
    (defengine brave-search
        "https://search.brave.com/search?q=%s"
        :keybinding "b")
    (defengine github
        "https://github.com/search?ref=simplesearch&q=%s")
    (defengine google
        "https://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
        :keybinding "g")
    (defengine google-images
        "https://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s")
    (defengine google-maps
        "https://maps.google.com/maps?q=%s"
        :docstring "Mappin' it up.")
    (defengine project-gutenberg
        "https://www.gutenberg.org/ebooks/search/?query=%s")
    (defengine qwant
        "https://www.qwant.com/?q=%s")
    (defengine stack-overflow
        "https://stackoverflow.com/search?q=%s")
    (defengine twitter
        "https://twitter.com/search?q=%s")
    (defengine wikipedia
        "https://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
        :keybinding "w"
        :browser 'eww-browse-url
        :docstring "Searchin' the wikis.")
    (defengine wiktionary
        "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s")
    (defengine wolfram-alpha
        "https://www.wolframalpha.com/input/?i=%s")
    (defengine youtube
        "https://www.youtube.com/results?aq=f&oq=&search_query=%s")
    (defengine cppreference
        "https://en.cppreference.com/w/?search=%s"
        :keybinding "c")
    :config
    (engine-mode t))

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

(use-package diredfl
    :init (add-hook 'dired-mode-hook 'diredfl-mode))

(use-package all-the-icons-dired
    :init (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; Hide hidden files
(use-package dired-hide-dotfiles
    :hook (dired-mode . dired-hide-dotfiles-mode)
    :bind (:map dired-mode-map ("." . dired-hide-dotfiles-mode)))

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

(provide 'my-init-misc)
;;; my-init-misc.el ends here

;;; my-packages.el --- Packages Configs -*- lexical-binding: t; -*-

;;; Commentary:
;;; Packages Customizations

;;; Code:

;; ibuffer
(progn
  (customize-set-variable 'ibuffer-expert t)
  (customize-set-variable 'ibuffer-show-empty-filter-groups nil)
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")
              (ibuffer-auto-mode 1))))

;; Isearch
(progn
  (defadvice isearch-search (after isearch-no-fail activate)
    (unless isearch-success
      (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
      (ad-activate 'isearch-search)
      (isearch-repeat (if isearch-forward 'forward))
      (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
      (ad-activate 'isearch-search)))
  (customize-set-variable 'isearch-resume-in-command-history t) ; Use history for isearch as well
  (customize-set-variable 'search-whitespace-regexp ".*?") ;; Isearch convenience, space matches anything (non-greedy)
  (customize-set-variable 'isearch-lax-whitespace t)
  (customize-set-variable 'isearch-allow-motion t)) ;; Enable Emacs 28 isearch motions

;; dired : built-in navigation of folders
(progn
  (if (version< emacs-version "28.1")
      nil
    (progn
      (require 'dired-x))) ;; for dired-jump

  (customize-set-variable 'dired-dwim-target t)  ; suggest a target for moving/copying intelligently
  (customize-set-variable 'dired-hide-details-hide-symlink-targets nil)
  ;; don't prompt to revert, just do it
  (customize-set-variable 'dired-auto-revert-buffer #'dired-buffer-stale-p)
  ;; Always copy/delete recursively
  (customize-set-variable 'dired-recursive-copies 'always)
  (customize-set-variable 'dired-recursive-deletes 'top)
  ;; Ask whether destination dirs should get created when copying/removing files.
  (customize-set-variable 'dired-create-destination-dirs 'ask)
  ;; Where to store image caches
  (customize-set-variable 'image-dired-dir (concat crafted-config-var-directory "image-dired/"))
  (customize-set-variable 'image-dired-db-file (concat image-dired-dir "db.el"))
  (customize-set-variable 'image-dired-gallery-dir (concat image-dired-dir "gallery/"))
  (customize-set-variable 'image-dired-temp-image-file (concat image-dired-dir "temp-image"))
  (customize-set-variable 'image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image"))
  ;; Screens are larger nowadays, we can afford slightly larger thumbnails
  (customize-set-variable 'image-dired-thumb-size 150)
  (customize-set-variable 'dired-ls-F-marks-symlinks t) ;; mark symlinks

  ;; Dired listing switches
  ;;  -a : Do not ignore entries starting with .
  ;;  -l : Use long listing format.
  ;;  -h : Human-readable sizes like 1K, 234M, ..
  ;;  -v : Do natural sort .. so the file names starting with . will show up first.
  ;;  -F : Classify filenames by appending '*' to executables, '/' to directories, etc.
  (customize-set-variable 'dired-listing-switches (if (eq system-type 'windows-nt)
                                                      "-alh"
                                                    "-alhvF --group-directories-first")))

;; tramp : Transparent Remote (file) Access, Multiple Protocol
(progn
  (customize-set-variable 'tramp-verbose 2)
  (customize-set-variable 'tramp-use-ssh-controlmaster-options nil) ; Don't override SSH config.
  (customize-set-variable 'tramp-default-method "ssh")    ; ssh is faster than scp and supports ports.
  (customize-set-variable 'tramp-password-prompt-regexp   ; Add verification code support.
                          (concat
                           "^.*"
                           (regexp-opt
                            '("passphrase" "Passphrase"
                              "password" "Password"
                              "Verification code")
                            t)
                           ".*:\0? *")))

;; persistent-scratch : preserve scratch buffer across sessions
(crafted-package-install-package 'persistent-scratch)
(progn
  (persistent-scratch-setup-default)
  (persistent-scratch-autosave-mode 1))

;; saveplace : remembers your location in a file when saving files
(crafted-package-install-package 'saveplace)
(progn
  (customize-set-variable 'save-place-file (expand-file-name "saveplace" crafted-config-var-directory))
  (save-place-mode +1))

;; savehist : save minibuffer history
(crafted-package-install-package 'savehist)
(progn
  (customize-set-variable 'savehist-additional-variables '(search-ring regexp-search-ring)) ;; search entries
  (customize-set-variable 'savehist-autosave-interval 60) ;; save every minute
  (customize-set-variable 'savehist-file (expand-file-name "savehist" crafted-config-var-directory)) ;; keep the home clean
  (savehist-mode +1))

;; recentf : recent files
(progn
  (push (list (expand-file-name ".emacs.chemacs2/" +my/dotfiles-path)) recentf-exclude)
  (push (list (expand-file-name ".emacs.crafted/" +my/dotfiles-path)) recentf-exclude)
  (push (list (expand-file-name ".emacs.d/" +my/dotfiles-path)) recentf-exclude)
  (push (list (expand-file-name ".emacs.doom/" +my/dotfiles-path)) recentf-exclude)
  (push (list (expand-file-name ".tmuxifier/" +my/dotfiles-path)) recentf-exclude)
  (push "~/.mail" recentf-exclude)
  (push "\\.git" recentf-exclude)
  (push "/tmp/" recentf-exclude)
  (push "/ssh:" recentf-exclude)
  (push "~/\\.emacs\\.d/.local" recentf-exclude)
  (push "~/mail" recentf-exclude)
  (push "/var" recentf-exclude)
  (push "/etc" recentf-exclude)
  (push "/usr" recentf-exclude)
  (push "\\.?ido\\.last$" recentf-exclude)
  (push "^/nix/store/" recentf-exclude)
  (push ".+\\.mp3$" recentf-exclude)
  (customize-set-variable 'recentf-max-saved-items 500)
  (customize-set-variable 'recentf-max-menu-items 15)
  (customize-set-variable 'recentf-auto-cleanup 'never)
  (recentf-mode +1))

;; editorconfig : editorconfig for Emacs
(crafted-package-install-package 'editorconfig)
(progn
  (editorconfig-mode 1))

;; exec-path-from-shell : Sane environment variables
(crafted-package-install-package 'exec-path-from-shell)
(progn
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var))

  (when (daemonp)
    (exec-path-from-shell-initialize)))

;; avy : GNU Emacs package for jumping to visible text using a char-based decision tree
(crafted-package-install-package 'avy)
(progn
  (customize-set-variable 'avy-all-windows t)
  (customize-set-variable 'avy-background t))

;; diff-hl : highlights uncommitted changes on the left side of the window
(crafted-package-install-package 'diff-hl)
(progn
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; ace-window : GNU Emacs package for selecting a window to switch to
(crafted-package-install-package 'ace-window)

;; crux : A Collection of Ridiculously Useful eXtensions for Emacs
(crafted-package-install-package 'crux)

;; vterm : terminal emulator
(crafted-package-install-package 'vterm)
(progn
  (add-hook
   'vterm-mode-hook (lambda ()
                      (add-to-list 'vterm-tramp-shells '("ssh" "/bin/sh"))
                      (customize-set-variable 'global-hl-line-mode nil)
                      (display-line-numbers-mode 0)))
  (customize-set-variable 'vterm-shell "/usr/bin/bash"))

;; eshell : the emacs shell
(progn
  (customize-set-variable 'eshell-highlight-prompt nil)
  (customize-set-variable 'eshell-scroll-to-bottom-on-input nil)
  (customize-set-variable 'eshell-scroll-to-bottom-on-output nil)
  (customize-set-variable 'eshell-prefer-lisp-functions nil)
  (customize-set-variable 'eshell-error-if-no-glob t)
  (customize-set-variable 'eshell-hist-ignoredups t)
  (customize-set-variable 'eshell-save-history-on-exit t)
  (customize-set-variable 'eshell-destroy-buffer-when-process-dies t)

  ;; Aliases
  (add-hook 'eshell-mode-hook
            (lambda ()
              ;; The 'ls' executable requires the Gnu version on the Mac
              (let ((ls (if (file-exists-p "/usr/local/bin/gls")
                            "/usr/local/bin/gls"
                          "/bin/ls")))
                (eshell/alias "ll" (concat ls " -alh --group-directories-first --color=auto")))
              (eshell/alias "ff" "find-file $1")
              (eshell/alias "e" "find-file-other-window $1")
              (eshell/alias "d" "dired $1"))))

;; whole-line-or-region : better C-w and M-w
(crafted-package-install-package 'whole-line-or-region)
(progn
  (whole-line-or-region-global-mode))

;; magit : Git front end (amazing!)
(crafted-package-install-package 'magit)
(progn
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defadvice magit-quit-window (after magit-restore-screen activate)
    (jump-to-register :magit-fullscreen))

  (customize-set-variable 'git-commit-summary-max-length 80)
  (customize-set-variable 'vc-handled-backends (delq 'Git vc-handled-backends))
  (add-hook 'git-commit-mode-hook (lambda () (setq-local fill-column 80))))

;; which-key : Displays command shortcuts when typing commands
(crafted-package-install-package 'which-key)
(progn
  (which-key-mode +1))

;; hydra : Keybindings combinations
(crafted-package-install-package 'hydra)

;; Expand Region : expand or contract selection
(crafted-package-install-package 'expand-region)

;; Helpful : nice looking and more complete help buffers
(crafted-package-install-package 'helpful)

;; neotree : A Emacs tree plugin like NerdTree for Vim.
;; (crafted-package-install-package 'neotree)
;; (progn
;;     (setc neo-theme 'ascii)
;;     (setc neo-window-width 32)
;;     (setc neo-smart-open t)
;;     (setc neo-create-file-auto-open nil)
;;     (setc neo-show-updir-line t)
;;     (setc neo-show-hidden-files t)
;;     (setc neo-auto-indent-point t)
;;     (setc neo-vc-integration nil)
;;     (setc neo-autorefresh nil)
;;     (setc neo-auto-indent-point nil)
;;     (setc neo-mode-line-type 'none)
;;     (setc neo-banner-message nil)
;;     (setc neo-confirm-create-file #'off-p)
;;     (setc neo-confirm-create-directory #'off-p)
;;     (setc neo-keymap-style 'concise)
;;     (setc neo-hidden-regexp-list
;;         '(;; vcs folders
;;              "^\\.\\(?:git\\|hg\\|svn\\)$"
;;              ;; compiled files
;;              "\\.\\(?:pyc\\|o\\|elc\\|lock\\|css.map\\|class\\)$"
;;              ;; generated files, caches or local pkgs
;;              "^\\(?:node_modules\\|vendor\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"
;;              ;; org-mode folders
;;              "^\\.\\(?:sync\\|export\\|attach\\)$"
;;              ;; temp files
;;              "~$"
;;              "^#.*#$"
;;              ;; Others
;;              "^\\.\\(cache\\|tox\\|coverage\\)$"
;;              "^\\.\\(DS_Store\\|python\\-version\\)"
;;              "^\\(htmlcov\\)$" "\\.elcs$"
;;              "^\\.coverage\\..*" "\\.ipynb.*$" "\\.py[cod]$"
;;              "^\\.#.*$" "^__pycache__$"
;;              "\\.gcda$" "\\.gcov$" "\\.gcno$" "\\.lo$" "\\.o$" "\\.so$"
;;              "^\\.cproject$" "^\\.project$" "^\\.projectile$"
;;              "^\\.log$"
;;              "\\.egg\-info$")))

;; treemacs : a tree layout file explorer for Emacs
(crafted-package-install-package 'treemacs)
(progn
  (customize-set-variable 'treemacs-no-png-images t)
  (customize-set-variable 'treemacs-is-never-other-window nil)
  (customize-set-variable 'treemacs-follow-after-init t)
  (customize-set-variable 'treemacs-sorting 'alphabetic-case-insensitive-asc)
  (customize-set-variable 'treemacs-persist-file (concat crafted-config-var-directory "treemacs-persist"))
  (customize-set-variable 'treemacs-last-error-persist-file (concat crafted-config-var-directory "treemacs-last-error-persist")))

;; deft : plain text notes
(crafted-package-install-package 'deft)
(progn
  (customize-set-variable 'deft-directory (expand-file-name "Apps/org/notes" +my/dropbox-path))
  (customize-set-variable 'deft-extensions '("org" "md" "txt"))
  (customize-set-variable 'deft-default-extension "org")
  (customize-set-variable 'deft-recursive nil)
  (customize-set-variable 'deft-use-filename-as-title nil)
  (customize-set-variable 'deft-use-filter-string-for-filename t)
  (customize-set-variable 'deft-file-naming-rules '((noslash . "-")
                                                    (nospace . "-")
                                                    (case-fn . downcase)))
  (customize-set-variable 'deft-auto-save-interval 0))

;; markdown-mode : edit markdown-formatted text
(crafted-package-install-package 'markdown-mode)
(progn
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist
               '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))

  (autoload 'gfm-mode "markdown-mode"
    "Major mode for editing GitHub Flavored Markdown files" t)
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
  (customize-set-variable 'markdown-command "multimarkdown"))

;; csv : edit csv-formatted text
(crafted-package-install-package 'csv-mode)

;; yaml : edit yaml-formatted text
(crafted-package-install-package 'yaml-mode)

(provide 'my-packages)
;;; my-packages.el ends here

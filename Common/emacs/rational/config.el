;;; config.el ---  Emacs Configuration -*- lexical-binding: t; -*-
;;;
;;; https://github.com/SystemCrafters/rational-emacs
;;; https://systemcrafters.net/live-streams/july-1-2022/

;;; Rational Modules
(require 'rational-defaults)   ;;; Sane Defaults
(require 'rational-ui)         ;;; User Interface
;; (require 'rational-evil)       ;;; Evil?
(require 'rational-completion) ;;; Completions and Actions
(require 'rational-project)    ;;; Project Management
(require 'rational-screencast) ;;; Presentations
(require 'rational-ide)  ;;; IDE
(require 'rational-lisp)  ;;; Lisp Editing

;;;; Misc
(defconst +my/savefile-dir (expand-file-name "savefile" user-emacs-directory))
(unless (file-exists-p +my/savefile-dir)
    (make-directory +my/savefile-dir)) ;; create the savefile dir if it doesn't exist

;;; Defaults
(progn
    (setq user-full-name "Julio C. Villasante"
        user-mail-address "jvillasantegomez@gmail.com"
        user-login-name "jvillasante")
    (setq load-prefer-newer t) ;; Always load newest byte code
    (blink-cursor-mode -1) ;; the blinking cursor is nothing, but an annoyance
    (setq visible-cursor nil) ;; make it work in terminal too
    (setq auto-window-vscroll nil  ; fast scrolling
        fast-but-imprecise-scrolling t
        scroll-margin 2
        scroll-conservatively 101
        scroll-preserve-screen-position t)
    (when (fboundp 'pixel-scroll-precision-mode)
        (pixel-scroll-precision-mode t))
    (if (boundp 'use-short-answers) ;; Use "y" and "n" to confirm/negate prompt
        (setq use-short-answers t)
        (advice-add 'yes-or-no-p :override #'y-or-n-p))
    (setq large-file-warning-threshold 100000000) ;; warn when opening files bigger than 100MB
    (setq confirm-kill-processes nil) ;; quit Emacs directly even if there are running processes
    (savehist-mode) ; Save history for commands
    (setq isearch-resume-in-command-history t) ; Use history for isearch as well
    (setq-default auto-revert-verbose t) ; show message when file changes
    (setq-default auto-revert-avoid-polling t) ; use save signal
    (global-auto-revert-mode t) ; Refresh files automatically when modified from outside emacs
    (setq enable-local-eval t) ; Enable eval blocks in .dir-locals.el
    (setq enable-local-variables :all) ; Enable by default variables in .dir-locals.el
    (setq ring-bell-function 'ignore) ; Disable the bell for emacs
    (setq debug-on-error nil) ; Display the stacktrace if error encountered in one of the lisp method
    (setq completions-detailed t) ; Detailed description for the built in describe symbol etc
    (delete-selection-mode 1) ; If text is selected, we expect that typing will replace the selection
    (save-place-mode +1) ; Remember point in files
    (electric-pair-mode +1) ; auto-insert matching parenteses
    (show-paren-mode +1) ; Highlight the matching parenthesis
    (global-so-long-mode +1) ; long files
    (setq kill-do-not-save-duplicates t) ; Do not save duplicates in kill-ring
    (setq auto-save-default nil) ; Don't autosave files with default Emacs package (we'll use `super-save' pacakge isntead)
    (setq search-whitespace-regexp ".*?") ;; Isearch convenience, space matches anything (non-greedy)
    (setq next-error-message-highlight t) ; When jumping between errors, occurs, etc, highlight the current line
    (setq use-short-answers t) ; Abreviate Yes/No to y or n
    (setq require-final-newline t) ;; Newline at end of file
    (setq-default fill-column 132) ;; Wrap lines at 132 characters
    (setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
    (setq-default tab-width 4)            ;; but maintain correct appearance
    (setq indent-line-function 'insert-tab) ;; indent the current line
    (setq standard-indent 4)
    (setq-default c-basic-offset  4) ; Base indent size when indented automatically
    (c-set-offset 'cpp-macro 0 nil) ; Indent C/C++ macros as normal code
    (c-set-offset 'substatement-open 0) ; Align braces with the if/for statement. If not set, a half indent will be used
    (c-set-offset 'arglist-intro '+) ; Align multiline arguments with a standard indent (instead of with parenthesis)
    (c-set-offset 'arglist-close 0) ; Align the parenthesis at the end of the arguments with the opening statement indent
    (setq make-backup-files nil) ; Do not use backup files (filename~)
    (setq create-lockfiles nil)) ; Do not use lock files (.#filename)

;;; User Interface
;; theme
(defun +my/switch-theme (theme)
    "This interactive call is taken from `load-theme'."
    (interactive
        (list
            (intern (completing-read "Load custom theme: "
                        (mapcar 'symbol-name
                            (custom-available-themes))))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t))
(rational-package-install-package 'modus-themes)
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs nil
      modus-themes-region '(bg-only no-extend))
(modus-themes-load-themes);
(+my/switch-theme 'modus-operandi)

;; modeline
(rational-package-install-package 'minions)
(add-hook 'doom-modeline-mode-hook 'minions-mode)

;; Set configuration variables
(custom-set-variables '(rational-ui-display-line-numbers t)
                      '(doom-modeline-height 35))

;;; Evil stuff
;; Set configuration variables
;; (custom-set-variables '(rational-evil-discourage-arrow-keys t)
;;                       '(evil-want-C-u-scroll t))
;; Set preferred key bindings
;; (global-set-key (kbd "M-/") 'evilnc-comment-or-uncomment-lines)
;; (global-set-key (kbd "C-M-u") 'universal-argument)

;;; Completions and Actions
(setq vertico-cycle t
      vertico-count 15)
;; (define-key vertico-map (kbd "C-f") 'vertico-exit)
;; (define-key minibuffer-local-map (kbd "C-d") 'embark-act)
;; (define-key project-prefix-map (kbd "g") 'consult-ripgrep)
;; (global-set-key (kbd "C-M-j") 'consult-buffer)

;;; Source Control
(rational-package-install-package 'magit)
(setq git-commit-summary-max-length 80
      magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
(global-set-key (kbd "C-x g") #'magit-status)

;;; Org Mode
;; Turn on variable pitch for non-monospace fonts
(variable-pitch-mode 1)

;;; IDE
(rational-package-install-package 'typescript-mode)

;;; Shells
(rational-package-install-package 'exec-path-from-shell)
(when (daemonp) (exec-path-from-shell-initialize))
(rational-package-install-package 'vterm)
(setq vterm-shell "/usr/bin/bash"
      vterm-max-scrollback 10000)
(add-hook 'vterm-mode-hook (lambda ()
                             (setq-local global-hl-line-mode nil)
                             (display-line-numbers-mode nil)))
(rational-package-install-package 'xterm-color)

;;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list
    '(try-expand-dabbrev
         try-expand-dabbrev-all-buffers
         try-expand-dabbrev-from-kill
         try-complete-file-name-partially
         try-complete-file-name
         try-expand-all-abbrevs
         try-expand-list
         try-expand-line
         try-complete-lisp-symbol-partially
         try-complete-lisp-symbol))

;;; scratch buffer
(with-current-buffer "*scratch*" (emacs-lock-mode 'kill))
(rational-package-install-package 'persistent-scratch)
(persistent-scratch-setup-default)
(persistent-scratch-autosave-mode 1)

;;; saveplace : remembers your location in a file when saving files
(setq save-place-file (expand-file-name "saveplace" +my/savefile-dir))
(save-place-mode +1)

;;; savehist : save minibuffer history
(setq savehist-additional-variables
      '(search-ring regexp-search-ring) ;; search entries
      savehist-autosave-interval 60 ;; save every minute
      savehist-file (expand-file-name "savehist" +my/savefile-dir)) ;; keep the home clean
(savehist-mode +1)

;;; recentf : recent files
(setq recentf-save-file (expand-file-name "recentf" +my/savefile-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      recentf-auto-cleanup 'never)
(recentf-mode +1)

;;; dired : built-in navigation of folders
(setq dired-ls-F-marks-symlinks t) ;; mark symlinks
(setq dired-recursive-copies 'always) ;; Never prompt for recursive copies of a directory
(setq dired-recursive-deletes 'always) ;; Never prompt for recursive deletes of a directory
(setq dired-dwim-target t) ;; makes dired guess the target directory
(setq dired-auto-revert-buffer t) ;; auto-revert dired buffers if file changed on disk
(setq projectile-switch-project-action 'projectile-dired) ;; dired loads on project switch

;; Dired listing switches
;;  -a : Do not ignore entries starting with .
;;  -l : Use long listing format.
;;  -h : Human-readable sizes like 1K, 234M, ..
;;  -v : Do natural sort .. so the file names starting with . will show up first.
;;  -F : Classify filenames by appending '*' to executables, '/' to directories, etc.
(setq dired-listing-switches (if (eq system-type 'windows-nt)
                                 "-alh"
                               "-alhvF --group-directories-first"))
(require 'dired-x) ;; enable some really cool extensions like C-x C-j(dired-jump)

;;; whitespace : visualize blanks (tabs, spaces, newline, etc)
(setq show-trailing-whitespace t)
(setq whitespace-action '(auto-cleanup))
(setq whitespace-line-column fill-column) ;; limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))
(add-hook 'phyton-mode-hook #'whitespace-mode)
(add-hook 'makefile-mode-hook #'whitespace-mode)
(add-hook 'before-save-hook #'whitespace-cleanup)

;;; editorconfig : editorconfig for emacs
(rational-package-install-package 'editorconfig)
(editorconfig-mode 1)

;;; ace-window : emacs package for selecting a window to switch to.
(rational-package-install-package 'ace-window)

;;; crux : a collection of ridiculously useful extensions for emacs
(rational-package-install-package 'crux)

;;; avy : emacs package for jumping to visible text using a char-based decision tree.
(rational-package-install-package 'avy)
(setq avy-all-windows t
      avy-background t)

;;; super-save : auto-saves your buffers, when certain events happen
(rational-package-install-package 'super-save)
(setq
 super-save-exclude '(".gpg")
 super-save-auto-save-when-idle t
 super-save-remote-files nil)
(super-save-mode +1)
(add-to-list 'super-save-triggers 'ace-window)

;;; easy-kill : kill things easily
(rational-package-install-package 'easy-kill)

;;; expand-region : expand or contract selection
(rational-package-install-package 'expand-region)

;;; neotree : a emacs tree plugin like NerdTree for Vim
(rational-package-install-package 'neotree)
(setq neo-theme 'ascii
      neo-window-width 42
      neo-smart-open t
      neo-create-file-auto-open nil
      neo-show-updir-line t
      neo-show-hidden-files t
      neo-auto-indent-point t
      neo-vc-integration nil
      neo-autorefresh nil
      projectile-switch-project-action 'neotree-projectile-action
      neo-hidden-regexp-list
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
        "\\.egg\-info$"))

;;; deft : play text notes.
(rational-package-install-package 'deft)
(setq
 deft-directory "~/Dropbox/Apps/org/notes"
 deft-extensions '("org" "md" "txt")
 deft-default-extension "org"
 deft-recursive nil
 deft-use-filename-as-title nil
 deft-use-filter-string-for-filename t
 deft-file-naming-rules '((noslash . "-")
                          (nospace . "-")
                          (case-fn . downcase))
 deft-auto-save-interval 0)

;;; elfeed : rss
(rational-package-install-package 'elfeed)
(setq elfeed-search-title-min-width 60)
(setq elfeed-search-title-max-width 100)
(setq elfeed-search-trailing-width 0)
(setq elfeed-search-filter "@6-months-ago +unread")
(setq elfeed-db-directory "~/Dropbox/Apps/elfeed/elfeed_db")

;;; Some Hooks
(add-hook 'text-mode-hook (lambda () (visual-line-mode)))
(dolist (hook '(special-mode-hook
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                minibuffer-setup-hook))
  (add-hook hook (lambda () (setq show-trailing-whitespace nil))))

;;; encryption
;; https://orgmode.org/worg/org-tutorials/encrypting-files.html
(progn
  (require 'epa-file)
  (epa-file-enable)
  (setq epa-file-encrypt-to user-mail-address
        epa-file-select-keys 'silent
        epa-file-cache-passphrase-for-symmetric-encryption nil)
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-crypt-disable-auto-save nil
        org-tags-exclude-from-inheritance (quote ("crypt"))
        org-crypt-key nil
      org-crypt-key user-mail-address))

;;; keybindings
(rational-package-install-package 'general)

;; * Global Keybindings
;; `general-define-key' acts like `global-set-key' when :keymaps is not
;; specified (because ":keymaps 'global" is the default)
;; kbd is not necessary and arbitrary amount of key def pairs are allowed
(general-define-key
    ;; general keys
    "C-x C-m" 'execute-extended-command ; ALT may or may not be available
    "C-c C-m" 'execute-extended-command ; ALT may or may not be available
    "C-c u" 'browse-url-at-point ; simple browse url
    "C-x k" 'kill-this-buffer ; kill buffer without prompt
    "C-x K" 'kill-buffer ; prompt for buffer to kill
    "M-/" 'hippie-expand ; use hippie-expand instead of debbrev
    [remap list-buffers] 'ibuffer ; ibuffer is better than list-buffers

    ;; easy-kill
    [remap kill-ring-save] 'easy-kill

    ;;;; avy
    "C-;" 'avy-goto-char-timer ; most usefull avy function

    ;; ace-window
    [remap other-window] 'ace-window ; better other window

    ;; anzu
    ;; "M-%" 'anzu-query-replace
    ;; "C-M-%" 'anzu-query-replace-regexp

    ;; expand-region
    "C-=" 'er/expand-region

    ;; magit
    "C-x g" 'magit-status

    ;; embark
    "C-." 'embark-act        ; pick some comfortable binding
    "C-;" 'embark-dwim       ; good alternative: "M-."
    "C-h B" 'embark-bindings ; alternative for `describe-bindings'

    ;; projectile
    ;; "C-c p" 'projectile-command-map

    ;; consult
    ;; C-c bindings (mode-specific-map)
    "C-c h" 'consult-history
    "C-c m" 'consult-mode-command
    ;; "C-c k" 'consult-kmacro
    ;; C-x bindings (Ctl-x-map)
    "C-x M-:" 'consult-complex-command     ; orig. repeat-complex-command
    "C-x b" 'consult-buffer                ; orig. switch-to-buffer
    "C-x 4 b" 'consult-buffer-other-window ; orig. switch-to-buffer-other-window
    "C-x 5 b" 'consult-buffer-other-frame  ; orig. switch-to-buffer-other-frame
    "C-x r b" 'consult-bookmark            ; orig. bookmark-jump
    "C-x p b" 'consult-project-buffer      ; orig. project-switch-to-buffer
    ;; Custom M-# bindings for fast register access
    "M-#" 'consult-register-load
    "M-'" 'consult-register-store          ; orig. abbrev-prefix-mark (unrelated)
    "C-M-#" 'consult-register
    ;; Other custom bindings
    "M-y" 'consult-yank-pop                ; orig. yank-pop
    "<help> a" 'consult-apropos            ; orig. apropos-command
    ;; M-g bindings (goto-map)
    "M-g e" 'consult-compile-error
    "M-g f" 'consult-flymake               ; Alternative: consult-flycheck
    "M-g g" 'consult-goto-line             ; orig. goto-line
    "M-g M-g" 'consult-goto-line           ; orig. goto-line
    "M-g o" 'consult-outline               ; Alternative: consult-org-heading
    "M-g m" 'consult-mark
    "M-g k" 'consult-global-mark
    "M-g i" 'consult-imenu
    "M-g I" 'consult-imenu-multi
    ;; M-s bindings (search-map)
    "M-s d" 'consult-find
    "M-s D" 'consult-locate
    "M-s g" 'consult-grep
    "M-s G" 'consult-git-grep
    "M-s r" 'consult-ripgrep
    "M-s l" 'consult-line
    "M-s L" 'consult-line-multi
    "M-s m" 'consult-multi-occur
    "M-s k" 'consult-keep-lines
    "M-s u" 'consult-focus-lines
    ;; Isearch integration
    "M-s e" 'consult-isearch-history

    ;; crux
    ;; "C-c o" 'crux-open-with
    ;; "C-c u" 'crux-view-url
    "C-o" 'crux-smart-open-line
    "M-o" 'crux-smart-open-line-above
    "C-x C-r" 'crux-recentf-find-file
    "C-c f" 'crux-recentf-find-file
    "C-c F" 'crux-recentf-find-directory
    ;; "C-c n" 'crux-cleanup-buffer-or-region
    "C-M-z" 'crux-indent-defun
    "C-c e" 'crux-eval-and-replace
    "C-c w" 'crux-swap-windows
    "C-c D" 'crux-delete-file-and-buffer
    "C-c r" 'crux-rename-buffer-and-file
    "C-c t" 'crux-visit-term-buffer
    "C-c k" 'crux-kill-other-buffers
    "C-c TAB" 'crux-indent-rigidly-and-copy-to-clipboard
    "C-c I" 'crux-find-user-custom-file
    "C-c S" ' crux-find-shell-init-file
    "C-^" 'crux-top-join-line
    "C-c s" 'crux-ispell-word-then-abbrev
    "C-k" 'crux-smart-kill-line
    "C-<backspace>" 'crux-kill-line-backwards
    "C-x 4 t" 'crux-transpose-windows
    "C-x C-u" 'crux-upcase-region
    "C-x C-l" 'crux-downcase-region
    "C-x M-c" 'crux-capitalize-region
    [remap move-beginning-of-line] 'crux-move-beginning-of-line
    [shift return] 'crux-smart-open-line
    [control shift return] 'crux-smart-open-line-above
    [remap kill-whole-line] 'crux-kill-whole-line)

;; * Prefix Keybindings
;; :prefix can be used to prevent redundant specification of prefix keys
(general-define-key :prefix "C-c c" ; code
    "d" 'lsp-describe-thing-at-point
    "f" 'format-all-buffer)
(general-define-key :prefix "C-c n" ; notes
    "d" 'deft-find-file
    "D" 'deft)
(general-define-key :prefix "C-c o" ; open
    "c" 'calc
    "C" 'quick-calc
    "e" 'elfeed
    "n" 'neotree-toggle
    "v" 'vterm)

;; * Mode Keybindings
;; `general-define-key' is comparable to `define-key' when :keymaps is specified
(general-define-key :keymaps 'dired-mode-map
    "C-c o" 'crux-open-with)
(general-define-key :keymaps 'isearch-mode-map
    "M-e" 'consult-isearch-history         ; orig. isearch-edit-string
    "M-s e" 'consult-isearch-history       ; orig. isearch-edit-string
    "M-s l" 'consult-line                  ; needed by consult-line to detect isearch
    "M-s L" 'consult-line-multi)           ; needed by consult-line to detect isearch
(general-define-key :keymaps 'minibuffer-local-map
    "M-s" 'consult-history                 ; orig. next-matching-history-element
    "M-r" 'consult-history)                ; orig. previous-matching-history-element

;; To not load `custom.el' after `config.el', uncomment this line.
;; (setq rational-load-custom-file nil)

;;; example-config.el ends here

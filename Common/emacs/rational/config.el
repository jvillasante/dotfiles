;;; config.el --- Emacs Configuration -*- lexical-binding: t; -*-
;;
;;; https://github.com/SystemCrafters/rational-emacs
;;; https://systemcrafters.net/live-streams/july-1-2022/

;;;; Rational Modules
(require 'rational-defaults)    ; Sensible default settings for Emacs
(require 'rational-use-package) ; Configuration for `use-package`
(require 'rational-updates)     ; Tools to upgrade Rational Emacs
(require 'rational-completion)  ; selection framework based on `vertico`
(require 'rational-ui)          ; Better UI experience (modeline etc.)
(require 'rational-windows)     ; Window management configuration
(require 'rational-editing)     ; Whitspace trimming, auto parens etc.
;;(require 'rational-evil)        ; An `evil-mode` configuration
(require 'rational-org)         ; org-appear, clickable hyperlinks etc.
(require 'rational-project)     ; built-in alternative to projectile
(require 'rational-speedbar)    ; built-in file-tree
;;(require 'rational-screencast)  ; show current command and binding in modeline
(require 'rational-python)
(require 'rational-lisp)
(require 'rational-ide)
(require 'rational-compile)
;;(require 'osx)

;;;; Autoloads
(load "autoload")

;;;; Misc
(defconst +my/savefile-dir (expand-file-name "savefile" user-emacs-directory))
(unless (file-exists-p +my/savefile-dir)
    (make-directory +my/savefile-dir)) ;; create the savefile dir if it doesn't exist

;; frame title
(setq-default frame-title-format
    '(:eval
         (format "%s@%s: %s"
             (or (file-remote-p default-directory 'user)
                 user-real-login-name)
             (or (file-remote-p default-directory 'host)
                 system-name)
             (cond
                 (buffer-file-truename
                     (concat buffer-file-truename))
                 (dired-directory
                     (concat dired-directory))
                 (t (buffer-name))))))

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

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

;;; repead-mode by default
(repeat-mode)

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

;;;; User Interface
;;; theme
(rational-package-install-package 'modus-themes)
(with-eval-after-load "modus-themes"
    (setq
        modus-themes-mode-line '(borderless (padding 1) (height 0.9))
        modus-themes-bold-constructs nil
        modus-themes-italic-constructs t
        modus-themes-fringes 'subtle
        modus-themes-tabs-accented t
        modus-themes-subtle-line-numbers t
        modus-themes-diffs 'desaturated
        modus-themes-region '(bg-only no-extend)
        modus-themes-headings
        '((1 . (monochrome variable-pitch 1.3))
             (2 . (monochrome variable-pitch 1.2))
             (3 . (monochrome variable-pitch 1.1))
             (t . (monochrome)))))
(modus-themes-load-themes)
(if (daemonp)
    (add-hook 'after-make-frame-functions
        (lambda (frame)
            (with-selected-frame frame
                (+my/switch-theme 'modus-operandi)))) ;; OR (modus-themes-load-vivendi)
    (+my/switch-theme 'modus-operandi)) ;; OR (modus-themes-load-vivendi))

;;; modeline
(with-eval-after-load "doom-modeline"
    (setq doom-modeline-icon nil)
    (setq doom-modeline-height 1)
    (setq doom-modeline-lsp t)
    (custom-set-faces
        '(mode-line ((t (:height 0.9))))
        '(mode-line-active ((t (:height 0.9))))
        '(mode-line-inactive ((t (:height 0.9))))))

(rational-package-install-package 'minions)
(add-hook 'doom-modeline-mode-hook 'minions-mode)

;; Emacs 29 improved scrolling
(pixel-scroll-precision-mode)

;;; line numbers
(custom-set-variables
    '(rational-ui-line-numbers-disabled-modes `(org-mode vterm-mode term-mode shell-mode eshell-mode))
    '(rational-ui-display-line-numbers t))

;; all-the-icons
(setq all-the-icons-scale-factor 1.1)

;;; which-key : display keybindings
(rational-package-install-package 'which-key)
(which-key-mode)

;;;; Source Control
(rational-package-install-package 'magit)
(with-eval-after-load "magit"
    (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
    (setq git-commit-summary-max-length 80))

;;;; Org Mode
(with-eval-after-load "org"
    (setq org-return-follows-link  t
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-startup-indented t
        org-startup-folded t
        org-hide-emphasis-markers t))
(add-hook 'org-mode-hook
    (lambda ()
        ;; disable auto-pairing of "<" in org-mode
        (setq-local electric-pair-inhibit-predicate
            `(lambda (c) (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))
        (add-to-list 'recentf-exclude ".*org$") ; Ignore org files from recentf due to agenda loading everything
        (variable-pitch-mode)
        (visual-line-mode)))

;;;; IDE
;; (rational-package-install-package 'typescript-mode)
(rational-package-install-package 'yasnippet)

;;;; c++ mode
(use-package c++-mode
    :ensure nil  ; Part of emacs
    :mode ("\\.h\\'" "\\.cpp\\'" "\\.hpp\\'" "\\.hxx\\'" "\\.cxx\\'")
    :config
    (advice-add 'c-update-modeline :override #'ignore)) ;; Don't use a modeline suffix (i.e C++//l)

;; C & C++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hxx\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.C\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-mode))
(with-eval-after-load "c++-mode"
    (c-set-offset 'innamespace 0)
    (setq c-default-style "stroustrup"
        c-basic-offset 4
        indent-tabs-mode t))

;; Rust
(rational-package-install-package 'rustic)
(with-eval-after-load "rustic"
    (setq rustic-format-on-save nil
        rustic-lsp-server 'rust-analyzer
        lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-inlay-hints-mode nil
        lsp-rust-analyzer-server-display-inlay-hints nil))

;; eglot
(with-eval-after-load "eglot"
    (setq eldoc-echo-area-use-multiline-p nil)
    (setq eglot-extend-to-xref t)
    (add-to-list 'eglot-server-programs
        `(rust-mode . ("rust-analyzer"))
        `(c-mode c++-mode
             . ("/usr/bin/clangd"
                   "-j=4"
                   "--malloc-trim"
                   "--log=error"
                   "--background-index"
                   "--clang-tidy"
                   "--cross-file-rename"
                   "--completion-style=detailed"
                   "--pch-storage=memory"
                   "--header-insertion=never"
                   "--header-insertion-decorators=0"))))

;;; Shells
(rational-package-install-package 'exec-path-from-shell)
(when (daemonp) (exec-path-from-shell-initialize))

;; vterm is kind of nice (but I still prefer shell)
(rational-package-install-package 'vterm)
(with-eval-after-load "vterm"
    (setq vterm-shell "/usr/bin/bash"
        vterm-max-scrollback 10000)
    (add-hook 'vterm-mode-hook
        (lambda ()
            (setq-local global-hl-line-mode nil)
            (display-line-numbers-mode nil))))

;; nice colors
(rational-package-install-package 'xterm-color)

;; eshell
(with-eval-after-load "eshell"
    (setq eshell-highlight-prompt nil
        eshell-scroll-to-bottom-on-input 'all
        eshell-error-if-no-glob t
        eshell-hist-ignoredups t
        eshell-save-history-on-exit t
        eshell-prefer-lisp-functions nil
        eshell-destroy-buffer-when-process-dies t)

    ;; Aliases
    (add-hook 'eshell-mode-hook
        (lambda ()
            ;; The 'ls' executable requires the Gnu version on the Mac
            (let ((ls (if (file-exists-p "/usr/local/bin/gls")
                          "/usr/local/bin/gls"
                          "/bin/ls")))
                (eshell/alias "ll" (concat ls " -AlohG --color=always"))))))

;;;; Stuff
;;; tramp
(with-eval-after-load "tramp"
    (setq tramp-use-ssh-controlmaster-options nil) ; Don't override SSH config.
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

;;; scratch buffer
(with-current-buffer "*scratch*" (emacs-lock-mode 'kill))
(rational-package-install-package 'persistent-scratch)
(persistent-scratch-setup-default)
(persistent-scratch-autosave-mode 1)

;;; isearch
(defadvice isearch-search (after isearch-no-fail activate)
    (unless isearch-success
        (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
        (ad-activate 'isearch-search)
        (isearch-repeat (if isearch-forward 'forward))
        (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
        (ad-activate 'isearch-search)))

;;; saveplace : remembers your location in a file when saving files
(setq save-place-file (expand-file-name "saveplace" +my/savefile-dir))
(save-place-mode +1)

;;; savehist : save minibuffer history
(setq savehist-additional-variables
    '(search-ring regexp-search-ring) ;; search entries
    savehist-autosave-interval 60 ;; save every minute
    savehist-file (expand-file-name "savehist" +my/savefile-dir)) ;; keep the home clean
(savehist-mode +1)

;;; super-save : auto-saves your buffers, when certain events happen
(rational-package-install-package 'super-save)
(with-eval-after-load "super-save"
    (when (fboundp 'ace-window) (add-to-list 'super-save-triggers 'ace-window))
    (setq super-save-exclude '(".gpg")
        super-save-auto-save-when-idle t
        super-save-remote-files nil))
(super-save-mode +1)

;;; recentf : recent files
(setq recentf-save-file (expand-file-name "recentf" +my/savefile-dir)
    recentf-max-saved-items 500
    recentf-max-menu-items 15
    recentf-auto-cleanup 'never)
(push #'+org-is-agenda-file recentf-exclude)
(push "~/Dropbox/Apps/elfeed" recentf-exclude)
(push "~/.emacs.d" recentf-exclude)
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
(recentf-mode +1)

;;; dired : built-in navigation of folders
(with-eval-after-load "dired"
    (setq dired-ls-F-marks-symlinks t) ;; mark symlinks
    (setq dired-recursive-copies 'always) ;; Never prompt for recursive copies of a directory
    (setq dired-recursive-deletes 'always) ;; Never prompt for recursive deletes of a directory
    (setq dired-dwim-target t) ;; makes dired guess the target directory
    (setq dired-auto-revert-buffer t) ;; auto-revert dired buffers if file changed on disk
    (setq projectile-switch-project-action 'projectile-dired) ;; dired loads on project switch
    (setq wdired-allow-to-change-permissions t) ;; allow to edit permissions in wdired

    ;; Dired listing switches
    ;;  -a : Do not ignore entries starting with .
    ;;  -l : Use long listing format.
    ;;  -h : Human-readable sizes like 1K, 234M, ..
    ;;  -v : Do natural sort .. so the file names starting with . will show up first.
    ;;  -F : Classify filenames by appending '*' to executables, '/' to directories, etc.
    (setq dired-listing-switches (if (eq system-type 'windows-nt)
                                     "-alh"
                                     "-alhvF --group-directories-first")))

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

;;; crux : a collection of ridiculously useful extensions for emacs
(rational-package-install-package 'crux)

;;; avy : emacs package for jumping to visible text using a char-based decision tree.
(rational-package-install-package 'avy)
(with-eval-after-load "avy"
    (setq avy-all-windows t
        avy-background t))

;;; whole-line-or-region : operate on the current line if no region is active
(rational-package-install-package 'whole-line-or-region)
(whole-line-or-region-global-mode)

;;; expand-region : expand or contract selection
(rational-package-install-package 'expand-region)

;;; deft : plain text notes.
(rational-package-install-package 'deft)
(with-eval-after-load "deft"
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
        deft-auto-save-interval 0))

;;; format-all : auto-format source code in many languages using the same command for all languages
(rational-package-install-package 'format-all)
(with-eval-after-load "format-all"
    (custom-set-variables
        '(format-all-formatters (quote (("C++" clang-format)
                                           ("Python" black))))))
(add-hook 'c-mode 'format-all-mode);
(add-hook 'c++-mode 'format-all-mode);
(add-hook 'python-mode 'format-all-mode);
(add-hook 'format-all-mode 'format-all-ensure-formatter)

;;; anzu : displays current match and total matches information in the mode-line in various search modes
(rational-package-install-package 'anzu)
(global-anzu-mode)

;;; markdown-mode : edit markdown-formatted text
(rational-package-install-package 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
(with-eval-after-load "markdown-mode"
    (setq markdown-fontify-code-blocks-natively t))

;;; csv-mode : Support for csv files (use csv-align-mode for alignment)
(rational-package-install-package 'csv-mode)
(add-to-list 'auto-mode-alist '("\\.csv\\'" . csv-mode))

;;; yaml-mode : Support gitlab-ci.yml
(rational-package-install-package 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;;; web-mode : Support various web files
(rational-package-install-package 'web-mode)
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;;; prettier-js : Formatting on save, used by my-ts-mode for .js and .ts files
(rational-package-install-package 'prettier-js)
(with-eval-after-load "prettier-js"
    (prettier-js-show-errors nil)
    (prettier-js-args '("--semi" "false"
                           "--single-quote" "false"
                           "--tab-width" "4"
                           "--trailing-comma" "all"
                           "--print-width" "150")))

;;; hl-todo : highlight TODO and similar keywords in comments and strings
(rational-package-install-package 'hl-todo)
(with-eval-after-load "hl-todo"
    (setq hl-todo-highlight-punctuation ":"))
(global-hl-line-mode);

;; docker : Emacs integration for Docker
(rational-package-install-package 'docker)
(rational-package-install-package 'docker-tramp)
(with-eval-after-load "docker"
    (setq docker-container-shell-file-name "/bin/bash")
    (add-to-list 'docker-image-run-custom-args
        `("^sm" ("-v \"$HOME\"/Workspace/Work/Projects/dmxs:/tmp/sm"
                    "-u jenkins"
                    "-w /tmp/sm"
                    "--name dmxs" . ,docker-image-run-default-args))))

;;; elfeed : rss
(rational-package-install-package 'elfeed)
(with-eval-after-load "elfeed"
    (setq elfeed-feeds
        '(;; General
             ;; "http://feeds.bbci.co.uk/news/rss.xml" ; BBC News
             ;; ycombinator
             "https://news.ycombinator.com/rss"
             ;; VPN
             "https://mullvad.net/blog/feed/atom"
             ;; Emacs
             "https://planet.emacslife.com/atom.xml"
             "http://www.terminally-incoherent.com/blog/feed"
             "http://nullprogram.com/feed"
             "http://fasciism.com/feed.xml"
             "https://protesilaos.com/master.xml"
             "https://jeffbowman.writeas.com/feed/"
             "https://www.masteringemacs.org/feed"
             "https://irreal.org/blog/?feed=rss2"
             ;; Embedded
             "http://www.embeddedrelated.com/blogs_rss.php"
             ;; C++
             "https://isocpp.org/blog/rss"
             "http://arne-mertz.de/feed/"
             "http://herbsutter.com/feed/"
             "http://feeds.feedburner.com/CppSoup"
             "http://feeds.feedburner.com/CppTruths"
             "http://www.drdobbs.com/articles/cpp/rss"
             "http://scottmeyers.blogspot.com/feeds/posts/default"
             "http://bartoszmilewski.com/feed/"
             "https://akrzemi1.wordpress.com/feed/"
             "https://www.fayewilliams.com/feed/"
             "http://feeds.woboq.com/woboq"
             "https://oopscenities.net/feed/"
             "http://articles.emptycrate.com/node/feed.xml"
             "https://tartanllama.github.io/feed.xml"
             "https://marcoarena.wordpress.com/feed/"
             "http://www.nirfriedman.com/atom.xml"
             "http://tristanbrindle.com/feed.xml"
             "http://templated-thoughts.blogspot.com/feeds/posts/default"
             "http://www.fluentcpp.com/feed/"
             "https://pniedzielski.net/feed.xml"
             "https://dvmirchevcpp.blogspot.com/feeds/posts/default"
             "http://szelei.me/atom.xml"
             "https://blog.galowicz.de//feed.xml"
             "https://baptiste-wicht.com/rss.xml"
             "http://feeds.feedburner.com/abseilio"
             "https://mariusbancila.ro/blog/feed/"
             "https://www.computist.xyz/feeds/posts/default?alt=rss"
             "http://www.nuonsoft.com/blog/feed/"
             "http://blog.vorbrodt.me/?feed=rss2"
             "https://levelofindirection.com/main.rss"
             "https://wgml.pl/feed.xml"
             "https://panky-codes.github.io/feed.xml"
             "https://philippegroarke.com//posts/index.xml"
             "https://codingnest.com/rss/"
             "https://cor3ntin.github.io/index.xml"
             "https://bitbashing.io/feed.xml"
             "https://oleksandrkvl.github.io/feed.xml"
             "https://www.sandordargo.com/feed.xml"
             "https://muit.tech/posts/index.xml"
             "https://quuxplusone.github.io/blog/feed.xml"
             "https://brevzin.github.io/feed.xml"
             ;; Golang
             "https://blog.golang.org/feed.atom"
             ;; Rust
             "https://blog.rust-lang.org/feed.xml"
             "https://readrust.net/all/feed.rss"
             "http://www.integer32.com/feed.xml"
             "https://odetorust.com/feed.xml"
             "https://ehsanmkermani.com/feed/"
             "https://www.jameselford.com/rss.xml"
             "https://blog.adamchalmers.com/atom.xml"
             "https://itsallaboutthebit.com/atom.xml"
             ;; Misc
             "http://www.norvig.com/rss-feed.xml"
             "http://eli.thegreenplace.net/feeds/all.atom.xml"
             "https://pniedzielski.net/feed.xml"
             "https://eklitzke.org/index.rss"
             "https://www.murrayc.com/feed/"
             "https://gendignoux.com/blog/feed.xml"
             "https://drewdevault.com/blog/index.xml"
             "https://incolumitas.com/feeds/all.atom.xml"
             "http://www.mycpu.org/feed.xml"
             "https://muit.tech/index.xml"
             "https://blog.codinghorror.com/rss/"
             "https://www.micahcantor.com/atom.xml"
             "https://h2x.sh/atom.xml"
             "https://kerkour.com/feed.xml"
             "https://cliffle.com/rss.xml"))

    (setq elfeed-search-title-min-width 60)
    (setq elfeed-search-title-max-width 100)
    (setq elfeed-search-trailing-width 0)
    (setq elfeed-search-filter "@6-months-ago +unread")
    (setq elfeed-db-directory "~/Dropbox/Apps/elfeed/elfeed_db"))

;;; Some Hooks
(add-hook 'text-mode-hook (lambda () (visual-line-mode)))
(dolist (hook '(special-mode-hook
                   term-mode-hook
                   comint-mode-hook
                   compilation-mode-hook
                   minibuffer-setup-hook))
    (add-hook hook (lambda () (setq show-trailing-whitespace nil))))

;;;; keybindings : https://www.masteringemacs.org/article/mastering-key-bindings-emacs
;;; Global
(define-key global-map (kbd "C-c u") 'browse-url-at-point) ;; browse url
(define-key global-map (kbd "C-x k") 'kill-this-buffer)    ;; kill buffer without prompt
(define-key global-map (kbd "C-x K") 'kill-buffer)         ;; prompt for buffer to kill
(define-key global-map (kbd "C-z") nil)                    ;; suspend-frame does not work well
(define-key global-map (kbd "C-x C-z") nil)                ;; same
(define-key global-map [remap list-buffers] 'ibuffer)      ;; `ibuffer' is better than `list-buffers'
(define-key global-map [remap dabbrev-expand] 'hippie-expand) ;; use `hippie-expand'

;; zap
(when (fboundp 'zap-up-to-char)
    (define-key global-map (kbd "M-S-z") 'zap-up-to-char))

;; fill/unfill
(define-key global-map [remap fill-paragraph] '+my/fill-or-unfill)
(define-key global-map (kbd "M-Q") '+my/unfill-paragraph)
(define-key global-map (kbd "C-M-Q") '+my/unfill-region)
(add-hook 'org-mode-hook
    (lambda ()
        (interactive)
        (define-key org-mode-map [remap fill-paragraph] '+my/org-fill-or-unfill)))

;; windows
(define-key ctl-x-4-map (kbd "t") '+my/toggle-window-split)

;; ace-window
(when (package-installed-p 'ace-window)
    (define-key global-map [remap other-window] 'ace-window))

;; easy-kill
(when (package-installed-p 'easy-kill)
    (define-key global-map [remap kill-ring-save] 'easy-kill))

;; avy
(when (package-installed-p 'avy)
    (define-key global-map (kbd "C-;") 'avy-goto-char-timer))

;; expand-region
(when (package-installed-p 'expand-region)
    (define-key global-map (kbd "C-=") 'er/expand-region))

;; magit
(when (fboundp 'magit-status)
    (define-key global-map (kbd "C-x g") 'magit-status))

;; embark
;; (when (package-installed-p 'embark)
;;     (define-key global-map (kbd "C-.") 'embark-act))

;; crux
(when (package-installed-p 'crux)
    (define-key global-map (kbd "C-o") 'crux-smart-open-line)
    (define-key global-map (kbd "M-o") 'crux-smart-open-line-above)
    (define-key global-map (kbd "C-^") 'crux-top-join-line)
    (define-key global-map (kbd "C-k") 'crux-smart-kill-line)
    (define-key global-map (kbd "C-x C-r") 'crux-recentf-find-file)
    (define-key global-map (kbd "C-x C-u") 'crux-upcase-region)
    (define-key global-map (kbd "C-x C-l") 'crux-downcase-region)
    (define-key global-map [remap kill-whole-line] 'crux-kill-whole-line)
    (define-key global-map [remap move-beginning-of-line] 'crux-move-beginning-of-line))

;; undo-fu
(when (package-installed-p 'undo-fu)
    (define-key global-map (kbd "C-/") 'undo-fu-only-undo)
    (define-key global-map (kbd "C-?") 'undo-fu-only-redo))

;;; Prefix
;; C-c f : File
(define-key global-map (kbd "C-c f s") 'save-buffer)
(define-key global-map (kbd "C-c f S") '+my/save-all)

;; C-c n : Notes
(define-key global-map (kbd "C-c n d") 'deft-find-file)
(define-key global-map (kbd "C-c n D") 'deft)

;; C-c o : Open
(define-key global-map (kbd "C-c o c") 'calc)
(define-key global-map (kbd "C-c o C") 'quick-calk)
(define-key global-map (kbd "C-c o e") 'elfeed)
;; (define-key global-map (kbd "C-c o v") 'vterm)
(when (package-installed-p 'neotree)
    (define-key global-map (kbd "C-c o p") 'neotree-toggle))
(when (package-installed-p 'treemacs)
    (define-key global-map (kbd "C-c o p") 'treemacs))

;;; Modal
;; dired
(add-hook 'dired-mode-hook
    (lambda ()
        (interactive)
        (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
        (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))
        (define-key dired-mode-map (kbd "C-c o") 'crux-open-with)))

;; isearch
(add-hook 'isearch-mode-hook
    (lambda()
        (interactive)
        ;; Prevents issue where you have to press backspace twice when trying to remove the first character that fails a search
        (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)
        ;; Better navigation
        (define-key isearch-mode-map (kbd "C-n") 'isearch-repeat-forward)
        (define-key isearch-mode-map (kbd "C-p") 'isearch-repeat-backward)))

;; completion
(define-key vertico-map (kbd "C-f") 'vertico-exit)
(define-key minibuffer-local-map (kbd "C-.") 'embark-act)
(define-key project-prefix-map (kbd "g") 'consult-ripgrep)

;; smartparens
(when (package-installed-p 'smartparens)
    (define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-backward-kill-sexp))

;; vterm
(when (package-installed-p 'vterm)
    (add-hook 'vterm-mode-hook
        (lambda ()
            (interactive)
            (define-key vterm-mode-map (kbd "C-y") 'vterm-yank))))

;; To not load `custom.el' after `config.el', uncomment this line.
(setq rational-load-custom-file nil)

;;; config.el ends here

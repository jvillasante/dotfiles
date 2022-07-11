;;; lisp/init.el -*- lexical-binding: t; -*-

(setq user-full-name "Julio C. Villasante"
    user-mail-address "jvillasantegomez@gmail.com"
    user-login-name "jvillasante"
    +my/home-path (expand-file-name "~/")
    +my/dotfiles-path (expand-file-name "Workspace/Public/dotfiles/" +my/home-path)
    +my/software-path (expand-file-name "Workspace/Software/" +my/home-path)
    +my/dropbox-path (expand-file-name "Dropbox/" +my/home-path)
    +my/splash-path (expand-file-name "Misc/splash/emacs-logo.png" +my/dotfiles-path))

(cond
    (IS-MAC
        (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
        (setq browse-url-browser-function 'browse-url-generic
            browse-url-generic-program "open")
        (setq +my/clang-path "/usr/local/opt/llvm/bin/clang"
            +my/mu-path "/usr/local/bin/mu"
            +my/msmtp-path "/usr/local/bin/msmtp"
            vterm-module-cmake-args " -DUSE_SYSTEM_LIBVTERM=yes")
        (setq ns-use-proxy-icon       nil
            ns-use-thin-smoothing     t
            ns-alternate-modifier     nil
            mac-command-modifier      'meta
            mac-option-modifier       'alt
            mac-right-option-modifier 'alt))
    (IS-LINUX
        (setq browse-url-browser-function 'browse-url-generic
            browse-url-generic-program "xdg-open")
        (setq +my/clang-path "/usr/bin/clang"
            +my/mu-path "/usr/bin/mu"
            +my/msmtp-path "/usr/bin/msmtp"
            vterm-module-cmake-args " -DUSE_SYSTEM_LIBVTERM=yes")))

;; Start maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Select and raise the frame, always
(select-frame-set-input-focus (selected-frame))

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; org-directory needs to be set early
(setq org-directory (expand-file-name "Apps/org" +my/dropbox-path))

;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)

;; disable risky local variables warning
(advice-add 'risky-local-variable-p :override #'ignore)

;; emacs does not need a pager
(setenv "PAGER" "cat")

;; encryption
(progn
    (require 'epa-file)
    (epa-file-enable)
    (setq epa-file-encrypt-to user-mail-address
        epa-file-select-keys 'silent
        epa-file-cache-passphrase-for-symmetric-encryption nil))

;; Minibuffer setup
(setq-hook! 'minibuffer-setup-hook
    show-trailing-whitespace nil
    line-spacing 1)

;; aspell
(advice-add #'ispell-init-process :around #'doom-shut-up-a)
(setq ispell-dictionary "en_US") ;; default dictionary

;;
;; Some default
;;

(setq-default
    delete-by-moving-to-trash t      ; Delete files to trash
    window-combination-resize t      ; take new window space from all other windows (not just current)
    delete-trailing-lines t          ; `M-x delete-trailing-whitespace' deletes trailing lines
    x-stretch-cursor nil)            ; Stretch cursor to the glyph width

(setq
    undo-limit 80000000             ; Raise undo-limit to 80Mb
    suggest-key-bindings nil        ; very annoying
    auto-save-default t             ; Nobody likes to loose work, I certainly don't
    password-cache-expiry nil       ; I can trust my computers ... can't I?
    major-mode 'fundamental-mode
    use-dialog-box nil
    vc-follow-symlinks t
    load-prefer-newer t
    truncate-lines t                   ; Don't fold lines
    truncate-partial-width-windows nil ; for vertically-split windows
    split-width-threshold 160          ; Split verticaly by default
    scroll-margin 3
    uniquify-buffer-name-style 'forward   ; Uniquify buffer names
    indent-tabs-mode nil                  ; use space to indent by default

    ;; set appearance of a tab that is represented by 4 spaces
    tab-width 4
    standard-indent 4
    highlight-tabs t  ; show those ugly tabs

    ;; Whitespace settings
    show-trailing-whitespace t
    whitespace-action '(auto-cleanup)
    whitespace-style '(indentation::space
                          space-after-tab
                          space-before-tab
                          trailing
                          lines-tail
                          tab-mark
                          face
                          tabs)

    doc-view-continuous t

    ;; LaTeX
    font-latex-fontify-script nil
    TeX-newline-function 'reindent-then-newline-and-indent

    ;; other defaults
    auto-window-vscroll nil
    sp-escape-quotes-after-insert nil

    ;; I do not know what this is :)
    max-specpdl-size 5000
    url-queue-timeout 30)

;; Default Encoding
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-charset-priority 'unicode)
(set-language-environment "UTF-8")
(set-locale-environment "en_US.UTF-8")
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8) ; included by set-selection-coding-system
(set-keyboard-coding-system 'utf-8) ; configured by prefer-coding-system
(set-terminal-coding-system 'utf-8) ; configured by prefer-coding-system
(setq buffer-file-coding-system 'utf-8) ; utf-8
(setq save-buffer-coding-system 'utf-8) ; nil
(setq process-coding-system-alist
    (cons '("grep" utf-8 . utf-8) process-coding-system-alist))

;; write over selected text on input... like all modern editors do
(delete-selection-mode t)

;; no whitespace-mode, just enabled for makefiles
(advice-add #'doom-highlight-non-default-indentation-h :override #'ignore)

;; auto revert mode; automatically update a buffer if a file changes on disk
(global-auto-revert-mode t)

;; remove doom advice, I don't need deal with comments when newline
(advice-remove #'newline-and-indent #'doom*newline-indent-and-continue-comments)

;; https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
(setq world-clock-list
    '(("UTC" "UTC")
         ("America/New_York" "Tampa")
         ("Europe/Ljubljana" "Slovenia")
         ("Asia/Calcutta" "India")
         ("America/Havana" "Havana")))
(setq world-clock-time-format "%a, %d %b %I:%M %p %Z")

;; line spacing
(setq-default line-spacing 0.1)

;; Isearch
(progn
    (setq search-whitespace-regexp ".*?") ;; Isearch convenience, space matches anything (non-greedy)
    (setq isearch-lax-whitespace t)
    (setq isearch-allow-motion t)) ;; Enable Emacs 28 isearch motions

;; Color compilation buffer
(require 'xterm-color)
(setq compilation-environment '("TERM=xterm-256color"))
(defun +my/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
(advice-add 'compilation-filter :around #'+my/advice-compilation-filter)

;; Workaround for terminal buffer scroll
(setq term-char-mode-point-at-process-mark nil)

;; Toggle visualization of matching parens
(setq show-paren-mode 1)

;; filling
(setq-default fill-column 132)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list
    '(yas-hippie-try-expand
         try-expand-dabbrev
         try-expand-dabbrev-all-buffers
         try-expand-dabbrev-from-kill
         try-complete-file-name-partially
         try-complete-file-name
         try-expand-all-abbrevs
         try-expand-list
         try-expand-line
         try-complete-lisp-symbol-partially
         try-complete-lisp-symbol))

;; compilation stuff
(defun +my/bury-compile-buffer (buffer msg)
    "Bury compilation buffer if succeeded without warnings."
    (if (and
            (buffer-live-p buffer)
            (string-match "^finished" msg)
            (not (with-current-buffer buffer
                     (goto-char (point-min))
                     (search-forward "warning" nil t))))
        (progn
            (run-at-time "2 sec" nil 'delete-windows-on buffer)
            (message "Compilation Successful :-)"))
        (message "Compilation Failed :-("))
    (setq current-frame (car (car (cdr (current-frame-configuration)))))
    (select-frame-set-input-focus current-frame))
(add-to-list 'compilation-finish-functions '+my/bury-compile-buffer)

;; Add online search engines for +lookup/online
(add-to-list '+lookup-provider-url-alist '("cppreference" "https://en.cppreference.com/w/?search=%s"))

;; Add system-wide mu4e installation
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

;; Hooks
(add-hook 'phyton-mode-hook #'whitespace-mode)
(add-hook 'makefile-mode-hook #'whitespace-mode)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)    ;; auto-fill insert hard line breaks
(add-hook 'text-mode-hook 'turn-on-visual-line-mode) ;; ... visual-line-mode is much better
(add-hook 'prog-mode-hook '+my/comment-auto-fill)    ;; ... but add comment auto-fill in prog-mode

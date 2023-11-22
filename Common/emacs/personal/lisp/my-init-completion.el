;;; my-init-completion.el -*- lexical-binding: t; -*-

(use-package emacs
    :ensure nil ;; emacs built-in
    :init
    ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
    ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
    (setq read-extended-command-predicate #'command-completion-default-include-p)

    ;; Minibuffer configurations
    (setq completion-styles '(basic substring initials flex orderless)) ; also see `completion-category-overrides'
    (setq completion-category-defaults nil)

    ;; A list of known completion categories:
    ;;
    ;; - `bookmark'
    ;; - `buffer'
    ;; - `charset'
    ;; - `coding-system'
    ;; - `color'
    ;; - `command' (e.g. `M-x')
    ;; - `customize-group'
    ;; - `environment-variable'
    ;; - `expression'
    ;; - `face'
    ;; - `file'
    ;; - `function' (the `describe-function' command bound to `C-h f')
    ;; - `info-menu'
    ;; - `imenu'
    ;; - `input-method'
    ;; - `kill-ring'
    ;; - `library'
    ;; - `minor-mode'
    ;; - `multi-category'
    ;; - `package'
    ;; - `project-file'
    ;; - `symbol' (the `describe-symbol' command bound to `C-h o')
    ;; - `theme'
    ;; - `unicode-name' (the `insert-char' command bound to `C-x 8 RET')
    ;; - `variable' (the `describe-variable' command bound to `C-h v')
    ;;
    ;; From the `consult' package:
    ;;
    ;; - `consult-grep'
    ;; - `consult-isearch'
    ;; - `consult-kmacro'
    ;; - `consult-location'
    ;;
    ;; From the `embark' package:
    ;;
    ;; - `embark-keybinding'
    ;;
    (setq completion-category-overrides
          ;; NOTE 2021-10-25: I am adding `basic' because it works better as a
          ;; default for some contexts.  Read:
          ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=50387>.
          ;;
          ;; `partial-completion' is a killer app for files, because it
          ;; can expand ~/.l/s/fo to ~/.local/share/fonts.
          ;;
          ;; If `basic' cannot match my current input, Emacs tries the
          ;; next completion style in the given order.  In other words,
          ;; `orderless' kicks in as soon as I input a space or one of its
          ;; style dispatcher characters.
          '((file (styles . (basic partial-completion orderless)))
            (bookmark (styles . (basic substring)))
            (library (styles . (basic substring)))
            (embark-keybinding (styles . (basic substring)))
            (imenu (styles . (basic substring orderless)))
            (consult-location (styles . (basic substring orderless)))
            (kill-ring (styles . (emacs22 orderless)))
            (eglot (styles . (emacs22 substring orderless)))))

    ;; TAB cycle if there are only few candidates
    (setq completion-cycle-threshold 3)
    ;; Enable indentation+completion using the TAB key.
    ;; `completion-at-point' is often bound to M-TAB.
    (setq tab-always-indent 'complete)

    (setq completion-ignore-case t)
    (setq read-buffer-completion-ignore-case t)
    (setq read-file-name-completion-ignore-case t)
    (setq-default case-fold-search t)   ; For general regexp

    (setq enable-recursive-minibuffers t)
    ;; Allow Emacs to resize mini windows, otherwise this does not work:
    ;;   (setq org-use-fast-todo-selection 'expert)
    (setq resize-mini-windows t)
    (setq minibuffer-eldef-shorten-default t)

    (setq read-answer-short t) ; also check `use-short-answers' for Emacs28
    (setq echo-keystrokes 0.25)
    (setq kill-ring-max 60) ; Keep it small

    ;; Do not allow the cursor to move inside the minibuffer prompt.  I
    ;; got this from the documentation of Daniel Mendler's Vertico
    ;; package: <https://github.com/minad/vertico>.
    (setq minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

    ;; Add prompt indicator to `completing-read-multiple'.  We display
    ;; [CRM<separator>], e.g., [CRM,] if the separator is a comma.  This
    ;; is copied from the README of the `vertico' package.  I made some
    ;; small tweaks to propertize the segments of the prompt.
    (defun crm-indicator (args)
        (cons (format "[`crm-separator': %s]  %s"
                      (propertize
                       (replace-regexp-in-string
                        "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                        crm-separator)
                       'face 'error)
                      (car args))
              (cdr args)))
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

    (file-name-shadow-mode 1)
    (minibuffer-depth-indicate-mode 1)
    (minibuffer-electric-default-mode 1))

(use-package dabbrev
    :ensure nil ;; emacs built-in
    :init
    ;; `dabbrev' (dynamic word completion (dynamic abbreviations))
    (setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
    (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
    (setq dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
    (setq dabbrev-backward-only nil)
    (setq dabbrev-case-distinction 'case-replace)
    (setq dabbrev-case-fold-search nil)
    (setq dabbrev-case-replace 'case-replace)
    (setq dabbrev-check-other-buffers t)
    (setq dabbrev-eliminate-newlines t)
    (setq dabbrev-upcase-means-case-search t)

    ;; `abbrev' (Abbreviations, else Abbrevs)
    (setq abbrev-file-name (expand-file-name "abbrevs" no-littering-etc-directory))
    (setq only-global-abbrevs nil))

;; hippie expand is dabbrev expand on steroids
(use-package hippie-exp
    ;; A composable expansion tool that I find compliments `corfu' in that it
    ;; looks in a different manner for completions.
    ;;
    ;; TODO: Perhaps I should spend a bit time investigating removing `hippie-exp'
    ;; in favor of `corfu' and `cape' behavior.  Definitely spend a bit of time exploring
    ;; this option.
    :config
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
    :init (global-set-key [remap dabbrev-expand] 'hippie-expand))

(use-package vertico
    :init
    (setq vertico-count 12)
    (setq vertico-resize nil)
    (vertico-mode +1)

    ;; This works with `file-name-shadow-mode' enabled.  When you are in
    ;; a sub-directory and use, say, `find-file' to go to your home '~/'
    ;; or root '/' directory, Vertico will clear the old path to keep
    ;; only your current input.
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
    :bind (:map vertico-map
                ("M-," . #'vertico-quick-insert)
                ("M-." . #'vertico-quick-exit)))

(use-package orderless
    :init
    (setq orderless-component-separator " +")
    ;; Remember to check my `completion-styles' and the `completion-category-overrides'.
    (setq orderless-matching-styles
          '(orderless-prefixes orderless-regexp))

    ;; SPC should never complete: use it for `orderless' groups.
    ;; The `?' is a regexp construct.
    (let ((map minibuffer-local-completion-map))
        (define-key map (kbd "SPC") nil)
        (define-key map (kbd "?") nil)))

(use-package corfu
    :custom ((corfu-auto t)
             (corf-auto-prefix 2)
             (corfu-quit-no-match t)
             (corfu-quit-at-boundary 'separator))  ;; Enable cycling for `corfu-next/previous'
    :init
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-noninterruptible)
    (add-hook 'eshell-mode-hook
              (lambda ()
                  (setq-local corfu-auto nil)
                  (corfu-mode)))
    (global-corfu-mode))

(use-package cape
    :init
    (setq cape-dabbrev-min-length 2)
    (setq cape-symbol-wrapper
          '((org-mode ?~ ?~)
            (markdown-mode ?` ?`)
            (log-edit-mode ?' ?')
            (message-mode ?' ?')))
    (dolist (backend '(cape-symbol cape-keyword cape-file cape-history cape-dabbrev))
        (add-to-list 'completion-at-point-functions backend)))

(use-package consult
    :bind (;; C-c bindings (mode-specific-map)
           ("C-c M-x" . consult-mode-command)
           ("C-c h" . consult-history)
           ("C-c k" . consult-kmacro)
           ("C-c m" . consult-man)
           ("C-c i" . consult-info)
           ([remap Info-search] . consult-info)
           ;; C-x bindings (ctl-x-map)
           ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
           ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
           ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
           ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
           ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
           ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
           ("C-x C-r" . consult-recent-file)         ;; orig. find-file-readonly
           ;; Custom M-# bindings for fast register access
           ("M-#" . consult-register-load)
           ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
           ("C-M-#" . consult-register)
           ;; Other custom bindings
           ("M-y" . consult-yank-pop)                ;; orig. yank-pop
           ;; M-g bindings (goto-map)
           ("M-g e" . consult-compile-error)
           ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
           ("M-g g" . consult-goto-line)             ;; orig. goto-line
           ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
           ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
           ("M-g m" . consult-mark)
           ("M-g k" . consult-global-mark)
           ("M-g i" . consult-imenu)
           ("M-g I" . consult-imenu-multi)
           ;; M-s bindings (search-map)
           ("M-s d" . consult-find)
           ("M-s D" . consult-locate)
           ("M-s g" . consult-grep)
           ("M-s G" . consult-git-grep)
           ("M-s r" . consult-ripgrep)
           ("M-s l" . consult-line)
           ("M-s L" . consult-line-multi)
           ("M-s k" . consult-keep-lines)
           ("M-s u" . consult-focus-lines)
           ;; Isearch integration
           ("M-s e" . consult-isearch-history)
           :map isearch-mode-map
           ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
           ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
           ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
           ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
           ;; Minibuffer history
           :map minibuffer-local-map
           ("M-s" . consult-history)                 ;; orig. next-matching-history-element
           ("M-r" . consult-history))                ;; orig. previous-matching-history-element
    :init
    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)

    ;; Optionally configure the register formatting. This improves the register
    ;; preview for `consult-register', `consult-register-load',
    ;; `consult-register-store' and the Emacs built-ins.
    (setq register-preview-delay 0.5
          register-preview-function #'consult-register-format)

    ;; Optionally tweak the register preview window.
    ;; This adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window)

    (setq consult-line-numbers-widen t)
    ;; (setq completion-in-region-function #'consult-completion-in-region)
    (setq consult-async-min-input 3)
    (setq consult-async-input-debounce 0.5)
    (setq consult-async-input-throttle 0.8)
    (setq consult-narrow-key nil)
    (setq consult-preview-key 'any)
    (setq consult-find-args
          (concat "find . -not ( "
                  "-path */.git* -prune "
                  "-or -path */.cache* -prune )"))
    ;; (add-to-list 'consult-mode-histories '(vc-git-log-edit-mode . log-edit-comment-ring))
    (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)
    (require 'consult-imenu))

(use-package consult-dir
    :bind (([remap list-directory] . consult-dir)
           :map vertico-map
           ("C-c C-d" . 'consult-dir)
           ("C-c C-j" . 'consult-dir-jump-file)))

(use-package embark
    :bind (("C-;" . embark-act)            ;; pick some comfortable binding
           ("M-;" . embark-dwim)        ;; good alternative: C-M-;
           ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

    :init
    ;; Optionally replace the key help with a completing-read interface
    (setq prefix-help-command #'embark-prefix-help-command)

    ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
    ;; strategy, if you want to see the documentation from multiple providers.
    ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
    ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

    :config
    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
    :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
    ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
    ;; available in the *Completions* buffer, add it to the
    ;; `completion-list-mode-map'.
    :bind (:map minibuffer-local-map
                ("M-A" . marginalia-cycle))
    :init (marginalia-mode))

;; jinx : Enchanted Spell Checker
(use-package jinx
    :hook (emacs-startup . global-jinx-mode)
    :bind (("M-$" . jinx-correct)
           ("C-M-$" . jinx-languages)))

;; tempel - Simple templates for Emacs
(use-package tempel-collection :disabled t :after tempel)
(use-package tempel
    :disabled t
    :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
           ("M-*" . tempel-insert))
    :init
    (setq tempel-path (expand-file-name "tempel-templates" no-littering-etc-directory))

    ;; Setup completion at point
    (defun tempel-setup-capf ()
        ;; Add the Tempel Capf to `completion-at-point-functions'.
        ;; `tempel-expand' only triggers on exact matches. Alternatively use
        ;; `tempel-complete' if you want to see all matches, but then you
        ;; should also configure `tempel-trigger-prefix', such that Tempel
        ;; does not trigger too often when you don't expect it. NOTE: We add
        ;; `tempel-expand' *before* the main programming mode Capf, such
        ;; that it will be tried first.
        (setq-local completion-at-point-functions
                    (cons #'tempel-expand
                          completion-at-point-functions)))

    (add-hook 'conf-mode-hook 'tempel-setup-capf)
    (add-hook 'prog-mode-hook 'tempel-setup-capf)
    (add-hook 'text-mode-hook 'tempel-setup-capf)

    ;; Optionally make the Tempel templates available to Abbrev,
    ;; either locally or globally. `expand-abbrev' is bound to C-x '.
    ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
    ;; (global-tempel-abbrev-mode)
    )

;; yasnippet : template system for Emacs
(use-package yasnippet-snippets :after yasnippet)
(use-package yasnippet
    :bind (("M-+" . yas-expand)
           ("M-*" . yas-insert-snippet))
    :config (yas-reload-all)
    :hook (after-init . yas-global-mode))

;; languagetool : multilingual grammar, style, and spell checker
(use-package langtool
    :defer t
    :bind (("C-x 4 w" . langtool-check)
           ("C-x 4 W" . langtool-check-done)
           ("C-x 4 c" . langtool-interactive-correction)
           ("C-x 4 l" . langtool-switch-default-language)
           ("C-x 4 4" . langtool-show-message-at-point))
    :config
    (setq langtool-default-language "en-US")
    (setq langtool-java-user-arguments '("-Dfile.encoding=UTF-8"))
    (setq langtool-language-tool-jar
          (expand-file-name "LanguageTool-6.2/languagetool-commandline.jar" my--software-path))
    (setq langtool-language-tool-server-jar
          (expand-file-name "LanguageTool-6.2/languagetool-server.jar" my--software-path)))

(provide 'my-init-completion)
;;; my-init-completion.el ends here

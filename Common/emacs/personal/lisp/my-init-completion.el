;;; my-init-completion.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

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
    (vertico-mode 1)
    (vertico-multiform-mode 1)
    :config
    (setq vertico-count 13)
    (setq vertico-resize nil)
    (setq vertico-multiform-categories
          '((t reverse)))
    (setq vertico-multiform-commands
          '((consult-ripgrep buffer)
            (consult-project-buffer buffer)
            ;; (consult-line buffer)
            ;; (consult-line-thing-at-point buffer)
            ;; (consult-recent-file buffer)
            ;; (consult-mode-command buffer)
            ;; (consult-complex-command buffer)
            ;; (embark-bindings buffer)
            ;; (consult-locate buffer)
            ;; (consult-fd buffer)
            ))

    ;; This works with `file-name-shadow-mode' enabled.  When you are in
    ;; a sub-directory and use, say, `find-file' to go to your home '~/'
    ;; or root '/' directory, Vertico will clear the old path to keep
    ;; only your current input.
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

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
    :custom
    ((corfu-auto t)
     (corf-auto-prefix 2)
     ;; (corfu-auto-delay 0.0)
     ;; (corfu-popupinfo-delay '(0.5 . 0.2))
     ;; (corfu-preview-current 'insert) ; Do not preview current candidate
     ;; (corfu-preselect 'prompt)
     ;; (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
     (corfu-quit-no-match t)
     (corfu-quit-at-boundary 'separator))  ;; Enable cycling for `corfu-next/previous'
    :init
    (add-hook 'eshell-mode-hook
              (lambda ()
                  (setq-local corfu-auto nil)
                  (corfu-mode)))
    (global-corfu-mode)
    (corfu-history-mode)
    (corfu-popupinfo-mode))

(use-package cape
    :init
    ;; eglot integration with cape
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-noninterruptible)

    ;; Add to the global default value of `completion-at-point-functions' which is
    ;; used by `completion-at-point'.  The order of the functions matters, the
    ;; first function returning a result wins.  Note that the list of buffer-local
    ;; completion functions takes precedence over the global list.
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-elisp-block)
    ;;(add-to-list 'completion-at-point-functions #'cape-history)
    ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
    ;;(add-to-list 'completion-at-point-functions #'cape-tex)
    ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
    ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
    ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
    ;;(add-to-list 'completion-at-point-functions #'cape-dict)
    ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
    ;;(add-to-list 'completion-at-point-functions #'cape-line)
    )

(use-package consult
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
    :after consult)

(use-package consult-eglot
    :after (consult eglot))

(use-package embark
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
    :init (marginalia-mode))

;; jinx : Enchanted Spell Checker
(use-package jinx
    :hook (emacs-startup . global-jinx-mode))

;; tempel - Simple templates for Emacs
(use-package tempel-collection :disabled t :after tempel)
(use-package tempel
    :disabled t
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
    :config
    (let ((snippets-shim (expand-file-name "yasnippet-treesitter-shim/snippets/" no-littering-etc-directory)))
        (when (file-directory-p snippets-shim)
            (add-to-list 'yas-snippet-dirs snippets-shim)))
    (yas-reload-all)
    :hook (after-init . yas-global-mode))

;; languagetool : multilingual grammar, style, and spell checker
(use-package langtool
    :defer t
    :config
    (setq langtool-default-language "en-US")
    (setq langtool-java-user-arguments '("-Dfile.encoding=UTF-8"))
    (setq langtool-language-tool-jar
          (expand-file-name "LanguageTool-6.3/languagetool-commandline.jar" my--software-path))
    (setq langtool-language-tool-server-jar
          (expand-file-name "LanguageTool-6.3/languagetool-server.jar" my--software-path)))

(provide 'my-init-completion)
;;; my-init-completion.el ends here

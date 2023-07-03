;;; my-init-completion.el -*- lexical-binding: t; -*-

(use-package emacs
    :ensure nil ;; emacs built-in
    :init
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
    (setq abbrev-file-name (locate-user-emacs-file "abbrevs"))
    (setq only-global-abbrevs nil)

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
             try-complete-lisp-symbol)))

(use-package vertico
    :init
    ;; Those are the default values, but check the user option
    ;; `vertico-multiform-categories' for per-category tweaks.  That
    ;; variable is in the file vertico-multiform.el and will work once
    ;; `vertico-multiform-mode' is enabled.
    (setq vertico-scroll-margin 0)
    (setq vertico-count 14)
    (setq vertico-resize nil)
    (setq vertico-cycle t)
    (vertico-mode 1)

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
    ;; Remember to check my `completion-styles' and the
    ;; `completion-category-overrides'.
    (setq orderless-matching-styles
        '(orderless-prefixes orderless-regexp))

    ;; SPC should never complete: use it for `orderless' groups.
    ;; The `?' is a regexp construct.
    (let ((map minibuffer-local-completion-map))
        (define-key map (kbd "SPC") nil)
        (define-key map (kbd "?") nil)))

(use-package corfu
    :config
    (setq corfu-auto t
        ;; corfu-auto-delay 0
        ;; corfu-auto-prefix 0
        ;; corfu-quit-no-match 'separator
        )
    :init
    (global-corfu-mode 1)
    (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'
    ;;(define-key corfu-map (kbd "<tab>") #'corfu-complete)

    ;; Adapted from Corfu's manual.
    (defun contrib/corfu-enable-always-in-minibuffer ()
        "Enable Corfu in the minibuffer if MCT or Vertico is not active.
Useful for prompts such as `eval-expression' and `shell-command'."
        (unless (or (bound-and-true-p vertico--input)
                    (bound-and-true-p mct--active))
            (corfu-mode 1)))
    (add-hook 'minibuffer-setup-hook #'contrib/corfu-enable-always-in-minibuffer 1))

(use-package cape
    :init
    (setq cape-dabbrev-min-length 3)
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
    (setq consult-line-numbers-widen t)
    ;; (setq completion-in-region-function #'consult-completion-in-region)
    (setq consult-async-min-input 3)
    (setq consult-async-input-debounce 0.5)
    (setq consult-async-input-throttle 0.8)
    (setq consult-narrow-key nil)
    (setq register-preview-delay 0.8
        register-preview-function #'consult-register-format)
    (setq consult-find-args
        (concat "find . -not ( "
                "-path */.git* -prune "
                "-or -path */.cache* -prune )"))
    (setq consult-preview-key 'any)

    ;; (add-to-list 'consult-mode-histories '(vc-git-log-edit-mode . log-edit-comment-ring))
    (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)
    (require 'consult-imenu))

(use-package consult-dir
    :bind (([remap list-directory] . consult-dir)
              :map vertico-map
              ("C-c C-d" . 'consult-dir)
              ("C-c C-j" . 'consult-dir-jump-file))
    :config
    ;; TODO: doomemacs configures docker paths for consult dir
    ;; when docker-tramp is configured, will take a reference from it.
    (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-local t)
    (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t))

(use-package embark
    :init
    (setq which-key-use-C-h-commands nil
        prefix-help-command #'embark-prefix-help-command)
    ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
    :config
    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
        '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
             nil
             (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
    :ensure t ; only need to install it, embark loads it after consult if found
    :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package all-the-icons-completion)
(use-package marginalia
    :init
    (add-hook 'pre-command-hook 'marginalia-mode)
    :config
    (if (display-graphic-p)
        (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)))

;; Configure Tempel
(use-package tempel
    :disabled t
    ;; Require trigger prefix before template name when completing.
    ;; :custom
    ;; (tempel-trigger-prefix "<")

    :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
              ("M-*" . tempel-insert))

    :init
    (setq tempel-path (expand-file-name "templates" no-littering-etc-directory))

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

    (add-hook 'prog-mode-hook 'tempel-setup-capf)
    (add-hook 'text-mode-hook 'tempel-setup-capf)

    ;; Optionally make the Tempel templates available to Abbrev,
    ;; either locally or globally. `expand-abbrev' is bound to C-x '.
    ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
    ;; (global-tempel-abbrev-mode)
    )

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection
    :disabled t)

(use-package yasnippet-snippets)
(use-package yasnippet
    :config (yas-reload-all)
    :hook (after-init . yas-global-mode))

(provide 'my-init-completion)
;;; my-init-completion.el ends here

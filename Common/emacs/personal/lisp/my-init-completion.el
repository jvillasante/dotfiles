;;; my-init-completion.el -*- lexical-binding: t; -*-

(use-package emacs
    :ensure nil ;; emacs built-in
    :preface
    ;; Add prompt indicator to `completing-read-multiple'.
    ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
    (defun crm-indicator (args)
        (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                      "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                      crm-separator)
                  (car args))
            (cdr args)))
    :init
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

    ;; Do not allow the cursor in the minibuffer prompt
    (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

    ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
    ;; Vertico commands are hidden in normal buffers.
    ;; (setq read-extended-command-predicate
    ;;       #'command-completion-default-include-p)

    ;; Enable recursive minibuffers
    (setq enable-recursive-minibuffers t))

(use-package vertico
    :init
    (vertico-mode)
    (setq vertico-resize nil
        vertico-count 13
        vertico-cycle t)

    ;; Use `consult-completion-in-region' if Vertico is enabled.
    ;; Otherwise use the default `completion--in-region' function.
    (setq completion-in-region-function
        (lambda (&rest args)
            (apply (if vertico-mode
                       #'consult-completion-in-region
                       #'completion--in-region)
                args)))

    ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
    ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
    (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

(use-package orderless
    :init
    (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package all-the-icons-completion)
(use-package marginalia
    :init
    (add-hook 'pre-command-hook 'marginalia-mode)
    :config
    (if (display-graphic-p)
        (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)))

;; Example configuration for Consult
(use-package consult
    ;; Replace bindings. Lazily loaded due by `use-package'.
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

    ;; Enable automatic preview at point in the *Completions* buffer. This is
    ;; relevant when you use the default completion UI.
    :hook (completion-list-mode . consult-preview-at-point-mode)

    ;; The :init configuration is always executed (Not lazy)
    :init

    ;; Optionally configure the register formatting. This improves the register
    ;; preview for `consult-register', `consult-register-load',
    ;; `consult-register-store' and the Emacs built-ins.
    (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

    ;; Optionally tweak the register preview window.
    ;; This adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window)

    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

    ;; Configure other variables and modes in the :config section,
    ;; after lazily loading the package.
    :config

    ;; Optionally configure preview. The default value
    ;; is 'any, such that any key triggers the preview.
    ;; (setq consult-preview-key 'any)
    ;; (setq consult-preview-key "M-.")
    ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
    ;; For some commands and buffer sources it is useful to configure the
    ;; :preview-key on a per-command basis using the `consult-customize' macro.
    (consult-customize
        consult-theme :preview-key '(:debounce 0.2 any)
        consult-ripgrep consult-git-grep consult-grep
        consult-bookmark consult-recent-file consult-xref
        consult--source-bookmark consult--source-file-register
        consult--source-recent-file consult--source-project-recent-file
        ;; :preview-key "M-."
        :preview-key '(:debounce 0.4 any))

    ;; Optionally configure the narrowing key.
    ;; Both < and C-+ work reasonably well.
    (setq consult-narrow-key "<") ;; "C-+"

    ;; Optionally make narrowing help available in the minibuffer.
    ;; You may want to use `embark-prefix-help-command' or which-key instead.
    ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

    ;; By default `consult-project-function' uses `project-root' from project.el.
    ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
    ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
    ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
    ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
    ;; (autoload 'projectile-project-root "projectile")
    ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
    ;; (setq consult-project-function nil)
    )

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

(use-package corfu
    :preface
    (defun corfu-send-shell (&rest _)
        "Send completion candidate when inside comint/eshell."
        (cond
            ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
                (eshell-send-input))
            ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
                (comint-send-input))))
    (defun corfu-move-to-minibuffer ()
        (interactive)
        (let ((completion-extra-properties corfu--extra)
                 completion-cycle-threshold completion-cycling)
            (apply #'consult-completion-in-region completion-in-region--data)))
    :config
    (setq corfu-auto t
        ;; corfu-auto-delay 0.0
        corfu-quit-no-match 'separator)
    :init
    (global-corfu-mode)
    (global-set-key (kbd "M-i") #'completion-at-point)
    (keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)
    (add-hook 'eshell-mode-hook
        (lambda ()
            (setq-local corfu-auto nil)
            (corfu-mode)))
    (advice-add #'corfu-insert :after #'corfu-send-shell)
    (when (< emacs-major-version 29)
        ;; Silence the pcomplete capf, no errors or messages!
        (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

        ;; Ensure that pcomplete does not write to the buffer
        ;; and behaves as a pure `completion-at-point-function'.
        (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)))

(use-package corfu-terminal
    :after corfu
    :init
    (unless (display-graphic-p)
        (corfu-terminal-mode +1)))

(use-package cape
    ;; Bind dedicated completion commands
    ;; Alternative prefix keys: C-c p, M-p, M-+, ...
    :bind (("C-c p p" . completion-at-point) ;; capf
              ("C-c p t" . complete-tag)        ;; etags
              ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
              ("C-c p h" . cape-history)
              ("C-c p f" . cape-file)
              ("C-c p k" . cape-keyword)
              ("C-c p s" . cape-symbol)
              ("C-c p a" . cape-abbrev)
              ("C-c p i" . cape-ispell)
              ("C-c p l" . cape-line)
              ("C-c p w" . cape-dict)
              ("C-c p \\" . cape-tex)
              ("C-c p _" . cape-tex)
              ("C-c p ^" . cape-tex)
              ("C-c p &" . cape-sgml)
              ("C-c p r" . cape-rfc1345))
    :init
    ;; Add `completion-at-point-functions', used by `completion-at-point'.
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)
    ;;(add-to-list 'completion-at-point-functions #'cape-history)
    ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
    ;;(add-to-list 'completion-at-point-functions #'cape-tex)
    ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
    ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
    ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
    ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
    ;;(add-to-list 'completion-at-point-functions #'cape-dict)
    ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
    ;;(add-to-list 'completion-at-point-functions #'cape-line)
    )

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

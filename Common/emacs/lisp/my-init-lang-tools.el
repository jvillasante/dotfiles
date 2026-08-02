;;; my-init-lang-tools.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; needed somehow
;; (use-package request)

;; hideshow : code folding
(use-package hideshow
    :ensure nil ;; emacs built-in
    :bind (("C-c TAB" . hs-toggle-hiding)
              ("C-\\" . hs-toggle-hiding)
              ("M-+" . hs-show-all))
    :hook (prog-mode . hs-minor-mode)
    :custom
    (hs-indicator-type nil)
    (hs-show-indicators nil)
    (hs-display-lines-hidden t))

;; subword-mode: treats CamelCase as distinct words
(use-package subword
    :ensure nil ;; emacs built-in
    :hook (after-init . global-subword-mode))

;; eldoc
(use-package eldoc
    :ensure nil ;; emacs built-in
    :hook (after-init . (lambda ()
                            (eldoc-add-command #'xref-find-definitions)
                            (eldoc-add-command #'xref-go-back)
                            (eldoc-add-command #'avy-goto-char-timer)
                            (global-eldoc-mode)))
    :bind (("C-h ." . eldoc))
    :custom
    (eldoc-help-at-pt t)
    (eldoc-echo-area-prefer-doc-buffer t)
    (eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
    (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))

;; flymake
(use-package flymake
    :ensure nil ;; emacs built-in
    :hook ((prog-mode . (lambda ()
                            (which-function-mode)
                            (flymake-mode))))
    :bind (:map prog-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error))
    :custom
    (flymake-suppress-zero-counters t)
    (flymake-no-changes-timeout 3)) ;; Don't be so hasty in syntax checking.

;; flymake-proselint : use proselint with Emacs built-in Flymake
(use-package flymake-proselint
    :disabled t
    :hook (text-mode . (lambda ()
                           (flymake-mode)
                           (flymake-proselint-setup))))

;; Elisp packaging requirements
(use-package package-lint-flymake
    :hook (flymake-diagnostic-functions . package-lint-flymake))

(defvar-local my/format-on-save t
    "When non-nil, format buffer on save via eglot.")

(use-package eglot
    :ensure nil ;; emacs built-in
    :defer t
    :preface
    (defun my/vertico-eglot-commands ()
        "Display Eglot commands, leveraging native Marginalia annotations."
        (interactive)
        (let ((cmds '()))
            ;; Extract the raw commands from the menu
            (map-keymap
                (lambda (_key binding)
                    (when (and (consp binding) (eq (car binding) 'menu-item))
                        (let ((cmd (nth 2 binding)))
                            (when (commandp cmd)
                                (push (symbol-name cmd) cmds)))))
                eglot-menu)

            ;; Tag the completion with the 'command category so Marginalia annotates it
            (let* ((completion-extra-properties '(:category command))
                      (selection (completing-read "Eglot: " (nreverse cmds) nil t)))
                (call-interactively (intern selection)))))
    :hook ((eglot-managed-mode .
                  (lambda ()
                      ;; Show flymake diagnostics first.
                      (setq eldoc-documentation-functions
                          (cons #'flymake-eldoc-function
                              (remove #'flymake-eldoc-function eldoc-documentation-functions)))

                      ;; Show all eldoc feedback.
                      (setq eldoc-documentation-strategy #'eldoc-documentation-compose)

                      ;; Format on save via LSP.
                      (add-hook 'before-save-hook
                          (lambda ()
                              (when my/format-on-save
                                  (eglot-format-buffer)))
                          nil t))))
    :bind (("C-c l l" . eglot)
              ("C-c l Q" . eglot-shutdown-all)
              :map eglot-mode-map
              ("C-c l q" . eglot-shutdown)
              ("C-c l R" . eglot-reconnect)
              ("C-c l r" . eglot-rename)
              ("C-c l d" . eglot-find-declaration)
              ("C-c l ." . eglot-find-typeDefinition)
              ("C-c l i" . eglot-find-implementation)
              ("C-c l a" . eglot-code-actions)
              ("C-c l x" . eglot-code-action-quickfix)
              ("C-c l m" . my/vertico-eglot-commands)
              ("C-c l o" . eglot-code-action-organize-imports)
              ("C-c l =" . eglot-format-buffer)
              ("C-c l f" . eglot-format))
    :config
    (setf (plist-get eglot-events-buffer-config :size) 0)
    (fset #'jsonrpc--log-event #'ignore)
    (setq jsonrpc-event-hook nil)
    (setq eglot-autoshutdown t)
    (setq eglot-autoreconnect nil)
    (setq eglot-extend-to-xref t)
    (setq eglot-sync-connect 3)
    (setq eglot-report-progress t)
    (setq eglot-confirm-server-edits '((eglot-rename . nil)
                                          (t . diff)))
    (setq eglot-ignored-server-capabilities
        '(:documentOnTypeFormattingProvider
             :documentLinkProvider
             :inlayHintProvider
             :colorProvider
             :foldingRangeProvider))

    ;; Cleans up Emacs 31 visual noise
    (setopt eglot-code-action-indications nil)

    ;; Setting the workspace configuration for every buffer, this can also be
    ;; done as dir-local variables for project/directory.
    (setq-default eglot-workspace-configuration
        '(:gopls (:staticcheck t :usePlaceholders t)
             :haskell (:formattingProvider "ormolu")
             :typescript (:format (:baseIndentSize 0
                                      :convertTabsToSpaces t
                                      :indentSize 2
                                      :semicolons "remove"
                                      :tabSize 2))
             :javascript (:format (:baseIndentSize 0
                                      :convertTabsToSpaces t
                                      :indentSize 2
                                      :semicolons "remove"
                                      :tabSize 2))
             :rust-analyzer (:check (:command "clippy")
                                :cargo (:sysroot "discover"
                                           :features "all"
                                           :buildScripts (:enable t))
                                :diagnostics (:disabled ["macro-error"])
                                :procMacro (:enable t))))

    ;; don't try to manage these
    (add-to-list 'eglot-stay-out-of 'eldoc-documentation-strategy)
    (add-to-list 'eglot-stay-out-of 'imenu)
    ;; (add-to-list 'eglot-stay-out-of 'flymake)
    ;; (add-to-list 'eglot-stay-out-of 'yasnippet)

    ;; Writing
    (add-to-list 'eglot-server-programs
        '(text-mode . ("harper-ls" "--stdio")))
    (add-to-list 'eglot-server-programs
        '(markdown-mode . ("harper-ls" "--stdio")))
    (add-to-list 'eglot-server-programs
        '(org-mode . ("harper-ls" "--stdio")))
    (add-to-list 'eglot-server-programs
        '(org-msg-mode . ("harper-ls" "--stdio")))

    ;; TypeScript / JavaScript
    (add-to-list 'eglot-server-programs
        '((typescript-ts-mode tsx-ts-mode js-ts-mode)
             . ("typescript-language-server" "--stdio")))

    ;; C++
    (add-to-list 'eglot-server-programs
        '((c++-ts-mode c-ts-mode c++-mode c-mode objc-mode)
             . ("clangd"
                   "-j=8"
                   "--enable-config"
                   "--query-driver=/**/*"
                   "--log=error"
                   "--malloc-trim"
                   "--background-index"
                   "--clang-tidy"
                   "--all-scopes-completion"
                   "--completion-style=detailed"
                   "--pch-storage=memory"
                   "--header-insertion=never"
                   "--header-insertion-decorators=0"))))

;; consult-eglot : A consulting-read interface for eglot
(use-package consult-eglot
  :after (consult eglot)
  :bind (:map eglot-mode-map
              ("C-c l s" . consult-eglot-symbols))
  :custom
  (consult-eglot-sort-results t)     ; Pre-sort search results by their LSP matching score
  (consult-eglot-ignore-column nil)) ; Jump to the exact symbol column, not just the start of the line

;; consult-eglot-embark : Support for embark exports and action overrides to consult-eglot searches
(use-package consult-eglot-embark
  :after (consult-eglot embark)
  :config
  (consult-eglot-embark-mode 1))

;; eglot-inactive-regions : Eglot extension to visually style inactive pre-processor branches
(use-package eglot-inactive-regions
    :disabled t
    :custom
    (eglot-inactive-regions-style 'darken-foreground)
    (eglot-inactive-regions-opacity 0.4)
    :config
    (eglot-inactive-regions-mode 1))

;; geiser : hacking scheme in emacs
(use-package geiser
    :defer t)

;; geiser support for guile
(use-package geiser-guile
    :after geiser
    :custom (geiser-guile-binary "guile3.0"))

;; dape : Debug Adapter Protocol for Emacs
(use-package dape :defer t)

(use-package compile
    :ensure nil ; Emacs built in
    :defer t
    :custom
    (compilation-scroll-output 'first-error) ; Scroll until the first error appears
    (compilation-skip-threshold 1)           ; Skip info when navigating with next-error
    (compilation-always-kill t)
    (compilation-finish-functions            ; Auto-close the compilation window on success
        (list (lambda (buf status)
                  (when (string-match-p "finished" status)
                      (run-at-time 1 nil #'delete-windows-on buf))))))

(use-package fancy-compilation
    :disabled t
    :defer t
    :init
    (with-eval-after-load 'compile
        (fancy-compilation-mode))
    :custom
    (fancy-compilation-override-colors nil))

;; rmsbolt : compiler-explorer for Emacs
(use-package rmsbolt
    :defer t
    :custom
    (rmsbolt-automatic-recompile nil))

(provide 'my-init-lang-tools)
;;; my-init-lang-tools.el ends here

;;; my-init-lang-tools.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; needed somehow
;; (use-package request)

;; hideshow : code folding
(use-package hideshow
    :ensure nil ;; emacs built-in
    :hook (prog-mode . hs-minor-mode))

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

(use-package eglot
    :ensure nil ;; emacs built-in
    :defer t
    :hook ((eglot-managed-mode . (lambda ()
                                     ;; Show flymake diagnostics first.
                                     (setq eldoc-documentation-functions
                                           (cons #'flymake-eldoc-function
                                                 (remove #'flymake-eldoc-function eldoc-documentation-functions)))

                                     ;; Show all eldoc feedback.
                                     (setq eldoc-documentation-strategy #'eldoc-documentation-compose))))
    :bind (:map c-ts-base-mode-map
                ("C-x C-o" . my-eglot-clangd-find-other-file))
    :config
    (setf (plist-get eglot-events-buffer-config :size) 0)
    (fset #'jsonrpc--log-event #'ignore)
    (setq jsonrpc-event-hook nil)
    (setq eglot-autoshutdown t)
    (setq eglot-autoreconnect nil)
    (setq eglot-extend-to-xref nil)
    (setq eglot-sync-connect nil)
    (setq eglot-report-progress t)
    (setq eglot-confirm-server-edits '((eglot-rename . nil)
                                       (t . diff)))
    (setq eglot-ignored-server-capabilities
          '(;; :hoverProvider
            ;; :documentHighlightProvider
            ;; :documentFormattingProvider
            ;; :documentRangeFormattingProvider
            :documentOnTypeFormattingProvider
            :documentLinkProvider
            :colorProvider
            :foldingRangeProvider
            :inlayHintProvider))

    ;; Setting the workspace configuration for every buffer, this can also be
    ;; done as dir-local variables for project/directory.
    (setq-default eglot-workspace-configuration
                  '(
                    :gopls (:staticcheck t :usePlaceholders t)
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

    ;; C++
    (add-to-list 'eglot-server-programs
                 '((c-ts-mode c++-ts-mode c-mode c++-mode)
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

;; eglot-inactive-regions : Eglot extension to visually style inactive pre-processor branches
(use-package eglot-inactive-regions
    :disabled t
    :custom
    (eglot-inactive-regions-style 'darken-foreground)
    (eglot-inactive-regions-opacity 0.4)
    :config
    (eglot-inactive-regions-mode 1))

(use-package lsp-mode
    :disabled t
    :commands lsp
    :hook
    (c++-ts-mode . lsp)
    (lsp-mode . (lambda ()
                    (my-lsp-eldoc)
                    (lsp-enable-which-key-integration)))
    :custom
    (lsp-log-io nil)
    (lsp-idle-delay 0.1)                      ; clangd is fast
    (lsp-lens-enable nil)
    (lsp-auto-guess-root t)                   ; Detect project root
    (lsp-keymap-prefix "C-c c")
    (lsp-enable-indentation nil)              ; no formatting (use `apheleia')
    (lsp-enable-symbol-highlighting nil)
    (lsp-headerline-breadcrumb-enable nil)
    (lsp-modeline-code-actions-enable nil)
    (lsp-modeline-diagnostics-enable nil)
    (lsp-clients-clangd-args
     '("-j=8"
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
       "--header-insertion-decorators=0")))

;; optionally
(use-package lsp-ui
    :disabled t
    :commands lsp-ui-mode
    :custom
    (lsp-ui-doc-enable nil)
    (lsp-ui-sideline-enable nil))

(use-package lsp-bridge
    :disabled t
    :vc (:url "git@github.com:manateelazycat/lsp-bridge.git"
              :rev :newest)
    :hook (after-init . global-lsp-bridge-mode))

;; geiser : hacking scheme in emacs
(use-package geiser
    :defer t)

;; geiser support for guile
(use-package geiser-guile
    :defer t
    :custom (geiser-guile-binary "guile3.0"))

;; dape : Debug Adapter Protocol for Emacs
(use-package dape :defer t)

;; apheleia : Good code is automatically formatted
(use-package apheleia
    :preface
    (defun my-disable-apheleia ()
        (apheleia-mode -1))
    :hook
    ((prog-mode . apheleia-mode)
     (web-mode . my-disable-apheleia))
    :config
    ;; Set custom formatting commands
    (dolist (formatter-cmd '((shfmt    . ("shfmt" "-i" "4" "-ci" "-kp" "-sr"))
                             (zigfmt   . ("zig" "fmt" "--stdin"))
                             (fourmolu . ("fourmolu" "--indentation" "2" "--stdin-input-file"
                                          (or (buffer-file-name) (buffer-name))))))
        (add-to-list #'apheleia-formatters formatter-cmd))

    ;; Set custom formatters for modes
    (dolist (formatter-mode '((emacs-lisp-mode . lisp-indent)
                              (clojure-mode . lisp-indent)
                              (zig-mode     . zigfmt)
                              (haskell-mode . fourmolu)))
        (add-to-list #'apheleia-mode-alist formatter-mode)))

(use-package compile
    :ensure nil ; Emacs built in
    :defer t
    :custom
    (compilation-always-kill t))

(use-package fancy-compilation
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

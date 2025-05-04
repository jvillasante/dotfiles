;;; my-init-lang-tools.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; needed somehow
(use-package request)

;; hideshow : code folding
(use-package hideshow
    :ensure nil ;; emacs built-in
    :hook (prog-mode . hs-minor-mode))

;; subword-mode
(use-package subword
    :ensure nil ;; emacs built-in
    :hook ((prog-mode . subword-mode)))

;; flymake
(use-package flymake
    :ensure nil ;; emacs built-in
    :hook ((prog-mode . (lambda ()
                            (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
                            (which-function-mode)
                            (flymake-mode))))
    :custom
    (flymake-suppress-zero-counters t)
    (flymake-no-changes-timeout 3)) ;; Don't be so hasty in syntax checking.

;; flymake-proselint : use proselint with Emacs built-in Flymake
(use-package flymake-proselint
    :disabled t
    :after flymake
    :hook (text-mode . (lambda ()
                           (flymake-mode)
                           (flymake-proselint-setup))))

(use-package eldoc
    :ensure nil ;; emacs built-in
    :hook (after-init . (lambda ()
                            (eldoc-add-command #'xref-find-definitions)
                            (eldoc-add-command #'xref-go-back)
                            (eldoc-add-command #'avy-goto-char-timer)
                            (global-eldoc-mode)))
    :custom
    (eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
    (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))

(use-package eglot
    :ensure nil ;; emacs built-in
    :preface
    (defun my/eglot-eldoc ()
        "Show flymake diagnostics first."
        (setq eldoc-documentation-functions
              (cons #'flymake-eldoc-function
                    (remove #'flymake-eldoc-function eldoc-documentation-functions))))
    (defun my/maybe-start-eglot ()
        "Exlude some mode from eglot."
        (let ((disabled-modes '(emacs-lisp-mode
                                cmake-mode)))
            (unless (apply 'derived-mode-p disabled-modes)
                (eglot-ensure))))
    (defun my/eglot-clangd-find-other-file ()
        "Switch between the corresponding C/C++ source and header file."
        (interactive)
        (let* ((server (eglot--current-server-or-lose))
               (rep
                (jsonrpc-request
                 server
                 :textDocument/switchSourceHeader
                 (eglot--TextDocumentIdentifier))))
            (unless (equal rep nil)
                (if (equal current-prefix-arg nil)
                        (funcall #'find-file (eglot--uri-to-path rep))
                    (funcall #'find-file-other-window (eglot--uri-to-path rep))))))
    :hook((eglot-managed-mode . my/eglot-eldoc)
          (prog-mode . my/maybe-start-eglot))
    :config
    (setf (plist-get eglot-events-buffer-config :size) 0)
    (fset #'jsonrpc--log-event #'ignore)
    (setq jsonrpc-event-hook nil)
    (setq eglot-autoshutdown t)
    (setq eglot-extend-to-xref nil)
    (setq eglot-sync-connect nil)
    (setq eglot-confirm-server-initiated-edits nil)
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
    (setq eglot-workspace-configuration
          '(:gopls (:staticcheck t :usePlaceholders t)))

    ;; don't try to manage these
    (add-to-list 'eglot-stay-out-of 'eldoc-documentation-strategy)
    ;; (add-to-list 'eglot-stay-out-of 'flymake)
    ;; (add-to-list 'eglot-stay-out-of 'yasnippet)

    ;; Rust
    (add-to-list 'eglot-server-programs
                 '((rust-ts-mode rust-mode) .
                   ("rust-analyzer" :initializationOptions (:checkOnSave (:command "clippy")))))

    ;; Zig
    (add-to-list 'eglot-server-programs
                 '(zig-mode . ("zls")))

    ;; C++
    (add-to-list 'eglot-server-programs
                 '((c-ts-mode c++-ts-mode c-mode c++-mode)
                   . ("clangd"
                      "-j=4"
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
    :after eglot
    :custom
    (eglot-inactive-regions-style 'darken-foreground)
    (eglot-inactive-regions-opacity 0.4)
    :config
    (eglot-inactive-regions-mode 1))

;; eglot-booster : Boost eglot using lsp-booster
(use-package eglot-booster
    :disabled t
    :after eglot
    :vc (:url "git@github.com:jdtsmith/eglot-booster.git"
              :rev :newest)
    :custom
    ((eglot-booster-no-remote-boost t)
     (eglot-booster-io-only t))
    :config (eglot-booster-mode))

;; geiser support for guile
(use-package geiser-guile
    :defer t
    :config
    (setq geiser-guile-binary "guile3.0"))

;; dape : Debug Adapter Protocol for Emacs
(use-package dape :defer t)

;; apheleia : Good code is automatically formatted
(use-package apheleia
    :preface
    (defun my/disable-apheleia ()
        (apheleia-mode -1))
    :hook
    ((prog-mode . apheleia-mode)
     (web-mode . my/disable-apheleia))
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
    :custom
    (compilation-always-kill t))

(use-package fancy-compilation
    :init
    (with-eval-after-load 'compile
        (fancy-compilation-mode))
    :custom
    (fancy-compilation-override-colors nil))

;; rmsbolt : compiler-explorer for Emacs
(use-package rmsbolt
    :custom
    (rmsbolt-automatic-recompile nil))

(provide 'my-init-lang-tools)
;;; my-init-lang-tools.el ends here

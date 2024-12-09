;;; my-init-lang-tools.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; needed somehow
(use-package request)

;; hideshow : code folding
(use-package hideshow
    :ensure nil ;; emacs built-in
    :preface
    (defun my/hs-toggle-hidding ()
        (interactive)
        (save-excursion
            (end-of-line)
            (hs-toggle-hiding)))
    :init
    (add-hook 'prog-mode-hook 'hs-minor-mode))

;; subword-mode
(use-package subword
    :ensure nil ;; emacs built-in
    :hook ((prog-mode . subword-mode)))

;; flymake
(use-package flymake
    :ensure nil ;; emacs built-in
    :config
    (setq flymake-suppress-zero-counters t)
    (setq flymake-no-changes-timeout 3) ;; Don't be so hasty in syntax checking.
    (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
    :hook ((prog-mode . (lambda ()
                            (flymake-mode)
                            (which-function-mode)))))

;; flymake-proselint : use proselint with Emacs built-in Flymake
(use-package flymake-proselint
    :disabled t
    :after flymake
    :hook (text-mode . (lambda ()
                           (flymake-mode)
                           (flymake-proselint-setup))))

(use-package eldoc
    :ensure nil ;; emacs built-in
    :config
    (eldoc-add-command #'xref-find-definitions)
    (eldoc-add-command #'xref-go-back)
    (eldoc-add-command #'avy-goto-char-timer)
    (setq eldoc-echo-area-use-multiline-p nil)
    (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
    (global-eldoc-mode))

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
        (let ((disabled-modes '(emacs-lisp-mode)))
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
    :hook
    ((eglot-managed-mode . my/eglot-eldoc)
     (prog-mode . my/maybe-start-eglot))
    :config
    (setf (plist-get eglot-events-buffer-config :size) 0)
    (fset #'jsonrpc--log-event #'ignore)
    (setq jsonrpc-event-hook nil)
    (setq eglot-autoshutdown t)
    (setq eglot-extend-to-xref t)
    (setq eglot-sync-connect nil)
    (setq eglot-confirm-server-initiated-edits nil)
    (setq eglot-ignored-server-capabilities
          '(;; :documentHighlightProvider
            ;; :documentFormattingProvider
            ;; :documentRangeFormattingProvider
            :documentOnTypeFormattingProvider
            :inlayHintProvider))

    ;; Setting the workspace configuration for every buffer, this can also be
    ;; done as dir-local variables for project/directory.
    (setq eglot-workspace-configuration
          '(:gopls (:staticcheck t :usePlaceholders t)))

    ;; don't try to manage these
    (add-to-list 'eglot-stay-out-of 'eldoc-documentation-strategy)
    ;; (add-to-list 'eglot-stay-out-of 'yasnippet)

    ;; Rust
    (add-to-list 'eglot-server-programs
                 '((rust-ts-mode rust-mode) .
                   ("rust-analyzer" :initializationOptions (:checkOnSave (:command "clippy")))))

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
    :after eglot
    :custom
    (eglot-inactive-regions-style 'darken-foreground)
    (eglot-inactive-regions-opacity 0.4)
    :config
    (eglot-inactive-regions-mode 1))

;; eglot-booster : Boost eglot using lsp-booster
(use-package eglot-booster
    :vc (:url "git@github.com:jdtsmith/eglot-booster.git"
              :rev :newest)
    :after eglot
    :custom (eglot-booster-no-remote-boost t)
    :config (eglot-booster-mode))

;; dape : Debug Adapter Protocol for Emacs
(use-package dape
    :disabled t)

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

(provide 'my-init-lang-tools)
;;; my-init-lang-tools.el ends here

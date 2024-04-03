;;; my-init-lang-tools.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; hideshow
(use-package hideshow
    :ensure nil ;; emacs built-in
    :preface
    (defun my--toggle-fold ()
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
                            (flymake-mode +1)
                            (which-function-mode)))))

(use-package eldoc
    :ensure nil ;; emacs built-in
    :config
    (setq eldoc-echo-area-use-multiline-p nil)
    (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
    (global-eldoc-mode))

(use-package eglot
    :ensure nil ;; emacs built-in
    :preface
    (defun my--eglot-eldoc ()
        "Show flymake diagnostics first."
        (setq eldoc-documentation-functions
              (cons #'flymake-eldoc-function
                    (remove #'flymake-eldoc-function eldoc-documentation-functions))))
    (defun my--maybe-start-eglot ()
        "Exlude some mode from eglot."
        (let ((disabled-modes '(emacs-lisp-mode dockerfile-ts-mode)))
            (unless (apply 'derived-mode-p disabled-modes)
                (eglot-ensure))))
    :hook
    ((eglot-managed-mode . my--eglot-eldoc)
     (prog-mode . my--maybe-start-eglot))
    :config
    (fset #'jsonrpc--log-event #'ignore)
    (setq eglot-events-buffer-size 0)
    (setq eglot-autoshutdown t)
    (setq eglot-extend-to-xref t)
    (setq eglot-sync-connect nil)
    (setq eglot-confirm-server-initiated-edits nil)
    (setq eglot-ignored-server-capabilities
          '(;; :documentHighlightProvider
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

(use-package eglot-hierarchy
    :after eglot
    :vc (:fetcher github :repo "dolmens/eglot-hierarchy"))

;; apheleia : Good code is automatically formatted
(use-package apheleia
    :preface
    (defun my--disable-apheleia ()
        (apheleia-mode -1))
    :hook
    ((prog-mode . apheleia-mode)
     (web-mode . my--disable-apheleia))
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

(use-package rmsbolt)

(provide 'my-init-lang-tools)
;;; my-init-lang-tools.el ends here

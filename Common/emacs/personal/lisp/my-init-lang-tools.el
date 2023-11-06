;;; my-init-lang-tools.el -*- lexical-binding: t; -*-

;; hideshow
(use-package hideshow
    :ensure nil ;; emacs built-in
    :preface
    (defun my/toggle-fold ()
        (interactive)
        (save-excursion
            (end-of-line)
            (hs-toggle-hiding)))
    :init (add-hook 'prog-mode-hook 'hs-minor-mode))

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
    (add-to-list 'display-buffer-alist
                 '("^\\*eldoc for" display-buffer-at-bottom
                   (window-height . 4)))
    (setq eldoc-echo-area-use-multiline-p nil)
    (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))

(use-package eglot
    :ensure nil ;; emacs built-in
    :preface (defun my/eglot-eldoc ()
                 ;; Show flymake diagnostics first.
                 (setq eldoc-documentation-functions
                       (cons #'flymake-eldoc-function
                             (remove #'flymake-eldoc-function eldoc-documentation-functions)))

                 ;; Show all eldoc feedback.
                 (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly))
    :hook ((eglot-managed-mode . my/eglot-eldoc)
           (sh-mode . eglot-ensure)
           (bash-ts-mode . eglot-ensure)
           (cmake-mode . eglot-ensure)
           (cmake-ts-mode . eglot-ensure)
           (c-mode . eglot-ensure)
           (c-ts-mode . eglot-ensure)
           (c++-mode . eglot-ensure)
           (c++-ts-mode . eglot-ensure)
           (rustic-mode . eglot-ensure)
           (zig-mode . eglot-ensure)
           (js-mode . eglot-ensure)
           (js-ts-mode . eglot-ensure)
           (python-mode . eglot-ensure)
           (python-ts-mode . eglot-ensure)
           (go-mode . eglot-ensure)
           (go-ts-mode . eglot-ensure)
           (sql-mode . eglot-ensure))
    :config
    (setq eglot-autoshutdown t)
    (setq eglot-events-buffer-size 0)
    (setq eglot-extend-to-xref t)
    (setq eglot-sync-connect nil)
    (setq eglot-ignored-server-capabilities
          '(:hoverProvider
            :inlayHintProvider
            :documentHighlightProvider
            :documentFormattingProvider
            :documentRangeFormattingProvider
            :documentOnTypeFormattingProvider
            :colorProvider
            :foldingRangeProvider))

    ;; Setting the workspace configuration for every buffer, this can also be
    ;; done as dir-local variables for project/directory.
    (setq eglot-workspace-configuration
          '(:gopls (:staticcheck t :usePlaceholders t)
                   :rust-analyzer (:check (:command "clippy"))))

    ;; don't try to manage these
    (add-to-list 'eglot-stay-out-of 'eldoc-documentation-strategy)
    ;; (add-to-list 'eglot-stay-out-of 'yasnippet)

    ;; C++
    (add-to-list 'eglot-server-programs
                 '((c-mode c-ts-mode c++-mode c++-ts-mode)
                   . ("clangd"
                      "-j=8"
                      "--log=error"
                      "--malloc-trim"
                      "--background-index"
                      "--clang-tidy"
                      "--completion-style=detailed"
                      "--pch-storage=memory"
                      "--header-insertion=never"
                      "--header-insertion-decorators=0"))))

;; apheleia : Good code is automatically formatted
(use-package apheleia
    :preface (defun my/disable-apheleia ()
                 (apheleia-mode -1))
    :hook ((prog-mode . apheleia-mode)
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
    :custom (compilation-always-kill t))

(use-package fancy-compilation
    :commands (fancy-compilation-mode)
    :init (with-eval-after-load 'compile
              (fancy-compilation-mode))
    :custom (fancy-compilation-override-colors nil))

(use-package rmsbolt)

(provide 'my-init-lang-tools)
;;; my-init-langtools.el ends here

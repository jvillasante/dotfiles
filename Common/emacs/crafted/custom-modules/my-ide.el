;;; my-ide.el --- Custom IDE Configs -*- lexical-binding: t; -*-

;;; Commentary:
;;; IDE Customizations

;;; Code:

;; lsp
(defvar my-ide-lsp-backend 'lsp-mode
  "The lsp backend in use ['eglot or 'lsp-mode].")

;; Project.el enhancements
(with-eval-after-load 'project
  (defcustom project-root-markers
    '("Cargo.toml" "compile_commands.json" "compile_flags.txt"
      "project.clj" ".git" "deps.edn" "shadow-cljs.edn")
    "Files or directories that indicate the root of a project."
    :type '(repeat string)
    :group 'project)

  (defun project-root-p (path)
    "Check if the current PATH has any of the project root markers."
    (catch 'found
      (dolist (marker project-root-markers)
        (when (file-exists-p (concat path marker))
          (throw 'found marker)))))

  (defun project-find-root (path)
    "Search up the PATH for `project-root-markers'."
    (when-let ((root (locate-dominating-file path #'project-root-p)))
      (cons 'transient (expand-file-name root))))
  (add-to-list 'project-find-functions #'project-find-root)

  (defun project-save-some-buffers (&optional arg)
    "Save some modified file-visiting buffers in the current project.

Optional argument ARG (interactively, prefix argument) non-nil
means save all with no questions."
    (interactive "P")
    (let* ((project-buffers (project-buffers (project-current)))
           (pred (lambda () (memq (current-buffer) project-buffers))))
      (funcall-interactively #'save-some-buffers arg pred)))

  (define-advice project-compile (:around (fn) save-project-buffers)
    "Only ask to save project-related buffers."
    (let* ((project-buffers (project-buffers (project-current)))
           (compilation-save-buffers-predicate
            (lambda () (memq (current-buffer) project-buffers))))
      (funcall fn)))

  (define-advice recompile (:around (fn &optional edit-command) save-project-buffers)
    "Only ask to save project-related buffers if inside a project."
    (if (project-current)
        (let* ((project-buffers (project-buffers (project-current)))
               (compilation-save-buffers-predicate
                (lambda () (memq (current-buffer) project-buffers))))
          (funcall fn edit-command))
      (funcall fn edit-command))))

;; Eldoc
(setq! eldoc-echo-area-use-multiline-p nil)

;; hideshow
(progn
  (add-hook 'prog-mode-hook 'hs-minor-mode)
  (defun +my/toggle-fold ()
    (interactive)
    (save-excursion
      (end-of-line)
      (hs-toggle-hiding))))

;; flymake
(progn
  ;; Underline warnings and errors from Flymake
  (custom-set-faces
   '(flymake-errline ((((class color)) (:underline "red"))))
   '(flymake-warnline ((((class color)) (:underline "yellow")))))

  ;; Display error and warning messages in minibuffer.
  (custom-set-variables
   '(help-at-pt-timer-delay 0.5)
   '(help-at-pt-display-when-idle '(flymake-overlay))))

;; format-all : auto format source code
(crafted-package-install-package 'format-all)
(progn
  (add-hook 'prog-mode-hook 'format-all-mode)
  (add-hook 'format-all-mode-hook #'format-all-ensure-formatter))

;; yasnippet
(crafted-package-install-package 'yasnippet)
(crafted-package-install-package 'yasnippet-snippets)
(with-eval-after-load 'yasnippet
  (yas-global-mode 1))

;; C++
(progn
  (setq! c-default-style "linux")
  (setq! c-basic-offset 4)
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.hxx\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.C\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode)))

;; rustic : rust mode
(crafted-package-install-package 'rustic)
(with-eval-after-load 'rustic
  (when (eq my-ide-lsp-backend 'eglot)
    (setq! rustic-lsp-client 'eglot))
  (setq! rustic-format-on-save nil))

;; web
(crafted-package-install-package 'web-mode)
(with-eval-after-load 'web-mode
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode)))

;; Eglot
(when (eq my-ide-lsp-backend 'eglot)
  (when (< emacs-major-version 29) (crafted-package-install-package 'eglot))
  (add-hook 'c-mode-hook #'eglot-ensure)
  (add-hook 'c++-mode-hook #'eglot-ensure)
  (add-hook 'rustic-mode-hook #'eglot-ensure)
  (add-hook 'js-mode-hook #'eglot-ensure)
  (with-eval-after-load 'eglot
    (setq! eglot-autoshutdown t)
    (setq! eglot-extend-to-xref t)
    (setq! eglot-ignored-server-capabilities
           (quote (:documentFormattingProvider :documentRangeFormattingProvider)))

    (add-to-list
     'eglot-server-programs
     '((c-mode c++-mode)
       . ("clangd"
          "-j=8"
          "--log=error"
          "--malloc-trim"
          "--background-index"
          "--clang-tidy"
          "--cross-file-rename"
          "--completion-style=detailed"
          "--pch-storage=memory"
          "--header-insertion=never"
          "--header-insertion-decorators=0")))))

;; lsp-mode
(when (eq my-ide-lsp-backend 'lsp-mode)
  ;; flycheck
  (crafted-package-install-package 'flycheck)
  (crafted-package-install-package 'flycheck-inline)
  (progn
    (with-eval-after-load 'flycheck
      (setq! flycheck-temp-prefix "flycheck_tmp")
      (add-to-list 'flycheck-disabled-checkers 'c/c++-clang)
      (add-to-list 'flycheck-disabled-checkers 'c/c++-gcc)
      (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)
      (add-hook 'flycheck-mode-hook 'flycheck-inline-mode))
    (add-hook 'after-init-hook 'global-flycheck-mode))

  ;; company
  (crafted-package-install-package 'company)
  (progn
    (add-hook
     'after-init-hook (lambda ()
                        (global-corfu-mode -1)
                        (global-company-mode +1)))

    (with-eval-after-load 'company
      (setq! company-tooltip-limit 20)
      (setq! company-idle-delay 0.1)
      (setq! company-echo-delay 0.1)
      (setq! company-show-quick-access t)
      (setq! company-minimum-prefix-length 2)
      (setq! company-tooltip-align-annotations t)
      (setq! company-auto-commit nil)
      (setq! company-global-modes
             '(not erc-mode
                   circe-mode
                   message-mode
                   help-mode
                   gud-mode
                   vterm-mode))
      (setq! company-frontends
             '(company-pseudo-tooltip-frontend  ; always show candidates in overlay tooltip
               company-echo-metadata-frontend))))  ; show selected candidate docs in echo area

  ;; lsp-mode
  (crafted-package-install-package 'lsp-mode)
  (progn
    ;; Rust hack!
    (cl-defmethod lsp-clients-extract-signature-on-hover (contents (_server-id (eql rust-analyzer)))
      (-let* (((&hash "value") contents)
              (groups (--partition-by (s-blank? it) (s-lines (s-trim value))))
              (sig_group (if (s-equals? "```rust" (car (-third-item groups)))
                             (-third-item groups)
                           (car groups)))
              (sig (--> sig_group
                        (--drop-while (s-equals? "```rust" it) it)
                        (--take-while (not (s-equals? "```" it)) it)
                        (--map (s-trim it) it)
                        (s-join " " it))))
        (lsp--render-element (concat "```rust\n" sig "\n```"))))

    ;; Hooks
    (add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration)
    (add-hook 'c-mode-hook 'lsp)
    (add-hook 'c++-mode-hook 'lsp)
    (add-hook 'js-mode-hook 'lsp)
    (add-hook 'js-jsx-mode-hook 'lsp)
    (add-hook 'typescript-mode-hook 'lsp)
    (add-hook 'web-mode-hook 'lsp)

    ;; Customizations
    (with-eval-after-load 'lsp-mode
      (setq! lsp-keymap-prefix "C-c l") ;; set prefix for lsp-command-keymap
      (setq! lsp-session-file (expand-file-name ".lsp-session" crafted-config-var-directory))
      (setq! lsp-idle-delay 0.5)
      (setq! lsp-file-watch-threshold 15000)
      (setq! lsp-auto-guess-root t)
      (setq! lsp-log-io nil)
      (setq! lsp-restart 'auto-restart)
      (setq! lsp-enable-symbol-highlighting t)
      (setq! lsp-lens-enable nil)
      (setq! lsp-headerline-breadcrumb-enable nil)
      (setq! lsp-modeline-code-actions-enable t)
      (setq! lsp-modeline-diagnostics-enable t)
      (setq! lsp-eldoc-enable-hover t)
      (setq! lsp-signature-auto-activate t)
      (setq! lsp-signature-render-documentation nil)
      (setq! lsp-completion-show-detail t)
      (setq! lsp-completion-show-kind nil)
      (setq! read-process-output-max (* 1024 1024)) ;; 1MB

      ;; Rust
      (setq! lsp-rust-analyzer-cargo-watch-command "clippy")
      (setq! lsp-rust-analyzer-completion-auto-import-enable nil)

      ;; Zig
      (setq! lsp-zig-zls-executable
             (expand-file-name "zig/zls/zig-out/bin/zls" +my/software-path))

      ;; C++
      (setq! lsp-clients-clangd-args
             '("-j=8"
               "--log=error"
               "--malloc-trim"
               "--background-index"
               "--clang-tidy"
               "--cross-file-rename"
               "--completion-style=detailed"
               "--pch-storage=memory"
               "--header-insertion=never"
               "--header-insertion-decorators=0")))

    ;; Other
    (with-eval-after-load 'flycheck
      (add-to-list 'flycheck-disabled-checkers 'c/c++-clang)
      (add-to-list 'flycheck-disabled-checkers 'c/c++-gcc))
    (with-eval-after-load 'lsp-clangd (set-lsp-priority! 'clangd 2)))

  ;; lsp-ui
  (crafted-package-install-package 'lsp-ui)
  (with-eval-after-load 'lsp-ui
    (setq! lsp-ui-doc-enable nil)
    (setq! lsp-ui-doc-show-with-cursor nil)
    (setq! lsp-ui-doc-show-with-mouse nil)
    (setq! lsp-ui-sideline-enable nil)
    (setq! lsp-ui-sideline-show-code-actions nil)
    (setq! lsp-ui-sideline-show-hover nil)))

(provide 'my-ide)
;;; my-ide.el ends here

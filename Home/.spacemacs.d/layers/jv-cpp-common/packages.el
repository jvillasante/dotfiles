(defconst jv-cpp-common-packages
  '(cc-mode
     lsp-mode
     lsp-ui
     company
     modern-cpp-font-lock))

(defun jv-cpp-common/post-init-cc-mode ()
  (add-hook 'c++-mode-hook
    (lambda () (progn
                 (setq-default flycheck-c/c++-clang-executable jv/clang-path)
                 (setq-default flycheck-clang-standard-library "libc++")
                 (setq-default flycheck-clang-language-standard "c++17")
                 (setq company-clang-arguments '("-std=c++17"))))))

(defun jv-cpp-common/post-init-lsp-mode ()
  (setq lsp-remap-xref-keybindings nil)
  (setq lsp-navigation 'simple))

(defun jv-cpp-common/post-init-lsp-ui ()
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-include-signature nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-symbol nil)
  (setq lsp-ui-sideline-ignore-dupliate nil))

(defun jv-cpp-common/post-init-company ()
  (setq company-clang-executable jv/clang-path)
  (setq company-idle-delay 0))

(defun jv-cpp-common/init-modern-cpp-font-lock ()
  (use-package modern-cpp-font-lock
    :ensure t
    :init
    (progn
      (modern-c++-font-lock-global-mode t)
      (spacemacs|diminish modern-c++-font-lock-mode " ⊗" " x"))))

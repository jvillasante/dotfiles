(defconst jv-cpp-common-packages
  '(cc-mode
     company
     modern-cpp-font-lock
     rtags))

(defun jv-cpp-common/post-init-cc-mode ()
  (add-hook 'c++-mode-hook
    (lambda () (progn
                 (setq-default flycheck-c/c++-clang-executable jv/clang-path)
                 (setq-default flycheck-clang-standard-library "libc++")
                 (setq-default flycheck-clang-language-standard "c++17")
                 (setq company-clang-arguments '("-std=c++17"))))))

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

(defun jv-cpp-common/init-rtags ()
  (use-package rtags
    :defer f
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'c++-mode
        "d" 'rtags-find-symbol-at-point
        "D" 'jv-cpp-common/rtags-find-symbol-at-point-other-file
        "/" 'rtags-find-symbol
        "R" 'rtags-find-references-at-point
        ;; "r" 'rtags-find-references-at-point-in-file to implement
        "v" 'rtags-find-virtuals-at-point
        "i" 'rtags-imenu
        "C-r" 'rtags-rename-symbol

        ;; print prefix
        "pt" 'rtags-print-class-hierarchy
        "pe" 'rtags-print-enum-value-at-point
        "pi" 'rtags-print-dependencies
        "ps" 'rtags-print-symbol-info
        "pp" 'rtags-preprocess-file

        ;; TODO: planned micro state
        ;; "o" (rtags-occurence-transient state)
        ;; "n" 'rtags-next-match
        ;; "N/p" 'rtags-previous-match
        )
      (add-hook 'rtags-jump-hook 'evil-set-jump)
      (add-to-list 'spacemacs-jump-handlers-c++-mode '(rtags-find-symbol-at-point :async t)))
    :config
    (progn
      (require 'rtags-helm)
      (setq rtags-jump-to-first-match nil)
      (setq rtags-use-helm t))))

(defconst jv-cpp-packages
  '(cc-mode
     irony
     irony-eldoc
     company-irony
     company-irony-c-headers
     flycheck-irony
     company
     ;; ycmd
     rtags
     modern-cpp-font-lock))

(defun jv-cpp/post-init-cc-mode ()
  (add-hook 'c++-mode-hook
    (lambda () (progn
                 (setq company-clang-arguments '("-std=c++14")
                   flycheck-clang-language-standard "c++14"
                   flycheck-gcc-language-standard "c++14"
                   disaster-cxxflags "-std=c++14 -O1 -g3")))))

;; (defun jv-cpp/post-init-ycmd ()
;;   (setq ycmd-server-command (list "python" (file-truename "~/Software/src/ycmd/ycmd")))
;;   (setq ycmd-force-semantic-completion t)

;;   (add-hook 'c++-mode-hook 'ycmd-mode)
;;   (add-hook 'rust-mode-hook 'ycmd-mode))

(defun jv-cpp/init-irony ()
  (use-package irony
    :init
    (defun jv-irony-mode-hook ()
      (define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async)
      (define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async))

    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'irony-mode-hook 'jv-irony-mode-hook)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    (spacemacs|diminish irony-mode "ⓘ" "i")))

(defun jv-cpp/init-irony-eldoc ()
  (use-package irony-eldoc
    :init
    (add-hook 'c-mode-hook 'eldoc-mode)
    (add-hook 'c++-mode-hook 'eldoc-mode)
    (add-hook 'irony-mode-hook 'irony-eldoc)))

(defun jv-cpp/init-company-irony ()
  (use-package company-irony
    :init
    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)))

(defun jv-cpp/init-company-irony-c-headers ()
  (use-package company-irony-c-headers))

(defun jv-cpp/init-flycheck-irony ()
  (use-package flycheck-irony
    :init
    (progn
      (eval-after-load 'flycheck
        '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))))

(defun jv-cpp/post-init-company ()
  ;; (spacemacs|add-company-backends
  ;;  :modes c++-mode)
  ;; (spacemacs|add-company-backends
  ;;  :backends company-cmake
  ;;  :modes cmake-mode)

  (add-hook 'c-mode-hook (lambda () (add-to-list 'company-backends '(company-irony-c-headers company-irony))))
  (add-hook 'c++-mode-hook (lambda () (add-to-list 'company-backends '(company-irony-c-headers company-irony)))))

(defun jv-cpp/init-rtags ()
  (use-package rtags
    :defer f
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'c++-mode
        "d" 'rtags-find-symbol-at-point
        "D" 'jv-cpp/rtags-find-symbol-at-point-other-file
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

(defun jv-cpp/init-modern-cpp-font-lock ()
  (use-package modern-cpp-font-lock
    :init
    (modern-c++-font-lock-global-mode t)
    (spacemacs|diminish modern-c++-font-lock-mode "ⓜ" "m")))

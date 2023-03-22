;;; my-init-completion.el -*- lexical-binding: t; -*-

(straight-use-package '(vertico :host github :repo "minad/vertico" :files (:defaults "extensions/*.el")))
(straight-use-package 'orderless)
(straight-use-package 'consult)
(straight-use-package 'compat)
(straight-use-package 'consult-dir)
(straight-use-package 'embark)
(straight-use-package 'embark-consult)
(straight-use-package 'marginalia)
(straight-use-package 'cape)
(straight-use-package 'corfu)
(straight-use-package 'corfu-terminal)
(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)
(straight-use-package 'all-the-icons-completion)

(use-package vertico
    :init
    (add-hook 'pre-command-hook 'vertico-mode)
    :config
    (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t)
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
    (setq-default completion-in-region-function #'my/completion-in-region)

    ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
    ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
    (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

(use-package orderless
    :init
    (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
    :init
    (add-hook 'pre-command-hook 'marginalia-mode)
    :config
    (if (display-graphic-p)
        (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)))

(use-package consult
    :config
    (setq consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay 0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1))

(use-package consult-dir
    :init
    (general-define-key
        [remap list-directory] #'consult-dir)
    (general-define-key
        :keymaps 'vertico-map
        "C-c C-d " #'consult-dir
        "C-c C-j" #'consult-dir-jump-file)

    :config
    ;; TODO: doomemacs configures docker paths for consult dir
    ;; when docker-tramp is configured, will take a reference from it.
    (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-local t)
    (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t))

(use-package embark
    :init
    (setq which-key-use-C-h-commands nil
        prefix-help-command #'embark-prefix-help-command)
    ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
    :config
    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
        '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
             nil
             (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
    :ensure t ; only need to install it, embark loads it after consult if found
    :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
    :demand t
    :preface
    (defun corfu-send-shell (&rest _)
        "Send completion candidate when inside comint/eshell."
        (cond
            ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
                (eshell-send-input))
            ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
                (comint-send-input))))
    (defun corfu-move-to-minibuffer ()
        (interactive)
        (let ((completion-extra-properties corfu--extra)
                 completion-cycle-threshold completion-cycling)
            (apply #'consult-completion-in-region completion-in-region--data)))
     :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous))
    :config
    (setq corfu-cycle t
        corfu-auto t
        corfu-auto-delay 0.0
        corfu-quit-no-match 'separator)
    :init
    (global-corfu-mode)
    (global-set-key (kbd "M-i") #'completion-at-point)
    (keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)
    (add-hook 'eshell-mode-hook
        (lambda ()
            (setq-local corfu-auto nil)
            (corfu-mode)))
    (advice-add #'corfu-insert :after #'corfu-send-shell)
    (when (< emacs-major-version 29)
        ;; Silence the pcomplete capf, no errors or messages!
        (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

        ;; Ensure that pcomplete does not write to the buffer
        ;; and behaves as a pure `completion-at-point-function'.
        (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)))

(use-package corfu-terminal
    :after corfu
    :init
    (unless (display-graphic-p)
        (corfu-terminal-mode +1)))

(use-package cape
    ;; Bind dedicated completion commands
    ;; Alternative prefix keys: C-c p, M-p, M-+, ...
    :bind (("C-c p p" . completion-at-point) ;; capf
              ("C-c p t" . complete-tag)        ;; etags
              ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
              ("C-c p h" . cape-history)
              ("C-c p f" . cape-file)
              ("C-c p k" . cape-keyword)
              ("C-c p s" . cape-symbol)
              ("C-c p a" . cape-abbrev)
              ("C-c p i" . cape-ispell)
              ("C-c p l" . cape-line)
              ("C-c p w" . cape-dict)
              ("C-c p \\" . cape-tex)
              ("C-c p _" . cape-tex)
              ("C-c p ^" . cape-tex)
              ("C-c p &" . cape-sgml)
              ("C-c p r" . cape-rfc1345))
    :init
    ;; Add `completion-at-point-functions', used by `completion-at-point'.
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)
    ;;(add-to-list 'completion-at-point-functions #'cape-history)
    ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
    ;;(add-to-list 'completion-at-point-functions #'cape-tex)
    ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
    ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
    ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
    ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
    ;;(add-to-list 'completion-at-point-functions #'cape-dict)
    ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
    ;;(add-to-list 'completion-at-point-functions #'cape-line)
    )

(use-package yasnippet
    :init
    (yas-global-mode +1)
    :config
    (yas-reload-all)
    (push (expand-file-name "snippets/" user-emacs-directory) yas-snippet-dirs))

(provide 'my-init-completion)
;;; my-init-completion.el ends here

;;; my-init-completion.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;;
;;; Code:

;; Vertico, Consult, and Embark collectively enhance Emacs' completion and
;; navigation capabilities. Vertico provides a vertical completion interface,
;; making it easier to navigate and select from completion candidates (e.g.,
;; when M-x is pressed). Consult offers a suite of commands for efficient
;; searching, previewing, and interacting with buffers, file contents, and
;; more, improving various tasks. Embark integrates with these tools to provide
;; context-sensitive actions and quick access to commands based on the current
;; selection, further improving user efficiency and workflow within Emacs.
;; Together, they create a cohesive and powerful environment for managing
;; completions and interactions.

;; Tip: You can remove the `vertico-mode' use-package and replace it
;;      with the built-in `fido-vertical-mode'.
(use-package vertico
    ;; (Note: It is recommended to also enable the savehist package.)
    :ensure t
    :defer t
    :commands vertico-mode
    :hook (after-init . vertico-mode))

(use-package orderless
    ;; Vertico leverages Orderless' flexible matching capabilities, allowing users
    ;; to input multiple patterns separated by spaces, which Orderless then
    ;; matches in any order against the candidates.
    :ensure t
    :custom
    (completion-styles '(orderless basic))
    (completion-category-defaults nil)
    (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
    ;; Marginalia allows Embark to offer you preconfigured actions in more contexts.
    ;; In addition to that, Marginalia also enhances Vertico by adding rich
    ;; annotations to the completion candidates displayed in Vertico's interface.
    :ensure t
    :defer t
    :commands (marginalia-mode marginalia-cycle)
    :hook (after-init . marginalia-mode))

(use-package embark
    ;; Embark is an Emacs package that acts like a context menu, allowing
    ;; users to perform context-sensitive actions on selected items
    ;; directly from the completion interface.
    :ensure t
    :defer t
    :bind
    (("C-." . embark-act)         ;; pick some comfortable binding
     ("C-;" . embark-dwim)        ;; good alternative: M-.
     ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

    :init
    (setq prefix-help-command #'embark-prefix-help-command)

    :config
    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none)))))

(use-package embark-consult
    :ensure t
    :hook
    (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
    :ensure t
    :bind (;; C-c bindings in `mode-specific-map'
           ("C-c M-x" . consult-mode-command)
           ("C-c h" . consult-history)
           ("C-c k" . consult-kmacro)
           ("C-c m" . consult-man)
           ("C-c i" . consult-info)
           ([remap Info-search] . consult-info)
           ;; C-x bindings in `ctl-x-map'
           ("C-x M-:" . consult-complex-command)
           ("C-x b" . consult-buffer)
           ("C-x 4 b" . consult-buffer-other-window)
           ("C-x 5 b" . consult-buffer-other-frame)
           ("C-x t b" . consult-buffer-other-tab)
           ("C-x r b" . consult-bookmark)
           ("C-x p b" . consult-project-buffer)
           ;; Custom M-# bindings for fast register access
           ("M-#" . consult-register-load)
           ("M-'" . consult-register-store)
           ("C-M-#" . consult-register)
           ;; Other custom bindings
           ("M-y" . consult-yank-pop)
           ;; M-g bindings in `goto-map'
           ("M-g e" . consult-compile-error)
           ("M-g f" . consult-flymake)
           ("M-g g" . consult-goto-line)
           ("M-g M-g" . consult-goto-line)
           ("M-g o" . consult-outline)
           ("M-g m" . consult-mark)
           ("M-g k" . consult-global-mark)
           ("M-g i" . consult-imenu)
           ("M-g I" . consult-imenu-multi)
           ;; M-s bindings in `search-map'
           ("M-s d" . consult-find)
           ("M-s c" . consult-locate)
           ("M-s g" . consult-grep)
           ("M-s G" . consult-git-grep)
           ("M-s r" . consult-ripgrep)
           ("M-s l" . consult-line)
           ("M-s L" . consult-line-multi)
           ("M-s k" . consult-keep-lines)
           ("M-s u" . consult-focus-lines)
           ;; Isearch integration
           ("M-s e" . consult-isearch-history)
           :map isearch-mode-map
           ("M-e" . consult-isearch-history)
           ("M-s e" . consult-isearch-history)
           ("M-s l" . consult-line)
           ("M-s L" . consult-line-multi)
           ;; Minibuffer history
           :map minibuffer-local-map
           ("M-s" . consult-history)
           ("M-r" . consult-history))

    ;; Enable automatic preview at point in the *Completions* buffer.
    :hook (completion-list-mode . consult-preview-at-point-mode)

    :init
    ;; Optionally configure the register formatting. This improves the register
    (setq register-preview-delay 0.5
          register-preview-function #'consult-register-format)

    ;; Optionally tweak the register preview window.
    (advice-add #'register-preview :override #'consult-register-window)

    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)

    :config
    (consult-customize
     consult-theme :preview-key '(:debounce 0.2 any)
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-bookmark consult--source-file-register
     consult--source-recent-file consult--source-project-recent-file
     ;; :preview-key "M-."
     :preview-key '(:debounce 0.4 any))
    (setq consult-narrow-key "<"))

(provide 'my-init-completion)
;;; my-init-completion.el ends here

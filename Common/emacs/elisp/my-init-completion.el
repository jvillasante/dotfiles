;;; my-init-completion.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;; Minibuffer Completion:
;;     vertico    - Frontend completion UI
;;     orderless  - Backend completion style
;;     consult    - Backend completion functions
;;     marginalia - Annotations
;;     embark     - Actions on completion buffer
;; In-buffer Completion:
;;     corfu     - Frontend completion UI
;;     orderless - Backend completion style
;;     cape      - Backend completion functions
;;
;;; Code:

(use-package emacs
    :ensure nil ;; emacs built-in
    :init
    (when (>= emacs-major-version 30)
        ;; Disable Ispell completion function. Try `cape-dict' as an alternative.
        ;; (text-mode-ispell-word-completion nil)

        ;; subdivide grep output into sections, one per file.
        (setq grep-use-headings t))

    ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
    ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
    (setq read-extended-command-predicate #'command-completion-default-include-p)

    ;; TAB cycle if there are only few candidates
    (setq completion-cycle-threshold 3)
    (setq tab-always-indent 'complete)

    (setq completion-ignore-case t)
    (setq read-buffer-completion-ignore-case t)
    (setq read-file-name-completion-ignore-case t)
    (setq-default case-fold-search t)   ; For general regexp

    (setq enable-recursive-minibuffers t)
    (setq resize-mini-windows t)
    (setq minibuffer-eldef-shorten-default t)

    (setq read-answer-short t) ; also check `use-short-answers' for Emacs28
    (setq echo-keystrokes 0.25)
    (setq kill-ring-max 60) ; Keep it small

    ;; Do not allow the cursor to move inside the minibuffer prompt.  I
    ;; got this from the documentation of Daniel Mendler's Vertico
    ;; package: <https://github.com/minad/vertico>.
    (setq minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

    (file-name-shadow-mode 1)
    (minibuffer-depth-indicate-mode 1)
    (minibuffer-electric-default-mode 1)

    ;; Add prompt indicator to `completing-read-multiple'.  We display
    ;; [CRM<separator>], e.g., [CRM,] if the separator is a comma.  This
    ;; is copied from the README of the `vertico' package.  I made some
    ;; small tweaks to propertize the segments of the prompt.
    (defun crm-indicator (args)
        (cons (format "[`crm-separator': %s]  %s"
                      (propertize
                       (replace-regexp-in-string
                        "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                        crm-separator)
                       'face 'error)
                      (car args))
              (cdr args)))
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator))

;; uniquify : Making Buffer Names Unique
(use-package uniquify
    :ensure nil ; emacs built-in
    :custom
    (uniquify-buffer-name-style 'reverse)
    (uniquify-separator " - ")
    (uniquify-after-kill-buffer-p t)
    (uniquify-ignore-buffers-re "^\\*"))

;; emacs default completion
(use-package icomplete
    :disabled t
    :ensure nil ;; emacs built-in
    :bind
    (:map icomplete-minibuffer-map
          ("C-n" . icomplete-forward-completions)
          ("C-p" . icomplete-backward-completions)
          ("C-v" . icomplete-vertical-toggle)
          ("RET" . icomplete-force-complete-and-exit))
    :hook
    (after-init . (lambda ()
                      (fido-mode -1)
                      (icomplete-mode 1)
                      (icomplete-vertical-mode 1)
                      (completion-preview-mode 1)))
    :config
    (setf completion-styles '(basic flex)
          completion-auto-select t ;; Show completion on first call
          completion-auto-help 'visible ;; Display *Completions* upon first request
          completions-format 'one-column ;; Use only one column
          completions-sort 'historical ;; Order based on minibuffer history
          completions-max-height 20 ;; Limit completions to 15 (completions start at line 5)
          completion-ignore-case t)
    (setq tab-always-indent 'complete)  ;; Starts completion with TAB
    (setq icomplete-delay-completions-threshold 0)
    (setq icomplete-compute-delay 0)
    (setq icomplete-show-matches-on-no-input t)
    (setq icomplete-hide-common-prefix nil)
    (setq icomplete-prospects-height 10)
    (setq icomplete-separator " . ")
    (setq icomplete-with-completion-tables t)
    (setq icomplete-in-buffer t)
    (setq icomplete-max-delay-chars 0)
    (setq icomplete-scroll t)
    (advice-add 'completion-at-point
                :after #'minibuffer-hide-completions))

(use-package imenu
    :ensure nil ;; emacs built-in
    :config
    (setq imenu-flatten 'prefix))

(use-package dabbrev
    :ensure nil ;; emacs built-in
    :init
    ;; `dabbrev' (dynamic word completion (dynamic abbreviations))
    (setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
    (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
    (setq dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
    (setq dabbrev-backward-only nil)
    (setq dabbrev-case-distinction 'case-replace)
    (setq dabbrev-case-fold-search nil)
    (setq dabbrev-case-replace 'case-replace)
    (setq dabbrev-check-other-buffers t)
    (setq dabbrev-eliminate-newlines t)
    (setq dabbrev-upcase-means-case-search t)

    ;; `abbrev' (Abbreviations, else Abbrevs)
    (setq abbrev-file-name (expand-file-name "abbrevs" my/etc-dir))
    (setq only-global-abbrevs nil))

;; hippie expand is dabbrev expand on steroids
;; A composable expansion tool that I find compliments `corfu' in that it
;; looks in a different manner for completions.
;;
;; TODO: Perhaps I should spend a bit time investigating removing `hippie-exp'
;; in favor of `corfu' and `cape' behavior.  Definitely spend a bit of time exploring
;; this option.
(use-package hippie-exp
    :ensure nil ;; emacs built-in
    :config
    (setq hippie-expand-try-functions-list
          '(yas-hippie-try-expand
            try-expand-dabbrev
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill
            try-complete-file-name-partially
            try-complete-file-name
            try-expand-all-abbrevs
            try-expand-list
            try-expand-line
            try-complete-lisp-symbol-partially
            try-complete-lisp-symbol))
    :init (global-set-key [remap dabbrev-expand] 'hippie-expand))

(use-package vertico
    :init
    (vertico-mode 1)
    ;; (vertico-multiform-mode 1)
    :config
    (setq vertico-count 15)
    (setq vertico-resize nil)
    ;; This works with `file-name-shadow-mode' enabled.  When you are in
    ;; a sub-directory and use, say, `find-file' to go to your home '~/'
    ;; or root '/' directory, Vertico will clear the old path to keep
    ;; only your current input.
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

(use-package orderless
    :custom
    (completion-styles '(orderless basic))
    (completion-category-defaults nil)
    (completion-category-overrides
     '((file (styles partial-completion))
       (eglot (styles orderless))))
    :init
    (setq orderless-component-separator " +")
    (setq orderless-matching-styles
          '(orderless-prefixes orderless-regexp))

    ;; SPC should never complete: use it for `orderless' groups.
    ;; The `?' is a regexp construct.
    (let ((map minibuffer-local-completion-map))
        (define-key map (kbd "SPC") nil)
        (define-key map (kbd "?") nil)))

(use-package corfu
    :custom
    ((corfu-auto t)
     (corfu-auto-prefix 3)
     (corfu-quit-no-match t)
     (corfu-preview-current nil)
     (corfu-min-width 20)
     (corfu-popupinfo-delay '(1.25 . 0.5))
     (corfu-quit-at-boundary 'separator))  ;; Enable cycling for `corfu-next/previous'
    :bind (:map corfu-map ("<tab>" . corfu-complete))
    :hook ((after-init . (lambda ()
                             (global-corfu-mode)
                             (corfu-popupinfo-mode)
                             (with-eval-after-load 'savehist
                                 (corfu-history-mode 1)
                                 (add-to-list 'savehist-additional-variables 'corfu-history))))
           (eshell-mode . (lambda ()
                              (setq-local corfu-auto nil)
                              (corfu-mode)))))

(use-package cape
    :init
    ;; eglot integration with cape
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-noninterruptible)

    ;; Add to the global default value of `completion-at-point-functions' which is
    ;; used by `completion-at-point'.  The order of the functions matters, the
    ;; first function returning a result wins.  Note that the list of buffer-local
    ;; completion functions takes precedence over the global list.
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-elisp-block)
    ;;(add-to-list 'completion-at-point-functions #'cape-history)
    ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
    ;;(add-to-list 'completion-at-point-functions #'cape-tex)
    ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
    ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
    ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
    ;;(add-to-list 'completion-at-point-functions #'cape-dict)
    ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
    ;;(add-to-list 'completion-at-point-functions #'cape-line)
    )

(use-package consult
    :init
    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)

    ;; Optionally configure the register formatting. This improves the register
    ;; preview for `consult-register', `consult-register-load',
    ;; `consult-register-store' and the Emacs built-ins.
    (setq register-preview-delay 0.5
          register-preview-function #'consult-register-format)

    ;; Optionally tweak the register preview window.
    ;; This adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window)

    (setq consult-line-numbers-widen t)
    ;; (setq completion-in-region-function #'consult-completion-in-region)
    (setq consult-async-min-input 3)
    (setq consult-async-input-debounce 0.5)
    (setq consult-async-input-throttle 0.8)
    (setq consult-narrow-key nil)
    (setq consult-preview-key 'any)
    (setq consult-find-args
          (concat "find . -not ( "
                  "-path */.git* -prune "
                  "-or -path */.cache* -prune )"))
    ;; (add-to-list 'consult-mode-histories '(vc-git-log-edit-mode . log-edit-comment-ring))
    (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)
    (require 'consult-imenu))

(use-package consult-dir
    :after consult)

(use-package consult-eglot
    :disabled t
    :after (consult eglot))

;; consult-xref-stack : Navigate the Xref stack with Consult.
(use-package consult-xref-stack
    :vc (:url "git@github.com:brett-lempereur/consult-xref-stack.git"
              :rev :newest)
    :after consult
    :bind (("C-," . consult-xref-stack-backward)))

;; consult-notes : select notes via consult
(use-package consult-notes
    :after consult
    :config
    ;; (expand-file-name "Apps/org/notes" my/dropbox-path)
    (setq consult-notes-file-dir-sources
          `(("Org"       ?o ,(expand-file-name "Apps/org" my/dropbox-path))
            ("Org Notes" ?n ,(expand-file-name "Apps/org/notes" my/dropbox-path)))))

(use-package embark
    :init
    ;; Optionally replace the key help with a completing-read interface
    (setq prefix-help-command #'embark-prefix-help-command)

    :config
    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
    :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
    ;; :config
    ;; (setq marginalia-annotator-registry
    ;;       (assq-delete-all 'file marginalia-annotator-registry))
    :hook (emacs-startup . marginalia-mode))

;; jinx : Enchanted Spell Checker
(use-package jinx
    :hook (emacs-startup . global-jinx-mode))

;; tempel - Simple templates for Emacs
(use-package tempel-collection :disabled t :after tempel)
(use-package tempel
    :disabled t
    :init
    (setq tempel-path (expand-file-name "tempel-templates" my/etc-dir))

    ;; Setup completion at point
    (defun tempel-setup-capf ()
        ;; Add the Tempel Capf to `completion-at-point-functions'.
        ;; `tempel-expand' only triggers on exact matches. Alternatively use
        ;; `tempel-complete' if you want to see all matches, but then you
        ;; should also configure `tempel-trigger-prefix', such that Tempel
        ;; does not trigger too often when you don't expect it. NOTE: We add
        ;; `tempel-expand' *before* the main programming mode Capf, such
        ;; that it will be tried first.
        (setq-local completion-at-point-functions
                    (cons #'tempel-expand
                          completion-at-point-functions)))

    (add-hook 'conf-mode-hook 'tempel-setup-capf)
    (add-hook 'prog-mode-hook 'tempel-setup-capf)
    (add-hook 'text-mode-hook 'tempel-setup-capf)

    ;; Optionally make the Tempel templates available to Abbrev,
    ;; either locally or globally. `expand-abbrev' is bound to C-x '.
    ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
    ;; (global-tempel-abbrev-mode)
    )

;; yasnippet : template system for Emacs
(use-package yasnippet-snippets :after yasnippet)
(use-package yasnippet
    :config (yas-reload-all)
    :hook (after-init . yas-global-mode))

;; yasnippet-capf
(use-package yasnippet-capf
    :disabled t
    :after cape)

;; languagetool : multilingual grammar, style, and spell checker
(use-package langtool
    :disabled t
    :config
    (setq langtool-default-language "en-US")
    (setq langtool-java-user-arguments '("-Dfile.encoding=UTF-8"))
    (setq langtool-language-tool-jar
          (expand-file-name "LanguageTool-6.4/languagetool-commandline.jar" my/software-path))
    (setq langtool-language-tool-server-jar
          (expand-file-name "LanguageTool-6.4/languagetool-server.jar" my/software-path)))

(provide 'my-init-completion)
;;; my-init-completion.el ends here

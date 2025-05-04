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
        (setq text-mode-ispell-word-completion nil)

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

    ;; Prompt indicator for `completing-read-multiple'.
    (when (< emacs-major-version 31)
        (advice-add #'completing-read-multiple :filter-args
                    (lambda (args)
                        (cons (format "[CRM%s] %s"
                                      (string-replace "[ \t]*" "" crm-separator)
                                      (car args))
                              (cdr args))))))

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
    (setq completion-styles '(basic flex)
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
    :hook ((after-init . vertico-mode)
           ;; This works with `file-name-shadow-mode' enabled.  When you are in
           ;; a sub-directory and use, say, `find-file' to go to your home '~/'
           ;; or root '/' directory, Vertico will clear the old path to keep
           ;; only your current input.
           (rfn-eshadow-update-overlay-hook . vertico-directory-tidy))
    :custom
    (vertico-count 15)
    (vertico-resize nil))

(use-package orderless
    :custom
    (completion-styles '(orderless basic))
    (completion-category-overrides
     '((file (styles basic partial-completion))))
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
    (corfu-auto t)
    (corfu-auto-prefix 2)
    (corfu-quit-no-match t)
    :bind (:map corfu-map
                ("SPC" . corfu-insert-separator)
                ("<tab>" . corfu-complete))
    :hook ((after-init . (lambda ()
                             (global-corfu-mode)
                             (corfu-popupinfo-mode t)
                             (with-eval-after-load 'savehist
                                 (corfu-history-mode 1)
                                 (add-to-list 'savehist-additional-variables 'corfu-history))))
           ((shell-mode eshell-mode) . (lambda ()
                                           (setq-local corfu-auto nil)
                                           (corfu-mode)))))

(use-package cape
    :bind ("C-c p" . cape-prefix-map)
    :init
    ;; eglot integration with cape
    (with-eval-after-load 'eglot
        (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
        (advice-add 'eglot-completion-at-point :around #'cape-wrap-noninterruptible))

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
    ;; Enable automatic preview at point in the *Completions* buffer. This is
    ;; relevant when you use the default completion UI.
    :hook (completion-list-mode . consult-preview-at-point-mode)
    :init
    ;; Tweak the register preview for `consult-register-load',
    ;; `consult-register-store' and the built-in commands.  This improves the
    ;; register formatting, adds thin separator lines, register sorting and hides
    ;; the window mode line.
    (advice-add #'register-preview :override #'consult-register-window)
    (setq register-preview-delay 0.5)

    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)

    ;; Optionally configure the register formatting. This improves the register
    ;; preview for `consult-register', `consult-register-load',
    ;; `consult-register-store' and the Emacs built-ins.
    (setq register-preview-delay 0.5
          register-preview-function #'consult-register-format)

    :config
    (setq consult-line-numbers-widen t)
    (setq consult-find-args
          (concat "find . -not ( "
                  "-path */.git* -prune "
                  "-or -path */.cache* -prune )"))
    (setq consult-ripgrep-args
          (concat "rg --null --line-buffered --color=never"
                  " --max-columns=1000 --path-separator /"
                  " --smart-case --no-heading --with-filename"
                  " --line-number --search-zip"
                  " --hidden"))

    ;; Optionally configure the narrowing key.
    ;; Both < and C-+ work reasonably well.
    (setq consult-narrow-key "<") ;; "C-+"

    ;; Optionally make narrowing help available in the minibuffer.
    ;; You may want to use `embark-prefix-help-command' or which-key instead.
    (keymap-set consult-narrow-map
                (concat consult-narrow-key " ?") #'consult-narrow-help)

    ;; Optionally configure preview. The default value
    ;; is 'any, such that any key triggers the preview.
    ;; (setq consult-preview-key 'any)
    ;; (setq consult-preview-key "M-.")
    ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
    ;; For some commands and buffer sources it is useful to configure the
    ;; :preview-key on a per-command basis using the `consult-customize' macro.
    (consult-customize
     consult-theme :preview-key '(:debounce 0.2 any)
     consult-ripgrep consult-git-grep consult-grep consult-man
     consult-bookmark consult-recent-file consult-xref
     consult--source-bookmark consult--source-file-register
     consult--source-recent-file consult--source-project-recent-file
     ;; :preview-key "M-."
     :preview-key '(:debounce 0.4 any)))

(use-package consult-dir
    :after consult)

;; consult-xref-stack : Navigate the Xref stack with Consult.
(use-package consult-xref-stack
    :disabled t
    :after consult
    :vc (:url "git@github.com:brett-lempereur/consult-xref-stack.git"
              :rev :newest)
    :bind (("C-," . consult-xref-stack-backward)))

;; consult-notes : select notes via consult
(use-package consult-notes
    :after consult
    :config
    (setq consult-notes-file-dir-sources
          `(("Org"    ?o ,(expand-file-name "Apps/org"       my/dropbox-path))
            ("Notes"  ?n ,(expand-file-name "Apps/org/notes" my/dropbox-path))
            ("Ledger" ?l ,(expand-file-name "Apps/ledger"    my/dropbox-path)))))

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
    :config (add-to-list 'yas-snippet-dirs
                         (expand-file-name "yasnippet/snippets" my/etc-dir))
    :hook (after-init . (lambda ()
                            (yas-reload-all)
                            (yas-global-mode))))

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

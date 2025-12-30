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

    ;; When using ‘grep’
    ;;  ‘-i’  Ignore case distinctions
    ;;  ‘-n’  Prefix each line of output with line number
    ;;  ‘-H’  Print the filename for each match.
    ;;  ‘-e’  Protect patterns beginning with a hyphen character, ‘-’
    ;; (setq grep-command "grep -i -nH -e ")
    ;;
    ;; Use ripgrep for grep
    (setq grep-command "rg -nS --no-heading ")
    (setq grep-find-ignored-directories
          '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".jj" ".git" ".hg"
            ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist"))

    ;; Find an existing buffer, even if it has a different name
    ;;   This avoids problems with symbolic links.
    (setq find-file-existing-other-name t)

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
    (setq abbrev-file-name (expand-file-name "abbrevs" my-etc-dir))
    (setq only-global-abbrevs nil))

;; hippie expand is dabbrev expand on steroids
;;
;; TODO: Perhaps I should spend a bit time investigating removing `hippie-exp'
;; in favor of `corfu' and `cape' behavior.
(use-package hippie-exp
    :ensure nil ;; emacs built-in
    :bind ([remap dabbrev-expand] . hippie-expand)
    :custom (hippie-expand-try-functions-list
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
               try-complete-lisp-symbol)))

(use-package vertico
    :hook ((after-init . (lambda ()
                             (vertico-mode)
                             (vertico-multiform-mode)))
           ;; This works with `file-name-shadow-mode' enabled.  When you are in
           ;; a sub-directory and use, say, `find-file' to go to your home '~/'
           ;; or root '/' directory, Vertico will clear the old path to keep
           ;; only your current input.
           (rfn-eshadow-update-overlay-hook . vertico-directory-tidy))
    :bind (:map vertico-map
                ;; ("RET" . vertico-directory-enter)
                ;; ("DEL" . vertico-directory-delete-word)
                ("M-d" . vertico-directory-delete-char)
                ("M-," . vertico-quick-insert)
                ("M-." . vertico-quick-exit)
                ("M-G" . vertico-multiform-grid)
                ("M-F" . vertico-multiform-flat)
                ("M-R" . vertico-multiform-reverse)
                ("M-U" . vertico-multiform-unobtrusive))
    :config
    (setq vertico-multiform-categories
          '((embark-keybinding buffer)
            (consult-grep buffer)
            (imenu buffer)))
    :custom
    (vertico-count 15)
    (vertico-resize nil))

(use-package orderless
    :custom
    (completion-styles '(orderless basic))
    (completion-category-overrides
     '((file (styles basic partial-completion))))
    ;; (orderless-style-dispatchers '(orderless-kwd-dispatch
    ;;                                orderless-affix-dispatch))
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
                             (with-eval-after-load 'savehist
                                 (corfu-history-mode 1)
                                 (add-to-list 'savehist-additional-variables 'corfu-history))))
           ((shell-mode eshell-mode) . (lambda ()
                                           (setq-local corfu-auto nil)
                                           (keymap-set corfu-map "RET" #'corfu-send)
                                           (corfu-mode)))))

(use-package cape
    :bind ("C-c p" . cape-prefix-map)
    :init
    ;; Sanitize the `pcomplete-completions-at-point' Capf.
    (when (< emacs-major-version 29)
        (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
        (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

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
    :bind (;; C-c bindings (mode-specific-map)
           ("C-c M-x" . consult-mode-command)
           ("C-c h"   . consult-history)
           ("C-c k"   . consult-kmacro)
           ("C-c m"   . consult-man)
           ("C-c i"   . consult-info)
           ([remap Info-search] . consult-info)

           ;; C-x bindings (ctl-x-map)
           ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
           ("C-x b"   . consult-buffer)              ;; orig. switch-to-buffer
           ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
           ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
           ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
           ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
           ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
           ("C-x C-r" . consult-recent-file)         ;; orig. find-file-readonly

           ;; Custom M-# bindings for fast register access
           ("M-#"   . consult-register-load)
           ("M-'"   . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
           ("C-M-#" . consult-register)

           ;; Other custom bindings
           ("M-y" . consult-yank-pop) ;; orig. yank-pop

           ;; M-g bindings (goto-map)
           ("M-g e"   . consult-compile-error)
           ("M-g f"   . consult-flymake)        ;; Alternative: consult-flycheck
           ("M-g g"   . consult-goto-line)      ;; orig. goto-line
           ("M-g M-g" . consult-goto-line)      ;; orig. goto-line
           ("M-g o"   . consult-outline)        ;; Alternative: consult-org-heading
           ("M-g m"   . consult-mark)
           ("M-g k"   . consult-global-mark)
           ("M-g i"   . consult-imenu)
           ("M-i"     . consult-imenu)
           ("M-g I"   . consult-imenu-multi)
           ("M-I"     . consult-imenu-multi)

           ;; M-s bindings (search-map)
           ("M-s d" . consult-fd)         ;; Alternative: consult-find
           ("M-s D" . consult-locate)
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
           ("M-e"   . consult-isearch-history) ;; orig. isearch-edit-string
           ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
           ("M-s l" . consult-line)            ;; needed by consult-line to detect isearch
           ("M-s L" . consult-line-multi)      ;; needed by consult-line to detect isearch

           ;; Minibuffer history
           :map minibuffer-local-map
           ("M-s" . consult-history)   ;; orig. next-matching-history-element
           ("M-r" . consult-history))  ;; orig. previous-matching-history-element
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
          (concat consult-ripgrep-args
                  " --hidden")) ;; consider hidden folders/files

    ;; Optionally configure the narrowing key.
    ;; Both < and C-+ work reasonably well.
    (setq consult-narrow-key "<") ;; "C-+"

    ;; Optionally make narrowing help available in the minibuffer.
    ;; You may want to use `embark-prefix-help-command' or which-key instead.
    ;; (keymap-set consult-narrow-map
    ;;             (concat consult-narrow-key " ?") #'consult-narrow-help)

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
    :bind (([remap list-directory] . consult-dir)
           :map vertico-map
           ("C-c C-d" . consult-dir)
           ("C-c C-j" . consult-dir-jump-file)))

;; consult-xref-stack : Navigate the Xref stack with Consult.
(use-package consult-xref-stack
    :disabled t
    :vc (:url "git@github.com:brett-lempereur/consult-xref-stack.git"
              :rev :newest)
    :bind (("C-," . consult-xref-stack-backward)))

;; consult-notes : select notes via consult
(use-package consult-notes
    :config
    (setq consult-notes-file-dir-sources
          `(("Org"    ?o ,(expand-file-name "Apps/org"       my-dropbox-path))
            ("Notes"  ?n ,(expand-file-name "Apps/org/notes" my-dropbox-path))
            ("Ledger" ?l ,(expand-file-name "Apps/ledger"    my-dropbox-path)))))

;; embark : Emacs Mini-Buffer Actions Rooted in Keymaps
(use-package embark
    :bind (("C-;"   . embark-act)
           ("M-;"   . embark-dwim)
           ("C-h B" . embark-bindings))
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
    ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
    ;; available in the *Completions* buffer, add it to the `completion-list-mode-map'.
    :bind (:map minibuffer-local-map
                ("M-A" . marginalia-cycle))
    :hook (emacs-startup . marginalia-mode))

;; jinx : Enchanted Spell Checker
(use-package jinx
    :bind (("M-$"   . jinx-correct)
           ("C-M-$" . jinx-languages))
    :hook (after-init . (lambda ()
                            (global-jinx-mode)
                            (with-eval-after-load 'vertico
                                (add-to-list 'vertico-multiform-categories
                                             '(jinx grid (vertico-grid-annotate . 20)))))))
;; tempel - Simple templates for Emacs
(use-package tempel-collection :disabled t :after tempel)
(use-package tempel
    :disabled t
    :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
           ("M-*" . tempel-insert))
    :init
    (setq tempel-path (expand-file-name "tempel-templates" my-etc-dir))

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
    :hook (after-init . yas-global-mode)
    :bind (("M-+" . yas-expand)
           ("M-*" . yas-insert-snippet))
    :config (add-to-list 'yas-snippet-dirs
                         (expand-file-name "yasnippet/snippets" my-etc-dir)))

;; yasnippet-capf
(use-package yasnippet-capf :disabled t)

;; languagetool : multilingual grammar, style, and spell checker
(use-package langtool
    :disabled t
    :bind (("C-x 4 w" . langtool-check)
           ("C-x 4 W" . langtool-check-done)
           ("C-x 4 c" . langtool-interactive-correction)
           ("C-x 4 l" . langtool-switch-default-language)
           ("C-x 4 4" . langtool-show-message-at-point))
    :config
    (setq langtool-default-language "en-US")
    (setq langtool-java-user-arguments '("-Dfile.encoding=UTF-8"))
    (setq langtool-language-tool-jar
          (expand-file-name "LanguageTool-6.4/languagetool-commandline.jar" my-software-path))
    (setq langtool-language-tool-server-jar
          (expand-file-name "LanguageTool-6.4/languagetool-server.jar" my-software-path)))

(provide 'my-init-completion)
;;; my-init-completion.el ends here

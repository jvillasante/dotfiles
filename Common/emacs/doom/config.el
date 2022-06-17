;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(when noninteractive
    (after! undo-tree
        (global-undo-tree-mode -1)))

(when (featurep! :editor evil)
    (after! evil
        (setq evil-want-fine-undo t) ;; By default while in insert all changes are one big blob. Be more granular
        (setq evil-cross-lines t)    ;; Make horizontal movement cross lines
        (setq evil-shift-width 4)    ;; evil shift width
        (setq evil-ex-search-persistent-highlight nil) ;; No highlight persistence on evil search

        ;; Make movement keys work like they should
        (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
        (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
        (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
        (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)))

(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "main"))

(after! tramp
    (setq tramp-use-ssh-controlmaster-options nil) ; Don't override SSH config.
    (setq tramp-default-method "ssh")    ; ssh is faster than scp and supports ports.
    (setq tramp-password-prompt-regexp   ; Add verification code support.
        (concat
            "^.*"
            (regexp-opt
                '("passphrase" "Passphrase"
                     "password" "Password"
                     "Verification code")
                t)
            ".*:\0? *")))

(after! emacs-everywhere
    (remove-hook 'emacs-everywhere-init-hooks #'org-mode)
    (add-hook 'emacs-everywhere-init-hooks #'markdown-mode)

    (setq emacs-everywhere-frame-parameters
        `((name . "emacs-everywhere")
             (width . 120)
             (height . 20))))

(after! yasnippet
    (push (expand-file-name "snippets/" doom-private-dir) yas-snippet-dirs))

(after! recentf
    (push (list (expand-file-name ".emacs.d/" +my/dotfiles-path)) recentf-exclude)
    (push (list (expand-file-name ".tmuxifier/" +my/dotfiles-path)) recentf-exclude)
    (push #'+org-is-agenda-file recentf-exclude)
    (push "~/.mail" recentf-exclude)
    (push "\\.git" recentf-exclude)
    (push "/tmp/" recentf-exclude)
    (push "/ssh:" recentf-exclude)
    (push "~/\\.emacs\\.d/.local" recentf-exclude)
    (push "~/mail" recentf-exclude)
    (push "/var" recentf-exclude)
    (push "/etc" recentf-exclude)
    (push "/usr" recentf-exclude)
    (push "\\.?ido\\.last$" recentf-exclude)
    (push "^/nix/store/" recentf-exclude)
    (push ".+\\.mp3$" recentf-exclude))

(setq +lookup-open-url-fn #'browse-url)
;; (setq +lookup-open-url-fn #'+lookup-xwidget-webkit-open-url-fn)
(after! dash-docs
    (set-docsets! 'c-mode "C")
    (set-docsets! 'c++-mode "C" "C++")
    (set-docsets! 'js2-mode "JavaScript" "JQuery")
    (set-docsets! 'nodejs-mode :remove "JQuery")
    (setq dash-docs-browser-func #'+lookup-xwidget-webkit-open-url-fn))

(after! projectile
    (setq
        projectile-switch-project-action 'projectile-dired
        projectile-require-project-root t
        projectile-project-root-files-bottom-up '(".projectile" ".git")
        projectile-sort-order 'recentf
        projectile-indexing-method 'hybrid)

    (setq projectile-ignored-project-function
        (lambda (project-root)
            (or (file-remote-p project-root)
                (string-prefix-p temporary-file-directory project-root)
                (string-prefix-p (expand-file-name ".password-store/" +my/home-path) project-root)
                (string-prefix-p (expand-file-name ".emacs.d/" +my/home-path) project-root)
                (string-prefix-p (expand-file-name ".emacs.d/" +my/dotfiles-path) project-root)
                (string-prefix-p (expand-file-name ".emacs.doom/" +my/dotfiles-path) project-root)
                (string-prefix-p (expand-file-name ".emacs.chemacs2/" +my/dotfiles-path) project-root)
                (string-prefix-p (expand-file-name ".emacs.rational/" +my/dotfiles-path) project-root)
                (string-prefix-p (expand-file-name ".tmuxifier/" +my/dotfiles-path) project-root)
                (string-prefix-p (expand-file-name ".bin/" +my/home-path) project-root)
                (string-prefix-p (expand-file-name ".bin/" +my/dotfiles-path) project-root)
                (string-prefix-p (expand-file-name ".oh-my-zsh/" +my/dotfiles-path) project-root)
                (string-prefix-p (expand-file-name "Workspace/Software/zig/" +my/home-path) project-root)
                (string-prefix-p (expand-file-name ".cargo/" +my/home-path) project-root)
                (string-prefix-p (expand-file-name ".rustup/" +my/home-path) project-root)))))

(after! ivy
    (setq
        ivy-display-style nil
        ivy-count-format "(%d/%d) "
        ivy-use-selectable-prompt t    ; much better than C-M-j
        ivy-use-virtual-buffers t      ; to make ivy-views appear on the buffers list
        ivy-virtual-abbreviate 'full   ; default is name
        ivy-initial-inputs-alist nil   ; remove initial ^ input.
        ivy-extra-directories nil      ; remove . and .. directory. (default value: ("../" "./"))
        ivy-height 10)

    ;; While in an ivy mini-buffer C-o shows a list of all possible actions one may take.
    ;; By default this is #'ivy-read-action-by-key however a better interface to this is using Hydra.
    (setq ivy-read-action-function #'ivy-hydra-read-action)

    (setq ivy-display-functions-alist
        '((counsel-irony . ivy-display-function-overlay)
             (ivy-completion-in-region . ivy-display-function-overlay))))

(after! avy
    (setq avy-all-windows t))

(after! counsel
    (setq
        counsel-rg-base-command "rg -S --no-heading --line-number --color never %s ."
        counsel-ag-base-command "ag -S --nocolor --nogroup %s"))

(after! smartparens
    (require 'smartparens-config)
    (show-smartparens-global-mode +1)
    (smartparens-global-mode 1)
    (show-paren-mode t)

    ;; global
    (sp-pair "`" "`" :actions nil)

    ;; c++
    (sp-with-modes '(c-mode c++-mode)
        (sp-local-pair "<" ">" :actions nil)

        ;; when you press RET, the curly braces automatically add another newline
        (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
        (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC") ("* ||\n[i]" "RET"))))

    ;; rust
    (sp-with-modes '(rustic-mode)
        (sp-local-pair "|" "|" :actions nil)))

(after! flyspell
    (setq flyspell-lazy-idle-seconds 2)) ; default is 2

(after! spell-fu
    (setq spell-fu-idle-delay 0.5) ; default is 0.25
    (add-hook! 'spell-fu-mode-hook
        (lambda ()
            (spell-fu-dictionary-add (spell-fu-get-ispell "es")))))

(after! flycheck
    (setq flycheck-temp-prefix "flycheck_tmp")
    (setq flycheck-indication-mode 'left-fringe))

(after! company
    (setq
        company-idle-delay 0.1
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t)

    (when (featurep! :editor evil)
        ;; make aborting less annoying.
        (add-hook! evil-normal-state-entry #'company-abort)))

(after! ws-butler
    (setq ws-butler-global-exempt-modes
        (append ws-butler-global-exempt-modes
            '(prog-mode org-mode))))

(after! python
    (setq python-shell-interpreter "python3"))

(after! elisp-mode
    (remove-hook 'emacs-lisp-mode-hook #'+emacs-lisp-extend-imenu-h))

(after! common-lisp
    (setq inferior-lisp-program "/usr/local/bin/sbcl")

    (setq slime-lisp-implementations
        `((ccl ("~/.cim/bin/ccl-1.9") :coding-system utf-8-unix)
             (alisp ("/usr/local/bin/alisp") :coding-system utf-8-unix)
             (ecl ("/usr/local/bin/ecl"))  ; :coding-system utf-8-unix)
             (cmucl ("/usr/local/bin/cmucl") :coding-system utf-8-unix)
             (sbcl ("/usr/local/bin/sbcl" "+R" "-l" "~/.sbclrc") :coding-system utf-8-unix)
             (abcl ("~/.cim/bin/abcl-1.3.1" "-XX:MaxPermSize=256m" "-Dfile.encoding=UTF-8") :coding-system utf-8-unix)
             (clisp ("/usr/local/bin/clisp") :coding-system utf-8-unix)))

    (setq slime-default-lisp 'sbcl)
    (setq slime-net-coding-system 'utf-8-unix))

(after! format
    (setq +format-on-save-enabled-modes
        '(not emacs-lisp-mode ; elisp's mechanisms are good enough
             sql-mode         ; sqlformat is currently broken
             tex-mode         ; latexindent is broken
             latex-mode))

    ;; Do not format with lsp, use `format` instead
    (setq +format-with-lsp nil))

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

(after! lsp-mode
    (setq lsp-restart 'ignore
        lsp-headerline-breadcrumb-enable nil
        lsp-enable-symbol-highlighting t
        lsp-enable-indentation nil
        lsp-eldoc-enable-hover t
        lsp-eldoc-render-all nil
        lsp-signature-render-documentation nil
        lsp-signature-auto-activate nil
        lsp-signature-doc-lines 1
        lsp-auto-guess-root nil
        lsp-enable-file-watchers nil
        lsp-enable-on-type-formatting nil)

    ;; Zig
    (setq lsp-zig-zls-executable
        (expand-file-name "zig/zls/zig-out/bin/zls" +my/software-path))

    ;; C++
    (setq lsp-clients-clangd-args
        '("-j=4"
             "--malloc-trim"
             "--log=error"
             "--background-index"
             "--clang-tidy"
             "--cross-file-rename"
             "--completion-style=detailed"
             "--pch-storage=memory"
             "--header-insertion=never"
             "--header-insertion-decorators=0"))
    (add-to-list 'flycheck-disabled-checkers 'c/c++-clang)
    (add-to-list 'flycheck-disabled-checkers 'c/c++-gcc)
    (after! lsp-clangd (set-lsp-priority! 'clangd 2)))

(after! lsp-ui
    (setq lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-code-actions nil)

    (setq lsp-ui-peek-enable t
        lsp-ui-peek-always-show t
        lsp-ui-peek-show-directory t)

    (setq lsp-ui-doc-enable t)
    (setq lsp-ui-imenu-enable t))

(after! eglot
    (add-to-list 'flycheck-disabled-checkers 'c/c++-clang)
    (add-to-list 'flycheck-disabled-checkers 'c/c++-gcc)
    (setq eldoc-echo-area-use-multiline-p nil)
    (setq eglot-extend-to-xref t)
    (add-to-list 'eglot-server-programs
        `(rust-mode . ("rust-analyzer"))
        `(c-mode c++-mode
             . ("/usr/bin/clangd"
                   "-j=4"
                   "--malloc-trim"
                   "--log=error"
                   "--background-index"
                   "--clang-tidy"
                   "--cross-file-rename"
                   "--completion-style=detailed"
                   "--pch-storage=memory"
                   "--header-insertion=never"
                   "--header-insertion-decorators=0"))))

(after! rustic
    (setq
        rustic-lsp-server 'rust-analyzer
        rustic-format-on-save nil
        lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-inlay-hints-mode nil
        lsp-rust-analyzer-server-display-inlay-hints nil))

(after! elfeed
    (setq elfeed-search-title-min-width 60)
    (setq elfeed-search-title-max-width 100)
    (setq elfeed-search-trailing-width 0)
    (setq elfeed-search-filter "@6-months-ago +unread")
    (setq elfeed-db-directory (expand-file-name "Apps/elfeed/elfeed_db" +my/dropbox-path)))

(after! elfeed-org
    (setq rmh-elfeed-org-files (list (expand-file-name "Apps/elfeed/elfeed.org" +my/dropbox-path))))

(after! elfeed-search
    (add-hook! 'elfeed-search-mode-hook 'elfeed-update)
    (when (featurep! :editor evil)
        (set-evil-initial-state! 'elfeed-search-mode 'normal)))

(after! elfeed-show-mode
    (when (featurep! :editor evil)
        (set-evil-initial-state! 'elfeed-show-mode 'normal)))

(after! evil-snipe
    (push 'elfeed-show-mode   evil-snipe-disabled-modes)
    (push 'elfeed-search-mode evil-snipe-disabled-modes))

(after! ediff
    (defun +my/ediff-copy-both-to-C ()
        (interactive)
        (ediff-copy-diff ediff-current-difference nil 'C nil
            (concat
                (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
    (defun +my/add-d-to-ediff-mode-map () (define-key ediff-mode-map "C" '+my/ediff-copy-both-to-C))
    (add-hook 'ediff-keymap-setup-hook '+my/add-d-to-ediff-mode-map))

(after! magit
    ;; Have magit-status go full screen and quit to previous configuration.
    ;; Taken from http://whattheemacsd.com/setup-magit.el-01.html#comment-748135498
    ;; and http://irreal.org/blog/?p=2253
    (defadvice magit-status (around magit-fullscreen activate)
        (window-configuration-to-register :magit-fullscreen)
        ad-do-it
        (delete-other-windows))
    (defadvice magit-quit-window (after magit-restore-screen activate)
        (jump-to-register :magit-fullscreen))

    (setq
        git-commit-summary-max-length 80
        vc-handled-backends (delq 'Git vc-handled-backends)))

(after! dired
    (setq dired-ls-F-marks-symlinks t) ;; mark symlinks
    (setq dired-recursive-copies 'always) ;; Never prompt for recursive copies of a directory
    (setq dired-recursive-deletes 'always) ;; Never prompt for recursive deletes of a directory
    (setq dired-dwim-target t) ;; makes dired guess the target directory
    (setq dired-auto-revert-buffer t) ;; auto-revert dired buffers if file changed on disk
    (setq projectile-switch-project-action 'projectile-dired) ;; dired loads on project switch

    (let ((gls "/usr/local/bin/gls"))
        (if (file-exists-p gls) (setq insert-directory-program gls)))

    ;; Dired listing switches
    ;;  -a : Do not ignore entries starting with .
    ;;  -l : Use long listing format.
    ;;  -h : Human-readable sizes like 1K, 234M, ..
    ;;  -v : Do natural sort .. so the file names starting with . will show up first.
    ;;  -F : Classify filenames by appending '*' to executables, '/' to directories, etc.
    (setq dired-listing-switches (if (eq system-type 'windows-nt)
                                     "-alh"
                                     "-alhvF --group-directories-first")))

(after! dired-quick-sort
    (dired-quick-sort-setup))

(after! pdf-tools
    (pdf-tools-install)

    (defun +my/config-pdf ()
        (if (eq (modus-themes--current-theme) 'modus-vivendi)
            (pdf-view-midnight-minor-mode 1)
            (pdf-view-midnight-minor-mode -1)))
    (add-hook! 'pdf-view-mode-hook '+my/config-pdf))

(after! neotree
    (setq
        neo-theme 'ascii
        neo-window-width 42
        neo-smart-open t
        neo-create-file-auto-open nil
        neo-show-updir-line nil
        neo-show-hidden-files t
        neo-auto-indent-point t
        neo-vc-integration nil
        neo-autorefresh nil)

    ;; When running `projectile-switch-project`, `neotree` will change root automatically.
    (setq projectile-switch-project-action 'neotree-projectile-action)

    ;; Hidden files
    (setq neo-hidden-regexp-list
        '(;; vcs folders
             "^\\.\\(?:git\\|hg\\|svn\\)$"
             ;; compiled files
             "\\.\\(?:pyc\\|o\\|elc\\|lock\\|css.map\\|class\\)$"
             ;; generated files, caches or local pkgs
             "^\\(?:node_modules\\|vendor\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"
             ;; org-mode folders
             "^\\.\\(?:sync\\|export\\|attach\\)$"
             ;; temp files
             "~$"
             "^#.*#$"
             ;; Others
             "^\\.\\(cache\\|tox\\|coverage\\)$"
             "^\\.\\(DS_Store\\|python\\-version\\)"
             "^\\(htmlcov\\)$" "\\.elcs$"
             "^\\.coverage\\..*" "\\.ipynb.*$" "\\.py[cod]$"
             "^\\.#.*$" "^__pycache__$"
             "\\.gcda$" "\\.gcov$" "\\.gcno$" "\\.lo$" "\\.o$" "\\.so$"
             "^\\.cproject$" "^\\.project$" "^\\.projectile$"
             "\\.egg\-info$")))

(after! treemacs
    (setq treemacs-no-png-images t)

    (defvar treemacs-file-ignore-extensions '()
        "File extension which `treemacs-ignore-filter' will ensure are ignored")
    (defvar treemacs-file-ignore-globs '()
        "Globs which will are transformed to `treemacs-file-ignore-regexps' which `treemacs-ignore-filter' will ensure are ignored")
    (defvar treemacs-file-ignore-regexps '()
        "RegExps to be tested to ignore files, generated from `treeemacs-file-ignore-globs'")
    (setq treemacs-file-ignore-extensions
        '(;; LaTeX
             "aux"
             "ptc"
             "fdb_latexmk"
             "fls"
             "synctex.gz"
             "toc"
             ;; LaTeX - glossary
             "glg"
             "glo"
             "gls"
             "glsdefs"
             "ist"
             "acn"
             "acr"
             "alg"
             ;; LaTeX - pgfplots
             "mw"
             ;; LaTeX - pdfx
             "pdfa.xmpi"))
    (setq treemacs-file-ignore-globs
        '(;; LaTeX
             "*/_minted-*"
             ;; AucTeX
             "*/.auctex-auto"
             "*/_region_.log"
             "*/_region_.tex"))
    (defun treemacs-file-ignore-generate-regexps ()
        "Generate `treemacs-file-ignore-regexps' from `treemacs-file-ignore-globs'"
        (setq treemacs-file-ignore-regexps (mapcar 'dired-glob-regexp treemacs-file-ignore-globs)))
    (if (equal treemacs-file-ignore-globs '()) nil (treemacs-file-ignore-generate-regexps))
    (defun treemacs-ignore-filter (file full-path)
        "Ignore files specified by `treemacs-file-ignore-extensions', and `treemacs-file-ignore-regexps'"
        (or (member (file-name-extension file) treemacs-file-ignore-extensions)
            (let ((ignore-file nil))
                (dolist (regexp treemacs-file-ignore-regexps ignore-file)
                    (setq ignore-file (or ignore-file (if (string-match-p regexp full-path) t nil)))))))
    (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-filter))

(after! pass
    (setq password-store-password-length 25))

(after! evil-org
    (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

(after! deft
    (setq
        deft-directory (expand-file-name "Apps/org/notes" +my/dropbox-path)
        deft-extensions '("org" "md" "txt")
        deft-default-extension "org"
        deft-recursive nil
        deft-use-filename-as-title nil
        deft-use-filter-string-for-filename t
        deft-file-naming-rules '((noslash . "-")
                                    (nospace . "-")
                                    (case-fn . downcase))
        deft-auto-save-interval 0))

(after! org-roam
    (setq
        org-roam-directory (expand-file-name "Apps/org/roam" +my/dropbox-path)
        org-roam-completion-everywhere t))

(after! org
    (defun +my/org-custom-faces ()
        "Set up a nice proportional font, in different sizes, for the headlines.
        The fonts listed will be tried in sequence, and the first one found will be used."
        (when (display-graphic-p)
            (let* ((variable-tuple
                       (cond
                           ((x-list-fonts "Iosevka")         '(:font   "Iosevka"))
                           ((x-list-fonts "Source Code Pro") '(:font   "Source Code Pro"))
                           ((x-list-fonts "JetBrains Mono")  '(:font   "JetBrains Mono"))
                           ((x-list-fonts "Source Sans Pro") '(:font   "Source Sans Pro"))
                           ((x-list-fonts "Lucida Grande")   '(:font   "Lucida Grande"))
                           ((x-list-fonts "Verdana")         '(:font   "Verdana"))
                           ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                           (nil (warn "Cannot find Font."))))
                      (base-font-color (face-foreground 'default nil 'default))
                      (headline `(:inherit default :weight bold :foreground ,base-font-color)))
                (custom-theme-set-faces
                    'user
                    `(org-level-8        ((t (,@headline ,@variable-tuple :height 1.04))))
                    `(org-level-7        ((t (,@headline ,@variable-tuple :height 1.07))))
                    `(org-level-6        ((t (,@headline ,@variable-tuple :height 1.10))))
                    `(org-level-5        ((t (,@headline ,@variable-tuple :height 1.13))))
                    `(org-level-4        ((t (,@headline ,@variable-tuple :height 1.16))))
                    `(org-level-3        ((t (,@headline ,@variable-tuple :height 1.19))))
                    `(org-level-2        ((t (,@headline ,@variable-tuple :height 1.22))))
                    `(org-level-1        ((t (,@headline ,@variable-tuple :height 1.25))))
                    `(org-headline-done  ((t (,@headline ,@variable-tuple :strike-through t))))
                    `(org-document-title ((t (,@headline ,@variable-tuple :height 1.25 :underline nil))))))))

    ;; Latex previews in org-mode
    (plist-put org-format-latex-options :background 'default)

    ;; To get the most out of themes
    (setq
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t)

    ;; settings
    (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|org\\.txt\\)$" . org-mode))
    (setq org-startup-indented t)
    (setq org-startup-folded t)
    (setq org-cycle-separator-lines 2)
    (setq org-blank-before-new-entry '((heading . t) (plain-list-item . nil)))
    (setq org-agenda-file-regexp "\\`[^.].*\\.\\(org\\.txt\\|org\\)\\'")
    (setq org-log-done t)
    (setq org-startup-with-inline-images t)
    (setq org-image-actual-width nil)
    (setq org-src-fontify-natively t)
    (setq org-src-tab-acts-natively t)
    (setq org-imenu-depth 8)
    (setq org-hide-emphasis-markers t) ;; hide the emphasis markup (e.g. /.../ for italics, *...* for bold, etc.)
    (setq
        org-indent-mode t
        org-adapt-indentation nil ;; prevent demoting heading also shifting text inside sections
        org-src-preserve-indentation nil
        org-edit-src-content-indentation 2)

    ;; Show the daily agenda by default.
    (setq org-agenda-span 'day)
    (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t% s")
             (todo   . " ")
             (tags   . " %i %-12:c")
             (search . " %i %-12:c")))
    (setq org-agenda-hide-tags-regexp ".") ;; ask the agenda to hide any tag (.) that may be present.
    (setq org-capture-templates            ;; set our capture templates
        `(("i" "Inbox" entry (file "inbox.org")
              ,(concat "* TODO %?\n"
                   "/Entered on/ %U"))
             ("n" "Note" entry (file "notes.org")
                 ,(concat "* Note (%a)\n"
                      "/Entered on/ %U\n" "\n" "%?"))))

    (if (daemonp)
        (add-hook 'after-make-frame-functions
            (lambda (frame)
                (with-selected-frame frame
                    (+my/org-custom-faces))))
        (+my/org-custom-faces)))

;; https://orgmode.org/worg/org-tutorials/encrypting-files.html
(use-package! org-crypt
    :after org
    :config
    (setq org-crypt-disable-auto-save nil) ;; don't ask to disable auto-save
    (setq org-tags-exclude-from-inheritance (quote ("crypt")))
    (setq org-crypt-key nil)
    (setq org-crypt-key user-mail-address))

(use-package! visual-regexp
    :commands (vr/query-replace vr/replace))

(load! "+ui")
(load! "+config")
(load! "+bindings")
(load! "+hydras")

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.


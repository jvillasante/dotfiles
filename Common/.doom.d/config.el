;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(load! "+early-init.el")

(when noninteractive
    (after! undo-tree
        (global-undo-tree-mode -1)))

(after! evil
    (setq evil-want-fine-undo t))

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
    (setq emacs-everywhere-frame-parameters
        `((name . "emacs-everywhere")
             (width . 80)
             (height . 20))))

(after! yasnippet
    (push (expand-file-name "snippets/" doom-private-dir) yas-snippet-dirs))

(after! recentf
    (push (list (expand-file-name ".emacs.d/" +my/dropbox-path)) recentf-exclude)
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

(after! lookup
    (setq
        dash-docs-browser-func #'browse-url
        counsel-dash-browser-func #'browse-url
        +lookup-open-url-fn #'browse-url))

(after! projectile
    (setq
        projectile-require-project-root t
        projectile-project-root-files-bottom-up '(".projectile" ".git")
        projectile-sort-order 'recentf
        projectile-indexing-method 'hybrid)

    (setq projectile-ignored-project-function
        (lambda (project-root)
            (or (file-remote-p project-root)
                (string-prefix-p temporary-file-directory project-root)
                (string-prefix-p (expand-file-name ".emacs.d/" +my/home-path) project-root)
                (string-prefix-p (expand-file-name ".emacs.d/" +my/dotfiles-path) project-root)
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

    (sp-with-modes '(c-mode c++-mode)
        ;; (sp-local-pair "<" ">")
        (sp-local-pair "<" ">" :actions nil)

        ;; when you press RET, the curly braces automatically add another newline
        (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
        (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC") ("* ||\n[i]" "RET"))))

    (sp-with-modes '(rustic-mode)
        (sp-local-pair "|" "|")))

(after! flyspell
    (setq flyspell-lazy-idle-seconds 3))

(after! flycheck
    (setq flycheck-indication-mode 'left-fringe))

(after! company
    (setq
        company-idle-delay 0.1
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t)

    ;; make aborting less annoying.
    (add-hook! evil-normal-state-entry #'company-abort))

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
    (add-to-list '+format-on-save-enabled-modes 'text-mode 'append)
    (add-to-list '+format-on-save-enabled-modes 'web-mode 'append)
    (add-to-list '+format-on-save-enabled-modes 'gitignore-mode 'append)
    (add-to-list '+format-on-save-enabled-modes 'makefile-gmake-mode 'append)

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
    (setq
        lsp-restart 'ignore
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
        (expand-file-name "zig/zls/zig-cache/bin/zls" +my/software-path))

    ;; Rust
    (setq
        lsp-rust-server 'rust-analyzer
        lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-server-display-inlay-hints nil)

    ;; C++
    (setq lsp-clients-clangd-args
        '("-j=4"
             "--log=error"
             "--background-index"
             "--clang-tidy"
             "--completion-style=detailed"
             "--pch-storage=memory"
             "--header-insertion=never"
             "--header-insertion-decorators=0"))
    (add-to-list 'flycheck-disabled-checkers 'c/c++-clang)
    (add-to-list 'flycheck-disabled-checkers 'c/c++-gcc)
    (after! lsp-clangd (set-lsp-priority! 'clangd 2)))

(after! lsp-ui
    (setq
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-code-actions nil)

    (setq
        lsp-ui-peek-enable t
        lsp-ui-peek-always-show t
        lsp-ui-peek-show-directory t)

    (setq
        lsp-ui-doc-enable nil)

    (setq
        lsp-ui-imenu-enable t))

(after! rustic
    (setq
        rustic-lsp-server 'rust-analyzer
        rustic-format-on-save nil))

(after! deft
    (setq
        deft-directory (expand-file-name "Apps/org/notes" +my/dropbox-path)
        deft-extensions '("org" "md" "txt")
        deft-default-extension "org"
        deft-recursive t
        deft-use-filename-as-title nil
        deft-use-filter-string-for-filename t
        deft-file-naming-rules '((noslash . "-")
                                    (nospace . "-")
                                    (case-fn . downcase))
        deft-auto-save-interval 0))

(after! org-roam
    (setq
        org-roam-directory (expand-file-name "Apps/org/roam" +my/dropbox-path)
        org-roam-dailies-directory (expand-file-name "Apps/org/roam/daily" +my/dropbox-path)
        org-roam-db-gc-threshold most-positive-fixnum
        org-roam-tag-sources '(prop last-directory)
        org-id-link-to-org-use-id t)

    (setq org-roam-capture-templates
        '(("p" "personal" plain #'org-roam-capture--get-point
              :file-name "personal/${slug}"
              :head "#+title: ${title}\n#+ROAM_TAGS: %^{org-roam-tags}\n\n%?"
              :unnarrowed t :jump-to-captured t)
             ("w" "work" plain #'org-roam-capture--get-point
                 :file-name "work/${slug}"
                 :head "#+title: ${title}\n#+ROAM_TAGS: %^{org-roam-tags}\n\n%?"
                 :unnarrowed t :jump-to-captured t)))

    (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
              #'org-roam-capture--get-point
              "* %?"
              :file-name "daily/%<%Y-%m-%d>"
              :head "#+title: %<%Y-%m-%d>\n\n"))))

(after! elfeed
    (setq elfeed-search-filter "@6-months-ago +unread")
    (setq elfeed-db-directory (expand-file-name "Apps/elfeed/elfeed_db" +my/dropbox-path)))

(after! elfeed-org
    (setq rmh-elfeed-org-files (list (expand-file-name "Apps/elfeed/elfeed.org" +my/dropbox-path))))

(after! elfeed-search
    (set-evil-initial-state! 'elfeed-search-mode 'normal))

(after! elfeed-show-mode
    (set-evil-initial-state! 'elfeed-show-mode 'normal))

(after! evil-snipe
    (push 'elfeed-show-mode   evil-snipe-disabled-modes)
    (push 'elfeed-search-mode evil-snipe-disabled-modes))

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

    ;; Taken from: https://jakemccrary.com/blog/2020/11/14/speeding-up-magit/
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent

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
    ;; default value for dired: "-al"
    (setq dired-listing-switches (if (eq system-type 'windows-nt)
                                     "-alh"
                                     "-alhvF --group-directories-first")))

(after! dired-quick-sort
    (dired-quick-sort-setup))

(after! neotree
    (setq
        neo-theme 'ascii
        neo-window-width 38
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
    (setq treemacs-dotfiles-regex
        (concat
            "\\("   "^\\.\\(git\\|cache\\|tox\\|coverage\\)$" "\\)" "\\|"
            "\\("   "^\\.\\(DS_Store\\|python\\-version\\)"   "\\)" "\\|"
            "\\("   "^\\(htmlcov\\|node_modules\\)$"          "\\)" "\\|"
            "\\("   "\\.elcs$"                                "\\)" "\\|"
            "\\("   "^\\.coverage\\..*"                       "\\)" "\\|"
            "\\("   "\\.ipynb.*$"                             "\\)" "\\|"
            "\\("   "\\.py[cod]$"                             "\\)" "\\|"
            "\\("   "~$"                                      "\\)" "\\|"
            "\\("   "^#.*#$"                                  "\\)" "\\|"
            "\\("   "^\\.#.*$"                                "\\)" "\\|"
            "\\("   "^__pycache__$"                           "\\)" "\\|"
            "\\("   "\\.gcda$"                                "\\)" "\\|"
            "\\("   "\\.gcov$"                                "\\)" "\\|"
            "\\("   "\\.gcno$"                                "\\)" "\\|"
            "\\("   "\\.lo$"                                  "\\)" "\\|"
            "\\("   "\\.o$"                                   "\\)" "\\|"
            "\\("   "\\.so$"                                  "\\)" "\\|"
            "\\("   "^\\.cproject$"                           "\\)" "\\|"
            "\\("   "^\\.project$"                            "\\)" "\\|"
            "\\("   "^\\.projectile$"                         "\\)" "\\|"
            "\\("   "\\.egg\-info$"                           "\\)" "\\|"
            "\\("   "^\\..+"                                  "\\)")))

(after! evil-org
    (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

(after! pass
    (setq password-store-password-length 25))

(after! org
    ;; doom fold level
    (setq +org-initial-fold-level 1)

    ;; Latex previews in org-mode
    (plist-put org-format-latex-options :background 'default)

    ;; To get the most out of themes
    (setq
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t)

    ;; settings
    (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|org\\.txt\\)$" . org-mode))
    (setq org-agenda-window-setup (quote current-window)) ;; open agenda in current window
    (setq org-startup-indented t)
    (setq org-indent-mode t)
    (setq org-startup-folded t)
    (setq org-adapt-indentation nil) ;; prevent demoting heading also shifting text inside sections
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

    ;; set up a nice proportional font, in different sizes, for the headlines.
    ;; the fonts listed will be tried in sequence, and the first one found will be used.
    (let* ((variable-tuple
               (cond
                   ((x-list-fonts   "JetBrains Mono") '(:font   "JetBrains Mono"))
                   ((x-list-fonts   "Source Code Pro") '(:font   "Source Pro Pro"))
                   ((x-list-fonts   "Lucida Grande")   '(:font   "Lucida Grande"))
                   ((x-list-fonts   "Verdana")         '(:font   "Verdana"))
                   ((x-family-fonts "Sans Serif")      '(:family "Sans Serif"))
                   (nil (warn "Cannot find a Sans Serif Font."))))
              (base-font-color (face-foreground 'default nil 'default))
              (headline `(:inherit default :weight bold :foreground ,base-font-color)))
        (custom-theme-set-faces
            'user
            `(org-level-8        ((t (,@headline ,@variable-tuple))))
            `(org-level-7        ((t (,@headline ,@variable-tuple))))
            `(org-level-6        ((t (,@headline ,@variable-tuple))))
            `(org-level-5        ((t (,@headline ,@variable-tuple))))
            `(org-level-4        ((t (,@headline ,@variable-tuple :height 1.05))))
            `(org-level-3        ((t (,@headline ,@variable-tuple :height 1.05))))
            `(org-level-2        ((t (,@headline ,@variable-tuple :height 1.15))))
            `(org-level-1        ((t (,@headline ,@variable-tuple :height 1.25))))
            `(org-headline-done  ((t (,@headline ,@variable-tuple :strike-through t))))
            `(org-document-title ((t (,@headline ,@variable-tuple :height 1.30 :underline nil))))))

    ;; todos
    (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                   (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MIGRATED(m@/!)" "PHONE" "MEETING"))))

    (setq org-todo-state-tags-triggers
        (quote (("CANCELLED" ("CANCELLED" . t))
                   ("WAITING" ("WAITING" . t))
                   ("MIGRATED" ("MIGRATED" . t))
                   ("HOLD" ("WAITING") ("HOLD" . t))
                   (done ("WAITING") ("HOLD"))
                   ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                   ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                   ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

    ;; organizer directory
    (setq org-directory (expand-file-name "Apps/org/" +my/dropbox-path))
    (setq org-default-notes-file (concat org-directory "inbox.org"))
    (setq +my/org-work-file (concat org-directory "work.org"))
    (setq +my/org-bookmarks-file (concat org-directory "bookmarks.org"))

    ;; agenda
    (setq org-agenda-files (list org-directory))
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-agenda-skip-deadline-if-done t)

    ;; Tags with fast selection keys
    (setq org-tag-alist (quote
                            ((:startgroup)
                                ("@errand" . ?e)
                                ("@office" . ?o)
                                ("@home" . ?H)
                                (:endgroup)
                                ("WAITING" . ?w)
                                ("MIGRATED" . ?M)
                                ("HOLD" . ?h)
                                ("IDEA" . ?i)
                                ("PERSONAL" . ?P)
                                ("DRAFT" . ?D)
                                ("WORK" . ?W)
                                ("NOTE" . ?n)
                                ("CANCELLED" . ?c)
                                ("FLAGGED" . ??))))

    ;; capture
    (setq org-capture-templates
        (quote (
                   ("t" "Personal Task" entry (file+headline org-default-notes-file "Tasks")
                       "* TODO %?\nSCHEDULED: %U\n")
                   ("w" "Work Task" entry (file+headline +my/org-work-file "Tasks")
                       "* TODO %?\nSCHEDULED: %U\n")
                   ("m" "Meeting" entry (file+headline org-default-notes-file "Meetings")
                       "* MEETING with %? :MEETING:\n%U")
                   ("i" "Idea" entry (file+headline org-default-notes-file "Ideas")
                       "* %? :IDEA:\n%U\n%a\n")
                   ("n" "Note" entry (file+headline org-default-notes-file "Notes")
                       "* %? :NOTE:\n%U\n%a\n")
                   ("b" "Bookmark" entry (file+headline +my/org-bookmarks-file "Bookmarks")
                       "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
                   )))

    ;; refiling
    (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                        (org-agenda-files :maxlevel . 9)))))

;; https://orgmode.org/worg/org-tutorials/encrypting-files.html
(use-package! org-crypt
    :after org
    :init   (org-crypt-use-before-save-magic)
    :custom (org-crypt-key user-mail-address)
    :config
    (setq org-crypt-disable-auto-save nil) ;; don't ask to disable auto-save
    (setq org-tags-exclude-from-inheritance (quote ("crypt"))))

(use-package! visual-regexp
    :commands (vr/query-replace vr/replace))

(use-package! super-save
    :config
    ;; this just defines when to trigger an auto-save (e.g. when you change focus out of the current window)
    (setq super-save-triggers
        '(ace-window evil-window-down evil-window-up evil-window-left
             evil-window-right +ivy/projectile-find-file +ivy/switch-workspace-buffer
             +ivy/switch-workspace-buffer-other-window))
    (super-save-mode +1))

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


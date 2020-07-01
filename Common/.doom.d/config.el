;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(load! "+early-init.el")

(when noninteractive
    (after! undo-tree
        (global-undo-tree-mode -1)))

(after! evil
  (setq evil-want-fine-undo t))

(after! dash-docs
    (setq dash-docs-browser-func #'browse-url))

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
    (push "/usr" recentf-exclude)
    (push "\\.?ido\\.last$" recentf-exclude)
    (push "^/nix/store/" recentf-exclude)
    (push ".+\\.mp3$" recentf-exclude))

(after! projectile
    (defun +my/projectile-ignore-project-p (project-root)
        (string-match-p "/\\.emacs\\.d/\\.local/straight/repos" project-root))

    (setq
        projectile-require-project-root t
        projectile-project-root-files-bottom-up '(".projectile" ".git")
        projectile-sort-order 'recentf
        projectile-indexing-method 'hybrid
        projectile-ignored-project-function #'+my/projectile-ignore-project-p))

(after! ivy
    (setq ivy-display-style nil
        ivy-count-format "(%d/%d) "
        ivy-use-selectable-prompt t    ; much better than C-M-j
        ivy-use-virtual-buffers t      ; to make ivy-views appear on the buffers list
        ivy-virtual-abbreviate 'full   ; default is name
        ivy-initial-inputs-alist nil   ; remove initial ^ input.
        ivy-extra-directories nil      ; remove . and .. directory. (default value: ("../" "./"))
        ivy-height 10)

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
    (show-paren-mode t))

(after! flyspell-lazy
    (add-hook 'prog-mode-hook 'flyspell-lazy-mode))

(after! flycheck
    (setq
        flycheck-indication-mode 'left-fringe))

(after! company
    (setq
        company-idle-delay 0.2
        company-show-numbers nil))

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

(after! lsp-mode
    (setq
        lsp-eldoc-render-all nil
        lsp-signature-render-documentation nil
        lsp-signature-auto-activate nil
        lsp-signature-doc-lines 1)

    (setq
        lsp-auto-guess-root nil
        lsp-enable-file-watchers nil
        lsp-enable-on-type-formatting nil)

    ;; Rust
    (setq
        lsp-rust-server 'rust-analyzer)

    ;; C++
    (setq lsp-clients-clangd-args
        '("-j=2"
             "--log=error"
             ;; "--compile-commands-dir=build"
             "--background-index"
             "--clang-tidy"
             "--completion-style=detailed"
             "--pch-storage=memory"
             "--header-insertion=never"
             "--header-insertion-decorators=0")))

(after! lsp-ui
    (setq
        lsp-ui-peek-enable t
        lsp-ui-sideline-enable nil))

(after! rustic
    ;; (sp-local-pair 'rustic-mode "|" "|") ;; closures, can interefere bitwise operators
    ;; (sp-local-pair 'rustic-mode "<" ">") ;; messes with comparison operator
    ;; (sp-local-pair 'rustic-mode "'" "'" :actions nil) ;; lifetime annotations

    (setq
        rustic-lsp-server 'rust-analyzer
        rustic-lsp-client 'lsp-mode))

(after! deft
    (setq
        deft-directory (expand-file-name "Apps/org/notes" +my/dropbox-path)
        deft-extensions '("org" "md" "txt")
        deft-default-extension "org"
        deft-recursive t
        deft-use-filename-as-title t
        deft-use-filter-string-for-filename t
        deft-file-naming-rules '((noslash . "-")
                                    (nospace . "-")
                                    (case-fn . downcase))
        deft-auto-save-interval 0))

(after! elfeed
    (setq elfeed-search-filter "@6-months-ago +unread")
    (setq elfeed-db-directory (expand-file-name "Apps/elfeed/elfeed_db" +my/dropbox-path)))

(after! elfeed-org
    (setq rmh-elfeed-org-files (list (expand-file-name "Apps/elfeed/elfeed.org" +my/dropbox-path))))

(after! magit
    ;; Temporary workaround for +magit/quit hang with lots of buffers
    ;; (define-key magit-status-mode-map [remap magit-mode-bury-buffer] nil)

    (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
    (setq-default git-magit-status-fullscreen t)
    (setq
        magit-completing-read-function 'magit-builtin-completing-read
        magit-push-always-verify nil)

    (setq
        git-commit-summary-max-length 80
        vc-handled-backends (delq 'Git vc-handled-backends)))

(after! dired
    ;; mark symlinks
    (setq dired-ls-F-marks-symlinks t)
    ;; Never prompt for recursive copies of a directory
    (setq dired-recursive-copies 'always)
    ;; Never prompt for recursive deletes of a directory
    (setq dired-recursive-deletes 'always)
    ;; makes dired guess the target directory
    (setq dired-dwim-target t)

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
                                     "-alhvF --group-directories-first"))

    ;; auto-revert dired buffers if file changed on disk
    (setq dired-auto-revert-buffer t)

    (setq projectile-switch-project-action 'projectile-dired) ; dired loads on project switch

    ;; Hydra for dired
    (after! hydra
        (defhydra +my/hydra-dired (:hint nil :color pink)
            "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell       crux
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary        C-c o : crux-open-with
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
            ("\\" dired-do-ispell)
            ("(" dired-hide-details-mode)
            (")" dired-omit-mode)
            ("+" dired-create-directory)
            ("=" diredp-ediff)         ;; smart diff
            ("?" dired-summary)
            ("$" diredp-hide-subdir-nomove)
            ("A" dired-do-find-regexp)
            ("C" dired-do-copy)        ;; Copy all marked files
            ("D" dired-do-delete)
            ("E" dired-mark-extension)
            ("e" dired-ediff-files)
            ("F" dired-do-find-marked-files)
            ("G" dired-do-chgrp)
            ("g" revert-buffer)        ;; read all directories again (refresh)
            ("i" dired-maybe-insert-subdir)
            ("l" dired-do-redisplay)   ;; relist the marked or singel directory
            ("M" dired-do-chmod)
            ("m" dired-mark)
            ("O" dired-display-file)
            ("o" dired-find-file-other-window)
            ("Q" dired-do-find-regexp-and-replace)
            ("R" dired-do-rename)
            ("r" dired-do-rsynch)
            ("S" dired-do-symlink)
            ("s" dired-sort-toggle-or-edit)
            ("t" dired-toggle-marks)
            ("U" dired-unmark-all-marks)
            ("u" dired-unmark)
            ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
            ("w" dired-kill-subdir)
            ("Y" dired-do-relsymlink)
            ("z" diredp-compress-this-file)
            ("Z" dired-do-compress)
            ("q" nil)
            ("." nil :color blue))))

(after! dired-quick-sort
    (dired-quick-sort-setup)
    (define-key dired-mode-map "s" 'hydra-dired-quick-sort/body))

(after! neotree
    (setq
        doom-themes-neotree-file-icons nil
        neo-theme 'ascii
        neo-window-width 32
        neo-create-file-auto-open t
        neo-show-updir-line t
        neo-smart-open t
        neo-show-hidden-files t
        neo-auto-indent-point t
        neo-vc-integration nil
        neo-autorefresh nil)

    (if IS-MAC
        (setq neo-default-system-application "open"))

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

(after! org
    ;; hook
    (add-hook 'org-mode-hook
        (lambda ()
            (set-fill-column 110)))

    ;; doom fold level
    (setq +org-initial-fold-level 1)

    ;; org problems
    (setq org-planning-line-re "^[    ]*\\(\\(?:CLOSED\\|DEADLINE\\|SCHEDULED\\):\\)")
    (setq org-clock-line-re "^[    ]*CLOCK:")

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
    (setq org-list-description-max-indent 5) ;; set maximum indentation for description lists
    (setq org-adapt-indentation nil) ;; prevent demoting heading also shifting text inside sections
    (setq org-cycle-separator-lines 2)
    (setq org-blank-before-new-entry '((heading . t) (plain-list-item . nil)))
    (setq org-agenda-file-regexp "\\`[^.].*\\.\\(org\\.txt\\|org\\)\\'")
    (setq org-clock-idle-time 15)

    ;; more settings
    (setq
        org-clock-persist-file (expand-file-name ".emacs.d/.cache/org-clock-save.el" +my/dotfiles-path)
        org-id-locations-file (expand-file-name ".emacs.d/.cache/.org-id-locations" +my/dotfiles-path)
        org-publish-timestamp-directory (expand-file-name ".emacs.d/.cache/.org-timestamps/" +my/dotfiles-path)
        org-log-done t
        org-startup-with-inline-images t
        org-image-actual-width nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        ;; this is consistent with the value of
        ;; `helm-org-headings-max-depth'.
        org-imenu-depth 8)

    ;; hide the emphasis markup (e.g. /.../ for italics, *...* for bold, etc.)
    (setq org-hide-emphasis-markers t)

    ;; set up a font-lock substitution for list markers (- => •)
    (font-lock-add-keywords 'org-mode
        '(("^ *\\([-]\\) "
              (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

    ;; set up a nice proportional font, in different sizes, for the headlines.
    ;; the fonts listed will be tried in sequence, and the first one found will be used.
    (let* ((variable-tuple
               (cond
                   ((x-list-fonts   "Source Code Pro") '(:font   "Source Code Pro"))
                   ((x-list-fonts   "Source Sans Pro") '(:font   "Source Sans Pro"))
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

    ;; tags
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
    :config (setq org-tags-exclude-from-inheritance (quote ("crypt"))))

(use-package! crux
    :bind (("C-c o" . crux-open-with)))

(use-package! visual-regexp
    :commands (vr/query-replace vr/replace))

(load! "+ui")
(load! "+config")
(load! "+bindings")

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


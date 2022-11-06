;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(load! "lisp/init")
(load! "lisp/ui")

(when (modulep! :editor evil)
    (after! evil
        (csetq evil-want-fine-undo t) ;; By default while in insert all changes are one big blob. Be more granular
        (csetq evil-cross-lines t)    ;; Make horizontal movement cross lines
        (csetq evil-shift-width 4)    ;; evil shift width
        (csetq evil-ex-search-persistent-highlight nil) ;; No highlight persistence on evil search

        ;; Make movement keys work like they should
        (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
        (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
        (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
        (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)))

;; enable repeat-mode, see: `describe-repeat-maps'
(repeat-mode)

(when noninteractive
    (after! undo-tree
        (global-undo-tree-mode -1)))

(after! which-key
    (csetq which-key-popup-type 'minibuffer))

(after! isearch
    (defadvice isearch-search (after isearch-no-fail activate)
        (unless isearch-success
            (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
            (ad-activate 'isearch-search)
            (isearch-repeat (if isearch-forward 'forward))
            (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
            (ad-activate 'isearch-search))))

(after! vterm
    (add-to-list 'vterm-tramp-shells '("ssh" "/bin/sh")))

(after! eshell
    (csetq eshell-highlight-prompt nil)
    (csetq eshell-scroll-to-bottom-on-input nil)
    (csetq eshell-scroll-to-bottom-on-output nil)
    (csetq eshell-prefer-lisp-functions nil)
    (csetq eshell-error-if-no-glob t)
    (csetq eshell-hist-ignoredups t)
    (csetq eshell-save-history-on-exit t)
    (csetq eshell-destroy-buffer-when-process-dies t)

    ;; Aliases
    (add-hook 'eshell-mode-hook
        (lambda ()
            ;; The 'ls' executable requires the Gnu version on the Mac
            (let ((ls (if (file-exists-p "/usr/local/bin/gls")
                          "/usr/local/bin/gls"
                          "/bin/ls")))
                (eshell/alias "ll" (concat ls " -alh --group-directories-first --color=auto")))
            (eshell/alias "ff" "find-file $1")
            (eshell/alias "e" "find-file-other-window $1")
            (eshell/alias "d" "dired $1"))))

(after! persp-mode
    (csetq persp-emacsclient-init-frame-behaviour-override "main"))

(after! tramp
    (csetq tramp-verbose 2)
    (csetq tramp-use-ssh-controlmaster-options nil) ; Don't override SSH config.
    (csetq tramp-default-method "ssh")    ; ssh is faster than scp and supports ports.
    (csetq tramp-password-prompt-regexp   ; Add verification code support.
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

    (csetq emacs-everywhere-frame-parameters
        `((name . "emacs-everywhere")
             (width . 120)
             (height . 20))))

(after! yasnippet
    (push (expand-file-name "snippets/" doom-user-dir) yas-snippet-dirs))

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

(csetq +lookup-open-url-fn #'browse-url)
;; (csetq +lookup-open-url-fn #'+lookup-xwidget-webkit-open-url-fn)
(after! dash-docs
    (set-docsets! 'c-mode "C")
    (set-docsets! 'c++-mode "C" "C++")
    (set-docsets! 'js2-mode "JavaScript" "JQuery")
    (set-docsets! 'nodejs-mode :remove "JQuery")
    (csetq dash-docs-browser-func #'+lookup-xwidget-webkit-open-url-fn))

(after! projectile
    (csetq projectile-switch-project-action 'projectile-dired)
    (csetq projectile-require-project-root t)
    (csetq projectile-project-root-files-bottom-up '(".projectile" ".git"))
    (csetq projectile-sort-order 'recentf)
    (csetq projectile-indexing-method 'hybrid)

    (csetq projectile-ignored-project-function
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
                (string-prefix-p (expand-file-name "Workspace/Software/zig/" +my/home-path) project-root)
                (string-prefix-p (expand-file-name ".cargo/" +my/home-path) project-root)
                (string-prefix-p (expand-file-name ".rustup/" +my/home-path) project-root)))))

(after! ivy
    (csetq ivy-display-style nil)
    (csetq ivy-count-format "(%d/%d) ")
    (csetq ivy-use-selectable-prompt t)    ; much better than C-M-j
    (csetq ivy-use-virtual-buffers t)      ; to make ivy-views appear on the buffers list
    (csetq ivy-virtual-abbreviate 'full)   ; default is name
    (csetq ivy-initial-inputs-alist nil)   ; remove initial ^ input.
    (csetq ivy-extra-directories nil)      ; remove . and .. directory. (default value: ("../" "./"))
    (csetq ivy-height 10)

    ;; While in an ivy mini-buffer C-o shows a list of all possible actions one may take.
    ;; By default this is #'ivy-read-action-by-key however a better interface to this is using Hydra.
    (csetq ivy-read-action-function #'ivy-hydra-read-action)

    (csetq ivy-display-functions-alist
        '((counsel-irony . ivy-display-function-overlay)
             (ivy-completion-in-region . ivy-display-function-overlay))))

(after! avy
    (csetq avy-all-windows t))

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
    (csetq flyspell-lazy-idle-seconds 2)) ; default is 2

(after! spell-fu
    (csetq spell-fu-idle-delay 0.5) ; default is 0.25
    (add-hook! 'spell-fu-mode-hook
        (lambda ()
            (spell-fu-dictionary-add (spell-fu-get-ispell "es")))))

(after! flycheck
    (csetq flycheck-temp-prefix "flycheck_tmp")
    (csetq flycheck-indication-mode 'left-fringe))

(after! company
    (csetq company-idle-delay 0.0)
    (csetq company-minimum-prefix-length 1)
    (csetq company-tooltip-align-annotations t)

    (when (modulep! :editor evil)
        ;; make aborting less annoying.
        (add-hook! evil-normal-state-entry #'company-abort)))

(after! format
    (csetq +format-on-save-enabled-modes
        '(not
             emacs-lisp-mode  ; elisp's mechanisms are good enough
             sql-mode         ; sqlformat is currently broken
             tex-mode         ; latexindent is broken
             web-mode         ; I just don't like tidy
             ;; nxml-mode        ; work make me do this
             latex-mode))

    ;; Do not format with lsp, use `format` instead
    (csetq +format-with-lsp nil))

(after! ws-butler
    (csetq ws-butler-global-exempt-modes
        (append ws-butler-global-exempt-modes
		    '(prog-mode org-mode))))

(progn
    (csetq c-default-style "linux")
    (csetq c-basic-offset 4)
    (after! cc
        (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
        (add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
        (add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))
        (add-to-list 'auto-mode-alist '("\\.C\\'" . c++-mode))
        (add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
        (add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))
        (setq-default flycheck-c/c++-clang-executable +my/clang-path)
        (setq-default flycheck-clang-standard-library "libc++")
        (setq-default flycheck-clang-language-standard "c++20")))

(after! python
    (csetq python-shell-interpreter "python3"))

(after! elisp-mode
    (remove-hook 'emacs-lisp-mode-hook #'+emacs-lisp-extend-imenu-h))

(after! common-lisp
    (csetq inferior-lisp-program "/usr/local/bin/sbcl")

    (csetq slime-lisp-implementations
        `((ccl ("~/.cim/bin/ccl-1.9") :coding-system utf-8-unix)
             (alisp ("/usr/local/bin/alisp") :coding-system utf-8-unix)
             (ecl ("/usr/local/bin/ecl"))  ; :coding-system utf-8-unix)
             (cmucl ("/usr/local/bin/cmucl") :coding-system utf-8-unix)
             (sbcl ("/usr/local/bin/sbcl" "+R" "-l" "~/.sbclrc") :coding-system utf-8-unix)
             (abcl ("~/.cim/bin/abcl-1.3.1" "-XX:MaxPermSize=256m" "-Dfile.encoding=UTF-8") :coding-system utf-8-unix)
             (clisp ("/usr/local/bin/clisp") :coding-system utf-8-unix)))

    (csetq slime-default-lisp 'sbcl)
    (csetq slime-net-coding-system 'utf-8-unix))

(after! scheme
    (csetq geiser-guile-binary "guile3.0"))

(after! lsp-mode
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

    ;; General
    (csetq lsp-idle-delay 0.1) ;; clangd is fast
    (csetq lsp-restart 'auto-restart)
    ;; (csetq lsp-headerline-breadcrumb-enable nil)
    ;; (csetq lsp-enable-indentation nil)
    ;; (csetq lsp-eldoc-enable-hover t)
    ;; (csetq lsp-eldoc-render-all nil)
    ;; (csetq lsp-signature-render-documentation nil)
    ;; (csetq lsp-signature-auto-activate nil)
    ;; (csetq lsp-signature-doc-lines 1)
    ;; (csetq lsp-auto-guess-root nil)
    ;; (csetq lsp-enable-file-watchers nil)
    ;; (csetq lsp-enable-on-type-formatting nil)

    ;; Rust
    (csetq lsp-rust-analyzer-cargo-watch-command "clippy")
    (csetq lsp-rust-analyzer-completion-auto-import-enable nil)

    ;; Zig
    (csetq lsp-zig-zls-executable
        (expand-file-name "zig/zls/zig-out/bin/zls" +my/software-path))

    ;; C++
    (csetq lsp-clients-clangd-args
        '("-j=8"
             "--log=error"
             "--malloc-trim"
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
    (csetq lsp-ui-doc-enable nil)
    (csetq lsp-ui-doc-show-with-cursor nil)
    (csetq lsp-ui-doc-show-with-mouse nil)
    (csetq lsp-lens-enable nil)
    (csetq lsp-ui-sideline-enable nil)
    (csetq lsp-ui-sideline-show-code-actions nil)
    (csetq lsp-ui-sideline-enable nil)
    (csetq lsp-ui-sideline-show-hover nil))

(csetq eldoc-echo-area-use-multiline-p nil)
(after! eglot
    (csetq eglot-autoshutdown t)
    (csetq eglot-extend-to-xref t)
    (csetq eglot-ignored-server-capabilities
        (quote (:documentFormattingProvider :documentRangeFormattingProvider)))

    (add-to-list 'eglot-server-programs
        '(c-mode c++-mode
		     . ("clangd"
                   "-j=8"
                   "--log=error"
                   "--malloc-trim"
                   "--background-index"
                   "--clang-tidy"
                   "--cross-file-rename"
                   "--completion-style=detailed"
                   "--pch-storage=memory"
                   "--header-insertion=never"
                   "--header-insertion-decorators=0"))))

(after! rustic
    (when (modulep! :tools lsp +eglot)
        (csetq rustic-lsp-client 'eglot))
    (csetq rustic-format-on-save nil))

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
    (csetq git-commit-summary-max-length 80))

(after! dired
    (csetq dired-ls-F-marks-symlinks t) ;; mark symlinks
    (csetq dired-recursive-copies 'always) ;; Never prompt for recursive copies of a directory
    (csetq dired-recursive-deletes 'always) ;; Never prompt for recursive deletes of a directory
    (csetq dired-dwim-target t) ;; makes dired guess the target directory
    (csetq dired-auto-revert-buffer t) ;; auto-revert dired buffers if file changed on disk
    (csetq projectile-switch-project-action 'projectile-dired) ;; dired loads on project switch
    (csetq wdired-allow-to-change-permissions t) ;; allow to edit permissions in wdired

    (let ((gls "/usr/local/bin/gls"))
        (if (file-exists-p gls) (csetq insert-directory-program gls)))

    ;; Dired listing switches
    ;;  -a : Do not ignore entries starting with .
    ;;  -l : Use long listing format.
    ;;  -h : Human-readable sizes like 1K, 234M, ..
    ;;  -v : Do natural sort .. so the file names starting with . will show up first.
    ;;  -F : Classify filenames by appending '*' to executables, '/' to directories, etc.
    (csetq dired-listing-switches (if (eq system-type 'windows-nt)
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
    (csetq neo-theme 'ascii)
    (csetq neo-window-width 32)
    (csetq neo-smart-open t)
    (csetq neo-create-file-auto-open nil)
    (csetq neo-show-updir-line nil)
    (csetq neo-show-hidden-files t)
    (csetq neo-auto-indent-point t)
    (csetq neo-vc-integration nil)
    (csetq neo-autorefresh nil)

    ;; When running `projectile-switch-project`, `neotree` will change root automatically.
    (csetq projectile-switch-project-action 'neotree-projectile-action)

    ;; Hidden files
    (csetq neo-hidden-regexp-list
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
             "^\\.log$"
             "^\\.egg\-info$")))

(after! treemacs
    (csetq treemacs-no-png-images t)
    (defvar treemacs-file-ignore-extensions '()
        "File extension which `treemacs-ignore-filter' will ensure are ignored")
    (defvar treemacs-file-ignore-globs '()
        "Globs which will are transformed to `treemacs-file-ignore-regexps' which `treemacs-ignore-filter' will ensure are ignored")
    (defvar treemacs-file-ignore-regexps '()
        "RegExps to be tested to ignore files, generated from `treeemacs-file-ignore-globs'")
    (csetq treemacs-file-ignore-extensions
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
    (csetq treemacs-file-ignore-globs
        '(;; LaTeX
             "*/_minted-*"
             ;; AucTeX
             "*/.auctex-auto"
             "*/_region_.log"
             "*/_region_.tex"))
    (defun treemacs-file-ignore-generate-regexps ()
        "Generate `treemacs-file-ignore-regexps' from `treemacs-file-ignore-globs'"
        (csetq treemacs-file-ignore-regexps (mapcar 'dired-glob-regexp treemacs-file-ignore-globs)))
    (if (equal treemacs-file-ignore-globs '()) nil (treemacs-file-ignore-generate-regexps))
    (defun treemacs-ignore-filter (file full-path)
        "Ignore files specified by `treemacs-file-ignore-extensions', and `treemacs-file-ignore-regexps'"
        (or (member (file-name-extension file) treemacs-file-ignore-extensions)
            (let ((ignore-file nil))
                (dolist (regexp treemacs-file-ignore-regexps ignore-file)
                    (csetq ignore-file (or ignore-file (if (string-match-p regexp full-path) t nil)))))))
    (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-filter))

(after! pass
    (csetq password-store-password-length 25))

(after! evil-org
    (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

(after! deft
    (csetq deft-directory (expand-file-name "Apps/org/notes" +my/dropbox-path))
    (csetq deft-extensions '("org" "md" "txt"))
    (csetq deft-default-extension "org")
    (csetq deft-recursive nil)
    (csetq deft-use-filename-as-title nil)
    (csetq deft-use-filter-string-for-filename t)
    (csetq deft-file-naming-rules '((noslash . "-")
                                       (nospace . "-")
                                       (case-fn . downcase)))
    (csetq deft-auto-save-interval 0))

(after! org-roam
    (csetq org-roam-directory (expand-file-name "Apps/org/roam" +my/dropbox-path))
    (csetq org-roam-completion-everywhere t))

(after! org
    (csetq org-return-follows-link  t)
    (csetq org-hide-emphasis-markers t)
    (csetq org-startup-folded t))

(after! docker
    (csetq docker-container-shell-file-name "/bin/bash")
    (add-to-list 'docker-image-run-custom-args
        `("^sm" ("-v \"$HOME\"/Workspace/Work/Projects/dmxs:/tmp/sm"
			        "-u jenkins"
			        "-w /tmp/sm"
			        "--name dmxs" . ,docker-image-run-default-args))))

(use-package! mu4e
    :config
    (csetq sendmail-program (executable-find "msmtp"))
    (csetq send-mail-function #'smtpmail-send-it)
    (csetq message-sendmail-f-is-evil t)
    (csetq message-sendmail-extra-arguments '("--read-envelope-from"))
    (csetq message-send-mail-function #'message-send-mail-with-sendmail)

    (set-email-account! "gmail"
		'((mu4e-sent-folder          . "/gmail/[Gmail]/Sent Mail")
			 (mu4e-drafts-folder     . "/gmail/[Gmail]/Drafts")
			 (mu4e-trash-folder      . "/gmail/[Gmail]/Trash")
			 (mu4e-refile-folder     . "/gmail/[Gmail]/All Mail")
			 (smtpmail-smtp-user     . "jvillasantegomez@gmail.com")
			 (mu4e-compose-signature . "---\nRegards,\nJulio"))
		t)
    (set-email-account! "icloud"
		'((mu4e-sent-folder          . "/icloud/Sent Messages")
			 (mu4e-drafts-folder     . "/icloud/Drafts")
			 (mu4e-trash-folder      . "/icloud/Deleted Messages")
			 (mu4e-refile-folder     . "/icloud/Archive")
			 (smtpmail-smtp-user     . "julio.villasante@icloud.com")
			 (mu4e-compose-signature . "---\nRegards,\nJulio"))
		t))

;; https://orgmode.org/worg/org-tutorials/encrypting-files.html
(use-package! org-crypt
    :after org
    :config
    (csetq org-crypt-disable-auto-save nil) ;; don't ask to disable auto-save
    (csetq org-tags-exclude-from-inheritance (quote ("crypt")))
    (csetq org-crypt-key nil)
    (csetq org-crypt-key user-mail-address))

;; better C-w and M-w
(use-package! whole-line-or-region
    :config
    (whole-line-or-region-global-mode))

(load! "lisp/elfeed")
(if (modulep! :editor evil)
    (load! "lisp/bindings-evil")
    (load! "lisp/bindings-emacs"))
;; (load! "lisp/hydras")

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

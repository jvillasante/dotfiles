;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(load! "lisp/init")
(load! "lisp/ui")

(when (modulep! :editor evil)
    (after! evil
        (customize-set-variable 'evil-want-fine-undo t) ;; By default while in insert all changes are one big blob. Be more granular
        (customize-set-variable 'evil-cross-lines t)    ;; Make horizontal movement cross lines
        (customize-set-variable 'evil-shift-width 4)    ;; evil shift width
        (customize-set-variable 'evil-ex-search-persistent-highlight nil) ;; No highlight persistence on evil search

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
    (customize-set-variable 'which-key-popup-type 'minibuffer))

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
    (setc
        eshell-highlight-prompt nil
        eshell-scroll-to-bottom-on-input nil
        eshell-scroll-to-bottom-on-output nil
        eshell-prefer-lisp-functions nil
        eshell-error-if-no-glob t
        eshell-hist-ignoredups t
        eshell-save-history-on-exit t
        eshell-destroy-buffer-when-process-dies t)

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
    (customize-set-variable 'persp-emacsclient-init-frame-behaviour-override "main"))

(after! tramp
    (customize-set-variable 'tramp-verbose 2)
    (customize-set-variable 'tramp-use-ssh-controlmaster-options nil) ; Don't override SSH config.
    (customize-set-variable 'tramp-default-method "ssh")    ; ssh is faster than scp and supports ports.
    (customize-set-variable 'tramp-password-prompt-regexp   ; Add verification code support.
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

    (customize-set-variable 'emacs-everywhere-frame-parameters
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

(customize-set-variable '+lookup-open-url-fn #'browse-url)
;; (customize-set-variable '+lookup-open-url-fn #'+lookup-xwidget-webkit-open-url-fn)
(after! dash-docs
    (set-docsets! 'c-mode "C")
    (set-docsets! 'c++-mode "C" "C++")
    (set-docsets! 'js2-mode "JavaScript" "JQuery")
    (set-docsets! 'nodejs-mode :remove "JQuery")
    (customize-set-variable 'dash-docs-browser-func #'+lookup-xwidget-webkit-open-url-fn))

(after! projectile
    (customize-set-variable 'projectile-switch-project-action 'projectile-dired)
    (customize-set-variable 'projectile-require-project-root t)
    (customize-set-variable 'projectile-project-root-files-bottom-up '(".projectile" ".git"))
    (customize-set-variable 'projectile-sort-order 'recentf)
    (customize-set-variable 'projectile-indexing-method 'hybrid)

    (customize-set-variable 'projectile-ignored-project-function
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
    (customize-set-variable 'ivy-display-style nil)
    (customize-set-variable 'ivy-count-format "(%d/%d) ")
    (customize-set-variable 'ivy-use-selectable-prompt t)    ; much better than C-M-j
    (customize-set-variable 'ivy-use-virtual-buffers t)      ; to make ivy-views appear on the buffers list
    (customize-set-variable 'ivy-virtual-abbreviate 'full)   ; default is name
    (customize-set-variable 'ivy-initial-inputs-alist nil)   ; remove initial ^ input.
    (customize-set-variable 'ivy-extra-directories nil)      ; remove . and .. directory. (default value: ("../" "./"))
    (customize-set-variable 'ivy-height 10)

    ;; While in an ivy mini-buffer C-o shows a list of all possible actions one may take.
    ;; By default this is #'ivy-read-action-by-key however a better interface to this is using Hydra.
    (customize-set-variable 'ivy-read-action-function #'ivy-hydra-read-action)

    (customize-set-variable 'ivy-display-functions-alist
        '((counsel-irony . ivy-display-function-overlay)
             (ivy-completion-in-region . ivy-display-function-overlay))))

(after! avy
    (customize-set-variable 'avy-all-windows t))

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
    (customize-set-variable 'flyspell-lazy-idle-seconds 2)) ; default is 2

(after! spell-fu
    (customize-set-variable 'spell-fu-idle-delay 0.5) ; default is 0.25
    (add-hook! 'spell-fu-mode-hook
        (lambda ()
            (spell-fu-dictionary-add (spell-fu-get-ispell "es")))))

(after! flycheck
    (customize-set-variable 'flycheck-temp-prefix "flycheck_tmp")
    (customize-set-variable 'flycheck-indication-mode 'left-fringe))

(after! company
    (customize-set-variable 'company-idle-delay 0.1)
    (customize-set-variable 'company-tooltip-limit 10)
    (customize-set-variable 'company-minimum-prefix-length 2)
    (customize-set-variable 'company-tooltip-align-annotations t)

    (when (modulep! :editor evil)
        ;; make aborting less annoying.
        (add-hook! evil-normal-state-entry #'company-abort)))

(after! format
    (customize-set-variable '+format-on-save-enabled-modes
        '(not
             emacs-lisp-mode ; elisp's mechanisms are good enough
             sql-mode         ; sqlformat is currently broken
             tex-mode         ; latexindent is broken
             web-mode         ; I just don't like tidy
             ;; nxml-mode        ; work make me do this
             latex-mode))

    ;; Do not format with lsp, use `format` instead
    (customize-set-variable '+format-with-lsp nil))

(after! ws-butler
    (customize-set-variable 'ws-butler-global-exempt-modes
        (append ws-butler-global-exempt-modes
            '(prog-mode org-mode))))

(progn
    (setc
        c-default-style "linux"
        c-basic-offset 4)

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
    (customize-set-variable 'python-shell-interpreter "python3"))

(after! elisp-mode
    (remove-hook 'emacs-lisp-mode-hook #'+emacs-lisp-extend-imenu-h))

(after! common-lisp
    (customize-set-variable 'inferior-lisp-program "/usr/local/bin/sbcl")

    (customize-set-variable 'slime-lisp-implementations
        `((ccl ("~/.cim/bin/ccl-1.9") :coding-system utf-8-unix)
             (alisp ("/usr/local/bin/alisp") :coding-system utf-8-unix)
             (ecl ("/usr/local/bin/ecl"))  ; :coding-system utf-8-unix)
             (cmucl ("/usr/local/bin/cmucl") :coding-system utf-8-unix)
             (sbcl ("/usr/local/bin/sbcl" "+R" "-l" "~/.sbclrc") :coding-system utf-8-unix)
             (abcl ("~/.cim/bin/abcl-1.3.1" "-XX:MaxPermSize=256m" "-Dfile.encoding=UTF-8") :coding-system utf-8-unix)
             (clisp ("/usr/local/bin/clisp") :coding-system utf-8-unix)))

    (customize-set-variable 'slime-default-lisp 'sbcl)
    (customize-set-variable 'slime-net-coding-system 'utf-8-unix))

(after! scheme
    (customize-set-variable 'geiser-guile-binary "guile3.0"))

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
    (customize-set-variable 'lsp-restart 'ignore
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

    ;; Rust
    (setc
        lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-completion-auto-import-enable nil)

    ;; Zig
    (customize-set-variable 'lsp-zig-zls-executable
        (expand-file-name "zig/zls/zig-out/bin/zls" +my/software-path))

    ;; C++
    (customize-set-variable 'lsp-clients-clangd-args
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
    (customize-set-variable 'lsp-ui-sideline-enable nil)
    (customize-set-variable 'lsp-ui-sideline-show-symbol nil)
    (customize-set-variable 'lsp-ui-sideline-show-diagnostics nil)
    (customize-set-variable 'lsp-ui-sideline-show-hover nil)
    (customize-set-variable 'lsp-ui-sideline-show-code-actions nil)

    (customize-set-variable 'lsp-ui-peek-enable nil)
    (customize-set-variable 'lsp-ui-peek-always-show nil)
    (customize-set-variable 'lsp-ui-peek-show-directory nil)

    (customize-set-variable 'lsp-ui-doc-enable nil)
    (customize-set-variable 'lsp-ui-imenu-enable t))

(customize-set-variable 'eldoc-echo-area-use-multiline-p nil)
(after! eglot
    (customize-set-variable 'eglot-autoshutdown t)
    (customize-set-variable 'eglot-extend-to-xref t)
    (customize-set-variable 'eglot-ignored-server-capabilities
        (quote (:documentFormattingProvider :documentRangeFormattingProvider)))

    (add-to-list 'eglot-server-programs
        '(c-mode c++-mode
             . ("clangd"
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
    (customize-set-variable 'rustic-lsp-client 'eglot)
    (customize-set-variable 'rustic-format-on-save nil))

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
    (customize-set-variable 'git-commit-summary-max-length 80))

(after! dired
    (customize-set-variable 'dired-ls-F-marks-symlinks t) ;; mark symlinks
    (customize-set-variable 'dired-recursive-copies 'always) ;; Never prompt for recursive copies of a directory
    (customize-set-variable 'dired-recursive-deletes 'always) ;; Never prompt for recursive deletes of a directory
    (customize-set-variable 'dired-dwim-target t) ;; makes dired guess the target directory
    (customize-set-variable 'dired-auto-revert-buffer t) ;; auto-revert dired buffers if file changed on disk
    (customize-set-variable 'projectile-switch-project-action 'projectile-dired) ;; dired loads on project switch
    (customize-set-variable 'wdired-allow-to-change-permissions t) ;; allow to edit permissions in wdired

    (let ((gls "/usr/local/bin/gls"))
        (if (file-exists-p gls) (customize-set-variable 'insert-directory-program gls)))

    ;; Dired listing switches
    ;;  -a : Do not ignore entries starting with .
    ;;  -l : Use long listing format.
    ;;  -h : Human-readable sizes like 1K, 234M, ..
    ;;  -v : Do natural sort .. so the file names starting with . will show up first.
    ;;  -F : Classify filenames by appending '*' to executables, '/' to directories, etc.
    (customize-set-variable 'dired-listing-switches (if (eq system-type 'windows-nt)
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
    (customize-set-variable 'neo-theme 'ascii)
    (customize-set-variable 'neo-window-width 32)
    (customize-set-variable 'neo-smart-open t)
    (customize-set-variable 'neo-create-file-auto-open nil)
    (customize-set-variable 'neo-show-updir-line nil)
    (customize-set-variable 'neo-show-hidden-files t)
    (customize-set-variable 'neo-auto-indent-point t)
    (customize-set-variable 'neo-vc-integration nil)
    (customize-set-variable 'neo-autorefresh nil)

    ;; When running `projectile-switch-project`, `neotree` will change root automatically.
    (customize-set-variable 'projectile-switch-project-action 'neotree-projectile-action)

    ;; Hidden files
    (customize-set-variable 'neo-hidden-regexp-list
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
    (customize-set-variable 'treemacs-no-png-images t)
    (defvar treemacs-file-ignore-extensions '()
        "File extension which `treemacs-ignore-filter' will ensure are ignored")
    (defvar treemacs-file-ignore-globs '()
        "Globs which will are transformed to `treemacs-file-ignore-regexps' which `treemacs-ignore-filter' will ensure are ignored")
    (defvar treemacs-file-ignore-regexps '()
        "RegExps to be tested to ignore files, generated from `treeemacs-file-ignore-globs'")
    (customize-set-variable 'treemacs-file-ignore-extensions
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
    (customize-set-variable 'treemacs-file-ignore-globs
        '(;; LaTeX
             "*/_minted-*"
             ;; AucTeX
             "*/.auctex-auto"
             "*/_region_.log"
             "*/_region_.tex"))
    (defun treemacs-file-ignore-generate-regexps ()
        "Generate `treemacs-file-ignore-regexps' from `treemacs-file-ignore-globs'"
        (customize-set-variable 'treemacs-file-ignore-regexps (mapcar 'dired-glob-regexp treemacs-file-ignore-globs)))
    (if (equal treemacs-file-ignore-globs '()) nil (treemacs-file-ignore-generate-regexps))
    (defun treemacs-ignore-filter (file full-path)
        "Ignore files specified by `treemacs-file-ignore-extensions', and `treemacs-file-ignore-regexps'"
        (or (member (file-name-extension file) treemacs-file-ignore-extensions)
            (let ((ignore-file nil))
                (dolist (regexp treemacs-file-ignore-regexps ignore-file)
                    (customize-set-variable 'ignore-file (or ignore-file (if (string-match-p regexp full-path) t nil)))))))
    (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-filter))

(after! pass
    (customize-set-variable 'password-store-password-length 25))

(after! evil-org
    (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

(after! deft
    (customize-set-variable 'deft-directory (expand-file-name "Apps/org/notes" +my/dropbox-path))
    (customize-set-variable 'deft-extensions '("org" "md" "txt"))
    (customize-set-variable 'deft-default-extension "org")
    (customize-set-variable 'deft-recursive nil)
    (customize-set-variable 'deft-use-filename-as-title nil)
    (customize-set-variable 'deft-use-filter-string-for-filename t)
    (customize-set-variable 'deft-file-naming-rules '((noslash . "-")
                                                         (nospace . "-")
                                                         (case-fn . downcase)))
    (customize-set-variable 'deft-auto-save-interval 0))

(after! org-roam
    (customize-set-variable 'org-roam-directory (expand-file-name "Apps/org/roam" +my/dropbox-path))
    (customize-set-variable 'org-roam-completion-everywhere t))

(after! org
    (customize-set-variable 'org-return-follows-link  t)
    (customize-set-variable 'org-hide-emphasis-markers t)
    (customize-set-variable 'org-startup-folded t))

(after! docker
    (customize-set-variable 'docker-container-shell-file-name "/bin/bash")
    (add-to-list 'docker-image-run-custom-args
        `("^sm" ("-v \"$HOME\"/Workspace/Work/Projects/dmxs:/tmp/sm"
                    "-u jenkins"
                    "-w /tmp/sm"
                    "--name dmxs" . ,docker-image-run-default-args))))

(use-package! mu4e
    :config
    (customize-set-variable 'sendmail-program (executable-find "msmtp"))
    (customize-set-variable 'send-mail-function #'smtpmail-send-it)
    (customize-set-variable 'message-sendmail-f-is-evil t)
    (customize-set-variable 'message-sendmail-extra-arguments '("--read-envelope-from"))
    (customize-set-variable 'message-send-mail-function #'message-send-mail-with-sendmail)

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
    (customize-set-variable 'org-crypt-disable-auto-save nil) ;; don't ask to disable auto-save
    (customize-set-variable 'org-tags-exclude-from-inheritance (quote ("crypt")))
    (customize-set-variable 'org-crypt-key nil)
    (customize-set-variable 'org-crypt-key user-mail-address))

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


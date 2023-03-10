;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(load! "lisp/init")

(when (modulep! :editor evil)
    (after! evil
        (setq! evil-want-fine-undo t) ;; By default while in insert all changes are one big blob. Be more granular
        (setq! evil-cross-lines t)    ;; Make horizontal movement cross lines
        (setq! evil-shift-width 4)    ;; evil shift width
        (setq! evil-ex-search-persistent-highlight nil) ;; No highlight persistence on evil search

        ;; Make movement keys work like they should
        (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
        (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
        (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
        (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)))

;; the mark ring
(setq-default set-mark-command-repeat-pop t)

;; enable repeat-mode, see: `describe-repeat-maps'
(when (> emacs-major-version 27)
    (repeat-mode +1))

(when noninteractive
    (after! undo-tree
        (global-undo-tree-mode -1)))

(after! which-key
    (setq! which-key-popup-type 'minibuffer))

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
    (setq! eshell-highlight-prompt nil)
    (setq! eshell-scroll-to-bottom-on-input nil)
    (setq! eshell-scroll-to-bottom-on-output nil)
    (setq! eshell-prefer-lisp-functions nil)
    (setq! eshell-error-if-no-glob t)
    (setq! eshell-hist-ignoredups t)
    (setq! eshell-save-history-on-exit t)
    (setq! eshell-destroy-buffer-when-process-dies t)

    ;; Aliases
    (add-hook 'eshell-mode-hook
        (lambda ()
            ;; The 'ls' executable requires the Gnu version on the Mac
            (let ((ls (if (file-exists-p "/usr/local/bin/gls")
                          "/usr/local/bin/gls"
                          "/bin/ls")))
                (eshell/alias "ls" (concat ls " --group-directories-first --color"))
                (eshell/alias "ll" (concat ls " -AlFh --group-directories-first --color")))
            (eshell/alias "ff" "find-file $1")
            (eshell/alias "e" "find-file-other-window $1")
            (eshell/alias "d" "dired $1"))))

(after! persp-mode
    (setq! persp-emacsclient-init-frame-behaviour-override "main"))

(after! tramp
    (setq! tramp-verbose 2)
    (setq! tramp-default-method "ssh")    ; ssh is faster than scp and supports ports.
    (setq! tramp-password-prompt-regexp   ; Add verification code support.
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

    (setq! emacs-everywhere-frame-parameters
        `((name . "emacs-everywhere")
             (width . 120)
             (height . 20))))

(after! yasnippet
    (push (expand-file-name "snippets/" doom-user-dir) yas-snippet-dirs))

(after! recentf
    (push (list (expand-file-name ".emacs.chemacs2/" +my/dotfiles-path)) recentf-exclude)
    (push (list (expand-file-name ".emacs.crafted/" +my/dotfiles-path)) recentf-exclude)
    (push (list (expand-file-name ".emacs.d/" +my/dotfiles-path)) recentf-exclude)
    (push (list (expand-file-name ".emacs.doom/" +my/dotfiles-path)) recentf-exclude)
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

(setq! +lookup-open-url-fn #'browse-url)
;; (setq! +lookup-open-url-fn #'+lookup-xwidget-webkit-open-url-fn)
(after! dash-docs
    (set-docsets! 'c-mode "C")
    (set-docsets! 'c++-mode "C" "C++")
    (set-docsets! 'js2-mode "JavaScript" "JQuery")
    (set-docsets! 'nodejs-mode :remove "JQuery")
    (setq! dash-docs-browser-func #'+lookup-xwidget-webkit-open-url-fn))

(after! projectile
    (setq! projectile-project-root-files-bottom-up '(".projectile" ".git"))
    (setq! projectile-sort-order 'recentf)
    (setq! projectile-ignored-project-function
        (lambda (project-root)
            (or (file-remote-p project-root)
                (string-prefix-p temporary-file-directory project-root)
                (string-prefix-p (expand-file-name ".password-store/" +my/home-path) project-root)
                (string-prefix-p (expand-file-name ".emacs.d/" +my/home-path) project-root)
                (string-prefix-p (expand-file-name ".emacs.d/" +my/dotfiles-path) project-root)
                (string-prefix-p (expand-file-name ".emacs.doom/" +my/dotfiles-path) project-root)
                (string-prefix-p (expand-file-name ".emacs.chemacs2/" +my/dotfiles-path) project-root)
                (string-prefix-p (expand-file-name ".emacs.crafted/" +my/dotfiles-path) project-root)
                (string-prefix-p (expand-file-name "Workspace/Software/zig/" +my/home-path) project-root)
                (string-prefix-p (expand-file-name ".cargo/" +my/home-path) project-root)
                (string-prefix-p (expand-file-name ".rustup/" +my/home-path) project-root)))))

(after! ivy
    (setq! ivy-display-style nil)
    (setq! ivy-count-format "(%d/%d) ")
    (setq! ivy-use-selectable-prompt t)    ; much better than C-M-j
    (setq! ivy-use-virtual-buffers t)      ; to make ivy-views appear on the buffers list
    (setq! ivy-virtual-abbreviate 'full)   ; default is name
    (setq! ivy-initial-inputs-alist nil)   ; remove initial ^ input.
    (setq! ivy-extra-directories nil)      ; remove . and .. directory. (default value: ("../" "./"))
    (setq! ivy-height 10)

    ;; While in an ivy mini-buffer C-o shows a list of all possible actions one may take.
    ;; By default this is #'ivy-read-action-by-key however a better interface to this is using Hydra.
    (setq! ivy-read-action-function #'ivy-hydra-read-action)

    (setq! ivy-display-functions-alist
        '((counsel-irony . ivy-display-function-overlay)
             (ivy-completion-in-region . ivy-display-function-overlay))))

(after! avy
    (setq! avy-all-windows t))

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
    (setq! flyspell-lazy-idle-seconds 2)) ; default is 2

(after! spell-fu
    (setq! spell-fu-idle-delay 0.5) ; default is 0.25
    (add-hook! 'spell-fu-mode-hook
        (lambda ()
            (spell-fu-dictionary-add (spell-fu-get-ispell "es")))))

(after! flycheck
    (setq! flycheck-temp-prefix "flycheck_tmp")
    (setq! flycheck-indication-mode 'left-fringe))

(after! company
    (setq! company-idle-delay 0.0)
    (setq! company-minimum-prefix-length 1)
    (setq! company-tooltip-align-annotations t)

    (when (modulep! :editor evil)
        ;; make aborting less annoying.
        (add-hook! evil-normal-state-entry #'company-abort)))

(after! format
    (setq! +format-on-save-enabled-modes
        '(not
             emacs-lisp-mode  ; elisp's mechanisms are good enough
             sql-mode         ; sqlformat is currently broken
             tex-mode         ; latexindent is broken
             web-mode         ; I just don't like tidy
             ;; nxml-mode        ; work make me do this
             latex-mode))

    ;; Do not format with lsp, use `format` instead
    (setq! +format-with-lsp nil))

(after! ws-butler
    (setq! ws-butler-global-exempt-modes
        (append ws-butler-global-exempt-modes
            '(prog-mode org-mode))))

(progn
    (setq! c-default-style "linux")
    (setq! c-basic-offset 4)
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
    (setq! python-shell-interpreter "python3"))

(after! elisp-mode
    (remove-hook 'emacs-lisp-mode-hook #'+emacs-lisp-extend-imenu-h))

(after! common-lisp
    (setq! inferior-lisp-program "/usr/local/bin/sbcl")

    (setq! slime-lisp-implementations
        `((ccl ("~/.cim/bin/ccl-1.9") :coding-system utf-8-unix)
             (alisp ("/usr/local/bin/alisp") :coding-system utf-8-unix)
             (ecl ("/usr/local/bin/ecl"))  ; :coding-system utf-8-unix)
             (cmucl ("/usr/local/bin/cmucl") :coding-system utf-8-unix)
             (sbcl ("/usr/local/bin/sbcl" "+R" "-l" "~/.sbclrc") :coding-system utf-8-unix)
             (abcl ("~/.cim/bin/abcl-1.3.1" "-XX:MaxPermSize=256m" "-Dfile.encoding=UTF-8") :coding-system utf-8-unix)
             (clisp ("/usr/local/bin/clisp") :coding-system utf-8-unix)))

    (setq! slime-default-lisp 'sbcl)
    (setq! slime-net-coding-system 'utf-8-unix))

(after! scheme
    (setq! geiser-guile-binary "guile3.0"))

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
    (setq! lsp-idle-delay 0.5)
    (setq! lsp-file-watch-threshold 15000)
    (setq! lsp-auto-guess-root t)
    (setq! lsp-log-io nil)
    (setq! lsp-restart 'ignore)
    (setq! lsp-enable-symbol-highlighting t)
    (setq! lsp-lens-enable nil)
    (setq! lsp-headerline-breadcrumb-enable nil)
    (setq! lsp-modeline-code-actions-enable t)
    (setq! lsp-modeline-diagnostics-enable t)
    (setq! lsp-eldoc-enable-hover t)
    (setq! lsp-signature-auto-activate t)
    (setq! lsp-signature-render-documentation nil)
    (setq! lsp-completion-show-detail t)
    (setq! lsp-completion-show-kind nil)
    (setq! read-process-output-max (* 1024 1024)) ;; 1MB

    ;; Rust
    (setq! lsp-rust-analyzer-cargo-watch-command "clippy")
    (setq! lsp-rust-analyzer-completion-auto-import-enable nil)

    ;; Zig
    (setq! lsp-zig-zls-executable
        (expand-file-name "zig/zls/zig-out/bin/zls" +my/software-path))

    ;; C++
    (setq! lsp-clients-clangd-args
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
    (setq! lsp-ui-doc-enable nil)
    (setq! lsp-ui-doc-show-with-cursor nil)
    (setq! lsp-ui-doc-show-with-mouse nil)
    (setq! lsp-ui-sideline-enable nil)
    (setq! lsp-ui-sideline-show-code-actions nil)
    (setq! lsp-ui-sideline-show-hover nil))

(after! eglot
    (setq! eldoc-echo-area-use-multiline-p nil)
    (setq! eglot-autoshutdown t)
    (setq! eglot-extend-to-xref t)
    (setq! eglot-ignored-server-capabilities
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
        (setq! rustic-lsp-client 'eglot))
    (setq! rustic-format-on-save nil))

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
    (setq! git-commit-summary-max-length 50))

(after! dired
    (setq! dired-ls-F-marks-symlinks t) ;; mark symlinks
    (setq! dired-recursive-copies 'always) ;; Never prompt for recursive copies of a directory
    (setq! dired-recursive-deletes 'always) ;; Never prompt for recursive deletes of a directory
    (setq! dired-dwim-target t) ;; makes dired guess the target directory
    (setq! dired-auto-revert-buffer t) ;; auto-revert dired buffers if file changed on disk
    (setq! wdired-allow-to-change-permissions t) ;; allow to edit permissions in wdired

    (let ((gls "/usr/local/bin/gls"))
        (if (file-exists-p gls) (setq! insert-directory-program gls)))

    ;; Dired listing switches
    ;;  -a : Do not ignore entries starting with .
    ;;  -l : Use long listing format.
    ;;  -h : Human-readable sizes like 1K, 234M, ..
    ;;  -v : Do natural sort .. so the file names starting with . will show up first.
    ;;  -F : Classify filenames by appending '*' to executables, '/' to directories, etc.
    (setq! dired-listing-switches (if (eq system-type 'windows-nt)
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
    (setq! neo-theme 'ascii)
    (setq! neo-window-width 32)
    (setq! neo-smart-open t)
    (setq! neo-create-file-auto-open nil)
    (setq! neo-show-updir-line nil)
    (setq! neo-show-hidden-files t)
    (setq! neo-auto-indent-point t)
    (setq! neo-vc-integration nil)
    (setq! neo-autorefresh nil)

    ;; Hidden files
    (setq! neo-hidden-regexp-list
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
    (setq! treemacs-no-png-images t)
    (defvar treemacs-file-ignore-extensions '()
        "File extension which `treemacs-ignore-filter' will ensure are ignored")
    (defvar treemacs-file-ignore-globs '()
        "Globs which will are transformed to `treemacs-file-ignore-regexps' which `treemacs-ignore-filter' will ensure are ignored")
    (defvar treemacs-file-ignore-regexps '()
        "RegExps to be tested to ignore files, generated from `treeemacs-file-ignore-globs'")
    (setq! treemacs-file-ignore-extensions
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
    (setq! treemacs-file-ignore-globs
        '(;; LaTeX
             "*/_minted-*"
             ;; AucTeX
             "*/.auctex-auto"
             "*/_region_.log"
             "*/_region_.tex"))
    (defun treemacs-file-ignore-generate-regexps ()
        "Generate `treemacs-file-ignore-regexps' from `treemacs-file-ignore-globs'"
        (setq! treemacs-file-ignore-regexps (mapcar 'dired-glob-regexp treemacs-file-ignore-globs)))
    (if (equal treemacs-file-ignore-globs '()) nil (treemacs-file-ignore-generate-regexps))
    (defun treemacs-ignore-filter (file full-path)
        "Ignore files specified by `treemacs-file-ignore-extensions', and `treemacs-file-ignore-regexps'"
        (or (member (file-name-extension file) treemacs-file-ignore-extensions)
            (let ((ignore-file nil))
                (dolist (regexp treemacs-file-ignore-regexps ignore-file)
                    (setq! ignore-file (or ignore-file (if (string-match-p regexp full-path) t nil)))))))
    (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-filter))

(after! pass
    (setq! password-store-password-length 25))

(after! evil-org
    (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

(after! deft
    (setq! deft-directory (expand-file-name "Apps/org/notes" +my/dropbox-path))
    (setq! deft-extensions '("org" "md" "txt"))
    (setq! deft-default-extension "org")
    (setq! deft-recursive nil)
    (setq! deft-use-filename-as-title nil)
    (setq! deft-use-filter-string-for-filename t)
    (setq! deft-file-naming-rules '((noslash . "-")
                                       (nospace . "-")
                                       (case-fn . downcase)))
    (setq! deft-auto-save-interval 0))

(after! org-roam
    (setq! org-roam-directory (expand-file-name "Apps/org/roam" +my/dropbox-path))
    (setq! org-roam-completion-everywhere t))

(after! org
    ;; Capture templates
    ;; (setq! org-capture-templates
    ;;     `(("i" "Inbox (inbox.org)" entry  (file "inbox.org")
    ;;           ,(concat "* TODO %?\n"
    ;;                "/Entered on/ %U"))
    ;;          ("m" "Meeting (agenda.org)" entry  (file+headline "agenda.org" "Future")
    ;;              ,(concat "* %? :meeting:\n"
    ;;                   "<%<%Y-%m-%d %a %H:00>>"))
    ;;          ("n" "Note (notes.org)" entry  (file "notes.org")
    ;;              ,(concat "* Note (%a)\n"
    ;;                   "/Entered on/ %U\n" "\n" "%?"))))

    ;; Agenda
    (setq! org-agenda-hide-tags-regexp ".")
    (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t% s")
             (todo   . " ")
             (tags   . " %i %-12:c")
             (search . " %i %-12:c")))

    ;; General Config
    (setq! org-return-follows-link  t)
    (setq! org-hide-emphasis-markers t)
    (setq! org-startup-folded t))

(after! docker
    (setq! docker-container-shell-file-name "/bin/bash")
    (add-to-list 'docker-image-run-custom-args
        `("^sm" ("-v \"$HOME\"/Workspace/Work/Projects/dmxs:/tmp/sm"
                    "-u jenkins"
                    "-w /tmp/sm"
                    "--name dmxs" . ,docker-image-run-default-args))))

;; https://orgmode.org/worg/org-tutorials/encrypting-files.html
(use-package! org-crypt
    :after org
    :config
    (setq! org-crypt-disable-auto-save nil) ;; don't ask to disable auto-save
    (setq! org-tags-exclude-from-inheritance (quote ("crypt")))
    (setq! org-crypt-key nil)
    (setq! org-crypt-key user-mail-address))

;; better C-w and M-w
(use-package! whole-line-or-region
    :config
    (whole-line-or-region-global-mode))

;; better shell commands
(use-package! dwim-shell-command
    :demand
    :bind
    (([remap shell-command] . dwim-shell-command)
        ([remap async-shell-command] . dwim-shell-command)
        :map dired-mode-map
        ([remap dired-do-async-shell-command] . dwim-shell-command)
        ([remap dired-do-shell-command] . dwim-shell-command)
        ([remap dired-smart-shell-command] . dwim-shell-command))
    :init
    (require 'dwim-shell-commands)
    :config
    (defun my/dwim-shell-command-convert-to-gif ()
        "Convert all marked videos to optimized gif(s)."
        (interactive)
        (dwim-shell-command-on-marked-files
            "Convert to gif"
            "ffmpeg -loglevel quiet -stats -y -i <<f>> -pix_fmt rgb24 -r 15 <<fne>>.gif"
            :utils "ffmpeg")))

(use-package! file-info
    :demand
    :bind (("C-c d" . 'file-info-show))
    :config
    (setq hydra-hint-display-type 'posframe)
    (setq hydra-posframe-show-params `(:poshandler posframe-poshandler-frame-center
                                          :internal-border-width 2
                                          :internal-border-color "#61AFEF"
                                          :left-fringe 16
                                          :right-fringe 16)))

(load! "lisp/ui")
(load! "lisp/elfeed")
(load! "lisp/hydras")
(if (modulep! :editor evil)
    (load! "lisp/bindings-evil")
    (load! "lisp/bindings-emacs"))

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

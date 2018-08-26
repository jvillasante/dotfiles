;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
    ;; Base distribution to use. This is a layer contained in the directory
    ;; `+distribution'. For now available distributions are `spacemacs-base'
    ;; or `spacemacs'. (default 'spacemacs)
    dotspacemacs-distribution 'spacemacs
    ;; Lazy installation of layers (i.e. layers are installed only when a file
    ;; with a supported type is opened). Possible values are `all', `unused'
    ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
    ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
    ;; lazy install any layer that support lazy installation even the layers
    ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
    ;; installation feature and you have to explicitly list a layer in the
    ;; variable `dotspacemacs-configuration-layers' to install it.
    ;; (default 'unused)
    dotspacemacs-enable-lazy-installation 'unused
    ;; If non-nil then Spacemacs will ask for confirmation before installing
    ;; a layer lazily. (default t)
    dotspacemacs-ask-for-lazy-installation t
    ;; If non-nil layers with lazy install support are lazy installed.
    ;; List of additional paths where to look for configuration layers.
    ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
    dotspacemacs-configuration-layer-path '()
    ;; List of configuration layers to load.
    dotspacemacs-configuration-layers
    '(
       ;; ----------------------------------------------------------------
       ;; Example of useful layers you may want to use right away.
       ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
       ;; <M-m f e R> (Emacs style) to install them.
       ;; ----------------------------------------------------------------
       ivy
       osx
       dash
       (auto-completion :variables
         auto-completion-return-key-behavior 'complete
         auto-completion-tab-key-behavior 'cycle
         auto-completion-complete-with-key-sequence nil
         auto-completion-complete-with-key-sequence-delay 0.1
         auto-completion-private-snippets-directory "~/.spacemacs.d/snippets"
         auto-completion-enable-help-tooltip t
         auto-completion-enable-sort-by-usage t
         auto-completion-enable-snippets-in-popup t
         :disabled-for org erc git markdown eshell ledger text)
       (syntax-checking
         :variables syntax-checking-enable-tooltips nil)
       (spell-checking :variables
         spell-checking-enable-by-default nil
         spell-checking-enable-auto-dictionary nil)
       nlinum
       emacs-lisp
       (git :variables
         git-use-magit-next t
         git-enable-github-support t)
       version-control
       (org :variables
         org-enable-github-support t)
       (markdown :variables
         markdown-italic-underscore t)
       (ibuffer :variables
         ibuffer-group-buffers-by 'projects)
       (shell :variables
         shell-default-shell 'ansi-term
         ;; shell-default-shell 'eshell
         shell-default-position 'bottom
         shell-default-height 50)
       (evil-snipe :variables
         evil-snipe-enable-alternate-f-and-t-behaviors t)
       evil-commentary
       shell-scripts
       search-engine
       (html :variables
         css-indent-offset 2
         web-mode-code-indent-offset 2
         web-mode-markup-indent-offset 2
         web-mode-css-indent-offset 2)
       ;; semantic
       ;; javascript
       ;; common-lisp
       python
       (c-c++ :variables
         c-c++-enable-clang-support t
         c-c++-default-mode-for-headers 'c++-mode)
       (rust :variables
         rust-format-on-save t)
       (go :variables
         ;; gofmt-command "goimports"
         go-tab-width 2)
       restclient
       csv
       deft
       ;; themes-megapack
       (elfeed :variables
         rmh-elfeed-org-files (list (concat jv/dropbox-path "/Personal/elfeed/elfeed.org")))

       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       jv
       jv-zenburn
       jv-cpp-common
       ;; jv-cpp-irony
       ycmd
       jv-cpp-ycmd
       jv-deft
       jv-dired
       jv-elfeed
       jv-ivy
       jv-lisp
       jv-magit
       jv-org)

    ;; List of additional packages that will be installed without being
    ;; wrapped in a layer. If you need some configuration for these
    ;; packages, then consider creating a layer. You can also put the
    ;; configuration in `dotspacemacs/user-config'.
    dotspacemacs-additional-packages '(ag yasnippet-snippets)
    ;; A list of packages that cannot be updated.
    dotspacemacs-frozen-packages '()
    ;; A list of packages that will not be installed and loaded.
    dotspacemacs-excluded-packages '(evil-search-highlight-persist)
    ;; Defines the behaviour of Spacemacs when installing packages.
    ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
    ;; `used-only' installs only explicitly used packages and uninstall any
    ;; unused packages as well as their unused dependencies.
    ;; `used-but-keep-unused' installs only the used packages but won't uninstall
    ;; them if they become unused. `all' installs *all* packages supported by
    ;; Spacemacs and never uninstall them. (default is `used-only')
    dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
    ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
    ;; possible. Set it to nil if you have no way to use HTTPS in your
    ;; environment, otherwise it is strongly recommended to let it set to t.
    ;; This variable has no effect if Emacs is launched with the parameter
    ;; `--insecure' which forces the value of this variable to nil.
    ;; (default t)
    dotspacemacs-elpa-https t
    ;; Maximum allowed time in seconds to contact an ELPA repository.
    dotspacemacs-elpa-timeout 10
    ;; If non nil then spacemacs will check for updates at startup
    ;; when the current branch is not `develop'. Note that checking for
    ;; new versions works via git commands, thus it calls GitHub services
    ;; whenever you start Emacs. (default nil)
    dotspacemacs-check-for-update nil
    ;; If non-nil, a form that evaluates to a package directory. For example, to
    ;; use different package directories for different Emacs versions, set this
    ;; to `emacs-version'.
    dotspacemacs-elpa-subdirectory nil
    ;; One of `vim', `emacs' or `hybrid'.
    ;; `hybrid' is like `vim' except that `insert state' is replaced by the
    ;; `hybrid state' with `emacs' key bindings. The value can also be a list
    ;; with `:variables' keyword (similar to layers). Check the editing styles
    ;; section of the documentation for details on available variables.
    ;; (default 'vim)
    dotspacemacs-editing-style 'vim
    ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
    dotspacemacs-verbose-loading t
    ;; Specify the startup banner. Default value is `official', it displays
    ;; the official spacemacs logo. An integer value is the index of text
    ;; banner, `random' chooses a random text banner in `core/banners'
    ;; directory. A string value must be a path to an image format supported
    ;; by your Emacs build.
    ;; If the value is nil then no banner is displayed. (default 'official)
    dotspacemacs-startup-banner nil
    ;; List of items to show in startup buffer or an association list of
    ;; the form `(list-type . list-size)`. If nil then it is disabled.
    ;; Possible values for list-type are:
    ;; `recents' `bookmarks' `projects' `agenda' `todos'."
    ;; Example for 5 recent files and 7 projects: '((recents . 5) (projects . 7))
    ;; List sizes may be nil, in which case
    ;; `spacemacs-buffer-startup-lists-length' takes effect.
    ;; (default nil)
    dotspacemacs-startup-lists '((projects . 5) (recents . 5) bookmarks)
    ;; True if the home buffer should respond to resize events.
    dotspacemacs-startup-buffer-responsive t
    ;; Default major mode of the scratch buffer (default `text-mode')
    dotspacemacs-scratch-mode 'text-mode
    ;; List of themes, the first of the list is loaded when spacemacs starts.
    ;; Press <SPC> T n to cycle to the next theme in the list (works great
    ;; with 2 themes variants, one dark and one light)
    dotspacemacs-themes '(zenburn
                           solarized-light
                           solarized-dark
                           spacemacs-light
                           spacemacs-dark
                           leuven
                           monokai)
    ;; If non nil the cursor color matches the state color in GUI Emacs.
    dotspacemacs-colorize-cursor-according-to-state t
    ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
    ;; quickly tweak the mode-line size to make separators look not too crappy.
    dotspacemacs-default-font '("Source Code Pro"
                                 :size 22
                                 :weight normal
                                 :width normal
                                 :powerline-scale 1.0)
    ;; The leader key
    dotspacemacs-leader-key "SPC"
    ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
    ;; (default "SPC")
    dotspacemacs-emacs-command-key ":"
    ;; The key used for Vim Ex commands (default ":")
    dotspacemacs-ex-command-key ":"
    ;; The leader key accessible in `emacs state' and `insert state'
    ;; (default "M-m")
    dotspacemacs-emacs-leader-key "M-m"
    ;; Major mode leader key is a shortcut key which is the equivalent of
    ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
    dotspacemacs-major-mode-leader-key ","
    ;; Major mode leader key accessible in `emacs state' and `insert state'.
    ;; (default "C-M-m)
    dotspacemacs-major-mode-emacs-leader-key "C-M-m"
    ;; These variables control whether separate commands are bound in the GUI to
    ;; the key pairs C-i, TAB and C-m, RET.
    ;; Setting it to a non-nil value, allows for separate commands under <C-i>
    ;; and TAB or <C-m> and RET.
    ;; In the terminal, these pairs are generally indistinguishable, so this only
    ;; works in the GUI. (default nil)
    dotspacemacs-distinguish-gui-tab nil
    ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
    dotspacemacs-remap-Y-to-y$ t
    ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
    ;; there. (default t)
    dotspacemacs-retain-visual-state-on-shift t
    ;; If non-nil, J and K move lines up and down when in visual mode.
    ;; (default nil)
    dotspacemacs-visual-line-move-text nil
    ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
    ;; (default nil)
    dotspacemacs-ex-substitute-global nil
    ;; Name of the default layout (default "Default")
    dotspacemacs-default-layout-name "Default"
    ;; If non nil the default layout name is displayed in the mode-line.
    ;; (default nil)
    dotspacemacs-display-default-layout t
    ;; If non nil then the last auto saved layouts are resume automatically upon
    ;; start. (default nil)
    dotspacemacs-auto-resume-layouts nil
    ;; Size (in MB) above which spacemacs will prompt to open the large file
    ;; literally to avoid performance issues. Opening a file literally means that
    ;; no major mode or minor modes are active. (default is 1)
    dotspacemacs-large-file-size 1
    ;; Location where to auto-save files. Possible values are `original' to
    ;; auto-save the file in-place, `cache' to auto-save the file to another
    ;; file stored in the cache directory and `nil' to disable auto-saving.
    ;; (default 'cache)
    dotspacemacs-auto-save-file-location 'cache
    ;; Maximum number of rollback slots to keep in the cache. (default 5)
    dotspacemacs-max-rollback-slots 5
    ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
    dotspacemacs-helm-resize nil
    ;; if non nil, the helm header is hidden when there is only one source.
    ;; (default nil)
    dotspacemacs-helm-no-header t
    ;; define the position to display `helm', options are `bottom', `top',
    ;; `left', or `right'. (default 'bottom)
    dotspacemacs-helm-position 'bottom
    ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
    ;; in all non-asynchronous sources. If set to `source', preserve individual
    ;; source settings. Else, disable fuzzy matching in all sources.
    ;; (default 'always)
    dotspacemacs-helm-use-fuzzy 'always
    ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
    ;; several times cycle between the kill ring content. (default nil)
    dotspacemacs-enable-paste-transient-state nil
    ;; Which-key delay in seconds. The which-key buffer is the popup listing
    ;; the commands bound to the current keystroke sequence. (default 0.4)
    dotspacemacs-which-key-delay 0.4
    ;; Which-key frame position. Possible values are `right', `bottom' and
    ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
    ;; right; if there is insufficient space it displays it at the bottom.
    ;; (default 'bottom)
    dotspacemacs-which-key-position 'bottom
    ;; If non nil a progress bar is displayed when spacemacs is loading. This
    ;; may increase the boot time on some systems and emacs builds, set it to
    ;; nil to boost the loading time. (default t)
    dotspacemacs-loading-progress-bar nil
    ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
    ;; (Emacs 24.4+ only)
    dotspacemacs-fullscreen-at-startup nil
    ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
    ;; Use to disable fullscreen animations in OSX. (default nil)
    dotspacemacs-fullscreen-use-non-native nil
    ;; If non nil the frame is maximized when Emacs starts up.
    ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
    ;; (default nil) (Emacs 24.4+ only)
    dotspacemacs-maximized-at-startup t
    ;; A value from the range (0..100), in increasing opacity, which describes
    ;; the transparency level of a frame when it's active or selected.
    ;; Transparency can be toggled through `toggle-transparency'. (default 90)
    dotspacemacs-active-transparency 90
    ;; A value from the range (0..100), in increasing opacity, which describes
    ;; the transparency level of a frame when it's inactive or deselected.
    ;; Transparency can be toggled through `toggle-transparency'. (default 90)
    dotspacemacs-inactive-transparency 90
    ;; If non nil show the titles of transient states. (default t)
    dotspacemacs-show-transient-state-title t
    ;; If non nil show the color guide hint for transient state keys. (default t)
    dotspacemacs-show-transient-state-color-guide t
    ;; If non nil unicode symbols are displayed in the mode line. (default t)
    dotspacemacs-mode-line-unicode-symbols t
    ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
    ;; scrolling overrides the default behavior of Emacs which recenters point
    ;; when it reaches the top or bottom of the screen. (default t)
    dotspacemacs-smooth-scrolling t
    ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
    ;; derivatives. If set to `relative', also turns on relative line numbers.
    ;; (default nil)
    dotspacemacs-line-numbers t
    ;; Code folding method. Possible values are `evil' and `origami'.
    ;; (default 'evil)
    dotspacemacs-folding-method 'origami
    ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
    ;; (default nil)
    dotspacemacs-smartparens-strict-mode nil
    ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
    ;; over any automatically added closing parenthesis, bracket, quote, etc…
    ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
    dotspacemacs-smart-closing-parenthesis nil
    ;; Select a scope to highlight delimiters. Possible values are `any',
    ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
    ;; emphasis the current one). (default 'all)
    dotspacemacs-highlight-delimiters 'all
    ;; If non nil, advise quit functions to keep server open when quitting.
    ;; (default nil)
    dotspacemacs-persistent-server t
    ;; List of search tool executable names. Spacemacs uses the first installed
    ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
    ;; (default '("ag" "pt" "ack" "grep"))
    dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
    ;; The default package repository used if no explicit repository has been
    ;; specified with an installed package.
    ;; Not used for now. (default nil)
    dotspacemacs-default-package-repository nil
    ;; Delete whitespace while saving buffer. Possible values are `all'
    ;; to aggressively delete empty line and long sequences of whitespace,
    ;; `trailing' to delete only the whitespace at end of lines, `changed'to
    ;; delete only whitespace for changed lines or `nil' to disable cleanup.
    ;; (default nil)
    dotspacemacs-whitespace-cleanup 'trailing))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  (cond
    ((spacemacs/system-is-mac)
      (setq
        jv/dropbox-path "~/Dropbox"
        jv/zsh-path "/usr/local/bin/zsh"
        jv/clang-path "/usr/local/opt/llvm/bin/clang"))
    ((spacemacs/system-is-linux)
      (setq
        jv/dropbox-path "~/Dropbox"
        jv/zsh-path "/usr/bin/zsh"
        jv/clang-path "/usr/bin/clang")))

  (setq
    flycheck-check-syntax-automatically '(mode-enabled save))

  ;; https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
  (setq display-time-world-list '(("UTC" "UTC")
                                   ("US/Eastern" "Miami")
                                   ("America/Havana" "Habana")
                                   ("America/New_York" "New York")
                                   ("Europe/Amsterdam" "Amsterdam")
                                   ("Europe/Copenhagen" "Denmark")
                                   ("Asia/Shanghai" "China")
                                   ("Asia/Calcutta" "India")))

  (setq max-specpdl-size 5000)
  (setf url-queue-timeout 30))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (setq ispell-program-name "aspell")
  (setq auto-window-vscroll nil)
  (setq sp-escape-quotes-after-insert nil)

  ;; Customize frame title format.
  (setq frame-title-format
    '("emacs%@" (:eval (system-name)) ": "
       (:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b"))
       " [%*]"))

  (setq-default
    user-full-name "Julio C. Villasante"
    user-mail-address "jvillasantegomez@gmail.com"

    major-mode 'text-mode
    use-dialog-box nil
    vc-follow-symlinks t
    paradox-github-token t
    load-prefer-newer t
    fill-column 110                    ; Maximum line width
    truncate-lines t                   ; Don't fold lines
    truncate-partial-width-windows nil ; for vertically-split windows
    split-width-threshold 160          ; Split verticaly by default
    evil-cross-lines t                 ; Make horizontal movement cross lines

    ;; scroll
    scroll-margin 3

    ;; my coding style, bsd but with 2 spaces indentation (and no tab
    ;; characters, only spaces)
    c-basic-indent 2
    c-basic-offset 2
    tab-width 2
    indent-tabs-mode nil
    highlight-tabs t

    ;; Whitespace settings
    whitespace-action '(auto-cleanup)
    whitespace-style '(indentation::space
                        space-after-tab
                        space-before-tab
                        trailing
                        lines-tail
                        tab-mark
                        face
                        tabs)

    doc-view-continuous t

    ;; tramp mode
    tramp-default-method "ssh"

    ;; LaTeX
    font-latex-fontify-script nil
    TeX-newline-function 'reindent-then-newline-and-indent)

  ;; UTF-8 please
  (set-charset-priority 'unicode)
  (setq locale-coding-system   'utf-8)   ; pretty
  (set-terminal-coding-system  'utf-8)   ; pretty
  (set-keyboard-coding-system  'utf-8)   ; pretty
  (set-selection-coding-system 'utf-8)   ; please
  (prefer-coding-system        'utf-8)   ; with sugar on top
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  ;; multiterm
  (setq multi-term-program jv/zsh-path)

  ;; line spacing
  (setq-default line-spacing 0.1)

  ;; Isearch convenience, space matches anything (non-greedy)
  (setq search-whitespace-regexp ".*?")

  ;; Hooks
  (add-hook 'spacemacs-buffer-mode-hook
    'spacemacs/toggle-visual-line-navigation-on)
  (add-hook 'term-mode-hook
    (lambda ()
      (setq term-buffer-maximum-size 10000)))
  (add-hook 'focus-out-hook
    (lambda ()
      (save-some-buffers t)))
  (add-hook 'prog-mode-hook
    (lambda ()
      (set-fill-column 110)
      (flyspell-prog-mode)))
  (add-hook 'text-mode-hook 'turn-on-flyspell)
  (add-hook 'makefile-mode-hook 'whitespace-mode)
  (remove-hook 'prog-mode-hook 'spacemacs//show-trailing-whitespace)

  ;; use evil-matchit everywhere
  (global-evil-matchit-mode 1)

  ;; Make movement keys work like they should
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

  ;; use count-words instead of count-words-region as it works on buffer
  ;; if no region is selected
  (global-set-key (kbd "M-=") 'count-words)

  ;; use company everywhere
  (with-eval-after-load 'company
    ;; // delete only needed for irony, not for ycmd
    ;; (setq company-backends (delete 'company-semantic company-backends))
    (add-hook 'after-init-hook 'global-company-mode))

  (cond
    ((spacemacs/system-is-mac)
      (setq browse-url-browser-function 'browse-url-generic
        engine/browser-function 'browse-url-generic
        browse-url-generic-program "open"))
    ((spacemacs/system-is-linux)
      (executable-find "google-chrome")))

  ;; yasnippet
  (spacemacs/set-leader-keys "is" 'yas-insert-snippet)
  (spacemacs/set-leader-keys "id" 'yas-describe-tables)

  ;; (golden-ratio-mode 1)
  (show-paren-mode 1)
  (spaceline-compile))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (go-guru go-eldoc company-go go-mode yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode cython-mode company-anaconda anaconda-mode pythonic flycheck-ycmd company-ycmd ycmd request-deferred deferred password-store-otp password-store spinner org-category-capture nlinum markdown-mode parent-mode haml-mode fringe-helper git-gutter+ git-gutter flyspell-correct flx with-editor smartparens iedit anzu evil goto-chg undo-tree highlight simple-httpd ace-jump-mode noflet powerline popwin elfeed f hydra projectile pkg-info epl helm-dash dash-functional restclient know-your-http-well pos-tip irony rust-mode bind-map bind-key yasnippet packed async dash avy auto-complete popup modern-cpp-font-lock web-mode spaceline org-download insert-shebang htmlize hl-todo flycheck-rust evil-matchit engine-mode editorconfig company-web company counsel swiper flycheck helm helm-core ivy alert org-plus-contrib magit magit-popup git-commit ghub s zenburn-theme yasnippet-snippets xterm-color ws-butler winum which-key wgrep web-completion-data volatile-highlights vi-tilde-fringe uuidgen use-package toml-mode toc-org tagedit sr-speedbar solarized-theme smex smeargle slim-mode shell-pop scss-mode sass-mode rtags reveal-in-osx-finder restart-emacs request rainbow-delimiters racer pug-mode persp-mode pcre2el pbcopy pass paradox ox-gfm osx-trash osx-dictionary origami orgit org-projectile org-present org-pomodoro org-mime org-bullets open-junk-file ob-restclient ob-http nlinum-relative neotree multi-term move-text monokai-theme mmm-mode markdown-toc magit-gitflow macrostep lorem-ipsum log4e link-hint less-css-mode launchctl ivy-hydra irony-eldoc indent-guide ibuffer-projectile hungry-delete highlight-parentheses highlight-numbers highlight-indentation helm-make google-translate golden-ratio gnuplot gntp gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md fuzzy flyspell-lazy flyspell-correct-ivy flycheck-pos-tip flycheck-irony flx-ido fish-mode fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-snipe evil-numbers evil-mc evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-commentary evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav elfeed-web elfeed-org elfeed-goodies dumb-jump disaster dired-quick-sort diminish diff-hl deft dash-at-point csv-mode crux counsel-projectile counsel-dash company-statistics company-shell company-restclient company-quickhelp company-irony-c-headers company-irony company-c-headers column-enforce-mode cmake-mode clean-aindent-mode clang-format cargo auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent ag adaptive-wrap ace-window ace-link ac-ispell)))
 '(safe-local-variable-values
   (quote
    ((eval add-hook
           (quote before-save-hook)
           (function rust-format-buffer)
           nil t)
     (eval add-hook
           (quote before-save-hook)
           (function clang-format-buffer)
           nil t)
     (flycheck-clang-language-standard . c++17)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#DCDCCC" :background "#3F3F3F"))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))

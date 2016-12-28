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
     better-defaults
     (spell-checking :variables
                     spell-checking-enable-by-default nil
                     spell-checking-enable-auto-dictionary nil)
     (auto-completion :variables
                      auto-completion-return-key-behavior 'nil
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-complete-with-key-sequence-delay 0.1
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup t
                      :disabled-for org erc git)
     syntax-checking
     nlinum
     emacs-lisp
     helm
     ;; ivy
     (git :variables
          git-use-magit-next t
          git-enable-github-support t)
     version-control
     (org :variables
          org-enable-github-support t)
     markdown
     vimscript
     (ibuffer :variables
              ibuffer-group-buffers-by 'projects)
     (ranger :variables
             ranger-show-preview t)
     (shell :variables
            shell-default-term-shell "/bin/zsh"
            shell-default-shell 'ansi-term
            shell-default-position 'bottom
            shell-default-height 30)
     search-engine
     evil-commentary
     (html :variables
           css-indent-offset 2
           web-mode-code-indent-offset 2
           web-mode-markup-indent-offset 2
           web-mode-css-indent-offset 2)
     ;; semantic
     ;; gtags
     yaml
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode
            c-c++-enable-clang-support t)
     ;; go
     ;; restclient
     deft
     (elfeed :variables
             rmh-elfeed-org-files (list (concat my-dropbox-path "/Personal/elfeed/elfeed.org")))
     (mu4e :variables
           mu4e-installation-path my-mu4e-path
           mu4e-enable-notifications nil
           mu4e-enable-mode-line t
           mu4e-alert-interesting-mail-query "")
     jvillasante)
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(dash
                                    evil-search-highlight-persist)
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
   dotspacemacs-verbose-loading nil
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
   dotspacemacs-startup-lists '(projects recents bookmarks)
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(zenburn
                         spacemacs-dark
                         spacemacs-light
                         solarized-light
                         solarized-dark
                         leuven
                         monokai)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 18
                               :weight normal
                               :width normal
                               :powerline-scale 1.2)
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
   dotspacemacs-display-default-layout nil
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
   dotspacemacs-loading-progress-bar t
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
   dotspacemacs-folding-method 'evil
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
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  (setq
   my-dropbox-path "~/Dropbox"
   my-mu4e-path    "/usr/local/share/emacs/site-lisp/mu4e"
   my-docsets-path "~/.local/share/Zeal/Zeal/docsets")

  ;; https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
  (setq display-time-world-list '(("UTC" "UTC")
                                  ("US/Eastern" "Miami")
                                  ("America/Havana" "Habana")
                                  ("America/New_York" "New York")
                                  ("Europe/Amsterdam" "Amsterdam")
                                  ("Europe/Copenhagen" "Denmark")
                                  ("Asia/Shanghai" "China")
                                  ("Asia/Calcutta" "India")))

  (setq max-specpdl-size 3000)
  (setf url-queue-timeout 30))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;; server (emacs --daemon)
  (unless (server-running-p)
    (server-start))

  ;; emacsclient maximized frame
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Display Visited File's Path in the Frame Title
  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))))

  (setq-default
   ;; Miscellaneous
   user-full-name "Julio C. Villasante"
   user-mail-address "jvillasantegomez@gmail.com"
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
   helm-echo-input-in-header-line nil

   ;; tramp mode
   tramp-default-method "ssh"

   ;; LaTeX
   font-latex-fontify-script nil
   TeX-newline-function 'reindent-then-newline-and-indent)

  ;; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
  (setq utf-translate-cjk-mode nil)

  (set-language-environment 'utf-8)
  (setq locale-coding-system 'utf-8)

  ;; set the default encoding system
  (prefer-coding-system 'utf-8)
  (setq default-file-name-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)

  ;; backwards compatibility as default-buffer-file-coding-system
  ;; is deprecated in 23.2.
  (if (boundp buffer-file-coding-system)
      (setq buffer-file-coding-system 'utf-8)
    (setq default-buffer-file-coding-system 'utf-8))

  ;; Treat clipboard input as UTF-8 string first; compound text next, etc.
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

  ;; multiterm
  (setq multi-term-program "/usr/bin/zsh")
  (add-hook 'term-mode-hook
            (lambda ()
              (setq term-buffer-maximum-size 10000)))

  ;; line spacing
  (setq-default line-spacing 0.1)

  ;; Isearch convenience, space matches anything (non-greedy)
  (setq search-whitespace-regexp ".*?")

  (add-hook 'markdown-mode-hook
            (lambda ()
              (auto-fill-mode 0)
              (visual-line-mode 1)))
  (add-hook 'prog-mode-hook
            (lambda ()
              (set-fill-column 110)
              (flyspell-prog-mode)))
  (add-hook 'text-mode-hook 'turn-on-flyspell)
  (add-hook 'makefile-mode-hook 'whitespace-mode)
  (remove-hook 'prog-mode-hook 'spacemacs//show-trailing-whitespace)

  ;; use company everywhere
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-backends (delete 'company-semantic company-backends))

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

  ;; org
  (with-eval-after-load 'org
    ;; org problems
    (setq org-planning-line-re "^[    ]*\\(\\(?:CLOSED\\|DEADLINE\\|SCHEDULED\\):\\)")
    (setq org-clock-line-re "^[    ]*CLOCK:")

    (add-hook 'org-mode-hook
              (lambda ()
                (spacemacs/toggle-auto-fill-mode-on)
                (set-fill-column 110)))

    (setq org-startup-indented t)
    (setq org-indent-mode t)

    ;; set maximum indentation for description lists
    (setq org-list-description-max-indent 5)

    ;; prevent demoting heading also shifting text inside sections
    (setq org-adapt-indentation nil))

  ;; magit
  (setq-default git-magit-status-fullscreen t)
  (setq magit-completing-read-function 'magit-builtin-completing-read
        magit-push-always-verify nil)

  ;; company
  (add-hook 'c-mode-hook (lambda () (add-to-list 'company-backends '(company-irony-c-headers company-irony))))
  (add-hook 'c++-mode-hook (lambda () (add-to-list 'company-backends '(company-irony-c-headers company-irony))))

  ;; mu4e
  (require 'mu4e-contrib)
  (require 'org-mu4e)
  (require 'gnus-dired)

  (add-hook 'mu4e-compose-mode-hook
            (lambda ()
              (auto-fill-mode 0)
              (visual-line-mode 1)))

  (setq mu4e-maildir "~/.Maildir/gmail"
        mu4e-view-show-images t
        mu4e-view-image-max-width 800
        ;; mu4e-use-fancy-chars t
        ;; mu4e-view-prefer-html t
        ;; mu4e-html2text-command 'mu4e-shr2text
        ;; mu4e-html2text-command "html2text -utf8 -nobs -width 72"
        mu4e-html2text-command "w3m -dump -cols 80 -T text/html"
        ;; mu4e-html2text-command "html2markdown --body-width=0 | sed \"s/&nbsp_place_holder;/ /g; /^$/d\""
        mu4e-headers-skip-duplicates t
        mu4e-headers-full-search t
        mu4e-get-mail-command "mbsync -a"
        mu4e-update-interval 300
        mu4e-attachment-dir "~/Downloads"
        mu4e-sent-messages-behavior 'delete
        message-kill-buffer-on-exit t
        mu4e-hide-index-messages t
        mu4e-compose-signature-auto-include t
        mu4e-headers-include-related t
        mu4e-confirm-quit nil
        mu4e-compose-dont-reply-to-self t
        mu4e-compose-keep-self-cc nil
        mu4e-headers-auto-update t
        mu4e-headers-leave-behavior 'ask
        mu4e-headers-visible-lines 22
        mu4e-view-show-addresses t
        mail-user-agent 'mu4e-user-agent
        message-citation-line-format "On %m/%d/%Y %H:%M:%S, %f wrote:"
        message-citation-line-function 'message-insert-formatted-citation-line
        mu4e-change-filenames-when-moving t
        mu4e-headers-results-limit 250
        ;; mu4e-index-cleanup nil      ;; don't do a full cleanup check
        ;; mu4e-index-lazy-check t     ;; don't consider up-to-date dirs
        )

  (setq mu4e-drafts-folder "/[Gmail].Drafts"
        mu4e-sent-folder   "/[Gmail].Sent Mail"
        mu4e-trash-folder  "/[Gmail].Trash"
        mu4e-refile-folder "/[Gmail].All Mail")
  (setq mu4e-maildir-shortcuts
        '( ("/Inbox"               . ?i)
           ("/[Gmail].Important"   . ?I)
           ("/[Gmail].Sent Mail"   . ?s)
           ("/[Gmail].Spam"        . ?p)
           ("/[Gmail].Trash"       . ?t)
           ("/[Gmail].Drafts"      . ?d)
           ("/[Gmail].Starred"     . ?S)
           ("/[Gmail].All Mail"    . ?a)))
  (add-to-list 'mu4e-bookmarks
               '((concat "maildir:/Inbox AND date:"
                         (format-time-string "%Y%m%d" (subtract-time (current-time) (days-to-time 7))))
                 "Inbox messages in the last 7 days" ?W) t)
  (add-to-list 'mu4e-bookmarks
               '("size:5M..500M" "Big messages" ?b) t)

  (setq
   user-mail-address "jvillasantegomez@gmail.com"
   user-full-name  "Julio C. Villasante"
   mu4e-compose-signature
   (concat
    "Kind Regards,\n"
    "Julio C. Villasante\n"
    "--\n"
    "Sent from GNU Emacs\n"))

  (setq
   mu4e-date-format-long "%m/%d/%Y %H:%M:%S"
   mu4e-headers-date-format "%m/%d/%Y"
   mu4e-headers-time-format "%H:%M:%S")

  ;; Trim down the types of columns we show, to leave more room for the sender & subject.
  (setq mu4e-headers-fields '((:human-date .    12)
                              (:flags      .     6)
                              (:from-or-to .    22)
                              (:subject    . nil)))

  ;; Trim the number of fields shown in the email view. This is customizable. See mu4e-view.el for a full list.
  (setq mu4e-view-fields '(:from :to :cc :bcc :subject :date :tags :attachments :flags :maildir))
  (setq mu4e-view-show-addresses t)

  ;; mu4e - attachment
  (defun gnus-dired-mail-buffers ()
    "Return a list of active message buffers."
    (let (buffers)
      (save-current-buffer
        (dolist (buffer (buffer-list t))
          (set-buffer buffer)
          (when (and (derived-mode-p 'message-mode)
                     (null message-sent-message-via))
            (push (buffer-name buffer) buffers))))
      (nreverse buffers)))
  (setq gnus-dired-mail-mode 'mu4e-user-agent)
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

  ;; make shr/eww readable with dark themes
  (setq shr-color-visible-luminance-min 80)

  ;; If you use the mu4e-shr2text, it might be useful to emulate some of the shr key bindings
  (add-hook 'mu4e-view-mode-hook
            (lambda()
              ;; try to emulate some of the eww key-bindings
              (local-set-key (kbd "<tab>") 'shr-next-link)
              (local-set-key (kbd "<backtab>") 'shr-previous-link)))

  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; mu4e - actions
  (defun search-for-sender (msg)
    "Search for messages sent by the sender of the message at point."
    (mu4e-headers-search
     (concat "from:" (cdar (mu4e-message-field msg :from)))))

  (defun show-number-of-recipients (msg)
    "Display the number of recipients for the message at point."
    (message "Number of recipients: %d"
             (+ (length (mu4e-message-field msg :to))
                (length (mu4e-message-field msg :cc)))))

  (add-to-list 'mu4e-headers-actions
               '("Number of recipients" . show-number-of-recipients) t)
  (add-to-list 'mu4e-view-actions
               '("xsearch for sender" . search-for-sender) t)
  (add-to-list 'mu4e-view-actions
               '("wView with XWidget" . mu4e-action-view-with-xwidget) t)

  ;; mu4e - sending mail
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "/usr/bin/msmtp")
  (setq message-sendmail-extra-arguments '("--read-envelope-from"))
  (setq message-sendmail-f-is-evil 't)

  (add-hook 'mu4e-compose-mode-hook
            (lambda ()
              (auto-fill-mode 0)
              (visual-line-mode 1)))
  (add-hook 'mu4e-view-mode-hook
            (lambda () (visual-line-mode 1)))

  ;; mu4e - gpg
  ;; When composing an e-mail, C-c C-e s to sign your message then C-c C-e e to encrypt.
  ;; When receiving a PGP encrypted e-mail: C-c C-e v to verify the signature, and C-c C-e d to decrypt.
  (add-hook 'mu4e-compose-mode-hook 'epa-mail-mode)
  (add-hook 'mu4e-view-mode-hook 'epa-mail-mode)

  ;; mu4e-alert
  (with-eval-after-load 'org
    (setq mu4e-alert-interesting-mail-query
          (concat
           "flag:unread"
           " AND NOT flag:trashed"
           " AND maildir:"
           "\"/Inbox\"")))

  ;; xwidget
  (evil-set-initial-state 'xwidget-webkit-mode 'emacs)
  (add-hook 'xwidget-webkit-mode-hook
            (lambda ()
              (define-key xwidget-webkit-mode-map [mouse-4] 'xwidget-webkit-scroll-down)
              (define-key xwidget-webkit-mode-map [mouse-5] 'xwidget-webkit-scroll-up)
              (define-key xwidget-webkit-mode-map (kbd "<up>") 'xwidget-webkit-scroll-down)
              (define-key xwidget-webkit-mode-map (kbd "<down>") 'xwidget-webkit-scroll-up)
              (define-key xwidget-webkit-mode-map (kbd "C-p") 'xwidget-webkit-scroll-down)
              (define-key xwidget-webkit-mode-map (kbd "C-n") 'xwidget-webkit-scroll-up)
              (define-key xwidget-webkit-mode-map (kbd "M-w") 'xwidget-webkit-copy-selection-as-kill)
              (define-key xwidget-webkit-mode-map (kbd "C-c") 'xwidget-webkit-copy-selection-as-kill)

              ;; make xwidget default browser (not for now... maybe in the future!)
              ;; (setq browse-url-browser-function (lambda (url session)
              ;;                                     (other-window 1)
              ;;                                     (xwidget-browse-url-no-reuse url)))

              ;; adapt webkit according to window configuration change automatically
              ;; without this hook, every time you change your window configuration,
              ;; you must press 'a' to adapt webkit content to new window size
              (add-hook 'window-configuration-change-hook (lambda ()
                                                            (when (equal major-mode 'xwidget-webkit-mode)
                                                              (xwidget-webkit-adjust-size-dispatch))))

              ;; by default, xwidget reuses previous xwidget window,
              ;; thus overriding your current website, unless a prefix argument
              ;; is supplied
              ;;
              ;; This function always opens a new website in a new window
              (defun xwidget-browse-url-no-reuse (url &optional sessoin)
                (interactive (progn
                               (require 'browse-url)
                               (browse-url-interactive-arg "xwidget-webkit URL: ")))
                (xwidget-webkit-browse-url url t))))

  ;; deft
  (setq deft-directory (concat my-dropbox-path "/Personal/notes")
        deft-default-extension "org"
        deft-extensions '("org")
        deft-recursive t
        deft-text-mode 'org-mode
        deft-use-filename-as-title t
        deft-use-filter-string-for-filename t
        deft-file-naming-rules '((noslash . "-")
                                 (nospace . "-")
                                 (case-fn . downcase))
        deft-auto-save-interval 0)

  ;; neotree
  (setq neo-theme 'ascii)
  (custom-set-faces
   '(neo-banner-face ((t . (:inherit shadow :underline nil))) t)
   '(neo-header-face ((t . (:inherit shadow :underline nil))) t)
   '(neo-root-dir-face ((t . (:inherit link-visited :underline nil))) t)
   '(neo-dir-link-face ((t . (:inherit dired-directory :underline nil))) t)
   '(neo-file-link-face ((t . (:inherit default :underline nil))) t)
   '(neo-button-face ((t . (:inherit dired-directory :underline nil))) t)
   '(neo-expand-btn-face ((t . (:inherit button :underline nil))) t))

  ;; search engine
  (setq browse-url-browser-function 'browse-url-generic
        engine/browser-function 'browse-url-generic
        browse-url-generic-program "google-chrome")

  (helm-mode 1)
  (golden-ratio-mode 1))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (which-key web-mode use-package pug-mode persp-mode org-plus-contrib neotree indent-guide git-gutter-fringe eyebrowse elfeed highlight yasnippet request helm helm-core irony markdown-mode magit magit-popup git-commit zenburn-theme zeal-at-point yaml-mode xterm-color ws-butler with-editor window-numbering volatile-highlights vimrc-mode vi-tilde-fringe uuidgen toc-org tagedit sr-speedbar spacemacs-theme spaceline smeargle slim-mode shell-pop scss-mode sass-mode restart-emacs ranger rainbow-delimiters quelpa pcre2el password-store paradox ox-gfm orgit org-projectile org-present org-pomodoro org-download org-bullets open-junk-file nlinum-relative mwim multi-term mu4e-maildirs-extension mu4e-alert move-text modern-cpp-font-lock mmm-mode markdown-toc magit-gitflow macrostep lorem-ipsum link-hint less-css-mode irony-eldoc info+ ido-vertical-mode ibuffer-projectile hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter gh-md forecast flyspell-lazy flyspell-correct-helm flycheck-pos-tip flycheck-irony flx-ido fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-numbers evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-commentary evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help engine-mode emmet-mode elisp-slime-nav elfeed-web elfeed-org elfeed-goodies dumb-jump disaster diminish diff-hl deft define-word dactyl-mode company-web company-statistics company-quickhelp company-irony-c-headers company-irony company-c-headers column-enforce-mode cmake-mode clean-aindent-mode clang-format bind-key auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

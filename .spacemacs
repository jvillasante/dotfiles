(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     org
     search-engine
     shell-scripts
     syntax-checking
     (spell-checking :variables
                     spell-checking-enable-by-default nil)
     (ibuffer :variables
              ibuffer-group-buffers-by 'modes)
     (shell :variables
            shell-default-term-shell "/bin/zsh"
            shell-default-shell 'ansi-term
            shell-default-position 'bottom
            shell-default-height 30)
     (auto-completion :variables
                      auto-completion-return-key-behavior 'nil
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-complete-with-key-sequence-delay 0.1
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup t
                      :disabled-for org erc git)
     (evil-snipe :variables
                 evil-snipe-enable-alternate-f-and-t-behaviors t)
     evil-commentary
     vim-empty-lines
     (git :variables
          git-use-magit-next t
          git-enable-github-support t)
     (version-control :variables
                      version-control-diff-tool 'diff-hl
                      version-control-global-margin t)
     semantic
     ranger
     (html :variables
           css-indent-offset 2
           web-mode-code-indent-offset 2
           web-mode-markup-indent-offset 2
           web-mode-css-indent-offset 2)
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode
            c-c++-enable-clang-support t)
     gtags
     ;; ycmd
     go
     javascript
     markdown
     emacs-lisp
     ;; python
     restclient
     deft
     (elfeed :variables
             rmh-elfeed-org-files (list "~/Dropbox/Personal/elfeed/elfeed.org"))
     (mu4e :variables
           mu4e-installation-path "/usr/local/share/emacs/site-lisp/mu4e")

     ;; private layers
     my-irony
     my-forecast
     my-mu4e
     my-javascript
     my-password-store
     my-zeal)

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(dash evil-jumper smooth-scrolling ws-butler)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

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
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update t
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 2
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents bookmarks projects)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(zenburn
                         material
                         spacemacs-dark
                         spacemacs-light
                         solarized-dark
                         solarized-light
                         monokai
                         leuven)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 18
                               :weight normal
                               :width normal
                               :powerline-scale 1.15)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
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
   dotspacemacs-distinguish-gui-tab t
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to miminimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header t
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
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
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling nil
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
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
   dotspacemacs-whitespace-cleanup 'changed
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code."
  ;; start server
  ;; (server-start)

  (with-eval-after-load 'org
    ;; org problems
    (setq org-planning-line-re "^[    ]*\\(\\(?:CLOSED\\|DEADLINE\\|SCHEDULED\\):\\)")
    (setq org-clock-line-re "^[    ]*CLOCK:")

    (setq org-startup-indented t)
    (setq org-indent-mode t)

    ;; set maximum indentation for description lists
    (setq org-list-description-max-indent 5)

    ;; prevent demoting heading also shifting text inside sections
    (setq org-adapt-indentation nil)
    )

  ;; smooth scrolling
  (setq redisplay-dont-pause t
        scroll-margin 3
        scroll-step 1
        scroll-conservatively 101
        mouse-wheel-scroll-amount '(1)
        mouse-wheel-progressive-speed nil
        scroll-preserve-screen-position 1)

  (setq-default
   ;; Miscellaneous
   user-full-name "Julio C. Villasante"
   user-mail-address "jvillasantegomez@gmail.com"
   vc-follow-symlinks t
   ring-bell-function 'ignore
   require-final-newline t
   indent-tabs-mode nil
   system-time-locale "C"
   paradox-github-token t
   open-junk-file-find-file-function 'find-file
   load-prefer-newer t
   fill-column 110                    ; Maximum line width
   truncate-lines t                   ; Don't fold lines
   truncate-partial-width-windows nil ; for vertically-split windows
   split-width-threshold 160          ; Split verticly by default
   auto-fill-function 'do-auto-fill   ; Auto-fill-mode everywhere
   evil-cross-lines t                 ; Make horizontal movement cross lines

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

   ;; Magit
   git-magit-status-fullscreen t
   magit-popup-show-common-commands t

   ;; Flycheck
   flycheck-check-syntax-automatically '(save mode-enabled)

   ;; Avy
   avy-all-windows 'all-frames

   ;; Ranger
   ranger-override-dired t

   ;; deft
   deft-directory "~/Dropbox/Personal/notes"
   deft-extensions '("org" "md" "txt")
   deft-text-mode "org"
   deft-use-filename-as-title t
   deft-use-filter-string-for-filename t
   deft-auto-save-interval 0

   ;; tramp mode
   tramp-default-method "ssh"

   ;; LaTeX
   font-latex-fontify-script nil
   TeX-newline-function 'reindent-then-newline-and-indent

   ;; Web
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2)

  ;; ycmd
  ;; (set-variable 'ycmd-server-command '("python" "/home/jvillasante/Software/src/ycmd/ycmd"))
  ;; (set-variable 'ycmd-global-config "/home/jvillasante/Hacking/workspace/dotfiles/.emacs.d/layers/+tools/ycmd/global_conf.py")
  ;; (set-variable 'ycmd-extra-conf-whitelist '("/home/jvillasante/Hacking/workspace/*"))
  ;; (setq ycmd--log-enabled t)

  ;; utf-8
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

  ;; https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
  (setq display-time-world-list '(("UTC" "UTC")
                                  ("US/Eastern" "Miami")
                                  ("America/Havana" "Habana")
                                  ("America/New_York" "New York")
                                  ("Europe/Amsterdam" "Amsterdam")
                                  ("Europe/Copenhagen" "Denmark")
                                  ("Asia/Shanghai" "China")
                                  ("Asia/Calcutta" "India")))

  (setf url-queue-timeout 30))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
 This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."

  ;; default is 1000, increase the backtrace level
  (setq max-specpdl-size 3000)

  ;; line spacing
  (setq-default line-spacing 0.1)

  ;; Isearch convenience, space matches anything (non-greedy)
  (setq search-whitespace-regexp ".*?")

  ;; Misc
  (setq dired-listing-switches "-lha")
  (helm-mode)
  (global-highlight-parentheses-mode)
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  (global-hl-line-mode -1)
  ;; (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  ;; (add-hook 'lisp-mode-hook #'aggressive-indent-mode)
  ;; (add-hook 'scheme-mode-hook #'aggressive-indent-mode)
  (setf git-gutter-fr+-side 'left-fringe)
  (setf diff-hl-side 'left)
  (diff-hl-flydiff-mode)

  ;; helm
  ;; (setq helm-echo-input-in-header-line nil)
  (helm-projectile-on)
  (with-eval-after-load 'helm-mode
    (setq helm-google-suggest-use-curl-p t
          helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
          ;; helm-quick-update t ; do not display invisible candidates
          helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.

          ;; you can customize helm-do-grep to execute ack-grep
          ;; helm-grep-default-command "ack-grep -Hn --smart-case --no-group --no-color %e %p %f"
          ;; helm-grep-default-recurse-command "ack-grep -H --smart-case --no-group --no-color %e %p %f"
          helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window

          helm-echo-input-in-header-line nil

          ;; helm-candidate-number-limit 500 ; limit the number of displayed canidates
          helm-ff-file-name-history-use-recentf t
          helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
          helm-buffer-skip-remote-checking t

          helm-buffers-fuzzy-matching t ; fuzzy matching buffer names when non-nil
                                        ; useful in helm-mini that lists buffers
          helm-org-headings-fontify t
          ;; helm-find-files-sort-directories t
          ;; ido-use-virtual-buffers t
          helm-semantic-fuzzy-match t
          helm-M-x-fuzzy-match t
          helm-imenu-fuzzy-match t
          helm-lisp-fuzzy-completion t
          ;; helm-apropos-fuzzy-match t
          helm-buffer-skip-remote-checking t
          helm-locate-fuzzy-match t
          helm-display-header-line nil)

    (defun helm-hide-minibuffer-maybe ()
      (when (with-helm-buffer helm-echo-input-in-header-line)
        (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
          (overlay-put ov 'window (selected-window))
          (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                  `(:background ,bg-color :foreground ,bg-color)))
          (setq-local cursor-type nil))))
    (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

    (setq helm-ag-insert-at-point 'symbol)

    (setq helm-autoresize-max-height 40)
    (setq helm-autoresize-min-height 40)
    (helm-autoresize-mode 1)

    (with-eval-after-load 'helm-semantic
      (push '(c-mode . semantic-format-tag-summarize) helm-semantic-display-style)
      (push '(c++-mode . semantic-format-tag-summarize) helm-semantic-display-style))
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
    (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

    (define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
    (define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
    (define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)
    (define-key global-map [remap find-tag] 'helm-etags-select)
    ;; (define-key global-map [remap jump-to-register] 'helm-register)
    (define-key global-map [remap list-buffers] 'helm-mini)
    ;; (define-key global-map [remap find-tag] 'helm-gtags-select)
    (add-hook 'ielm-mode-hook
              #'(lambda ()
                  (define-key ielm-map [remap completion-at-point] 'helm-lisp-completion-or-file-name-at-point)))

    ;; tramp
    ;; (add-to-list 'tramp-default-proxies-alist
    ;;              '("10.51.58.80" nil "/ssh:xtuudoo@jogvan:"))

    (add-hook 'after-init-hook 'server-start)
    (setq server-raise-frame t)

    ;; (if window-system
    ;;     (add-hook 'server-done-hook
    ;;               (lambda () (shell-command "stumpish 'eval (stumpwm::return-es-called-win stumpwm::*es-win*)'"))))

    (cond
     ((eq (car custom-enabled-themes) 'darktooth)
      ;; (set-face-attribute 'region nil :background "#1a1a1a")
      (set-face-attribute 'header-line nil
                          :box '(:line-width -1 :style released-button))
      (set-face-attribute 'helm-source-header nil
                          :box '(:line-width -1 :style released-button)))
     ((eq (car custom-enabled-themes) 'jbeans)
      (set-face-attribute 'helm-ff-directory nil
                          :foreground (face-attribute 'helm-buffer-directory :foreground)))
     ((eq (car custom-enabled-themes) 'stekene-dark)
      (set-face-attribute 'fringe nil
                          :background "#242424"))
     ((eq (car custom-enabled-themes) 'jazz)
      ;; (set-face-attribute 'region nil
      ;;                     :foreground nil
      ;;                     :background "#000000")
      ;; (set-face-attribute 'hl-line nil
      ;;                     :foreground nil
      ;;                     :background "#404040")
      (set-face-attribute 'helm-selection nil
                          :foreground nil
                          :background nil
                          :inherit 'highlight)
      (set-face-attribute 'vhl/default-face nil
                          :foreground nil
                          :background "#1a1a1a")
      (set-face-attribute 'sp-show-pair-match-face nil
                          :foreground nil
                          :background "gray20")))

    (persp-mode)
    (ido-mode -1)
    (setq-default gc-cons-threshold 800000)
    (defun my-minibuffer-setup-hook ()
      (setq gc-cons-threshold most-positive-fixnum))

    (defun my-minibuffer-exit-hook ()
      (setq gc-cons-threshold 800000))

    (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
    (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)
    (spacemacs/home)
    )

  (add-hook 'mu4e-compose-mode-hook
            (lambda ()
              (auto-fill-mode 0)
              (visual-line-mode 1)))
  (add-hook 'markdown-mode-hook
            (lambda ()
              (auto-fill-mode 0)
              (visual-line-mode 1)))
  (add-hook 'prog-mode-hook
            (lambda ()
              (set-fill-column 80)
              (flyspell-prog-mode)))
  (add-hook 'org-mode-hook
            (lambda ()
              (set-fill-column 110)))
  (add-hook 'text-mode-hook 'turn-on-flyspell)
  (add-hook 'makefile-mode-hook 'whitespace-mode)
  (remove-hook 'prog-mode-hook 'spacemacs//show-trailing-whitespace)


  ;; Semantic fucks up scrolling
  (with-eval-after-load 'semantic
    (setq semantic-submode-list (delq 'global-semantic-stickyfunc-mode semantic-submode-list)))

  ;; Display Visited File's Path in the Frame Title
  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))))

  ;; c support
  (push '("pconnect_linux.*\\.h\\'" . c-mode) auto-mode-alist)
  (push '("learn-c-the-hard-way.*\\.h\\'" . c-mode) auto-mode-alist)

  ;; compilation mode
  (ignore-errors
    (require 'ansi-color)
    (defun my-colorize-compilation-buffer ()
      (when (eq major-mode 'compilation-mode)
        (ansi-color-apply-on-region compilation-filter-start (point-max))))
    (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))
  (defun my-desperately-compile ()
    "Traveling up the path, find a Makefile and `compile'."
    (interactive)
    (with-temp-buffer
      (while (and (not (file-exists-p "Makefile"))
                  (not (equal "/" default-directory)))
        (cd ".."))
      (when (file-exists-p "Makefile")
        (compile "make -k"))))
  (evil-leader/set-key
    "oc" 'my-desperately-compile)

  ;; use company everywhere
  (global-company-mode 1)

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

  ;; linum-mode
  (defun my-turn-off-linum-mode ()
    (message "Deactivated linum mode.")
    (linum-mode -1))
  (setq linum-delay t
        linum-eager nil
        linum-format "%4d")
  (add-hook 'prog-mode-hook 'linum-mode)
  (add-hook 'text-mode-hook 'linum-mode)
  (add-hook 'org-mode-hook 'my-turn-off-linum-mode)

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

  (defun my-hotspots ()
    "helm interface to my hotspots, which includes my locations, org-files and bookmarks"
    (interactive)
    (helm :sources `(((name . "Mail and News")
                      (candidates . (("Mail" . mu4e)
                                     ("Google Inbox" . (lambda () (browse-url "https://inbox.google.com")))
                                     ("RSS" . elfeed)
                                     ("Facebook" . (lambda ()  (browse-url "https://www.facebook.com/")))
                                     ("Calendar" . (lambda ()  (browse-url "https://www.google.com/calendar/render")))
                                     ("Agenda" . (lambda () (org-agenda "" "w")))))
                      (action . (("Open" . (lambda (x) (funcall x))))))
                     ;; ((name . "My Locations")
                     ;;   (candidates . (("master" . "~/Dropbox/org-mode/master.org")
                     ;;                   (".emacs.d" . "~/Dropbox/kitchingroup/jmax" )
                     ;;                   ("blog" . "~/blogofile-jkitchin.github.com/_blog/blog.org")
                     ;;                   ("ese" . "~/Dropbox/books/ese-book/ese.org" )
                     ;;                   ("passwords" . "~/Dropbox/org-mode/passwords.org.gpg")
                     ;;                   ("Pycse" . "~/Dropbox/books/pycse/pycse.org")
                     ;;                   ("references" . "~/Dropbox/bibliography/references.bib")
                     ;;                   ("notes" . "~/Dropbox/bibliography/notes.org")
                     ;;                   ("journal" . "~/Dropbox/org-mode/journal.org")
                     ;;                   ("tasks" . "~/Dropbox/org-mode/tasks.org")))
                     ;;   (action . (("Open" . (lambda (x) (find-file x))))))
                     ;; ((name . "My org files")
                     ;;   (candidates . ,(f-entries "~/Dropbox/org-mode"))
                     ;;   (action . (("Open" . (lambda (x) (find-file x))))))
                     helm-source-bookmarks
                     helm-source-bookmark-set
                     helm-source-recentf)))
  (evil-leader/set-key
    "oh" 'my-hotspots)

  ;; others
  (golden-ratio-mode 1)
  (global-evil-search-highlight-persist nil))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elfeed-goodies/entry-pane-position (quote bottom))
 '(elfeed-goodies/entry-pane-size 0.75)
 '(package-selected-packages
   (quote
    (pyvenv pytest pyenv-mode pip-requirements macrostep hy-mode helm-pydoc helm-gtags ggtags elisp-slime-nav cython-mode company-anaconda auto-compile packed anaconda-mode pythonic mu4e-alert ht irony-eldoc flycheck-irony company-irony irony zenburn-theme zeal-at-point xterm-color ws-butler window-numbering which-key web-mode web-beautify volatile-highlights use-package toc-org tagedit stickyfunc-enhance srefactor spacemacs-theme spaceline smeargle slim-mode shell-pop scss-mode sass-mode restclient restart-emacs ranger rainbow-delimiters quelpa persp-mode pcre2el password-store paradox page-break-lines orgit org-repo-todo org-present org-pomodoro org-plus-contrib org-bullets open-junk-file nodejs-repl neotree multi-term move-text mmm-mode markdown-toc magit-gitflow magit-gh-pulls lorem-ipsum linum-relative leuven-theme less-css-mode json-mode js2-refactor js-doc jade-mode info+ indent-guide ido-vertical-mode ibuffer-projectile hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flyspell helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag google-translate golden-ratio go-eldoc gnuplot github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md forecast flycheck-pos-tip flx-ido fish-mode fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-snipe evil-search-highlight-persist evil-numbers evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-commentary evil-args evil-anzu eval-sexp-fu eshell-prompt-extras esh-help engine-mode emmet-mode elfeed-web elfeed-org elfeed-goodies disaster diff-hl deft define-word company-web company-tern company-statistics company-quickhelp company-go company-c-headers coffee-mode cmake-mode clean-aindent-mode clang-format buffer-move bracketed-paste auto-yasnippet auto-highlight-symbol auto-dictionary aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(safe-local-variable-values
   (quote
    ((c-c++-default-mode-for-headers . c-mode)
     (c-c++-default-mode-for-headers . c++-mode)
     (c-c++-default-mode-for-headers quote c++-mode)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#DCDCCC" :background "#3F3F3F"))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(neo-banner-face ((t :inherit shadow :underline nil)) t)
 '(neo-button-face ((t :inherit dired-directory :underline nil)) t)
 '(neo-dir-link-face ((t :inherit dired-directory :underline nil)) t)
 '(neo-expand-btn-face ((t :inherit button :underline nil)) t)
 '(neo-file-link-face ((t :inherit default :underline nil)) t)
 '(neo-header-face ((t :inherit shadow :underline nil)) t)
 '(neo-root-dir-face ((t :inherit link-visited :underline nil)) t))

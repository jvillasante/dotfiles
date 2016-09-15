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
     (auto-completion :variables
                      auto-completion-return-key-behavior 'nil
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-complete-with-key-sequence-delay 0.1
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup t
                      :disabled-for org erc git)
     better-defaults
     emacs-lisp
     (git :variables
          git-use-magit-next t
          git-enable-github-support t)
     github
     ;; version-control
     markdown
     org
     (ibuffer :variables
              ibuffer-group-buffers-by 'projects)
     (ranger :variables
             ranger-show-preview t)
     (shell :variables
            shell-default-term-shell "/bin/zsh"
            shell-default-shell 'ansi-term
            shell-default-position 'bottom
            shell-default-height 30)
     spell-checking
     syntax-checking
     search-engine
     version-control
     evil-commentary
     (html :variables
           css-indent-offset 2
           web-mode-code-indent-offset 2
           web-mode-markup-indent-offset 2
           web-mode-css-indent-offset 2)
     ;; semantic
     ;; gtags
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode
            c-c++-enable-clang-support t)
     go
     restclient
     deft
     (elfeed :variables
             rmh-elfeed-org-files (list (concat my-dropbox-path "/Personal/elfeed/elfeed.org")))
     (mu4e :variables
           mu4e-installation-path my-mu4e-path)
     jvillasante-mu4e
     jvillasante)
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(dash
                                    evil-search-highlight-persist
                                    smooth-scrolling)
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
   dotspacemacs-startup-banner nil
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(projects recents)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 10
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
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 18
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
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
   dotspacemacs-distinguish-gui-tab nil
   ;; (Not implemented) dotspacemacs-distinguish-gui-ret nil
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
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
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
   dotspacemacs-mode-line-unicode-symbols nil
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
  ;; scroll one line at a time (less "jumpy" than defaults)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
  (setq mouse-wheel-progressive-speed nil)            ; don't accelerate scrolling
  (setq-default smooth-scroll-margin 3)
  (setq scroll-step 1
        scroll-margin 3
        scroll-conservatively 100000)

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

   ;; deft
   deft-directory (concat my-dropbox-path "/Personal/notes")
   deft-extensions '("org" "md" "txt")
   deft-text-mode "org"
   deft-use-filename-as-title t
   deft-use-filter-string-for-filename t
   deft-auto-save-interval 0

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
              (set-fill-column 110)
              (flyspell-prog-mode)))
  (add-hook 'org-mode-hook
            (lambda ()
              (spacemacs/toggle-auto-fill-mode-on)
              (set-fill-column 110)))
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
  (add-hook 'after-init-hook 'server-start)
  (setq server-raise-frame t)

  (helm-mode 1)
  (golden-ratio-mode 1))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elfeed-goodies/entry-pane-position (quote bottom))
 '(elfeed-web-enabled t)
 '(package-selected-packages
   (quote
    (f hydra spinner ht alert log4e gntp markdown-mode parent-mode request haml-mode gitignore-mode fringe-helper git-gutter+ git-gutter pkg-info epl flx magit-popup git-commit with-editor iedit anzu highlight simple-httpd s ace-jump-mode noflet powerline popwin web-completion-data pos-tip irony company packed dash async auto-complete package-build bind-key bind-map evil avy flycheck yasnippet popup helm-core projectile elfeed smartparens helm magit modern-cpp-font-lock zenburn-theme zeal-at-point xterm-color ws-butler window-numbering which-key web-mode volatile-highlights vi-tilde-fringe use-package toc-org tagedit spacemacs-theme spaceline smeargle slim-mode shell-pop scss-mode sass-mode restclient restart-emacs rainbow-delimiters quelpa persp-mode pcre2el password-store paradox page-break-lines orgit org-repo-todo org-present org-pomodoro org-plus-contrib org-bullets open-junk-file neotree multi-term mu4e-alert move-text mmm-mode markdown-toc magit-gitflow macrostep lorem-ipsum linum-relative leuven-theme less-css-mode jade-mode irony-eldoc info+ indent-guide ido-vertical-mode ibuffer-projectile hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flyspell helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-gutter-fringe git-gutter-fringe+ gh-md forecast flyspell-lazy flycheck-pos-tip flycheck-irony flx-ido fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-numbers evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-commentary evil-args evil-anzu eval-sexp-fu eshell-prompt-extras esh-help engine-mode emmet-mode elisp-slime-nav elfeed-web elfeed-org elfeed-goodies disaster diff-hl deft define-word company-web company-statistics company-quickhelp company-irony-c-headers company-irony company-c-headers cmake-mode clean-aindent-mode clang-format buffer-move bracketed-paste auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(neo-banner-face ((t :inherit shadow :underline nil)) t)
 '(neo-button-face ((t :inherit dired-directory :underline nil)) t)
 '(neo-dir-link-face ((t :inherit dired-directory :underline nil)) t)
 '(neo-expand-btn-face ((t :inherit button :underline nil)) t)
 '(neo-file-link-face ((t :inherit default :underline nil)) t)
 '(neo-header-face ((t :inherit shadow :underline nil)) t)
 '(neo-root-dir-face ((t :inherit link-visited :underline nil)) t))

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
     better-defaults
     emacs-lisp
     spell-checking
     search-engine
     ibuffer
     (auto-completion :variables
                      auto-completion-return-key-behavior 'nil
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup t)
     version-control
     (git :variables
          git-magit-status-fullscreen t)
     (html :variables
           css-indent-offset 2
           web-mode-code-indent-offset 2
           web-mode-markup-indent-offset 2
           web-mode-css-indent-offset 2)
     semantic
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode
            c-c++-enable-clang-support t)
     go
     javascript
     markdown
     latex
     shell-scripts
     (shell :variables
            shell-default-term-shell "/bin/zsh"
            shell-default-shell 'multi-term
            shell-default-position 'bottom
            shell-default-height 30)
     (org :variables
          org-enable-github-support t)
     syntax-checking
     restclient
     evil-commentary
     ;; eyebrowse
     deft
     chrome
     osx
     dash

     ;; private layers
     ;; my-symon
     my-sunshine
     my-javascript
     ;; my-mail
     ;; my-define-word
     my-password-store
     my-rss
     ;; my-zeal
     my-twitter)

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
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
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(zenburn
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
                               :powerline-scale 1.2)
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
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to miminimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
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
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
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
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code."

  (defun system-is-mac ()
    (interactive)
    (string-equal system-type "darwin"))

  (defun system-is-linux ()
    (interactive)
    (string-equal system-type "gnu/linux"))

  ;; start server
  (server-start)

  ;; ispell
  (if (system-is-mac)
      (setq ispell-program-name "/usr/local/bin/aspell"))

  ;; utf-8
  (prefer-coding-system 'utf-8)
  (set-language-environment 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)

  ;; Get rid of these no matter what, and do it early
  (setq load-prefer-newer t)

  ;; me
  (setq user-full-name "Julio C. Villasante")
  (setq user-mail-address "jvillasantegomez@gmail.com")

  ;; Suppressing ad-handle-definition Warnings
  (setq ad-redefinition-action 'accept)

  ;; my coding style, bsd but with 2 spaces indentation (and no tab
  ;; characters, only spaces)
  (setq-default c-basic-indent 2 c-basic-offset 2)
  (setq-default tab-width 2 indent-tabs-mode nil)
  (setq-default highlight-tabs t)

  ;; Whitespace settings
  (setq whitespace-action '(auto-cleanup))
  (setq whitespace-style '(indentation::space
                           space-after-tab
                           space-before-tab
                           trailing
                           lines-tail
                           tab-mark
                           face
                           tabs))
  ;; remove whitespace before saving
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; dont wrap lines
  (setq-default truncate-lines t)
  (setq truncate-partial-width-windows nil) ;; for vertically-split windows

  ;; show matching parens
  (show-paren-mode t)

  ;; multiterm
  (setq multi-term-program "/bin/zsh")
  (add-hook 'term-mode-hook
            (lambda ()
              (setq term-buffer-maximum-size 10000)))

  ;; env
  ;; (defun my-set-PATH-from-shell-PATH ()
  ;;   (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
  ;;     (setenv "PATH" path-from-shell)
  ;;     (setq exec-path (split-string path-from-shell path-separator))))
  ;; (if window-system
  ;;     (progn
  ;;       (setenv "GOPATH" "~/Hacking/workspace/go")))

  ;; Allow editing of binary .plist files.
  (add-to-list 'jka-compr-compression-info-list
               ["\\.plist$"
                "converting text XML to binary plist"
                "plutil"
                ("-convert" "binary1" "-o" "-" "-")
                "converting binary plist to text XML"
                "plutil"
                ("-convert" "xml1" "-o" "-" "-")
                nil nil "bplist"])

  ;; It is necessary to perform an update!
  (jka-compr-update)

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

  ;; company
  (global-company-mode)

  ;; deft
  (setq deft-directory "~/Dropbox/Personal/notes")
  (setq deft-extensions '("org" "md" "txt"))
  (setq deft-text-mode "org")
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-auto-save-interval 0)

  ;; tramp mode
  (setq tramp-default-method "ssh")

  ;; use evil-matchit everywhere
  (global-evil-matchit-mode 1)

  ;; Make movement keys work like they should
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

  ;; Make horizontal movement cross lines
  (setq-default evil-cross-lines t)

  ;; use count-words instead of count-words-region as it works on buffer
  ;; if no region is selected
  (global-set-key (kbd "M-=") 'count-words)

  ;; don't use default persistent search highlight
  evil-search-highlight-persist nil

  ;; linum-mode
  (setq linum-delay t
        linum-eager nil)
  (add-hook 'prog-mode-hook 'linum-mode)
  (setq linum-format "%4d")

  ;; turn off linum-mode on org files
  (defun my-turn-off-linum-mode ()
    (message "Deactivated linum mode.")
    (linum-mode -1))
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

  (defun my-hotspots ()
    "helm interface to my hotspots, which includes my locations, org-files and bookmarks"
    (interactive)
    (helm :sources `(((name . "Mail and News")
                      (candidates . (;; ("Mail"  . mu4e)
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
  ;; (golden-ratio-mode 1)
  (global-evil-search-highlight-persist -1)
)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (diminish go-mode highlight multiple-cursors json-reformat tern popup reveal-in-osx-finder pbcopy launchctl helm-dash dash-at-point auto-complete f async go-eldoc company-go markdown-mode magit-popup html-to-markdown hydra symon avy yasnippet haml-mode gitignore-mode git-commit company auctex evil-leader evil package-build bind-key s dash anzu smartparens flycheck helm helm-core projectile js2-mode magit smeargle paradox linum-relative leuven-theme helm-swoop google-translate alert zenburn-theme zeal-at-point window-numbering which-key web-mode web-beautify volatile-highlights vi-tilde-fringe use-package twittering-mode toc-org tagedit sunshine stickyfunc-enhance srefactor spray spinner spacemacs-theme smooth-scrolling slim-mode shell-pop scss-mode sass-mode restclient rainbow-delimiters quelpa powerline popwin pcre2el password-store page-break-lines org-repo-todo org-present org-pomodoro org-plus-contrib org-bullets open-junk-file nodejs-repl neotree multi-term move-text monokai-theme mmm-mode markdown-toc magit-gitflow macrostep log4e less-css-mode json-mode js2-refactor js-doc jade-mode info+ indent-guide ido-vertical-mode ibuffer-projectile hungry-delete htmlize highlight-parentheses highlight-numbers highlight-indentation helm-themes helm-projectile helm-mode-manager helm-make helm-gitignore helm-flyspell helm-descbinds helm-css-scss helm-c-yasnippet helm-ag golden-ratio gnuplot gntp gmail-message-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger gh-md flycheck-pos-tip flx-ido fish-mode fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-org evil-numbers evil-matchit evil-lisp-state evil-jumper evil-indent-textobject evil-iedit-state evil-exchange evil-escape evil-commentary evil-args evil-anzu eval-sexp-fu eshell-prompt-extras esh-help engine-mode emmet-mode elisp-slime-nav elfeed edit-server disaster diff-hl deft define-word company-web company-tern company-statistics company-quickhelp company-c-headers company-auctex coffee-mode cmake-mode clean-aindent-mode clang-format buffer-move auto-yasnippet auto-highlight-symbol auto-dictionary aggressive-indent adaptive-wrap ace-window ace-link ac-ispell)))
 '(paradox-github-token t)
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
 '(neo-banner-face ((t :inherit shadow :underline nil)))
 '(neo-button-face ((t :inherit dired-directory :underline nil)))
 '(neo-dir-link-face ((t :inherit dired-directory :underline nil)))
 '(neo-expand-btn-face ((t :inherit button :underline nil)))
 '(neo-file-link-face ((t :inherit default :underline nil)))
 '(neo-header-face ((t :inherit shadow :underline nil)))
 '(neo-root-dir-face ((t :inherit link-visited :underline nil))))

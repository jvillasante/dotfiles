;; -*- mode: dotspacemacs -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers '(better-defaults
                                       ;; editorconfig
                                       search-engine
                                       ibuffer
                                       (auto-completion :variables
                                                        auto-completion-return-key-behavior 'nil
                                                        auto-completion-tab-key-behavior 'complete
                                                        auto-completion-complete-with-key-sequence nil
                                                        auto-completion-enable-help-tooltip t
                                                        auto-completion-enable-sort-by-usage t
                                                        auto-completion-enable-snippets-in-popup t)
                                       (colors :variables
                                               colors-enable-rainbow-identifiers nil)
                                       version-control
                                       github
                                       (git :variables
                                            git-magit-status-fullscreen t)
                                       gtags
                                       (html :variables
                                             css-indent-offset 2
                                             web-mode-code-indent-offset 2
                                             web-mode-markup-indent-offset 2
                                             web-mode-css-indent-offset 2)
                                       javascript
                                       markdown
                                       c-c++
                                       go
                                       ;; php
                                       sql
                                       emacs-lisp
                                       latex
                                       shell-scripts
                                       extra-langs
                                       semantic
                                       (shell :variables
                                              shell-default-term-shell "/bin/zsh"
                                              shell-default-shell 'multi-term
                                              shell-default-position 'bottom
                                              shell-default-height 30)
                                       (org :variables
                                            org-enable-github-support t)
                                       syntax-checking
                                       ycmd
                                       evil-commentary
                                       restclient
                                       themes-megapack
                                       slime
                                       vim-empty-lines
                                       eyebrowse
                                       deft

                                       ;; private layers
                                       ;; my-symon
                                       my-mail
                                       my-define-word
                                       my-password-store
                                       my-rss
                                       my-twitter
                                       my-zeal)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(dash)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progess in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to a .PNG file.
   ;; If the value is nil then no banner is displayed.
   ;; dotspacemacs-startup-banner 'official
   dotspacemacs-startup-banner 1
   ;; t if you always want to see the changelog at startup
   dotspacemacs-always-show-changelog nil
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(zenburn
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
                               :slant normal
                               :powerline-scale 1.2)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil the paste micro-state is enabled. While enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state t
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; use ido on find files instead of helm
   dotspacemacs-use-ido nil
   ;; Add =highlight-parentheses= package which can activated by setting
   dotspacemacs-highlight-delimiters 'current
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil)
  ;; User initialization goes here

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

  ;; multiterm
  (setq multi-term-program "/bin/zsh")
  (add-hook 'term-mode-hook
            (lambda ()
              (setq term-buffer-maximum-size 10000)))

  ;; yasnippet
  ;; (setq yas-snippet-dirs '("~/.emacs.d/private/snippets"
  ;;                          yas-installed-snippets-dir))

  ;; ycmd
  (set-variable 'ycmd-server-command '("python" "~/bin/ycmd/ycmd"))

  ;; env
  (defun set-PATH-from-shell-PATH ()
    (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
      (setenv "PATH" path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator))))
  (if window-system
      (progn
        (set-PATH-from-shell-PATH)
        (setenv "GOPATH" "/home/jvillasante/Hacking/workspace/go")))

  ;; Enable mouse support
  (unless window-system
    (require 'mouse)
    (xterm-mouse-mode t)
    (global-set-key [mouse-4] (lambda ()
                                (interactive)
                                (scroll-down 1)))
    (global-set-key [mouse-5] (lambda ()
                                (interactive)
                                (scroll-up 1)))
    (defun track-mouse (e))
    (setq mouse-sel-mode t
          mouse-yank-at-point t))

  (setq x-select-enable-clipboard t
        x-select-enable-primary t
        save-interprogram-paste-before-kill t
        apropos-do-all t
        require-final-newline t
        visible-bell t
        ediff-window-setup-function 'ediff-setup-windows-plain
        redisplay-dont-pause t
        git-enable-github-support t)

  ;; smooth scroll
  ;; (setq redisplay-dont-pause t
  ;;   scroll-margin 5
  ;;   scroll-step 1
  ;;   scroll-conservatively 10000
  ;;   scroll-preserve-screen-position 3)

  ;; emacs url-queue package timeout
  (setf url-queue-timeout 30))

(defun dotspacemacs/config ()
  "Configuration function.
  This function is called at the very end of Spacemacs initialization after
  layers configuration."

  ;; my coding style, bsd but with 2 spaces indentation (and no tab
  ;; characters, only spaces)
  (setq-default c-basic-indent 2 c-basic-offset 2)
  (setq-default tab-width 2 indent-tabs-mode nil)
  (setq-default highlight-tabs t)

  ;; Whitespace settings
  (setq whitespace-action '(auto-cleanup))
  (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))

  ;; remove whitespace before saving
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; dont wrap lines
  (setq-default truncate-lines t)

  ;; show matching parens
  (show-paren-mode t)

  ;; change projectile action
  (projectile-global-mode)
  (setq projectile-switch-project-action 'helm-projectile)

  ;; Display Visited File's Path in the Frame Title
  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))))

  ;; Fortune path
  (require 'fortune)
  (setq fortune-dir "/usr/share/games/fortunes"
        fortune-file "/usr/share/games/fortunes/fortunes")

  ;; company
  (global-company-mode)

  ;; deft
  (use-package deft
    :config
    (progn
      (setq deft-directory "~/Dropbox/Personal/Notes")
      (setq deft-extension "org")
      (setq deft-text-mode 'org-mode)
      (setq deft-use-filename-as-title t)
      (setq deft-use-filter-string-for-filename t)
      (setq deft-auto-save-interval 0)
      ))

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

  ;; ycmd
  (add-hook 'after-init-hook #'global-ycmd-mode)

  ;; don't use default persistent search highlight
  evil-search-highlight-persist nil

  ;; line numbers when i'm coding pls
  (add-to-hooks 'linum-mode '(c-mode-hook
                              c++-mode-hook
                              clojure-mode-hook
                              css-mode-hook
                              dockerfile-mode-hook
                              emacs-lisp-mode-hook
                              go-mode-hook
                              haml-mode-hook
                              haskell-mode-hook
                              html-mode-hook
                              jade-mode-hook
                              jekyll-html-mode-hook
                              js2-mode-hook
                              jsx-mode-hook
                              lisp-mode-hook
                              php-mode-hook
                              python-mode-hook
                              sass-mode-hook
                              scss-mode-hook
                              stylus-mode-hook
                              text-mode-hook
                              web-mode-hook
                              yaml-mode-hook))
  (setq linum-format "%4d")

  ;; turn off linum-mode on org files
  (defun my/turn-off-linum-mode ()
    (message "Deactivated linum mode.")
    (linum-mode -1))
  (add-hook 'org-mode-hook 'my/turn-off-linum-mode)

  ;; TODO move js2-mode settings to config layer
  (eval-after-load 'js2-mode
    `(progn
       ;; BUG: self is not a browser extern, just a convention that needs checking
       (setq js2-browser-externs (delete "self" js2-browser-externs))

       ;; Consider the chai 'expect()' statement to have side-effects, so we don't warn about it
       (defun js2-add-strict-warning (msg-id &optional msg-arg beg end)
         (if (and js2-compiler-strict-mode
                  (not (and (string= msg-id "msg.no.side.effects")
                            (string= (buffer-substring-no-properties beg (+ beg 7)) "expect("))))
             (js2-report-warning msg-id msg-arg beg
                                 (and beg end (- end beg)))))))
  (setq js2-basic-offset 2
        js2-bounce-indent-p t)
  (add-hook 'js2-mode-hook (lambda () (electric-indent-mode -1)))

  ;; Highlight node.js stacktraces in *compile* buffers
  (defvar my-nodejs-compilation-regexp
    '("^[ \t]+at +\\(?:.+(\\)?\\([^()\n]+\\):\\([0-9]+\\):\\([0-9]+\\))?$" 1 2 3))

  (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'nodejs my-nodejs-compilation-regexp))
  (add-to-list 'compilation-error-regexp-alist 'nodejs)

  ;; Open files that start with "#!/usr/bin/env node" in js2-mode
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))

  ;; jsx syntax highlighting with web-mode
  (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))

  ;; jsx syntax checking for web-mode
  (require 'flycheck)
  (setq flycheck-check-syntax-automatically '(save mode-enabled idle-change))
  (setq flycheck-idle-change-delay 5)
  (flycheck-define-checker jsxhint-checker
    "A JSX syntax and style checker based on JSXHint."
    :command ("jsxhint" (config-file "--config=" jshint-configuration-path) source)
    :error-patterns ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
    :modes (web-mode))

  (defun find-jshintrc ()
    (expand-file-name ".jshintrc"
                      (locate-dominating-file
                       (or (buffer-file-name) default-directory) ".jshintrc")))

  (add-hook 'web-mode-hook
            (lambda ()
              (when (equal web-mode-content-type "jsx")
                (tern-mode t)
                (setq-local jshint-configuration-path (find-jshintrc))
                (flycheck-select-checker 'jsxhint-checker)
                (flycheck-mode))))

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

  ;; sqlite client
  (setq sql-postgres-login-params
        '((user :default "postgres")
          (database :default "reactive-dev")
          (server :default "localhost")
          (port :default 5432)))

  (defun hotspots ()
    "helm interface to my hotspots, which includes my locations, org-files and bookmarks"
    (interactive)
    (helm :sources `(((name . "Mail and News")
                      (candidates . (("Mail"  . mu4e)
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
    "oh" 'hotspots)

  ;; others
  (golden-ratio-mode 1)
  (global-evil-search-highlight-persist -1))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(global-mark-ring-max 5000)
 '(linum-delay t)
 '(linum-eager nil)
 '(paradox-github-token t)
 '(ring-bell-function (quote ignore) t))
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

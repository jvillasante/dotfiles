;;; config.el --- Rational Emacs Main Config -*- lexical-binding: t; -*-
;;;

(setq next-line-add-newlines t)

(setq user-full-name "Julio C. Villasante"
    user-mail-address "jvillasantegomez@gmail.com"
    user-login-name "jvillasante"
    +my/home-path (expand-file-name "~/")
    +my/dotfiles-path (expand-file-name "Workspace/Public/dotfiles/" +my/home-path)
    +my/software-path (expand-file-name "Workspace/Software/" +my/home-path)
    +my/dropbox-path (expand-file-name "Dropbox/" +my/home-path)
    +my/splash-path (expand-file-name "Misc/splash/emacs-logo.png" +my/dotfiles-path))

(cond
    (ON-MAC
        (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
        (setq browse-url-browser-function 'browse-url-generic
            browse-url-generic-program "open")
        (setq +my/zsh-path "/usr/local/bin/zsh"
            +my/clang-path "/usr/local/opt/llvm/bin/clang"
            +my/mu-path "/usr/local/bin/mu"
            +my/msmtp-path "/usr/local/bin/msmtp"
            vterm-module-cmake-args " -DUSE_SYSTEM_LIBVTERM=yes")
        (setq ns-use-proxy-icon         nil
            ns-use-thin-smoothing     t
            ns-alternate-modifier     nil
            mac-command-modifier      'meta
            mac-option-modifier       'alt
            mac-right-option-modifier 'alt))
    (ON-LINUX
        (setq browse-url-browser-function 'browse-url-generic
            browse-url-generic-program "xdg-open")
        (setq +my/zsh-path "/usr/bin/zsh"
            +my/clang-path "/usr/bin/clang"
            +my/mu-path "/usr/bin/mu"
            +my/msmtp-path "/usr/bin/msmtp"
            vterm-module-cmake-args " -DUSE_SYSTEM_LIBVTERM=yes")))

;; Start maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Select and raise the frame, always
(select-frame-set-input-focus (selected-frame))

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; org-directory needs to be set early
(setq org-directory (expand-file-name "Apps/org" +my/dropbox-path))

;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)

;;; rational modules
;;;
(require 'rational-defaults)    ; Sensible default settings for Emacs
(require 'rational-use-package) ; Configuration for `use-package`
(require 'rational-updates)     ; Tools to upgrade Rational Emacs
(require 'rational-completion)  ; selection framework based on `vertico`
(require 'rational-ui)          ; Better UI experience (modeline etc.)
(require 'rational-windows)     ; Window management configuration
(require 'rational-editing)     ; Whitspace trimming, auto parens etc.
;; (require 'rational-evil)        ; An `evil-mode` configuration
(require 'rational-org)         ; org-appear, clickable hyperlinks etc.
(require 'rational-project)     ; built-in alternative to projectile
(require 'rational-speedbar)    ; built-in file-tree
;; (require 'rational-screencast)  ; show current command and binding in modeline
(require 'rational-compile)     ; automatically compile some emacs lisp files

;; font
(custom-set-variables
    '(rational-ui-default-font
         '(:font "Iosevka" :weight normal :width normal :height 160)))

;; theme
(defun jv/switch-theme (theme)
    ;; This interactive call is taken from `load-theme'
    (interactive
        (list
            (intern (completing-read "Load custom theme: "
                        (mapcar 'symbol-name
                            (custom-available-themes))))))
    (mapcar #'disable-theme custom-enabled-themes)
    (load-theme theme t))
(rational-package-install-package 'modus-themes)
(jv/switch-theme 'modus-operandi)

;; neotree
(rational-package-install-package 'neotree)
(setq neo-theme 'ascii
    neo-window-width 42
    neo-smart-open t
    neo-create-file-auto-open nil
    neo-show-updir-line t
    neo-show-hidden-files t
    neo-auto-indent-point t
    neo-vc-integration nil
    neo-autorefresh nil
    ;; projectile-switch-project-action 'neotree-projectile-action
    neo-hidden-regexp-list
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
         "\\.egg\-info$"))

;; speed-type
(rational-package-install-package 'speed-type)

;; crux
(rational-package-install-package 'crux)

;; bindings
;;

;; get rid of `find-file-read-only' and replace it with something more useful
(global-set-key (kbd "C-x C-r") 'crux-recentf-find-file)

;; "C-c f" to `crux-cleanup-buffer-or-region'
(global-set-key (kbd "C-c f") 'crux-cleanup-buffer-or-region)

;; "C-c n" to `neotree-toggle'
(global-set-key (kbd "C-c n") 'neotree-toggle)

;; "C-x K" `kill-this-buffer'
(global-set-key (kbd "C-x K") 'kill-this-buffer)

;; To not load `custom.el' after `config.el', uncomment this line.
;; (setq rational-load-custom-file nil)

;;; example-config.el ends here

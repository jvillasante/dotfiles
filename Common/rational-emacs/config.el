;;; config.el --- Rational Emacs Main Config -*- lexical-binding: t; -*-
;;;

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
        (setq +my/clang-path "/usr/local/opt/llvm/bin/clang"
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
        (setq +my/clang-path "/usr/bin/clang"
            +my/mu-path "/usr/bin/mu"
            +my/msmtp-path "/usr/bin/msmtp"
            vterm-module-cmake-args " -DUSE_SYSTEM_LIBVTERM=yes")))

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

;; Some Defaults
;;
;; (setq next-line-add-newlines t) ; C-n inserts new line to avoid "end of buffer" errors
(setq scroll-error-top-bottom 1)   ; allow pgup to move to the 1st line
(setq help-window-select t)        ; https://emacs.stackexchange.com/q/21770
(setq ring-bell-function 'ignore)  ; no flashing!

;; Taken from https://github.com/Ergus/mini_dotemacs
(setq-default auto-revert-verbose t         ; show message when file changes
              auto-revert-avoid-polling t)  ; use save signal
(global-auto-revert-mode t)                 ; Autoload files changed on disk
(delete-selection-mode t)                   ; Replace selection with typing
(save-place-mode 1)                         ; Remember point in files

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

;; Auto-save on focus lost - https://www.emacswiki.org/emacs/AutoSave
(defun +my/save-all() (interactive) (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)

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
(global-set-key (kbd "C-c n") 'neotree-toggle)

;; crux - collection of ridiculously useful extensions for emacs
;; https://github.com/bbatsov/crux
;; https://superuser.com/a/1228716/45605
(rational-package-install-package 'crux)
(global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
(global-set-key (kbd "C-c d") #'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-x C-r") 'crux-recentf-find-file)
(global-set-key (kbd "C-c f") 'crux-cleanup-buffer-or-region)

;; helpful - Prettier docs
;; https://github.com/wilfred/helpful
(rational-package-install-package 'helpful)
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h o") #'helpful-symbol)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h C") #'helpful-command)

;; expand-region - Expand region increases the selected region by semantic units
;; https://github.com/magnars/expand-region.el
(rational-package-install-package 'expand-region)
(global-set-key (kbd "C-=") #'er/expand-region)
(global-set-key (kbd "M-<up>") #'er/expand-region)
(global-set-key (kbd "M-<down>") #'er/contract-region)

;; speed-type
(rational-package-install-package 'speed-type)

;;
;; bindings

;; instead of `kill-buffer', which prompts for buffer name https://superuser.com/a/354878
(global-set-key (kbd "C-x k") #'kill-this-buffer)
(global-set-key (kbd "C-x K") #'kill-buffer)

;; https://emacs.stackexchange.com/questions/835/make-buffer-list-take-focus
(global-set-key [remap list-buffers] #'ibuffer)

;; To not load `custom.el' after `config.el', uncomment this line.
;; (setq rational-load-custom-file nil)

;;; example-config.el ends here

;;; personal/init.el --- Prelude personal init -*- lexical-binding: t; -*-
;;;

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

;; Auto-save on focus lost - https://www.emacswiki.org/emacs/AutoSave
(defun +my/save-all() (interactive) (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)

;;; font
(set-face-attribute 'default nil :family "Iosevka" :height 160 :weight 'normal :width 'normal)

;;; theme
(defun +my/switch-theme (theme)
    "This interactive call is taken from `load-theme'"
    (interactive
        (list
            (intern (completing-read "Load custom theme: "
                        (mapcar 'symbol-name
                            (custom-available-themes))))))
    (mapcar #'disable-theme custom-enabled-themes)
    (load-theme theme t))
(prelude-require-package 'modus-themes)
(+my/switch-theme 'modus-operandi)

;; neotree
(prelude-require-package 'neotree)
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
(define-key prelude-mode-map (kbd "C-c n") nil)
(global-set-key (kbd "C-c n") 'neotree-toggle)

(prelude-require-package 'helpful)
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h o") #'helpful-symbol)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h C") #'helpful-command)

;; expand-region - Expand region increases the selected region by semantic units
;; https://github.com/magnars/expand-region.el
(prelude-require-package 'expand-region)
(global-set-key (kbd "C-=") #'er/expand-region)
(global-set-key (kbd "M-<up>") #'er/expand-region)
(global-set-key (kbd "M-<down>") #'er/contract-region)

;; speed-type
(prelude-require-package 'speed-type)

;; Bindings
;;

;; get rid of `find-file-read-only' and replace it with something more useful
(global-set-key (kbd "C-x C-r") 'crux-recentf-find-file)

;; remap "C-c f" to `crux-cleanup-buffer-or-region'
(define-key prelude-mode-map (kbd "C-c f") nil)
(global-set-key (kbd "C-c f") 'crux-cleanup-buffer-or-region)

;; instead of `kill-buffer', which prompts for buffer name https://superuser.com/a/354878
(global-set-key (kbd "C-x k") #'kill-this-buffer)
(global-set-key (kbd "C-x K") #'kill-buffer)

;; ;; https://emacs.stackexchange.com/questions/835/make-buffer-list-take-focus
;; (global-set-key [remap list-buffers] #'ibuffer)

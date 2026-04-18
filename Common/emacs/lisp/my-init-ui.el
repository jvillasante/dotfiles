;;; my-init-ui.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; Start maximized
;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; remove the title bar only when frame is maximized
;; (add-hook 'window-size-change-functions 'frame-hide-title-bar-when-maximized)

;; Select and raise the frame, always
(add-hook 'server-after-make-frame-hook
    (lambda ()
        (select-frame-set-input-focus (selected-frame))))

;; Resizing the Emacs frame can be costly when changing the font. Disable this
;; to improve startup times with fonts larger than the system default.
(setq frame-resize-pixelwise t)

;; However, do not resize windows pixelwise, as this can cause crashes in some
;; cases when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)
(setq resize-mini-windows 'grow-only)

;; Make URLs in comments/strings clickable, (Emacs > v22).
;; (add-hook 'find-file-hooks 'goto-address-prog-mode)

;; Help Windows
(setq help-window-select t)
(setq help-window-keep-selected t)

;; default amount of padding to use when calling `aligh-regexp'
(setq align-default-spacing 0)

;; activate `mouse-avoidance-mode'
(mouse-avoidance-mode 'cat-and-mouse)

;; avoid inadvertently changing font size when scrolling
(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)

;; emacs29 - mouse settings
(when (>= emacs-major-version 29)
    (setq dired-mouse-drag-files t)
    (setq mouse-drag-and-drop-region-cross-program t))

;; Emacs 29 - context menu
(when (>= emacs-major-version 29)
    (add-hook 'after-init-hook #'context-menu-mode))

;; Maximum height for resizing mini-windows (the minibuffer and the echo area).
(setq max-mini-window-height 1)

;; Filling - I like it!
(setq-default fill-column 80)  ;; Wrap lines at 80 characters
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; display line indication on fringe
(setq visual-line-fringe-indicators
    '(left-curly-arrow right-curly-arrow))

;; these ones just make sense
(setq sentence-end-double-space nil)
(setq sentence-end-without-period nil)
(setq colon-double-space nil)
(setq use-hard-newlines nil)
(setq adaptive-fill-mode t)

;; Set the font (M-x `describe-font' to see available fonts)
(defface fixed-pitch-large
    '((t :inherit fixed-pitch))
    "A larger variant of `fixed-pitch' for reading modes.")

(defface variable-pitch-large
    '((t :inherit variable-pitch))
    "A larger variant of `variable-pitch' for reading modes.")

(defun my/setup-fonts ()
    "Set up fonts at startup."
    (setq x-underline-at-descent-line nil)
    (let ((fixed-font "Berkeley Mono")
             (variable-font "Berkeley Mono Variable"))
        ;; Default fonts for all text
        (set-face-attribute 'default nil :family fixed-font :height 148)
        (set-face-attribute 'fixed-pitch nil :family fixed-font :height 148)
        (set-face-attribute 'variable-pitch nil :family variable-font :height 1.0)

        ;; Larger variants for reading modes (e.g. nov, eww)
        (set-face-attribute 'fixed-pitch-large nil :family fixed-font :height 170)
        (set-face-attribute 'variable-pitch-large nil :family variable-font :height 170)

        ;; Current line number
        ;; (set-face-attribute 'line-number-current-line nil :foreground "yellow" :inherit 'line-number)

        ;; Modeline
        ;; (set-face-attribute 'mode-line nil :family fixed-font :weight 'bold)

        ;; Programming
        ;; (set-face-attribute 'font-lock-function-name-face nil :family fixed-font :slant 'italic)
        ;; (set-face-attribute 'font-lock-variable-name-face nil :family variable-font :weight 'bold)
        (set-face-attribute 'font-lock-comment-face nil :family fixed-font :slant 'italic)))

;; cursor
(setq-default cursor-type t)
(if (fboundp 'blink-cursor-mode)
    (blink-cursor-mode -1)) ; annoying

;; Don't Render Cursors in Non-Focused Windows
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Use variable-pitch fonts
(add-hook 'text-mode-hook 'variable-pitch-mode)

;; Set default frame title
(setq-default frame-title-format
    '(:eval
         (format "%s@%s: %s"
             (or (file-remote-p default-directory 'user)
                 user-real-login-name)
             (or (file-remote-p default-directory 'host)
                 system-name)
             (cond
                 (buffer-file-truename
                     (concat buffer-file-truename))
                 (dired-directory
                     (concat dired-directory))
                 (t (buffer-name))))))
(setq-default icon-title-format frame-title-format)

;; windmove : directional window-selection routines
(use-package windmove
    :ensure nil ; emacs built-in
    :hook (after-init . windmove-default-keybindings)
    :custom (windmove-wrap-around t))

;; winner-mode : "undo" and "redo" changes in window configurations
(use-package winner
    :disabled t ; does not understand tabs!
    :ensure nil ; emacs built-in
    :hook (after-init . winner-mode))

;; scrolling
(progn
    ;; Enables faster scrolling through unfontified regions. This may result in
    ;; brief periods of inaccurate syntax highlighting immediately after scrolling,
    ;; which should quickly self-correct.
    (setq fast-but-imprecise-scrolling t)

    ;; Skip fontification when input is pending (pairs with fast-but-imprecise-scrolling).
    (setq redisplay-skip-fontification-on-input t)

    ;; Move point to top/bottom of buffer before signaling a scrolling error.
    (setq scroll-error-top-bottom t)

    ;; Emacs spends excessive time recentering the screen when the cursor moves more
    ;; than N lines past the window edges (where N is the value of
    ;; `scroll-conservatively`). This can be particularly slow in larger files
    ;; during extensive scrolling. If `scroll-conservatively` is set above 100, the
    ;; window is never automatically recentered. The default value of 0 triggers
    ;; recentering too aggressively. Setting it to 10 reduces excessive recentering
    ;; and only recenters the window when scrolling significantly off-screen.
    (setq scroll-conservatively 10)

    ;; Try to keep screen position when PgDn/PgUp.
    (setq scroll-preserve-screen-position t)

    ;; Start scrolling when marker at top/bottom.
    (setq scroll-margin 1)

    ;; Horizontal scrolling
    (setq hscroll-margin 2
        hscroll-step 1)

    ;; Mouse scroll moves 1 line at a time, instead of 5 lines.
    (setq mouse-wheel-scroll-amount '(1))

    ;; On a long mouse scroll keep scrolling by 1 line.
    (setq mouse-wheel-progressive-speed nil)

    ;; Speedup cursor movement.
    ;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
    (setq auto-window-vscroll nil)

    ;; smooth scrolling
    (use-package pixel-scroll
        :ensure nil ;; emacs built-in
        :preface
        (defvar my/default-scroll-lines 25) ;; scroll less than default
        (defun my/pixel-scroll-up-command (&optional lines)
            "Similar to `scroll-up-command' but with pixel scrolling.
Optional LINES overrides the default scroll distance."
            (interactive "^P")
            (let ((n (or lines my/default-scroll-lines)))
                (pixel-scroll-precision-interpolate (- (* n (line-pixel-height))))))
        (defun my/pixel-scroll-down-command (&optional lines)
            "Similar to `scroll-down-command' but with pixel scrolling.
Optional LINES overrides the default scroll distance."
            (interactive "^P")
            (let ((n (or lines my/default-scroll-lines)))
                (pixel-scroll-precision-interpolate (* n (line-pixel-height)))))
        (defun my/pixel-recenter-top-bottom (&optional arg)
            "Similar to `recenter-top-bottom' but with pixel scrolling."
            (interactive "^P")
            (let* ((current-row (cdr (nth 6 (posn-at-point))))
                      (target-row (save-window-excursion
                                      (recenter-top-bottom)
                                      (cdr (nth 6 (posn-at-point)))))
                      (distance-in-pixels (* (- target-row current-row) (line-pixel-height))))
                (pixel-scroll-precision-interpolate distance-in-pixels)))
        :bind (([remap scroll-up-command]   . my/pixel-scroll-up-command)
                  ([remap scroll-down-command] . my/pixel-scroll-down-command)
                  ([remap recenter-top-bottom] . my/pixel-recenter-top-bottom))
        :custom ((pixel-scroll-precision-interpolation-factor 1.0)
                    (pixel-scroll-precision-use-momentum nil)
                    (pixel-scroll-precision-interpolate-mice t)
                    (pixel-scroll-precision-large-scroll-height 10.0)
                    (pixel-scroll-precision-interpolation-total-time 0.1)
                    (pixel-scroll-precision-interpolate-page t))
        :hook (after-init . pixel-scroll-precision-mode)))

;; display line numbers in the left margin of the window.
(use-package display-line-numbers
    :ensure nil ;; emacs built-in
    :hook ((prog-mode . display-line-numbers-mode)
              (conf-mode . display-line-numbers-mode))
    :config
    (setq display-line-numbers-type t)
    (setq display-line-numbers-width 3)
    (setq display-line-numbers-widen t))

;; whitespace : visualize blanks (tabs, spaces, newline, etc)
(use-package whitespace
    :ensure nil ;; emacs built-in
    :hook (after-init . global-whitespace-mode)
    :config
    ;; Don't enable whitespace for
    (setq-default whitespace-global-modes
        '(not shell-mode
             comint-mode
             vterm-mode
             ghostel-mode
             eat-mode
             help-mode
             magit-mode
             magit-diff-mode
             archive-mode
             ibuffer-mode
             dired-mode
             occur-mode
             circe-mode
             nov-mode))

    ;; Define the whitespace style (`C-h v whitespace-style' for more styles)
    (setq-default whitespace-style
        '(face empty tabs newline trailing tab-mark newline-mark))

    ;; Set whitespace actions (`C-h f whitespace-cleanup' for more cleanup actions)
    ;; Not needed, using editor-config instead
    ;; (setq-default whitespace-action
    ;;               '(auto-cleanup))

    ;; Make these characters represent whitespace
    (setq-default whitespace-display-mappings
        '(;; space -> · else .
             ;; (space-mark 32 [183] [46])
             ;; new line -> ¬ else $
             ;; (newline-mark ?\n [172 ?\n] [36 ?\n])
             ;; carriage return (Windows) -> ¶ else #
             ;; (newline-mark ?\r [182] [35])
             ;; tabs -> » else >
             (tab-mark ?\t [187 ?\t] [62 ?\t]))))

;; tab-bar : frame-local tabs with named persistent window configurations
(use-package tab-bar
    :ensure nil ;; emacs built-in
    :hook ((after-init . tab-bar-mode)
              (pre-command . tab-bar-history-mode))
    :custom
    (tab-bar-show t)                           ;; always show tab-bar
    (tab-bar-new-button nil)                   ;; hide button for creating new tabs
    (tab-bar-close-button nil)                 ;; hide tab close / X button
    (tab-bar-history-limit 100)                ;; the number of history elements
    (tab-bar-new-tab-choice "*scratch*")       ;; buffer to show in new tabs
    (tab-bar-tab-hints t)                      ;; show tab numbers
    (tab-bar-auto-width nil)                   ;; do not resize tabs
    (tab-bar-format '(tab-bar-format-tabs      ;; elements to include in bar
                         tab-bar-separator)))

;; vim-tab-bar : A Vim-Inspired Emacs Tab-Bar That Automatically Adapts to Any Theme
(use-package vim-tab-bar
    :commands vim-tab-bar-mode
    :config (setq vim-tab-bar-show-groups t)
    :hook (after-init . vim-tab-bar-mode))

;; modeline
(setq column-number-mode t)
(setq mode-line-compact 'long)
(setq mode-line-right-align-edge 'right-fringe)
(setq find-file-visit-truename t) ; only display real names in the modeline
(setq-default mode-line-buffer-identification
    '(:eval (my/shrunk-path)))

;; theme
(use-package modus-themes
    :ensure nil ;; emacs built-in
    :preface
    (defun my/modus-themes-org-fontify-block-delimiter-lines ()
        "Match `org-fontify-whole-block-delimiter-line' to theme style.
Run this function at the post theme load phase, such as with the
`modus-themes-after-load-theme-hook'."
        (if (eq modus-themes-org-blocks 'gray-background)
            (setq org-fontify-whole-block-delimiter-line t)
            (setq org-fontify-whole-block-delimiter-line nil)))
    :init
    (require-theme 'modus-themes) ; `require-theme' is ONLY for the built-in `modus-themes'
    :hook
    (modus-themes-after-load-theme
        . my/modus-themes-org-fontify-block-delimiter-lines)
    :config
    ;; In all of the following, WEIGHT is a symbol such as `semibold',
    ;; `light', `bold', or anything mentioned in `modus-themes-weights'.
    (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-custom-auto-reload t
        modus-themes-disable-other-themes t

        ;; Options for `modus-themes-prompts' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `italic', `WEIGHT'
        modus-themes-prompts '(italic bold)

        ;; The `modus-themes-completions' is an alist that reads two
        ;; keys: `matches', `selection'.  Each accepts a nil value (or
        ;; empty list) or a list of properties that can include any of
        ;; the following (for WEIGHT read further below):
        ;;
        ;; `matches'   :: `underline', `italic', `WEIGHT'
        ;; `selection' :: `underline', `italic', `WEIGHT'
        modus-themes-completions
        '((matches . (extrabold))
             (selection . (semibold italic text-also)))

        modus-themes-org-blocks 'gray-background ; {nil,'gray-background,'tinted-background}

        ;; The `modus-themes-headings' is an alist: read the manual's
        ;; node about it or its doc string.  Basically, it supports
        ;; per-level configurations for the optional use of
        ;; `variable-pitch' typography, a height value as a multiple of
        ;; the base font size (e.g. 1.5), and a `WEIGHT'.
        modus-themes-headings
        '((1 . (variable-pitch 1.5))
             (2 . (1.3))
             (agenda-date . (1.3))
             (agenda-structure . (variable-pitch light 1.8))
             (t . (1.1)))

        ;; Theme overrides
        modus-themes-common-palette-overrides
        `(;; Make the mode line border-less
             (border-mode-line-active unspecified)
             (border-mode-line-inactive unspecified)

             ;; Make line numbers pop - Only needed for `modus-themes-preset-overrides-faint'
             ;; (fg-line-number-active fg-main) (fg-line-number-inactive "grey50")
             ;; (bg-line-number-active bg-inactive) (bg-line-number-inactive bg-dim)

             ;; Make the theme look less colorful/intense
             ,@modus-themes-preset-overrides-faint)))

;; minions : menu that lists enabled minor-modes
;; In Emacs 31 see `mode-line-collapse-minor-modes'
(use-package minions
    :hook (after-init . minions-mode)
    :config
    (progn
        (with-eval-after-load 'minions
            (push 'flymake-mode minions-prominent-modes)
            (push 'overwrite-mode minions-prominent-modes))))

;; which-key : displays the key bindings following your currently entered incomplete command (a prefix) in a popup.
(use-package which-key
    :ensure nil ;; emacs built-in
    :disabled t ;; using vertico
    :hook (after-init . (lambda ()
                            (which-key-mode)
                            (which-key-setup-side-window-bottom)))
    :config
    (setq which-key-show-early-on-C-h t
        which-key-idle-delay 1e6 ; 11 days - wait for C-h to appear
        which-key-idle-secondary-delay 0.05))

;; hl-todo : Highlight TODO and similar keywords in comments and strings
(use-package hl-todo
    :hook ((prog-mode . hl-todo-mode)
              (conf-mode . hl-todo-mode))
    :config (setq hl-todo-highlight-punctuation ":"))

;; anzu : displays current match and total matches information in the mode-line in various search modes.
(use-package anzu
    :disabled t ; using isearch builtin
    :hook (after-init . global-anzu-mode)
    :custom
    (anzu-mode-lighter "")
    (anzu-deactivate-region t)
    (anzu-search-threshold 1000)
    (anzu-replace-threshold 50)
    (anzu-replace-to-string-separator " => ")
    :config
    (global-set-key [remap query-replace] 'anzu-query-replace)
    (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
    (define-key isearch-mode-map [remap isearch-query-replace] #'anzu-isearch-query-replace)
    (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp))

;; ace-window : GNU Emacs package for selecting a window to switch to
(use-package ace-window
    :disabled t
    :bind (("C-x o" . ace-window))
    :init (custom-set-faces
              '(aw-leading-char-face
                   ((t (:inherit ace-jump-face-foreground :height 4.0)))))
    :custom (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
                aw-char-position 'left
                aw-ignore-current nil
                aw-leading-char-style 'char
                aw-scope 'frame))

;; Load fonts/theme/etc
(if (daemonp)
    (add-hook 'after-make-frame-functions
        (lambda (frame)
            (with-selected-frame frame
                (my/setup-fonts)
                (modus-themes-load-theme 'modus-operandi))))
    (add-hook 'after-init-hook  (lambda ()
                                    (my/setup-fonts)
                                    (modus-themes-load-theme 'modus-operandi))))

;; Windows: https://www.reddit.com/r/emacs/comments/179t67l/window_management_share_your_displaybufferalist/
;;          https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(progn
    ;; window split and resizing
    (setq window-combination-resize t) ; Proportional Window Resizing
    (setq split-height-threshold 80)   ; Minimum height for splitting windows sensibly
    (setq split-width-threshold 150)   ; Minimum width for splitting windows sensibly.

    ;; defaults
    (setq switch-to-buffer-in-dedicated-window 'pop)
    (setq switch-to-buffer-obey-display-actions t)

    ;; do not show native-compilation warning/log
    ;; (add-to-list 'display-buffer-alist
    ;;              '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
    ;;                (display-buffer-no-window)
    ;;                (allow-no-window . t)))

    ;; do not show async shell command window
    (add-to-list 'display-buffer-alist
        '("\\*Async Shell Command\\*"
             display-buffer-no-window
             (allow-no-window . t)))

    ;; same window for proced
    (add-to-list 'display-buffer-alist
        '("\\*Proced\\*"
             display-buffer-same-window))

    ;; several buffers that should not pop new windows
    (add-to-list 'display-buffer-alist
        '("\\*\\(Backtrace\\|Messages\\|[Cc]ompilation\\)\\*"
             display-buffer-reuse-window))

    ;; hide compilation buffer
    ;; (add-to-list 'display-buffer-alist
    ;;              '((major-mode . compilation-mode)
    ;;                (display-buffer-no-window)
    ;;                (allow-no-window . t)))

    ;; Info/Help windows
    (add-to-list 'display-buffer-alist
        '((or (major-mode . Info-mode)
              (major-mode . help-mode)
              (major-mode . helpful-mode)
              (major-mode . Man-mode)
              (major-mode . woman-mode)
              (major-mode . occur-mode)
              (major-mode . rg-mode))
             (display-buffer-reuse-mode-window
                 display-buffer-pop-up-window))))

(provide 'my-init-ui)
;;; my-init-ui.el ends here

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

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Resizing the Emacs frame can be costly when changing the font. Disable this
;; to improve startup times with fonts larger than the system default.
(setq frame-resize-pixelwise t)

;; However, do not resize windows pixelwise, as this can cause crashes in some
;; cases when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)
(setq resize-mini-windows 'grow-only)

(blink-cursor-mode -1) ; annoying!
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ; Disable scrollbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))     ; Disable toolbar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))     ; Disable menubar

;; Disable GUIs because they are inconsistent across systems, desktop
;; environments, and themes, and they don't match the look of Emacs.
(setq use-file-dialog nil)
(setq use-dialog-box nil)

;; Make URLs in comments/strings clickable, (Emacs > v22).
;; (add-hook 'find-file-hooks 'goto-address-prog-mode)

;; Switching the focus to the help window when it's opened.
(setq help-window-select t)

;; default amount of padding to use when calling `aligh-regexp'
(setq align-default-spacing 0)

;; only display real names in the modeline
(setq find-file-visit-truename t)

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

;; setup visual-line and auto-fill
(setq visual-line-fringe-indicators
      '(left-curly-arrow right-curly-arrow))              ;; display line indication on fringe
(setq-default fill-column 80)                             ;; Wrap lines at 80 characters
(remove-hook 'text-mode-hook 'turn-on-auto-fill)          ;; auto-fill insert hard line breaks
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)      ;; ... visual-line-mode is much better
;; (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode) ;; ... fill column indication on prog-mode is nice

;; Set the font (M-x `menu-set-font' to see font faces)
(defun my/setup-fonts ()
    "Set up fonts at startup."
    (setq x-underline-at-descent-line nil)
    (set-face-attribute 'default nil
                        :family "Berkeley Mono"
                        :height 140
                        :width  'normal
                        :weight 'normal)
    (set-face-attribute 'fixed-pitch nil
                        :family "Berkeley Mono")
    (set-face-attribute 'variable-pitch nil
                        :family "Berkeley Mono Variable"))
(if (daemonp)
        (add-hook 'after-make-frame-functions
                  (lambda (frame)
                      (with-selected-frame frame
                          (my/setup-fonts))))
    (add-hook 'after-init-hook #'my/setup-fonts))

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

;; unique buffer names dependent on file name
(use-package uniquify
    :ensure nil ;; emacs built-in
    :custom
    (uniquify-buffer-name-style 'reverse)
    (uniquify-separator "•")
    (uniquify-after-kill-buffer-p t)
    (uniquify-ignore-buffers-re "^\\*"))

;; winner-mode : "undo" and "redo" changes in window configurations
(use-package winner
    :ensure nil ;; emacs built-in
    :config
    (winner-mode 1))

;; scrolling
(progn
    ;; Enables faster scrolling through unfontified regions. This may result in
    ;; brief periods of inaccurate syntax highlighting immediately after scrolling,
    ;; which should quickly self-correct.
    (setq fast-but-imprecise-scrolling t)

    ;; Move point to top/bottom of buffer before signaling a scrolling error.
    (setq scroll-error-top-bottom t)

    ;; The number of lines to try scrolling a window by when point moves out.
    (setq scroll-step 1)

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
    (setq scroll-margin 0)

    ;; Horizontal scrolling
    (setq hscroll-margin 2
          hscroll-step 1)

    ;; Mouse scroll moves 1 line at a time, instead of 5 lines.
    (setq mouse-wheel-scroll-amount '(1))

    ;; On a long mouse scroll keep scrolling by 1 line.
    (setq mouse-wheel-progressive-speed nil)

    ;; Speedup cursor movement.
    ;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
    (setq auto-window-vscroll nil))
(use-package pixel-scroll
    :ensure nil ;; emacs built-in
    :if (fboundp 'pixel-scroll-precision-mode)
    :preface
    (defvar my/default-scroll-lines 25) ;; scroll less than default
    (defun my/pixel-scroll-up-command ()
        "Similar to `scroll-up-command' but with pixel scrolling."
        (interactive)
        (pixel-scroll-precision-interpolate (- (* my/default-scroll-lines (line-pixel-height)))))
    (defun my/pixel-scroll-down-command ()
        "Similar to `scroll-down-command' but with pixel scrolling."
        (interactive)
        (pixel-scroll-precision-interpolate (* my/default-scroll-lines (line-pixel-height))))
    (defun my/pixel-recenter-top-bottom ()
        "Similar to `recenter-top-bottom' but with pixel scrolling."
        (interactive)
        (let* ((current-row (cdr (nth 6 (posn-at-point))))
               (target-row (save-window-excursion
                               (recenter-top-bottom)
                               (cdr (nth 6 (posn-at-point)))))
               (distance-in-pixels (* (- target-row current-row) (line-pixel-height))))
            (pixel-scroll-precision-interpolate distance-in-pixels)))
    :custom ((pixel-scroll-precision-interpolation-factor 0.75)
             (pixel-scroll-precision-use-momentum nil)
             (pixel-scroll-precision-interpolate-mice t)
             (pixel-scroll-precision-large-scroll-height 10.0)
             (pixel-scroll-precision-interpolation-total-time 0.2)
             (pixel-scroll-precision-interpolate-page t))
    :hook (after-init . pixel-scroll-precision-mode))

;; display line numbers in the left margin of the window.
(use-package display-line-numbers
    :ensure nil ;; emacs built-in
    :hook (prog-mode . display-line-numbers-mode)
    :config
    (setq display-line-numbers-type t)
    (setq display-line-numbers-width 3)
    (setq display-line-numbers-widen t))

;; whitespace : visualize blanks (tabs, spaces, newline, etc)
(use-package whitespace
    :ensure nil ;; emacs built-in
    :init
    (global-whitespace-mode)
    :config
    ;; Don't enable whitespace for
    (setq-default whitespace-global-modes
                  '(not shell-mode
                        vterm-mode
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
    (setq-default whitespace-action
                  '(auto-cleanup))

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
    :config
    (setq tab-bar-show 1                           ;; hide bar if <= 1 tabs open
          tab-bar-new-button nil                   ;; hide button for creating new tabs
          tab-bar-close-button-show nil            ;; hide tab close / X button
          tab-bar-history-limit 100                ;; the number of history elements
          tab-bar-new-tab-choice "*scratch*"       ;; buffer to show in new tabs
          tab-bar-tab-hints t                      ;; show tab numbers
          tab-bar-format '(tab-bar-format-tabs     ;; elements to include in bar
                           tab-bar-separator)))

;; modeline
(setq mode-line-compact 'long)
(setq mode-line-right-align-edge 'right-fringe)
(use-package shrink-path
    :preface
    (defun my/pretty-buffername ()
        (if buffer-file-truename
                (let* ((cur-dir (file-name-directory buffer-file-truename))
                       (two-up-dir (-as-> cur-dir it (or (f-parent it) "") (or (f-parent it) "")))
                       (shrunk (shrink-path-file-mixed two-up-dir cur-dir buffer-file-truename)))
                    (concat (car shrunk)
                            (mapconcat #'identity (butlast (cdr shrunk)) "/")
                            (car (last shrunk))))
            (buffer-name)))
    :init
    (setq-default mode-line-buffer-identification
                  '(:eval (my/pretty-buffername))))

;; theme
(use-package modus-themes
    :preface
    (defun my/modus-themes-org-fontify-block-delimiter-lines ()
        "Match `org-fontify-whole-block-delimiter-line' to theme style.
Run this function at the post theme load phase, such as with the
`modus-themes-after-load-theme-hook'."
        (if (eq modus-themes-org-blocks 'gray-background)
                (setq org-fontify-whole-block-delimiter-line t)
            (setq org-fontify-whole-block-delimiter-line nil)))
    :config
    ;; In all of the following, WEIGHT is a symbol such as `semibold',
    ;; `light', `bold', or anything mentioned in `modus-themes-weights'.
    (setq modus-themes-italic-constructs t
          modus-themes-bold-constructs nil
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
            (t . (1.1))))

    ;; Theme overrides
    (customize-set-variable 'modus-themes-common-palette-overrides
                            `(;; Make the mode-line borderless
                              (bg-mode-line-active bg-inactive)
                              (fg-mode-line-active fg-main)
                              (bg-mode-line-inactive bg-inactive)
                              (fg-mode-line-active fg-dim)
                              (border-mode-line-active unspecified)
                              (border-mode-line-inactive unspecified)))

    (add-hook 'modus-themes-after-load-theme-hook
              #'my/modus-themes-org-fontify-block-delimiter-lines)

    ;; Load theme
    ;; (my/switch-theme 'modus-operandi)
    (modus-themes-load-theme 'modus-operandi))

;; minions : menu that lists enabled minor-modes
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
    :hook (after-init . which-key-mode)
    :custom ((which-key-idle-delay 1)
             (which-key-popup-type 'minibuffer)))

(use-package window
    :ensure nil
    :preface
    (defun my/split-window-below ()
        "Focus to the last created horizontal window."
        (interactive)
        (split-window-below)
        (other-window 1))
    (defun my/split-window-right ()
        "Focus to the last created vertical window."
        (interactive)
        (split-window-right)
        (other-window 1))
    :config
    (global-set-key (kbd "C-x 2") #'my/split-window-below)
    (global-set-key (kbd "C-x 3") #'my/split-window-right))

;; anzu : displays current match and total matches information in the mode-line in various search modes.
(use-package anzu
    :init (global-anzu-mode +1)
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
    :config
    (setq aw-minibuffer-flag t)
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Windows: https://www.reddit.com/r/emacs/comments/179t67l/window_management_share_your_displaybufferalist/
;;          https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(progn
    ;; window split and resizing
    ;; (setq window-combination-resize t)
    ;; (setq split-height-threshold 80)
    ;; (setq split-width-threshold 135)

    ;; defaults
    (setq switch-to-buffer-in-dedicated-window 'pop)
    (setq switch-to-buffer-obey-display-actions t)

    ;; do not show warnings
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
                 '("\\*\\(Backtrace\\|Compile-log\\|Messages\\|Warnings\\|[Cc]ompilation\\)\\*"
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
                       (major-mode . woman-mode))
                   (display-buffer-reuse-mode-window
                    display-buffer-pop-up-window)
                   ;; (inhibit-same-window . t)
                   )))

(provide 'my-init-ui)
;;; my-init-ui.el ends here

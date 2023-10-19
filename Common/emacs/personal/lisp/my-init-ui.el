;; my-init-ui.el -*- lexical-binding: t; -*-

;; Start maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Select and raise the frame, always
(select-frame-set-input-focus (selected-frame))

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(blink-cursor-mode -1) ; annoying!
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ; Disable scrollbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))     ; Disable toolbar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))     ; Disable menubar

;; ???
(set-display-table-slot standard-display-table 'truncation 32)
(set-display-table-slot standard-display-table 'wrap 32)

;; Use Ctrl+arrow keys to move between windows.
(windmove-default-keybindings 'control)

;; activate `mouse-avoidance-mode'
(mouse-avoidance-mode 'cat-and-mouse)

;; by default when a long line is truncated, emacs displays
;; a "$" sign at the border of window, which is ugly, replace "$" with " "
;; see discussion here: URL `https://emacs.stackexchange.com/questions/54817/remove-dollar-sign-at-beginning-of-line'
(add-hook 'prog-mode-hook #'my/display-truncation-and-wrap-indicator-as-whitespace)
(add-hook 'text-mode-hook #'my/display-truncation-and-wrap-indicator-as-whitespace)

;; setup autofill and visual-line
(setq-default fill-column 120)                       ;; Wrap lines at 120 characters
(remove-hook 'text-mode-hook 'turn-on-auto-fill)     ;; auto-fill insert hard line breaks
(add-hook 'text-mode-hook 'turn-on-visual-line-mode) ;; ... visual-line-mode is much better
(add-hook 'prog-mode-hook                            ;; ... but add comment auto-fill in prog-mode
          (lambda ()
              (setq-local comment-auto-fill-only-comments t)
              (auto-fill-mode 1)))

;; Set the font
(add-hook 'emacs-startup-hook
          (lambda ()
              (setq x-underline-at-descent-line nil)
              (setq-default text-scale-remap-header-line t)
              (custom-set-faces
               `(default ((t (:font "Iosevka Comfy 16"))))
               `(fixed-pitch ((t (:inherit (default)))))
               `(fixed-pitch-serif ((t (:inherit (default)))))
               `(variable-pitch ((t (:font "Iosevka Comfy Duo")))))))

;; Use variable-pitch fonts
(add-hook 'text-mode-hook (lambda ()
                              (setq-local line-spacing 0.1)))
;; (add-hook 'org-mode-hook 'variable-pitch-mode)
;; (add-hook 'markdown-mode-hook 'variable-pitch-mode)

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

(setq window-combination-resize t
      ;; unless you have a really wide screen, always prefer
      ;; horizontal split (ale `split-window-below')
      split-width-threshold 200)

;; display line numbers in the left margin of the window.
(use-package display-line-numbers
    :ensure nil ;; emacs built-in
    :init (setq display-line-numbers-type t)
    :config (add-hook 'prog-mode-hook #'display-line-numbers-mode))

;; whitespace : visualize blanks (tabs, spaces, newline, etc)
(use-package whitespace
    :ensure nil ;; emacs built-in
    :init
    (global-whitespace-mode)
    :config
    ;; Don't enable whitespace for
    (setq-default whitespace-global-modes
                  '(not shell-mode
                        help-mode
                        magit-mode
                        magit-diff-mode
                        ibuffer-mode
                        dired-mode
                        occur-mode))

    ;; Define the whitespace style (`C-h v whitespace-style' for more styles)
    (setq-default whitespace-style
                  '(face empty tabs newline trailing tab-mark newline-mark))

    ;; Set whitespace actions (`C-h f whitespace-cleanup' for more cleanup actions)
    (setq-default whitespace-action
                  '(cleanup auto-cleanup))

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

(use-package tab-bar
    :ensure nil ;; emacs built-in
    :init
    (setq tab-bar-show 1
          tab-bar-close-button-show nil
          tab-bar-new-tab-choice "*scratch*"
          tab-bar-tab-hints t
          tab-bar-format '(tab-bar-format-tabs-groups
                           tab-bar-separator))
    (add-hook 'pre-command-hook 'tab-bar-history-mode))

(use-package modus-themes
    :preface
    (defun +my/modus-themes-org-fontify-block-delimiter-lines ()
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
          modus-themes-variable-pitch-ui nil
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
                              (border-mode-line-active bg-inactive)
                              (border-mode-line-inactive bg-main)))

    (add-hook 'modus-themes-after-load-theme-hook
              #'+my/modus-themes-org-fontify-block-delimiter-lines)

    ;; Load theme
    (my/switch-theme 'modus-operandi))

;; minions : menu that lists enabled minor-modes
(use-package minions
    :config
    (progn
        (with-eval-after-load 'minions
            (push 'flymake-mode minions-prominent-modes)
            (push 'overwrite-mode minions-prominent-modes))
        (add-hook 'after-init-hook 'minions-mode)))

(use-package which-key
    :init
    (add-hook 'pre-command-hook 'which-key-mode)
    :config
    (setq which-key-idle-delay 1
          which-key-popup-type 'minibuffer))

(use-package anzu
    :init (global-anzu-mode +1)
    :config
    (custom-set-variables
     '(anzu-mode-lighter "")
     '(anzu-deactivate-region t)
     '(anzu-search-threshold 1000)
     '(anzu-replace-threshold 50)
     '(anzu-replace-to-string-separator " => "))
    (global-set-key [remap query-replace] 'anzu-query-replace)
    (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
    (define-key isearch-mode-map [remap isearch-query-replace] #'anzu-isearch-query-replace)
    (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp))

;; Windows: https://www.reddit.com/r/emacs/comments/179t67l/window_management_share_your_displaybufferalist/
(progn
    (setq switch-to-buffer-obey-display-actions t
          async-shell-command-display-buffer nil
          fit-window-to-buffer-horizontally t
          fit-frame-to-buffer t)
    (setq display-buffer-base-action
          '((display-buffer-reuse-window
             display-buffer-in-previous-window
             display-buffer-reuse-mode-window)))

    (add-to-list 'display-buffer-alist
                 '("\\*\\(e?shell\\|vterm\\|ielm\\|eat\\)\\*"
                   (display-buffer-reuse-window
                    display-buffer-in-direction
                    display-buffer-in-side-window)
                   (body-function . select-window)
                   (window-height . .40)
                   (window-width .  .40)
                   (direction . below)
                   (side . bottom)
                   (slot . 1)))

    (add-to-list 'display-buffer-alist
                 '("\\*\\(Backtrace\\|Compile-log\\|Messages\\|Warnings\\|[Cc]ompilation\\)\\*"
                   (display-buffer-reuse-window
                    display-buffer-in-direction
                    display-buffer-in-side-window)
                   (body-function . select-window)
                   (window-height . .40)
                   (window-width .  .50)
                   (direction . right)
                   (side . right)
                   (slot . 1)))
    )

;; discover : Discover more of emacs using context menus.
(use-package discover
    :disabled t
    :config
    (global-discover-mode 1))

(provide 'my-init-ui)
;;; my-init-ui.el ends here

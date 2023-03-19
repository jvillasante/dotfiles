;;; my-init-ui.el -*- lexical-binding: t; -*-

(straight-use-package 'all-the-icons)
(straight-use-package 'which-key)
(straight-use-package 'modus-themes)
(straight-use-package 'minions)
(straight-use-package 'anzu)

;; Start maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

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

;; by default when a long line is truncated, emacs displays
;; a "$" sign at the border of window, which is ugly, replace "$" with " "
;; see discussion here: URL `https://emacs.stackexchange.com/questions/54817/remove-dollar-sign-at-beginning-of-line'
(add-hook 'prog-mode-hook #'my/display-truncation-and-wrap-indicator-as-whitespace)
(add-hook 'text-mode-hook #'my/display-truncation-and-wrap-indicator-as-whitespace)

;; set default font
(add-to-list 'default-frame-alist
    '(font . "Iosevka-16"))

;; set font for text that should be displayed more like prose.
(set-face-attribute 'variable-pitch nil :family "Iosevka" :height 160)

;; set default frame title
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

;; display line numbers in the left margin of the window.
(use-package display-line-numbers
    :init
    (setq display-line-numbers-type t)
    (global-display-line-numbers-mode)
    :config
    (dolist (mode '(org-mode-hook
                       vterm-mode-hook
                       term-mode-hook
                       shell-mode-hook
                       eshell-mode-hook))
        (add-hook mode (lambda () (display-line-numbers-mode 0)))))

;; whitespace : visualize blanks (tabs, spaces, newline, etc)
(use-package whitespace
    :init
    (add-hook 'before-save-hook #'whitespace-cleanup)
    :config
    (setq delete-trailing-lines t)      ; `M-x delete-trailing-whitespace' deletes trailing lines
    (setq show-trailing-whitespace t)   ; show those whitespace we need to delete
    (setq whitespace-style
        '(face spaces empty tabs newline trailing lines-tail space-mark tab-mark newline-mark))
    (setq whitespace-action '(cleanup auto-cleanup))
    (setq whitespace-global-modes
        '(not shell-mode
             vterm-mode
             eshell-mode
             help-mode
             magit-mode
             magit-diff-mode
             ibuffer-mode
             dired-mode
             occur-mode)))

(defvar my/side-window-slots
    '((helpful . 1)     ;; 1 is the default
         (vterm . -1)
         (eldoc . 1)
         (python . -1)
         (R . -1)
         (Rhelp . 1)
         (Rdired . -1)
         (RWatch . -2)
         (xwidget-plot . -1)
         (dired-sidebar . -1))
    "The slot for different mode if used as side window.
This is for configuring `display-buffer-in-side-window',
configuring this would avoid buffer swallows other buffer's window
if they are side window.")

(defvar my/side-window-sides
    '((helpful . bottom)    ;; bottom is the default
         (vterm . bottom)
         (eldoc . bottom)
         (python . bottom)
         (R . bottom)
         (Rhelp . bottom)
         (Rdired . right)
         (RWatch . right)                  ;
         (xwidget-plot . right)
         (dired-sidebar . left)
         (pdf-outline . left))
    "The side different mode if used as side window.
This is for configuring `display-buffer-in-side-window',
configuring this would avoid buffer swallows other buffer's window
if they are side window.")

(setq window-combination-resize t
    ;; unless you have a really wide screen, always prefer
    ;; horizontal split (ale `split-window-below')
    split-width-threshold 300)

(use-package all-the-icons
    :if (display-graphic-p)
    :commands (all-the-icons-octicon
                  all-the-icons-faicon
                  all-the-icons-fileicon
                  all-the-icons-wicon
                  all-the-icons-material
                  all-the-icons-alltheicon))

(use-package modus-themes
    :demand t
    :config
    (setq modus-themes-italic-constructs t)
    (setq modus-themes-bold-constructs t)
    (setq modus-themes-variable-pitch-ui t)
    (setq modus-themes-mixed-fonts t)

    ;; Color customizations
    (setq modus-themes-prompts '(italic bold))
    (setq modus-themes-completions
        '((matches . (extrabold))
             (selection . (semibold italic text-also))))
    (setq modus-themes-org-blocks 'gray-background)

    ;; Font sizes for titles and headings, including org
    (setq modus-themes-headings '((1 . (variable-pitch 1.5))
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

    ;; Load theme
    (my/switch-theme 'modus-operandi))

;; minions : menu that lists enabled minor-modes
(use-package minions
    :demand
    :config
    (progn
        (with-eval-after-load 'minions
            (push 'flycheck-mode minions-prominent-modes)
            (push 'overwrite-mode minions-prominent-modes))
        (add-hook 'after-init-hook 'minions-mode)))

(use-package tab-bar
    :init
    (setq tab-bar-show 1
        tab-bar-close-button-show nil
        tab-bar-new-tab-choice "*scratch*"
        tab-bar-tab-hints t
        tab-bar-new-button-show nil
        tab-bar-format '(tab-bar-format-tabs-groups
                            tab-bar-separator))
    (add-hook 'pre-command-hook 'tab-bar-history-mode)
    (advice-add #'tab-bar-new-tab :around #'my/set-scratch-directory))

(use-package which-key
    :init
    (add-hook 'pre-command-hook 'which-key-mode)

    :config
    (setq which-key-idle-delay 1
        which-key-popup-type 'minibuffer))

;; anzu : displays current match and total matches information in the mode-line in various search modes
(use-package anzu
    :demand t
    :config
    (global-anzu-mode +1))

(provide 'my-init-ui)
;;; my-init-ui.el ends here

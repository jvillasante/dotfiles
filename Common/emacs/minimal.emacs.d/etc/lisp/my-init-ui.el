;;; my-init-ui.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;;
;;; Code:

;; remove the title bar only when frame is maximized
(add-hook 'window-size-change-functions 'frame-hide-title-bar-when-maximized)

;; misc
(display-time-mode)
(show-paren-mode +1)  ; Paren match highlighting
(winner-mode 1)
(pixel-scroll-precision-mode 1)

;; Window dividers separate windows visually. Window dividers are bars that can
;; be dragged with the mouse, thus allowing you to easily resize adjacent
;; windows.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Window-Dividers.html
(add-hook 'after-init-hook #'window-divider-mode)

;; setup visual-line and auto-fill
(setq visual-line-fringe-indicators
      '(left-curly-arrow right-curly-arrow))              ;; display line indication on fringe
(setq-default fill-column 79)                             ;; Wrap lines at 79 characters
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

(provide 'my-init-ui)
;;; my-init-ui.el ends here

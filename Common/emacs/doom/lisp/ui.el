;;; lisp/ui.el -*- lexical-binding: t; -*-

;; font
(setq
    doom-font-increment 1
    doom-font (font-spec :family "Iosevka" :size 16.0)
    doom-big-font (font-spec :family "Iosevka" :size 18.0)
    doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 16.0)
    doom-unicode-font (font-spec :family "IBM Plex Mono" :size 16.0)
    doom-serif-font (font-spec :family "IBM Plex Serif" :size 14.0))

;; frame title
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

;; Emacs 29 improved scrolling
(pixel-scroll-precision-mode)

;; Dash highlighting
(after! dash
    (global-dash-fontify-mode))

;; Do not modify neotree
(remove-hook 'doom-load-theme-hook #'doom-themes-neotree-config)

;; dashboard
(setq fancy-splash-image +my/splash-path)

;; all-the-icons
(setq all-the-icons-scale-factor 1.1)

;; theme
(remove-hook 'window-setup-hook #'doom-init-theme-h)
(add-hook 'after-init-hook #'doom-init-theme-h 'append)
(delq! t custom-theme-load-path)
(use-package! modus-themes
    :demand t
    :init
    (setq
        modus-themes-mode-line '(borderless (padding 1) (height 0.9))
        modus-themes-bold-constructs nil
        modus-themes-italic-constructs t
        modus-themes-fringes 'subtle
        modus-themes-tabs-accented t
        modus-themes-subtle-line-numbers t
        modus-themes-diffs 'desaturated
        modus-themes-region '(bg-only no-extend)
        modus-themes-headings
        '((1 . (monochrome variable-pitch 1.3))
             (2 . (monochrome variable-pitch 1.2))
             (3 . (monochrome variable-pitch 1.1))
             (t . (monochrome))))
    (modus-themes-load-themes)
    :config
    (+my/switch-theme 'modus-operandi)
    :bind ("<f5>" . modus-themes-toggle))

;; modeline
(after! doom-modeline
    (setq doom-modeline-icon t)
    (setq doom-modeline-height 1)
    (setq doom-modeline-lsp t)
    (custom-set-faces
        '(mode-line ((t (:height 0.9))))
        '(mode-line-active ((t (:height 0.9))))
        '(mode-line-inactive ((t (:height 0.9))))))

(after! anzu
    (global-anzu-mode))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(dolist (mode '(org-mode-hook
                   vterm-mode-hook
                   term-mode-hook
                   shell-mode-hook
                   eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; popup windows & rules
(when (featurep! :ui popup)
    (setq +popup-default-parameters
        '((transient . t)   ; remove later
             (quit . t)        ; remove later
             (select . ignore) ; remove later
             (no-other-window . nil))) ;; Allow `other-window'

    (set-popup-rule! "^\\*doom:scratch*" :size 0.4 :ttl 0 :quit t)
    (set-popup-rule! "^\\*ielm" :size 0.4 :quit t :ttl 0)
    (set-popup-rule! "^\\*compilation" :size 0.6 :quit t :ttl 0)
    (set-popup-rule! "^\\*info*" :size 0.6 :ttl 0 :quit nil)
    (set-popup-rule! "^\\*Man" :size 0.6 :quit t :select t)
    (set-popup-rule! "^\\*xwidget-webkit*" :size 0.8 :quit t :select t)
    (set-popup-rule! "^\\*Capture\\*$\\|CAPTURE-.*$" :size 0.6 :quit nil :select t :autosave nil)
    (set-popup-rule! "^\\*Diff" :size 0.6 :quit t :select t)
    (after! tldr (set-popup-rule! "^\\*tldr*" :size 0.6 :ttl 0 :quit nil))
    (after! lsp (set-popup-rule! "^\\*lsp" :size 0.4 :quit t :ttl 0))
    (after! eglot (set-popup-rule! "^\\*eglot-.*$" :size 0.4 :quit t :ttl 0))
    (after! vterm (set-popup-rule! "^\\*doom:vterm-popup" :size 0.4 :ttl 0 :quit nil))
    (after! pass (set-popup-rule! "^\\*Password-Store*" :side 'left :size 0.35 :quit nil)))

;; https://gitlab.com/protesilaos/dotfiles/-/raw/master/emacs/.emacs.d/prot-lisp/modus-themes-exporter.el
(load! "site-lisp/ui-modus-exporter.el")

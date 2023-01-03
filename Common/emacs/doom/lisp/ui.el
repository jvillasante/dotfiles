;;; lisp/ui.el -*- lexical-binding: t; -*-

;; font
(setq! doom-font-increment 1)
(setq! doom-font (font-spec :family "Iosevka" :size 16.0))
(setq! doom-big-font (font-spec :family "Iosevka" :size 18.0))
(setq! doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 16.0))
(setq! doom-unicode-font (font-spec :family "IBM Plex Mono" :size 16.0))
(setq! doom-serif-font (font-spec :family "IBM Plex Serif" :size 14.0))

;; frame title
(setq! frame-title-format
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
(when (> emacs-major-version 28)
    (pixel-scroll-precision-mode))

;; Dash highlighting
(after! dash
    (global-dash-fontify-mode))

;; Do not modify neotree
(remove-hook 'doom-load-theme-hook #'doom-themes-neotree-config)

;; doom-dashboard
(setq! fancy-splash-image +my/splash-path)
(setq! +doom-dashboard-functions
    '(doom-dashboard-widget-banner
         doom-dashboard-widget-shortmenu
         doom-dashboard-widget-loaded))
(setq! +doom-dashboard-menu-sections
    '(("Reload last session"
          :icon nil
          :when (cond ((modulep! :ui workspaces)
                          (file-exists-p (expand-file-name persp-auto-save-fname persp-save-dir)))
                    ((require 'desktop nil t)
                        (file-exists-p (desktop-full-file-name))))
          :face (:inherit (doom-dashboard-menu-title bold))
          :action doom/quickload-session)
         ("Open org-agenda"
             :icon nil
             :when (fboundp 'org-agenda)
             :action org-agenda)
         ("Recently opened files"
             :icon nil
             :action recentf-open-files)
         ("Open project"
             :icon nil
             :action projectile-switch-project)
         ("Jump to bookmark"
             :icon nil
             :action bookmark-jump)
         ("Open private configuration"
             :icon nil
             :when (file-directory-p doom-user-dir)
             :action doom/open-private-config)
         ("Open documentation"
             :icon nil
             :action doom/help)))

;; all-the-icons
(setq! all-the-icons-scale-factor 1.1)

;; theme
(remove-hook 'window-setup-hook #'doom-init-theme-h)
(add-hook 'after-init-hook #'doom-init-theme-h 'append)
(delq! t custom-theme-load-path)
(use-package! modus-themes
    :demand t
    :init
    (setq! modus-themes-italic-constructs t)
    (setq! modus-themes-bold-constructs t)
    (setq! modus-themes-variable-pitch-ui t)
    (setq! modus-themes-mixed-fonts t)

    ;; Color customizations
    (setq! modus-themes-prompts '(italic bold))
    (setq! modus-themes-completions
        '((matches . (extrabold))
             (selection . (semibold italic text-also))))
    (setq! modus-themes-org-blocks 'gray-background)

    ;; Font sizes for titles and headings, including org
    (setq! modus-themes-headings '((1 . (variable-pitch 1.5))
                                      (2 . (1.3))
                                      (agenda-date . (1.3))
                                      (agenda-structure . (variable-pitch light 1.8))
                                      (t . (1.1))))

    ;; Theme overrides
    (customize-set-variable 'modus-themes-common-palette-overrides
        `(
             ;; Make the mode-line borderless
             (bg-mode-line-active bg-inactive)
             (fg-mode-line-active fg-main)
             (bg-mode-line-inactive bg-inactive)
             (fg-mode-line-active fg-dim)
             (border-mode-line-active bg-inactive)
             (border-mode-line-inactive bg-main)))
    :config
    (+my/switch-theme 'modus-operandi))

;; modeline
(after! doom-modeline
    (setq! doom-modeline-icon nil)
    (setq! doom-modeline-major-mode-icon nil)
    (setq! doom-modeline-major-mode-color-icon nil)
    (setq! doom-modeline-buffer-state-icon nil)
    (setq! doom-modeline-buffer-modification-icon nil)
    (setq! doom-modeline-persp-icon nil)
    (setq! doom-modeline-time-icon nil)
    (setq! doom-modeline-modal-icon t)
    (setq! doom-modeline-modal nil)
    (setq! doom-modeline-height 1)
    (setq! doom-modeline-lsp t))

(after! anzu
    (global-anzu-mode))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq! display-line-numbers-type t)
(dolist (mode '(org-mode-hook
                   vterm-mode-hook
                   term-mode-hook
                   shell-mode-hook
                   eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; popup windows & rules
(when (modulep! :ui popup)
    (setq! +popup-default-parameters
        '((transient . t)   ; remove later
             (quit . t)        ; remove later
             (select . ignore) ; remove later
             (no-other-window . nil))) ;; Allow `other-window'

    (set-popup-rule! "^\\*doom:scratch*" :size 0.4 :ttl 0 :quit t)
    (set-popup-rule! "^\\*ielm" :size 0.4 :quit t :ttl 0)
    (set-popup-rule! "^\\*compilation" :size 0.8 :quit t :ttl 0)
    (set-popup-rule! "^\\*info*" :size 0.8 :ttl 0 :quit nil)
    (set-popup-rule! "^\\*Man" :size 0.8 :quit t :select t)
    (set-popup-rule! "^\\*xwidget-webkit*" :size 0.8 :quit t :select t)
    (set-popup-rule! "^\\*Capture\\*$\\|CAPTURE-.*$" :size 0.4 :quit nil :select t :autosave nil)
    (set-popup-rule! "^\\*Diff" :size 0.6 :quit t :select t)
    (set-popup-rule! "^\\*Shell Command" :size 0.4 :quit t :select t)
    (set-popup-rule! "^\\*Async Shell Command" :size 0.4 :quit t :select t)
    (after! tldr (set-popup-rule! "^\\*tldr*" :size 0.6 :ttl 0 :quit nil))
    (after! lsp (set-popup-rule! "^\\*lsp" :size 0.4 :quit t :ttl 0))
    (after! eglot (set-popup-rule! "^\\*eglot-.*$" :size 0.4 :quit t :ttl 0))
    (after! flymake (set-popup-rule! "^\\*Flymake" :size 0.4 :quit t :ttl 0))
    (after! rustic (set-popup-rule! "^\\*rustic-" :size 0.4 :quit t :ttl 0))
    (after! rustic (set-popup-rule! "^\\*cargo-" :size 0.4 :quit t :ttl 0))
    (after! vterm (set-popup-rule! "^\\*doom:vterm-popup" :size 0.4 :ttl 0 :quit nil))
    (after! vterm (set-popup-rule! "^\\*doom:eshell-popup" :size 0.4 :ttl 0 :quit nil)))

;; https://gitlab.com/protesilaos/dotfiles/-/raw/master/emacs/.emacs.d/prot-lisp/modus-themes-exporter.el
(load! "site-lisp/ui-modus-exporter.el")

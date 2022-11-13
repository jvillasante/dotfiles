;;; lisp/ui.el -*- lexical-binding: t; -*-

;; font
(csetq doom-font-increment 1)
(csetq doom-font (font-spec :family "Iosevka" :size 16.0))
(csetq doom-big-font (font-spec :family "Iosevka" :size 18.0))
(csetq doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 16.0))
(csetq doom-unicode-font (font-spec :family "IBM Plex Mono" :size 16.0))
(csetq doom-serif-font (font-spec :family "IBM Plex Serif" :size 14.0))

;; frame title
(csetq frame-title-format
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
(csetq fancy-splash-image +my/splash-path)
(csetq +doom-dashboard-functions
    '(doom-dashboard-widget-banner
         doom-dashboard-widget-shortmenu
         doom-dashboard-widget-loaded))
(csetq +doom-dashboard-menu-sections
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
(csetq all-the-icons-scale-factor 1.1)

;; theme
(remove-hook 'window-setup-hook #'doom-init-theme-h)
(add-hook 'after-init-hook #'doom-init-theme-h 'append)
(delq! t custom-theme-load-path)
(use-package! modus-themes
    :demand t
    :init
    (csetq modus-themes-variable-pitch-ui t)
    (csetq modus-themes-mode-line '(borderless (padding 1) (height 0.9)))
    (csetq modus-themes-bold-constructs t)
    (csetq modus-themes-italic-constructs nil)
    (csetq modus-themes-paren-match '(bold intense))
    (csetq modus-themes-fringes 'subtle)
    (csetq modus-themes-tabs-accented t)
    (csetq modus-themes-subtle-line-numbers t)
    (csetq modus-themes-diffs 'desaturated)
    (csetq modus-themes-region '(bg-only no-extend))
    (csetq modus-themes-mixed-fonts t)
    (csetq modus-themes-completions '((matches . (extrabold background intense))
                                         (selection . (extrabold accented intense))
                                         (popup . (accented))))
    (csetq modus-themes-headings
        '((1 . (monochrome variable-pitch 1.3))
             (2 . (monochrome variable-pitch 1.2))
             (3 . (monochrome variable-pitch 1.1))
             (t . (monochrome))))
    (modus-themes-load-themes)
    :config
    (+my/switch-theme 'modus-operandi))

;; modeline
(after! doom-modeline
    (csetq doom-modeline-icon nil)
    (csetq doom-modeline-major-mode-icon nil)
    (csetq doom-modeline-major-mode-color-icon nil)
    (csetq doom-modeline-buffer-state-icon nil)
    (csetq doom-modeline-buffer-modification-icon nil)
    (csetq doom-modeline-persp-icon nil)
    (csetq doom-modeline-time-icon nil)
    (csetq doom-modeline-modal-icon t)
    (csetq doom-modeline-modal nil)
    (csetq doom-modeline-height 1)
    (csetq doom-modeline-lsp t))

(after! anzu
    (global-anzu-mode))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(csetq display-line-numbers-type t)
(dolist (mode '(org-mode-hook
                   vterm-mode-hook
                   term-mode-hook
                   shell-mode-hook
                   eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; popup windows & rules
(when (modulep! :ui popup)
    (csetq +popup-default-parameters
        '((transient . t)   ; remove later
             (quit . t)        ; remove later
             (select . ignore) ; remove later
             (no-other-window . nil))) ;; Allow `other-window'

    (set-popup-rule! "^\\*doom:scratch*" :size 0.4 :ttl 0 :quit t)
    (set-popup-rule! "^\\*ielm" :size 0.4 :quit t :ttl 0)
    (set-popup-rule! "^\\*compilation" :size 0.4 :quit t :ttl 0)
    (set-popup-rule! "^\\*info*" :size 0.6 :ttl 0 :quit nil)
    (set-popup-rule! "^\\*Man" :size 0.6 :quit t :select t)
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
    (after! pass (set-popup-rule! "^\\*Password-Store*" :side 'left :size 0.8 :quit nil)))

;; https://gitlab.com/protesilaos/dotfiles/-/raw/master/emacs/.emacs.d/prot-lisp/modus-themes-exporter.el
(load! "site-lisp/ui-modus-exporter.el")

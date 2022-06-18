;;; +ui.el -*- lexical-binding: t; -*-

;; font
(setq
    doom-font-increment 1
    doom-font (font-spec :family "Iosevka" :size 16.0)
    doom-big-font (font-spec :family "Iosevka" :size 18.0)
    doom-variable-pitch-font (font-spec :family "IBM Plex Sans Condensed" :size 16.0)
    doom-unicode-font (font-spec :family "IBM Plex Mono" :size 16.0)
    doom-serif-font (font-spec :family "IBM Plex Serif" :size 14.0))

;; Dash highlighting
(after! dash
    (global-dash-fontify-mode))

;; Do not modify neotree
(remove-hook 'doom-load-theme-hook #'doom-themes-neotree-config)

;; dashboard
(setq fancy-splash-image +my/splash-path)

;; theme
(remove-hook 'window-setup-hook #'doom-init-theme-h)
(add-hook 'after-init-hook #'doom-init-theme-h 'append)
(delq! t custom-theme-load-path)
(+my/switch-theme 'modus-operandi)

;; modeline
(use-package! mini-modeline
    :init
    (setq mode-line-position (list "%l:%c %p"))
    (setq mode-line-modes (list "%m"))
    (setq mini-modeline-enhance-visual t)
    (setq mini-modeline-display-gui-line nil)
    (setq mini-modeline-echo-duration 10)
    (setq mini-modeline-right-padding 1)
    (setq mini-modeline-l-format nil)
    (setq mini-modeline-r-format
        '("%e"
             mode-line-front-space
             mode-line-position
             " "
             mode-line-mule-info     ; Information on character sets, encodings, and other human-language details
             mode-line-client        ; Identifies frames created by emacsclient
             mode-line-modified      ; Modified and read-only status
             mode-line-remote        ; At-sign (@) for buffers visiting remote files, otherwise a dash
             " "
             mode-line-modes))
    :config
    (if (daemonp)
        (add-hook 'after-make-frame-functions
            (lambda (frame)
                (with-selected-frame frame
                    (mini-modeline-mode t))))
        (mini-modeline-mode t)))

;; no icons
(after! all-the-icons
    (defun +my/disable-all-the-icons (&rest _)
        nil)
    (dolist (fn '(all-the-icons-octicon
                     all-the-icons-material
                     all-the-icons-faicon
                     all-the-icons-fileicon
                     all-the-icons-wicon
                     all-the-icons-alltheicon))
        (advice-add fn :override #'+my/disable-all-the-icons)))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(dolist (mode '(org-mode-hook
                   vterm-mode-hook
                   term-mode-hook
                   shell-mode-hook
                   eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; popup rules
(set-popup-rule! "^\\*doom:scratch*" :size 0.4 :ttl 0 :quit t)
(set-popup-rule! "^\\*ielm" :size 0.4 :quit t :ttl 0)
(set-popup-rule! "^\\*compilation" :size 0.6 :quit t :ttl 0)
(set-popup-rule! "^\\*info*" :size 0.6 :ttl 0 :quit nil)
(set-popup-rule! "^\\*Man" :size 0.6 :quit t :select t)
(set-popup-rule! "^\\*xwidget-webkit*" :size 0.8 :quit t :select t)
(set-popup-rule! "^\\*Capture\\*$\\|CAPTURE-.*$" :size 0.6 :quit nil :select t :autosave nil)
(after! tldr (set-popup-rule! "^\\*tldr*" :size 0.6 :ttl 0 :quit nil))
(after! lsp (set-popup-rule! "^\\*lsp" :size 0.4 :quit t :ttl 0))
(after! eglot (set-popup-rule! "^\\*eglot-.*$" :size 0.4 :quit t :ttl 0))
(after! vterm (set-popup-rule! "^\\*doom:vterm-popup" :size 0.4 :ttl 0 :quit nil))
(after! pass (set-popup-rule! "^\\*Password-Store*" :side 'left :size 0.35 :quit nil))

;; https://gitlab.com/protesilaos/dotfiles/-/raw/master/emacs/.emacs.d/prot-lisp/modus-themes-exporter.el
(load! "+ui-modus-exporter.el")
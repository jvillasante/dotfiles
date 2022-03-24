;;; +ui.el -*- lexical-binding: t; -*-

;; font
(setq
    ;; doom-font (font-spec :family "JetBrains Mono" :size 20)
    ;; doom-big-font (font-spec :family "JetBrains Mono" :size 40)
    doom-font (font-spec :family "Source Code Pro" :size 20)
    doom-big-font (font-spec :family "Source Code Pro" :size 40)
    doom-variable-pitch-font (font-spec :family "Overpass" :size 24)
    doom-unicode-font (font-spec :family "JuliaMono")
    doom-serif-font (font-spec :family "IBM Plex Mono" :weight 'light))

;; Dash highlighting
(after! dash (dash-enable-font-lock))

;; Do not modify neotree
(remove-hook 'doom-load-theme-hook #'doom-themes-neotree-config)

;; dashboard
(setq fancy-splash-image +my/splash-path)

;; theme
(setq doom-theme 'modus-vivendi)
;; (setq doom-theme 'modus-operandi)
(remove-hook 'window-setup-hook #'doom-init-theme-h)
(add-hook 'after-init-hook #'doom-init-theme-h 'append)
(delq! t custom-theme-load-path)

;; modeline
(use-package! mini-modeline
    :config
    (setq mode-line-position (list "%l:%c %p"))
    (setq mode-line-modes (list "%m"))
    (setq mini-modeline-enhance-visual t)
    (setq mini-modeline-display-gui-line nil)
    (setq mini-modeline-l-format nil)
    (setq mini-modeline-r-format
        '("%e"
             mode-line-front-space
             mode-line-position
             " "
             ;; mode-line-client        ; Identifies frames created by emacsclient
             mode-line-mule-info	 ; Information on character sets, encodings, and other human-language details
             mode-line-modified      ; Modified and read-only status
             mode-line-remote        ; At-sign (@) for buffers visiting remote files, otherwise a dash
             " "
             mode-line-modes))

    (if (daemonp)
        (add-hook 'after-make-frame-functions
            (lambda (frame)
                (with-selected-frame frame
                    (if (window-system frame)
                        (mini-modeline-mode 1)))))
        (mini-modeline-mode 1)))

;; no icons
(after! all-the-icons
    (defun +jv/disable-all-the-icons (&rest _)
        nil)
    (dolist (fn '(all-the-icons-octicon
                     all-the-icons-material
                     all-the-icons-faicon
                     all-the-icons-fileicon
                     all-the-icons-wicon
                     all-the-icons-alltheicon))
        (advice-add fn :override #'+jv/disable-all-the-icons)))

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
(set-popup-rule! "^\\*doom:vterm-popup" :size 0.4 :ttl 0 :quit nil)
(set-popup-rule! "^\\*lsp" :size 0.4 :quit t :ttl 0)
(set-popup-rule! "^\\*ielm" :size 0.4 :quit t :ttl 0)
(set-popup-rule! "^\\*ivy-occur" :size 0.4 :ttl 0 :quit nil)
(set-popup-rule! "^\\*Embark Export" :size 0.4 :ttl 0 :quit nil)
(set-popup-rule! "^\\*Racket REPL*" :size 0.4 :ttl 0 :quit nil)
(set-popup-rule! "^\\*tldr*" :size 0.6 :ttl 0 :quit nil)
(set-popup-rule! "^\\*compilation" :size 0.6 :quit t :ttl 0)
(set-popup-rule! "^\\*cargo-*" :size 0.6 :ttl 0 :quit t)
(set-popup-rule! "^\\*rustic-*" :size 0.6 :ttl 0 :quit t)
(set-popup-rule! "^\\*osx-dictionary" :size 0.6 :quit t :ttl 0)
(set-popup-rule! "^\\*Password-Store" :side 'left :size 0.65 :quit nil)
(set-popup-rule! "^\\*Capture\\*$\\|CAPTURE-.*$" :size 0.6 :quit nil :select t :autosave nil)
(set-popup-rule! "^\\*info*" :size 0.6 :ttl 0 :quit nil)
(set-popup-rule! "^\\*test reference" :size 0.6 :ttl 0 :quit t)

;; https://gitlab.com/protesilaos/dotfiles/-/raw/master/emacs/.emacs.d/prot-lisp/modus-themes-exporter.el
(load! "+ui-modus-exporter.el")

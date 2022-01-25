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

;; theme
;; (setq doom-theme 'modus-vivendi)
(setq doom-theme 'modus-operandi)

(after! doom-themes
    (setq
        doom-themes-enable-bold t     ; if nil, bold is universally disabled
        doom-themes-enable-italic t)  ; if nil, italics is universally disabled

    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)

    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config))

;; modeline
(setq mode-line-format nil)
(setq-default mode-line-format nil)
(use-package! awesome-tray
    :init
    (awesome-tray-mode 1)
    :config
    (setq awesome-tray-active-modules '("mode-name" "location")))

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
(set-popup-rule! "^\\*Capture\\*$\\|CAPTURE-.*$" :size 0.4 :quit nil :select t :autosave nil)
(set-popup-rule! "^\\*doom:scratch*" :size 0.4 :ttl 0 :quit nil)
(set-popup-rule! "^\\*doom:vterm-popup" :size 0.4 :ttl 0 :quit nil)
(set-popup-rule! "^\\*lsp" :size 0.4 :quit t :ttl 0)
(set-popup-rule! "^\\*ielm" :size 0.4 :quit t :ttl 0)
(set-popup-rule! "^\\*Password-Store" :side 'left :size 0.4 :quit nil)
(set-popup-rule! "^\\*ivy-occur" :size 0.4 :ttl 0 :quit nil)
(set-popup-rule! "^\\*Embark Export" :size 0.4 :ttl 0 :quit nil)
(set-popup-rule! "^\\*Racket REPL*" :size 0.4 :ttl 0 :quit nil)
(set-popup-rule! "^\\*tldr*" :size 0.6 :ttl 0 :quit nil)
(set-popup-rule! "^\\*compilation" :size 0.6 :quit t :ttl 0)
(set-popup-rule! "^\\*cargo-*" :size 0.6 :ttl 0 :quit nil)
(set-popup-rule! "^\\*rustic-*" :size 0.6 :ttl 0 :quit nil)
(set-popup-rule! "^\\*osx-dictionary" :size 0.6 :quit t :ttl 0)
(set-popup-rule! "^\\*Password-Store" :side 'left :size 0.60 :quit nil)

;; https://gitlab.com/protesilaos/dotfiles/-/raw/master/emacs/.emacs.d/prot-lisp/modus-themes-exporter.el
(load! "+ui-modus-exporter.el")

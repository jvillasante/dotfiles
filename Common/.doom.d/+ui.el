;;; +ui.el -*- lexical-binding: t; -*-

;; font
(setq
    doom-font (font-spec :family "JetBrains Mono" :size 20)
    doom-big-font (font-spec :family "JetBrains Mono" :size 40)
    doom-variable-pitch-font (font-spec :family "Overpass" :size 24)
    doom-unicode-font (font-spec :family "JuliaMono")
    doom-serif-font (font-spec :family "IBM Plex Mono" :weight 'light))

;; Dash highlighting
(after! dash (dash-enable-font-lock))

;; solaire-mode with modus themes
;; (use-package! solaire-mode
;;     :functions persp-load-state-from-file
;;     :hook (((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
;;               (minibuffer-setup . solaire-mode-in-minibuffer))
;;     :init
;;     (solaire-global-mode 1)
;;     (advice-add #'persp-load-state-from-file :after #'solaire-mode-restore-persp-mode-buffers)
;;     :config
;;     (setq solaire-mode-remap-modeline nil)
;;     (setq solaire-mode-auto-swap-bg t)
;;     (setq solaire-mode-remap-line-numbers t)
;;     (setq solaire-mode-themes-to-face-swap '("^modus-")))

;; solaire-mode does not works correctly with org-mode
;; (after! mixed-pitch
;;     (setq mixed-pitch-set-height t))
;; (add-hook 'org-mode-hook #'solaire-mode-reset)

;; Do not modify neotree
(remove-hook 'doom-load-theme-hook #'doom-themes-neotree-config)

;; theme
;; (setq doom-theme 'modus-vivendi)
(setq doom-theme 'modus-operandi)
;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-one-light)

(after! doom-themes
    (setq
        doom-themes-enable-bold t     ; if nil, bold is universally disabled
        doom-themes-enable-italic t)  ; if nil, italics is universally disabled

    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)

    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config))

;; modeline
(after! doom-modeline
    (setq all-the-icons-scale-factor 0.8)
    (advice-add #'doom-modeline--font-height :override (lambda () (frame-char-height))))

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
(set-popup-rule! "^\\*lsp" :size 0.4 :quit t :ttl 0)
(set-popup-rule! "^\\*compilation" :size 0.4 :quit t :ttl 0)
(set-popup-rule! "^\\*ielm" :size 0.4 :quit t :ttl 0)
(set-popup-rule! "^\\*osx-dictionary" :size 0.4 :quit t :ttl 0)
(set-popup-rule! "^\\*Password-Store" :side 'left :size 0.4 :quit nil)
(set-popup-rule! "^\\*ivy-occur" :size 0.4 :ttl 0 :quit nil)
(set-popup-rule! "^\\*cargo-test*" :size 0.4 :ttl 0 :quit nil)
(set-popup-rule! "^\\*Racket REPL*" :size 0.4 :ttl 0 :quit nil)
(set-popup-rule! "^\\*tldr*" :size 0.4 :ttl 0 :quit nil)

;; https://gitlab.com/protesilaos/dotfiles/-/raw/master/emacs/.emacs.d/prot-lisp/modus-themes-exporter.el
(load! "+ui-modus-exporter.el")

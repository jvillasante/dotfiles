;;; my-init-misc.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; encryption
;; https://orgmode.org/worg/org-tutorials/encrypting-files.html
;; https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources
(progn
    ;; never save
    (setq auth-source-save-behavior nil)

    ;; setup epg
    (require 'epg)
    (setq epg-gpg-home-directory (expand-file-name ".gnupg" my--home-path))
    (setq epg-gpg-program (executable-find "gpg2"))
    ;; (setq epg-pinentry-mode 'loopback)

    ;; setup epa
    (require 'epa-file)
    (setq epa-file-encrypt-to user-mail-address
          epa-file-cache-passphrase-for-symmetric-encryption nil)
    ;; (epa-file-enable) ;; already enabled?

    ;; setup auth-sources
    (require 'auth-source)
    (setq auth-sources
          (list (expand-file-name "secrets/.authinfo.gpg" no-littering-etc-directory)))

    ;; setup org-crypt
    (require 'org-crypt)
    (org-crypt-use-before-save-magic)
    (setq org-crypt-disable-auto-save nil
          org-tags-exclude-from-inheritance (quote ("crypt"))
          org-crypt-key nil
          org-crypt-key user-mail-address))

;; fortune : a fortune front-end for Emacs.
(use-package fortune
    :preface
    (defun my--fortune ()
        (interactive)
        (when (executable-find "fortune")
            (switch-to-buffer (make-temp-name "fortune"))
            (shell-command "fortune" t)
            (concat (buffer-string) "\n")))
    ;; :init
    ;; (when (executable-find "fortune")
    ;;     (setq initial-scratch-message
    ;;           (with-temp-buffer
    ;;               (shell-command "fortune" t)
    ;;               (let ((comment-start ";;"))
    ;;                   (comment-region (point-min) (point-max)))
    ;;               (concat (buffer-string) "\n"))))
    :config
    (setq fortune-dir "/usr/share/games/fortune")
    (setq fortune-file "/usr/share/games/fortune/fortunes"))

;; persistent-scratch : preserves the state of scratch buffers accross Emacs sessions
(use-package persistent-scratch
    :hook (after-init . persistent-scratch-setup-default))

(use-package re-builder
    :ensure nil ;; emacs built-in
    :config (setq reb-re-syntax 'string))

;; http://www.yummymelon.com/devnull/using-ediff-in-2023.html
(use-package ediff
    :ensure nil ;; emacs built-in
    :config
    (setq ediff-keep-variants nil) ;; Kill variants upon quitting an Ediff session
    (setq ediff-split-window-function 'split-window-horizontally) ;; Show diffs side-by-side
    (setq ediff-window-setup-function 'ediff-setup-windows-plain)) ;; Puts the control panel in the same frame

(use-package electric
    :ensure nil ;; emacs built-in
    :preface (defun my--electric-indent-local-mode-maybe ()
                 "Enable `electric-indent-local-mode' if appropriate."
                 (unless (or (eq major-mode 'fundamental-mode)
                             (eq major-mode 'text-mode)
                             (eq major-mode 'conf-mode))
                     (electric-indent-local-mode 1)))
    :init
    (setq-default electric-indent-chars '(?\n ?\^?))
    (setq-default electric-indent-inhibit t) ;; Making electric-indent behave sanely
    (electric-indent-mode 0) ;; disable by default
    (add-hook 'after-change-major-mode-hook #'my--electric-indent-local-mode-maybe))

(use-package isearch
    :ensure nil ;; emacs built-in
    :config
    (setq isearch-resume-in-command-history t) ; use history for isearch as well
    (setq search-whitespace-regexp ".*?") ; isearch convenience, space matches anything (non-greedy)
    (setq isearch-lax-whitespace t)
    (setq isearch-allow-motion t) ; enable Emacs28 isearch motions
    (setq isearch-lazy-count nil) ; using anzu instead
    (setq lazy-count-prefix-format "(%s/%s) ")
    (setq lazy-count-suffix-format nil)
    (defadvice isearch-search (after isearch-no-fail activate)
        (unless isearch-success
            (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
            (ad-activate 'isearch-search)
            (isearch-repeat (if isearch-forward 'forward))
            (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
            (ad-activate 'isearch-search))))

;; saveplace : remembers your location in a file when saving files
(use-package saveplace
    :ensure nil ;; emacs built-in
    :init (save-place-mode +1))

;; savehist : save minibuffer history
(use-package savehist
    :ensure nil ;; emacs built-in
    :init (savehist-mode +1)
    :config
    (setq history-length t
          history-delete-duplicates t
          savehist-save-minibuffer-history t
          savehist-additional-variables '(register-alist kill-ring search-ring regexp-search-ring)))

;; recentf : recent files
(use-package recentf
    :ensure nil ;; emacs built-in
    :init (recentf-mode +1)
    :config
    (setq recentf-max-saved-items 500
          recentf-max-menu-items 15
          recentf-auto-cleanup 'never)
    (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-var-directory))
    (add-to-list 'recentf-exclude (recentf-expand-file-name (expand-file-name "secrets/" no-littering-etc-directory)))
    (add-to-list 'recentf-exclude (recentf-expand-file-name (expand-file-name "Apps/elfeed/elfeed_db/" my--dropbox-path)))
    (add-to-list 'recentf-exclude (recentf-expand-file-name (expand-file-name ".password-store/" my--home-path)))
    (add-to-list 'recentf-exclude (recentf-expand-file-name (expand-file-name ".mail/" my--home-path)))
    (add-to-list 'recentf-exclude "/dev/shm/")
    (add-to-list 'recentf-exclude "\\.git")
    (add-to-list 'recentf-exclude "\\.jar$\\.rar$\\.zip$\\.tar$\\.gz$\\.iso$\\.7z$")
    (add-to-list 'recentf-exclude "/tmp/")
    (add-to-list 'recentf-exclude "/ssh:")
    (add-to-list 'recentf-exclude "/usr/")
    (add-to-list 'recentf-exclude "\\.?ido\\.last$")
    (add-to-list 'recentf-exclude "^/nix/store/")
    (add-to-list 'recentf-exclude ".+\\.mp3$"))

;; project.el : default project manager
(use-package project
    :ensure nil ;; emacs built-in
    :custom
    ((project-list-file (expand-file-name "projects" no-littering-var-directory))
     (project-vc-extra-root-markers '(".project.el" ".projectile" ".dir-locals.el"))
     (project-vc-ignores '("target/" "bin/" "build/" "obj/")))
    :config
    (add-to-list 'project-switch-commands '(project-dired "Dired at root")))

;; tramp : Transparent Remote Access, Multiple Protocols
(use-package tramp
    :ensure nil ;; emacs built-in
    :init
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
    :config
    (setq remote-file-name-inhibit-locks t)
    (setq remote-file-name-inhibit-cache nil)
    (setq vc-ignore-dir-regexp ;; make sure vc stuff is not making tramp slower
          (format "%s\\|%s"
                  vc-ignore-dir-regexp
                  tramp-file-name-regexp))
    (setq tramp-verbose 1)
    (setq tramp-default-method "ssh")    ; ssh is faster than scp and supports ports.
    (setq tramp-completion-use-auth-sources nil) ; do use `.authinfo.gpg' for tramp
    (setq tramp-shell-prompt-pattern
          "\\(?:^\\|\\)[^]\n#-%>]*#?[]#-%>].*[[:blank:]]*") ; Tramp hangs: Not recognising the remote shell prompt
    (setq tramp-password-prompt-regexp ; Add verification code support.
          (concat
           "^.*"
           (regexp-opt
            '("passphrase" "Passphrase"
              "password" "Password"
              "Verification code")
            t)
           ".*:\0? *")))

(use-package autorevert
    :ensure nil ;; emacs built-in
    :config
    (setq auto-revert-verbose t) ; show message when file changes
    (setq auto-revert-avoid-polling t) ; use save signal
    (setq global-auto-revert-non-file-buffers t) ; Global Auto-Revert Mode operates only on file-visiting buffers.
    (global-auto-revert-mode t)) ; Refresh files automatically when modified from outside emacs

;; electric pair : work with pairs in emacs
(use-package elec-pair
    :ensure nil ;; emacs built-in
    :config
    ;; make electric-pair-mode work on more brackets
    (setq-default electric-pair-pairs
                  '((?\" . ?\")
                    (?\( . ?\))
                    (?\{ . ?\})
                    (?\[ . ?\])))
    (setq-default electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
    (add-hook 'after-init-hook 'electric-pair-mode))

;; ibuffer
(use-package ibuffer
    :ensure nil  ;; emacs built-in
    :config
    (require 'hl-line)
    (require 'mouse)
    (add-hook 'ibuffer-mode-hook #'hl-line-mode)
    (add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)
    (keymap-set ibuffer-mode-map "<double-mouse-1>" #'ibuffer-visit-buffer)
    (keymap-set ibuffer-mode-map "M-<double-mouse-1>" #'ibuffer-visit-buffer-other-window))

;; ibuffer :
(use-package ibuffer-project
    :config
    (setq ibuffer-project-use-cache t)
    (add-to-list 'ibuffer-project-root-functions '(file-remote-p . "Remote"))
    (add-hook 'ibuffer-hook
              (lambda ()
                  (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
                  (unless (eq ibuffer-sorting-mode 'project-file-relative)
                      (ibuffer-do-sort-by-project-file-relative)))))
;; vundo : visual undo
(use-package vundo
    :disabled t
    :config
    (setq vundo-compact-display nil))

(use-package helpful)

(use-package rainbow-delimiters
    :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; crux : A Collection of Ridiculously Useful eXtensions for Emacs
(use-package crux)

;; editorconfig : editorconfig for Emacs
(use-package editorconfig
    :ensure nil ;; emacs built-in
    :config
    (setq editorconfig-exclude-regexps
          '("\\.jar$" "\\.rar$" "\\.zip$" "\\.tar$" "\\.gz$" "\\.iso$" "\\.7z$"))
    (editorconfig-mode 1))

;; avy : GNU Emacs package for jumping to visible text using a char-based decision tree
(use-package avy
    :config
    (setq avy-orders-alist
          '((avy-goto-char-timer . avy-order-closest)
            (avy-goto-line . avy-order-closest))))

;; Expand Region : expand or contract selection
(use-package expand-region :disabled t)

;; Expreg : like expand-region but nicer
(use-package expreg)

;; better C-w and M-w
(use-package whole-line-or-region
    :disabled t ;; using easy-kill instead
    :init (whole-line-or-region-global-mode))

;; easy-kill : Kill & Mark Things Easily in Emacs
(use-package easy-kill)

;; multiple-cursors: Multiple cursors for Emacs
(use-package multiple-cursors)

;; wgrep : edit grep results
(use-package wgrep)

;; surround : insert, change, and, delete surrounding pairs of quotes, braces, etc.
(use-package surround)

;; monkeytype : A typing game/tutor inspired by the open source and community driven monkeytype.com
(use-package monkeytype
    :config
    (setq monkeytype-directory
          (expand-file-name "monkeytype" no-littering-etc-directory)))

;; devdocs.el : Emacs viewer for DevDocs
(use-package devdocs
    :disabled t
    :custom (devdocs-data-dir (no-littering-expand-var-file-name "devdocs")))

;; devdocs-browser : Browse devdocs.io documents inside Emacs!
(use-package devdocs-browser
    :custom (devdocs-browser-cache-directory (no-littering-expand-var-file-name "devdocs-browser")))

;; engine-mode : search the web
(use-package engine-mode
    :init
    (defengine duckduckgo
        "https://duckduckgo.com/?q=%s"
        :keybinding "d")
    (defengine brave-search
        "https://search.brave.com/search?q=%s"
        :keybinding "b")
    (defengine github
        "https://github.com/search?ref=simplesearch&q=%s")
    (defengine google
        "https://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
        :keybinding "g")
    (defengine google-images
        "https://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s")
    (defengine google-maps
        "https://maps.google.com/maps?q=%s"
        :docstring "Mappin' it up.")
    (defengine project-gutenberg
        "https://www.gutenberg.org/ebooks/search/?query=%s")
    (defengine qwant
        "https://www.qwant.com/?q=%s")
    (defengine stack-overflow
        "https://stackoverflow.com/search?q=%s")
    (defengine twitter
        "https://twitter.com/search?q=%s")
    (defengine wikipedia
        "https://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
        :keybinding "w"
        :browser 'eww-browse-url
        :docstring "Searchin' the wikis.")
    (defengine wiktionary
        "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s")
    (defengine wolfram-alpha
        "https://www.wolframalpha.com/input/?i=%s")
    (defengine youtube
        "https://www.youtube.com/results?aq=f&oq=&search_query=%s")
    (defengine cppreference
        "https://en.cppreference.com/w/?search=%s"
        :keybinding "c")
    :config
    (engine-mode t))

(provide 'my-init-misc)
;;; my-init-misc.el ends here

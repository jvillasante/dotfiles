;;; init.el --- Personal configuration -*- lexical-binding: t -*-

;; URL: https://emacs.amodernist.com/
;; Keywords: dotfiles emacs

;;; Commentary:
;; This is my personal Emacs configuration.

;;; Code:

;; Add the NonGNU ELPA package archive
(require 'package)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(unless package-archive-contents  (package-refresh-contents))

;; Load a custom theme
(load-theme 'modus-operandi t)

;; Set default font face
(set-face-attribute 'default nil :font "Iosevka Comfy 16")

;; Disable the menu bar
(menu-bar-mode -1)

;; Disable the tool bar
(tool-bar-mode -1)

;; Disable the scroll bars
(scroll-bar-mode -1)

;; Disable splash screen
(setq inhibit-startup-screen t)

;;; Completion framework
(unless (package-installed-p 'vertico)
    (package-install 'vertico))

;; Enable completion by narrowing
(vertico-mode t)

;; Improve directory navigation
(with-eval-after-load 'vertico
    (define-key vertico-map (kbd "RET") #'vertico-directory-enter)
    (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-word)
    (define-key vertico-map (kbd "M-d") #'vertico-directory-delete-char))

;;; Extended completion utilities
(unless (package-installed-p 'consult)
    (package-install 'consult))
(global-set-key [rebind switch-to-buffer] #'consult-buffer)
(global-set-key (kbd "C-c j") #'consult-line)
(global-set-key (kbd "C-c i") #'consult-imenu)
(setq read-buffer-completion-ignore-case t
    read-file-name-completion-ignore-case t
    completion-ignore-case t)

;; Enable line numbering in `prog-mode'
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Automatically pair parentheses
(electric-pair-mode t)

;;; LSP Support
(unless (package-installed-p 'eglot)
    (package-install 'eglot))

;; Enable LSP support by default in programming buffers
(add-hook 'prog-mode-hook #'eglot-ensure)

;; Create a memorable alias for `eglot-ensure'.
(defalias 'start-lsp-server #'eglot)

;;; Inline static analysis

;; Enabled inline static analysis
(add-hook 'prog-mode-hook #'flymake-mode)

;; Display messages when idle, without prompting
(setq help-at-pt-display-when-idle t)

;; Message navigation bindings
(with-eval-after-load 'flymake
    (define-key flymake-mode-map (kbd "C-c n") #'flymake-goto-next-error)
    (define-key flymake-mode-map (kbd "C-c p") #'flymake-goto-prev-error))

;;; Pop-up completion
(unless (package-installed-p 'corfu)
    (package-install 'corfu))

;; Enable autocompletion by default in programming buffers
(add-hook 'prog-mode-hook #'corfu-mode)

;; Enable automatic completion.
(setq corfu-auto t)

;;; Git client
(unless (package-installed-p 'magit)
    (package-install 'magit))

;; Bind the `magit-status' command to a convenient key.
(global-set-key (kbd "C-c g") #'magit-status)

;; Show word-granularity differences within diff hunks
(setq magit-diff-refine-hunk t)

;;; Indication of local VCS changes
(unless (package-installed-p 'diff-hl)
    (package-install 'diff-hl))

;; Enable `diff-hl' support by default in programming buffers
(add-hook 'prog-mode-hook #'diff-hl-mode)

;; Update the highlighting without saving
(diff-hl-flydiff-mode t)

;;; Go Support
(unless (package-installed-p 'go-mode)
    (package-install 'go-mode))

;;; JSON Support
(unless (package-installed-p 'json-mode)
    (package-install 'json-mode))

;;; Julia Support
(unless (package-installed-p 'julia-mode)
    (package-install 'julia-mode))

;;; Lua Support
(unless (package-installed-p 'lua-mode)
    (package-install 'lua-mode))

;;; NASM Support
(unless (package-installed-p 'nasm-mode)
    (package-install 'nasm-mode))

;;; Rust Support
(unless (package-installed-p 'rust-mode)
    (package-install 'rust-mode))

;;; Additional Lisp support
(unless (package-installed-p 'sly)
    (package-install 'sly))

;;; Typescript Support
(unless (package-installed-p 'typescript-mode)
    (package-install 'typescript-mode))

;;; YAML Support
(unless (package-installed-p 'yaml-mode)
    (package-install 'yaml-mode))

;;; LaTeX support
;; (unless (package-installed-p 'auctex)
;;     (package-install 'auctex))
;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)
;; (setq-default TeX-master nil)

;; Enable LaTeX math support
;; (add-hook 'LaTeX-mode-map #'LaTeX-math-mode)

;; Enable reference mangment
;; (add-hook 'LaTeX-mode-map #'reftex-mode)

;;; Markdown support
(unless (package-installed-p 'markdown-mode)
    (package-install 'markdown-mode))

;;; Outline-based notes management and organizer
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)

;;; Additional Org-mode related functionality
(unless (package-installed-p 'org-contrib)
    (package-install 'org-contrib))

;;; EditorConfig support
(unless (package-installed-p 'editorconfig)
    (package-install 'editorconfig))

;; Enable EditorConfig
(editorconfig-mode t)

;;; In-Emacs Terminal Emulation
(unless (package-installed-p 'eat)
    (package-install 'eat))

;; Close the terminal buffer when the shell terminates.
(setq eat-kill-buffer-on-exit t)

;; Enable mouse-support.
(setq eat-enable-mouse t)

;;; Jump to arbitrary positions
(unless (package-installed-p 'avy)
    (package-install 'avy))
(global-set-key (kbd "C-c z") #'avy-goto-word-1)

;; Miscellaneous options
(setq-default major-mode
    (lambda () ; guess major mode from file name
        (unless buffer-file-name
            (let ((buffer-file-name (buffer-name)))
                (set-auto-mode)))))
(setq confirm-kill-emacs #'yes-or-no-p)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(defalias 'yes-or-no #'y-or-n-p)

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
    (load custom-file))

(provide 'init)
;;; init.el ends here

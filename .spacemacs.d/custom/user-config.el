(setq ispell-program-name "aspell")
(setq auto-window-vscroll nil)
(setq sp-escape-quotes-after-insert nil)

(setq-default
  user-full-name "Julio C. Villasante"
  user-mail-address "jvillasantegomez@gmail.com"

  major-mode 'text-mode
  use-dialog-box nil
  vc-follow-symlinks t
  paradox-github-token t
  load-prefer-newer t
  fill-column 110                    ; Maximum line width
  truncate-lines t                   ; Don't fold lines
  truncate-partial-width-windows nil ; for vertically-split windows
  split-width-threshold 160          ; Split verticaly by default
  evil-cross-lines t                 ; Make horizontal movement cross lines

  ;; scroll
  scroll-margin 3

  ;; my coding style, bsd but with 2 spaces indentation (and no tab
  ;; characters, only spaces)
  c-basic-indent 2
  c-basic-offset 2
  tab-width 2
  indent-tabs-mode nil
  highlight-tabs t

  ;; Whitespace settings
  whitespace-action '(auto-cleanup)
  whitespace-style '(indentation::space
                      space-after-tab
                      space-before-tab
                      trailing
                      lines-tail
                      tab-mark
                      face
                      tabs)

  doc-view-continuous t

  ;; tramp mode
  tramp-default-method "ssh"

  ;; LaTeX
  font-latex-fontify-script nil
  TeX-newline-function 'reindent-then-newline-and-indent)

;; UTF-8 please
(set-charset-priority 'unicode)
(setq locale-coding-system   'utf-8)   ; pretty
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; please
(prefer-coding-system        'utf-8)   ; with sugar on top
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; multiterm
(setq multi-term-program jv/zsh-path)

;; line spacing
(setq-default line-spacing 0.1)

;; Isearch convenience, space matches anything (non-greedy)
(setq search-whitespace-regexp ".*?")

;; Hooks
(add-hook 'spacemacs-buffer-mode-hook
  'spacemacs/toggle-visual-line-navigation-on)
(add-hook 'term-mode-hook
  (lambda ()
    (setq term-buffer-maximum-size 10000)))
(add-hook 'focus-out-hook
  (lambda ()
    (save-some-buffers t)))
(add-hook 'prog-mode-hook
  (lambda ()
    (set-fill-column 110)
    (flyspell-prog-mode)))
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'makefile-mode-hook 'whitespace-mode)
(remove-hook 'prog-mode-hook 'spacemacs//show-trailing-whitespace)

;; use evil-matchit everywhere
(global-evil-matchit-mode 1)

;; Make movement keys work like they should
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

;; use count-words instead of count-words-region as it works on buffer
;; if no region is selected
(global-set-key (kbd "M-=") 'count-words)

;; use company everywhere
(with-eval-after-load 'company
  ;; // delete only needed for irony, not for ycmd
  ;; (setq company-backends (delete 'company-semantic company-backends))
  (add-hook 'after-init-hook 'global-company-mode))

(cond
  ((spacemacs/system-is-mac)
    (setq browse-url-browser-function 'browse-url-generic
      engine/browser-function 'browse-url-generic
      browse-url-generic-program "open"))
  ((spacemacs/system-is-linux)
    (executable-find "google-chrome")))

;; yasnippet
(spacemacs/set-leader-keys "is" 'yas-insert-snippet)
(spacemacs/set-leader-keys "id" 'yas-describe-tables)

;; (golden-ratio-mode 1)
(show-paren-mode 1)
(spaceline-compile)

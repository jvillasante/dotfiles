;;; my-init-bindings.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;;; Links:
;; https://www.masteringemacs.org/article/mastering-key-bindings-emacs

;; General
(global-set-key (kbd "C-x C-m") 'execute-extended-command) ; ALT may or may not be available
(global-set-key (kbd "C-c C-m") 'execute-extended-command) ; ALT may or may not be available
(global-set-key (kbd "C-x C-M") 'execute-extended-command-for-buffer) ; ALT may or may not be available
(global-set-key (kbd "C-c C-M") 'execute-extended-command-for-buffer) ; ALT may or may not be available
(global-set-key (kbd "C-c u") 'browse-url-at-point) ; simple browse url
(global-set-key (kbd "C-x k") 'kill-current-buffer) ; kill buffer without prompt
(global-set-key (kbd "C-x K") 'kill-buffer) ; prompt for buffer to kill
(global-set-key (kbd "C-x S") 'my/save-all) ; save some buffers without prompt

;; Both `C-z' and `C-x C-z' are bound to `suspend-frame' which will effectively
;; suspend the current frame. To bring back the suspended frame in GUI mode we
;; need to send `SIGCONT' to emacs by `kill -CONT $emacs_pid' or `killall -CONT
;; emacs' while in terminal `fg' or `%emacs' will work just fine.
(global-unset-key (kbd "C-z"))
;; (global-unset-key (kbd "C-x C-z"))

;; useful for C/C++ finding header/impl files (override with eglot)
(global-set-key (kbd "C-x C-o") 'ff-find-other-file)
(with-eval-after-load 'c-ts-mode
    (define-key c-ts-base-mode-map (kbd "C-x C-o") 'my/eglot-clangd-find-other-file))

;; Use Ctrl+arrow keys to move between windows.
(windmove-default-keybindings 'control)

;; better `keyword-quit'
(define-key global-map [remap keyboard-quit] 'my/keyboard-quit-dwim)

;; Repeat in emacs is not as good as vim :(
(global-set-key (kbd "C-.") 'repeat)

;; really kill emacs even on `emacsclient'
(global-set-key (kbd "C-x C-S-c") 'save-buffers-kill-emacs)

;; Clone the current buffer in a new window with `q' to exit
(global-set-key (kbd "C-x 9") 'my/clone-buffer-in-new-window-readonly) ; same

;; ctl-x-4-map
(define-key ctl-x-4-map (kbd "T") 'my/toggle-window-split)

;; duplicate current line or region
(global-set-key (kbd "C-x j") #'duplicate-dwim)

;; upcase, downcase and capitalize
(define-key (current-global-map) [remap capitalize-word] 'capitalize-dwim)
(define-key (current-global-map) [remap downcase-word] 'downcase-dwim)
(define-key (current-global-map) [remap upcase-word] 'upcase-dwim)

;; count-words
(define-key (current-global-map) [remap count-words-region] 'count-words)

;; Open line(s) below/above current one (using crux implementation)
;; (global-set-key (kbd "C-o") 'my/open-next-line)
;; (global-set-key (kbd "M-o") 'my/open-previous-line)

;; better comment/un-comment
(global-set-key (kbd "M-;") 'my/comment-or-uncomment)
(global-set-key (kbd "C-x C-;") 'my/comment-or-uncomment)

;; fill-unfill
(define-key global-map [remap fill-paragraph] 'my/fill-or-unfill)
(global-set-key (kbd "M-Q") 'my/unfill-paragraph)
(global-set-key (kbd "C-M-Q") 'my/unfill-region)

;; eldoc
(with-eval-after-load 'eldoc
    (global-set-key (kbd "C-h .") 'eldoc))

;; prog-mode
(with-eval-after-load 'prog-mode
    (define-key emacs-lisp-mode-map (kbd "C-h .") 'helpful-at-point)
    (with-eval-after-load 'treesit
        (global-set-key (kbd "M-<up>")   'treesit-beginning-of-defun)
        (global-set-key (kbd "M-<down>") 'treesit-end-of-defun))
    (with-eval-after-load 'flymake
        (define-key prog-mode-map (kbd "M-n") 'flymake-goto-next-error)
        (define-key prog-mode-map (kbd "M-p") 'flymake-goto-prev-error))
    (with-eval-after-load 'flycheck
        (define-key prog-mode-map (kbd "M-n") 'flycheck-next-error)
        (define-key prog-mode-map (kbd "M-p") 'flycheck-previous-error)))

(with-eval-after-load 'isearch
    ;; Prevents issue where you have to press backspace twice when trying to remove the first character that fails a search
    (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)
    (define-key isearch-mode-map (kbd "C-o") 'isearch-occur) ; occur
    (define-key isearch-mode-map (kbd "C-n") 'isearch-repeat-forward)
    (define-key isearch-mode-map (kbd "C-p") 'isearch-repeat-backward))

(with-eval-after-load 'ibuffer
    (define-key global-map [remap list-buffers] 'ibuffer)
    (define-key ibuffer-mode-map (kbd "q") 'my/close-buffer-and-window)
    (keymap-set ibuffer-mode-map "{" #'ibuffer-backwards-next-marked)
    (keymap-set ibuffer-mode-map "}" #'ibuffer-forward-next-marked)
    (keymap-set ibuffer-mode-map "[" #'ibuffer-backward-filter-group)
    (keymap-set ibuffer-mode-map "]" #'ibuffer-forward-filter-group)
    (keymap-set ibuffer-mode-map "$" #'ibuffer-toggle-filter-group)
    (keymap-set ibuffer-mode-map "<double-mouse-1>" #'ibuffer-visit-buffer)
    (keymap-set ibuffer-mode-map "M-<double-mouse-1>" #'ibuffer-visit-buffer-other-window))

(with-eval-after-load 'vertico
    ;; (define-key vertico-map (kbd "RET") 'vertico-directory-enter)
    ;; (define-key vertico-map (kbd "DEL") 'vertico-directory-delete-word)
    (define-key vertico-map (kbd "M-d") 'vertico-directory-delete-char)
    (define-key vertico-map (kbd "M-,") 'vertico-quick-insert)
    (define-key vertico-map (kbd "M-.") 'vertico-quick-exit)
    (define-key vertico-map (kbd "M-G") 'vertico-multiform-grid)
    (define-key vertico-map (kbd "M-F") 'vertico-multiform-flat)
    (define-key vertico-map (kbd "M-R") 'vertico-multiform-reverse)
    (define-key vertico-map (kbd "M-U") 'vertico-multiform-unobtrusive))

(with-eval-after-load 'corfu
    (define-key corfu-map (kbd "SPC") 'corfu-insert-separator))

(with-eval-after-load 'cape
    (global-set-key (kbd "C-c p") 'cape-prefix-map))

;; consult
(when (package-installed-p 'consult)
    ;; C-c bindings (mode-specific-map)
    (global-set-key (kbd "C-c M-x") 'consult-mode-command)
    (global-set-key (kbd "C-c h")   'consult-history)
    (global-set-key (kbd "C-c k")   'consult-kmacro)
    (global-set-key (kbd "C-c m")   'consult-man)
    (global-set-key (kbd "C-c i")   'consult-info)
    (define-key global-map [remap Info-search] 'consult-info)

    ;; C-x bindings (ctl-x-map)
    (global-set-key (kbd "C-x M-:") 'consult-complex-command)     ;; orig. repeat-complex-command
    (global-set-key (kbd "C-x b")   'consult-buffer)              ;; orig. switch-to-buffer
    (global-set-key (kbd "C-x 4 b") 'consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
    (global-set-key (kbd "C-x 5 b") 'consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
    (global-set-key (kbd "C-x t b") 'consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
    (global-set-key (kbd "C-x r b") 'consult-bookmark)            ;; orig. bookmark-jump
    (global-set-key (kbd "C-x p b") 'consult-project-buffer)      ;; orig. project-switch-to-buffer
    (global-set-key (kbd "C-x C-r") 'consult-recent-file)         ;; orig. find-file-readonly

    ;; Custom M-# bindings for fast register access
    (global-set-key (kbd "M-#")   'consult-register-load)
    (global-set-key (kbd "M-'")   'consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
    (global-set-key (kbd "C-M-#") 'consult-register)

    ;; Other custom bindings
    (global-set-key (kbd "M-y") 'consult-yank-pop) ;; orig. yank-pop

    ;; M-g bindings (goto-map)
    (global-set-key (kbd "M-g e")   'consult-compile-error)
    (global-set-key (kbd "M-g f")   'consult-flymake)        ;; Alternative: consult-flycheck
    (global-set-key (kbd "M-g g")   'consult-goto-line)      ;; orig. goto-line
    (global-set-key (kbd "M-g M-g") 'consult-goto-line)      ;; orig. goto-line
    (global-set-key (kbd "M-g o")   'consult-outline)        ;; Alternative: consult-org-heading
    (global-set-key (kbd "M-g m")   'consult-mark)
    (global-set-key (kbd "M-g k")   'consult-global-mark)
    (global-set-key (kbd "M-g i")   'consult-imenu)
    (global-set-key (kbd "M-g I")   'consult-imenu-multi)

    ;; M-s bindings (search-map)
    (global-set-key (kbd "M-s d") 'consult-fd)         ;; Alternative: consult-find
    (global-set-key (kbd "M-s D") 'consult-locate)
    (global-set-key (kbd "M-s g") 'consult-grep)
    (global-set-key (kbd "M-s G") 'consult-git-grep)
    (global-set-key (kbd "M-s r") 'consult-ripgrep)
    (global-set-key (kbd "M-s l") 'consult-line)
    (global-set-key (kbd "M-s L") 'consult-line-multi)
    (global-set-key (kbd "M-s k") 'consult-keep-lines)
    (global-set-key (kbd "M-s u") 'consult-focus-lines)

    ;; Isearch integration
    (global-set-key (kbd "M-s e") 'consult-isearch-history)
    (define-key isearch-mode-map (kbd "M-e")   'consult-isearch-history) ;; orig. isearch-edit-string
    (define-key isearch-mode-map (kbd "M-s e") 'consult-isearch-history) ;; orig. isearch-edit-string
    (define-key isearch-mode-map (kbd "M-s l") 'consult-line)            ;; needed by consult-line to detect isearch
    (define-key isearch-mode-map (kbd "M-s L") 'consult-line-multi)      ;; needed by consult-line to detect isearch

    ;; Minibuffer history
    (define-key minibuffer-local-map (kbd "M-s") 'consult-history)   ;; orig. next-matching-history-element
    (define-key minibuffer-local-map (kbd "M-r") 'consult-history))  ;; orig. previous-matching-history-element

(with-eval-after-load 'consult-dir
    (define-key global-map [remap list-directory] 'consult-dir)
    (with-eval-after-load 'vertico
        (define-key vertico-map (kbd "C-c C-d") 'consult-dir)
        (define-key vertico-map (kbd "C-c C-j") 'consult-dir-jump-file)))

(with-eval-after-load 'embark
    ;; (define-key minibuffer-local-map (kbd "C-;")   'embark-act)
    ;; (define-key minibuffer-local-map (kbd "C-SPC") 'embark-act-all)
    ;; (define-key minibuffer-local-map (kbd "M-;")   'embark-dwim)
    ;; (define-key minibuffer-local-map (kbd "C-h B") 'embark-bindings)
    (global-set-key (kbd "C-;")   'embark-act)
    (global-set-key (kbd "M-;")   'embark-dwim)
    (global-set-key (kbd "C-h B") 'embark-dwim))

(with-eval-after-load 'marginalia
    ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
    ;; available in the *Completions* buffer, add it to the `completion-list-mode-map'.
    (define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle))

(with-eval-after-load 'jinx
    (global-set-key (kbd "M-$") 'jinx-correct)
    (global-set-key (kbd "C-M-$") 'jinx-languages))

(with-eval-after-load 'tempel
    (global-set-key (kbd "M-+") 'tempel-complete) ;; Alternative tempel-expand
    (global-set-key (kbd "M-*") 'tempel-insert))

(with-eval-after-load 'yasnippet
    (global-set-key (kbd "M-+") 'yas-expand)
    (global-set-key (kbd "M-*") 'yas-insert-snippet))

(with-eval-after-load 'langtool
    (global-set-key (kbd "C-x 4 w") 'langtool-check)
    (global-set-key (kbd "C-x 4 W") 'langtool-check-done)
    (global-set-key (kbd "C-x 4 c") 'langtool-interactive-correction)
    (global-set-key (kbd "C-x 4 l") 'langtool-switch-default-language)
    (global-set-key (kbd "C-x 4 4") 'langtool-show-message-at-point))

(when (package-installed-p 'magit)
    (global-set-key (kbd "C-x g") 'magit-status)
    (with-eval-after-load 'magit
        (define-key magit-status-mode-map (kbd "q") 'my/magit-kill-buffers)
        (define-key magit-status-mode-map (kbd "C-x k") 'my/magit-kill-buffers)))

(with-eval-after-load 'org
    (define-key org-mode-map [remap fill-paragraph] 'my/org-fill-or-unfill)
    (define-key org-mode-map (kbd "M-n") 'org-next-visible-heading)
    (define-key org-mode-map (kbd "M-p") 'org-previous-visible-heading)
    (when (package-installed-p 'verb)
        (define-key org-mode-map (kbd "C-c C-r") verb-command-map)))

(with-eval-after-load 'markdown-mode
    (define-key markdown-mode-map (kbd "TAB") 'markdown-cycle))

;; shells
(global-set-key (kbd "C-c o e") 'eshell)
(global-set-key (kbd "C-c o E") 'my/eshell-other-window)
(global-set-key (kbd "C-c o s") 'shell)
(global-set-key (kbd "C-c o S") 'my/shell-other-window)
(when (package-installed-p 'vterm)
    ;; global keys
    (global-set-key (kbd "C-c o t") 'vterm)
    (global-set-key (kbd "C-c o T") 'vterm-other-window)

    (with-eval-after-load 'project
        ;; project-prefix-map
        (define-key project-prefix-map (kbd "t") 'my/vterm-project)
        (define-key project-prefix-map (kbd "T") 'my/vterm-project-other-window))

    (with-eval-after-load 'vterm
        ;; vterm-copy-mode-map
        (define-key vterm-copy-mode-map (kbd "<return>") 'my/vterm-copy-mode-cancel)
        (define-key vterm-copy-mode-map (kbd "RET")      'my/vterm-copy-mode-cancel)

        ;; vterm-mode-map
        (define-key vterm-mode-map (kbd "<insert>") 'ignore)
        (define-key vterm-mode-map (kbd "C-g")      'vterm-send-escape)
        (define-key vterm-mode-map (kbd "M-[")      'vterm-copy-mode)
        (define-key vterm-mode-map (kbd "C-q")      'vterm-send-next-key)))

(with-eval-after-load 'dwim-shell-command
    (define-key global-map [remap shell-command] 'dwim-shell-command)
    (define-key global-map [remap async-shell-command] 'dwim-shell-command)
    (define-key dired-mode-map [remap dired-do-shell-command] 'dwim-shell-command)
    (define-key dired-mode-map [remap dired-do-async-shell-command] 'dwim-shell-command)
    (define-key dired-mode-map [remap dired-smart-shell-command] 'dwim-shell-command))

(with-eval-after-load 'helpful
    ;; (define-key helpful-mode-map (kbd "q") 'kill-current-buffer)
    (define-key global-map [remap describe-function] 'helpful-callable)
    (define-key global-map [remap describe-variable] 'helpful-variable)
    (define-key global-map [remap describe-command]  'helpful-command)
    (define-key global-map [remap describe-key]      'helpful-key)
    (define-key global-map [remap describe-symbol]   'helpful-symbol)
    (global-set-key (kbd "C-c C-d")                  'helpful-at-point)
    (global-set-key (kbd "C-h F")                    'helpful-function)
    (global-set-key (kbd "C-h K")                    'describe-keymap))

(with-eval-after-load 'crux
    ;; (define-key dired-mode-map (kbd "C-<return>") 'crux-open-with)
    ;; (global-set-key (kbd "C-c o") 'crux-open-with)
    (global-set-key (kbd "C-k") 'crux-smart-kill-line)
    (global-set-key (kbd "M-o") 'crux-smart-open-line-above)
    (global-set-key (kbd "C-o") 'crux-smart-open-line)
    ;; (global-set-key (kbd "C-c n") 'crux-cleanup-buffer-or-region)
    ;; (global-set-key (kbd "C-c f") 'crux-recentf-find-file)
    ;; (global-set-key (kbd "C-c F") 'crux-recentf-find-directory)
    ;; (global-set-key (kbd "C-c u") 'crux-view-url)
    ;; (global-set-key (kbd "C-c e") 'crux-eval-and-replace)
    (global-set-key (kbd "C-x 4 t") 'crux-transpose-windows)
    (global-set-key (kbd "C-c D") 'crux-delete-file-and-buffer)
    ;; (global-set-key (kbd "C-c c") 'crux-copy-file-preserve-attributes)
    (global-set-key (kbd "C-c d") 'crux-duplicate-current-line-or-region)
    (global-set-key (kbd "C-c M-d") 'crux-duplicate-and-comment-current-line-or-region)
    (global-set-key (kbd "C-c r") 'crux-rename-buffer-and-file)
    ;; (global-set-key (kbd "C-c t") 'crux-visit-term-buffer)
    ;; (global-set-key (kbd "C-c k") 'crux-kill-other-buffers)
    ;; (global-set-key (kbd "C-M-z") 'crux-indent-defun)
    ;; (global-set-key (kbd "C-c TAB") 'crux-indent-rigidly-and-copy-to-clipboard)
    ;; (global-set-key (kbd "C-c i") 'crux-find-user-init-file)
    ;; (global-set-key (kbd "C-c I") 'crux-find-user-custom-file)
    ;; (global-set-key (kbd "C-c S") 'crux-find-shell-init-file)
    (global-set-key (kbd "C-^") 'crux-top-join-line)
    ;; (global-set-key (kbd "C-<backspace>" . crux-kill-line-backwards)
    ;; (global-set-key (kbd "C-S-Backspace") 'crux-kill-and-join-forward)
    ;; (global-set-key (kbd "C-c P") 'crux-kill-buffer-truename)
    ;; (global-set-key (kbd "C-c s") 'crux-ispell-word-then-abbrev)
    ;; (global-set-key (kbd "C-x C-u") 'crux-upcase-region)
    ;; (global-set-key (kbd "C-x C-l") 'crux-downcase-region)
    ;; (global-set-key (kbd "C-x M-c") 'crux-capitalize-region)
    ;; (global-set-key (kbd "m-o") 'crux-other-window-or-switch-buffer)
    ;; (global-set-key (kbd "C-c w") 'crux-swap-windows)
    (define-key global-map [remap move-beginning-of-line] 'crux-move-beginning-of-line)
    (define-key global-map [remap kill-whole-line] 'crux-kill-whole-line))

(with-eval-after-load 'avy
    (global-set-key (kbd "M-j") 'avy-goto-char-timer)
    (global-set-key (kbd "C-c C-j") 'avy-resume)

    ;; Isearch integration
    (define-key isearch-mode-map (kbd "M-j") 'avy-isearch))

(with-eval-after-load 'expand-region
    (global-set-key (kbd "C-=") 'er/expand-region)
    (global-set-key (kbd "C--") 'er/contract-region))

(with-eval-after-load 'easy-kill
    (global-set-key [remap kill-ring-save] 'easy-kill)
    (global-set-key [remap mark-sexp] 'easy-mark))

(with-eval-after-load 'expreg
    (global-set-key (kbd "C-=") 'expreg-expand)
    (global-set-key (kbd "C--") 'expreg-contract))

(when (package-installed-p 'multiple-cursors)
    (global-set-key (kbd "H-SPC") 'set-rectangular-region-anchor)
    (global-set-key (kbd "C-M-SPC") 'set-rectangular-region-anchor)
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
    (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines))

(with-eval-after-load 'wgrep
    (define-key grep-mode-map (kbd "e") 'wgrep-change-to-wgrep-mode)
    (define-key grep-mode-map (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode)
    (define-key grep-mode-map (kbd "C-c C-c") 'wgrep-finish-edit))

(with-eval-after-load 'surround
    (define-key global-map (kbd "M-'") surround-keymap))

(with-eval-after-load 'devdocs
    (global-set-key (kbd "C-h D") 'devdocs-lookup))
(with-eval-after-load 'devdocs-browser
    (global-set-key (kbd "C-h D") 'devdocs-browser-open))

(with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "C-<return>") 'dired-do-open)
    (define-key dired-mode-map (kbd "q") 'my/close-buffer-and-window)
    (define-key dired-mode-map (kbd "E") 'dired-toggle-read-only))

(with-eval-after-load 'dired-x
    (define-key dired-mode-map (kbd ".") 'dired-omit-mode))

(when (package-installed-p 'dired-sidebar)
    (global-set-key (kbd "C-x C-n") 'dired-sidebar-toggle-sidebar))

(when (package-installed-p 'neotree)
    (global-set-key (kbd "C-x C-n") 'my/neotree-project-dir)
    (with-eval-after-load 'neotree
        (define-key neotree-mode-map (kbd ".") 'neotree-hidden-file-toggle)))

(with-eval-after-load 'ace-window
    (global-set-key (kbd "C-x o") 'ace-window))

(with-eval-after-load 'pixel-scroll
    (define-key global-map [remap scroll-up-command]   'my/pixel-scroll-up-command)
    (define-key global-map [remap scroll-down-command] 'my/pixel-scroll-down-command)
    (define-key global-map [remap recenter-top-bottom] 'my/pixel-recenter-top-bottom))

(with-eval-after-load 'gptel
    (define-key gptel-mode-map (kbd "C-c C-c") 'gptel-send))

(with-eval-after-load 'copilot
    (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
    (define-key copilot-completion-map (kbd "TAB")   'copilot-accept-completion))

(with-eval-after-load 'pdf-tools
    (define-key pdf-view-mode-map (kbd "q") nil)
    (define-key pdf-view-mode-map (kbd "o") 'pdf-outline))

(with-eval-after-load 'nov
    (define-key nov-mode-map (kbd "q") nil))

(when (package-installed-p 'docker)
    (global-set-key (kbd "C-c o d") 'docker))

;;; Prefix
;; C-c f : find
(global-set-key (kbd "C-c f f") 'project-find-file)
(global-set-key (kbd "C-c f F") 'find-file)
(global-set-key (kbd "C-c f D") 'project-dired)
(global-set-key (kbd "C-c f o") 'consult-recent-file)
(global-set-key (kbd "C-c f r") 'consult-yank-from-kill-ring)
(global-set-key (kbd "C-c f b") 'consult-buffer)
(global-set-key (kbd "C-c f p") 'project-switch-project)
(global-set-key (kbd "C-c f g") 'consult-ripgrep)
(global-set-key (kbd "C-c f a") 'embark-act)
;; (global-set-key (kbd "C-c f j") 'evil-collection-consult-jump-list)
;; (global-set-key (kbd "C-c f m") 'evil-collection-consult-mark)
(global-set-key (kbd "C-c f i") 'consult-imenu)
(global-set-key (kbd "C-c f I") 'consult-imenu-multi)
(global-set-key (kbd "C-c f l") 'consult-line)
(global-set-key (kbd "C-c f L") 'consult-line-multi)

;; C-c w : windows
(global-set-key (kbd "C-c w m") 'minimize-window)
(global-set-key (kbd "C-c w M") 'maximize-window)
(global-set-key (kbd "C-c w =") 'balance-windows)
(with-eval-after-load 'crux
    (global-set-key (kbd "C-c w w") 'crux-swap-windows))
(with-eval-after-load 'winner
    (global-set-key (kbd "C-c w u") 'winner-undo)
    (global-set-key (kbd "C-c w r") 'winner-redo))

;; C-c c : Code
(with-eval-after-load 'hs-minor-mode
    (define-key hs-minor-mode-map (kbd "C-c c f") 'my/hs-toggle-hidding))
(with-eval-after-load 'consult-eglot
    (define-key eglot-mode-map (kbd "C-c c s") 'consult-eglot-symbols))
(with-eval-after-load 'eglot
    (define-key eglot-mode-map (kbd "C-c c R") 'eglot-reconnect)
    (define-key eglot-mode-map (kbd "C-c c r") 'eglot-rename)
    (define-key eglot-mode-map (kbd "C-c c d") 'eglot-find-declaration)
    (define-key eglot-mode-map (kbd "C-c c .") 'eglot-find-typeDefinition)
    (define-key eglot-mode-map (kbd "C-c c i") 'eglot-find-implementation)
    (define-key eglot-mode-map (kbd "C-c c a") 'eglot-code-actions)
    (define-key eglot-mode-map (kbd "C-c c o") 'eglot-code-action-organize-imports))
(with-eval-after-load 'flymake
    (define-key flymake-mode-map (kbd "C-c c e") 'flymake-show-buffer-diagnostics)
    (define-key flymake-mode-map (kbd "C-c c E") 'flymake-show-project-diagnostics))

;; C-c o : Open
(when (package-installed-p 'elfeed)
    (global-set-key (kbd "C-c o f") 'elfeed))
(when (package-installed-p 'consult-notes)
    (global-set-key (kbd "C-c o n") 'consult-notes))
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o c") 'calc)
(global-set-key (kbd "C-c o i") 'ielm)
(global-set-key (kbd "C-c o b") 'my/open-link-at-point-or-minibuffer-with-choice)
(global-set-key (kbd "C-c o m") 'my/new-scratch-buffer-in-markdown)
(global-set-key (kbd "C-c o o") 'my/new-scratch-buffer-in-org)

;; C c n : Notes
(when (package-installed-p 'consult-notes)
    (global-set-key (kbd "C-c n n") 'consult-notes))
(global-set-key (kbd "C-c n a") 'org-agenda)

;; C-c l : lookup
(when (fboundp 'dictionary-lookup-definition)
    (global-set-key (kbd "C-c l d") 'dictionary-lookup-definition)
    (global-set-key (kbd "C-c l D") 'dictionary-search))

(provide 'my-init-bindings)
;;; my-init-bindings.el ends here

;;; my-init-bindings.el --- -*- lexical-binding: t; -*-
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
(global-set-key (kbd "C-x k") 'kill-this-buffer) ; kill buffer without prompt
(global-set-key (kbd "C-x K") 'kill-buffer) ; prompt for buffer to kill
(global-set-key (kbd "C-x S") 'my--save-all) ; save some buffers without prompt
(global-set-key (kbd "C-x C-o") 'ff-find-other-file) ; useful for C/C++ finding header/impl files

;; Rebind C-z
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-set-key (kbd "C-z") 'repeat)
(global-set-key (kbd "C-x C-z") 'repeat)

;; really kill emacs even on `emacsclient'
(global-set-key (kbd "C-x C-S-c") 'save-buffers-kill-emacs)

;; Clone the current buffer in a new window with `q' to exit
(global-set-key (kbd "C-x 9") 'my--clone-buffer-in-new-window-readonly) ; same

;; ctl-x-4-map
(define-key ctl-x-4-map (kbd "t") 'my--toggle-window-split)

;; duplicate current line or region
(global-set-key (kbd "C-x j") #'duplicate-dwim)

;; upcase, downcase and capitalize
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

;; Open line(s) below/above current one
(global-set-key (kbd "C-o") 'my--open-next-line)
(global-set-key (kbd "M-o") 'my--open-previous-line)

;; better comment/un-comment
(global-set-key (kbd "M-;") 'my--comment-or-uncomment)
(global-set-key (kbd "C-x C-;") 'my--comment-or-uncomment)

;; fill-unfill
(define-key global-map [remap fill-paragraph] 'my--fill-or-unfill)
(global-set-key (kbd "M-Q") 'my--unfill-paragraph)
(global-set-key (kbd "C-M-Q") 'my--unfill-region)

;; prog-mode
(with-eval-after-load 'prog-mode
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
    (define-key isearch-mode-map (kbd "C-n") 'isearch-repeat-forward) ; better navigation
    (define-key isearch-mode-map (kbd "C-p") 'isearch-repeat-backward)) ; better navigation

(with-eval-after-load 'ibuffer
    (define-key global-map [remap list-buffers] 'ibuffer)
    (define-key ibuffer-mode-map (kbd "q") 'kill-this-buffer))

;; vertico
(with-eval-after-load 'vertico
    ;; (define-key vertico-map (kbd "RET") 'vertico-directory-enter)
    ;; (define-key vertico-map (kbd "DEL") 'vertico-directory-delete-word)
    (define-key vertico-map (kbd "M-d") 'vertico-directory-delete-char)
    (define-key vertico-map (kbd "M-,") 'vertico-quick-insert)
    (define-key vertico-map (kbd "M-.") 'vertico-quick-exit))

;; corfu
(with-eval-after-load 'corfu
    (define-key corfu-map (kbd "SPC") 'corfu-insert-separator))

;; cape
(with-eval-after-load 'cape
    (global-set-key (kbd "C-c p p") 'completion-at-point) ;; capf
    (global-set-key (kbd "C-c p t") 'complete-tag)        ;; etags
    (global-set-key (kbd "C-c p d") 'cape-dabbrev)        ;; or dabbrev-completion
    (global-set-key (kbd "C-c p h") 'cape-history)
    (global-set-key (kbd "C-c p f") 'cape-file)
    (global-set-key (kbd "C-c p k") 'cape-keyword)
    (global-set-key (kbd "C-c p s") 'cape-elisp-symbol)
    (global-set-key (kbd "C-c p e") 'cape-elisp-block)
    (global-set-key (kbd "C-c p a") 'cape-abbrev)
    (global-set-key (kbd "C-c p l") 'cape-line)
    (global-set-key (kbd "C-c p w") 'cape-dict)
    (global-set-key (kbd "C-c p :") 'cape-emoji)
    (global-set-key (kbd "C-c p \\") 'cape-tex)
    (global-set-key (kbd "C-c p _") 'cape-tex)
    (global-set-key (kbd "C-c p ^") 'cape-tex)
    (global-set-key (kbd "C-c p &") 'ape-sgml)
    (global-set-key (kbd "C-c p r") 'cape-rfc1345))

;; consult
(with-eval-after-load 'consult
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
    (global-set-key (kbd "M-s d") 'consult-find)
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
    (define-key vertico-map (kbd "C-c C-d") 'consult-dir)
    (define-key vertico-map (kbd "C-c C-j") 'consult-dir-jump-file))

(with-eval-after-load 'embark
    (global-set-key (kbd "C-;") 'embark-act)         ;; pick some comfortable binding
    (global-set-key (kbd "M-;") 'embark-dwim)        ;; good alternative: C-M-;
    (global-set-key (kbd "C-h B") 'embark-bindings)) ;; alternative for `describe-bindings'

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

(with-eval-after-load 'magit
    (global-set-key (kbd "C-x g") 'magit-status))

(with-eval-after-load 'org
    (define-key org-mode-map [remap fill-paragraph] 'my--org-fill-or-unfill))

(with-eval-after-load 'markdown-mode
    (define-key markdown-mode-map (kbd "TAB") 'markdown-cycle))

(with-eval-after-load 'eat
    (global-set-key (kbd "C-c o t") 'eat-other-window)
    (global-set-key (kbd "C-c o T") 'eat)
    (define-key project-prefix-map (kbd "t") 'eat-project-other-window)
    (define-key project-prefix-map (kbd "T") 'eat-project))

(with-eval-after-load 'vterm
    (defun my--vterm-project ()
        (interactive)
        (defvar vterm-buffer-name)
        (let* ((default-directory (project-root (project-current t)))
               (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
               (vterm-buffer (get-buffer vterm-buffer-name)))
            (if (and vterm-buffer (not current-prefix-arg))
                    (pop-to-buffer vterm-buffer (bound-and-true-p display-comint-buffer-action))
                (vterm))))
    (defun my--vterm-project-other-window ()
        (interactive)
        (defvar vterm-buffer-name)
        (let* ((default-directory (project-root     (project-current t)))
               (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
               (vterm-buffer (get-buffer vterm-buffer-name)))
            (if (and vterm-buffer (not current-prefix-arg))
                    (pop-to-buffer vterm-buffer (bound-and-true-p display-comint-buffer-action))
                (vterm-other-window))))

    ;; global keys
    (global-set-key (kbd "C-c o t") 'vterm-other-window)
    (global-set-key (kbd "C-c o T") 'vterm)

    ;; project-prefix-map
    (define-key project-prefix-map (kbd "t") 'my--vterm-project-other-window)
    (define-key project-prefix-map (kbd "T") 'my--vterm-project)

    ;; vterm-mode-map
    (define-key vterm-mode-map (kbd "<insert>") 'ignore)
    (define-key vterm-mode-map (kbd "RET")      'vterm-send-return)
    (define-key vterm-mode-map (kbd "C-q")      'vterm-send-next-key)
    (define-key vterm-mode-map (kbd "M-[")      'vterm-copy-mode)
    (define-key vterm-mode-map (kbd "C-y")      'vterm-yank)
    (define-key vterm-mode-map (kbd "C-g")      'vterm-send-escape)

    ;; vterm-copy-mode-map
    (define-key vterm-copy-mode-map (kbd "M-w") 'vterm-copy-mode-done)
    (define-key vterm-copy-mode-map (kbd "C-g") 'vterm-copy-mode-done))

(with-eval-after-load 'dwim-shell-command
    (define-key global-map [remap shell-command] 'dwim-shell-command)
    (define-key global-map [remap async-shell-command] 'dwim-shell-command)
    (define-key dired-mode-map [remap dired-do-shell-command] 'dwim-shell-command)
    (define-key dired-mode-map [remap dired-do-async-shell-command] 'dwim-shell-command)
    (define-key dired-mode-map [remap dired-smart-shell-command] 'dwim-shell-command))

(with-eval-after-load 'helpful
    (define-key global-map [remap describe-function] 'helpful-callable)
    (define-key global-map [remap describe-variable] 'helpful-variable)
    (define-key global-map [remap describe-command]  'helpful-command)
    (define-key global-map [remap describe-key]      'helpful-key)
    (define-key global-map [remap describe-symbol]   'helpful-symbol)
    (global-set-key (kbd "C-c C-d")                  'helpful-at-point)
    (global-set-key (kbd "C-h F")                    'helpful-function)
    (global-set-key (kbd "C-h K")                    'describe-keymap))

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
(global-set-key (kbd "C-c f j") 'evil-collection-consult-jump-list)
(global-set-key (kbd "C-c f m") 'evil-collection-consult-mark)
(global-set-key (kbd "C-c f i") 'consult-imenu)
(global-set-key (kbd "C-c f I") 'consult-imenu-multi)
(global-set-key (kbd "C-c f l") 'consult-line)
(global-set-key (kbd "C-c f L") 'consult-line-multi)

;; C-c w : windows
(global-set-key (kbd "C-c w M") 'maximize-window)
(global-set-key (kbd "C-c w m") 'minimize-window)
(with-eval-after-load 'crux
    (global-set-key (kbd "C-c w w") 'crux-swap-windows))
(with-eval-after-load 'winner
    (global-set-key (kbd "C-c w u") 'winner-undo)
    (global-set-key (kbd "C-c w r") 'winner-redo))

;; C-c c : Code
(global-set-key (kbd "C-c c f") 'my--toggle-fold)
(global-set-key (kbd "C-c c h") 'eldoc)
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

;; C-c n : Notes
(global-set-key (kbd "C-c n d") 'deft-find-file)
(global-set-key (kbd "C-c n D") 'deft)
(global-set-key (kbd "C-c n a") 'org-agenda)

;; C-c o : Open
(global-set-key (kbd "C-c o f") 'elfeed)
(global-set-key (kbd "C-c o p") 'dired-sidebar-toggle-sidebar)
(global-set-key (kbd "C-c o c") 'calc)
(global-set-key (kbd "C-c o i") 'ielm)
(global-set-key (kbd "C-c o e") 'eshell)

;; C-c l : lookup
(global-set-key (kbd "C-c l d") 'dictionary-lookup-definition)

(provide 'my-init-bindings)
;;; my-init-bindings.el ends here

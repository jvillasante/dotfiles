;; my-init-bindings.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;;; Links:
;; https://www.masteringemacs.org/article/mastering-key-bindings-emacs

;; General
(global-set-key (kbd "C-c u") 'browse-url-at-point) ; simple browse url

;; Both `C-z' and `C-x C-z' are bound to `suspend-frame' which will effectively
;; suspend the current frame. To bring back the suspended frame in GUI mode we
;; need to send `SIGCONT' to emacs by `kill -CONT $emacs_pid' or `killall -CONT
;; emacs' while in terminal `fg' or `%emacs' will work just fine.
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; The undo mechanism is weird but powerful, better to learn it well
(global-unset-key (kbd "C-?"))

;; smart kill buffers
(defun my-smart-kill-buffer ()
    "Kill buffers.
When run interactively:
- Without a prefix arg (`C-x C-k`), `kill-current-buffer'.
- With a prefix arg (`C-u C-x C-k`), `kill-buffer'."
    (interactive)
    (if current-prefix-arg
            (call-interactively #'kill-buffer)
        (kill-current-buffer)))
(global-set-key (kbd "C-x k") 'my-smart-kill-buffer)

;; smart kill emacs
(defun my-smart-kill-emacs ()
    "Kill Emacs client or daemon with confirmation.
When run interactively:
- Without a prefix arg (`C-x C-c`), ask to close the client frame.
- With a prefix arg (`C-u C-x C-c`), ask to kill the entire daemon."
    (interactive)
    (if current-prefix-arg
            ;; C-u was pressed: target the whole daemon
            (when (y-or-n-p "Really kill the Emacs daemon? ")
                (save-buffers-kill-emacs))
        ;; No prefix: target just this client/terminal
        (when (y-or-n-p "Really close this Emacs client? ")
            (save-buffers-kill-terminal))))
(when (daemonp)
    ;; Only override C-x C-c when running as a daemon
    (global-set-key (kbd "C-x C-c") #'my-smart-kill-emacs))

;; Emacs29 changes `cycle-spacing' default
(global-set-key [remap cycle-spacing] 'just-one-space)

;; `zap-up-to-char' is just better
(global-set-key [remap zap-to-char] 'zap-up-to-char)

;; Use Ctrl+arrow keys to move between windows.
(windmove-default-keybindings 'control)

;; better `keyword-quit'
(define-key global-map [remap keyboard-quit] 'my-keyboard-quit-dwim)

;; Repeat in emacs is not as good as vim :(
(global-set-key (kbd "C-.") 'repeat)

;; Clone the current buffer in a new window with `q' to exit
(global-set-key (kbd "C-x 9") 'my-clone-buffer-in-new-window-readonly) ; same

;; ctl-x-4-map
(define-key ctl-x-4-map (kbd "T") 'my-toggle-window-split)

;; duplicate current line or region
(global-set-key (kbd "C-x j") #'duplicate-dwim)

;; `delete-forward-char' is preferable for interactive use
(define-key (current-global-map) [remap delete-char] 'delete-forward-char)

;; upcase, downcase and capitalize
(define-key (current-global-map) [remap capitalize-word] 'capitalize-dwim)
(define-key (current-global-map) [remap downcase-word] 'downcase-dwim)
(define-key (current-global-map) [remap upcase-word] 'upcase-dwim)

;; count-words
(define-key (current-global-map) [remap count-words-region] 'count-words)
(global-set-key (kbd "C-M-=") 'count-matches)

;; Open line(s) below/above current one (using crux implementation)
;; (global-set-key (kbd "C-o") 'my-open-next-line)
;; (global-set-key (kbd "M-o") 'my-open-previous-line)

;; better comment/un-comment
;; (global-set-key (kbd "M-;") 'my-comment-or-uncomment)
;; (global-set-key (kbd "C-x C-;") 'my-comment-or-uncomment)
(global-set-key (kbd "C-x C-;") 'comment-dwim)

;; fill-unfill
(define-key global-map [remap fill-paragraph] 'my-fill-or-unfill)
(global-set-key (kbd "M-Q") 'my-unfill-paragraph)
(global-set-key (kbd "C-M-Q") 'my-unfill-region)

;;; Prefix
;; C-c w : windows
(global-set-key (kbd "C-c w m") 'minimize-window)
(global-set-key (kbd "C-c w M") 'maximize-window)
(global-set-key (kbd "C-c w =") 'balance-windows)
(with-eval-after-load 'crux
    (global-set-key (kbd "C-c w w") 'crux-swap-windows))
(with-eval-after-load 'winner
    (global-set-key (kbd "C-c w u") 'winner-undo)
    (global-set-key (kbd "C-c w r") 'winner-redo))
(with-eval-after-load 'tab-bar
    (global-set-key (kbd "M-[") 'tab-bar-history-back)
    (global-set-key (kbd "M-]") 'tab-bar-history-forward))

;; C-c c : Code
(with-eval-after-load 'hs-minor-mode
    (define-key hs-minor-mode-map (kbd "C-c c f") 'my-hs-toggle-hidding))
(with-eval-after-load 'consult-eglot
    (define-key eglot-mode-map (kbd "C-c c s") 'consult-eglot-symbols))
(with-eval-after-load 'eglot
    (global-set-key            (kbd "C-c c Q") 'eglot-shutdown-all)
    (define-key eglot-mode-map (kbd "C-c c q") 'eglot-shutdown)
    (define-key eglot-mode-map (kbd "C-c c R") 'eglot-reconnect)
    (define-key eglot-mode-map (kbd "C-c c m") 'eglot-menu)
    (define-key eglot-mode-map (kbd "C-c c r") 'eglot-rename)
    (define-key eglot-mode-map (kbd "C-c c d") 'eglot-find-declaration)
    (define-key eglot-mode-map (kbd "C-c c .") 'eglot-find-typeDefinition)
    (define-key eglot-mode-map (kbd "C-c c i") 'eglot-find-implementation)
    (define-key eglot-mode-map (kbd "C-c c a") 'eglot-code-actions)
    (define-key eglot-mode-map (kbd "C-c c o") 'eglot-code-action-organize-imports))
(with-eval-after-load 'lsp-mode
    (define-key lsp-mode-map (kbd "C-h .") 'lsp-describe-thing-at-point)
    (define-key lsp-mode-map (kbd "C-c c q") 'lsp-workspace-shutdown)
    (define-key lsp-mode-map (kbd "C-c c R") 'lsp-workspace-restart))
(with-eval-after-load 'flymake
    (define-key flymake-mode-map (kbd "C-c c e") 'flymake-show-buffer-diagnostics)
    (define-key flymake-mode-map (kbd "C-c c E") 'flymake-show-project-diagnostics))

;; C-c o : Open
(when (package-installed-p 'elfeed)
    (global-set-key (kbd "C-c o f") 'elfeed)
    (with-eval-after-load 'elfeed
        (define-key elfeed-show-mode-map (kbd "q") 'my-close-buffer-and-window)
        (define-key elfeed-search-mode-map (kbd "q") 'my-close-buffer-and-window)
        ;; (define-key elfeed-search-mode-map (kbd "t") 'my-elfeed-w3m-open)
        (define-key elfeed-search-mode-map (kbd "w") 'my-elfeed-eww-open)
        (define-key elfeed-search-mode-map (kbd "f") 'my-elfeed-firefox-open)))
(when (package-installed-p 'consult-notes)
    (global-set-key (kbd "C-c o n") 'consult-notes))
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o i") 'ielm)
(global-set-key (kbd "C-c o b") 'my-open-link-at-point-or-minibuffer-with-choice)
(global-set-key (kbd "C-c o m") 'my-new-scratch-buffer-in-markdown)
(global-set-key (kbd "C-c o o") 'my-new-scratch-buffer-in-org)

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

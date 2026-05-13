;;; my-init-shell.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; ielm : elisp shell
(use-package ielm
    :ensure nil ;; emacs built-in
    :defer t
    :hook (ielm-mode . eldoc-mode))

;; eshell : the emacs shell
(use-package eshell
    :ensure nil ;; emacs built-in
    :defer t
    :preface
    (defun my/eshell-other-window ()
        "Open a `eshell' in a new window."
        (interactive)
        (let ((buf (eshell)))
            (switch-to-buffer (other-buffer buf))
            (switch-to-buffer-other-window buf)))

    (defun my/project-eshell-other-window ()
        "Open `project-eshell' in another window."
        (interactive)
        (let* ((default-directory (project-root (project-current t)))
                  (eshell-buffer-name (project-prefixed-buffer-name "eshell"))
                  (buf (eshell)))
            (switch-to-buffer (other-buffer buf))
            (switch-to-buffer-other-window buf)))

    (defun my/eshell-prompt ()
        "Lightweight prompt: shrunk path in shadow + status char."
        (concat
            (propertize (my/shrunk-path) 'face 'shadow)
            (if (zerop eshell-last-command-status) " › " " × ")))

    (defun eshell/up (&optional n)
        "Go up N directories (default 1).  `up 3' is `cd ../../..'.
Eshell passes numeric args as strings, so coerce."
        (let* ((n (cond ((null n) 1)
                      ((stringp n) (string-to-number n))
                      (t n)))
                  (path (apply #'concat (make-list (max n 1) "../"))))
            (eshell/cd path)))

    (defun eshell/ff (pattern &optional dir)
        "Recursively find files matching PATTERN under DIR (default cwd)."
        (find-name-dired (or dir default-directory) pattern))

    (defun eshell/rcd (path)
        "Cd to PATH on the current remote host (auto-prepends current Tramp prefix).
Falls back to plain `cd' when `default-directory' is local."
        (let ((prefix (or (file-remote-p default-directory) "")))
            (eshell/cd (concat prefix path))))

    (defun eshell/tailf (&rest files)
        "Stream `tail -F' on FILES in an async shell buffer (Tramp-aware).
GNU tail prepends `==> filename <==' separators automatically when more
than one file is supplied.  Handles arbitrarily large logs since files
are never loaded — only trailing bytes stream from the remote `tail'
process.  `C-c C-c' to stop.

We replace the buffer's sentinel with `ignore' so the spurious
\"exited abnormally\" message does not appear on SIGINT-induced stops
(common over Tramp where SSH itself exits with 255 when the local
client is interrupted, regardless of what the remote `tail' did)."
        (unless files (error "tailf: At least one file required"))
        ;; Eshell tokenises numeric-looking args as integers; stringify upfront
        ;; so `file-name-nondirectory' and `shell-quote-argument' don't choke.
        (let* ((files (mapcar (lambda (a) (format "%s" a)) files))
                  (buf-name (format "*tail %s%s*"
                                (file-name-nondirectory (car files))
                                (if (cdr files) (format "+%d" (length (cdr files))) ""))))
            (async-shell-command
                (concat "tail -F -n 200 "
                    (mapconcat #'shell-quote-argument files " "))
                buf-name)
            (when-let ((proc (get-buffer-process buf-name)))
                (set-process-sentinel proc #'ignore))
            nil))

    (defun eshell/notify-when-done (&rest cmd)
        "Run CMD asynchronously, desktop-notify when it finishes.
Output buffer is auto-killed on success; kept on non-zero exit for inspection.
Falls back to `message' on systems lacking `notifications-notify'."
        (unless cmd (user-error "notify-when-done: command required"))
        (let* ((command (mapconcat (lambda (a) (format "%s" a)) cmd " "))
                  (start (current-time))
                  (buf (generate-new-buffer (format "*notify %s*" (car cmd))))
                  (proc (start-file-process-shell-command
                            (format "notify-%s" (car cmd)) buf command)))
            (set-process-sentinel proc
                (lambda (p _event)
                    (when (memq (process-status p) '(exit signal))
                        (let ((elapsed (float-time
                                           (time-subtract (current-time) start)))
                                 (status (process-exit-status p)))
                            (if (fboundp 'notifications-notify)
                                (notifications-notify
                                    :title "eshell"
                                    :body (format "%s\nfinished in %.1fs (exit %d)"
                                              command elapsed status))
                                (message "eshell: %s finished in %.1fs (exit %d)"
                                    command elapsed status))
                            (when (and (zerop status)
                                      (buffer-live-p (process-buffer p)))
                                (kill-buffer (process-buffer p)))))))
            nil))

    (defun my/eshell-per-host-history ()
        "Use a per-host history file when in a Tramp-remote dir.
NOTE: history is bound at eshell-mode start; cd'ing across hosts in a
single buffer does NOT swap files.  Use a fresh eshell per host for
clean separation."
        (when-let* ((host (file-remote-p default-directory 'host))
                       (dir (file-name-directory eshell-history-file-name)))
            (setq-local eshell-history-file-name
                (expand-file-name (format "history-%s" host) dir))))

    (defun my/eshell-extra-capfs ()
        "Append cape-history and cape-elisp-symbol after pcomplete.
Pcomplete (added by eshell itself) handles arg/path completion first;
these CAPFs fall through for whole-line history matches and elisp
symbol completion at the prompt."
        (add-hook 'completion-at-point-functions #'cape-history t t)
        (add-hook 'completion-at-point-functions #'cape-elisp-symbol t t))

    (defun my/eshell-please ()
        "Replace input with `sudo PREV-CMD' and submit.  Bash equivalent of `sudo !!'."
        (interactive)
        (if (or (not (boundp 'eshell-input-ring))
                (ring-empty-p eshell-input-ring))
            (user-error "No previous command")
            (let ((prev (ring-ref eshell-input-ring 0)))
                (eshell-bol)
                (delete-region (point) (point-max))
                (insert "sudo " prev)
                (eshell-send-input))))
    :bind (("C-c o e" . eshell)
              ("C-c o E" . my/eshell-other-window)
              :map project-prefix-map
              ("E" . my/project-eshell-other-window))
    :hook (eshell-mode . (lambda ()
                             (set (make-local-variable 'debug-on-error) nil)
                             (my/eshell-per-host-history)
                             (my/eshell-extra-capfs)))
    :config
    ;; `M-r' as fzf-style history search (overrides em-hist's default which
    ;; is a regex-only `eshell-previous-matching-input').
    (with-eval-after-load 'em-hist
        (define-key eshell-hist-mode-map (kbd "M-r") #'consult-history))
    ;; `eshell-mode-map' lives in `esh-mode.el', NOT in `eshell.el', so binding
    ;; it under use-package's `:bind :map' fails (eshell.el loads first; the
    ;; map is still void).  Defer to esh-mode load instead.
    (with-eval-after-load 'esh-mode
        ;; Output-block navigation (treat each prompt+output as a section)
        (define-key eshell-mode-map (kbd "C-c C-p") #'eshell-previous-prompt)
        (define-key eshell-mode-map (kbd "C-c C-n") #'eshell-next-prompt)
        (define-key eshell-mode-map (kbd "C-c C-o") #'eshell-kill-output)
        (define-key eshell-mode-map (kbd "C-c M-o") #'eshell-show-output)
        ;; `sudo !!' equivalent — re-run previous command with sudo prefix
        (define-key eshell-mode-map (kbd "C-c C-r") #'my/eshell-please))
    :custom
    (eshell-aliases-file (expand-file-name "eshell/aliases" my/etc-dir))
    ;; --- prompt ---
    (eshell-prompt-function #'my/eshell-prompt)
    (eshell-prompt-regexp "^.*[›×] ")  ;; must match prompt's terminator chars
    (eshell-highlight-prompt nil)      ;; let our face properties show through
    (eshell-banner-message "")         ;; suppress welcome blurb
    ;; --- scrolling ---
    (eshell-scroll-to-bottom-on-input 'this)
    (eshell-scroll-to-bottom-on-output nil)
    ;; --- behaviour ---
    (eshell-prefer-lisp-functions nil) ;; prefer external `ls' etc. over eshell's lisp impls
    (eshell-error-if-no-glob t)        ;; fail loudly when a glob matches nothing
    (eshell-glob-case-insensitive t)   ;; *.MD matches *.md
    (eshell-destroy-buffer-when-process-dies t)
    ;; --- history ---
    (eshell-history-size 500000)
    (eshell-hist-ignoredups 'erase)        ;; remove earlier dupes too, not just adjacent
    (eshell-input-filter-initial-space t)  ;; bash HISTCONTROL=ignorespace equivalent
    (eshell-save-history-on-exit t)
    ;; --- visual commands (spawned in term-mode for full TUI support) ---
    (eshell-visual-commands
        '("top" "htop" "btop" "nvtop"
             "vi" "vim" "nvim"
             "less" "more" "watch"
             "ssh" "tail" "lynx" "ranger" "tmux"
             "nmtui"))
    ;; subcommands of otherwise non-visual commands that ARE visual
    (eshell-visual-subcommands '(("git" "log" "diff" "show"))))

;; pcmpl-args : rich pcomplete arg completion for git/find/tar/ssh/curl/...
(use-package pcmpl-args
    :after eshell)

;; with-editor : route $EDITOR (e.g. `git commit') back to an Emacs buffer.
;; Only wired into eshell — shell-mode uses the `EDITOR=emacsclient' value
;; from `~/.bashrc' directly, which avoids the loud `export EDITOR=...'
;; banner with-editor would otherwise inject at session start.
(use-package with-editor
    :hook (eshell-mode . with-editor-export-editor))

;; shell : shell in emacs
(use-package shell
    :ensure nil ;; emacs built-in
    :preface
    (defun my/shell-other-window ()
        "Open a `shell' in a new window."
        (interactive)
        (let ((buf (shell)))
            (switch-to-buffer (other-buffer buf))
            (switch-to-buffer-other-window buf)))
    :bind (("C-c o s" . shell)
              ("C-c o S" . my/shell-other-window))
    :hook (shell-mode . compilation-shell-minor-mode) ;; M-g M-n on errors in build output
    :custom
    ;; --- shell binary (pin explicitly so behaviour is consistent) ---
    (explicit-shell-file-name "/bin/bash")
    (shell-file-name "/bin/bash")
    ;; --- prompt detection: match the shrunk-path prompt set in `Common/shell/system/custom'.
    ;; Used by `comint-bol', `comint-previous-prompt', etc.  ANSI escape codes
    ;; in the prompt are kept as text properties or literal bytes; `^.*' matches
    ;; through them either way.
    (shell-prompt-pattern "^.*[›×] ")
    ;; --- comint hygiene (apply to all comint-derived modes, including shell) ---
    (comint-prompt-read-only t)            ;; protect past prompts from accidental edits
    (comint-input-ignoredups t)            ;; dedupe adjacent history entries
    (comint-completion-autolist t)         ;; show completions on first TAB, not second
    (comint-completion-addsuffix t)        ;; add `/' for dirs, ` ' for executables
    (comint-history-isearch 'dwim)         ;; M-r does isearch through history
    (comint-scroll-to-bottom-on-input t)   ;; typing brings you to the bottom
    (comint-scroll-show-maximum-output t)) ;; keep cursor at bottom on output

;; bash-completion : full bash-aware TAB in shell-mode (git/systemctl/kubectl/...)
(use-package bash-completion
    :hook (shell-mode . bash-completion-setup))

;; eat: Emulate A Terminal (https://codeberg.org/akib/emacs-eat)
(use-package eat
    :disabled t
    :defer t
    :preface
    (defun my/eat-open (file)
        "Helper function to open files from eat terminal."
        (interactive)
        (if (file-exists-p file)
            (find-file-other-window file t)
            (warn "File doesn't exist")))
    :hook ((eshell-mode . eat-eshell-mode)
              (eat-exit . (lambda (&rest _) (kill-buffer-and-window))))
    :bind (("C-c o t" . eat)
              ("C-c o T" . eat-other-window)
              :map project-prefix-map
              ("t" . eat-project)
              ("T" . eat-project-other-window))
    :config
    (add-to-list 'project-switch-commands '(eat-project "Eat terminal") t)
    (add-to-list 'project-kill-buffer-conditions '(major-mode . eat-mode))
    (add-to-list 'eat-message-handler-alist (cons "open" #'my/eat-open))
    (setq eat-term-name "xterm-256color") ; https://codeberg.org/akib/emacs-eat/issues/119"
    (setq eat-kill-buffer-on-exit t)
    (setq eat-term-scrollback-size 500000)
    (setq eat-shell-prompt-annotation-failure-margin-indicator "")
    (setq eat-shell-prompt-annotation-running-margin-indicator "")
    (setq eat-shell-prompt-annotation-success-margin-indicator ""))

;; ghostel : Emacs terminal emulator powered by libghostty-vt
(use-package ghostel
    :disabled t
    :defer t
    :preface
    (defun my/ghostel-other-window ()
        "Open a `ghostel' terminal in another window."
        (interactive)
        (display-buffer-override-next-command
            (lambda (buffer alist)
                (cons (display-buffer-pop-up-window buffer alist) 'window)))
        (ghostel))
    (defun my/ghostel-project-other-window ()
        "Open a `ghostel' terminal in the project root in another window."
        (interactive)
        (display-buffer-override-next-command
            (lambda (buffer alist)
                (cons (display-buffer-pop-up-window buffer alist) 'window)))
        (ghostel-project))
    :config
    (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
    :bind (("C-c o t" . ghostel)
              ("C-c o T" . my/ghostel-other-window)
              :map ghostel-readonly-mode-map
              ("<return>" . ghostel-readonly-exit)
              ("RET"      . ghostel-readonly-exit)
              :map ghostel-mode-map
              ("M-[" . ghostel-copy-mode)
              :map project-prefix-map
              ("t" . ghostel-project)
              ("T" . my/ghostel-project-other-window))
    :custom
    (ghostel-module-auto-install 'download)  ; What to do when the native module is missing
    (ghostel-shell-integration t)            ; Automatically inject shell integration on startup
    (ghostel-readonly-fast-exit nil)         ; Don't exit copy mode automatically
    (ghostel-tramp-shell-integration nil)    ; Inject shell integration for remote TRAMP sessions
    (ghostel-password-prompt-functions nil)  ; Don't try to handle passwords
    (ghostel-tramp-shells
        '(("ssh" login-shell)
             ("scp" login-shell)
             ("docker" "/bin/bash")
             ("podman" "/bin/bash"))))

;; vterm : fully-fledged terminal emulator inside GNU emacs
(use-package vterm
    :defer t
    :preface
    (defun my/vterm-copy-mode-cancel ()
        (interactive)
        (vterm-copy-mode -1))
    (defun my/vterm-project ()
        (interactive)
        (defvar vterm-buffer-name)
        (let* ((default-directory (project-root (project-current t)))
                  (vterm-buffer-name (project-prefixed-buffer-name "vterm")))
            (vterm)))
    (defun my/vterm-project-other-window ()
        (interactive)
        (defvar vterm-buffer-name)
        (let* ((default-directory (project-root (project-current t)))
                  (vterm-buffer-name (project-prefixed-buffer-name "vterm")))
            (vterm-other-window)))
    :bind (("C-c o t" . vterm)
              ("C-c o T" . vterm-other-window)
              :map vterm-copy-mode-map
              ("<return>" . my/vterm-copy-mode-cancel)
              ("RET"      . my/vterm-copy-mode-cancel)
              :map vterm-mode-map
              ("<insert>" . ignore)
              ("C-g"      . vterm-send-escape)
              ("M-["      . vterm-copy-mode)
              ("C-q"      . vterm-send-next-key)
              :map project-prefix-map
              ("t" . my/vterm-project)
              ("T" . my/vterm-project-other-window))
    :config
    (add-to-list 'vterm-eval-cmds '("find-file-other-window" find-file-other-window))
    (add-to-list 'vterm-eval-cmds '("dired" dired))
    (add-to-list 'vterm-eval-cmds '("dired-other-window" dired-other-window))
    (add-to-list 'project-switch-commands '(my/vterm-project "vTerm") t)
    (add-to-list 'project-kill-buffer-conditions '(major-mode . vterm-mode))
    (setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=On -DCMAKE_BUILD_TYPE=Release")
    (setq vterm-always-compile-module nil)
    (setq vterm-timer-delay 0.01)
    (setq vterm-copy-exclude-prompt t)
    (setq vterm-copy-mode-remove-fake-newlines t)
    (setq vterm-kill-buffer-on-exit t)
    (setq vterm-max-scrollback 500000)
    (setq vterm-shell (executable-find "bash"))
    (setq vterm-tramp-shells '(("ssh" login-shell)
                                  ("scp" login-shell)
                                  ("docker" "/bin/bash")
                                  ("podman" "/bin/bash"))))

(provide 'my-init-shell)
;;; my-init-shell.el ends here

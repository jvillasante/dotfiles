;;; private/my-eshell/config.el -*- lexical-binding: t; -*-

(setenv "PAGER" "cat")

;; Git
(defun eshell/gst (&rest args)
    (magit-status (pop args) nil)
    (eshell/echo))   ;; The echo command suppresses output

;; Find File
(defun eshell/f (filename &optional dir try-count)
    "Searches for files matching FILENAME in either DIR or the
current directory. Just a typical wrapper around the standard
`find' executable.

Since any wildcards in FILENAME need to be escaped, this wraps the shell command.

If not results were found, it calls the `find' executable up to
two more times, wrapping the FILENAME pattern in wildcat
matches. This seems to be more helpful to me."
    (let* ((cmd (concat
                    (executable-find "find")
                    " " (or dir ".")
                    "      -not -path '*/.git*'"
                    " -and -not -path '*node_modules*'"
                    " -and -not -path '*classes*'"
                    " -and "
                    " -type f -and "
                    "-iname '" filename "'"))
              (results (shell-command-to-string cmd)))

        (if (not (s-blank-str? results))
            results
            (cond
                ((or (null try-count) (= 0 try-count))
                    (eshell/f (concat filename "*") dir 1))
                ((or (null try-count) (= 1 try-count))
                    (eshell/f (concat "*" filename) dir 2))
                (t "")))))

(defun eshell/ef (filename &optional dir)
    "Searches for the first matching filename and loads it into a
file to edit."
    (let* ((files (eshell/f filename dir))
              (file (car (s-split "\n" files))))
        (find-file file)))

(defun eshell/find (&rest args)
    "Wrapper around the ‘find’ executable."
    (let ((cmd (concat "find " (string-join args))))
        (shell-command-to-string cmd)))

;; Clear
(defun eshell/clear ()
    "Clear the eshell buffer."
    (let ((inhibit-read-only t))
        (erase-buffer)
        (eshell-send-input)))

;; Special Prompt
(defun curr-dir-git-branch-string (pwd)
    "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
    (interactive)
    (when (and (not (file-remote-p pwd))
              (eshell-search-path "git")
              (locate-dominating-file pwd ".git"))
        (let* ((git-url (shell-command-to-string "git config --get remote.origin.url"))
                  (git-repo (file-name-base (s-trim git-url)))
                  (git-output (shell-command-to-string (concat "git rev-parse --abbrev-ref HEAD")))
                  (git-branch (s-trim git-output))
                  (git-icon  "\xe0a0")
                  (git-icon2 (propertize "\xf020" 'face `(:family "octicons"))))
            (concat git-repo " " git-icon2 " " git-branch))))

(defun pwd-replace-home (pwd)
    "Replace home in PWD with tilde (~) character."
    (interactive)
    (let* ((home (expand-file-name (getenv "HOME")))
              (home-len (length home)))
        (if (and
                (>= (length pwd) home-len)
                (equal home (substring pwd 0 home-len)))
            (concat "~" (substring pwd home-len))
            pwd)))

(defun pwd-shorten-dirs (pwd)
    "Shorten all directory names in PWD except the last two."
    (let ((p-lst (split-string pwd "/")))
        (if (> (length p-lst) 2)
            (concat
                (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                                             (substring elm 0 1)))
                    (butlast p-lst 2)
                    "/")
                "/"
                (mapconcat (lambda (elm) elm)
                    (last p-lst 2)
                    "/"))
            pwd)))  ;; Otherwise, we just return the PWD

(defun split-directory-prompt (directory)
    (if (string-match-p ".*/.*" directory)
        (list (file-name-directory directory) (file-name-base directory))
        (list "" directory)))

(defun ruby-prompt ()
    "Returns a string (may be empty) based on the current Ruby Virtual Environment."
    (let* ((executable "~/.rvm/bin/rvm-prompt")
              (command    (concat executable "v g")))
        (when (file-exists-p executable)
            (let* ((results (shell-command-to-string executable))
                      (cleaned (string-trim results))
                      (gem     (propertize "\xe92b" 'face `(:family "alltheicons"))))
                (when (and cleaned (not (equal cleaned "")))
                    (s-replace "ruby-" gem cleaned))))))

(defun python-prompt ()
    "Returns a string (may be empty) based on the current Python
   Virtual Environment. Assuming the M-x command: `pyenv-mode-set'
   has been called."
    (when (fboundp #'pyenv-mode-version)
        (let ((venv (pyenv-mode-version)))
            (when venv
                (concat
                    (propertize "\xe928" 'face `(:family "alltheicons"))
                    (pyenv-mode-version))))))

(defun eshell/eshell-local-prompt-function ()
    "A prompt for eshell that works locally (in that is assumes
that it could run certain commands) in order to make a prettier,
more-helpful local prompt."
    (interactive)
    (let* ((pwd        (eshell/pwd))
              (directory (split-directory-prompt
                             (pwd-shorten-dirs
                                 (pwd-replace-home pwd))))
              (parent (car directory))
              (name   (cadr directory))
              (branch (curr-dir-git-branch-string pwd))
              (ruby   (when (not (file-remote-p pwd)) (ruby-prompt)))
              (python (when (not (file-remote-p pwd)) (python-prompt)))

              (dark-env (eq 'dark (frame-parameter nil 'background-mode)))
              (for-bars                 `(:weight bold))
              (for-parent  (if dark-env `(:foreground "dark orange") `(:foreground "blue")))
              (for-dir     (if dark-env `(:foreground "orange" :weight bold)
                               `(:foreground "blue" :weight bold)))
              (for-git                  `(:foreground "green"))
              (for-ruby                 `(:foreground "red"))
              (for-python               `(:foreground "#5555FF")))

        (concat
            (propertize "⟣─ "    'face for-bars)
            (propertize parent   'face for-parent)
            (propertize name     'face for-dir)
            (when branch
                (concat (propertize " ── "    'face for-bars)
                    (propertize branch   'face for-git)))
            (when ruby
                (concat (propertize " ── " 'face for-bars)
                    (propertize ruby   'face for-ruby)))
            (when python
                (concat (propertize " ── " 'face for-bars)
                    (propertize python 'face for-python)))
            (propertize "\n"     'face for-bars)
            (propertize (if (= (user-uid) 0) " #" " $") 'face `(:weight ultra-bold))
            ;; (propertize " └→" 'face (if (= (user-uid) 0) `(:weight ultra-bold :foreground "red") `(:weight ultra-bold)))
            (propertize " "    'face `(:weight bold)))))

(after! eshell
    (setq eshell-highlight-prompt nil)
    (setq-default eshell-prompt-function #'eshell/eshell-local-prompt-function)

    (setq ;; eshell-buffer-shorthand t ...  Can't see Bug#19391
        eshell-scroll-to-bottom-on-input 'all
        eshell-error-if-no-glob t
        eshell-hist-ignoredups t
        eshell-save-history-on-exit t
        eshell-prefer-lisp-functions nil
        eshell-destroy-buffer-when-process-dies t)

    ;; Visual Executables
    (add-hook 'eshell-mode-hook
        (lambda ()
            (add-to-list 'eshell-visual-commands "ssh")
            (add-to-list 'eshell-visual-commands "tail")
            (add-to-list 'eshell-visual-commands "top")))

    ;; Aliases
    (add-hook 'eshell-mode-hook
        (lambda ()
            (eshell/alias "e" "find-file $1")
            (eshell/alias "ff" "find-file $1")
            (eshell/alias "emacs" "find-file $1")
            (eshell/alias "ee" "find-file-other-window $1")

            (eshell/alias "gd" "magit-diff-unstaged")
            (eshell/alias "gds" "magit-diff-staged")
            (eshell/alias "d" "dired $1")

            ;; The 'ls' executable requires the Gnu version on the Mac
            (let ((ls (if (file-exists-p "/usr/local/bin/gls")
                          "/usr/local/bin/gls"
                          "/bin/ls")))
                (eshell/alias "ll" (concat ls " -AlohG --color=always"))))))

;;; init.el --- Init -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; set EDITOTR to `emacsclient' as early as possible
(setf (getenv "EDITOR") "emacsclient")

;;; Networking

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;;; package.el
(when (bound-and-true-p my/package-initialize-and-refresh)
    ;; Initialize and refresh package contents again if needed
    (package-initialize)
    (unless package-archive-contents
        (package-refresh-contents))

    ;; Install use-package if necessary
    (unless (package-installed-p 'use-package)
        (package-install 'use-package))

    ;; Ensure use-package is available at compile time
    (eval-when-compile
        (require 'use-package)))

;; Ensure the 'use-package' package is installed and loaded
(use-package use-package
    :ensure nil ;; emacs built-in
    :custom ((use-package-verbose t)
             (use-package-always-ensure t)
             (use-package-expand-minimally t)))

;; exec-path-from-shell : Sane environment variables
(use-package exec-path-from-shell
    :init
    (when (daemonp)
        (exec-path-from-shell-initialize)
        (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"
                       "CARGO_HOME" "GOPATH" "GOBIN" "NIX_SSL_CERT_FILE" "NIX_PATH" "VCPKG_ROOT"))
            (exec-path-from-shell-copy-env var))))

;;; Features, warnings, and errors

;; Disable warnings from the legacy advice API. They aren't useful.
(setq ad-redefinition-action 'accept)
(setq warning-suppress-types '((lexical-binding)))

;; Some features that are not represented as packages can be found in
;; `features', but this can be inconsistent. The following enforce consistency:
(if (fboundp #'json-parse-string)
        (push 'jansson features))
(if (string-match-p "HARFBUZZ" system-configuration-features) ; no alternative
        (push 'harfbuzz features))
(if (bound-and-true-p module-file-suffix)
        (push 'dynamic-modules features))

;;; Minibuffer
;; Allow nested minibuffers
(setq enable-recursive-minibuffers t)

;; Keep the cursor out of the read-only portions of the.minibuffer
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face
                  minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;; User interface

;; By default, Emacs "updates" its ui more often than it needs to
(setq idle-update-delay 1.0)

;; Allow for shorter responses: "y" for yes and "n" for no.
(if (boundp 'use-short-answers)
        (setq use-short-answers t)
    (advice-add #'yes-or-no-p :override #'y-or-n-p))
(defalias #'view-hello-file #'ignore)  ; Never show the hello file

;;; Misc

;; switch-to-buffer runs pop-to-buffer-same-window instead
(setq switch-to-buffer-obey-display-actions t)

(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

(setq whitespace-line-column nil)  ; whitespace-mode

;; I reduced the default value of 9 to simplify the font-lock keyword,
;; aiming to improve performance. This package helps differentiate
;; nested delimiter pairs, particularly in languages with heavy use of
;; parentheses.
(setq rainbow-delimiters-max-face-count 5)

(setq comint-prompt-read-only t)
(setq comint-buffer-maximum-size 2048)

(setq compilation-always-kill t
      compilation-ask-about-save nil
      compilation-scroll-output 'first-error)

(setq truncate-string-ellipsis "…")

;; Delete by moving to trash in interactive mode
(setq delete-by-moving-to-trash (not noninteractive))

;; Increase how much is read from processes in a single chunk
(setq read-process-output-max (* 512 1024))  ; 512kb

;; Collects and displays all available documentation immediately, even if
;; multiple sources provide it. It concatenates the results.
(setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)

;; For some reason, `abbrev_defs` is located in ~/.emacs.d/abbrev_defs, even
;; when `user-emacs-directory` is modified. This ensures the abbrev file is
;; correctly located based on the updated `user-emacs-directory`.
(setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))

;;; Files

;; Disable the warning "X and Y are the same file". Ignoring this warning is
;; acceptable since it will redirect you to the existing buffer regardless.
(setq find-file-suppress-same-file-warnings t)

;; Resolve symlinks when opening files, so that any operations are conducted
;; from the file's true directory (like `find-file').
(setq find-file-visit-truename t
      vc-follow-symlinks t)

;; Skip confirmation prompts when creating a new file or buffer
(setq confirm-nonexistent-file-or-buffer nil)

(setq uniquify-buffer-name-style 'forward)

(setq mouse-yank-at-point t)

;; Prefer vertical splits over horizontal ones
(setq split-width-threshold 170
      split-height-threshold nil)

;; The native border "uses" a pixel of the fringe on the rightmost
;; splits, whereas `window-divider` does not.
(setq window-divider-default-bottom-width 1
      window-divider-default-places t
      window-divider-default-right-width 1)

;;; Auto revert
;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(setq revert-without-query (list ".")  ; Do not prompt
      auto-revert-stop-on-user-input nil
      auto-revert-verbose t)

;; Revert other buffers (e.g, Dired)
(setq global-auto-revert-non-file-buffers t)

;;; Frames and windows

;;; Mouse Scroll

;;; Cursor
;; The blinking cursor is distracting and interferes with cursor settings in
;; some minor modes that try to change it buffer-locally (e.g., Treemacs).
;; Additionally, it can cause freezing, especially on macOS, for users with
;; customized and colored cursors.
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)

;; Reduce rendering/line scan work by not rendering cursors or regions in
;; non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;;; Annoyances

;; No beeping or blinking
(setq visible-bell nil)
(setq ring-bell-function #'ignore)

;; This controls how long Emacs will blink to show the deleted pairs with
;; `delete-pair'. A longer delay can be annoying as it causes a noticeable pause
;; after each deletion, disrupting the flow of editing.
(setq delete-pair-blink-delay 0.03)

;; Ensure window-start is never invisible. This enhances user experience when
;; folding/unfolding code (outline, org-mode, outline-minor-mode...)
(setq-default make-window-start-visible t)

;;; Indent and formatting
(setq-default left-fringe-width  8)
(setq-default right-fringe-width 8)

;; Do not show an arrow at the top/bottomin the fringe and empty lines
(setq-default indicate-buffer-boundaries nil)
(setq-default indicate-empty-lines nil)

;; Continue wrapped lines at whitespace rather than breaking in the
;; middle of a word.
(setq-default word-wrap t)

;; Disable wrapping by default due to its performance cost.
(setq-default truncate-lines t)

;; If enabled and `truncate-lines' is disabled, soft wrapping will not occur
;; when the window is narrower than `truncate-partial-width-windows' characters.
(setq truncate-partial-width-windows nil)

;; Prefer spaces over tabs. Spaces offer a more consistent default compared to
;; 8-space tabs. This setting can be adjusted on a per-mode basis as needed.
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Enable indentation and completion using the TAB key
(setq-default tab-always-indent nil)

;; Enable multi-line commenting which ensures that `comment-indent-new-line'
;; properly continues comments onto new lines, which is useful for writing
;; longer comments or docstrings that span multiple lines.
(setq comment-multi-line t)

;; We often split terminals and editor windows or place them side-by-side,
;; making use of the additional horizontal space.
(setq-default fill-column 80)

;; Disable the obsolete practice of end-of-line spacing from the
;; typewriter era.
(setq sentence-end-double-space nil)

;; According to the POSIX, a line is defined as "a sequence of zero or
;; more non-newline characters followed by a terminating newline".
(setq require-final-newline t)

;; Remove duplicates from the kill ring to reduce clutter
(setq kill-do-not-save-duplicates t)

;; Ensures that empty lines within the commented region are also commented out.
;; This prevents unintended visual gaps and maintains a consistent appearance,
;; ensuring that comments apply uniformly to all lines, including those that are
;; otherwise empty.
(setq comment-empty-lines t)

;; Eliminate delay before highlighting search matches
(setq lazy-highlight-initial-delay 0)

;;; Mode line

;; Setting `display-time-default-load-average' to nil makes Emacs omit the load
;; average information from the mode line.
(setq display-time-default-load-average nil)

;; Display the current line and column numbers in the mode line
(setq line-number-mode t)
(setq column-number-mode t)

;;; Filetype

;; Do not notify the user each time Python tries to guess the indentation offset
(setq python-indent-guess-indent-offset-verbose nil)

(setq sh-indent-after-continuation 'always)

;;; Font / Text scale

;; Avoid automatic frame resizing when adjusting settings.
(setq global-text-scale-adjust-resizes-frames nil)

;;; Ediff

;; Configure Ediff to use a single frame and split windows horizontally
(setq ediff-window-setup-function #'ediff-setup-windows-plain
      ediff-split-window-function #'split-window-horizontally)

;;; User stuff
(use-package compile-angel
    :demand t
    :custom
    (compile-angel-verbose nil)
    :config
    ;; (setq compile-angel-excluded-files-regexps '("/\\.dir-config\\.el$"
    ;;                                              "/\\.dir-locals\\.el$"))
    (compile-angel-on-load-mode)
    (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode))

;; Hide warnings and display only errors
(setq warning-minimum-level :error)

(display-time-mode)
(show-paren-mode +1)  ; Paren match highlighting
(winner-mode 1)

;; Configure Emacs to ask for confirmation before exiting
(setq confirm-kill-emacs 'y-or-n-p)

(use-package uniquify
    :ensure nil
    :custom
    (uniquify-buffer-name-style 'reverse)
    (uniquify-separator " - ")
    (uniquify-after-kill-buffer-p t)
    (uniquify-ignore-buffers-re "^\\*"))

;; Window dividers separate windows visually. Window dividers are bars that can
;; be dragged with the mouse, thus allowing you to easily resize adjacent
;; windows.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Window-Dividers.html
(add-hook 'after-init-hook #'window-divider-mode)

;; Paths
(defconst my/home-path (expand-file-name "~/"))
(defconst my/dotfiles-path (expand-file-name "Workspace/Public/dotfiles/" my/home-path))
(defconst my/software-path (expand-file-name "Workspace/Software/" my/home-path))
(defconst my/dropbox-path (expand-file-name "Dropbox/" my/home-path))

;; load config
(load (expand-file-name "Common/emacs/elisp/my-utils"              my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-early"         my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-completion"    my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-vcs"           my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-org"           my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-langs"         my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-lang-tools"    my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-docker"        my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-apps"          my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-shell"         my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-misc"          my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-filemanager"   my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-workspaces"    my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-modal"         my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-ui"            my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-ai"            my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/my-init-bindings"      my/dotfiles-path))
;; (load (expand-file-name "Common/emacs/elisp/my-init-transient"     my/dotfiles-path))
(load (expand-file-name "Common/emacs/elisp/modus-themes-exporter" my/dotfiles-path))

(provide 'init)

;;; init.el ends here

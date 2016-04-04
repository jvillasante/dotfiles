(setq jvillasante-packages
      '(
        auto-highlight-symbol
        auto-yasnippet
        avy
        ;; asm-mode
        bookmark+
        ;; caps-lock
        company
        corral
        cc-mode
        ;; csharp-mode
        clean-aindent-mode
        dtrt-indent
        describe-number
        eyebrowse
        eshell
        eww
        firestarter
        flycheck
        flyspell-lazy
        goto-chg
        magit
        ;; nasm-mode
        ;; helm-gtags
        ;; ggtags
        shell-pop
        slime
        persp-mode
        puppet-mode
        popwin
        irony
        lispy
        company-irony
        flycheck-irony
        semantic
        skeletor
        ;; srecode
        region-state
        sx
        org
        ;; omnisharp
        ;; pdf-tools
        yasnippet
        vhdl-mode
        quickrun
        ;; wiki-summary
        ;; x86-lookup
        ))

;; List of packages to exclude.
(setq jvillasante-excluded-packages '())

(defun jvillasante/post-init-avy ()
  (use-package avy
    :config
    (define-key isearch-mode-map (kbd "C-'") 'avy-isearch)

    (global-set-key (kbd "M-g l") 'avy-goto-line)
    (global-set-key (kbd "M-g d") 'avy-copy-line)
    (global-set-key (kbd "M-g r") 'avy-copy-region)
    (global-set-key (kbd "M-g w") 'avy-goto-word-1)
    (global-set-key (kbd "M-c") 'avy-goto-char-in-line)

    (defun avy-goto-conditional ()
      (interactive)
      (avy--generic-jump "\\s(\\(if\\|cond\\|when\\|unless\\)\\b" nil 'pre))
    (defun avy-goto-loop ()
      (interactive)
      (avy--generic-jump "\\s(\\(loop\\|while\\|do\\|for\\)\\b" nil 'pre))
    (defun avy-goto-def ()
      (interactive)
      (avy--generic-jump "\\s(\\(def\\)" nil 'pre))
    (defun avy-goto-set ()
      (interactive)
      (avy--generic-jump "\\s(\\(set\\)" nil 'pre))

    (global-set-key (kbd "M-g M-c") 'avy-goto-conditional)
    (global-set-key (kbd "M-g M-l") 'avy-goto-loop)
    (global-set-key (kbd "M-g M-d") 'avy-goto-def)
    (global-set-key (kbd "M-g M-s") 'avy-goto-set)

    (evil-leader/set-key
      "SPC" 'avy-goto-word-or-subword-1)))

(defun jvillasante/init-lispy ()
  (use-package lispy
    :init
    (add-hook 'emacs-lisp-mode-hook #'lispy-mode)
    (add-hook 'lisp-mode-hook #'lispy-mode)
    (add-hook 'scheme-mode-hook #'lispy-mode)
    (setq lispy-completion-method 'helm)
    (with-eval-after-load 'lispy
      (define-key lispy-mode-map-lispy (kbd "C-a") 'spacemacs/smart-move-beginning-of-line)
      (define-key lispy-mode-map-lispy (kbd "C-k") 'sp-kill-hybrid-sexp)
      (define-key lispy-mode-map-lispy (kbd "C-c [") 'lispy-backward)
      (define-key lispy-mode-map-lispy (kbd "C-c ]") 'lispy-forward)
      (define-key lispy-mode-map-lispy (kbd "M-p") 'ahs-backward)
      (define-key lispy-mode-map-lispy (kbd "M-n") 'ahs-forward)
      (define-key lispy-mode-map-lispy (kbd "M-e") 'er/expand-region)
      (define-key lispy-mode-map-lispy (kbd "M-i") 'lispy-iedit)
      (define-key lispy-mode-map-lispy (kbd "M-o") 'describe-number-at-point)
      (define-key lispy-mode-map-lispy (kbd "[") 'self-insert-command)
      (define-key lispy-mode-map-lispy (kbd "]") 'self-insert-command)
      (define-key lispy-mode-map-c-digits (kbd "C-9") 'corral-parentheses-backward)
      (lispy-define-key lispy-mode-map-special (kbd "j") 'lispy-backward)
      (lispy-define-key lispy-mode-map-special (kbd "l") 'lispy-forward)
      (lispy-define-key lispy-mode-map (kbd "I") 'lispy-knight-up)
      (lispy-define-key lispy-mode-map (kbd "K") 'lispy-knight-down)
      (lispy-define-key lispy-mode-map-special (kbd "k") 'lispy-down)
      (lispy-define-key lispy-mode-map-special (kbd "i") 'lispy-up)
      (lispy-define-key lispy-mode-map-special (kbd "j") 'lispy-ace-symbol)
      (lispy-define-key lispy-mode-map-special (kbd "h") 'lispy-tab))))

(defun jvillasante/post-init-clean-aindent-mode ()
  (add-hook 'prog-mode-hook 'clean-aindent-mode))

(defun jvillasante/post-init-company ()
  ;; Nicer looking faces for company
  (with-eval-after-load 'company
    (let* ((default-bg-str (face-attribute 'default :background))
           (default-bg-str (spacemacs//rgb-add default-bg-str -0.04)))
      (set-face-attribute 'company-scrollbar-bg nil
                          :background (face-attribute 'font-lock-comment-face :foreground))
      (set-face-attribute 'company-scrollbar-fg nil
                          :background (face-attribute 'font-lock-builtin-face :foreground))
      (set-face-attribute 'company-tooltip-selection nil
                          :background (face-attribute 'highlight :background))
      (set-face-attribute 'company-tooltip-common nil
                          :foreground (face-attribute 'font-lock-comment-face :foreground)
                          :weight 'bold
                          :background default-bg-str
                          :underline nil)
      ;; just disable the below face, it's redundant
      (set-face-attribute 'company-tooltip-common-selection nil
                          :foreground (face-attribute 'font-lock-keyword-face :foreground)
                          :background default-bg-str
                          :weight 'bold
                          :underline nil)
      (set-face-attribute 'company-tooltip-annotation nil
                          :foreground (face-attribute 'font-lock-constant-face :foreground))
      (set-face-attribute 'company-tooltip nil
                          :foreground (face-attribute 'default :foreground)
                          :background default-bg-str))))

(defun jvillasante/init-corral ()
  (use-package corral
    :defer t
    :init
    (setq corral-preserve-point t)
    (with-eval-after-load 'window-numbering
      (global-set-key (kbd "C-0") 'corral-parentheses-forward)
      (global-set-key (kbd "C-9") 'corral-parentheses-backward)
      ;; (global-set-key (kbd "M-[") 'corral-brackets-backward)
      ;; (global-set-key (kbd "M-]") 'corral-brackets-forward)
      (global-set-key (kbd "M-{") 'corral-braces-backward)
      (global-set-key (kbd "M-}") 'corral-braces-forward)
      (global-set-key (kbd "M-\"") 'corral-double-quotes-backward))))

(defun jvillasante/post-init-auto-highlight-symbol ()
  (with-eval-after-load 'auto-highlight-symbol
    (setq ahs-default-range 'ahs-range-whole-buffer)
    (setq ahs-idle-interval 1.0)
    (setq ahs-idle-timer nil)
    (ahs-start-timer)
    (set-face-attribute 'ahs-definition-face
                        nil
                        :underline t
                        :bold t
                        :foreground nil
                        :background nil
                        :inherit 'region)
    (set-face-attribute 'ahs-face
                        nil
                        :foreground nil
                        :background nil
                        :inherit 'region)
    (set-face-attribute 'ahs-plugin-whole-buffer-face
                        nil
                        :foreground nil
                        :background nil
                        :inherit 'region)
    (setq ahs-inhibit-face-list '(font-lock-comment-delimiter-face
                                  font-lock-comment-face
                                  font-lock-doc-face
                                  font-lock-doc-string-face
                                  font-lock-string-face))
    (global-set-key (kbd "M-p") 'ahs-backward)
    (global-set-key (kbd "M-n") 'ahs-forward)
    (define-key smartparens-mode-map (kbd "M-p") 'ahs-backward)
    (define-key smartparens-mode-map (kbd "M-n") 'ahs-forward)))

(defun jvillasante/post-init-persp-mode ()
  (setq persp-save-dir spacemacs-cache-directory)
  (setq persp-lighter ""))

(defun jvillasante/init-puppet-mode ()
  (use-package puppet-mode
    :defer t))

(defun jvillasante/init-skeletor ()
  (use-package skeletor
    :defer t))

(defun jvillasante/init-vhdl-mode ()
  (use-package vhdl-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.hdl\\'" . vhdl-mode))
      (defun jvillasante/hdl-comment-syntax ()
        (when (equal (file-name-extension (buffer-file-name (current-buffer))) "hdl")
          ;; define comment for this style: “/* … */” and “//....”
          (setq comment-start "//")
          (setq comment-end "")))
      (add-hook 'vhdl-mode-hook #'jvillasante/hdl-comment-syntax))
    :config
    (progn
      (modify-syntax-entry ?\/ ". 124" vhdl-mode-syntax-table)
      (modify-syntax-entry ?* ". 23b" vhdl-mode-syntax-table)
      (modify-syntax-entry ?\n ">" vhdl-mode-syntax-table))))

(defun jvillasante/post-init-helm-gtags ()
  (setenv "GTAGSLIBPATH" (concat (getenv "HOME") "/.gtags.d/")))

(defun jvillasante/post-init-ggtags ()
  (with-eval-after-load 'ggtags
    (define-key ggtags-mode-map (kbd "M-.") 'helm-gtags-dwim)))

(defun jvillasante/post-init-magit ()
  (with-eval-after-load 'magit
    (define-key magit-mode-map (kbd "C-n") 'next-line))
  (setq-default git-magit-status-fullscreen t)
  (setq magit-completing-read-function 'magit-builtin-completing-read
        magit-push-always-verify nil))

(defun jvillasante/init-nasm-mode ()
  (use-package nasm-mode
    :init
    (progn
      (add-hook 'nasm-mode-hook (lambda ()
                                  (setq indent-tabs-mode nil) ; use spaces to indent
                                  (setq tab-stop-list (number-sequence 2 60 2)) ; 2 spaces per tab
                                  (electric-indent-mode -1)
                                  (company-mode 1)))
      (add-to-list 'auto-mode-alist '("\\.[n]*\\(asm\\|s\\)$" . nasm-mode))
      (add-to-list 'auto-mode-alist '("\\.inc" . nasm-mode)))
    :config
    (progn
      (define-key nasm-mode-map (kbd "M-.") 'helm-etags-select)
      (define-key nasm-mode-map (kbd "C-j") 'newline)
      (define-key nasm-mode-map (kbd ";") 'self-insert-command)
      (define-key nasm-mode-map (kbd ":") 'self-insert-command)
      (define-key nasm-mode-map (kbd "<backtab>") 'company-complete))))

(defun jvillasante/init-srecode ()
  (use-package srecode
    :init
    (progn
      (setq srecode-prefix-key (kbd "M-m /"))
      (add-hook 'prog-mode-hook (lambda ()
                                  (require 'srecode)
                                  (srecode-minor-mode 1))))
    :config
    (progn
      (require 'compile)
      (require 'srecode/map)
      (require 'srecode/fields)
      (setq srecode-insert-ask-variable-method 'field)
      (defun jvillasante/srecode-field-exit-ask ()
        (interactive)
        (call-interactively 'srecode-field-exit-ask)
        (keyboard-quit))
      (define-key srecode-field-keymap (kbd "C-g") 'jvillasante/srecode-field-exit-ask)
      (add-to-list 'srecode-map-load-path (concat user-emacs-directory "private/jvillasante/srt/"))
      (add-to-list 'srecode-map-save-file (concat user-emacs-directory "private/jvillasante/srt/")))))

(defun jvillasante/init-region-state ()
  (use-package region-state
    :init
    (region-state-mode 1)))

(defmacro jvillasante/define-eyebrowse-binding (key)
  `(define-key window-numbering-keymap (kbd ,(concat "M-" key))
     (lambda ()
       (interactive)
       (funcall ',(intern (concat "eyebrowse-switch-to-window-config-" key)))
       (funcall ',(intern (concat "eyebrowse-switch-to-window-config-" key))))))

(defun jvillasante/post-init-eyebrowse ()
  (with-eval-after-load 'window-numbering
    (jvillasante/define-eyebrowse-binding "0")
    (jvillasante/define-eyebrowse-binding "1")
    (jvillasante/define-eyebrowse-binding "2")
    (jvillasante/define-eyebrowse-binding "3")
    (jvillasante/define-eyebrowse-binding "4")
    (jvillasante/define-eyebrowse-binding "5")
    (jvillasante/define-eyebrowse-binding "6")
    (jvillasante/define-eyebrowse-binding "7")
    (jvillasante/define-eyebrowse-binding "8")
    (jvillasante/define-eyebrowse-binding "9")))

(defun jvillasante/post-init-eshell ()
  (with-eval-after-load 'eshell
    (remove-hook 'eshell-mode-hook 'spacemacs//eshell-switch-company-frontend)))

(defun jvillasante/init-eww ()
  (use-package eww
    :defer t
    :init
    (evil-leader/set-key
      "ol" 'eww-list-bookmarks)))

(defun jvillasante/init-describe-number ()
  (use-package describe-number
    :defer t
    :init
    (progn
      (global-set-key (kbd "M-o") 'describe-number-at-point))))

(defun jvillasante/init-dtrt-indent ()
  (use-package dtrt-indent
    :defer t
    :init
    (add-hook 'prog-mode-hook 'dtrt-indent-mode)))

(defun jvillasante/init-asm-mode ()
  (use-package asm-mode
    :commands (asm-mode)
    :config
    (progn
      (add-hook 'asm-mode-hook (lambda ()
                                 (setq indent-tabs-mode nil) ; use spaces to indent
                                 (setq tab-stop-list (number-sequence 2 60 2)) ; 2 spaces per tab
                                 (electric-indent-mode -1)
                                 (company-mode 1)))
      ;; when we press ':' character, it runs `asm-colon' command in asm-mode.
      ;; The command automatically removes any indentation, since every
      ;; non-whitespace character before a colon is a label in asm, and label
      ;; has to be at the beginning of a line. However, the problem is that when
      ;; deleting indentation, trailing spaces are left between the colon and
      ;; point.
      ;;
      ;; These functions solve that problem. First, check whether we have any
      ;; space or tab after point. If so, don't do anything becuase the spaces are
      ;; there intentionally. If not, we delete all trailing spaces between
      ;; point and colon.
      (defvar asm-colon-has-space nil)
      (defun asm-colon-check-space ()
        (setq asm-colon-has-space nil)
        (when (member (string (char-after)) '(" " "\t"))
          (setq asm-colon-has-space t)))
      (defun asm-colon-delete-spaces ()
        (unless asm-colon-has-space
          (call-interactively 'delete-horizontal-space)))
      (advice-add 'asm-colon :before 'asm-colon-check-space)
      (advice-add 'asm-colon :after 'asm-colon-delete-spaces)
      (define-key asm-mode-map (kbd "M-.") 'helm-etags-select)
      (define-key asm-mode-map (kbd "<backtab>") 'company-complete))))

(defun jvillasante/init-bookmark+()
  (use-package bookmark+))

(defun jvillasante/post-init-cc-mode ()
  (add-hook 'c++-mode-hook
            '(lambda ( )
               (c-set-style "Stroustrup")
               (c-toggle-auto-state)))

  (setq c-basic-offset 2
        c-hungry-delete-key t
        c-default-style "linux"))

(defun jvillasante/init-csharp-mode ()
  (use-package csharp-mode
    :defer t
    :init
    (progn
      (add-to-list 'load-path (concat user-emacs-directory "private/jvillasante/extensions/"))
      (use-package wisent-csharp
        :defer t
        :init
        (add-hook 'csharp-mode-hook (lambda ()
                                      (require 'wisent-csharp)
                                      (wisent-csharp-default-setup)
                                      (semantic-mode)
                                      (flycheck-mode)))))))

(defun jvillasante/init-caps-lock()
  (use-package caps-lock))

(defun jvillasante/post-init-yasnippet ()
  (with-eval-after-load 'yasnippet
    (add-to-list 'yas-snippet-dirs (concat user-emacs-directory "private/snippets/"))))

(defun jvillasante/post-init-auto-yasnippet ()
  (setq aya-persist-snippets-dir (concat user-emacs-directory "private/snippets/"))
  (evil-leader/set-key
    "yy" 'aya-create
    "ye" 'aya-expand
    "ys" 'aya-persist-snippet))

(defun jvillasante/post-init-flycheck ()
  ;; (add-hook 'c-mode-hook (lambda ()
  ;;                          (flycheck-select-checker 'c/c++-gcc)))
  (add-hook 'c++-mode-hook (lambda ()
                             (setq flycheck-clang-language-standard "c++11"))))

(defun jvillasante/init-flyspell-lazy ()
  (use-package flyspell-lazy
    :init
    (add-hook 'prog-mode-hook 'flyspell-lazy-mode))
  )

(defun jvillasante/init-firestarter ()
  (use-package firestarter))

(defun jvillasante/init-irony ()
  (use-package irony
    :defer t
    :init
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode)
    ;; replace the `completion-at-point' and `complete-symbol' bindings in
    ;; irony-mode's buffers by irony-mode's function
    (defun my-irony-mode-hook ()
      (define-key irony-mode-map [remap completion-at-point]
        'irony-completion-at-point-async)
      (define-key irony-mode-map [remap complete-symbol]
        'irony-completion-at-point-async))
    (add-hook 'irony-mode-hook 'my-irony-mode-hook)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

(defun jvillasante/init-company-irony ()
  (use-package company-irony
    :init
    (add-hook 'c-mode-hook (lambda () (add-to-list 'company-backends 'company-irony)))
    (add-hook 'c++-mode-hook (lambda () (add-to-list 'company-backends 'company-irony)))
    ;; (optional) adds CC special commands to `company-begin-commands' in order to
    ;; trigger completion at interesting places, such as after scope operator
    ;;     std::|
    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)))

(defun jvillasante/init-flycheck-irony ()
  (use-package flycheck-irony
    :init
    (defun setup-flycheck-irony ()
      (when (featurep 'flycheck)
        (flycheck-irony-setup)))
    (add-hook 'c-mode-hook 'setup-flycheck-irony)
    (add-hook 'c++-mode-hook 'setup-flycheck-irony)))

(defun jvillasante/init-sx ()
  (use-package sx
    :defer t
    :init
    (evil-leader/set-key
      "oss" 'sx-search
      "osg" 'sx-tab-all-questions
      "osi" 'sx-inbox
      "osa" 'sx-ask)))

(defun jvillasante/post-init-org ()
  (add-hook 'org-mode-hook 'yas-minor-mode)

  (with-eval-after-load 'org
    ;; org problems
    (setq org-planning-line-re "^[    ]*\\(\\(?:CLOSED\\|DEADLINE\\|SCHEDULED\\):\\)")
    (setq org-clock-line-re "^[    ]*CLOCK:")

    (setq org-startup-indented t)
    (setq org-indent-mode t)

    ;; set maximum indentation for description lists
    (setq org-list-description-max-indent 5)

    ;; prevent demoting heading also shifting text inside sections
    (setq org-adapt-indentation nil))

  (progn
    (setq org-ellipsis " ▼ ")
    ;; (add-to-list 'load-path (concat user-emacs-directory "private/jvillasante/extensions/org/contrib/lisp"))

    (require 'org)

    (require 'org-eldoc)
    (add-hook 'org-mode-hook 'org-eldoc-load)
    (add-hook 'org-mode-hook 'eldoc-mode)

    (require 'org-table)
    ;; (require 'org-drill)
    ;; (require 'org-depend)
    (require 'ox-beamer)

    (global-unset-key (kbd "C-c a"))

    (add-to-list 'auto-mode-alist '("\\.org\\â€™" . org-mode))
    (add-hook 'org-mode-hook 'auto-fill-mode)
    (define-key org-mode-map (kbd "C-c j") 'helm-org-in-buffer-headings)

    ;; (global-set-key (kbd "C-c a") 'org-agenda)
    (global-set-key (kbd "C-c l") 'org-store-link)

    ;; (global-set-key "\C-cb" 'org-iswitchb)
    (setq org-log-done t)

    (add-hook 'org-mode-hook 'org-indent-mode)
    ;;org-key bindings
    ;; Custom Key Bindings
    ;; (global-unset-key (kbd "<f10>"))
    ;; (global-set-key (kbd "<f12>") 'org-agenda)
    ;; ;; (global-set-key (kbd "<f5>") 'bh/org-todo)
    ;; ;; (global-set-key (kbd "<S-f5>") 'bh/widen)
    ;; ;; (global-set-key (kbd "<f7>") 'bh/set-truncate-lines)
    ;; ;; (global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
    ;; ;; (global-set-key (kbd "<f10> <f10>") 'bh/show-org-agenda)
    ;; ;; (global-set-key (kbd "<f10> b") 'bbdb)
    ;; (global-set-key (kbd "<f10> c") 'calendar)
    ;; (global-set-key (kbd "<f10> f") 'boxquote-insert-file)
    ;; ;; (global-set-key (kbd "<f10> g") 'gnus)
    ;; (global-set-key (kbd "<f10> h") 'bh/hide-other)
    ;; (global-set-key (kbd "<f10> n") 'bh/toggle-next-task-display)
    ;; (global-set-key (kbd "<f10> w") 'widen)

    ;; (global-set-key (kbd "<f10> I") 'bh/punch-in)
    ;; (global-set-key (kbd "<f10> O") 'bh/punch-out)

    ;; ;; (global-set-key (kbd "<f10> o") 'bh/make-org-scratch)

    ;; (global-set-key (kbd "<f10> r") 'boxquote-region)
    ;; (global-set-key (kbd "<f10> s") 'bh/switch-to-scratch)

    ;; (global-set-key (kbd "<f10> t") 'bh/insert-inactive-timestamp)
    ;; (global-set-key (kbd "<f10> T") 'bh/toggle-insert-inactive-timestamp)

    ;; (global-set-key (kbd "<f10> v") 'visible-mode)
    ;; (global-set-key (kbd "<f10> l") 'org-toggle-link-display)
    ;; (global-set-key (kbd "<f10> SPC") 'bh/clock-in-last-task)
    ;; (global-set-key (kbd "C-<f10>") 'previous-buffer)
    ;; (global-set-key (kbd "M-<f10>") 'org-toggle-inline-images)
    ;; (global-set-key (kbd "C-x n r") 'narrow-to-region)
    ;; (global-set-key (kbd "C-<f10>") 'next-buffer)
    ;; (global-set-key (kbd "<f11>") 'org-clock-goto)
    ;; (global-set-key (kbd "C-<f11>") 'org-clock-in)
    ;; (global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)

    (defun bh/hide-other ()
      (interactive)
      (save-excursion
        (org-back-to-heading 'invisible-ok)
        (hide-other)
        (org-cycle)
        (org-cycle)
        (org-cycle)))

    (defun bh/set-truncate-lines ()
      "Toggle value of truncate-lines and refresh window display."
      (interactive)
      (setq truncate-lines (not truncate-lines))
      ;; now refresh window display (an idiom from simple.el):
      (save-excursion
        (set-window-start (selected-window)
                          (window-start (selected-window)))))

    ;; (defun bh/make-org-scratch ()
    ;;   (interactive)
    ;;   (find-file "/tmp/publish/scratch.org")
    ;;   (gnus-make-directory "/tmp/publish"))

    (defun bh/switch-to-scratch ()
      (interactive)
      (switch-to-buffer "*scratch*"))

    ;; (setq org-default-notes-file (concat org-directory "~/org-data/tasks.org"))
    (defvar custom--org-project-file-path "~/org-data/")

    (defun custom-org-project-files
        (directory-files custom--org-project-file-path nil "\\.org$"))8
        (defun custom-org-project-chooser ()
          (let ((completing-read-func (if (null ido-mode)
                                          'completing-read
                                        'ido-completing-read)))
            (setq project-file
                  (funcall completing-read-func
                           "Project: "
                           (custom-org-project-files)
                           nil
                           t))))

        ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
        (setq org-capture-templates
              (quote (("t" "todo" entry (file "~/org-data/refile.org")
                       "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                      ("l" "link" entry (file "~/org-data/refile.org")
                       "* %?\n%A\n" :clock-in t :clock-resume t)
                      ("r" "respond" entry (file "~/org-data/refile.org")
                       "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
                      ("n" "note" entry (file "~/org-data/refile.org")
                       "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                      ("j" "Journal" entry (file+datetree "~/org-data/diary.org")
                       "* %?\n%U\n" :clock-in t :clock-resume t)
                      ("w" "org-protocol" entry (file "~/org-data/refile.org")
                       "* TODO Review %c\n%U\n" :immediate-finish t)
                      ("m" "Meeting" entry (file "~/org-data/refile.org")
                       "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                      ("p" "Phone call" entry (file "~/org-data/refile.org")
                       "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                      ("h" "Habit" entry (file "~/org-data/refile.org")
                       "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

        (global-set-key (kbd "C-c c") 'org-capture)

        (setq org-clock-persist 'history)
        (org-clock-persistence-insinuate)
        (setq org-replace-disputed-keys nil)

        (org-babel-do-load-languages
         'org-babel-load-languages
         '((sh . t)))

        (setq org-agenda-files (list
                                "~/workspace/study/study-plan.org"
                                "~/Dropbox/org/projects.org"
                                "~/org-data/tasks.org"))
        (add-hook 'org-mode-hook
                  (lambda ()
                    (local-set-key "\M-\C-n" 'outline-next-visible-heading)
                    (local-set-key "\M-\C-p" 'outline-previous-visible-heading)
                    (local-set-key "\M-\C-u" 'outline-up-heading)
                    ;; table
                    (local-set-key "\M-\C-w" 'org-table-copy-region)
                    (local-set-key "\M-\C-y" 'org-table-paste-rectangle)
                    (local-set-key "\M-\C-l" 'org-table-sort-lines)
                    ;; display images
                    (local-set-key "\M-I" 'org-toggle-iimage-in-org)
                    (yas-minor-mode -1)))

        (setq org-use-speed-commands t)

        (setq org-confirm-babel-evaluate nil)

        (setq org-src-fontify-natively t)
        (setq org-src-tab-acts-natively t)

        (setq org-todo-keywords
              (quote ((sequence "TODO(t)" "NEXT(n)" "PROGRESS(p)" "|" "DONE(d)")
                      (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

        (setq org-todo-keyword-faces
              (quote (("TODO" :foreground "red" :weight bold)
                      ("NEXT" :foreground "blue" :weight bold)
                      ("PROGRESS" :foreground "yellow" :weight bold)
                      ("DONE" :foreground "forest green" :weight bold)
                      ("HOLD" :foreground "magenta" :weight bold)
                      ("WAITING" :foreground "orange" :weight bold)
                      ("CANCELLED" :foreground "forest green" :weight bold)
                      ("MEETING" :foreground "forest green" :weight bold)
                      ("PHONE" :foreground "forest green" :weight bold))))

        ;; ,----
        ;; | Refilling Tasks
        ;; `----
        ;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
        (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                         (org-agenda-files :maxlevel . 9))))

        ;; Use full outline paths for refile targets - we file directly with IDO
        (setq org-refile-use-outline-path t)

        ;; Targets complete directly with IDO
        (setq org-outline-path-complete-in-steps nil)

        ;; Allow refile to create parent tasks with confirmation
        (setq org-refile-allow-creating-parent-nodes (quote confirm))

        ;; Use IDO for both buffer and file completion and ido-everywhere to t
        ;; (setq org-completion-use-ido t)
        ;; (setq ido-everywhere t)
        ;; (setq ido-max-directory-size 100000)
        ;; (ido-mode (quote both))
        ;; Use the current window when visiting files and buffers with ido
        ;; (setq ido-default-file-method 'selected-window)
        ;; (setq ido-default-buffer-method 'selected-window)
        ;; Use the current window for indirect buffer display
        ;; (setq org-indirect-buffer-display 'current-window)

        ;; Refile settings
        ;; Exclude DONE state tasks from refile targets
        (defun bh/verify-refile-target ()
          "Exclude todo keywords with a done state from refile targets"
          (not (member (nth 2 (org-heading-components)) org-done-keywords)))

        (setq org-refile-target-verify-function 'bh/verify-refile-target)

        ;; ,----
        ;; | Agenda setup
        ;; `----
        ;; Do not dim blocked tasks
        (setq org-agenda-dim-blocked-tasks nil)

        ;; Compact the block agenda view
        (setq org-agenda-compact-blocks t)

        ;; Custom agenda command definitions
        (setq org-agenda-custom-commands
              (quote (("N" "Notes" tags "NOTE"
                       ((org-agenda-overriding-header "Notes")
                        (org-tags-match-list-sublevels t)))
                      ("h" "Habits" tags-todo "STYLE=\"habit\""
                       ((org-agenda-overriding-header "Habits")
                        (org-agenda-sorting-strategy
                         '(todo-state-down effort-up category-keep))))
                      (" " "Agenda"
                       ((agenda "" nil)
                        (tags "REFILE"
                              ((org-agenda-overriding-header "Tasks to Refile")
                               (org-tags-match-list-sublevels nil)))
                        (tags-todo "-CANCELLED/!"
                                   ((org-agenda-overriding-header "Stuck Projects")
                                    (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                                    (org-agenda-sorting-strategy
                                     '(category-keep))))
                        (tags-todo "-HOLD-CANCELLED/!"
                                   ((org-agenda-overriding-header "Projects")
                                    (org-agenda-skip-function 'bh/skip-non-projects)
                                    (org-tags-match-list-sublevels 'indented)
                                    (org-agenda-sorting-strategy
                                     '(category-keep))))
                        (tags-todo "-CANCELLED/!NEXT"
                                   ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                          (if bh/hide-scheduled-and-waiting-next-tasks
                                                                              ""
                                                                            " (including WAITING and SCHEDULED tasks)")))
                                    (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                                    (org-tags-match-list-sublevels t)
                                    (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                                    (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                                    (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                                    (org-agenda-sorting-strategy
                                     '(todo-state-down effort-up category-keep))))
                        (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                                   ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                          (if bh/hide-scheduled-and-waiting-next-tasks
                                                                              ""
                                                                            " (including WAITING and SCHEDULED tasks)")))
                                    (org-agenda-skip-function 'bh/skip-non-project-tasks)
                                    (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                                    (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                                    (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                                    (org-agenda-sorting-strategy
                                     '(category-keep))))
                        (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                                   ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                          (if bh/hide-scheduled-and-waiting-next-tasks
                                                                              ""
                                                                            " (including WAITING and SCHEDULED tasks)")))
                                    (org-agenda-skip-function 'bh/skip-project-tasks)
                                    (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                                    (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                                    (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                                    (org-agenda-sorting-strategy
                                     '(category-keep))))
                        (tags-todo "-CANCELLED+WAITING|HOLD/!"
                                   ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                                    (org-agenda-skip-function 'bh/skip-stuck-projects)
                                    (org-tags-match-list-sublevels nil)
                                    (org-agenda-todo-ignore-scheduled t)
                                    (org-agenda-todo-ignore-deadlines t)))
                        (tags "-REFILE/"
                              ((org-agenda-overriding-header "Tasks to Archive")
                               (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                               (org-tags-match-list-sublevels nil))))
                       nil))))

        (add-hook 'org-mode-hook 'yas-minor-mode-on)
        (add-hook 'org-mode-hook (lambda () (semantic-mode -1)))

        (defun yas/org-very-safe-expand ()
          (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

        (add-hook 'org-mode-hook
                  (lambda ()
                    (make-variable-buffer-local 'yas/trigger-key)
                    (setq yas/trigger-key [tab])
                    (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
                    (define-key yas/keymap [tab] 'yas/next-field)
                    ))

        ;; Make windmove work in org-mode:
        (add-hook 'org-shiftup-final-hook 'windmove-up)
        (add-hook 'org-shiftleft-final-hook 'windmove-left)
        (add-hook 'org-shiftdown-final-hook 'windmove-down)
        (add-hook 'org-shiftright-final-hook 'windmove-right)

        (defun my/org-add-ids-to-headlines-in-file ()
          "Add ID properties to all headlines in the current file which
do not already have one."
          (interactive)
          (org-map-entries 'org-id-get-create))

        ;; (add-hook 'org-mode-hook
        ;;           (lambda ()
        ;;             (add-hook 'before-save-hook 'my/org-add-ids-to-headlines-in-file nil 'local)))

        ;; (setq org-hide-emphasis-markers t)

        (require 'ox-html)
        ;; (require 'ox-rs)			;
        (require 'ox-publish)

        (setq org-publish-project-alist
              '(("blog-content"
                 :base-directory "~/Public/emacs-tutor/emacs-tutor/"
                 :html-extension "html"
                 :base-extension "org"
                 :publishing-directory "~/Public/emacs-tutor/"
                 :publishing-function (org-html-publish-to-html)
                 :recursive t               ; descend into sub-folders?
                 :section-numbers nil       ; don't create numbered sections
                 :with-toc t                ; don't create a table of contents
                 :with-latex t              ; do use MathJax for awesome formulas!
                 :html-preamble
                 (lambda (info)
                   (concat "<!-- Start of StatCounter Code for Default Guide -->
<script type=\"text/javascript\">
var sc_project=9874755;
var sc_invisible=1;
var sc_security=\"c2028bb7\";
var scJsHost = ((\"https:\" == document.location.protocol) ?
\"https://secure.\" : \"http://www.\");
document.write(\"<sc\"+\"ript type='text/javascript' src='\" + scJsHost+
\"statcounter.com/counter/counter.js'></\"+\"script>\");
</script>
<noscript><div class=\"statcounter\"><a title=\"hit counter\"
href=\"http://statcounter.com/free-hit-counter/\" target=\"_blank\"><img
class=\"statcounter\" src=\"http://c.statcounter.com/9874755/0/c2028bb7/1/\"
alt=\"hit counter\"></a></div></noscript>
<!-- End of StatCounter Code for Default Guide -->"
                           (cond ((and (listp (plist-get info :title))
                                       (or (string= (car (plist-get info :title)) "Table of Contents")
                                           (string= (car (plist-get info :title)) "Contents")))
                                  "")
                                 (t "
<h2><a href=\"index.html\">Back to Table of Contents</a></h2>
"))))                                   ; this stuff is put before your post
                 :html-postamble
                 (lambda (info)
                   (cond ((and (listp (plist-get info :title))
                               (or (string= (car (plist-get info :title)) "Table of Contents")
                                   (string= (car (plist-get info :title)) "Contents")))
                          "")
                         (t "
    <div id=\"disqus_thread\"></div>
    <script type=\"text/javascript\">
        /* * * CONFIGURATION VARIABLES: EDIT BEFORE PASTING INTO YOUR WEBPAGE * * */
        var disqus_shortname = 'emacs-tutor'; // required: replace example with your forum shortname

        /* * * DON'T EDIT BELOW THIS LINE * * */
        (function() {
            var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
            dsq.src = '//' + disqus_shortname + '.disqus.com/embed.js';
            (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
        })();
    </script>
    <noscript>Please enable JavaScript to view the <a href=\"http://disqus.com/?ref_noscript\">comments powered by Disqus.</a></noscript>
    <a href=\"http://disqus.com\" class=\"dsq-brlink\">comments powered by <span class=\"logo-disqus\">Disqus</span></a>
")
                         ;; (t "<div id=\"archive\"><a href=\"index.html\">index</a></div>")
                         ))
                 :html-head-extra "<link rel=\"stylesheet\" href=\"./static/worg.css\">"
                 ;; :auto-sitemap t
                 ;; :sitemap-filename "index.org"
                 ;; :sitemap-title "Table of Content"
                 ;; :sitemap-sort-files chronologically
                 ;; :sitemap-style list
                 ;; :makeindex t
                 :html-head-extra "<link rel=\"stylesheet\" href=\"./static/worg.css\">"
                 )
                ("blog-static"
                 :base-directory "~/Public/emacs-tutor/emacs-tutor/static/"
                 :base-extension "gif\\|png\\|jpg\\|css"
                 :publishing-directory "~/Public/emacs-tutor/static"
                 :recursive t
                 :publishing-function org-publish-attachment)
                ("blog"
                 :components ("blog-content" "blog-static"))
                ("enux"
                 :base-directory "~/enux"
                 :html-extension "html"
                 :base-extension "org"
                 :publishing-directory "~/enux"
                 :publishing-function (org-html-publish-to-html)
                 :recursive t               ; descend into sub-folders?
                 :section-numbers nil       ; don't create numbered sections
                 :with-toc t                ; don't create a table of contents
                 :with-latex t              ; do use MathJax for awesome formulas!
                 :html-head-extra "<link rel=\"stylesheet\" href=\"./worg.css\">"
                 )))

        (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n\"'")
        (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

        (setq org-use-speed-commands t)
        (add-to-list 'org-structure-template-alist '("n" "#+NAME:"))

        (defun org-babel-src-block-names (&optional file)
          "Returns the names of source blocks in FILE or the current buffer."
          (when file (find-file file))
          (save-excursion
            (goto-char (point-min))
            (let ((re (org-babel-named-src-block-regexp-for-name))
                  names)
              (while (ignore-errors (org-next-block 1 nil re))
                (push (org-match-string-no-properties 9) names))
              (nreverse  names))))))

(defun jvillasante/post-init-omnisharp ()
  (setq omnisharp-server-executable-path "/usr/local/bin/omnisharp"))

(defun jvillasante/init-pdf-tools ()
  (use-package pdf-tools
    :if (not (eq window-system 'ns))
    :init
    (pdf-tools-install)))


(defun jvillasante/post-init-popwin ()
  (push '(".*wiki-summary.*"
          :regexp t
          :dedicated nil
          :position bottom
          :stick nil
          :noselect nil
          :height 0.4)
        popwin:special-display-config)

  (defun jvillasante/popwin-local-key ()
    (local-set-key (kbd "C-g") (lambda ()
                                 (interactive)
                                 (cond
                                  ((string-match "wiki-summary" (buffer-name))
                                   (kill-buffer-and-window))
                                  (t (call-interactively 'keyboard-quit))))))
  (add-hook 'popwin:after-popup-hook #'jvillasante/popwin-local-key))

(defun jvillasante/post-init-semantic ()
  (add-hook 'java-mode-hook 'semantic-mode)
  (eval-after-load "semantic"
    (lambda ()
      (semantic-add-system-include "~/workspace/linux/kernel" 'c-mode)
      (semantic-add-system-include "~/workspace/linux/include" 'c-mode)
      (semantic-add-system-include "~/workspace/linux/kernel" 'c++-mode)
      (semantic-add-system-include "~/workspace/linux/include" 'c++-mode)))

  (setq semantic-idle-scheduler-max-buffer-size 2000000))

(defun jvillasante/init-quickrun ()
  (use-package quickrun
    :defer t
    :init
    (evil-leader/set-key
      "tq" 'quickrun-autorun-mode
      "oR" 'quickrun-with-arg
      "or" 'quickrun)
    (global-set-key (kbd "<f5>") 'quickrun-shell)
    :config
    (progn
      ;; Use this parameter as C++ default
      (quickrun-add-command "c++/c11"
                            '((:command . "g++")
                              (:exec    . ("%c -std=c++11 %o -o %e %s"
                                           "%e %a"))
                              (:remove  . ("%e")))
                            :default "c++")))
  )

(defun jvillasante/init-wiki-summary ()
  (use-package wiki-summary
    :defer t
    :init
    (evil-leader/set-key
      "ow" 'wiki-summary)))

(defun jvillasante/post-init-shell-pop ()
  (setq shell-default-shell 'eshell))

(defun jvillasante/post-init-slime ()
  (add-hook 'slime-mode-hook #'smartparens-mode)

  (defvar slime-repl-font-lock-keywords lisp-font-lock-keywords-2)
  (defun slime-repl-font-lock-setup ()
    (setq font-lock-defaults
          '(slime-repl-font-lock-keywords
            ;; From lisp-mode.el
            nil nil (("+-*/.<>=!?$%_&~^:@" . "w")) nil
            (font-lock-syntactic-face-function
             . lisp-font-lock-syntactic-face-function))))

  (add-hook 'slime-repl-mode-hook 'slime-repl-font-lock-setup)

  (defadvice slime-repl-insert-prompt (after font-lock-face activate)
    (let ((inhibit-read-only t))
      (add-text-properties
       slime-repl-prompt-start-mark (point)
       '(font-lock-face
         slime-repl-prompt-face
         rear-nonsticky
         (slime-repl-prompt read-only font-lock-face intangible)))))

  (defadvice eww-render (around eww-render-popwin activate)
    (save-window-excursion ad-do-it)
    (unless (get-buffer-window "*eww*")
      (pop-to-buffer "*eww*")))

  (defun eww-slime-hyperspec-lookup (symbol-name)
    (interactive (list (common-lisp-hyperspec-read-symbol-name
                        (slime-symbol-at-point))))
    (let ((browse-url-browser-function 'eww-browse-url)
          (popwin:special-display-config nil))
      (push '(eww-mode :position bottom :noselect t :height 0.5) popwin:special-display-config)
      (hyperspec-lookup symbol-name)))

  (setq common-lisp-hyperspec-root (concat "file://" (file-truename user-emacs-directory) "private/jvillasante/HyperSpec/"))
  (add-hook 'lisp-mode-hook (lambda ()
                              (semantic-default-elisp-setup)
                              (semantic-mode 1)
                              (semantic-idle-summary-mode -1)))

  (with-eval-after-load 'slime
    (define-key slime-repl-mode-map (kbd "M-m h d") 'eww-slime-hyperspec-lookup)
    (define-key slime-mode-map (kbd "M-m h d") 'eww-slime-hyperspec-lookup)
    (when (featurep 'lispy)
      (define-key slime-mode-map (kbd "C-M-SPC") 'special-lispy-space))))

(defun jvillasante/init-x86-lookup ()
  (use-package x86-lookup
    :init
    (setq x86-lookup-pdf "~/Downloads/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf")
    :config
    (progn
      (when (package-installed-p 'pdf-tools)
        (defun x86-lookup-browse-pdf-view-emacs (pdf page)
          "View PDF at PAGE using Emacs' `doc-view-mode' and `display-buffer'."
          (with-selected-window (display-buffer (find-file-noselect pdf :nowarn))
            (pdf-view-goto-page page)))
        (setq x86-lookup-browse-pdf-function 'x86-lookup-browse-pdf-view-emacs)))))

(defun jvillasante/init-goto-chg ()
  (use-package goto-chg
    :init
    (evil-leader/set-key
      "<" 'goto-last-change
      ">" 'goto-last-change-reverse)))

(defun jvillasante/post-init-nasm-mode ()
  (define-key nasm-mode-map (kbd "C-c d .") 'x86-lookup))

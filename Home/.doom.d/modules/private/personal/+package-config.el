;;; config/personal/+package-config.el -*- lexical-binding: t; -*-

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solarized setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! solarized
  (if (string= 'solarized-dark (car custom-enabled-themes))
      (update-solarize-dark)
    (update-solarize-light)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yasnippet setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! yasnippet
  (push (expand-file-name "snippets/" doom-private-dir) yas-snippet-dirs))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! org
  ;; org problems
  (setq org-planning-line-re "^[    ]*\\(\\(?:CLOSED\\|DEADLINE\\|SCHEDULED\\):\\)")
  (setq org-clock-line-re "^[    ]*CLOCK:")

  (add-hook 'org-mode-hook
            (lambda ()
              ;; (spacemacs/toggle-auto-fill-mode-on)
              (set-fill-column 110)))

  (setq org-startup-indented t)
  (setq org-indent-mode t)

  ;; set maximum indentation for description lists
  (setq org-list-description-max-indent 5)

  ;; prevent demoting heading also shifting text inside sections
  (setq org-adapt-indentation nil))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Neotree setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! neotree
  (defadvice neo-buffer--get-nodes
      (after neo-buffer--get-nodes-new-sorter activate)
    (setq ad-return-value
          (let ((nodes ad-return-value)
                (comparator (lambda (s1 s2) (string< (downcase s1)
                                                     (downcase s2)))))
            (apply 'cons (mapcar (lambda (x) (sort (apply x (list nodes))
                                                   comparator))
                                 '(car cdr))))))

  (setq neo-window-width 60
        neo-create-file-auto-open t
        neo-banner-message "Press ? for neotree help"
        neo-show-updir-line nil
        neo-mode-line-type 'neotree
        neo-smart-open t
        ;; neo-dont-be-alone t
        ;; neo-persist-show nil
        neo-show-hidden-files t
        neo-auto-indent-point t
        ;; neo-modern-sidebar t
        neo-vc-integration nil

        neo-theme 'ascii
        ;; doom-neotree-enable-variable-pitch nil
        neo-autorefresh nil)

  (when (eq 'darwin system-type)
    (setq neo-default-system-application "open"))

  (setq neo-hidden-regexp-list
        '("^\\.\\(git\\|cache\\|tox\\|coverage\\)$"
          "^\\.\\(DS_Store\\|python\\-version\\)"
          "^\\(htmlcov\\|node_modules\\)$" "\\.elcs$"
          "^\\.coverage\\..*" "\\.ipynb.*$" "\\.py[cod]$"
          "~$" "^#.*#$" "^\\.#.*$" "^__pycache__$"
          "\\.egg\-info$")))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! company
  (setq
   company-idle-delay 0.1
   company-minimum-prefix-length 3))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! magit
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq-default git-magit-status-fullscreen t)
  (setq magit-completing-read-function 'magit-builtin-completing-read
        magit-push-always-verify nil)

  (setq
   ;; magit-repository-directories '(("~/work" . 2))
   ;; magit-commit-arguments '("--gpg-sign=5F6C0EA160557395")
   ;; magit-rebase-arguments '("--autostash" "--gpg-sign=5F6C0EA160557395")
   ;; magit-pull-arguments   '("--rebase" "--autostash" "--gpg-sign=5F6C0EA160557395")
   +magit-hub-features t
   git-commit-summary-max-length 80
   vc-handled-backends (delq 'Git vc-handled-backends))

  ;; Temporary workaround for +magit/quit hang with lots of buffers
  (define-key magit-status-mode-map [remap magit-mode-bury-buffer] nil))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Avy setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! avy
  (setq avy-all-windows t))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Counsel setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! counsel
  (setq counsel-rg-base-command "rg -S --no-heading --line-number --color never %s ."
        counsel-ag-base-command "ag -S --nocolor --nogroup %s"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ivy setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! ivy
  (setq ivy-display-style 'fancy
        ivy-count-format "(%d/%d) "
        ivy-use-selectable-prompt t    ; much better than C-M-j
        ivy-use-virtual-buffers t      ; to make ivy-views appear on the buffers list
        ivy-virtual-abbreviate 'full   ; default is name
        ivy-initial-inputs-alist nil   ; remove initial ^ input.
        ivy-extra-directories nil      ; remove . and .. directory. (default value: ("../" "./"))
        ivy-height 10)

  (map!
   :map ivy-minibuffer-map
   "C-d" #'ivy-kill-line))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! flycheck
  (setq-default
   +flycheck-on-escape nil
   flycheck-check-syntax-automatically '(save mode-enable)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deft setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! deft
  (setq deft-directory (concat jv/dropbox-path "/Personal/notes")
        deft-default-extension "org"
        deft-extensions '("org")
        deft-recursive t
        deft-text-mode 'org-mode
        deft-use-filename-as-title t
        deft-use-filter-string-for-filename t
        deft-file-naming-rules '((noslash . "-")
                                 (nospace . "-")
                                 (case-fn . downcase))
        deft-auto-save-interval 0))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! dired
  ;; mark symlinks
  (setq dired-ls-F-marks-symlinks t)
  ;; Never prompt for recursive copies of a directory
  (setq dired-recursive-copies 'always)
  ;; Never prompt for recursive deletes of a directory
  (setq dired-recursive-deletes 'always)
  ;; makes dired guess the target directory
  (setq dired-dwim-target t)

  ;; Dired listing switches
  ;;  -a : Do not ignore entries starting with .
  ;;  -l : Use long listing format.
  ;;  -h : Human-readable sizes like 1K, 234M, ..
  ;;  -v : Do natural sort .. so the file names starting with . will show up first.
  ;;  -F : Classify filenames by appending '*' to executables,'/' to directories, etc.
  ;; default value for dired: "-al"
  ;; (setq dired-listing-switches (if (eq system-type 'windows-nt)
  ;;                                "-alh"
  ;;                                "-alhvF --group-directories-first"))

  ;; auto-revert dired buffers if file changed on disk
  (setq dired-auto-revert-buffer t)

  (setq projectile-switch-project-action 'projectile-dired) ; dired loads on project switch
  ;; (evil-leader/set-key "od" 'dired)

  ;; Hydra for dired
  (with-eval-after-load 'dired 'hydra
                        (defhydra hydra-dired (:hint nil :color pink)
                          "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell       crux
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary        C-c o : crux-open-with
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
                          ("\\" dired-do-ispell)
                          ("(" dired-hide-details-mode)
                          (")" dired-omit-mode)
                          ("+" dired-create-directory)
                          ("=" diredp-ediff)         ;; smart diff
                          ("?" dired-summary)
                          ("$" diredp-hide-subdir-nomove)
                          ("A" dired-do-find-regexp)
                          ("C" dired-do-copy)        ;; Copy all marked files
                          ("D" dired-do-delete)
                          ("E" dired-mark-extension)
                          ("e" dired-ediff-files)
                          ("F" dired-do-find-marked-files)
                          ("G" dired-do-chgrp)
                          ("g" revert-buffer)        ;; read all directories again (refresh)
                          ("i" dired-maybe-insert-subdir)
                          ("l" dired-do-redisplay)   ;; relist the marked or singel directory
                          ("M" dired-do-chmod)
                          ("m" dired-mark)
                          ("O" dired-display-file)
                          ("o" dired-find-file-other-window)
                          ("Q" dired-do-find-regexp-and-replace)
                          ("R" dired-do-rename)
                          ("r" dired-do-rsynch)
                          ("S" dired-do-symlink)
                          ("s" dired-sort-toggle-or-edit)
                          ("t" dired-toggle-marks)
                          ("U" dired-unmark-all-marks)
                          ("u" dired-unmark)
                          ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
                          ("w" dired-kill-subdir)
                          ("Y" dired-do-relsymlink)
                          ("z" diredp-compress-this-file)
                          ("Z" dired-do-compress)
                          ("q" nil)
                          ("." nil :color blue))

                        (define-key dired-mode-map "." 'hydra-dired/body)))

(after! dired-quick-sort
  (dired-quick-sort-setup)
  (define-key dired-mode-map "s" 'hydra-dired-quick-sort/body))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elfeed setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jv/elfeed-mark-all-as-read ()
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

;; functions to support syncing .elfeed between machines
;; makes sure elfeed reads index from disk before launching
(defun jv/elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force))

;; write to disk when quiting
(defun jv/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

(after! elfeed
  ;; elfeed db path
  (setq elfeed-db-directory (concat jv/dropbox-path "/Personal/elfeed/elfeed_db"))

  ;; load and save db on open and quit
  ;; (spacemacs/set-leader-keys "af" 'jv/elfeed-load-db-and-open)
  (evil-define-key 'evilified elfeed-search-mode-map "q" 'jv/elfeed-save-db-and-bury))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! lsp
  (require 'ccls)
  (setq ccls-executable "/usr/local/bin/ccls")

  (defun jv/setup-lsp-mode ()
    (setq lsp-remap-xref-keybindings nil)
    (setq lsp-navigation 'both))

  (defun jv/setup-lsp-ui-mode ()
    (setq lsp-ui-doc-enable t)
    (setq lsp-ui-doc-include-signature nil)
    (setq lsp-ui-sideline-enable nil)
    (setq lsp-ui-sideline-show-symbol nil)
    (setq lsp-ui-sideline-ignore-dupliate nil))

  (defun jv/setup-lsp-ui-mode-no-doc ()
    (setq lsp-ui-doc-enable nil)
    (setq lsp-ui-doc-include-signature nil)
    (setq lsp-ui-sideline-enable nil)
    (setq lsp-ui-sideline-show-symbol nil)
    (setq lsp-ui-sideline-ignore-dupliate nil))

  (add-hook 'cc-mode-hook 'jv/setup-lsp-mode)
  (add-hook 'rust-mode-hook 'jv/setup-lsp-mode)
  (add-hook 'go-mode-hook 'jv/setup-lsp-mode)

  (add-hook 'cc-mode-hook 'jv/setup-lsp-ui-mode-no-doc)
  (add-hook 'rust-mode-hook 'jv/setup-lsp-ui-mode-no-doc)
  (add-hook 'go-mode-hook 'jv/setup-lsp-ui-mode-no-doc))

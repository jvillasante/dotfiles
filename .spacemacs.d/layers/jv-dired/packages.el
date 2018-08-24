(defconst jv-dired-packages
  '(dired
     ;; dired+
     dired-quick-sort))

(defun jv-dired/post-init-dired ()
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
  (setq dired-listing-switches (if (eq system-type 'windows-nt)
                                 "-alh"
                                 "-alhvF --group-directories-first"))

  ;; auto-revert dired buffers if file changed on disk
  (setq dired-auto-revert-buffer t)

  (setq projectile-switch-project-action 'projectile-dired) ; dired loads on project switch
  (evil-leader/set-key "od" 'dired)

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

;; (defun jv-dired/init-dired+ ()
;;   (use-package dired+
;;     :defer t
;;     :config
;;     (require 'dired+)

;;     ;; reuse dired directories instead of opening a thousand `dired' buffers
;;     (diredp-toggle-find-file-reuse-dir 1)

;;     ;; show more details by default
;;     (setq diredp-hide-details-initially-flag nil)
;;     (setq diredp-hide-details-propagate-flag nil)))

(defun jv-dired/init-dired-quick-sort ()
  (use-package dired-quick-sort
    :defer t
    :config
    (dired-quick-sort-setup)
    (define-key dired-mode-map "s" 'hydra-dired-quick-sort/body)))

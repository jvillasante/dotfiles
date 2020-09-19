;;; +hydras.el -*- lexical-binding: t; -*-

(after! hydra
    ;; Hydra for dired
    (after! dired
        (defhydra +my/hydra-dired (:hint nil :color red)
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
            ("." nil :color blue)))

    ;; Hydra for org
    (after! org
        (defhydra +my/hydra-org (:color red :columns 3)
            "Org Mode Movements"
            ("n" outline-next-visible-heading "next heading")
            ("p" outline-previous-visible-heading "prev heading")
            ("N" org-forward-heading-same-level "next heading at same level")
            ("P" org-backward-heading-same-level "prev heading at same level")
            ("u" outline-up-heading "up heading")
            ("g" org-goto "goto" :exit t)))

    ;; Hydra for neotree
    (after! neotree
        (defhydra +my/hydra-neotree (:hint nil :color red)
            "
Navigation^^^^             Actions^^         Visual actions/config^^^
───────^^^^─────────────── ───────^^──────── ───────^^^────────────────
[_L_]   next sibling^^     [_c_] create      [_._] shrink/enlarge
[_H_]   previous sibling^^ [_C_] copy        [_|_]   vertical split
[_J_]   goto child^^       [_d_] delete      [_-_]   horizontal split
[_K_]   goto parent^^      [_r_] rename      [_gr_]  refresh^
[_l_]   open/expand^^      [_R_] change root [_s_]   hidden:^^^ %s(if neo-buffer--show-hidden-file-p \"on\" \"off\")
[_h_]   up/collapse^^      ^^                ^^^
[_j_]   line down^^        ^^                ^^^
[_k_]   line up^^          ^^                ^^
[_'_]   quick look         ^^                ^^
[_RET_] open               ^^^^              [_?_]   close hints
"
            ("RET" +neotree/expand-or-open)
            ("." neotree-stretch-toggle)
            ("|" neotree-enter-vertical-split)
            ("-" neotree-enter-horizontal-split)
            ("?" nil :exit t)
            ("'" neotree-quick-look)
            ("c" neotree-create-node)
            ("C" neotree-copy-node)
            ("d" neotree-delete-node)
            ("gr" neotree-refresh)
            ("h" +neotree/collapse-or-up)
            ("H" neotree-select-previous-sibling-node)
            ("j" neotree-next-line)
            ("J" neotree-select-down-node)
            ("k" neotree-previous-line)
            ("K" neotree-select-up-node)
            ("l" +neotree/expand-or-open)
            ("L" neotree-select-next-sibling-node)
            ("r" neotree-rename-node)
            ("R" neotree-change-root)
            ("s" neotree-hidden-file-toggle)))
    )

(defun jv/disable-all-themes ()
    (interactive)
    (mapc #'disable-theme custom-enabled-themes))

;;; Theme hooks

(defvar jv/theme-hooks nil
    "((theme-id . function) ...)")

(defun jv/add-theme-hook (theme-id hook-func)
    (add-to-list 'jv/theme-hooks (cons theme-id hook-func)))

(defun jv/load-theme-advice (f theme-id &optional no-confirm no-enable &rest args)
    "Enhances `load-theme' in two ways:
1. Disables enabled themes for a clean slate.
2. Calls functions registered using `jv/add-theme-hook'."
    (unless no-enable
        (jv/disable-all-themes))
    (prog1
        (apply f theme-id no-confirm no-enable args)
        (unless no-enable
            (pcase (assq theme-id jv/theme-hooks)
                (`(,_ . ,f) (funcall f))))))

(advice-add 'load-theme
    :around
    #'jv/load-theme-advice)

;;; Theme custom configs

(defun jv/solarized-theme-hook ()
    (set-face-attribute 'font-lock-constant-face nil :weight 'normal)
    (set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
    (set-face-attribute 'which-key-key-face nil :foreground
        (face-attribute 'error :foreground)))

;;; Hydra

(with-eval-after-load 'hydra
    (defhydra jv/themes-hydra (:hint nil :color pink)
        "
Themes

^Solarized^   ^Material^    ^Doom^                        ^Other
-----------------------------------------------------------------------------------
_s_: Dark     _m_: Dark     _d_: Doom Solarized Dark      _z_: Zenburn  _DEL_: none
_S_: Light    _M_: Light    _D_: Doom Solarized Light
^ ^           ^ ^           _o_: Doom One Dark
^ ^           ^ ^           _O_: Doom One Light
^ ^           ^ ^           _n_: Doom Nord Dark
^ ^           ^ ^           _N_: Doom Nord Light
^ ^           ^ ^           _p_: Doom Opera Dark
^ ^           ^ ^           _P_: Doom Opera Light
^ ^           ^ ^           _t_: Doom Tomorrow Dark
^ ^           ^ ^           _T_: Doom Tomorrow Light
^ ^           ^ ^           _v_: Doom Vibrant
^ ^           ^ ^           _k_: Doom molokai
"
        ("s" (load-theme 'solarized-dark       t))
        ("S" (load-theme 'solarized-light      t))
        ("m" (load-theme 'material             t))
        ("M" (load-theme 'material-light       t))
        ("d" (load-theme 'doom-solarized-dark  t))
        ("D" (load-theme 'doom-solarized-light t))
        ("o" (load-theme 'doom-one             t))
        ("O" (load-theme 'doom-one-light       t))
        ("n" (load-theme 'doom-nord            t))
        ("N" (load-theme 'doom-nord-light      t))
        ("p" (load-theme 'doom-opera           t))
        ("P" (load-theme 'doom-opera-light     t))
        ("t" (load-theme 'doom-tomorrow-night  t))
        ("T" (load-theme 'doom-tomorrow-day    t))
        ("v" (load-theme 'doom-vibrant         t))
        ("k" (load-theme 'doom-molokai         t))
        ("z" (load-theme 'zenburn              t))
        ("DEL" (jv/disable-all-themes))
        ("RET" nil "done" :color blue)))

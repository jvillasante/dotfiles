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

(defun jv/zenburn-theme-hook ()
    (custom-theme-set-faces
        'zenburn
        ;; make zenburn background theme darker
        '(default ((t (:foreground "#DCDCCC" :background "#383838"))))

        ;; mode-line related config
        '(mode-line ((t (:box (:line-width 1 :color "gray35")
                            :weight normal :foreground "#94BFF3"
                            :background "#383838"))))
        '(mode-line-inactive ((t (:box (:line-width 1 :color "gray35")
                                     :weight normal :foreground "gray70"
                                     :background "#383838"))))
        '(mode-line-buffer-id ((t (:foreground "#FFEBB9" :weight bold))))

        '(powerline-active1 ((t (:background "gray35" :foreground "#BFEBBF"))))
        '(powerline-active2 ((t (:background "gray35" :foreground "#BFEBBF"))))
        '(powerline-inactive1 ((t (:background "gray35"))))
        '(powerline-inactive2 ((t (:background "#383838"))))

        ;; ivy minibuffer config
        '(ivy-minibuffer-match-face-4 ((t (:background "pink4" :underline t))))
        '(ivy-minibuffer-match-face-3 ((t (:background "SteelBlue3" :underline t))))
        '(ivy-minibuffer-match-face-2 ((t (:background "DarkSeaGreen4" :underline t))))
        '(ivy-current-match ((t (:foreground "#F0DFAF" :underline nil :weight bold))))
        '(golden-ratio-scroll-highlight-line-face ((t (:background "gray27"
                                                          :weight normal))))
        ;; magit faces
        '(magit-popup-disabled-argument ((t (:foreground "gray55"))))
        '(magit-popup-key ((t (:foreground "#BFEBBF"))))
        '(magit-section-highlight ((t (:background "gray27"))))
        '(magit-diff-file-heading-highlight ((t (:background "gray27"))))
        '(magit-diff-hunk-heading-highlight ((t (:background "gray27"))))
        '(magit-diff-context-highlight ((t (:background "gray27"))))

        ;; make function face brighter so it's easily distinguishable
        '(font-lock-function-name-face ((t (:foreground "CadetBlue1"))))

        ;; fontify links to make them standout
        '(link ((t (:foreground "#C9B8A2"
                       :underline nil :weight normal))))
        '(link-visited ((t (:foreground "C9AE8C"
                               :underline nil :weight normal))))

        ;; make everything look gray
        '(font-lock-comment-delimiter-face ((t (:foreground "gray55"))))
        '(font-lock-comment-face ((t (:foreground "gray55"))))
        '(font-lock-doc-face ((t (:foreground "gray70"))))
        '(shm-current-face ((t (:background "gray27"))))
        '(hl-line ((t (:background "gray27"))))
        '(fringe ((t (:background "gray27"))))
        '(vhl/default-face ((t (:background "gray27"))))
        '(vertical-border ((t (:foreground "gray20"))))

        ;; strike through unmatched parenthesis
        '(rainbow-delimiters-unmatched-face ((t (:foreground "red" :inherit unspecified
                                                    :strike-through t))))

        ;; org-mode face
        '(org-checkbox ((t (:foreground "gray70" :background nil
                               :weight bold :box nil))))
        '(org-priority ((t (:foreground "gray70" :weight bold
                               :inherit nil))))
        '(org-date ((((class color)) (:underline nil))))

        ;; term face config
        '(term ((t (:foreground "#E5D9BD"))))
        '(term-color-green ((t (:background "grey30" :foreground "#9F8300"))))

        ;; markdown header face config
        '(markdown-header-face-1 ((t (:foreground "#DFAF8F" :weight bold :height 1.8))))
        '(markdown-header-face-2 ((t (:foreground "#BFEBBF" :weight bold :height 1.6))))
        '(markdown-header-face-3 ((t (:foreground "#7CB8BB" :weight bold :height 1.4))))
        '(markdown-header-face-4 ((t (:foreground "#D0BF8F" :weight bold :height 1.2))))
        '(markdown-header-face-5 ((t (:foreground "#93E0E3" :weight bold :height 1.1))))
        '(markdown-header-face-6 ((t (:foreground "#9FC59F" :weight bold))))))

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
^ ^           ^ ^           _k_: Doom molokai
^ ^           ^ ^           _g_: Doom Space gray
"
        ("s" (load-theme 'solarized-dark  t))
        ("S" (load-theme 'solarized-light t))
        ("m" (load-theme 'material        t))
        ("M" (load-theme 'material-light  t))
        ("d" (load-theme 'doom-solarized-dark  t))
        ("D" (load-theme 'doom-solarized-light t))
        ("o" (load-theme 'doom-one  t))
        ("O" (load-theme 'doom-one-light t))
        ("n" (load-theme 'doom-nord t))
        ("N" (load-theme 'doom-nord-light t))
        ("p" (load-theme 'doom-opera t))
        ("P" (load-theme 'doom-opera-light t))
        ("k" (load-theme 'doom-molokai t))
        ("g" (load-theme 'doom-spacegrey t))
        ("z" (load-theme 'zenburn         t))
        ("DEL" (jv/disable-all-themes))
        ("RET" nil "done" :color blue)))

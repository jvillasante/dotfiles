;;; +bindings.el --- description -*- lexical-binding: t; -*-

(map!
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Defaults
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    "M-s-<right>" #'other-frame
    "M-s-<left>"  #'other-frame

    "s-<up>"    #'windmove-up
    "s-<right>" #'windmove-right
    "s-<left>"  #'windmove-left
    "s-<down>"  #'windmove-down

    "C-s"       #'swiper-isearch
    "C-r"       #'swiper-isearch-backward
    "C-S-s"     #'isearch-forward
    "C-x k"     #'ido-kill-buffer
    "C-x K"     #'doom/kill-this-buffer-in-all-windows
    "C-x b"     #'switch-to-buffer
    "C-x B"     #'persp-switch-to-buffer

    "M-l"       #'pyim-convert-string-at-point
    "C-;"       #'pyim-delete-word-from-personal-buffer

    "C-c C-y"   #'company-yasnippet

    ;; ;;;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Doom
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (:leader
        (:prefix ("a" . "application")
            ;; :desc "Elfeed Rss Reader" :n "r" #'elfeed
            :desc "Elfeed Rss Reader" :n "r" #'+my/elfeed-load-db-and-open
            :desc "Deft"              :n "n" #'deft)

        (:prefix "b"
            :desc "Rename buffer" :n "R" #'rename-buffer
            :desc "Kill buffer"   :n "d" #'kill-this-buffer ; consistency with `SPC w d'
            :desc "Ibuffer"       :n "I" #'ibuffer)

        (:prefix "o"
            :desc "Dired" :n "d" #'dired
            :desc "Scratch Buffer" :n "s" #'doom/open-scratch-buffer
            (:after sr-speedbar
                :desc "Open speedbar" :n "x" #'sr-speedbar-toggle)
            (:when (featurep! :ui neotree)
                :desc "Open neotree"    :n "n" #'neotree-toggle
                ;; :desc "Open neotree"    :n "n" #'+neotree/open
                ;; :desc "File in neotree" :n "N" #'+neotree/find-this-file
                )
            (:when (featurep! :ui treemacs)
                :desc "Toggle treemacs"  :n "n" #'+treemacs/toggle
                :desc "File in treemacs" :n "N" #'+treemacs/find-file))

        (:prefix "w"
            :desc "Popup raise"    :n "p" #'+popup/raise
            :desc "Maximize frame" :n "M" #'toggle-frame-maximized
            :desc "Delete window"  :n "d" #'evil-quit)

        (:prefix ("f" . "file")
            :desc "Find git file" :n "g" #'counsel-git)

        (:prefix ("d" . "doom")
            :desc "Dashboard"                   "d" #'+doom-dashboard/open
            :desc "Recent files"                "f" #'recentf-open-files
            :desc "Popup other"                 "o" #'+popup/other
            :desc "Popup toggle"                "t" #'+popup/toggle
            :desc "Popup close"                 "c" #'+popup/close
            :desc "Popup close all"             "C" #'+popup/close-all
            :desc "Popup raise"                 "r" #'+popup/raise
            :desc "Popup restore"               "R" #'+popup/restore
            :desc "Scratch buffer"              "s" #'doom/open-scratch-buffer
            :desc "Switch to scratch buffer"    "S" #'doom/switch-to-scratch-buffer
            :desc "Sudo this file"              "u" #'doom/sudo-this-file
            :desc "Sudo find file"              "U" #'doom/sudo-find-file
            :desc "Terminal open popup"         "l" #'multi-term-dedicated-toggle
            :desc "Terminal open"               "L" #'multi-term
            :desc "Reload Private Config"       "R" #'doom/reload
            :desc "Open Lisp REPL"              ";" #'+eval/open-repl
            :desc "Toggle frame fullscreen"     "F" #'toggle-frame-fullscreen
            :desc "Toggle modal edition mode"   "m" #'modalka-global-mode)
        )

    (:after ivy
        (:leader
            (:prefix "/"
                :desc "Find in project" :n "/" #'+ivy/project-search)))

    (:after helm
        (:leader
            (:prefix "/"
                :desc "Find in project" :n "/" #'+helm/project-search)))

    (:after helm-files
        (:map helm-find-files-map
            :desc "Up one directory" :n "C-h" #'helm-find-files-up-one-level))

    (:after dired
        (:map dired-mode-map
            :desc "Dired Hydra" :n "." #'+my/hydra-dired/body))

    (:after neotree
        (:map neotree-mode-map
            :desc "Neotree Hydra" :n "?" #'+my/hydra-neotree/body))

    )

;;; -*- lexical-binding: t; -*-

(map!
    :desc "Redo" :n "U" #'undo-tree-redo

    (:after treemacs-evil
        (:map evil-treemacs-state-map
            "C-h" #'evil-window-left
            "C-l" #'evil-window-right))

    (:after ivy
        (:leader
            (:prefix "/"
                :desc "Find in project" :n "/" #'+ivy/project-search)))

    ;; (:after helm
    ;;   (:leader
    ;;     (:prefix "/"
    ;;     :desc "Find in project" :n "/" #'+helm/project-search)))
    ;;
    ;; (:after helm-files
    ;;   (:map helm-find-files-map
    ;;     :desc "Up one directory" "C-h" #'helm-find-files-up-one-level))

    (:leader
        (:prefix "b"
            :desc "Rename buffer" :n "R" #'rename-buffer
            :desc "Kill buffer" :n "d" #'kill-this-buffer ; consistency with `SPC w d'
            )
        (:prefix "o"
            :desc "Open directory browser" :n "b" #'treemacs
            )
        (:prefix "w"
            :desc "Maximize frame" :n "M" #'toggle-frame-maximized)))

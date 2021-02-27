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

    "C-c C-y"   #'company-yasnippet
    "C-c o"     #'crux-open-with

    ;; ;;;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Doom
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (:leader
        ;;;; Applications
        (:prefix ("a" . "Application")
            :desc "Email"             :n "m" #'mu4e
            :desc "Elfeed Rss Reader" :n "f" #'elfeed
            :desc "Deft"              :n "n" #'deft)

        ;;;; Buffers
        (:prefix ("b" . "Buffers")
            ;; :desc "Switch buffer"           :n "b" #'+ivy/switch-buffer
            ;; :desc "Switch workspace buffer" :n "B" #'+ivy/switch-workspace-buffer
            :desc "Rename buffer"           :n "R" #'rename-buffer
            :desc "Kill buffer"             :n "d" #'kill-this-buffer ; consistency with `SPC w d'
            :desc "Ibuffer"                 :n "I" #'ibuffer)

        ;;;; Open
        (:prefix ("o" . "Open")
            :desc "Dired"  :n "d" #'dired
            ;; :desc "Ranger" :n "r" #'ranger
            :desc "Pass"   :n "p" #'pass
            ;; :desc "Tramp"  :n "t" #'counsel-tramp
            (:when (featurep! :ui neotree)
                :desc "Toogle neotree"  :n "n" #'neotree-toggle
                :desc "File in neotree" :n "N" #'+neotree/find-this-file)
            (:when (featurep! :ui treemacs)
                :desc "Initialize or toggle treemacs"      :n "n" #'treemacs
                :desc "Doom initialize or toggle treemacs" :n "N" #'+treemacs/toggle))

        ;;;; Windows
        (:prefix ("w" . "Windows")
            :desc "Popup raise"    :n "p" #'+popup/raise
            :desc "Maximize frame" :n "M" #'toggle-frame-maximized
            :desc "Delete window"  :n "d" #'evil-quit)

        ;;; Files
        (:prefix ("f" . "File")
            :desc "Find git file" :n "g" #'counsel-git)

        ;;;; Doom
        (:prefix ("d" . "Doom")
            :desc "Dashboard"                 :n  "d" #'+doom-dashboard/open
            :desc "Recent files"              :n  "f" #'recentf-open-files
            :desc "Popup other"               :n  "o" #'+popup/other
            :desc "Popup toggle"              :n  "t" #'+popup/toggle
            :desc "Popup close"               :n  "c" #'+popup/close
            :desc "Popup close all"           :n  "C" #'+popup/close-all
            :desc "Popup raise"               :n  "r" #'+popup/raise
            :desc "Popup restore"             :n  "R" #'+popup/restore
            :desc "Scratch buffer"            :n  "s" #'doom/open-scratch-buffer
            :desc "Switch to scratch buffer"  :n  "S" #'doom/switch-to-scratch-buffer
            :desc "Sudo this file"            :n  "u" #'doom/sudo-this-file
            :desc "Sudo find file"            :n  "U" #'doom/sudo-find-file
            :desc "Reload Private Config"     :n  "R" #'doom/reload
            :desc "Open Lisp REPL"            :n  ";" #'+eval/open-repl
            :desc "Toggle frame fullscreen"   :n  "F" #'toggle-frame-fullscreen)

        ;;;; Code
        (:prefix ("c" . "Code")
            :desc "Compile"                     :n "c" #'compile
            :desc "Make"                        :n "m" #'+make/run
            :desc "Jump to definition"          :n "d" #'+lookup/definition
            :desc "Jump to references"          :n "D" #'+lookup/references
            :desc "Evaluate buffer/region"      :n "e" #'+eval/buffer-or-region
            :desc "Evaluate & replace region"   :n "E" #'+eval:replace-region
            :desc "Format buffer/region"        :n "f" #'+format/region-or-buffer
            :desc "Open REPL"                   :n "r" #'+eval/open-repl-other-window
            :desc "Delete trailing whitespace"  :n "w" #'delete-trailing-whitespace
            :desc "Delete trailing newlines"    :n "W" #'doom/delete-trailing-newlines
            :desc "List errors"                 :n "x" #'flycheck-list-errors
            :desc "Describe thing at point"     :n "t" #'lsp-describe-thing-at-point
            :desc "Lookup dash documentation"   :n "s" #'+lookup:dash
            :desc "Restart LSP Workspace"       :n "r" #'lsp-workspace-restart
            (:prefix ("g" . "Go to")
                :desc "Implementation"          :n "i" #'lsp-goto-implementation
                :desc "Definition"              :n "d" #'lsp-goto-type-definition
                :desc "Find Definition"         :n "D" #'lsp-find-definition
                :desc "Find References"         :n "r" #'lsp-find-references)
            (:prefix ("p" . "Peek")
                :desc "Implementation"          :n "i" #'lsp-ui-peek-find-implementation
                :desc "Definition"              :n "d" #'lsp-ui-peek-find-definitions
                :desc "Reference"               :n "r" #'lsp-ui-peek-find-references)
            ;; (:prefix ("l" . "Lens")
            ;;     :desc "Show"                    :n "l" #'lsp-lens-show
            ;;     :desc "Hide"                    :n "q" #'lsp-lens-hide)
            (:prefix ("u" . "menu")
                :desc "Show"                    :n "m" #'lsp-ui-imenu
                :desc "Hide"                    :n "q" #'lsp-ui-imenu--kill))

        ;;;; Search
        (:prefix ("s" . "Search")
            :desc "Search directory"             "d" #'+ivy/project-search-from-cwd
            :desc "Search directory (all files)" "D" (cmd! (+ivy/project-search-from-cwd t))
            :desc "Search project"               "p" #'+ivy/project-search
            :desc "Search project (all files)"   "P" (cmd! (+ivy/project-search t)))

        ;;;; Quit
        (:prefix ("q" . "Quit")
            :desc "Restart Emacs"                "r" #'doom/restart
            :desc "Restart & restore Emacs"      "R" #'doom/restart-and-restore
            :desc "Save buffers and kill server" "Q" #'save-buffers-kill-emacs)

        ;;;; Hydra
        (:leader
            (:when (featurep! :ui hydra)
                (:prefix "w"
                    :desc "Interactive menu"    "w" #'+hydra/window-nav/body)
                (:prefix ("z" . "zoom")
                    :desc "Text"                "t" #'+hydra/text-zoom/body)))

        ;;;; Isearch
        (:after isearch
            (:map isearch-mode-map
                [return] #'+isearch-exit-start-of-match
                "RET"    #'+isearch-exit-start-of-match
                "C-RET"  #'isearch-exit))

        ;;;; Ivy
        (:after ivy
            (:map ivy-minibuffer-map
                [return] #'ivy-alt-done
                "RET"    #'ivy-alt-done))

        ;;;; LSP UI
        (:after lsp-ui
            (:map lsp-ui-mode-map
                [remap xref-find-definitions] #'lsp-ui-peek-find-definitions
                [remap xref-find-references] #'lsp-ui-peek-find-references))

        ;;;; Flycheck
        (:after flycheck
            (:map flycheck-mode-map
                "M-n" #'flycheck-next-error
                "M-p" #'flycheck-previous-error))

        ;;;; Languages
        ;;;;; Rust
        (:after rustic
            (:map rustic-mode-map
                :localleader
                (:prefix ("r" . "Rustic")
                    :desc "Clippy pretty"     "C" #'rustic-cargo-clippy
                    :desc "Popup"             "r" #'rustic-popup
                    :desc "Format everything" "f" #'rustic-cargo-fmt
                    :desc "Cargo-outdated"    "u" #'rustic-cargo-outdated)))
        )

    (:after dired
        (:map dired-mode-map
            :desc "Dired single up" :n "h" #'dired-single-up-directory
            :desc "Dired single buffer" :n "l" #'dired-single-buffer
            :desc "Dired hide dotfiles" :n "H" #'dired-hide-dotfiles-mode
            :desc "Dired Hydra" :n "?" #'+my/hydra-dired/body
            :desc "Dired Quicksort Hydra" :n "s" #'hydra-dired-quick-sort/body))

    (:after org
        (:map org-mode-map
            :desc "Org Hydra" :n "?" #'+my/hydra-org/body))

    (:after neotree
        (:map neotree-mode-map
            :desc "Neotree Stretch" :n [tab] #'neotree-stretch-toggle
            :desc "Neotree Hydra" :n "?" #'+my/hydra-neotree/body))

    (:after elfeed
        (:map elfeed-search-mode-map
            :desc "Elfeed update"         :n "gr" #'elfeed-update
            :desc "Elfeed mark read"      :n "r"  #'elfeed-search-untag-all-unread
            :desc "Elfeed mark unread"    :n "u"  #'elfeed-search-tag-all-unread
            :desc "Elfed open in browser" :n "b"  #'elfeed-search-browse-url
            :desc "Elfeed quit"           :n "q"  #'elfeed-kill-buffer))
    )

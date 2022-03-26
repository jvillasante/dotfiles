;;; +bindings.el --- description -*- lexical-binding: t; -*-

(defun +jv/org-capture-inbox ()
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "i"))
(defun +jv/org-capture-notes()
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "n"))

(map!
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Defaults
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    "C-c C-y"   #'company-yasnippet
    "C-c o"     #'crux-open-with
    "C-c u"     #'browse-url-at-point

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Org Capture (GTD)
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    "C-c c" #'org-capture
    "C-c a" #'org-agenda
    "C-c i" #'+jv/org-capture-inbox
    "C-c n" #'+jv/org-capture-notes

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Vterm toggle quick
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (:when (featurep! :term vterm)
        :n "C-t" #'+vterm/toggle)

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Spell change dictionary
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (:when (featurep! :checkers spell)
        :n "zd" #'ispell-change-dictionary)

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Evil multiple cursors hydra
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (:when (featurep! :editor multiple-cursors)
        :prefix "g"
        :nv "z" #'+my/hydra-evil-mc/body)

    ;; ;;;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Doom
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (:leader
        ;;;; Buffers
        (:prefix ("b" . "Buffers")
            :desc "Rename buffer" :n "R" #'rename-buffer
            :desc "Kill buffer"   :n "d" #'kill-this-buffer ; consistency with `SPC w d'
            :desc "Ibuffer"       :n "I" #'+ibuffer/open-for-current-workspace)

        ;;;; Open
        (:prefix ("o" . "Open")
            :desc "Dired Jump" :n "d" #'dired-jump
            :desc "Dired"      :n "D" #'dired
            :desc "Pass"       :n "p" #'pass
            :desc "iElm"       :n "i" #'ielm
            :desc "Calc"       :n "c" #'calc
            :desc "Quick Calc" :n "C" #'quick-calc
            (:when (featurep! :ui neotree)
                :desc "Toogle neotree"  :n "n" #'neotree-toggle
                :desc "File in neotree" :n "N" #'+neotree/find-this-file)
            (:when (featurep! :ui treemacs)
                :desc "Initialize or toggle treemacs"      :n "n" #'+treemacs/toggle
                :desc "Doom initialize or toggle treemacs" :n "N" #'treemacs-find-file)
            (:prefix ("a" . "Application")
                :desc "Mu4e"               :n "m" #'mu4e
                :desc "Elfeed Rss Reader"  :n "f" #'elfeed
                :desc "Deft"               :n "n" #'deft))

        ;;;; Windows
        (:prefix ("w" . "Windows")
            :desc "Popup raise"    :n "p" #'+popup/raise
            :desc "Maximize frame" :n "M" #'toggle-frame-maximized
            :desc "Delete window"  :n "d" #'evil-quit)

        ;;;; Doom
        (:prefix ("d" . "Doom")
            :desc "Dashboard"                 :n  "d" #'+doom-dashboard/open
            :desc "Reload Private Config"     :n  "R" #'doom/reload
            :desc "Recent files"              :n  "f" #'recentf-open-files
            :desc "Popup other"               :n  "o" #'+popup/other
            :desc "Popup toggle"              :n  "t" #'+popup/toggle
            :desc "Popup close"               :n  "c" #'+popup/close
            :desc "Popup close all"           :n  "C" #'+popup/close-all
            :desc "Popup raise"               :n  "p" #'+popup/raise
            :desc "Popup restore"             :n  "P" #'+popup/restore
            :desc "Scratch buffer"            :n  "s" #'doom/open-scratch-buffer
            :desc "Switch to scratch buffer"  :n  "S" #'doom/switch-to-scratch-buffer
            :desc "Sudo this file"            :n  "u" #'doom/sudo-this-file
            :desc "Sudo find file"            :n  "U" #'doom/sudo-find-file
            :desc "Toggle frame fullscreen"   :n  "F" #'toggle-frame-fullscreen)

        ;;;; Code
        (:prefix ("c" . "Code")
            :desc "Compile"                     :n "c" #'compile
            :desc "Make"                        :n "m" #'+make/run
            :desc "Jump to definition"          :n "d" #'+lookup/definition
            :desc "Jump to references"          :n "D" #'+lookup/references
            :desc "Lookup zeal documentation"   :n "z" #'zeal-at-point
            :desc "Format buffer/region"        :n "f" #'+format/region-or-buffer
            :desc "List errors"                 :n "e" #'flycheck-list-errors
            :desc "Describe thing at point"     :n "t" #'lsp-describe-thing-at-point
            :desc "LSP Rename"                  :n "r" #'lsp-rename
            :desc "Restart LSP Workspace"       :n "q" #'lsp-workspace-restart
            :desc "Shutdown LSP Workspace"      :n "Q" #'lsp-workspace-shutdown
            (:prefix ("g" . "Go to")
                :desc "Implementation"          :n "i" #'lsp-goto-implementation
                :desc "Definition"              :n "d" #'lsp-goto-type-definition
                :desc "Find Definition"         :n "D" #'lsp-find-definition
                :desc "Find References"         :n "r" #'lsp-find-references)
            (:prefix ("p" . "Peek")
                :desc "Implementation"          :n "i" #'lsp-ui-peek-find-implementation
                :desc "Definition"              :n "d" #'lsp-ui-peek-find-definitions
                :desc "Reference"               :n "r" #'lsp-ui-peek-find-references)
            (:prefix ("u" . "menu")
                :desc "Show"                    :n "m" #'lsp-ui-imenu
                :desc "Hide"                    :n "q" #'lsp-ui-imenu--kill))

        ;;; Notes
        (:prefix ("n" . "Notes")
            :desc "Open deft" :n "D" #'deft
            :desc "Find deft file" :n "d" #'deft-find-file)

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

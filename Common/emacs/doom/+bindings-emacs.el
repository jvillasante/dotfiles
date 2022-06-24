;;; +bindings-emacs.el --- Vanilla Emacs oriented bindings -*- lexical-binding: t; -*-

;;;; Global & plugin bindings
(map!
    "C-x C-m" #'execute-extended-command  ;; ALT may or may not be available
    "C-c C-m" #'execute-extended-command  ;; ALT may or may not be available
    "C-c u"   #'browse-url-at-point       ;; browse url with default browser
    "C-x k"   #'kill-this-buffer          ;; kill buffer without prompt
    "C-x K"   #'kill-buffer               ;; prompt for buffer to kill

    ;;;; zap
    "M-S-z" #'zap-up-to-char ;; New in Emacs 28

    ;;;; hippie-expand is a better dabbrev
    [remap dabbrev-expand] #'hippie-expand

    ;;;; fill-unfill
    [remap fill-paragraph] #'+my/fill-or-unfill
    "M-Q"                  #'+my/unfill-paragraph
    "C-M-Q"                #'+my/unfill-region
    (:after org
        :map org-mode-map
        [remap fill-paragraph] #'+my/org-fill-or-unfill)

    ;;;; avy
    "C-," #'avy-goto-char-timer ;; most useful avy function

    ;;;; expand-region
    "C-=" #'er/expand-region

    ;;;; crux
    "C-o" #'crux-smart-open-line
    "M-o" #'crux-smart-open-line-above
    "C-^" #'crux-top-join-line
    "C-k" #'crux-smart-kill-line
    "C-x C-u" #'crux-upcase-region
    "C-x C-l" #'crux-downcase-region
    [remap kill-whole-line] #'crux-kill-whole-line

    (:when (daemonp)
        "C-z" nil)   ;; suspend-frame does not work well with daemon

    (:after isearch
        :map isearch-mode-map
        "C-n" #'isearch-repeat-forward
        "C-p" #'isearch-repeat-backward)

    (:after smartparens
        :map smartparens-mode-map
        "C-M-<backspace>" #'sp-backward-kill-sexp)

    (:when (featurep! :emacs undo)
        (:after undo-fu
            "C-/" #'undo-fu-only-undo
            "C-?" #'undo-fu-only-redo))

    (:after dired
        :map dired-mode-map
        "C-u o" #'crux-open-with)

    (:after neotree
        :map neotree-mode-map
        :desc "Neotree stretch" [tab] #'neotree-stretch-toggle)

    (:after elfeed
        :map elfeed-search-mode-map
        :desc "Elfeed update"         "U"  #'elfeed-update
        :desc "Elfeed mark read"      "r"  #'elfeed-search-untag-all-unread
        :desc "Elfeed mark unread"    "u"  #'elfeed-search-tag-all-unread
        :desc "Elfed open in browser" "b"  #'elfeed-search-browse-url
        :desc "Elfeed quit"           "q"  #'elfeed-kill-buffer)
    )

;;;; Leader key bindings (C-c)
(map! :leader
    (:prefix-map ("d" . "doom")
        :desc "Dashboard"                 "d" #'+doom-dashboard/open
        :desc "Reload Private Config"     "r" #'doom/reload
        :desc "Recent files"              "f" #'recentf-open-files
        :desc "Popup other"               "o" #'+popup/other
        :desc "Popup toggle"              "t" #'+popup/toggle
        :desc "Popup close"               "c" #'+popup/close
        :desc "Popup close all"           "C" #'+popup/close-all
        :desc "Popup raise"               "p" #'+popup/raise
        :desc "Popup restore"             "P" #'+popup/restore
        :desc "Scratch buffer"            "s" #'doom/open-scratch-buffer
        :desc "Switch to scratch buffer"  "S" #'doom/switch-to-scratch-buffer
        :desc "Sudo this file"            "u" #'doom/sudo-this-file
        :desc "Sudo find file"            "U" #'doom/sudo-find-file
        :desc "Toggle frame fullscreen"   "F" #'toggle-frame-fullscreen)

    (:prefix-map ("o" . "open")
        "p" nil ;; unbind project drawer
        "P" nil ;; unbind project drawer

        :desc "iElm" "i"       #'ielm
        :desc "Calc" "c"       #'calc
        :desc "Quick Calc" "C" #'quick-calc
        (:when (featurep! :ui neotree)
            :desc "Toggle Neotree"       "n" #'neotree-toggle
            :desc "Find file in Neotree" "N" #'+neotree/find-this-file)
        (:when (featurep! :ui treemacs)
            :desc "Toggle Treemacs"       "n" #'treemacs
            :desc "Doom toggle Treemacs"  "N" #'+treemacs/toggle)
        (:prefix-map ("p" . "Password Store")
            :desc "Clear secret in the kill ring"             "C" #'password-store-clear
            :desc "Add password for ENTRY into the kill ring" "c" #'password-store-copy
            :desc "Add FIELD for ENTRY into the kill ring"    "f" #'password-store-copy-field
            :desc "Edit password for ENTRY"                   "e" #'password-store-edit
            :desc "Insert a new ENTRY containing PASSWORD"    "i" #'password-store-insert
            :desc "Generate a new password for ENTRY"         "g" #'password-store-generate
            :desc "Rename ENTRY to NEW-ENTRY"                 "r" #'password-store-rename
            :desc "Remove existing password for ENTRY"        "R" #'password-store-remove)
        (:prefix-map ("a" . "Application")
            :desc "Mu4e"               "m" #'mu4e
            :desc "Elfeed Rss Reader"  "f" #'elfeed
            :desc "Deft"               "n" #'deft))

    (:prefix-map ("c" . "Code")
        :desc "Make run"                  "m" #'+make/run
        :desc "Make run last"             "M" #'+make/run-last
        :desc "Lookup zeal documentation" "z" #'zeal-at-point
        :desc "Flycheck list errors"      "x" #'flycheck-list-errors
        :desc "Restart LSP Workspace"     "q" #'lsp-workspace-restart
        :desc "Shutdown LSP Workspace"    "Q" #'lsp-workspace-shutdown
        (:prefix-map ("u" . "Menu")
            :desc "Show" "m" #'lsp-ui-imenu
            :desc "Hide" "q" #'lsp-ui-imenu--kill))

    (:prefix-map ("n" . "Notes")
        :desc "Find deft file" "d" #'deft-find-file
        :desc "Open deft"      "D" #'deft)
    )


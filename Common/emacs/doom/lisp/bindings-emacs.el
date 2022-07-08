;;; lisp/bindings-emacs.el --- Vanilla Emacs Keybindings -*- lexical-binding: t; -*-

;;; Global bindings
(map!
    "C-x C-m" #'execute-extended-command  ;; ALT may or may not be available
    "C-c C-m" #'execute-extended-command  ;; ALT may or may not be available
    "C-c u"   #'browse-url-at-point       ;; browse url with default browser
    "C-x k"   #'kill-this-buffer          ;; kill buffer without prompt
    "C-x K"   #'kill-buffer               ;; prompt for buffer to kill

    ;;; suspend-frame does not work well with daemon
    (:when (daemonp)
        "C-z" nil)

    ;;; zap
    "M-S-z" #'zap-up-to-char ;; New in Emacs 28

    ;;; hippie-expand is a better dabbrev
    [remap dabbrev-expand] #'hippie-expand

    ;;; fill-unfill
    [remap fill-paragraph] #'+my/fill-or-unfill
    "M-Q"                  #'+my/unfill-paragraph
    "C-M-Q"                #'+my/unfill-region
    (:after org
        :map org-mode-map
        [remap fill-paragraph] #'+my/org-fill-or-unfill)

    ;;; windows
    (:map ctl-x-4-map
        "t" #'+my/toggle-window-split)

    ;; (:when (featurep! :ui window-select)
    ;;     (:when 'ace-window
    ;;         [remap other-window] #'other-window
    ;;         "M-o" #'ace-window))

    ;;; avy
    (:when 'avy
        "C-." #'avy-goto-char-timer)

    ;;; expand-region
    (:when 'expand-region
        "C-=" #'er/expand-region)

    ;;; crux
    (:when 'crux
        "C-o" #'crux-smart-open-line
        "C-^" #'crux-top-join-line
        "C-k" #'crux-smart-kill-line
        "C-x C-u" #'crux-upcase-region
        "C-x C-l" #'crux-downcase-region
        [remap kill-whole-line] #'crux-kill-whole-line)

    ;;; isearch
    (:after isearch
        :map isearch-mode-map
        ;; Prevents issue where you have to press backspace twice when
        ;; trying to remove the first character that fails a search
        [remap isearch-delete-char] #'isearch-del-char

        ;; Better navigation
        "C-n" #'isearch-repeat-forward
        "C-p" #'isearch-repeat-backward)

    ;;; smartparens
    (:after smartparens
        :map smartparens-mode-map
        "C-M-<backspace>" #'sp-backward-kill-sexp)

    ;;; undo
    (:when (featurep! :emacs undo)
        (:when 'undo-fu
            "C-/" #'undo-fu-only-undo
            "C-?" #'undo-fu-only-redo))

    ;;; vterm
    (:when (featurep! :term vterm)
        (:when 'vterm
            :map vterm-mode-map
            "C-y" #'vterm-yank))

    ;;; dired
    (:after dired
        :map dired-mode-map
        "C-u o" #'crux-open-with)

    ;;; neotree
    (:when (featurep! :ui neotree)
        (:after neotree
            :map neotree-mode-map
            :desc "Neotree stretch" [tab] #'neotree-stretch-toggle))
    )

;;; Leader key bindings (C-c)
(map! :leader
    (:prefix ("f" . "file")
        :desc "Save file" "s" #'save-buffer
        :desc "Save all"  "S" #'+my/save-all)

    (:prefix ("o" . "open")
        "p" nil ;; unbind project drawer
        "P" nil ;; unbind project drawer

        :desc "Calc"       "c" #'calc
        :desc "Quick Calc" "C" #'quick-calc
        :desc "Elfeed"     "f" #'elfeed
        (:when (featurep! :ui neotree)
            :desc "Toggle Neotree"       "p" #'neotree-toggle
            :desc "Find file in Neotree" "P" #'+neotree/find-this-file)
        (:when (featurep! :ui treemacs)
            :desc "Toggle Treemacs"       "p" #'treemacs
            :desc "Doom toggle Treemacs"  "P" #'+treemacs/toggle))

    (:prefix ("c" . "Code")
        :desc "Make run"                  "m" #'+make/run
        :desc "Make run last"             "M" #'+make/run-last
        :desc "Lookup zeal documentation" "z" #'zeal-at-point
        :desc "Flycheck list errors"      "x" #'flycheck-list-errors
        :desc "Restart LSP Workspace"     "q" #'lsp-workspace-restart
        :desc "Shutdown LSP Workspace"    "Q" #'lsp-workspace-shutdown
        (:prefix-map ("u" . "Menu")
            :desc "Show" "m" #'lsp-ui-imenu
            :desc "Hide" "q" #'lsp-ui-imenu--kill))

    (:prefix ("n" . "Notes")
        "d" nil ;; unbind deft

        :desc "Find deft file" "d" #'deft-find-file
        :desc "Open deft"      "D" #'deft)
    )

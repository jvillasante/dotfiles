;;; lisp/bindings-emacs.el --- Vanilla Emacs Keybindings -*- lexical-binding: t; -*-

;;; Global bindings
(map!
    "C-x C-m" #'execute-extended-command  ;; ALT may or may not be available
    "C-c C-m" #'execute-extended-command  ;; ALT may or may not be available
    "C-c u"   #'browse-url-at-point       ;; browse url with default browser
    "C-x k"   #'kill-this-buffer          ;; kill buffer without prompt
    "C-x K"   #'kill-buffer               ;; prompt for buffer to kill
    "C-x S"   #'+my/save-all              ;; save some buffers without prompt
    "C-z"     nil                         ;; suspend frame should go away
    "C-x C-z" nil                         ;; same

    ;;; upcase, downcase and capitalize
    "M-u" #'upcase-dwim
    "M-l" #'downcase-dwim
    "M-c" #'capitalize-dwim

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

    ;;; avy
    (:when 'avy
        "C-." #'avy-goto-char-timer)

    ;;; anzu
    (:when 'anzu
        "M-%" #'anzu-query-replace
        "C-M-%" #'anzu-query-replace-regexp)

    ;;; expand-region
    (:when 'expand-region
        "C-=" #'er/expand-region
        "C--" #'er/contract-region)

    ;;; crux
    (:when 'crux
        "C-o" #'crux-smart-open-line
        "C-^" #'crux-top-join-line
        "C-k" #'crux-smart-kill-line
        [remap kill-whole-line] #'crux-kill-whole-line)

    ;;; consult
    (:when 'consult
        "C-x C-r" #'consult-recent-file)

    ;;; isearch
    (:after isearch
        :map isearch-mode-map
        ;; Prevents issue where you have to press backspace twice when
        ;; trying to remove the first character that fails a search
        [remap isearch-delete-char] #'isearch-del-char

        ;; Better navigation
        "C-n" #'isearch-repeat-forward
        "C-p" #'isearch-repeat-backward)

    ;;; ibuffer
    (:after ibuffer
        :map ibuffer-mode-map
        "q" #'kill-this-buffer)

    ;;; smartparens
    (:after smartparens
        :map smartparens-mode-map
        "C-M-<backspace>" #'sp-backward-kill-sexp)

    ;;; undo
    (:when (modulep! :emacs undo)
        (:when 'undo-fu
            "C-/" #'undo-fu-only-undo
            "C-?" #'undo-fu-only-redo))

    ;;; vterm
    (:when (modulep! :term vterm)
        (:when 'vterm
            :map vterm-mode-map
            "M-[" #'vterm-copy-mode
            "C-y" #'vterm-yank))

    ;;; dired
    (:after dired
        :map dired-mode-map
        "C-u C-o" #'crux-open-with)

    ;;; neotree
    (:when (modulep! :ui neotree)
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
        (:when (modulep! :ui neotree)
            :desc "Toggle Neotree"       "p" #'neotree-toggle
            :desc "Find file in Neotree" "P" #'+neotree/find-this-file)
        (:when (modulep! :ui treemacs)
            :desc "Toggle Treemacs"       "p" #'treemacs
            :desc "Doom toggle Treemacs"  "P" #'+treemacs/toggle))

    (:prefix ("c" . "Code")
        :desc "Make run"                  "m" #'+make/run
        :desc "Make run last"             "M" #'+make/run-last
        :desc "Lookup zeal documentation" "z" #'zeal-at-point)

    (:prefix ("n" . "Notes")
        "d" nil ;; unbind deft

        :desc "Find deft file" "d" #'deft-find-file
        :desc "Open deft"      "D" #'deft)
    )

;;; hooks
(add-hook 'c-mode-common-hook
    (lambda()
        (local-set-key  (kbd "C-c C-o") #'ff-find-other-file)))

;;; Projectile prefix on `C-x p'
(define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)

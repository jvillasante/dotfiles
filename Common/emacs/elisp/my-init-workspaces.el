;;; my-init-workspaces.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(use-package activities
    :disabled t
    :init
    (activities-mode)
    (activities-tabs-mode)
    (setq edebug-inhibit-emacs-lisp-mode-bindings t)
    :custom
    (activities-bookmark-store nil)
    (activities-always-persist nil)
    :bind
    (("C-x C-a C-n" . activities-new)
     ("C-x C-a C-d" . activities-define)
     ("C-x C-a C-a" . activities-resume)
     ("C-x C-a C-s" . activities-suspend)
     ("C-x C-a C-k" . activities-kill)
     ("C-x C-a RET" . activities-switch)
     ("C-x C-a b"   . activities-switch-buffer)
     ("C-x C-a g"   . activities-revert)
     ("C-x C-a l"   . activities-list)))

;; otpp : One tab per project, with unique names
(use-package otpp
    :after tab-bar project
    :init
    ;; If you like to define some aliases for better user experience
    (defalias 'one-tab-per-project-mode 'otpp-mode)
    (defalias 'one-tab-per-project-override-mode 'otpp-override-mode)
    ;; Enable `otpp-mode` globally
    (otpp-mode 1)
    ;; If you want to advice the commands in `otpp-override-commands`
    ;; to be run in the current's tab (so, current project's) root directory
    (otpp-override-mode 1))

;; tabspaces : create buffer-isolated workspaces
(use-package tabspaces
    :disabled t
    :preface
    (defun my/tabspace-setup ()
        "Set up tabspace at startup."
        ;; Add *Messages* and *splash* to Tab \`Default\'
        (tabspaces-mode 1)
        (progn
            (tab-bar-rename-tab "Default")
            (when (get-buffer "*Messages*")
                (set-frame-parameter nil
                                     'buffer-list
                                     (cons (get-buffer "*Messages*")
                                           (frame-parameter nil 'buffer-list))))
            (when (get-buffer "*splash*")
                (set-frame-parameter nil
                                     'buffer-list
                                     (cons (get-buffer "*splash*")
                                           (frame-parameter nil 'buffer-list))))))

    (defun my/consult-tabspaces ()
        "Deactivate isolated buffers when not using tabspaces."
        (require 'consult)
        (cond (tabspaces-mode
               ;; hide full buffer list (still available with "b")
               (consult-customize consult--source-buffer :hidden t :default nil)
               (add-to-list 'consult-buffer-sources 'consult--source-workspace))
              (t
               ;; reset consult-buffer to show all buffers
               (consult-customize consult--source-buffer :hidden nil :default t)
               (setq consult-buffer-sources (remove #'consult--source-workspace consult-buffer-sources)))))
    :commands
    (tabspaces-switch-or-create-workspace
     tabspaces-open-or-create-project-and-workspace)
    :hook
    ((after-init . my/tabspace-setup)
     (tabspaces-mode . my/consult-tabspaces))
    :init
    (with-eval-after-load 'consult
        ;; hide full buffer list (still available with "b" prefix)
        (consult-customize consult--source-buffer :hidden t :default nil)
        ;; set consult-workspace buffer list
        (defvar consult--source-workspace
            (list :name     "Workspace Buffers"
                  :narrow   ?w
                  :history  'buffer-name-history
                  :category 'buffer
                  :state    #'consult--buffer-state
                  :default  t
                  :items    (lambda () (consult--buffer-query
                                        :predicate #'tabspaces--local-buffer-p
                                        :sort 'visibility
                                        :as #'buffer-name)))

            "Set workspace buffer list for consult-buffer.")
        (add-to-list 'consult-buffer-sources 'consult--source-workspace))
    :custom
    (tabspaces-keymap-prefix "C-x TAB")
    (tabspaces-use-filtered-buffers-as-default t)
    (tabspaces-default-tab "Default")
    (tabspaces-remove-to-default t)
    (tabspaces-include-buffers '("*scratch*"))
    (tabspaces-initialize-project-with-todo nil)
    (tabspaces-todo-file-name "project-todo.org")
    ;; sessions
    (tabspaces-session nil)
    (tabspaces-session-auto-restore nil))

(provide 'my-init-workspaces)
;;; my-init-workspaces.el ends here

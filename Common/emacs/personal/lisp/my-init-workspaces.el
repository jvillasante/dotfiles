;; my-init-workspaces.el -*- lexical-binding: t; -*-

(use-package tabspaces
    :disabled t
    :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup.
    :commands (tabspaces-switch-or-create-workspace
                  tabspaces-open-or-create-project-and-workspace)
    :init
    ;; Filter Buffers for Consult-Buffer
    (with-eval-after-load 'consult
        ;; hide full buffer list (still available with "b" prefix)
        (consult-customize consult--source-buffer :hidden t :default nil)
        ;; set consult-workspace buffer list
        (defvar consult--source-workspace
            (list :name   "Workspace Buffers"
                :narrow   ?w
                :history  'buffer-name-history
                :category 'buffer
                :state    #'consult--buffer-state
                :default  t
                :items    (lambda () (consult--buffer-query
                                         :predicate #'tabspaces--local-buffer-p
                                         :sort 'visibility
                                         :as #'buffer-name)))
            (add-to-list 'consult-buffer-sources 'consult--source-workspace)))
    :custom
    (tabspaces-use-filtered-buffers-as-default t)
    (tabspaces-default-tab "Default")
    (tabspaces-remove-to-default t)
    (tabspaces-include-buffers '("*scratch*"))
    ;; sessions
    (tabspaces-session nil)
    (tabspaces-session-auto-restore nil))

(provide 'my-init-workspaces)
;;; my-init-workspaces.el ends here

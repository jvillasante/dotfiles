;;; my-init-workspaces.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; tabspaces : https://github.com/mclear-tools/tabspaces
(use-package tabspaces
    :preface
    (defun my/tabspace-setup ()
        "Set up tabspace at startup."
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
    :hook ((tabspaces-mode . my/consult-tabspaces))
    :init
    ;; Initialize correctly
    (if (daemonp)
            (add-hook 'after-make-frame-functions
                      (lambda (frame)
                          (with-selected-frame frame
                              (my/tabspace-setup))))
        (my/tabspace-setup))

    ;; Filter Buffers for Consult-Buffer
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
    (tabspaces-use-filtered-buffers-as-default t)
    (tabspaces-default-tab "Default")
    (tabspaces-remove-to-default t)
    (tabspaces-include-buffers '("*scratch*"))
    (tabspaces-initialize-project-with-todo nil)
    ;; sessions
    (tabspaces-session nil)
    (tabspaces-session-auto-restore nil))

(provide 'my-init-workspaces)
;;; my-init-workspaces.el ends here

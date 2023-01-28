;;; my-org.el --- Custom Org Configs -*- lexical-binding: t; -*-

;;; Commentary:
;;; Org Customizations

;;; Code:

(require 'crafted-org)
(progn
    (setq! org-pretty-entities t)
    (setq! org-fontify-whole-heading-line t)
    (setq! org-fontify-done-headline t)
    (setq! org-fontify-quote-and-verse-blocks t)
    (setq! org-startup-indented t)
    (setq! org-startup-folded t)
    (setq! org-hide-emphasis-markers t))

;; org-superstar : prettify headings and plain lists in Org mode
(crafted-package-install-package 'org-superstar) ;; bullets customization
(progn
    (setq! org-superstar-remove-leading-stars t)
    (setq! org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●"))
    (setq! org-superstar-special-todo-items t)
    (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(provide 'my-org)
;;; my-org.el ends here

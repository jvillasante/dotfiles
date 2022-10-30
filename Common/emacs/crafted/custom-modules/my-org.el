;;; my-org.el --- Custom Org Configs -*- lexical-binding: t; -*-

;;; Commentary:
;;; Org Customizations

;;; Code:

(require 'crafted-org)
(progn
    (setc org-pretty-entities t)
    (setc org-fontify-whole-heading-line t)
    (setc org-fontify-done-headline t)
    (setc org-fontify-quote-and-verse-blocks t)
    (setc org-startup-indented t)
    (setc org-startup-folded t)
    (setc org-hide-emphasis-markers t))

;; org-superstar : prettify headings and plain lists in Org mode
(crafted-package-install-package 'org-superstar) ;; bullets customization
(progn
    (setc org-superstar-remove-leading-stars t)
    (setc org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●"))
    (setc org-superstar-special-todo-items t)
    (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(provide 'my-org)
;;; my-org.el ends here

;;; my-org.el --- Custom Org Configs -*- lexical-binding: t; -*-

;;; Commentary:
;;; Org Customizations

;;; Code:

(require 'crafted-org)
(progn
  (csetq org-pretty-entities t)
  (csetq org-fontify-whole-heading-line t)
  (csetq org-fontify-done-headline t)
  (csetq org-fontify-quote-and-verse-blocks t)
  (csetq org-startup-indented t)
  (csetq org-startup-folded t)
  (csetq org-hide-emphasis-markers t))

;; org-superstar : prettify headings and plain lists in Org mode
(crafted-package-install-package 'org-superstar) ;; bullets customization
(progn
  (csetq org-superstar-remove-leading-stars t)
  (csetq org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●"))
  (csetq org-superstar-special-todo-items t)
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(provide 'my-org)
;;; my-org.el ends here

;;; my-org.el --- Custom Org Configs -*- lexical-binding: t; -*-

;;; Commentary:
;;; Org Customizations

;;; Code:

(require 'crafted-org)
(progn
  (customize-set-variable 'org-pretty-entities t)
  (customize-set-variable 'org-fontify-whole-heading-line t)
  (customize-set-variable 'org-fontify-done-headline t)
  (customize-set-variable 'org-fontify-quote-and-verse-blocks t)
  (customize-set-variable 'org-startup-indented t)
  (customize-set-variable 'org-startup-folded t)
  (customize-set-variable 'org-hide-emphasis-markers t))

;; org-superstar : prettify headings and plain lists in Org mode
(crafted-package-install-package 'org-superstar) ;; bullets customization
(progn
  (customize-set-variable 'org-superstar-remove-leading-stars t)
  (customize-set-variable 'org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●"))
  (customize-set-variable 'org-superstar-special-todo-items t)
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(provide 'my-org)
;;; my-org.el ends here

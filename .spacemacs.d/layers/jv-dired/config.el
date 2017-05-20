(with-eval-after-load 'magit
(setq projectile-switch-project-action 'projectile-dired) ; dired loads on project switch
(setq dired-listing-switches "-alh")
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))
(evil-leader/set-key "od" 'dired))

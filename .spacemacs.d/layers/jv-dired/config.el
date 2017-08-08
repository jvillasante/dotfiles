(with-eval-after-load 'dired
  (setq projectile-switch-project-action 'projectile-dired) ; dired loads on project switch
  (setq dired-listing-switches "-aBhl  --group-directories-first")
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))
  (evil-leader/set-key "od" 'dired))

(with-eval-after-load 'ranger
  (setq ranger-cleanup-on-disable t)
  (setq ranger-ignored-extensions '("mkv" "iso" "mp4"))
  (setq ranger-max-preview-size 10))

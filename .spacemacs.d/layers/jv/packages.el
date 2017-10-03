(defconst jv-packages
  '(editorconfig
     neotree
     dired-quick-sort
     sr-speedbar
     flyspell-lazy
     password-store
     ;; zeal-at-point
     ))

(defun jv/init-editorconfig ()
  (use-package editorconfig
    :ensure t
    :config
    (editorconfig-mode 1)
    (spacemacs|diminish editorconfig-mode "ⓔ" "e")))

(defun jv/post-init-neotree ()
  (defadvice neo-buffer--get-nodes
    (after neo-buffer--get-nodes-new-sorter activate)
    (setq ad-return-value
      (let ((nodes ad-return-value)
             (comparator (lambda (s1 s2) (string< (downcase s1)
                                           (downcase s2)))))
        (apply 'cons (mapcar (lambda (x) (sort (apply x (list nodes))
                                           comparator))
                       '(car cdr))))))

  ;; (setq neo-theme 'nerd)
  (setq neo-theme 'ascii)

  (setq neo-hidden-regexp-list
    '("^\\.\\(git\\|cache\\|tox\\|coverage\\)$"
       "^\\.\\(DS_Store\\|python\\-version\\)"
       "^\\(htmlcov\\|node_modules\\)$" "\\.elcs$"
       "^\\.coverage\\..*" "\\.ipynb.*$" "\\.py[cod]$"
       "~$" "^#.*#$" "^\\.#.*$" "^__pycache__$"
       "\\.egg\-info$")))

(defun jv/init-dired-quick-sort ()
  (use-package dired-quick-sort
    :ensure t
    :config
    (dired-quick-sort-setup)))

(defun jv/init-sr-speedbar ()
  (use-package sr-speedbar
    :defer t
    :init
    (spacemacs/set-leader-keys
      "sr" 'sr-speedbar-toggle)))

(defun jv/init-flyspell-lazy ()
  (use-package flyspell-lazy
    :init
    (add-hook 'prog-mode-hook 'flyspell-lazy-mode)))

(defun jv/init-password-store ()
  (use-package password-store
    :ensure t
    :init
    (setq password-store-password-length 25)
    (evil-leader/set-key
      "opc" 'password-store-copy
      "ope" 'password-store-edit
      "opg" 'password-store-generate)))

(defun jv/init-zeal-at-point ()
  (use-package zeal-at-point
    :defer t
    :init
    (spacemacs/set-leader-keys
      "dd" 'zeal-at-point
      "dD" 'zeal-at-point-set-docset)))

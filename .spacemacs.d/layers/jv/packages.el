(defconst jv-packages
  '(
     ;; editorconfig
     hydra
     neotree
     sr-speedbar
     flyspell-lazy
     password-store
     pass
     crux))

;; (defun jv/init-editorconfig ()
;;   (use-package editorconfig
;;     :ensure t
;;     :config
;;     (editorconfig-mode 1)
;;     (spacemacs|diminish editorconfig-mode "ⓔ" "e")))

(defun jv/post-init-hydra()
  (set-face-attribute 'hydra-face-red nil
    :foreground "#FF6956" :bold t :background "#383838")
  (set-face-attribute 'hydra-face-blue nil
    :foreground "Cyan" :bold t :background "#383838")
  (set-face-attribute 'hydra-face-amaranth nil
    :foreground "#e52b50" :bold t :background "#383838")
  (set-face-attribute 'hydra-face-pink nil
    :foreground "HotPink1" :bold t :background "#383838")
  (set-face-attribute 'hydra-face-teal nil
    :foreground "SkyBlue1" :bold t :background "#383838")
  (hydra-add-font-lock))

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
       "\\.egg\-info$"))
  )

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

(defun jv/init-pass ()
  (use-package pass
    :ensure t
    ))

(defun jv/init-crux ()
  (use-package crux
    :ensure t
    :bind (("C-c o" . crux-open-with))))

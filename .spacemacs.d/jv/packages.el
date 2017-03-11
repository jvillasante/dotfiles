(defconst jv-packages
  '(sr-speedbar
    flyspell-lazy
    password-store
    zeal-at-point))

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
    :defer t
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

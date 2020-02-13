(defconst jv-packages
    '(ag
         ivy
         avy
         counsel
         which-key
         neotree
         sr-speedbar
         flycheck
         flyspell
         flyspell-lazy
         pinentry
         crux))

(defun jv/init-ag ()
    (use-package ag
        :ensure t))

(defun jv/post-init-ivy ()
    (setq ivy-display-style nil
        ivy-count-format "(%d/%d) "
        ivy-use-selectable-prompt t    ; much better than C-M-j
        ivy-use-virtual-buffers t      ; to make ivy-views appear on the buffers list
        ivy-virtual-abbreviate 'full   ; default is name
        ivy-initial-inputs-alist nil   ; remove initial ^ input.
        ivy-extra-directories nil      ; remove . and .. directory. (default value: ("../" "./"))
        ivy-height 10)

    (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
    (define-key ivy-minibuffer-map (kbd "<escape>") #'minibuffer-keyboard-quit))

(defun jv/post-init-avy ()
    (setq avy-all-windows t))

(defun jv/post-init-counsel ()
    (setq counsel-rg-base-command "rg -S --no-heading --line-number --color never %s ."
        counsel-ag-base-command "ag -S --nocolor --nogroup %s"))

(defun jv/post-init-which-key ()
    (spacemacs|diminish which-key-mode "  " " k"))

(defun jv/post-init-neotree ()
    (setq neo-theme 'ascii)
    ;; (setq neo-autorefresh t)

    (defadvice neo-buffer--get-nodes
        (after neo-buffer--get-nodes-new-sorter activate)
        (setq ad-return-value
            (let ((nodes ad-return-value)
                     (comparator (lambda (s1 s2) (string< (downcase s1)
                                                     (downcase s2)))))
                (apply 'cons (mapcar (lambda (x) (sort (apply x (list nodes))
                                                     comparator))
                                 '(car cdr))))))

    (with-eval-after-load 'neotree
        (setq neo-hidden-regexp-list
            '("^\\.\\(git\\|cache\\|tox\\|coverage\\)$"
                 "^\\.\\(DS_Store\\|python\\-version\\)"
                 "^\\(htmlcov\\|node_modules\\)$" "\\.elcs$"
                 "^\\.coverage\\..*" "\\.ipynb.*$" "\\.py[cod]$"
                 "~$" "^#.*#$" "^\\.#.*$" "^__pycache__$"
                 "\\.gcda$" "\\.gcno$" "\\.lo$" "\\.o$" "\\.so$"
                 "\\.egg\-info$")))

    (evil-leader/set-key
        "on" 'neotree-find-project-root))

(defun jv/init-sr-speedbar ()
    (use-package sr-speedbar
        :defer t
        :init
        (spacemacs/set-leader-keys
            "sr" 'sr-speedbar-toggle)))

(defun jv/post-init-flycheck ()
    (setq-default
        +flycheck-on-escape nil
        flycheck-check-syntax-automatically '(save mode-enable)))

(defun jv/post-init-flyspell ()
    (spacemacs|diminish flyspell-mode "  " " s"))

(defun jv/init-flyspell-lazy ()
    (use-package flyspell-lazy
        :init
        (add-hook 'prog-mode-hook 'flyspell-lazy-mode)))

(defun jv/init-pinentry ()
    (use-package pinentry
        :ensure t
        :init
        (setq epa-pinentry-mode 'loopback)))

(defun jv/init-crux ()
    (use-package crux
        :ensure t
        :bind (("C-c o" . crux-open-with))))

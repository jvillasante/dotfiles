(defconst jv-packages
    '(
         ;; editorconfig
         ;; hydra
         ivy
         avy
         counsel
         ;; neotree
         treemacs
         sr-speedbar
         flycheck
         flyspell-lazy
         password-store
         pass
         crux))

(defun jv/post-init-ivy ()
    (setq ivy-display-style 'fancy
        ivy-count-format "(%d/%d) "
        ivy-use-selectable-prompt t    ; much better than C-M-j
        ivy-use-virtual-buffers t      ; to make ivy-views appear on the buffers list
        ivy-virtual-abbreviate 'full   ; default is name
        ivy-initial-inputs-alist nil   ; remove initial ^ input.
        ivy-extra-directories nil      ; remove . and .. directory. (default value: ("../" "./"))
        ivy-height 10))

(defun jv/post-init-avy ()
    (setq avy-all-windows t))

(defun jv/post-init-counsel ()
    (setq counsel-rg-base-command "rg -S --no-heading --line-number --color never %s ."
        counsel-ag-base-command "ag -S --nocolor --nogroup %s"))

;; (defun jv/init-editorconfig ()
;;   (use-package editorconfig
;;     :ensure t
;;     :config
;;     (editorconfig-mode 1)
;;     (spacemacs|diminish editorconfig-mode "ⓔ" "e")))

;; (defun jv/post-init-hydra()
;;     (set-face-attribute 'hydra-face-red nil
;;         :foreground "#FF6956" :bold t :background "#383838")
;;     (set-face-attribute 'hydra-face-blue nil
;;         :foreground "Cyan" :bold t :background "#383838")
;;     (set-face-attribute 'hydra-face-amaranth nil
;;         :foreground "#e52b50" :bold t :background "#383838")
;;     (set-face-attribute 'hydra-face-pink nil
;;         :foreground "HotPink1" :bold t :background "#383838")
;;     (set-face-attribute 'hydra-face-teal nil
;;         :foreground "SkyBlue1" :bold t :background "#383838")
;;     (hydra-add-font-lock))

;; (defun jv/post-init-neotree ()
;;     (defadvice neo-buffer--get-nodes
;;         (after neo-buffer--get-nodes-new-sorter activate)
;;         (setq ad-return-value
;;             (let ((nodes ad-return-value)
;;                      (comparator (lambda (s1 s2) (string< (downcase s1)
;;                                                      (downcase s2)))))
;;                 (apply 'cons (mapcar (lambda (x) (sort (apply x (list nodes))
;;                                                      comparator))
;;                                  '(car cdr))))))

;;     (setq neo-theme 'ascii)
;;     ;; (setq neo-autorefresh t)

;;     (setq neo-hidden-regexp-list
;;         '("^\\.\\(git\\|cache\\|tox\\|coverage\\)$"
;;              "^\\.\\(DS_Store\\|python\\-version\\)"
;;              "^\\(htmlcov\\|node_modules\\)$" "\\.elcs$"
;;              "^\\.coverage\\..*" "\\.ipynb.*$" "\\.py[cod]$"
;;              "~$" "^#.*#$" "^\\.#.*$" "^__pycache__$"
;;              "\\.egg\-info$")))

(defun jv/post-init-treemacs ()
    (setq
        treemacs-collapse-dirs                 (if (executable-find "python3") 3 0)
        treemacs-deferred-git-apply-delay      0.5
        treemacs-display-in-side-window        t
        treemacs-eldoc-display                 t
        treemacs-file-event-delay              5000
        treemacs-file-follow-delay             0.2
        treemacs-follow-after-init             nil
        treemacs-git-command-pipe              ""
        treemacs-goto-tag-strategy             'refetch-index
        treemacs-indentation                   2
        treemacs-indentation-string            " "
        treemacs-is-never-other-window         nil
        treemacs-max-git-entries               5000
        treemacs-missing-project-action        'ask
        treemacs-no-png-images                 t
        treemacs-no-delete-other-windows       t
        treemacs-project-follow-cleanup        nil
        treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-recenter-distance             0.1
        treemacs-recenter-after-file-follow    nil
        treemacs-recenter-after-tag-follow     nil
        treemacs-recenter-after-project-jump   'always
        treemacs-recenter-after-project-expand 'on-distance
        treemacs-show-cursor                   t
        treemacs-show-hidden-files             t
        treemacs-silent-filewatch              nil
        treemacs-silent-refresh                nil
        treemacs-sorting                       'alphabetic-case-insensitive-desc
        treemacs-space-between-root-nodes      t
        treemacs-tag-follow-cleanup            t
        treemacs-tag-follow-delay              1.5
        treemacs-width                         35)

    (with-eval-after-load 'treemacs
        (defun jv/treemacs-ignore (filename absolute-path)
            (or
                ;; (string-equal filename "foo")
                ;; (string-prefix-p "/x/y/z/" absolute-path)
                (string-match "^\\.\\(git\\|cache\\|tox\\|coverage\\)$" filename)
                (string-match "^\\.\\(DS_Store\\|python\\-version\\)" filename)
                (string-match "^\\(htmlcov\\|node_modules\\)$" filename)
                (string-match "\\.elcs$" filename)
                (string-match "^\\.coverage\\..*" filename)
                (string-match "\\.ipynb.*$" filename)
                (string-match "\\.py[cod]$" filename)
                (string-match "~$" filename)
                (string-match "^#.*#$" filename)
                (string-match "^\\.#.*$" filename)
                (string-match "^__pycache__$" filename)
                (string-match "\\.egg\-info$" filename)))

        (add-to-list 'treemacs-ignored-file-predicates #'jv/treemacs-ignore)))

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
        :ensure t))

(defun jv/init-crux ()
    (use-package crux
        :ensure t
        :bind (("C-c o" . crux-open-with))))

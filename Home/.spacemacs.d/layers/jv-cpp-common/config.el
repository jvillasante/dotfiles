;;; config.el --- jv-cpp Layer config File for Spacemacs
;;

(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.C\\'" . c++-mode))

(defun jv/c-mode-common-hook ()
    (setq
        c-default-style "stroustrup"
        c-basic-offset 4
        c-basic-indent 4)
    (c-set-offset 'substatement-open 0))
(add-hook 'c-mode-common-hook 'jv/c-mode-common-hook)

(add-hook 'c++-mode-hook (lambda ()
                             (push '(?< . ("< " . " >")) evil-surround-pairs-alist)))

(defvar jv-cpp-default-mode-for-headers 'c++-mode
    "Default mode to open header files. Can be `c-mode' or `c++-mode'.")

(spacemacs|define-jump-handlers c++-mode)

(with-eval-after-load 'smartparens
    (sp-with-modes '(c-mode c++-mode)
        ;; adds <> to c mode
        (sp-local-pair "<" ">")

        ;; when you press RET, the curly braces automatically
        ;; add another newline
        (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
        (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                                     ("* ||\n[i]" "RET")))))

;;; config.el --- jv-cpp Layer config File for Spacemacs
;;

(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))

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

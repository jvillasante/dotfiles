;;; config.el --- jv-cpp Layer config File for Spacemacs
;;

(defvar jv-cpp-default-mode-for-headers 'c++-mode
  "Default mode to open header files. Can be `c-mode' or `c++-mode'.")

(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))

(spacemacs|define-jump-handlers c++-mode)

;;; my-early-init.el --- Early Init Configs -*- lexical-binding: t; -*-

;;; Commentary:
;;; Early Init Customizations

;;; Code:

(defmacro csetq (variable value)
  "Set the VARIABLE to VALUE, but use `set-default' if needed."
  `(funcall (or (get ',variable 'custom-set) 'set-default) ',variable ,value))

(defmacro setq-if-exists (variable value)
  "Set VARIABLE to VALUE. Error out if VARIABLE is not special."
  `(if (special-variable-p ',variable)
       (setq ,variable ,value)
     (error (format "Variable %s does not exist" ',variable))))

(provide 'my-early-init)
;;; my-early-init.el ends here

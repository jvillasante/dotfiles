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

(defmacro setq! (&rest settings)
    "A more sensible `setopt' for setting customizable variables.

This can be used as a drop-in replacement for `setq' and *should* be used
instead of `setopt'. Unlike `setq', this triggers custom setters on variables.
Unlike `setopt', this won't needlessly pull in dependencies."
    (macroexp-progn
        (cl-loop for (var val) on settings by 'cddr
            collect `(funcall (or (get ',var 'custom-set) #'set-default-toplevel-value)
                         ',var ,val))))

(provide 'my-early-init)
;;; my-early-init.el ends here

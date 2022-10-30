;;; my-early-init.el --- Early Init Configs -*- lexical-binding: t; -*-

;;; Commentary:
;;; Early Init Customizations

;;; Code:

;; Taken from `general.el'
(defmacro setc (&rest settings)
    "A stripped-down `customize-set-variable' with the syntax of `setq'.
Like `setq', multiple variables can be set at once; SETTINGS should consist of
variable value pairs. Some variables have a custom setter (specified with
`defcustom' and :set) that is used to run code necessary for changes to take
effect (e.g. `auto-revert-interval'). If a package has already been loaded, and
the user uses `setq' to set one of these variables, the :set code will not
run (e.g. in the case of `auto-revert-interval', the timer will not be updated).
Like with `customize-set-variable', `general-setq' will use the custom :set
setter when it exists. If the package defining the variable has not yet been
loaded, the custom setter will not be known, but it will still be run upon
loading the package. Unlike `customize-set-variable', `general-setq' does not
attempt to load any dependencies for the variable and does not support giving
variables comments. It also falls back to `set' instead of `set-default', so
that like `setq' it will change the local value of a buffer-local variable
instead of the default value."
    `(progn
         ,@(cl-loop for (var val) on settings by 'cddr
               collect `(funcall (or (get ',var 'custom-set) #'set)
                            ',var ,val))))

(provide 'my-early-init)
;;; my-early-init.el ends here

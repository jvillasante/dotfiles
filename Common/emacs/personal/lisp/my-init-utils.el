;;; my-init-utils.el -*- lexical-binding: t; -*-

(straight-use-package 'use-package)
(straight-use-package 'general)

(general-create-definer my/leader
    :prefix "C-c")

(general-create-definer my/localleader
    :prefix "C-c c")

(my/leader
    "C-c" '(:ignore t :which-key "Local Leader")
    "o" '(:ignore t :which-key "Open")
    "t" '(:ignore t :which-key "Toggle"))

(general-create-definer my/open-map
    :prefix "C-c o"
    :prefix-map 'my/open-map)

(general-create-definer my/toggle-map
    :prefix "C-c t"
    :prefix-map 'my/toggle-map)

(provide 'my-init-utils)
;;; my-init-utils.el ends here

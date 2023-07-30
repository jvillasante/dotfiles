;;; early-init.el --- -*- lexical-binding: t -*-

;;; early-init.el --- Emacs early initialization for Crafted Emacs (optional) -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; Define Paths
(defconst my/home-path (expand-file-name "~/"))
(defconst my/dotfiles-path (expand-file-name "Workspace/Public/dotfiles/" my/home-path))
(defconst my/software-path (expand-file-name "Workspace/Software/" my/home-path))
(defconst my/dropbox-path (expand-file-name "Dropbox/" my/home-path))

;; Configures `package.el'
(load (convert-standard-filename
          (expand-file-name ".emacs.crafted/modules/crafted-early-init-config" my/dotfiles-path)))

(provide 'early-init)
;;; early-init.el ends here

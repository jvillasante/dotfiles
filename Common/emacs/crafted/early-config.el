;;; early-config.el --- Early Emacs Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Emacs early config file

;;; Code:

(setq package-archive-priorities nil)
(setq use-dialog-box nil) ;; remove gui box

(setq package-enable-at-startup nil)
(setq crafted-package-system 'package)
(crafted-package-bootstrap crafted-package-system)

;;; early-config.el ends here

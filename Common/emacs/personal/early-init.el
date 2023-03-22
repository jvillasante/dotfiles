;;; early-init.el --- -*- lexical-binding: t -*-

;; Author: Julio C. Villasante <jvillasantegomez@gmail.com>
;; URL: https://github.com/jvillasante/dotfiles
;; Keywords: dotfiles emacs

;;; Commentary:
;; This is my personal Emacs configuration.

;;; Code:

(setq package-enable-at-startup nil)

;; increase gc threshold to speedup starting up
(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold most-positive-fixnum)

(setq inhibit-startup-message t)

;; no menu bar, toolbar, scroll bar
(setq default-frame-alist
    '((menu-bar-lines . 0)
         (tool-bar-lines . 0)
         (horizontal-scroll-bars)
         (vertical-scroll-bars)))

(when (featurep 'native-compile)
    (defvar inhibit-automatic-native-compilation)
    (setq inhibit-automatic-native-compilation nil)

    (defvar native-comp-async-report-warnings-errors)
    (setq native-comp-async-report-warnings-errors 'silent))

(when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
        (convert-standard-filename
            (expand-file-name  "var/eln-cache/" user-emacs-directory))))
(provide 'early-init)
;;; early-init.el ends here

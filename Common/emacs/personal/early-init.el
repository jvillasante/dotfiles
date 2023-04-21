;;; early-init.el --- -*- lexical-binding: t -*-

;; Author: Julio C. Villasante <jvillasantegomez@gmail.com>
;; URL: https://github.com/jvillasante/dotfiles
;; Keywords: dotfiles emacs

;;; Commentary:
;; This is my personal Emacs configuration.

;;; Code:

;; increase gc threshold to speedup starting up
(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold most-positive-fixnum)

;; Initialise installed packages
(setq package-enable-at-startup t)

;; prefer loading newest compiled .el file
(setq load-prefer-newer t)

;;; Native compilation settings
(when (featurep 'native-compile)
    ;; Silence compiler warnings as they can be pretty disruptive
    (setq native-comp-async-report-warnings-errors 'silent)

    ;; Make native compilation happens asynchronously
    (setq native-comp-deferred-compilation t)

    ;; Set the right directory to store the native compilation cache
    ;; NOTE the method for setting the eln-cache directory depends on the emacs version
    (when (fboundp 'startup-redirect-eln-cache)
        (if (version< emacs-version "29")
            (add-to-list 'native-comp-eln-load-path
                (convert-standard-filename (expand-file-name "var/eln-cache/" user-emacs-directory)))
            (startup-redirect-eln-cache
                (convert-standard-filename (expand-file-name "var/eln-cache/" user-emacs-directory)))))
    (add-to-list 'native-comp-eln-load-path (expand-file-name "var/eln-cache/" user-emacs-directory)))

;;; UI configuration
(setq inhibit-startup-message t)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(mouse-color . "white") default-frame-alist)

;; Make the initial buffer load faster by setting its mode to fundamental-mode
(setq initial-major-mode 'fundamental-mode)

;; Paths
(defconst my/home-path (expand-file-name "~/"))
(defconst my/dotfiles-path (expand-file-name "Workspace/Public/dotfiles/" my/home-path))
(defconst my/software-path (expand-file-name "Workspace/Software/" my/home-path))
(defconst my/dropbox-path (expand-file-name "Dropbox/" my/home-path))

(provide 'early-init)
;;; early-init.el ends here

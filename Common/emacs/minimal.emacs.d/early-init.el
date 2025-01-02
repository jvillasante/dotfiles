;;; early-init.el --- Early Init -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Variables

(defvar my/user-directory user-emacs-directory
    "The default value of the `user-emacs-directory' variable.")

(defvar my/emacs-gc-cons-threshold (* 16 1024 1024)
    "The value of `gc-cons-threshold' after Emacs startup.")

;; Custom
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

;; Reducing clutter in ~/.emacs.d by redirecting files to ~/emacs.d/var/
(setq my/var-dir (expand-file-name "var/" my/user-directory))
(setq my/etc-dir (expand-file-name "etc/" my/user-directory))
(setq package-user-dir (expand-file-name "elpa" my/var-dir))
(setq user-emacs-directory my/var-dir)

;;; Native compilation and Byte compilation
(if (and (featurep 'native-compile)
         (fboundp 'native-comp-available-p)
         (native-comp-available-p))

        ;; Activate `native-compile'
        (setq native-comp-jit-compilation t
              package-native-compile t))

;;; UI elements

;; Disable startup screens and messages
(setq inhibit-splash-screen t)

;; some default-frame-alist
(push '(width . 160) default-frame-alist)
(push '(height . 46) default-frame-alist)
(push '(mouse-color . "white") default-frame-alist)
(push '(bottom-divider-width . 0) default-frame-alist)
(push '(right-divider-width . 1) default-frame-alist)

;; menu-bar
(push '(menu-bar-lines . 0) default-frame-alist)
(unless (memq window-system '(mac ns))
    (setq menu-bar-mode nil))

;; tool-bar
(push '(tool-bar-lines . 0) default-frame-alist)
(setq tool-bar-mode nil)

;; scroll bars
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(setq scroll-bar-mode nil)
(when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))

;; Disable GUIs because they are inconsistent across systems, desktop
;; environments, and themes, and they don't match the look of Emacs.
(setq use-file-dialog nil)
(setq use-dialog-box nil)

;;; package.el
(setq load-prefer-newer t)
(setq package-enable-at-startup t)
(setq package-install-upgrade-built-in t)
(setq package-archives '(("melpa"  . "https://melpa.org/packages/")
                         ("stable" . "https://stable.melpa.org/packages/")
                         ("gnu"    . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(customize-set-variable 'package-archive-priorities '(("gnu"    . 99)
                                                      ("nongnu" . 80)
                                                      ("stable" . 70)
                                                      ("melpa"  . 0)))
(provide 'early-init)

;;; early-init.el ends here
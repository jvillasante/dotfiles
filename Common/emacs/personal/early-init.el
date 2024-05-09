;;; early-init.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; increase gc threshold to speedup starting up
(let ((original-gc-cons-threshold gc-cons-threshold))
    (setq
     gc-cons-threshold most-positive-fixnum
     read-process-output-max (* 1024 1024 4) ; 4mb
     inhibit-startup-message t
     inhibit-compacting-font-caches t
     message-log-max 16384
     package-enable-at-startup t
     load-prefer-newer noninteractive)
    (add-hook 'emacs-startup-hook
              (lambda nil
                  (setq gc-cons-threshold original-gc-cons-threshold))))

;; set default UI
(setq-default default-frame-alist '((width . 170)
                                    (height . 56)
                                    (tool-bar-lines . 0)
                                    (menu-bar-lines . 0)
                                    (vertical-scroll-bars)
                                    (mouse-color . "white")
                                    (bottom-divider-width . 0)
                                    (right-divider-width . 1))
              initial-frame-alist default-frame-alist
              frame-inhibit-implied-resize t
              fringe-indicator-alist (assq-delete-all 'truncation fringe-indicator-alist))

(unless (or (daemonp) noninteractive)
    (let ((original-file-name-handler-alist file-name-handler-alist))
        (setq-default file-name-handler-alist nil)
        (add-hook 'emacs-startup-hook
                  (lambda nil
                      (setq file-name-handler-alist
                            (delete-dups (append file-name-handler-alist
                                                 original-file-name-handler-alist))))
                  101))
    (when (fboundp #'tool-bar-mode)
        (tool-bar-mode -1))
    (when (fboundp #'scroll-bar-mode)
        (scroll-bar-mode -1))
    (when (fboundp #'menu-bar-mode)
        (menu-bar-mode -1)))

;;; Native compilation settings
(when (featurep 'native-compile)
    ;; Silence compiler warnings as they can be pretty disruptive
    (defvar native-comp-async-report-warnings-errors)
    (setq native-comp-async-report-warnings-errors 'silent)

    ;; Set the right directory to store the native compilation cache
    ;; NOTE the method for setting the eln-cache directory depends on the emacs version
    (when (fboundp 'startup-redirect-eln-cache)
        (if (version< emacs-version "29")
                (add-to-list 'native-comp-eln-load-path
                             (convert-standard-filename (expand-file-name "var/eln-cache/" user-emacs-directory)))
            (startup-redirect-eln-cache
             (convert-standard-filename (expand-file-name "var/eln-cache/" user-emacs-directory)))))
    (add-to-list 'native-comp-eln-load-path (expand-file-name "var/eln-cache/" user-emacs-directory)))

;; Make the initial buffer load faster by setting its mode to fundamental-mode
;; (setq initial-major-mode 'fundamental-mode)

(provide 'early-init)
;;; early-init.el ends here

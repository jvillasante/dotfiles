;;; early-init.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; Startup speed, annoyance suppression
(setq gc-cons-threshold 10000000)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;; Silence stupid startup message
(setq inhibit-startup-echo-area-message (user-login-name))

;; Default frame configuration: full screen, good-looking title bar on macOS
(setq frame-resize-pixelwise t)
(tool-bar-mode -1)                      ; All these tools are in the menu-bar anyway
(setq-default default-frame-alist '((width . 170)
                                    (height . 50)
                                    (tool-bar-lines . 0)
                                    (menu-bar-lines . 0)
                                    (vertical-scroll-bars)
                                    (mouse-color . "white")
                                    (bottom-divider-width . 0)
                                    (right-divider-width . 1))
              initial-frame-alist default-frame-alist
              frame-inhibit-implied-resize t
              fringe-indicator-alist (assq-delete-all 'truncation fringe-indicator-alist))

(provide 'early-init)
;;; early-init.el ends here

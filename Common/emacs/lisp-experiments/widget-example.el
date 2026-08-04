;;; widget-example.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(require 'widget)

;; Logic

(defun my/backup-run (folder target zip)
    "Sync FOLDER into TARGET with rsync, or compress it to a zip when ZIP."
    (let* ((folder (expand-file-name folder))
              (target (expand-file-name target))
              (command
                  (if zip
                      (format "zip -r %s %s"
                          (shell-quote-argument
                              (expand-file-name
                                  (format-time-string "backup-%Y-%m-%d.zip") target))
                          (shell-quote-argument folder))
                      (format "rsync -a %s/ %s"
                          (shell-quote-argument folder)
                          (shell-quote-argument target)))))
        (async-shell-command command "*Backup log*")))

;; UI

(defun my/backup ()
    "Open a panel to back up a folder."
    (interactive)
    (switch-to-buffer "*Backup*")
    (kill-all-local-variables)
    (let ((inhibit-read-only t))
        (erase-buffer))
    (remove-overlays)
    (widget-insert "Folder backup\n\n")
    (let* ((folder (widget-create 'file
                       :format "Folder: %v\n"
                       (expand-file-name "~/")))
              (target (widget-create 'file
                          :format "Target: %v\n"
                          (expand-file-name "~/")))
              (zip (widget-create 'checkbox nil)))
        (widget-insert " Compress into zip\n\n")
        (widget-create 'push-button
            :notify (lambda (&rest _) (kill-buffer))
            "Cancel")
        (widget-insert " ")
        (widget-create 'push-button
            :notify (lambda (&rest _)
                        (my/backup-run
                            (widget-value folder)
                            (widget-value target)
                            (widget-value zip))
                        (kill-buffer))
            "Run"))
    (use-local-map widget-keymap)
    (widget-setup))

(provide 'widget-example)
;;; widget-example.el ends here

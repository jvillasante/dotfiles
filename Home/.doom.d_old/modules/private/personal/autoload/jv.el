;;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Full width comment box                                                 ;;
;; from http://irreal.org/blog/?p=374                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jv/comment-box (b e)
    "Draw a box comment around the region but arrange for the region to extend to
at least the fill column. Place the point after the comment box."
    (interactive "r")

    (let ((e (copy-marker e t)))
        (goto-char b)
        (end-of-line)
        (insert-char ?  (- fill-column (current-column)))
        (comment-box b e 1)
        (goto-char e)
        (set-marker e nil)))
(global-set-key (kbd "C-c b b") 'jv/comment-box)

;; Keeps the compilation buffer if there are warnings or errors,
;; and buries it otherwise (after 1 second).
(defun jv/bury-compile-buffer-if-successful (buffer string)
    "Bury a compilation buffer if succeeded without warnings "
    (when (and
              (buffer-live-p buffer)
              (string-match "compilation" (buffer-name buffer))
              (string-match "finished" string)
              (not
                  (with-current-buffer buffer
                      (goto-char (point-min))
                      (search-forward "warning" nil t))))
        (run-with-timer 1 nil
            (lambda (buf)
                (bury-buffer buf)
                ;; (switch-to-prev-buffer (get-buffer-window buf) 'kill)
                (delete-windows-on buf))
            buffer)))
(add-hook 'compilation-finish-functions 'jv/bury-compile-buffer-if-successful)

;; ANSI-colors in the compilation buffer output
(require 'ansi-color)
(defun jv/colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
        (ansi-color-apply-on-region
            compilation-filter-start (point))))
(add-hook 'compilation-filter-hook #'jv/colorize-compilation)

;; http://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
(defun jv/switch-to-previous-buffer ()
    "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))
;; (evil-leader/set-key
;;     "ob" 'jv/switch-to-previous-buffer)

;;; image-dimensions-minor-mode.el
;;
;; Display the image dimensions in the mode line, when viewing an image.
;;
;; Author: Phil S.
;;
;; Compatibility: GNU Emacs 24.3
;;
;; Installation:
;; (eval-after-load 'image-mode '(require 'image-dimensions-minor-mode))

;; Commentary:
;;
;; To also see this information in the frame title, your `frame-title-format'
;; variable might be something like the following:
;;
;; (setq frame-title-format
;;       '(buffer-file-name
;;         ("%b (Emacs) %f" image-dimensions-minor-mode-dimensions)
;;         (dired-directory
;;          (:eval (concat (buffer-name) " (Emacs) " dired-directory))
;;          ("%b (Emacs)"))))

(with-eval-after-load 'image-mode
    (eval-when-compile (require 'cl-macs))

    (declare-function image-get-display-property "image-mode")

    (defvar-local image-dimensions-minor-mode-dimensions nil
        "Buffer-local image dimensions for `image-dimensions-minor-mode'")

    (define-minor-mode image-dimensions-minor-mode
        "Displays the image dimensions in the mode line."
        :init-value nil
        :lighter image-dimensions-minor-mode-dimensions
        (when (not image-dimensions-minor-mode-dimensions)
            (let ((image (image-get-display-property)))
                (when image
                    (setq image-dimensions-minor-mode-dimensions
                        (cl-destructuring-bind (width . height)
                            (image-size image :pixels)
                            (format " (%dx%d)" width height)))))))

    (add-hook 'image-mode-hook 'image-dimensions-minor-mode))

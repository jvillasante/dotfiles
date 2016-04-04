(spacemacs|defvar-company-backends verilog-mode)

;; color utilities
(defun spacemacs//check-color (color)
  (cond
   ((> color 1.0)
    1.0)
   ((< color 0.0)
    0.0)
   (t color)))

(defun spacemacs//rgb-add (hex-str number)
  (let* ((rgb-values (color-name-to-rgb hex-str))
         (red (spacemacs//check-color (+ (car rgb-values) number)))
         (green (spacemacs//check-color (+ (cadr rgb-values) number)))
         (blue (spacemacs//check-color (+ (caddr rgb-values) number))))
    (color-rgb-to-hex red green blue)))

;; when we press ':' character, it runs `asm-colon' command in asm-mode.
;; The command automatically removes any indentation, since every
;; non-whitespace character before a colon is a label in asm, and label
;; has to be at the beginning of a line. However, the problem is that when
;; deleting indentation, trailing spaces are left between the colon and
;; point.
;;
;; These functions solve that problem. First, check whether we have any
;; space or tab after point. If so, don't do anything becuase the spaces are
;; there intentionally. If not, we delete all trailing spaces between
;; point and colon.
(defvar asm-colon-has-space nil)
(defun asm-colon-check-space ()
  (setq asm-colon-has-space nil)
  (when (member (string (char-after)) '(" " "\t"))
    (setq asm-colon-has-space t)))
(defun asm-colon-delete-spaces ()
  (unless asm-colon-has-space
    (call-interactively 'delete-horizontal-space)))
(advice-add 'asm-colon :before 'asm-colon-check-space)
(advice-add 'asm-colon :after 'asm-colon-delete-spaces)

;;; Compiled snippets and support files for `lisp-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'lisp-mode
                     '(("class" "(defclass ${1:name} (${2:inherits})\n   (${4:slot})\n   (:documentation \"${3:doc}\"))\n$0\n" "class" nil nil nil nil nil nil)
                       ("/*" "#|${1:type the comment here}|#\n$0\n" "comment" nil nil nil nil nil nil)
                       ("defp" "(defpackage #:${1:name}\n   (:nicknames #:${2:nick})\n   (:use #:cl #:closer-mop #:${3:package})\n   (:shadow :${4.symbol})\n   (:shadowing-import-from #:${5:package} #:${6:symbol})\n   (:export :$0))\n" "defpackage" nil nil nil nil nil nil)
                       ("defparam" "(defparameter ${1:name} ${2:value}\n  ${3:doc})\n" "defparam" nil nil nil nil nil nil)
                       ("def" "(defun ${1:name} (${2:args})\n  $3)" "defun" nil nil nil nil nil nil)
                       ("defv" "(defvar ${1:name} ${2:value}\n  ${3:doc})\n" "defv" nil nil nil nil nil nil)
                       ("do" "(do ((${1:var1} ${2:init-form} ${3:step-form})\n     (${4:var2} ${5:init-form} ${6:step-form}))  \n    (${7:condition} ${8:return-value})\n    (${9:body}))\n$0\n" "do" nil nil nil nil nil nil)
                       ("for" "for ${1:var} ${2:prepositional-phrase} ${3:list-form}" "for" nil nil nil nil nil nil)
                       ("forbeing" "for ${1:key} being the ${2:hask-keys} in ${3:hash-table-name}  ${4:using list-form}" "forbeing" nil nil nil nil nil nil)
                       ("print" "(format t \"~& $0 ~%\")\n" "format" nil nil nil nil nil nil)
                       ("forthen" "for ${1:var} then ${2:list-form}" "forthen" nil nil nil nil nil nil)
                       ("if" "(when (${1:condition})\n      (${2:then-do-this}))\n$0\n" "if" nil nil nil nil nil nil)
                       ("ifelse" "\n(if (${1:condition})\n    (${2:then})\n    (${3:else}))\n$0\n" "ifelse (...) (...) (...) ..." nil nil nil nil nil nil)
                       ("ifnot" "\n(unless (${1:condition})\n        (${2:then-do-this}))\n$0\n" "ifnot (...) (...)  ..." nil nil nil nil nil nil)
                       ("makepn" "(make-pathname\n  :type \"${1:type}\"\n  :defaults ${2:default-file})" "make-pathname" nil nil nil nil nil nil)
                       ("slot" "(${1:name} :initarg :${1:$(yas/substr yas-text \"[^: ]*\")}\n           :initform (error \":${1:$(yas/substr yas-text \"[^: ]*\")} must be specified\")\n           ;; :accessor ${1:$(yas/substr yas-text \"[^: ]*\")}\n           :reader ${1:$(yas/substr yas-text \"[^: ]*\")}-changed\n           :writer set-${1:$(yas/substr yas-text \"[^: ]*\")}\n           :type\n           :allocation ${3::class :instance}\n           :documentation \"${2:about-slot}\")\n$0\n" "slot" nil nil nil nil nil nil)
                       ("switch" "\n(cond (${1:case1} (${2:do-this}))\n      (${3:case2} (${4:do-this}))     \n      (t ${5:default}))\n$0\n" "switch" nil nil nil nil nil nil)
                       ("typecast" "(coerce ${1:object} ${2:type})\n$0\n" "cast" nil nil nil nil nil nil)
                       ("wof" "\n(with-open-file (stream \"${1:filename}\")\n  (let ((tmp (loop for ${3:var} = (${2:read-line/read-char/read} stream nil :eof\n                   until (eq line :eof)\n                   ${4:do})\n    $4\n  )))))" "with-open-file" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Thu Jul 16 22:43:23 2015

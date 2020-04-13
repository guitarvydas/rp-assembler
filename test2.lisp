(in-package :rp-assembler)

(defparameter *test-rpa-spec-third*
  "
= rmSpaces
  [ ?SPACE | ?COMMENT | * . ]

= sym
  SYMBOL symbolPush

= num
  INTEGER integerPush

= calculator
  ~rmSpaces
  @num '+' @num
  emitSymbol1
  emitPlus
  emitSymbol2
  symbolPop
  symbolPop

")

(defparameter *test-rpa-spec-fourth*
  "
= rmSpaces
  [ ?SPACE | ?COMMENT | * . ]

= sym
  SYMBOL symbolPush

= num
  INTEGER integerPush

= calculator
  ~rmSpaces
  emitOpen
  @num
  emitPlus
  emitSpace
  emitTop
  '+'
  emitSpace
  @num
  emitTop
  emitClose
  symbolPop
  symbolPop

")

(defparameter *test-dsl-code2*
  "
17 + 25
")


(defclass test-parser (parser)
  ((symbol-stack :accessor symbol-stack)))

(defmethod initially ((self test-parser) token-list)
  (setf (symbol-stack self) nil)
  (call-next-method))

;; test mechanisms
(defmethod emitOpen ((self test-parser)) (emit-string self "("))
(defmethod emitClose ((self test-parser)) (emit-string self ")"))
(defmethod emitOpenBrace ((self test-parser)) (emit-string self "{"))
(defmethod emitCloseBrace ((self test-parser)) (emit-string self "}"))
(defmethod emitSpace ((self test-parser)) (emit-string self " "))
(defmethod emitPlus ((self test-parser)) (emit-string self "+"))
(defmethod symbolPush ((self test-parser)) (push (accepted-token self) (symbol-stack self)))
(defmethod integerPush ((self test-parser)) (push (accepted-token self) (symbol-stack self)))
(defmethod symbolPop ((self test-parser)) (pop (symbol-stack self)))
(defmethod emitTop ((self test-parser)) (emit-string self "~a" (token-text (first (symbol-stack self)))))
;; end mechanisms
  
(defun test2 ()
  ;; cascade DSLs
  (let ((p (make-instance 'test-parser)))
    (let ((r (transpile p *test-rpa-spec-third* *test-dsl-code2* 'rp-assembler::calculator)))
      (format *standard-output* "~&      result=~a~%" r)
      (format *standard-output* "~&       final=~a~%" 
	      (transpile p *test-rpa-spec-fourth* r 'rp-assembler::calculator)))))

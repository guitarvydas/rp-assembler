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
  emitAtom1
  emitPlus
  emitAtom2
  stackPop
  stackPop

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
  emitCurriedPlus
  '+'
  emitSpace
  @num
  emitTop
  emitClose
  stackPop
  stackPop

")

(defparameter *test-dsl-code2*
  "
17 + 25
")


(defclass test-parser (parser)
  ((atom-stack :accessor atom-stack)))

(defmethod initially ((self test-parser) token-list)
  (setf (atom-stack self) nil)
  (call-next-method))

;; test mechanisms
(defmethod emitOpen ((self test-parser)) (emit-string self "("))
(defmethod emitClose ((self test-parser)) (emit-string self ")"))
(defmethod emitOpenBrace ((self test-parser)) (emit-string self "{"))
(defmethod emitCloseBrace ((self test-parser)) (emit-string self "}"))
(defmethod emitSpace ((self test-parser)) (emit-string self " "))
(defmethod emitPlus ((self test-parser)) (emit-string self "+"))
(defmethod symbolPush ((self test-parser)) (push (accepted-token self) (atom-stack self)))
(defmethod integerPush ((self test-parser)) (push (accepted-token self) (atom-stack self)))
(defmethod stackPop ((self test-parser)) (pop (atom-stack self)))
(defmethod emitAtom1 ((self test-parser)) (emit-string self "~a" (token-text (second (atom-stack self)))))
(defmethod emitAtom2 ((self test-parser)) (emit-string self "~a" (token-text (first (atom-stack self)))))
(defmethod emitTop ((self test-parser)) (emit-string self "~a" (token-text (first (atom-stack self)))))
(defmethod emitCurriedPlus ((self test-parser)) (emit-string self "(lambda(x)(+ ~a x))" (token-text (first (atom-stack self)))))
;; end mechanisms
  
(defun test2 ()
  ;; cascade DSLs
  (let ((p (make-instance 'test-parser)))
    (let ((r (transpile p *test-rpa-spec-third* *test-dsl-code2* 'rp-assembler::calculator)))
      (format *standard-output* "~&      result=~a~%" r)
      (format *standard-output* "~&       final=~a~%" 
	      (transpile p *test-rpa-spec-fourth* r 'rp-assembler::calculator)))))

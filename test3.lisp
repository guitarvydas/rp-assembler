(in-package :rp-assembler)

(defparameter *test-rpa-spec-3a*
  "
= emitOperands
  {[ &operandStackEmpty >
   | * emitSpace operandEmit operandPop emitClose
   ]}

= rmSpaces
  [ ?SPACE | ?COMMENT | * . ]

= sym
  SYMBOL operandPushSymbol

= num
  INTEGER operandPushInteger

= calculator
  ~rmSpaces
  @num '+' @num
  operandEmit
  operandPop
  emitPlus
  operandEmit
  operandPop
")

(defparameter *test-rpa-spec-3b*
  "
%% defined above
%= emitOperands
%  {[ &operandStackEmpty >
%   | * emitSpace operandEmit operandPop
%   ]}

%= rmSpaces
%  [ ?SPACE | ?COMMENT | * . ]

= emitOperations
  {[ &operatorStackEmpty >
   | * 
      emitOpen
      emitCurriedOperator
      operandPop 
      operatorPop 
      emitSpace 
      operandEmit
      operandPop
      emitClose
      %% curry consumes one operand and one operator
  ]}

= operand
  [ ?SYMBOL SYMBOL operandPushSymbol
  | ?INTEGER INTEGER operandPushInteger
  | *
  ]

= tail
  {[ ?'+'
    '+'
    operatorPush
    @operand
  | * >
  ]}

= calculator
  ~rmSpaces
  @operand
  @tail
  @emitOperations
  @emitOperands  %% one remaining operand
")

(defclass test-parser3 (parser)
  ((operand-stack :accessor operand-stack)
   (operator-stack :accessor operator-stack)))


(defmethod initially ((self test-parser3) token-list)
  (setf (operand-stack self) nil)
  (setf (operator-stack self) nil)
  (call-next-method))


;; mechanisms
(defmethod emitOpen ((self test-parser3)) (emit-string self "("))
(defmethod emitClose ((self test-parser3)) (emit-string self ")"))
(defmethod emitSpace ((self test-parser3)) (emit-string self " "))
(defmethod emitPlus ((self test-parser3)) (emit-string self "+"))
(defmethod operandStackEmpty ((self test-parser3)) (if (null (operand-stack self)) +succeed+ +fail+))
(defmethod operandPushSymbol ((self test-parser3)) (push (accepted-token self) (operand-stack self)))
(defmethod operandPushInteger ((self test-parser3)) (push (accepted-token self) (operand-stack self)))
(defmethod operandPop ((self test-parser3)) (pop (operand-stack self)))
(defmethod operandEmit ((self test-parser3)) (emit-string self "~a" (token-text (first (operand-stack self)))))
(defmethod operatorPush ((self test-parser3)) (push (accepted-token self) (operator-stack self)))
(defmethod operatorPop  ((self test-parser3)) (pop (operator-stack self)))
(defmethod operatorStackEmpty ((self test-parser3)) (if (null (operator-stack self)) +succeed+ +fail+))
(defmethod emitCurriedOperator ((self test-parser3)) 
  (emit-string self "(lambda(x)(~a x ~a))" 
	       (token-text (first (operator-stack self)))
	       (token-text (first (operand-stack self)))))
;; end mechanisms
  
(defparameter *test-dsl-code3* "16 + 26")

(defun test3 ()
  (format *standard-output* "~&test 3~%")
  (let ((p (make-instance 'test-parser3)))
    (let ((r (transpile p *test-rpa-spec-3a* *test-dsl-code3* 'rp-assembler::calculator)))
      (format *standard-output* "~&      result=~a~%" r)
      (format *standard-output* "~&       final=~a~%" 
	      (transpile p *test-rpa-spec-3b* r 'rp-assembler::calculator)))))

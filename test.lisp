(in-package :rp-assembler)

;; the test *test-string* produces gibberish Lisp, and is
;; only used to view the emission for each kind of RPA
;; operation (see READING.txt)
(defparameter *test-string*
"
% test rp DSL

= junk-rule
  'a'
  ?'a'
  AAA
  ?BBB
  CCC/ddd
  ?EEE/fff
  @rule
  &pred
  external-call
  {[ &pred @rule
   | &pred @rule
   | * >
  ]}
  
= delete-whitespace
  [ ?SPACE % if lookahead is a space
    SPACE  % accept it and don't emit anything
  | * accept-and-return-token
]
")

;; mini-test ... (total nonsense in meaning, but legal syntax)
(defparameter *str* "= junk-rule CCC/ddd")

(defparameter *test-rpa-spec-first*
  "
= calculator
  ~rmSpaces
  SYMBOL symbolPush '+' SYMBOL symbolPush
  emitSymbol1
  emitPlus
  emitSymbol2
  symbolPop
  symbolPop

= rmSpaces
  [ ?SPACE | ?COMMENT | * . ]
")

(defparameter *test-rpa-spec-second*
  "
= calculator
  ~rmSpaces
  SYMBOL symbolPush '+' SYMBOL symbolPush
  emitOpen
  emitPlus
  emitSpace
  emitSymbol1
  emitSpace
  emitSymbol2
  emitClose
  symbolPop
  symbolPop

= rmSpaces
  [ ?SPACE | ?COMMENT | * . ]
")

(defparameter *test-dsl-code*
  "
x + y
")







(defclass test-parser (parser)
  ((symbol-stack :accessor symbol-stack)))

(defmethod initially ((self test-parser) token-list)
  (setf (symbol-stack self) nil)
  (call-next-method))

;; test mechanisms
(defmethod emitOpen ((self test-parser)) (emit-string self "("))
(defmethod emitClose ((self test-parser)) (emit-string self ")"))
(defmethod emitSpace ((self test-parser)) (emit-string self " "))
(defmethod emitPlus ((self test-parser)) (emit-string self "+"))
(defmethod symbolPush ((self test-parser)) (push (accepted-token self) (symbol-stack self)))
(defmethod symbolPop ((self test-parser)) (pop (symbol-stack self)))
(defmethod emitSymbol1 ((self test-parser)) (emit-string self "~a" (token-text (second (symbol-stack self)))))
(defmethod emitSymbol2 ((self test-parser)) (emit-string self "~a" (token-text (first (symbol-stack self)))))
;; end mechanisms
  
(defun test0 ()
  (let ((p (make-instance 'test-parser)))
    (let ((r (transpile p *test-rpa-spec* *test-dsl-code* 'rp-assembler::calculator)))
      (format *standard-output* "~&      result=~a~%" r))))

(defun test ()
  ;; cascade DSLs
  (let ((p (make-instance 'test-parser)))
    (let ((r (transpile p *test-rpa-spec-first* *test-dsl-code* 'rp-assembler::calculator)))
      (format *standard-output* "~&      result=~a~%" r)
      (format *standard-output* "~&       final=~a~%" 
	      (transpile p *test-rpa-spec-second* r 'rp-assembler::calculator)))))

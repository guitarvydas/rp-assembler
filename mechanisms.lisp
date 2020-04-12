(in-package :rp-assembler)

(defmethod initially ((self parser) token-list)
  (let ((empty-token (scanner:make-token :kind :empty :text "" :line 0 :position 0)))
    (setf (next-token self) (pop token-list))
    (setf (accepted-token self) empty-token)
    (setf (token-stream self) token-list)
    (setf (output-string-stream self) (make-string-output-stream))))

(defmethod advance-next-token ((self parser))
  (if (null (token-stream self))
      (setf (next-token self) (scanner:make-token :kind :eof :text "generated eof" :line 0 :position 0))
      (setf (next-token self) (pop (token-stream self)))))
  
(defmethod accept ((self parser))
  (setf (accepted-token self) (next-token self))
  (unless (eq :EOF (accepted-token self))
    (advance-next-token self)))

(defun format-token (tok)
  (format nil "kind=~s line=~a position=~a text=~a" (token-kind tok) (token-line tok) (token-position tok)
	  (token-text tok)))

(defmethod rp-parse-error ((self parser) message)
  (let ((final-message (format nil "~s, but got ~s" message (format-token (next-token self)))))
    (error final-message)))

(defmethod lookahead-char? ((self parser) c)
  (let ((tok (next-token self)))
    (if (or
	 (and (eq :character (token-kind tok))
	      (char= c (token-text tok)))
	 (and (eq :symbol (token-kind tok))
	      (= 1 (length (token-text tok)))
	      (char= (char (token-text tok) 0) c)))
       +succeed+
      +fail+)))

(defmethod lookahead-symbol? ((self parser) str)
  (let ((tok (next-token self)))
    (if (and (eq :character (token-kind tok))
	      (string= str (token-text tok)))
       +succeed+
      +fail+)))

(defmethod lookahead? ((self parser) kind)
  (let ((tok (next-token self)))
    (if (eq kind (token-kind tok))
       +succeed+
      +fail+)))
       
(defmethod input-char ((self parser) c)
  (if (eq +succeed+ (lookahead-char? self c))
      (accept self)
     (rp-parse-error self (format nil "expected character ~a" c))))

(defmethod input-symbol ((self parser) str)
  (flet ((nope () (rp-parse-error self (format nil "expected :symbol with text ~a" str))))
    (if (lookahead? self :symbol)
	(if (string= str (scanner:token-text (rpa:next-token self)))
	   (accept)
	  (nope))
	(nope))))

(defmethod input ((self parser) kind)
  (let ((tok (next-token self)))
    (if (eq kind (token-kind tok))
       (accept self)
      (rp-parse-error self (format nil "expected token ~s" kind)))))
	
(defmethod emit-string ((self parser) fmtstr &rest args)
  (let ((out (output-string-stream self)))
    (apply 'cl:format out fmtstr args)))


(defmethod call-rule ((self parser) func)
  (funcall func))

(defmethod call-predicate ((self parser) func)
  (let ((%result (funcall func)))
    %result))

(defmethod call-external ((self parser) func)
  (funcall func))

(defmethod accept-and-return-token ((p parser))
  (accept p)
  (accepted-token p))

(defmethod rp-filter-stream ((p parser) rule-name)
  (let ((%new-list nil))
    (loop (when (eq :EOF (token-kind (accepted-token p)))
	    (return))
(format *standard-output* "~&filter ~s ~a ~s ~a~%"
	(token-kind (accepted-token p)) (token-text (accepted-token p))
	(token-kind (next-token p)) (token-text (next-token p)))
       (let ((%a (accepted-token p))
	     (%n (next-token p)))
	 (funcall rule-name p)
	 (if (and (eq %a (accepted-token p)) (eq %n (next-token p)))
	     (accept p)
	     (push (accepted-token p) %new-list))))
(format *standard-output* "~&finishing filter 1~%")
    (initially p (reverse %new-list))
(format *standard-output* "~&finishing filter 2~%")))

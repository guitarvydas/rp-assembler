(in-package :rp-assembler)

(defmethod input-upcase-symbol ((p parser))
  (let ((tok (next-token p)))
    (if (and (eq :symbol (token-kind tok))
	     (all-upcase? (token-text tok)))
       (accept p)
      (rp-parse-error p "expected upcase symbol"))))

(defmethod look-upcase-symbol? ((p parser))
  (let ((tok (next-token p)))
    (if (and (eq :symbol (token-kind tok))
	     (all-upcase? (token-text tok)))
	rpa:+succeed+
      rpa:+fail+)))

(defun all-upcase? (str)
  (string= str (string-upcase str)))

(defmethod lookahead-any-char? ((self parser))
  (let ((tok (rpa:next-token self)))
    (if (or
	 (eq :character (scanner:token-kind tok))
	 (and (eq :symbol (scanner:token-kind tok))
	      (= 1 (length (scanner:token-text tok)))))
       rpa:+succeed+
      rpa:+fail+)))

(defmethod input-any-character ((p parser))
  (if (eq rpa:+succeed+ (lookahead-any-char? p))
      (accept p)
     (rp-parse-error p (format nil "expected any character (or a symbol of length 1)"))))

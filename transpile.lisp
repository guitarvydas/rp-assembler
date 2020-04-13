(in-package :rp-assembler)

(defmethod transpile ((p parser) rpa-dsl-spec-string dsl-string start-function)
  ;; input == two strings (a) a spec for the DSL written in RPA and (b) a program written in the DSL
  ;;  a function symbol which is called to run the transpiled DSL
  ;; output == string, code (Lisp) that is the DSL program rewritten in Lisp
  (let ((scanned-rpa (scanner:scanner rpa-dsl-spec-string)))
    (initially p scanned-rpa)
    (rp-assembler::<rp> p)
    (let ((dsl-lisp-string (get-output-stream-string (rpa:output-string-stream p))))
      (compile-string-as-file dsl-lisp-string) 
      (let ((scanned-dsl (scanner:scanner dsl-string)))
	(initially p scanned-dsl)
	(funcall start-function p)
	(get-output-stream-string (rpa:output-string-stream p))))))

(defun compile-string-as-file (str)
  (with-open-file (f "/tmp/temp.lisp" :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-string "(in-package :rp-assembler)

" f)
    (write-string str f))
  (load "/tmp/temp.lisp"))

		    

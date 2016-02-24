(defun http-byte (c1 c2 &optional (default #.(char-code #\space)))
  (let ((code (parse-integer
	       (coerce (list (code-char c1) (code-char c2)) 'string)
	       :radix 16
	       :junk-allowed t)))
    (or code default)))

(defun decode-param (s)
  (labels ((f (lst)
	     (when lst
	       (case (car lst)
		 (#.(char-code #\%) (cons (http-byte (cadr lst) (caddr lst))
			    (f (cdddr lst))))
		 (#.(char-code #\+) (cons #.(char-code #\space) (f (cdr lst))))
		 (otherwise (cons (car lst) (f (cdr lst))))))))
    (sb-ext:octets-to-string
     (coerce (f (coerce (sb-ext:string-to-octets s :external-format :utf-8) 'list))
	     '(vector(unsigned-byte 8)))
     :external-format :utf-8)))

(defun parse-params (s)
  (let ((i1 (position #\= s))
	(i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
			  (decode-param (subseq s (1+ i1) i2)))
		    (and i2 (parse-params (subseq s (1+ i2))))))
	  ((equal s "") nil)
	  (t s))))

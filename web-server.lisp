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

(defun parse-url (s)
  (let* ((url (subseq s
		      (+ 2 (position #\space s))
		      (position #\space s :from-end t)))
	 (x (position #\? url)))
    (if x
	(cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
	(cons url '()))))

;; in http, a newline and a space is occasionally inserted to start a newline
;; when a request-header is too long to see.
;;
;; imput example: 
;;
;; (get-header (make-string-input-stream "foo: 1
;;  2345
;; bar: abc, 123
;;
;; "))
(defun get-header (stream)
  (let* ((lst (read-all-lines stream))
	 (h '()))
    (print lst)
    (labels ((connect-lines (l lst)
	       (print l)
	       (if (car lst)
		   (if (equal (subseq (car lst) 0 1) " ")
		       (connect-lines (concatenate 'string l (subseq (car lst) 1)) (cdr lst))
		       (progn (setf h (nconc h `(,l)))
			      (print h)
			      (connect-lines (car lst) (cdr lst))))
		   (setf h (nconc h `(,l))))))
      (connect-lines (car lst) (cdr lst))
      (mapcar #'(lambda (x)
		  (let ((i (position #\: x)))
		    (cons (intern (string-upcase (subseq x 0 i)))
			  (subseq x (+ i 2)))))
	      h))))

(defun read-all-lines (stream)
  (let ((s (read-line stream)))
    (unless (equal (subseq s 0) "")
      (list* s (read-all-lines stream)))))

(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
	(read-sequense content stream)
	(parse-params content)))))


(require :sb-bsd-sockets)

(defun serve (request-handler)
  (unwind-protect			
       (progn (defparameter *sbcl-socket* (make-instance
					   'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
	      (sb-bsd-sockets:socket-bind *sbcl-socket* #(127 0 0 1) 8000)
	      (sb-bsd-sockets:socket-listen *sbcl-socket* 5)
	      (loop (with-open-stream (stream (sb-bsd-sockets:socket-accept *sbcl-socket*))
		      (let* ((url (parse-url (read-line stream)))
			     (path (car url))
			     (header (get-header stream))
			     (params (append (cdr url)
					     (get-content-params stream header)))
			     (*standard-output* stream))
			(funcall request-handler path header params)))))
    (sb-bsd-sockets:socket-close *sbcl-socket*)))

(defun hello-request-handler (path header params)
  (if (equal path "greeting")
      (let ((name (assoc 'name params)))
	(if (not name)
	    (princ "<html><form>What is your name?<input name='name' /></form></html>")
	    (format t "<html>Nice to meet you, ~a!</html>" (cdr name))))
      (princ "Sorry, I don't know that page.")))

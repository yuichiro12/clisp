(defun main (m)
  (print (type-of m))
  (cond
     (eq (type-of m) 'cons) (if (and (eq (type-of (car m)) 'number) (complement (cdr m))
				m
				(princ "The element was not number. Bye."))
    ((eq (type-of m) 'list) (if ())
     (otherwise (princ "Input was not list. Bye.")))))
      

(defun add-widget (database widget)
  (cons widget database))

(defparameter *database* nil)

(defun main-loop ()
  (loop (princ "Please enter the name of a new widget:")
     (setf *database* (add-widget *database* (read)))
     (format t "The database contains the following: ~a~%" *database*)))

(defun add-two (list)
  (when list
    (cons (+ 2 (car list)) (add-two (cdr list)))))

(defun add-two-improve (list)
  (mapcar (lambda (x)
	    (+ x 2))
	  '(4 7 2 3)))

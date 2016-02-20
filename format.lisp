(defun random-animal ()
  (nth (random 5) '("dog" "tick" "tiger" "walrus" "kangaroo")))

(loop repeat 10
  do (format t "~5t~a ~15t~a ~25t~a~%"
	     (random-animal)
	     (random-animal)
	     (random-animal)))

(loop repeat 10
   do (format t "~30<~a~;~a~;~a~>~%"
	      (random-animal)
	      (random-animal)
	      (random-animal)))

(loop repeat 10
   do (format t "~10:@<~a~>~10:@<~a~>~10:@<~a~>~%"
	      (random-animal)
	      (random-animal)
	      (random-animal)))

(defparameter *animals* (loop repeat 10 collect (random-animal)))

(format t "~{I see a ~a! ~}" *animals*)
(fresh-line)

(format t "|~{~<|~%|~,33:;~2d ~>~}|" (loop for x below 100 collect x))

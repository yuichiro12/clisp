(defstruct person
  name
  age
  waist-size
  favorite-color)

(defparameter *bob*(make-person :name "Bob"
				:age 35
				:waist-size 32
				:favorite-color "blue"))

(defun make-person (name age waist-size favorite-color)
  (list name age waist-size favorite-color))

(defun person-age (person)
  (cadr person))

(print (reduce (lambda (best item)
		 (if (and (evenp item) (> item best))
		     item
		     best))
	       '(7 4 6 5 2)
	       :initial-value 0))

(defun sum (lst)
  (reduce #'+ lst))

(print (map 'string (lambda (x)
		    (if (eq x #\s)
			#\S
			x))
	    "this is a string"))

(defmethod add ((a number) (b number))
  (+ a b))

(defmethod add ((a list) (b list))
  (append a b))

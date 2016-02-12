(defparameter *nodes* '(
	(living-room (you are in the living-room.
		a wizard is snoring loudly on the couch.))
	(garden (you are in a beautiful garden.
		there is a well in front of you.))
	(attic (you are in the attic.
		there is a giant welding torch in the corner.))))

(print(assoc 'garden *nodes*))

(defun describe-location (location nodes) (cadr (assoc location nodes)))

(print(describe-location 'living-room *nodes*))

(defparameter *edges* '((living-room (garden west door)
				     (attic upstairs ladder))
			(garden (living-room east door))
			(attic (living-room downstairs ladder))))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(print (describe-path '(garden west door)))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(print (describe-paths 'living-room *edges*))

(print (mapcar #'sqrt '(1 2 3 4 5)))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
				   (bucket living-room)
				   (chain garden)
				   (frog garden)))

(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
		    (eq (cadr (assoc obj obj-locs)) loc)))
  (remove-if-not #'at-loc-p objs)))

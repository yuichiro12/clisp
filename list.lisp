(defparameter *drink-order* '((bill . double-espresso)
			      (lisa . small-drip-coffee)
			      (john . medium-latte)))

(print (assoc 'lisa *drink-order*))

(push '(lisa . large-macha-with-whipped-cream) *drink-order*)

(print (assoc 'lisa *drink-order*))

(defparameter *wizard-nodes* '((living-room (you are in the living-room.
						 awizard is snoring loudly on the couch.))
			       (garden (you are in a beautiful garden.
					    there is a well in front of you.))
			       (attic (you are in the attic. there
					   is a giant welding torch in the corner.))))
(defparameter *wizard-edges* '((living-room (garden west door)
					    (attic upstairs ladder))
			       (garden (living-room east door))
			       (attic (living-room downstairs ladder))))

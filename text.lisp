;; (progn (print "flip") (prin1 "flop"))

;; (defun say-hello ()
;;   (print "Please type your name:")
;;   (let ((name (read)))
;;     (print "Nice to meet you, ")
;;     (print name)))

;; (say-hello)

;; (defun add-five ()
;;   (print "please enter a number:")
;;   (let ((num (read)))
;;     (print "When I add five I get")
;;     (print (+ num 5))))

;; (add-five)

;; (progn (princ "This sentence will be interrupted")
;;        (princ #\newline)
;;        (princ "by an abboying newline character."))

(defun say-hello ()
  (princ "Please type your name:")
	 (let ((name (read-line)))
	   (princ "Nice to meet you, ")
		  (princ name)))
(say-hello)

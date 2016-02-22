(defun robots ()
  (loop named main
     with directions = '((q . -65) (w . -64) (e . -63) (a . -1)
			 (d .   1) (z .  63) (x .  64) (c . 65))
     for pos = 544
     then (progn (format t "~%qwe/asd/zxc to move, (t)eleport, (l)eave:")
		 (force-output)
		 (let* ((c (read))
			(d (assoc c directions)))
		   (cond (d (+ pos (cdr d))))))))

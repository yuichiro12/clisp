(defpackage #:generic-arithmetic 
  (:use :common-lisp)
  (:shadow "+" "-" "*" "/" exp sin cos tan sqrt)
  (:export))

(in-package #:generic-arithmetic)

(defstruct dual-number (r 0) (d 0))

(defun + (&rest x)
  (reduce 'binary+ (cdr x) :initial-value (car x)))

(defun - (&rest x)
  (reduce 'binary- (cdr x) :initial-value (car x)))

(defun * (&rest x)
  (reduce 'binary* (cdr x) :initial-value (car x)))

(defun / (&rest x)
  (reduce 'binary/ (cdr x) :initial-value (car x)))

(defun exp (x) (binaryexp x))

(defun sin (x) (binarysin x))

(defun cos (x) (binarycos x))

(defun tan (x) (binarytan x))

(defun sqrt (x) (binarysqrt x))


;; generic function
(defgeneric binary+ (a b))

(defgeneric binary- (a b))

(defgeneric binary* (a b))

(defgeneric binary/ (a b))

(defgeneric binaryexp (x))

(defgeneric binarysin (x))

(defgeneric binarycos (x))

(defgeneric binarytan (x))

(defgeneric binarysqrt (x))


;; + operator
(defmethod binary+ ((a number) (b number)) (cl:+ a b))

(defmethod binary+ ((a number) (b dual-number))
  (make-dual-number
   :r (+ a (dual-number-r b))
   :d (dual-number-d b)))

(defmethod binary+ ((a dual-number) (b number))
  (make-dual-number
   :r (+ (dual-number-r a) b)
   :d (dual-number-d a)))

(defmethod binary+ ((a dual-number) (b dual-number))
  (make-dual-number
   :r (+ (dual-number-r a) (dual-number-r b))
   :d (+ (dual-number-d a) (dual-number-d b))))


;; - operator
(defmethod binary- ((a number) (b number)) (cl:- a b))

(defmethod binary- ((a number) (b dual-number))
  (make-dual-number
   :r (- a (dual-number-r b))
   :d (- 0 (dual-number-d b))))

(defmethod binary- ((a dual-number) (b number))
  (make-dual-number
   :r (- (dual-number-r a) b)
   :d (dual-number-d a)))

(defmethod binary- ((a dual-number) (b dual-number))
  (make-dual-number
   :r (- (dual-number-r a) (dual-number-r b))
   :d (- (dual-number-d a) (dual-number-d b))))


;; * operator
(defmethod binary* ((a number) (b number)) (cl:* a b))

(defmethod binary* ((a number) (b dual-number))
 (make-dual-number
  :r (* a (dual-number-r b))
  :d (* a (dual-number-d b))))

(defmethod binary* ((a dual-number) (b number))
  (make-dual-number
   :r (* (dual-number-r a) b)
   :d (* (dual-number-d a) b)))

(defmethod binary* ((a dual-number) (b dual-number))
  (make-dual-number
   :r (* (dual-number-r a) (dual-number-r b))
   :d (+ (cl:* (dual-number-r a) (dual-number-d b))
	 (cl:* (dual-number-d a) (dual-number-r b)))))


;; / operator
(defmethod binary/ ((a number) (b number)) (cl:/ a b))

(defmethod binary/ ((a number) (b dual-number))
  (setf div (expt (dual-number-r b) 2))
  (make-dual-number
   :r (/ (* a (dual-number-r b)) div)
   :d (/ (* a (dual-number-d (conj b))) div)))

(defmethod binary/ ((a dual-number) (b number))
  (make-dual-number
   :r (/ (dual-number-r a) b)
   :d (/ (dual-number-d a) b)))

(defmethod binary/ ((a dual-number) (b dual-number))
  (setf div (expt (dual-number-r b) 2))
  (make-dual-number
   :r (/ (dual-number-r (* a (conj b))) div)
   :d (/ (dual-number-d (* a (conj b))) div)))


;; comparison
;; (defmethod binary>= ((a number) (b number)) (cl:>= a b))

;; (defmethod binary>= ((a dual-number) (b dual-number))
;;   (>= (dual-number-r a) (dual-number-r b)))

;; (defmethod binary<= ((a number) (b number)) (cl:<= a b))

;; (defmethod binary<= ((a dual-number) (b dual-number))
;;   (<= (dual-number-r a) (dual-number-r b)))


;; exp
(defmethod binaryexp ((x number)) (cl:exp x))

(defmethod binaryexp ((x dual-number))
  (* (exp (dual-number-r x)) (make-dual-number :r 1 :d (dual-number-d x))))


;; sin
(defmethod binarysin ((x number)) (cl:sin x))

(defmethod binarysin ((x dual-number))
  (make-dual-number
   :r (sin (dual-number-r x))
   :d (* (cos (dual-number-r x)) (dual-number-d x))))

;; cos
(defmethod binarycos ((x number)) (cl:cos x))

(defmethod binarycos ((x dual-number))
  (make-dual-number
   :r (cos (dual-number-r x))
   :d (- 0 (* (sin (dual-number-r x)) (dual-number-d x)))))

;; tan
(defmethod binarytan ((x number)) (cl:tan x))

(defmethod binarytan ((x dual-number))
  (setf a (expt (cos (dual-number-r x)) 2))
  (make-dual-number
   :r (/ (sin (* 2 (dual-number-r x))) 2 a)
   :d (/ (* (dual-number-d x) (cos (* 2 (dual-number-r x)))) a)))

;; sqrt
(defmethod binarysqrt ((x number)) (cl:sqrt x))

(defmethod binarysqrt ((x dual-number))
  (setf a (sqrt (dual-number-r x)))
  (make-dual-number
   :r a
   :d (/ (dual-number-d x) 2 a)))


;; conjugate for dual number
(defun conj (a)
  (make-dual-number
   :r (dual-number-r a)
   :d (- 0 (dual-number-d a))))


;; norm for dual number
(defun norm (a)
  (sqrt (expt (dual-number-r a) 2)))



;; return the differentiation
(defun diff (f a)
  (setf x (make-dual-number :r a :d 1))
  (dual-number-d `(,f x)))

(defun func (x) (+ (* (expt x) x) (cos x)))

;; (setf dual (make-dual-number r: 2 d: 6))

;; (print (func dual))



(defvar dual (make-dual-number :r 1 :d 6))
(defvar  dual2 (make-dual-number :r 2 :d 3))

(print (- 1 3 dual 3 dual2 dual 2))
(print (* 1 3 dual 3 dual2 dual 2 pi))
(print (/ 1 3 dual 3 dual2 dual 2))
(print (exp dual))
(print (sin pi))
(print (sin (make-dual-number :r pi :d 0)))
(print (cos pi))
(print (sqrt 3))
(print (sqrt dual2))


;; (print (tan (make-dual-number :r pi :d 0)))
;; (print (tan dual2))
;; (print (>= dual dual2))
;; (print (>= dual2 dual))
;; (print (<= dual2 dual))
;; (print (<= dual dual2))
;; (print (>= (make-dual-number :r 1 :d 6) (make-dual-number :r 2 :d 3)))
;; (print (> (make-dual-number :r 1 :d 6) (make-dual-number :r 2 :d 3)))
;; (print (<= (make-dual-number :r 1 :d 6) (make-dual-number :r 2 :d 3)))
;; (print (< (make-dual-number :r 1 :d 6) (make-dual-number :r 2 :d 3)))


;; TODO: tangent test
;; TODO: export
;; TODO: comparison operator

(funcall #'func dual)


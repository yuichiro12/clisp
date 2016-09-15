(defpackage #:generic-arithmetic 
  (:use "COMMON-LISP")
  (:shadow "+" "-" "*" "/" "exp" "sin" "cos"))

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

(defun exp (x) (exp x))

(defun sin (x) (sin x))

(defun cos (x) (cos x))


;; generic function
(defgeneric binary+ (a b))

(defgeneric binary- (a b))

(defgeneric binary* (a b))

(defgeneric binary/ (a b))

(defgeneric exp (x))

(defgeneric sin (x))

(defgeneric cos (x))


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

;; exp
(defmethod exp ((x number)) (cl:exp x))

(defmethod binaryexp ((x dual-number))
  (* (exp (dual-number-r x)) (make-dual-number :r 1 :d (dual-number-d x))))

;; conjugate for dual number
(defun conj (a)
  (make-dual-number
   :r (dual-number-r a)
   :d (- 0 (dual-number-d a))))


;; norm for dual number
(defun norm (a)
  (sqrt (expt (dual-number-r a) 2)))

(defvar dual (MAKE-DUAL-NUMBER :r 1 :d 6))
(defvar dual2 (MAKE-DUAL-NUMBER :r 2 :d 3))

(print (+ 1 3 dual 3 dual2 dual 2))
(print (- 1 3 dual 3 dual2 dual 2))
(print (* 1 3 dual 3 dual2 dual 2))
(print (/ 1 3 dual 3 dual2 dual 2))
(print (exp 4))

(require :sb-bsd-sockets)

(defparameter *client* nil)
(defvar *client-stream* nil)
(defparameter *sbcl-socket* nil)

(defparameter *sbcl-socket* (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))

(sb-bsd-sockets:socket-bind *sbcl-socket* #(127 0 0 1) 4444)
(sb-bsd-sockets:socket-listen *sbcl-socket* 5)
(defparameter *client* (sb-bsd-sockets:socket-accept *sbcl-socket*))

;; receive
;; (sb-bsd-sockets:socket-receive *client* buffer length)

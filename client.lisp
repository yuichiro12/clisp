(require :sb-bsd-sockets)
(defparameter *sbcl-socket* nil)

(defparameter *sbcl-socket* (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))

(sb-bsd-sockets:socket-connect *sbcl-socket* #(127 0 0 1) 4444)

;; send
;; (sb-bsd-sockets:socket-send *sbcl-socket* "aaaaa" 6)

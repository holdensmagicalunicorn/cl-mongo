(in-package :cl-mongo-tim)

(defclass mongo-pair ()
  ((key :accessor pair-key :initarg :key :initform "")
   (value :accessor pair-value :initarg :value :initform "")))

(defgeneric make-pair (key value)
  (:documentation "Make a key-value mongo-pair."))

(defmethod make-pair ((key string) value)
  (make-instance 'mongo-pair :key key :value value))

(defgeneric mongo-print (object &optional stream)
  (:documentation "Pretty print a mongo object."))

(defmethod mongo-print ((pair mongo-pair) &optional (stream *standard-output*))
  (format stream "(~A : ~A)"
          (mongo-print (pair-key pair) nil)
          (mongo-print (pair-value pair) nil)))

(defmethod mongo-print ((a-string string)
                        &optional (stream *standard-output*))
  (let ((*print-escape* t))
    (format stream "~S" a-string)))

(defmethod mongo-print ((an-int integer)
                        &optional (stream *standard-output*))
  (format stream "~A" an-int))

(defun get-pair-slot (pair &rest slot-positions)
  "Get direct key/value of a pair based on its level/position.  For
  example:

``(get-pair-slot (m-pair \"Hell\" (m-pair \"world\" 10)) 2 2) => 10``"
  (let ((current-pair pair))
    (loop
       for slot-pos in slot-positions
       do (setf current-pair (if (= 1 slot-pos)
                                 (pair-key current-pair)
                                 (pair-value current-pair))))
    current-pair))

;;;
;;; Simplified helper functions
;;;
(defun m-pair (key value)
  (make-pair key value))

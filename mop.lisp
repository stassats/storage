;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:movies)

(defclass storable-class (standard-class)
  ())

(defmethod validate-superclass
    ((class standard-class)
     (superclass storable-class))
  t)

(defmethod validate-superclass
    ((class storable-class)
     (superclass standard-class))
    t)

(defclass storable-slot-mixin ()
  ((store-type :initarg :store-type
               :initform t
               :reader store-type)))

(defclass storable-direct-slot-definition (storable-slot-mixin
                                           standard-direct-slot-definition)
  ())

(defclass storable-effective-slot-definition
    (storable-slot-mixin standard-effective-slot-definition)
  ())

(defmethod direct-slot-definition-class ((class storable-class)
                                         &rest initargs)
  (declare (ignore initargs))
  (find-class 'storable-direct-slot-definition))

(defmethod effective-slot-definition-class ((class storable-class)
                                            &rest initargs)
  (declare (ignore initargs))
  (find-class 'storable-effective-slot-definition))

(defmethod compute-effective-slot-definition
    ((class storable-class)
     slot-name
     direct-definitions)
  (let ((effective-definition (call-next-method)))
    (setf (slot-value effective-definition 'store-type)
          (store-type (car direct-definitions)))
    effective-definition))

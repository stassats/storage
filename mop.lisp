;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:storage)

(defclass storable-class (standard-class)
  ((slots-to-store :initform nil
                   :accessor slots-to-store)
   (class-id :initform 0
             :accessor class-id)
   (objects :initform nil
            :accessor objects-of-class)
   (data :initform nil
         :accessor storage-data
         :allocation :class)
   (file :initform nil
         :accessor storage-file
         :allocation :class)))

(defmethod validate-superclass
    ((class standard-class)
     (superclass storable-class))
  t)

(defmethod validate-superclass
    ((class storable-class)
     (superclass standard-class))
    t)

(defclass storable-slot-mixin ()
  ((storep :initarg :storep
           :initform t
           :reader store-slot-p)))

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
    (setf (slot-value effective-definition 'storep)
          (store-slot-p (car direct-definitions)))
    effective-definition))

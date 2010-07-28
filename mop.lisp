;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:storage)

(defclass storage ()
  ((data :initform nil
         :accessor storage-data)
   (file :initform nil
         :initarg :file
         :accessor storage-file)))

(defclass storable-class (standard-class)
  ((slots-to-store :initform nil
                   :accessor slots-to-store)
   (class-id :initform 0
             :accessor class-id)
   (objects :initform nil
            :accessor objects-of-class)
   (storage :initform nil
            :initarg :storage
            :accessor class-storage)))

(declaim (ftype (function (t) simple-vector)
                slots-to-store))

(defun initialize-storable-class (next-method class
                                  &rest args &key direct-superclasses &allow-other-keys)
  (apply next-method class
         (if direct-superclasses
             args
             (list* :direct-superclasses (list (find-class 'identifiable))
                    args))))

(defmethod initialize-instance ((class storable-class)
                                &rest args)
  (apply #'initialize-storable-class #'call-next-method class args))

(defmethod reinitialize-instance ((class storable-class)
                                  &rest args)
  (apply #'initialize-storable-class #'call-next-method class args))

;;;

(defvar *class-cache* #())

(defun grow-cache ()
  (let* ((next-position (length *class-cache*))
         (new-cache (make-array (+ next-position 20) :initial-element nil)))
    (replace new-cache *class-cache*)
    (setf *class-cache* new-cache)
    next-position))

(defun assign-id-to-class (class)
  (loop for i from 0
        for cached-class across *class-cache*
        unless cached-class
        return (cache-class-with-id class i)
        when (eq cached-class class)
        return (setf (class-id class) i)
        finally (cache-class-with-id class (grow-cache)))
  t)

(defun cache-class-with-id (class id)
  (setf (class-id class) id)
  (setf (aref *class-cache* id) class))

(defun find-class-by-id (id)
  (aref *class-cache* id))

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
           :reader store-slot-p)
   (relationship :initarg :relationship
                 :initform nil
                 :reader slot-relationship)))

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

(defmethod effective-slot-definition-class ((class storable-class) &key)
  (find-class 'storable-effective-slot-definition))

(defmethod compute-effective-slot-definition
    ((class storable-class)
     slot-name
     direct-definitions)
  (declare (ignore slot-name))
  (let ((effective-definition (call-next-method)))
    (setf (slot-value effective-definition 'storep)
          (store-slot-p (car direct-definitions))
          (slot-value effective-definition 'relationship)
          (slot-relationship (car direct-definitions)))
    effective-definition))

(defmethod compute-slots :around ((class storable-class))
  (let ((slots (call-next-method)))
    (setf (slot-value class 'slots-to-store)
          (coerce (remove-if-not #'store-slot-p slots)
                  'simple-vector))
    slots))

(defmethod initialize-instance :after ((class storable-class) &key)
  (assign-id-to-class class))

;;;

(defvar *last-id* -1)

(defclass identifiable (standard-object)
  ((id :accessor id
       :initarg :id
       :initform nil
       :storep nil)
   (relationships :initarg :relationships
                  :initform nil
                  :accessor relationships
                  :storep nil))
  (:metaclass storable-class))

(defgeneric relationship (object type))

(defmethod relationship (object type)
  (getf (relationships object) type))

(defmethod initialize-instance :after ((object identifiable)
                                       &key id)
  (if (integerp id)
      (setf *last-id* (max *last-id* id))
      (setf (id object) (incf *last-id*))))

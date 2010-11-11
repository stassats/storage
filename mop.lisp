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
            :accessor class-storage)
   (search-key :initform nil
               :initarg :search-key
               :accessor search-key)))

(declaim (ftype (function (t) simple-vector)
                slots-to-store))

(defun initialize-storable-class (next-method class &rest args
                                  &key direct-superclasses &allow-other-keys)
  (apply next-method class
         (if direct-superclasses
             args
             (list* :direct-superclasses (list (find-class 'identifiable))
                    args))))

(defmethod initialize-instance :around ((class storable-class)
                                        &rest args)
  (apply #'initialize-storable-class #'call-next-method class args))

(defmethod reinitialize-instance :around ((class storable-class)
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
   (relation :initarg :relation
             :initform nil
             :reader slot-relation)
   (db-type :initarg :db-type
            :initform nil
            :reader slot-db-type)
   (read-only-p :initarg :read-only-p
                :initform nil
                :reader slot-read-only-p)
   (unit :initarg :unit
         :initform nil
         :reader slot-unit)))

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
                                            &key &allow-other-keys)
  (find-class 'storable-effective-slot-definition))

(defmethod compute-effective-slot-definition
    ((class storable-class) slot-name direct-definitions)
  (declare (ignore slot-name))
  (let ((effective-definition (call-next-method))
        (direct-definition (car direct-definitions)))
    (with-slots (storep relation db-type
                 read-only-p unit)
        effective-definition
      (setf storep (store-slot-p direct-definition)
            relation (slot-relation direct-definition)
            db-type (slot-db-type direct-definition)
            read-only-p (slot-read-only-p direct-definition)
            unit (slot-unit direct-definition)))
    effective-definition))

(defmethod compute-slots :around ((class storable-class))
  (let ((slots (call-next-method)))
    (setf (slot-value class 'slots-to-store)
          (coerce (remove-if-not #'store-slot-p slots)
                  'simple-vector))
    (compute-search-key class slots)
    slots))

(defun compute-search-key (class slots)
  (with-slots (search-key) class
    (let* ((key (or search-key
                    (loop for superclass in (class-direct-superclasses class)
                          thereis (and (typep superclass 'storable-class)
                                       (search-key superclass)))))
           (slot-name (typecase key
                       (cons (car key))
                       (effective-slot-definition
                        (slot-definition-name key)))))
      (when slot-name
        (setf search-key
              (or (find slot-name slots
                        :key #'slot-definition-name)
                  (error "Search key ~a for an uknown slot in class ~a"
                         slot-name class)))))))

(defmethod initialize-instance :after ((class storable-class) &key)
  (when (class-storage class)
   (pushnew class (storage-data (class-storage class)) :test #'eq))
  (assign-id-to-class class))

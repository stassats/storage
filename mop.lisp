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
   (slot-locations-and-initforms
    :initform nil
    :accessor slot-locations-and-initforms)
   (all-slot-locations-and-initforms
    :initform nil
    :accessor all-slot-locations-and-initforms)
   (relations :initform nil
              :accessor class-relations)
   (initforms :initform nil
	      :accessor class-initforms)
   (objects :initform nil
            :accessor objects-of-class)
   (storage :initform nil
            :initarg :storage
            :accessor class-storage)
   (search-key :initform nil
               :initarg :search-key
               :accessor search-key)))

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

(defun slots-with-relations (class)
  (loop for slot across (slots-to-store class)
        for relation = (slot-relation slot)
        when relation
        collect (cons (slot-definition-location slot)
                      relation)))

(defun make-slots-cache (slot-definitions)
  (map 'vector
       (lambda (slot-definition)
	 (cons (slot-definition-location slot-definition)
	       (slot-definition-initform slot-definition)))
       slot-definitions))

(defun initialize-class-slots (class &key slots-to-store-only)
  (setf (slot-value class 'slot-locations-and-initforms)
	(make-slots-cache (slots-to-store class)))
  (unless slots-to-store-only
    (setf (slot-value class 'all-slot-locations-and-initforms)
	  (make-slots-cache (class-slots class))
	  (class-initforms class)
	  (map 'vector #'slot-definition-initform (class-slots class)))))

(defmethod finalize-inheritance :after ((class storable-class))
  (let ((slots (class-slots class)))
    (setf (slot-value class 'slots-to-store)
	  (coerce (remove-if-not #'store-slot-p slots)
		  'simple-vector))
    (initialize-class-slots class)
    (compute-search-key class slots)
    (setf (class-relations class) (slots-with-relations class))))

(defun find-slot (slot-name class)
  (find slot-name (class-slots class)
        :key #'slot-definition-name))

(defun compute-search-key (class slots)
  (with-slots (search-key) class
    (let* ((key (or search-key
                    (loop for superclass in (class-direct-superclasses class)
                          thereis (and (typep superclass 'storable-class)
                                       (search-key superclass)))))
           (slot-name (typecase key
                        (cons (car key))
                        (symbol key))))
      (setf search-key slot-name)
      (when slot-name
        (unless (find slot-name slots :key #'slot-definition-name)
          (setf search-key nil)
          (error "Search key ~a for an uknown slot in class ~a"
                 slot-name class))))))

(defmethod initialize-instance :after ((class storable-class) &key)
  (when (class-storage class)
    (pushnew class (storage-data (class-storage class)) :test #'eq)))

;;;

(defclass identifiable (standard-object)
  ((id :accessor id
       :initarg :id
       :initform nil
       :storep nil
       :read-only-p t
       :db-type :integer)
   (relations :initarg :relations
              :initform nil
              :accessor relations
              :storep nil
              :read-only-p t
              :db-type :integer))
  (:metaclass storable-class))

(defgeneric relation (object type))

(defmethod relation (object type)
  (getf (relations object) type))

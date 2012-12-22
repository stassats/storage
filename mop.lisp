;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:storage)

(defclass storage ()
  ((modified :initform nil
             :accessor modified)
   (data :initform nil
         :accessor storage-data)
   (file :initarg :file
         :initform nil
         :accessor storage-file)))

(defclass storable-class (standard-class)
  ((slots-to-store :initform nil
                   :accessor slots-to-store)
   (slot-locations-and-initforms
    :initform nil
    :accessor slot-locations-and-initforms)
   (slot-locations-and-initforms-read
    :initform nil
    :accessor slot-locations-and-initforms-read)
   (all-slot-locations-and-initforms
    :initform nil
    :accessor all-slot-locations-and-initforms)
   (number-of-bytes-for-slots
    :initform nil
    :accessor number-of-bytes-for-slots)
   (relations :initform nil
              :accessor class-relations)
   (initforms :initform nil
              :accessor class-initforms)
   (objects :initform nil
            :accessor objects-of-class)
   (storage :initarg :storage
            :initform nil
            :accessor class-storage)
   (search-key :initarg :search-key
               :initform nil
               :accessor search-key)))

(defun initialize-storable-class (next-method class &rest args
                                  &key direct-superclasses &allow-other-keys)
  (apply next-method class
         (if direct-superclasses
             args
             (list* :direct-superclasses (list (find-class 'identifiable))
                    args))))

(defmethod initialize-instance :around ((class storable-class) &rest args)
  (apply #'initialize-storable-class #'call-next-method class args))

(defmethod reinitialize-instance :around ((class storable-class) &rest args)
  (apply #'initialize-storable-class #'call-next-method class args))

;;;

(defmethod validate-superclass
    ((class standard-class) (superclass storable-class))
  t)

(defmethod validate-superclass
    ((class storable-class) (superclass standard-class))
  t)

(defvar *slot-dummy* (gensym))

(defclass storable-slot-mixin ()
  ((storep :initarg :storep
           :initform `(,*slot-dummy* t)
           :reader store-slot-p)
   (relation :initarg :relation
             :initform `(,*slot-dummy* nil)
             :reader slot-relation)
   (db-type :initarg :db-type
            :initform `(,*slot-dummy* nil)
            :reader slot-db-type)
   (read-only :initarg :read-only
                :initform `(,*slot-dummy* nil)
                :reader slot-read-only)
   (unit :initarg :unit
         :initform `(,*slot-dummy* nil)
         :reader slot-unit)))

(defclass storable-direct-slot-definition
    (storable-slot-mixin standard-direct-slot-definition)
  ())

(defclass storable-effective-slot-definition
    (storable-slot-mixin standard-effective-slot-definition)
  ())

(defmethod direct-slot-definition-class ((class storable-class) &key)
  (find-class 'storable-direct-slot-definition))

(defmethod effective-slot-definition-class ((class storable-class) &key)
  (find-class 'storable-effective-slot-definition))

(defun compute-slot-option (effective-definition
                            slot direct-definitions)
  (let ((value
          (loop for dd in direct-definitions
                for value = (slot-value dd slot)
                unless (and (consp value)
                            (eq (car value) *slot-dummy*))
                return value
                finally
                (return (cadr (slot-value (car direct-definitions)
                                          slot))))))
    (setf (slot-value effective-definition slot)
          value)))

(defmethod compute-effective-slot-definition
    ((class storable-class) slot-name direct-definitions)
  (declare (ignore slot-name))
  (let ((effective-definition (call-next-method)))
    (loop for slot in '(storep relation db-type read-only unit)
          do
          (compute-slot-option effective-definition
                               slot
                               direct-definitions)) 
    effective-definition))

(defmethod compute-slots ((class storable-class))
  (let* ((slots (call-next-method))
         (other-slots (remove-if (lambda (x)
                                   (or (eq x 'id)
                                       (eq x 'relations)))
                                 slots
                                 :key #'slot-definition-name)))
    (list* (or (find 'id slots :key #'slot-definition-name)
               (error "No ~s slot in ~s" 'id class))
           (or (find 'relations slots :key #'slot-definition-name)
               (error "No ~s slot in ~s" 'relations class))
           (stable-sort (copy-list other-slots)
                        (lambda (x y)
                          (and y (not x)))
                        :key #'store-slot-p))))

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

(defconstant +id-location+ 0)
(defconstant +relations-location+ 1)

(declaim (inline fast-id))
(defun fast-id (object)
  (standard-instance-access object +id-location+))

(declaim (inline fast-relations (setf fast-relations)))
(defun fast-relations (object)
  (standard-instance-access object +relations-location+))

(defun (setf fast-relations) (value object)
  (setf (standard-instance-access object +relations-location+)
        value))

(defun initialize-class-slots (class slots)
  (let* ((slots-to-store (coerce (remove-if-not #'store-slot-p slots)
                                 'simple-vector)))
    (when (> (length slots-to-store) 32)
      (error "Can't have classes with more than 32 storable slots."))
    (setf (slots-to-store class)
          slots-to-store)
    (setf (number-of-bytes-for-slots class)
          (ceiling (length slots-to-store) 8))
    (setf (slot-locations-and-initforms class)
          (make-slots-cache slots-to-store))
    (setf (slot-locations-and-initforms-read class)
          (copy-seq (slot-locations-and-initforms class)))
    (setf (all-slot-locations-and-initforms class)
          (make-slots-cache slots))
    (setf (class-initforms class)
          (map 'vector #'slot-definition-initform slots))
    (setf (class-relations class)
          (slots-with-relations class))
    (compute-search-key class)
    (assert
     (= (slot-definition-location
         (find-slot-or-error 'id class))
        +id-location+))
    (assert
     (= (slot-definition-location
         (find-slot-or-error 'relations class))
        +relations-location+))))

(defmethod finalize-inheritance :after ((class storable-class))
  (initialize-class-slots class (class-slots class)))

(defun find-slot (slot-name class)
  (find slot-name (class-slots class)
        :key #'slot-definition-name))

(defun find-slot-or-error (slot-name class)
  (or (find slot-name (class-slots class)
            :key #'slot-definition-name)
      (error "Can't find ~s slot in ~s."
             slot-name class)))

(defun compute-search-key (class)
  (with-slots (search-key) class
    (let* ((key (or search-key
                    (loop for superclass in (class-precedence-list class)
                          thereis (and (typep superclass 'storable-class)
                                       (search-key superclass)))))
           (slot-name (typecase key
                        (cons (car key))
                        (symbol key))))
      (setf search-key slot-name)
      (when slot-name
        (unless (find-slot slot-name class)
          (setf search-key nil)
          (error "Search key ~a for an uknown slot in class ~a"
                 slot-name class))))))

(defmethod initialize-instance :after ((class storable-class) &key)
  (when (class-storage class)
    (pushnew class (storage-data (class-storage class)))))

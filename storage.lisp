;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:storage)

(defvar *last-id* -1)

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
                  :storep nil))
  (:metaclass storable-class))

(defgeneric relation (object type))

(defmethod relation (object type)
  (getf (relations object) type))

(defmethod initialize-instance :after ((object identifiable)
                                       &key id)
  (if (integerp id)
      (setf *last-id* (max *last-id* id))
      (setf (id object) (incf *last-id*))))

;;;

(defvar *indexes* (make-hash-table))

(defun index-object (object)
  (setf (gethash (id object) *indexes*) object))

(defun object-with-id (id)
  (gethash id *indexes*))

;;;

(defvar *storage* nil)

(defvar *read-class-cache* #())

(defun grow-read-cache (id)
  (let ((new-cache (make-array (+ id 10) :initial-element nil)))
    (replace new-cache *read-class-cache*)
    (setf *read-class-cache* new-cache)))

(defun cache-class (class id)
  (unless (array-in-bounds-p *read-class-cache* id)
    (grow-read-cache id))
  (setf (aref *read-class-cache* id) class))

(defun find-class-by-id (id)
  (aref *read-class-cache* id))

(defun objects-of-type (type)
  (objects-of-class (find-class type)))

(defun (setf objects-of-type) (value type)
  (setf (objects-of-class (find-class type)) value))

(defun store-object (object)
  (pushnew object (objects-of-class (class-of object))
           :test #'eq)
  t)

(defun delete (object)
  (setf (objects-of-class (class-of object))
        (cl:delete object (objects-of-class (class-of object))))
  (when (typep object 'identifiable)
    (setf (id object) -1))
  t)

(defun map-data (function)
  (dolist (class (storage-data *storage*))
    (funcall function
             class (objects-of-class class))))

(defun map-type (type function)
  (let ((type (if (eql type t)
                  'identifiable
                  type)))
   (dolist (class (storage-data *storage*))
     (when (subtypep class type)
       (map nil function
            (objects-of-class class))))))

(defmethod update-instance-for-different-class
    :after ((previous identifiable) (current identifiable) &key)
  (setf (objects-of-class (class-of previous))
        (cl:delete current (objects-of-class (class-of previous))))
  (store-object current))

;;;

(defun interlink-slots (object slot relation-name)
  (dolist (slot (if (listp slot)
                    slot
                    (list slot)))
    (when (typep slot 'identifiable)
      (pushnew object (getf (relations slot) relation-name)))))

(defgeneric interlink-objects (object))

(defmethod interlink-objects ((object identifiable))
  (let ((class (class-of object)))
    (loop for slot across (slots-to-store class)
          for relation-name = (slot-relation slot)
          when relation-name
          do (interlink-slots object
                              (slot-value-using-class class object slot)
                              relation-name))))

;;;

(defun clear-cashes ()
  (clrhash *indexes*)
  (setf *read-class-cache* #()))

;;; Data manipulations

(defgeneric add (class &rest args))

(defmethod add (class &rest args)
  (add (apply #'make-instance class args)))

(defmethod add ((object identifiable) &key)
  (store-object object)
  (storage:interlink-objects object)
  (index-object object)
  object)

(defun where (&rest clauses)
  (let ((slots (loop for slot in clauses by #'cddr
                     collect slot))
        (values (loop for value in (cdr clauses) by #'cddr collect value)))
    (compile
     nil
     `(lambda (object)
        (with-slots ,slots object
          (and
           ,@(mapcar (lambda (slot value)
                       (typecase value
                         (function
                          `(funcall ,value ,slot))
                         (string
                          (if (= (length value) 1)
                              `(find ,(char value 0) ,slot :test #'char-equal)
                              `(do-kmp ,value ,(reverse-case value)
                                       ,slot ,(build-table value))))
                         (t
                          `(equalp ,value ,slot))))
                     slots values)))))))

(defun type-and-test (type test)
  (lambda (object) (and (typep object type)
                        (funcall test object))))

(defun lookup (type &optional test)
  (let (results)
    (map-data (lambda (key objects)
                (when (subtypep key type)
                  (setf results
                        (append (if test
                                    (remove-if-not test objects)
                                    objects)
                                results)))))
    (if (= (length results) 1)
        (car results)
        results)))

(defun count (type &optional test)
  (let ((count 0))
    (map-data (lambda (key objects)
                (when (subtypep key type)
                  (incf count
                        (if (null test)
                            (length objects)
                            (count-if test objects))))))
    count))

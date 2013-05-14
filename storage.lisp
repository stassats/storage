;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:storage)

(defclass identifiable (standard-object)
  ((id :accessor id
       :initform nil
       :storep nil
       :read-only t)
   (relations :initform nil
              :accessor relations
              :storep nil
              :read-only t))
  (:metaclass storable-class))

(defgeneric relation (object type))

(defmethod relation (object type)
  (getf (relations object) type))

;;;

(defvar *storage* nil)

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

(defun map-data (function &key (type t))
  (dolist (class (storage-data *storage*))
    (when (subtypep class type)
      (funcall function
               class (objects-of-class class)))))

(declaim (inline map-all-data))
(defun map-all-data (function)
  (dolist (class (storage-data *storage*))
    (let ((objects (objects-of-class class)))
      (when objects
        (funcall function class objects)))))

(defun map-type-class (superclass function)
  (dolist (class (storage-data *storage*))
    (when (subtypep class superclass)
      (map nil function
           (objects-of-class class)))))

(defun map-type (type function)
  (let ((class (and (symbolp type)
                    (find-class type nil))))
    (if class
        (map-type-class class function)
        (dolist (class (storage-data *storage*))
          (dolist (object (objects-of-class class))
            (when (typep object type)
              (funcall function object)))))))

(defmethod update-instance-for-different-class
    :after ((previous identifiable) (current identifiable) &key)
  (setf (objects-of-class (class-of previous))
        (cl:delete current (objects-of-class (class-of previous))))
  (store-object current))

;;;

(defmacro do-maybe-list ((var maybe-list) &body body)
  (let ((function-name (gensym))
        (list-name (gensym)))
    `(let ((,list-name ,maybe-list))
       (flet ((,function-name (,var)
                ,@body))
         (if (listp ,list-name)
             (dolist (,var ,list-name)
               (,function-name ,var))
             (,function-name ,list-name))))))

(defun link-slot (relation object target-object)
  (if (and (consp relation)
           (eql (car relation) :slot))
      (pushnew object (slot-value target-object (cadr relation))
               :test #'eq)
      (pushnew object (getf (relations target-object) relation)
               :test #'eq)))

(defun interlink-slots (object slot-value relation)
  (do-maybe-list (target slot-value)
    (when (typep target 'identifiable)
      (link-slot relation object target))))

(defun interlink-objects (object)
  (loop for (loc . relation) in (class-relations (class-of object))
        do
        (interlink-slots object
                         (standard-instance-access object loc)
                         relation)))

;;;

(defun interlink-all-objects-first-time ()
  (map-all-data
   (lambda (class objects)
     (let ((relations (class-relations class)))
       (when relations
         (loop for object in objects
               do (interlink-objects-first-time object relations)))))))

(declaim (inline prepend))
(defun prepend (item list)
  (psetf (car list) item
         (cdr list) (cons (car list) (cdr list))))

(declaim (inline fgetf))
(defun fgetf (place indicator)
  (loop for (key value) on place by #'cddr
        when (eq key indicator) return value))

(declaim (inline set-relations))
(defun set-relations (relation object target-object)
  (let* ((relations (fast-relations target-object))
         (list (fgetf relations relation)))
    (cond (list
           (prepend object list))
          (relations
           (prepend (list object) relations)
           (prepend relation relations))
          (t
           (setf (fast-relations target-object)
                 (list* relation (list object) relations))))))

(defun link-slot-first-time (relation object target-object)
  (if (and (consp relation)
           (eql (car relation) :slot))
      (push object (slot-value target-object (cadr relation)))
      (set-relations relation object target-object)))

(defun interlink-slots-first-time (object slot-value relation)
  (do-maybe-list (target slot-value)
    (link-slot-first-time relation object target)))

(defun interlink-objects-first-time (object relations)
  (loop for (loc . relation) in relations
        do
        (interlink-slots-first-time object
                                    (standard-instance-access object loc)
                                    relation)))

;;; Data manipulations

(defgeneric add (class &rest args))

(defmethod add (class &rest args)
  (add (apply #'make-instance class args)))

(defmethod add ((object identifiable) &key)
  (store-object object)
  (interlink-objects object)
  object)


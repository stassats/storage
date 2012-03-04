;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:storage)

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

(defun interlink-all-objects ()
  (map-data
   (lambda (class objects)
     (declare (ignore class))
     (loop for object in objects
           do (interlink-objects object)))))

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
  (map-data
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
  (let* ((relations (relations target-object))
         (list (fgetf relations relation)))
    (cond (list
           (prepend object list))
          (relations
           (prepend (list object) relations)
           (prepend relation relations))
          (t
           (setf (relations target-object)
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
                          (let ((reversed (reverse-case value)))
                            `(and (stringp ,slot)
                                  (do-kmp ,value ,reversed
                                          ,slot ,(build-table value reversed)))))
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

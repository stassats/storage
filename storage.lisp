;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:storage)

(defvar *storage* nil)

(defvar *read-class-cache* #())
(declaim (simple-vector *read-class-cache*))

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

(defun map-data (function &key (type t))
  (dolist (class (storage-data *storage*))
    (when (subtypep class type)
      (funcall function
               class (objects-of-class class)))))

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

(defgeneric interlink-objects (object))

(defmethod interlink-objects ((object identifiable))
  (loop for (loc . relation) in (class-relations (class-of object))
        do
        (interlink-slots object
                         (standard-instance-access object loc)
                         relation)))

;;;

(defun clear-cashes ()
  (setf *read-class-cache* #()))

;;; Data manipulations

(defgeneric add (class &rest args))

(defmethod add (class &rest args)
  (add (apply #'make-instance class args)))

(defmethod add ((object identifiable) &key)
  (store-object object)
  (storage:interlink-objects object)
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

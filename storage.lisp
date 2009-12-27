;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:movies)

(defvar *dump-classes*
  '((person . *persons*)
    (movie . *movies*)
    (theatre . *theatres*)
    (view . *views*)))

(defun data (type)
  (when type
    (symbol-value (cdr (assoc type *dump-classes* :test #'subtypep)))))

(defun (setf data) (list type)
  (when type
    (setf (symbol-value (cdr (assoc type *dump-classes* :test #'subtypep)))
          list)))

(defvar *persons-index* (make-hash-table))
(defvar *movies-index* (make-hash-table))
(defvar *views-index* (make-hash-table))

(defvar *indexes*
  '((person . *persons-index*)
    (movie . *movies-index*)
    (view . *views-index*)))

(defun index (type id)
  (when type
    (let ((index (symbol-value (cdr (assoc type *indexes* :test #'subtypep)))))
      (if (hash-table-p index)
          (gethash id index)
          (find id (data type) :key #'id)))))

(defun (setf index) (object type)
  (when type
    (let ((index (symbol-value (cdr (assoc type *indexes* :test #'subtypep)))))
      (when (hash-table-p index)
        (setf (gethash (id object) index) object)))))

(defun slots (class)
  #+ccl
  (mapcar #'ccl:slot-definition-name
          (ccl:class-slots class))
  #+sbcl
  (mapcar #'sb-mop:slot-definition-name
          (sb-mop:class-slots class)))

(defun slots-with-initform (class)
  #+ccl
  (mapcar (lambda (slot)
            (cons (ccl:slot-definition-name slot)
                  (ccl:slot-definition-initform slot)))
          (ccl:class-slots class))
  #+sbcl
  (mapcar (lambda (slot)
            (cons (sb-mop:slot-definition-name slot)
                  (sb-mop:slot-definition-initform slot)))
          (sb-mop:class-slots class)))

(defun replace-ids (value)
  (cond ((consp value)
         (mapcar 'replace-ids value))
        ((data (class-name (class-of value)))
         (cons (class-name (class-of value))
               (id value)))
        (t value)))

(defun dump-object (object stream)
  (let ((class (class-of object)))
    (fresh-line stream)
    (format stream "(~a" (class-name class))
    (loop with *print-pretty*
          for (slot . initform) in (slots-with-initform class)
          for value = (replace-ids (slot-value object slot))
          unless (equalp value initform)
          do (format stream " ~w ~w"
                     (find-symbol (symbol-name slot) 'keyword)
                     value))
    (write-char #\) stream)))

(defun dump-class (class stream)
  (dolist (object (data class))
    (dump-object object stream)))

(defun dump-data (stream)
  (loop for (class . nil) in *dump-classes*
        do (dump-class class stream)))



(defun find-object (class id)
  (index class id))

(defun %deidentify (value)
  (typecase value
    ((cons symbol integer)
     (find-object (car value) (cdr value)))
    (cons
     (mapcar '%deidentify value))
    (t value)))

(defun deidentify (object)
  (dolist (slot (slots (class-of object)))
    (setf (slot-value object slot)
          (%deidentify (slot-value object slot))))
  object)

(defun deidentify-all (list)
  (loop for (nil . objects) in list
        do (mapcar 'deidentify (symbol-value objects))))

(defun parse-class (string)
  (let* ((list (read-from-string string))
         (type (car list)))
    (push (apply #'make-instance list)
          (data type))))

(defun read-file (file)
  (with-standard-io-syntax
    (let ((*package* (find-package 'movies)))
      (with-open-file (stream file)
        (loop for string = (read-line stream nil)
              while string
              do (parse-class string))))))

(defun load-data (&optional (file *data-file*))
  (dolist (cons *dump-classes*) (setf (data (car cons)) nil))
  (read-file file)
  (deidentify-all *dump-classes*))

(defun save-data (&optional (file *data-file*))
  (with-standard-io-syntax
    (let ((*package* (find-package 'movies)))
      (with-open-file (stream file :direction :output :if-exists :supersede)
        (dump-data stream)))
    t))

(eval-when (:execute :load-toplevel)
  (load-data))

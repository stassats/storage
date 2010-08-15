;;; -*- Mode: Lisp -*-

(in-package #:storage)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *codes* #(ascii-string
                    identifiable cons
                    string symbol
                    storable-class
                    standard-object
                    fixnum bignum ratio)))

(declaim (type simple-vector *codes*))

(defvar *statistics* ())
(defun collect-stats (code)
  (let* ((type (aref *codes* code))
         (cons (assoc type *statistics*)))
    (if cons
        (incf (cdr cons))
        (push (cons type 1) *statistics*))
    type))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun type-code (type)
    (position type *codes*)))

(defvar *code-functions* (make-array (length *codes*)))
(declaim (type (simple-array function (*))))

(defmacro defreader (type (stream) &body body)
  (let ((name (intern (format nil "~a-~a" type '#:reader))))
    `(progn
       (defun ,name (,stream)
         ,@body)
       (setf (aref *code-functions* ,(type-code type))
             #',name))))

(defun call-reader (code stream)
  (collect-stats code)
  (funcall (aref *code-functions* code) stream))

;;;

(defconstant +sequence-length+ 2)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +fixnum-length+ 4))
(defconstant +char-length+ 2)
(defconstant +id-length+ 3)

(defconstant +end-of-slots+ 255)

(defconstant +ascii-char-limit+ (code-char 128))

(deftype ascii-string ()
  '(or #+sb-unicode simple-base-string  ; on #-sb-unicode the limit is 255
    (satisfies ascii-string-p)))

(defun ascii-string-p (string)
  (and (stringp string)
       (every (lambda (x)
                (char< x +ascii-char-limit+))
              string)))
;;;

(defun slot-effective-definition (class slot-name)
  (find slot-name (class-slots class) :key #'slot-definition-name))

(defgeneric write-object (object stream))
(defgeneric object-size (object))

(defun measure-size ()
  (let ((result 0))
    (map-data (lambda (class objects)
                (incf result (object-size class))
                (dolist (object objects)
                  (incf result
                        (standard-object-size object)))))
    result))

(defun dump-data (stream)
  (map-data (lambda (class objects)
              (declare (ignore objects))
              (write-object class stream)))
  (map-data (lambda (class objects)
              (declare (ignore class))
              (dolist (object objects)
                (write-standard-object object stream)))))

(defun read-next-object (stream)
  (call-reader (read-n-bytes 1 stream) stream))

;;; Symbol

(defmethod object-size ((object symbol))
  (+ 3 ;; type + package name length + symbol length
     (length (package-name (symbol-package object)))
     (length (symbol-name object))))

(defmethod write-object ((object symbol) stream)
  (write-n-bytes #.(type-code 'symbol) 1 stream)
  (let ((name (symbol-name object))
        (package (package-name (symbol-package object))))
    (write-n-bytes (length name) 1 stream)
    (write-ascii-string name stream)
    (write-n-bytes (length package) 1 stream)
    (write-ascii-string package stream)))

(defreader symbol (stream)
  (intern (read-ascii-string (read-n-bytes 1 stream) stream)
          (read-ascii-string (read-n-bytes 1 stream) stream)))

;;; Integer

(defmethod object-size ((object integer))
  (+ 1 ;; tag
     1 ;; sign
     (typecase object
       (#.`(signed-byte ,(* +fixnum-length+ 8))
          +fixnum-length+)
       (t (+ 1 ;; size
             (* (ceiling (integer-length (abs object))
                         (* +fixnum-length+ 8)) +fixnum-length+))))))

(defun write-fixnum (n stream)
  (write-n-bytes #.(type-code 'fixnum) 1 stream)
  (write-n-bytes (if (minusp n) 1 0) 1 stream)
  (write-n-bytes (abs n) +fixnum-length+ stream))

(defun write-bignum (n stream)
  (write-n-bytes #.(type-code 'bignum) 1 stream)
  (write-n-bytes (if (minusp n) 1 0) 1 stream)
  (let* ((n (abs n))
        (size (ceiling (integer-length n)
                       (* +fixnum-length+ 8))))
    (write-n-bytes size 1 stream)
    (loop for position to size
          do 
          (write-n-bytes (ldb (byte (* +fixnum-length+ 8)
                                    (* position (* +fixnum-length+ 8)))
                              n)
                         +fixnum-length+ stream))))

(defmethod write-object ((object integer) stream)
  (typecase object
    (#.`(signed-byte ,(* +fixnum-length+ 8))
       (write-fixnum object stream))
    (t (write-bignum object stream))))

(defreader bignum (stream)
  (* (if (plusp (read-n-bytes 1 stream))
         -1
         1)
     (loop with integer = 0
           for position to (read-n-bytes 1 stream)
           do 
           (setf (ldb (byte (* +fixnum-length+ 8)
                            (* position (* +fixnum-length+ 8)))
                      integer)
                 (read-n-bytes +fixnum-length+ stream))
           finally (return integer))))

(defreader fixnum (stream)
  (* (if (plusp (read-n-bytes 1 stream))
         -1
         1)
   (read-n-bytes +fixnum-length+ stream)))

;;; Ratio

(defmethod object-size ((object ratio))
  (+ 1
     (object-size (numerator object))
     (object-size (denominator object))))

(defmethod write-object ((object ratio) stream)
  (write-n-bytes #.(type-code 'ratio) 1 stream)
  (write-object (numerator object) stream)
  (write-object (denominator object) stream))

(defreader ratio (stream)
  (/ (read-next-object stream)
     (read-next-object stream)))

;;; Strings

(defmethod object-size ((string string))
  (+ 1
     +sequence-length+
     (etypecase string
       (ascii-string (length string))
       (string (* (length string)
                  +char-length+)))))

(defun write-ascii-string (string stream)
  (loop for char across string
        do (write-n-bytes (char-code char) 1 stream)))

(defun write-multibyte-string (string stream)
  (loop for char across string
        do (write-n-bytes (char-code char) +char-length+ stream)))

(defmethod write-object ((string string) stream)
  (etypecase string
    #+sb-unicode
    (simple-base-string
     (write-n-bytes #.(type-code 'ascii-string) 1 stream)
     (write-n-bytes (length string) +sequence-length+ stream)
     (write-ascii-string-optimized (length string) string stream))
    (ascii-string
     (write-n-bytes #.(type-code 'ascii-string) 1 stream)
     (write-n-bytes (length string) +sequence-length+ stream)
     (write-ascii-string string stream))
    (string
     (write-n-bytes #.(type-code 'string) 1 stream)
     (write-n-bytes (length string) +sequence-length+ stream)
     (write-multibyte-string string stream))))

(defun read-ascii-string (length stream)
  (let ((string (make-string length :element-type 'base-char)))
    #-sbcl
    (loop for i below length
          do (setf (char string i)
                   (code-char (read-n-bytes 1 stream))))
    #+(and sbcl (or x86 x86-64))
    (read-ascii-string-optimized length string stream)
    string))

(defreader ascii-string (stream)
  (read-ascii-string (read-n-bytes +sequence-length+ stream) stream))

(defreader string (stream)
  (let* ((length (read-n-bytes +sequence-length+ stream))
         (string (make-string length :element-type 'character)))
    (loop for i below length
          do (setf (char string i)
                   (code-char (read-n-bytes +char-length+ stream))))
    string))

;;; cons

(defmethod object-size ((list cons))
  (let ((count (+ 1 +sequence-length+)))
    (mapc (lambda (x)
            (incf count (object-size x)))
          list)
    count))

(defmethod write-object ((list cons) stream)
  (write-n-bytes #.(type-code 'cons) 1 stream)
  (write-n-bytes (length list) +sequence-length+ stream)
  (dolist (item list)
    (write-object item stream)))

(defreader cons (stream)
  (loop repeat (read-n-bytes +sequence-length+ stream)
        collect (read-next-object stream)))

;;; storable-class

(defmethod object-size ((class storable-class))
  (+ 1 ;; type
     (object-size (class-name class))
     1                 ;; class-id
     +sequence-length+ ;; list length
     (reduce #'+ (slots-to-store class)
             :key (lambda (x)
                    (object-size (slot-definition-name x))))))

(defmethod write-object ((class storable-class) stream)
  (write-n-bytes #.(type-code 'storable-class) 1 stream)
  (write-object (class-name class) stream)
  (write-n-bytes (class-id class) 1 stream)
  (let ((slots (slots-to-store class)))
    (write-n-bytes (length slots) +sequence-length+ stream)
    (loop for slot across slots
          do (write-object (slot-definition-name slot)
                           stream))))

(defreader storable-class (stream)
  (let ((class (find-class (read-next-object stream)))
        (id (read-n-bytes 1 stream)))
    (cache-class class id)
    (unless (class-finalized-p class)
      (finalize-inheritance class))
    (pushnew class (storage-data (class-storage class)))
    (setf (objects-of-class class) nil)
    (let* ((length (read-n-bytes +sequence-length+ stream))
           (vector (make-array length)))
      (loop for i below length
            do (setf (aref vector i)
                     (slot-effective-definition class
                                                (read-next-object stream))))
      (setf (slots-to-store class) vector))
    class))

;;; identifiable

(defmethod object-size ((object identifiable))
  (+ 2 ;; type + class id
     +id-length+))

(defmethod write-object ((object identifiable) stream)
  (write-n-bytes #.(type-code 'identifiable) 1 stream)
  (write-n-bytes (id object) +id-length+ stream)
  (write-n-bytes (class-id (class-of object)) 1 stream))

(defreader identifiable (stream)
  (let ((id (read-n-bytes +id-length+ stream))
        (class-id (read-n-bytes 1 stream)))
    (or (gethash id *indexes*)
        (setf (gethash id *indexes*)
              (make-instance (find-class-by-id class-id)
                             :id id)))))

;;; standard-object

(defun standard-object-size (object)
  (let* ((class (class-of object)))
    (+ 1                ;; data type
       1                ;; class id
       +id-length+ ;; id
       (loop for slot-def across (slots-to-store class)
             sum (let ((value (slot-value-using-class class object slot-def)))
                   (if (eql value (slot-definition-initform slot-def))
                       0
                       (+ 1 ;; slot id
                          (object-size value)))))
       1))) ;; end-of-slots

;;; Can't use write-object method, because it would conflict with
;;; writing a pointer to a standard object
(defun write-standard-object (object stream)
  (write-n-bytes #.(type-code 'standard-object) 1 stream)
  (let ((class (class-of object)))
    (write-n-bytes (class-id class) 1 stream)
    (write-n-bytes (id object) +id-length+ stream)
    (loop for slot-def across (slots-to-store class)
          for i from 0
          for value = (slot-value-using-class class object slot-def)
          unless (eql value (slot-definition-initform slot-def))
          do
          (write-n-bytes i 1 stream)
          (write-object value stream))
    (write-n-bytes +end-of-slots+ 1 stream)))

(declaim (inline get-instance))
(defun get-instance (id class)
  (or (gethash id *indexes*)
      (setf (gethash id *indexes*)
            (make-instance class :id id))))

(defreader standard-object (stream)
  (let* ((class (find-class-by-id (read-n-bytes 1 stream)))
         (id (read-n-bytes +id-length+ stream))
         (instance (get-instance id class))
         (slots (slots-to-store class)))
    (loop for slot-id = (read-n-bytes 1 stream)
          until (= slot-id +end-of-slots+)
          do (setf (slot-value-using-class class instance
                                           (aref slots slot-id))
                   (read-next-object stream)))
    (setf *last-id* (max *last-id* (id instance)))
    (push instance (objects-of-class class))
    instance))

;;;

(defun read-file (file)
  (with-io-file (stream file)
    (loop until (stream-end-of-file-p stream)
          do (read-next-object stream))))

(defun load-data (storage &optional file)
  (let ((*storage* storage))
    (clear-cashes)
    (read-file (or file (storage-file *storage*)))
    (map-data (lambda (type objects)
                (declare (ignore type))
                (mapc #'interlink-objects objects)))))

(defun save-data (storage &optional file)
  (let ((*storage* storage))
    (when (storage-data *storage*)
      (with-io-file (stream (or file (storage-file *storage*))
                            :direction :output
                            :size (measure-size))
        (dump-data stream)))))

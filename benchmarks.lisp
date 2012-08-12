(in-package #:storage)

;;; conses

(defvar *test-file* #p"/tmp/test.db")

(defun save-test (type amount &key object-size)
  (declare (fixnum amount))
  (with-writing-packages
   (let ((object (create-test-object type :object-size object-size)))
     (with-io-file (stream *test-file* :direction :output)
       (write-object amount stream)
       (loop repeat amount
             do (write-object object stream))))))

(defun gc ()
  #+sbcl (sb-ext:gc :full t)
  #+ccl (progn (ccl:set-lisp-heap-gc-threshold (* 1024 1024 64))
               (ccl:gc)))

(defmacro time-with-gc (&body body)
  `(progn (gc)
          (time (progn ,@body))))

(defun load-test ()
  (with-reading-packages
   (with-io-file (stream *test-file*)
     (time-with-gc
       (loop repeat (read-next-object stream)
             do (read-next-object stream))))))

(defun identity-test (x &optional (mode :both))
  (with-writing-packages
    (when (member mode '(:both :write))
      (with-io-file (stream *test-file* :direction :output)
        (write-object x stream))))
  (with-reading-packages
    (when (member mode '(:both :read))
      (with-io-file (stream *test-file*)
        (read-next-object stream)))))

(defgeneric create-test-object (type &key &allow-other-keys))

(defmethod create-test-object ((type (eql 'cons)) &key object-size)
  (make-list (or object-size 10000) :initial-element nil))

(defmethod create-test-object ((type (eql 'fixnum)) &key)
  -1)

(defmethod create-test-object ((type (eql 'string)) &key object-size)
  (make-string (or object-size 10000)))

(defmethod create-test-object ((type (eql 'simple-base-string)) &key)
  #.(coerce "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
            'simple-base-string))

(defmethod create-test-object ((type (eql 'ascii-string)) &key)
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

(defmethod create-test-object ((type (eql 'multibyte-string)) &key)
  "АБВГДΑΒΓΔΕΖΗΘΙΚΛαβаб广州话日本語ภาษาไทย한국어")

(defmethod create-test-object ((type (eql 'hash-table)) &key)
  (load-time-value (make-hash-table :test 'equal :size 500)))

(defmethod create-test-object ((type (eql 'complex-vector)) &key)
  (load-time-value (make-array 10 :adjustable nil :fill-pointer 5)))

(defmethod create-test-object ((type (eql 'complex-array)) &key)
  (load-time-value (make-array '(10 10 10) :adjustable t)))

(defmethod create-test-object ((type (eql 'fixnum-ratio)) &key)
  1/3)

(defmethod create-test-object ((type (eql 'bignum-ratio)) &key)
  2333333331232/2333333331231)

(defun class-preallocation-test (storage)
  (loop for class in (storage-data storage)
        for length = (length (objects-of-class class))
        when (plusp length)
        collect (cons class length) into info
        and
        sum length into array-length
        finally
        (preallocate-objects (setf *indexes* (make-array array-length))
                             info)))

(in-package #:storage)

;;; conses

(defvar *test-file* #p"/tmp/test.db")

(defun save-test (type amount &key object-size)
  (let ((object (create-test-object type :object-size object-size)))
    (with-io-file (stream *test-file* :direction :output
                                      :size (+ (object-size amount)
                                               (* (object-size object) amount)))
      (write-object amount stream)
      (loop repeat amount
            do (write-object object stream)))))

(defun gc ()
  #+sbcl (sb-ext:gc :full t)
  #+ccl (progn (ccl:set-lisp-heap-gc-threshold (* 1024 1024 64))
               (ccl:gc)))

(defmacro time-with-gc (&body body)
  `(progn (gc)
          (time (progn ,@body))))

(defun load-test ()
  (with-io-file (stream *test-file*)
    (time-with-gc
      (loop repeat (read-next-object stream)
            do (read-next-object stream)))))

(defgeneric create-test-object (type &key &allow-other-keys))

(defmethod create-test-object ((type (eql 'cons)) &key object-size)
  (make-list (or object-size 10000) :initial-element nil))

(defun class-preallocation-test (storage)
  (loop for class in (storage-data storage)
        for i = 0 then (+ i length)
        for length = (length (objects-of-class class))
        when (plusp length)
        collect (list class length i) into info
        and
        sum length into array-length
        finally
        (preallocate-objects (setf *indexes* (make-array array-length))
                             info)))

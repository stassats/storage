(in-package #:storage)

(declaim (inline fast-allocate-instance))
(defun fast-allocate-instance (wrapper initforms)
  (declare (simple-vector initforms))
  (let ((instance (sb-pcl::%make-standard-instance
                   (copy-seq initforms) (sb-pcl::get-instance-hash-code))))
    (setf (sb-pcl::std-instance-wrapper instance)
          wrapper)
    instance))

(defun preallocate-objects (array info)
  (declare (simple-vector array)
           (optimize speed))
  (loop with index = 0
        for (class . length) in info
        for initforms = (class-initforms class)
        for wrapper = (sb-pcl::class-wrapper class)
        do
        (setf (objects-of-class class)
              (loop repeat (the fixnum length)
                    for instance = (fast-allocate-instance wrapper initforms)
                    collect instance
                    do
                    (setf (aref array index) instance)
                    (incf index)))))

(defconstant +id-location+ 0)

(declaim (inline fast-id))
(defun fast-id (object)
  (standard-instance-access object +id-location+))

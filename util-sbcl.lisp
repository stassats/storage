(in-package #:storage)

(declaim (inline fast-allocate-instance))
(defun fast-allocate-instance (wrapper initforms)
  (declare (simple-vector initforms))
  (let ((instance (sb-pcl::%make-standard-instance
                   (copy-seq initforms)
                   #-(and (or linux bsd) x86-64) 0)))
    (setf (sb-kernel:%instance-layout instance) wrapper)
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

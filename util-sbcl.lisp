(in-package #:storage)

(declaim (inline fast-allocate-instance))
(defun fast-allocate-instance (wrapper initforms)
  (declare (simple-vector initforms))
  (let ((instance (sb-kernel:%make-instance (1+ sb-vm:instance-data-start))))
    (setf (sb-pcl::std-instance-slots instance) (copy-seq initforms))
    (sb-kernel:%set-instance-layout instance wrapper)
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

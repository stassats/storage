(in-package #:storage)

(defun initialize-slots (instance slot-cache)
  (loop for (location . value) across slot-cache
        do (setf (standard-instance-access instance location)
                 value))
  instance)

(defun preallocate-objects (array info)
  (declare (simple-array array))
  (loop with index = 0
        for (class . length) in info
        for slot-cache = (all-slot-locations-and-initforms class)
        do
        (setf (objects-of-class class)
              (loop repeat length
                    for instance = (allocate-instance class)
                    collect instance
                    do (initialize-slots instance slot-cache)
                       (setf (aref array index) instance)
                       (incf index)))))

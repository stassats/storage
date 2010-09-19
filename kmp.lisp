(in-package :storage)

(defun build-table (vector)
  (let* ((length (length vector))
         (table (make-array length :element-type 'fixnum)))
    (setf (aref table 0) -1)
    (setf (aref table 1) 0)
    (loop with pos = 2 and candidate = 0
          while (< pos length)
          do (cond ((char-equal (aref vector (1- pos))
                                (aref vector candidate))
                    (setf (aref table pos) (1+ candidate))
                    (incf pos)
                    (incf candidate))
                   ((plusp candidate)
                    (setf candidate (aref table candidate)))
                   (t
                    (setf (aref table pos) 0)
                    (incf pos))))
    table))

(declaim (inline do-kmp))
(defun do-kmp (sub-sequence sequence table)
  (declare (type simple-string sequence sub-sequence)
           (type (simple-array fixnum (*)) table)
           (optimize speed))
  (unless (> (length sub-sequence) (length sequence))
    (loop with m = 0 and i = 0
          while (< (#+sbcl sb-ext:truly-the #-sbcl the fixnum
                           (+ m i)) (length sequence))
          do (cond ((not (char-equal (aref sub-sequence i)
                                     (aref sequence (+ m i))))
                    (let ((backtrack (aref table i)))
                      (setf m (#+sbcl sb-ext:truly-the #-sbcl the fixnum
                                                (+ m (- i backtrack)))
                            i (max 0 backtrack))))
                   ((= (incf i) (length sub-sequence))
                    (return m))))))

(defun kmp (sub-sequence sequence &optional table)
  (declare (type vector sequence sub-sequence))
  (let ((sub-length (length sub-sequence))
        (length (length sequence)))
    (cond ((= sub-length 1)
           (position (elt sub-sequence 0) sequence))
          ((= sub-length 0)
           0)
          ((> sub-length length)
           nil)
          (t
           (do-kmp sub-sequence sequence
                   (or table (build-table sub-sequence)))))))

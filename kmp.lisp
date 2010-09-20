(in-package :storage)

(defun build-table (vector &optional (test #'char-equal))
  (let* ((length (length vector))
         (table (make-array length :element-type 'fixnum)))
    (setf (aref table 0) -1)
    (setf (aref table 1) 0)
    (loop with pos = 2 and candidate = 0
          while (< pos length)
          do (cond ((funcall test
                             (aref vector (1- pos))
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

(defun reverse-case (string)
  (map 'string (lambda (x)
                 (cond ((upper-case-p x)
                        (char-downcase x))
                       ((lower-case-p x)
                        (char-upcase x))
                       (t x)))
       string))

(declaim (inline do-kmp))
(defun do-kmp (pattern reverse-case string table)
  (declare (type simple-string pattern string)
           (type (or null simple-string) reverse-case)
           (type (simple-array fixnum (*)) table)
           (optimize speed))
  (let ((pattern-length (length pattern))
        (length (length string)))
   (unless (> pattern-length length)
     (loop with m = 0 and i = 0
           for m+i fixnum = (#+sbcl sb-ext:truly-the #-sbcl the fixnum
                                    (+ m i))
           while (< m+i length)
           for char = (schar string m+i)
           do (cond ((not (or (eql (schar pattern i) char)
                              (and reverse-case
                                   (eql (schar reverse-case i) char))))
                     (let ((backtrack (aref table i)))
                       (setf m (#+sbcl sb-ext:truly-the #-sbcl the fixnum
                                       (- m+i backtrack))
                             i (max 0 backtrack))))
                    ((= (incf i) pattern-length)
                     (return m)))))))

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

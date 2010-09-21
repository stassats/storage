(in-package :storage)

(declaim (inline build-table))
(defun build-table (string &optional (test #'char-equal))
  (declare (type simple-string string)
           (optimize speed
                     #+sbcl (sb-c::insert-array-bounds-checks 0)))
  (let* ((length (length string))
         (table (make-array length :element-type 'fixnum)))
    (setf (aref table 0) -1
          (aref table 1) 0)
    (loop with pos fixnum = 2 and candidate fixnum = 0
          while (< pos length)
          do (cond ((funcall test
                             (schar string (1- pos))
                             (schar string candidate))
                    (setf (aref table pos) (incf candidate))
                    (incf pos))
                   ((plusp candidate)
                    (setf candidate (aref table candidate)))
                   (t
                    (setf (aref table pos) 0)
                    (incf pos))))
    table))

(defun reverse-case (string)
  (map 'string
       (lambda (x)
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
            while (and (< m+i length)
                       (< i pattern-length)) ; hint for SBCL
            do (let ((char (schar string m+i)))
                 (cond ((not (or (char= (schar pattern i) char)
                                 (and reverse-case
                                      (char= (schar reverse-case i) char))))
                        (let ((backtrack (aref table i)))
                          (setf m (#+sbcl sb-ext:truly-the #-sbcl the fixnum
                                          (- m+i backtrack))
                                i (max 0 backtrack))))
                       ((= (incf i) pattern-length)
                        (return m))))))))

(defun kmp (pattern sequence &key (case-insensitive t))
  (declare (type simple-string sequence pattern))
  (let ((sub-length (length pattern))
        (length (length sequence)))
    (cond ((= sub-length 1)
           (position (elt pattern 0) sequence))
          ((= sub-length 0)
           0)
          ((> sub-length length)
           nil)
          (t
           (do-kmp pattern (when case-insensitive
                             (reverse-case pattern))
                   sequence
                   (build-table pattern))))))

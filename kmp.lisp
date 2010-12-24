(in-package :storage)

(declaim (inline build-table))
(defun build-table (string reverse-case)
  (declare (type simple-string string)
           (optimize speed
                     #+sbcl (sb-c::insert-array-bounds-checks 0)))
  (let* ((length (length string))
         (table (make-array length :element-type 'fixnum
                            :initial-element 0)))
    (setf (aref table 0) -1)
    (loop with pos fixnum = 2 and candidate fixnum = 0
          while (< pos length)
          do (let ((char (schar string candidate)))
               (cond ((or (char= (schar string (1- pos)) char)
                          (and reverse-case
                               (char= (schar reverse-case (1- pos)) char)))
                      (setf (aref table pos) (incf candidate))
                      (incf pos))
                     ((plusp candidate)
                      (setf candidate (aref table candidate)))
                     (t
                      (setf (aref table pos) 0)
                      (incf pos)))))
    table))

(declaim (inline reverse-case))
(defun reverse-case (string)
  (declare (simple-string string)
           (optimize speed)
           #+sbcl
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((reversed (make-string (length string))))
    (loop for char across string
          for i from 0
          do
          (setf (char reversed i)
                (if (upper-case-p char)
                    (char-downcase char)
                    (char-upcase char))))
    reversed))

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

(defun kmp (pattern string &key (case-insensitive t))
  (declare (type simple-string string pattern))
  (let ((sub-length (length pattern))
        (length (length string)))
    (cond ((= sub-length 0)
           0)
          ((> sub-length length)
           nil)
          (t
           (let ((reversed (when case-insensitive
                             (reverse-case pattern))))
             (do-kmp pattern reversed
                     string
                     (build-table pattern reversed)))))))

(defun make-kmp-searcher (pattern &key case-sensitive)
  (let ((reversed (if case-sensitive
                      nil
                      (reverse-case pattern))))
    (compile nil
             `(lambda (text) 
                (do-kmp ,pattern ,reversed
                        text ,(build-table pattern reversed))))))

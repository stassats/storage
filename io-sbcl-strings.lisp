(in-package #:storage)

(declaim (inline copy-mem))
(defun copy-mem (from to length)
  (declare (buffer-length length)
           (optimize speed))
  (loop for i fixnum by sb-vm:n-word-bytes below length
        do (setf (sb-sys:sap-ref-word to i)
                 (sb-sys:sap-ref-word from i))))

(declaim (inline copy-mem-non-base-string))
(defun copy-mem-non-base-string (from to length)
  (declare (buffer-length length)
           (optimize (safety 0)))
  (loop for string-index fixnum by 4
        for i fixnum below length
        do (setf (sb-sys:sap-ref-8 to i)
                 (sb-sys:sap-ref-8 from string-index))))

(declaim (inline copy-mem-multibyte-string))
(defun copy-mem-multibyte-string (from to length)
  (declare (buffer-length length)
           (optimize (safety 0)))
  (loop for buffer-index fixnum by 3 below length
        for string-index fixnum by 4 
        do (setf (sb-sys:sap-ref-32 to buffer-index)
                 (sb-sys:sap-ref-32 from string-index))))

(declaim (inline write-optimized-string-generic))
(defun write-optimized-string-generic (string copier stream
                                       &key (length-multiplier 1)
                                            (left-length-multiplier 1))
  (declare (optimize speed)
           (simple-string string)
           (sb-ext:muffle-conditions sb-ext:compiler-note)
           (type (integer 1 4) length-multiplier left-length-multiplier))
  (sb-sys:with-pinned-objects (string)
    (let* ((length (* (length string) length-multiplier))
           (position (output-stream-buffer-position stream))
           (string-sap (sb-sys:vector-sap string))
           (new-position (sb-ext:truly-the word (+ position length))))
      (declare (type word position new-position))
      (cond ((<= new-position (output-stream-buffer-end stream))
             (funcall copier string-sap (sb-sys:int-sap position) length)
             (setf (output-stream-buffer-position stream)
                   new-position))
            ((<= length +buffer-size+)
             (let ((left (sb-ext:truly-the word (- (output-stream-buffer-end stream) position))))
               (declare (buffer-length left))
               (multiple-value-bind (quot rem) (floor left length-multiplier)
                 (declare (word quot))
                 (let* ((start (output-stream-buffer-start stream))
                        (left (- left rem))
                        (left-length (sb-ext:truly-the word (- length left))))
                   (declare (word left left-length))
                   (funcall copier string-sap (sb-sys:int-sap position) left)
                   (setf (output-stream-buffer-position stream)
                         (sb-ext:truly-the word (- (output-stream-buffer-end stream) rem)))
                   (flush-buffer stream)
                   (funcall copier (sb-sys:sap+ string-sap (* quot left-length-multiplier))
                            (sb-sys:int-sap start) left-length)
                   (setf (output-stream-buffer-position stream)
                         (sb-ext:truly-the word (+ start left-length)))))))
            (t
             (error "Strings of more than ~a are not supported yet."
                    +buffer-size+)))))
  string)

(declaim (inline write-ascii-string-optimized))
(defun write-ascii-string-optimized (string stream)
  (declare (optimize speed)
           (simple-string string))
  (write-optimized-string-generic string #'copy-mem stream))


(declaim (inline write-ascii-non-base-string-optimized))
(defun write-ascii-non-base-string-optimized (string stream)
  (write-optimized-string-generic string #'copy-mem-non-base-string
                                  stream))

(declaim (inline write-multibyte-string-optimized))
(defun write-multibyte-string-optimized (string stream)
  (write-optimized-string-generic string #'copy-mem-multibyte-string stream
                                  :length-multiplier 3
                                  :left-length-multiplier 4))

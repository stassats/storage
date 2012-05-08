(in-package #:storage)

(declaim (inline copy-mem-generic))
(defun copy-mem-generic (from to length &key (memory-char-size 1)
                                             (buffer-char-size 1))
  (declare (buffer-length length)
           (type (integer 1 8) memory-char-size buffer-char-size)
           (sb-sys:system-area-pointer from to)
           (optimize speed (safety 0)))
  (let* ((from (sb-sys:sap-int from))
         (to (sb-sys:sap-int to))
         (end (+ to length)))
    (declare (word from to end))
    (loop for string-index of-type word = from
          then (+ string-index memory-char-size)
          for buffer-index of-type word = to
          then (+ buffer-index buffer-char-size)
          while (< buffer-index end)
          do
          (setf (sb-sys:sap-ref-word (sb-sys:int-sap buffer-index) 0)
                (sb-sys:sap-ref-word (sb-sys:int-sap string-index) 0)))))

(declaim (inline copy-mem))
(defun copy-mem (from to length)
  (declare (sb-sys:system-area-pointer from to)
           (optimize speed (safety 0)))
  (copy-mem-generic from to length
                    :memory-char-size sb-vm:n-word-bytes
                    :buffer-char-size sb-vm:n-word-bytes))

(declaim (inline copy-mem-non-base-string))
(defun copy-mem-non-base-string (from to length)
  (declare (sb-sys:system-area-pointer from to)
           (optimize speed (safety 0)))
  (copy-mem-generic from to length :memory-char-size 4))

(declaim (inline copy-multibyte-string-to-buffer))
(defun copy-multibyte-string-to-buffer (from to length)
  (declare (sb-sys:system-area-pointer from to)
           (optimize speed (safety 0)))
  (copy-mem-generic from to length
                    :memory-char-size 4
                    :buffer-char-size 3))

(declaim (inline copy-multibyte-string-to-memory))
(defun copy-multibyte-string-to-memory (from to length)
  (declare (buffer-length length)
           (optimize (safety 0)))
  (loop for buffer-index fixnum by 3 below length
        for string-index fixnum by 4
        do (setf (sb-sys:sap-ref-32 to string-index)
                 (sap-ref-24 from buffer-index))))

(declaim (inline write-optimized-string-generic))
(defun write-optimized-string-generic (string copier stream
                                       &key (disk-char-size 1)
                                            (memory-char-size 1))
  (declare (optimize speed)
           (sb-ext:muffle-conditions sb-ext:compiler-note)
           (simple-string string)
           (type (integer 1 4) disk-char-size memory-char-size)
           (function copier))
  (sb-sys:with-pinned-objects (string)
    (let* ((length (* (length string) disk-char-size))
           (position (output-stream-buffer-position stream))
           (string-sap (sb-sys:vector-sap string))
           (new-position (sb-ext:truly-the word (+ position length))))
      (declare (type word position new-position))
      (cond ((<= new-position (output-stream-buffer-end stream))
             (funcall copier string-sap (sb-sys:int-sap position) length)
             (setf (output-stream-buffer-position stream)
                   new-position))
            ((<= length +buffer-size+)
             (let ((left (sb-ext:truly-the
                          word
                          (- (output-stream-buffer-end stream) position))))
               (declare (buffer-length left))
               (multiple-value-bind (quot rem) (floor left disk-char-size)
                 (let* ((start (output-stream-buffer-start stream))
                        (left (- left rem))
                        (left-length (- length left)))
                   (declare (word left left-length))
                   (funcall copier string-sap (sb-sys:int-sap position) left)
                   (setf (output-stream-buffer-position stream)
                         (sb-ext:truly-the
                          word
                          (- (output-stream-buffer-end stream) rem)))
                   (flush-buffer stream)
                   (funcall copier
                            (sb-sys:sap+ string-sap
                                         (* quot memory-char-size))
                            (sb-sys:int-sap start) left-length)
                   (setf (output-stream-buffer-position stream)
                         (sb-ext:truly-the word (+ start left-length)))))))
            (t
             (error "Strings of more than ~a are not supported yet."
                    +buffer-size+)))))
  string)

(declaim (notinline write-ascii-string-optimized))
(defun write-ascii-string-optimized (string stream)
  (declare (optimize speed)
           (simple-string string))
  (write-optimized-string-generic string #'copy-mem stream))


(declaim (inline write-ascii-non-base-string-optimized))
(defun write-ascii-non-base-string-optimized (string stream)
  (write-optimized-string-generic string #'copy-mem-non-base-string
                                  stream
                                  :memory-char-size 4))

(declaim (inline write-multibyte-string-optimized))
(defun write-multibyte-string-optimized (string stream)
  (write-optimized-string-generic string
                                  #'copy-multibyte-string-to-buffer
                                  stream
                                  :disk-char-size 3
                                  :memory-char-size 4))

;;; reading

(declaim (inline read-optimized-string-generic))
(defun read-optimized-string-generic (length string copier stream
                                      &key (disk-char-size 1)
                                           (memory-char-size 1))
  (declare (type sb-int:index length)
           (type (integer 1 4) disk-char-size memory-char-size)
           (optimize speed)
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (sb-sys:with-pinned-objects (string)
    (let* ((position (input-stream-buffer-position stream))
           (length (* length disk-char-size))
           (string-sap (sb-sys:vector-sap string))
           (new-position (sb-ext:truly-the word (+ position length))))
      (declare (type word position new-position))
      (cond ((<= new-position (input-stream-buffer-end stream))
             (funcall copier (sb-sys:int-sap position) string-sap length)
             (setf (input-stream-buffer-position stream)
                   new-position))
            ((<= length +buffer-size+)
             (let ((left (sb-ext:truly-the
                          word
                          (- (input-stream-buffer-end stream) position))))
               (declare (buffer-length left))
               (multiple-value-bind (quot rem) (floor left disk-char-size)
                 (let* ((start (input-stream-buffer-start stream))
                        (end (input-stream-buffer-end stream))
                        (left (- left rem))
                        (left-bytes (- disk-char-size rem))
                        (left-length (- length left)))
                   (declare (word left left-length)
                            (type (integer 0 3) left-bytes))
                   (when (> left-length (input-stream-left stream))
                     (error "End of file ~a" stream))
                   (funcall copier (sb-sys:int-sap position) string-sap left)
                   (setf string-sap
                         (sb-sys:sap+ string-sap
                                      (* quot memory-char-size)))
                   (cond
                     ((> rem 0)
                      (let ((left-char
                              (sb-ext:truly-the
                               (unsigned-byte 24)
                               (n-sap-ref rem
                                          (sb-sys:int-sap (- end rem)) 0))))
                        (decf left-length 3)
                        (fill-buffer stream 0)
                        (setf (sb-sys:sap-ref-32 string-sap 0)
                              (logior left-char
                                      (ash
                                       (the (unsigned-byte 24)
                                            (n-sap-ref left-bytes
                                                       (sb-sys:int-sap start)))
                                       (* rem 8))))
                        (setf start
                              (sb-ext:truly-the word (+ start left-bytes)))
                        (setf string-sap
                              (sb-sys:sap+ string-sap
                                           memory-char-size))))
                     (t
                      (fill-buffer stream 0)))
                   (funcall copier (sb-sys:int-sap start) string-sap left-length)

                   (setf (input-stream-buffer-position stream)
                         (sb-ext:truly-the word (+ start left-length)))))))
            (t
             (error "Strings of more than ~a are not supported yet."
                    +buffer-size+)))))
  string)

(declaim (inline read-ascii-string-optimized))
(defun read-ascii-string-optimized (length string stream)
  (read-optimized-string-generic length string #'copy-mem stream))

(declaim (inline read-multibyte-string-optimized))
(defun read-multibyte-string-optimized (length string stream)
  (read-optimized-string-generic length string
                                 #'copy-multibyte-string-to-memory
                                 stream
                                 :disk-char-size 3
                                 :memory-char-size 4))

(in-package #:storage)

(declaim (inline copy-string))
(defun copy-string (string buffer buffer-end
                    &key (memory-char-size 1)
                         (buffer-char-size 1)
                         from-memory)
  (declare (type (integer 1 8) memory-char-size buffer-char-size)
           (word string buffer buffer-end)
           (optimize speed (safety 0))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (when (= 1 memory-char-size buffer-char-size)
    (setf memory-char-size sb-vm:n-word-bytes
          buffer-char-size sb-vm:n-word-bytes))
  (loop for string-index of-type word = string
        then (+ string-index memory-char-size)
        for buffer-index of-type word = buffer
        then (+ buffer-index buffer-char-size)
        while (< buffer-index buffer-end)
        do
        (if from-memory
            (setf (mem-ref-word buffer-index)
                  (mem-ref-word string-index))
            (setf (mem-ref-word string-index)
                  (if (< buffer-char-size memory-char-size)
                      (n-mem-ref buffer-char-size buffer-index)
                      (mem-ref-word buffer-index))))))

(declaim (inline write-optimized-string-generic))
(defun write-optimized-string-generic (string stream
                                       &key (buffer-char-size 1)
                                            (memory-char-size 1))
  (declare (optimize speed)
           (sb-ext:muffle-conditions sb-ext:compiler-note)
           (simple-string string)
           (type (integer 1 4) buffer-char-size memory-char-size))
  (sb-sys:with-pinned-objects (string)
    (let* ((length (* (length string) buffer-char-size))
           (position (output-stream-buffer-position stream))
           (string (vector-address string))
           (new-position (sb-ext:truly-the word (+ position length))))
      (declare (type word position new-position))
      (cond ((<= new-position (output-stream-buffer-end stream))
             (copy-string string position new-position
                          :buffer-char-size buffer-char-size
                          :memory-char-size memory-char-size
                          :from-memory t)
             (setf (output-stream-buffer-position stream)
                   new-position))
            ((<= length +buffer-size+)
             (let ((left (sb-ext:truly-the
                          word
                          (- (output-stream-buffer-end stream) position))))
               (declare (buffer-length left))
               (multiple-value-bind (quot rem) (floor left buffer-char-size)
                 (let* ((start (output-stream-buffer-start stream))
                        (left (- left rem))
                        (left-length (- length left)))
                   (declare (word left left-length))
                   (copy-string string position (+ position left)
                                :buffer-char-size buffer-char-size
                                :memory-char-size memory-char-size
                                :from-memory t)
                   (setf (output-stream-buffer-position stream)
                         (sb-ext:truly-the
                          word
                          (- (output-stream-buffer-end stream) rem)))
                   (flush-buffer stream)
                   (copy-string (+ string (* quot memory-char-size))
                                start (+ start left-length)
                                :buffer-char-size buffer-char-size
                                :memory-char-size memory-char-size
                                :from-memory t)
                   (setf (output-stream-buffer-position stream)
                         (sb-ext:truly-the word (+ start left-length)))))))
            (t
             (error "Strings of more than ~a are not supported yet."
                    +buffer-size+)))))
  string)

;;; reading

(declaim (inline read-optimized-string-generic))
(defun read-optimized-string-generic (length string stream
                                      &key (buffer-char-size 1)
                                           (memory-char-size 1))
  (declare (type sb-int:index length)
           (type (integer 1 4) buffer-char-size memory-char-size)
           (optimize speed)
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (sb-sys:with-pinned-objects (string)
    (let* ((position (input-stream-buffer-position stream))
           (length (* length buffer-char-size))
           (string (vector-address string))
           (new-position (sb-ext:truly-the word (+ position length))))
      (declare (type word position new-position))
      (cond ((<= new-position (input-stream-buffer-end stream))
             (copy-string string position new-position
                          :buffer-char-size buffer-char-size
                          :memory-char-size memory-char-size)
             (setf (input-stream-buffer-position stream)
                   new-position))
            ((<= length +buffer-size+)
             (let ((left (sb-ext:truly-the
                          word
                          (- (input-stream-buffer-end stream) position))))
               (declare (buffer-length left))
               (multiple-value-bind (quot rem) (floor left buffer-char-size)
                 (let* ((start (input-stream-buffer-start stream))
                        (end (input-stream-buffer-end stream))
                        (left (- left rem))
                        (left-bytes (- buffer-char-size rem))
                        (left-length (- length left)))
                   (declare (word left left-length)
                            (type (integer 0 3) left-bytes))
                   (when (> left-length (input-stream-left stream))
                     (error "End of file ~a" stream))
                   (copy-string string position (+ position left)
                                :buffer-char-size buffer-char-size
                                :memory-char-size memory-char-size)
                   (incf string (* quot memory-char-size))
                   (cond
                     ((> rem 0)
                      (let ((left-char
                              (sb-ext:truly-the
                               (unsigned-byte 24)
                               (n-mem-ref rem (- end rem)))))
                        (decf left-length 3)
                        (fill-buffer stream 0)
                        (setf (mem-ref-32 string)
                              (logior left-char
                                      (ash
                                       (the (unsigned-byte 24)
                                            (n-mem-ref left-bytes start))
                                       (* rem 8))))
                        (setf start
                              (sb-ext:truly-the word (+ start left-bytes)))
                        (incf string memory-char-size)))
                     (t
                      (fill-buffer stream 0)))
                   (copy-string string start (+ start left-length)
                                :buffer-char-size buffer-char-size
                                :memory-char-size memory-char-size)
                   (setf (input-stream-buffer-position stream)
                         (sb-ext:truly-the word (+ start left-length)))))))
            (t
             (error "Strings of more than ~a are not supported yet."
                    +buffer-size+)))))
  string)

(declaim (inline read-ascii-string-optimized))
(defun read-ascii-string-optimized (length string stream)
  (read-optimized-string-generic length string stream))

(declaim (inline write-ascii-string-optimized))
(defun write-ascii-string-optimized (string stream)
  (declare (simple-string string))
  (write-optimized-string-generic string stream))

(declaim (inline write-ascii-non-base-string-optimized))
(defun write-ascii-non-base-string-optimized (string stream)
  (write-optimized-string-generic string stream :memory-char-size 4))

(declaim (inline read-multibyte-string-optimized))
(defun read-multibyte-string-optimized (length string stream)
  (read-optimized-string-generic length string stream
                                 :buffer-char-size 3
                                 :memory-char-size 4))

(declaim (inline write-multibyte-string-optimized))
(defun write-multibyte-string-optimized (string stream)
  (write-optimized-string-generic string stream
                                  :buffer-char-size 3
                                  :memory-char-size 4))

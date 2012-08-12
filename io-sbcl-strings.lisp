(in-package #:storage)

(declaim (inline copy-string))
(defun copy-string (string buffer buffer-end
                    &key (memory-char-size 1)
                         (buffer-char-size 1)
                         from-memory)
  (declare (type (integer 1 8) memory-char-size buffer-char-size)
           (word string buffer buffer-end)
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (when (= 1 memory-char-size buffer-char-size)
    (setf memory-char-size sb-vm:n-word-bytes
          buffer-char-size sb-vm:n-word-bytes))
  (loop for string-index of-type word = string
        then (truly-the word (+ string-index memory-char-size))
        for buffer-index of-type word = buffer
        then (truly-the word (+ buffer-index buffer-char-size))
        while (< buffer-index buffer-end)
        do
        (if from-memory
            (setf (mem-ref-word buffer-index)
                  (mem-ref-word string-index))
            (setf (mem-ref-word string-index)
                  (if (< buffer-char-size memory-char-size)
                      (n-mem-ref buffer-char-size buffer-index)
                      (mem-ref-word buffer-index))))))

(defun write-string-boundary (length string stream
                              position buffer-char-size memory-char-size)
  (let* ((end (output-stream-buffer-end stream))
         (space-in-buffer (- end position)))
    (multiple-value-bind (quot rem)
        (floor space-in-buffer buffer-char-size)
      (let* ((start (output-stream-buffer-start stream))
             (new-position (+ start (- length (- space-in-buffer rem))))
             (adjusted-end (- end rem)))
        (copy-string string position adjusted-end
                     :buffer-char-size buffer-char-size
                     :memory-char-size memory-char-size
                     :from-memory t)
        (flush-buffer stream (- adjusted-end start))
        (copy-string (+ string (* quot memory-char-size))
                     start new-position
                     :buffer-char-size buffer-char-size
                     :memory-char-size memory-char-size
                     :from-memory t)
        (setf (output-stream-buffer-position stream)
              new-position)))))

(declaim (inline write-optimized-string-generic))
(defun write-optimized-string-generic (string stream
                                       &key (buffer-char-size 1)
                                            (memory-char-size 1))
  (declare (simple-string string)
           (type (integer 1 4) buffer-char-size memory-char-size)
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (sb-sys:with-pinned-objects (string)
    (let* ((length (* (length string) buffer-char-size))
           (position (output-stream-buffer-position stream))
           (string (vector-address string))
           (new-position (truly-the word (+ position length))))
      (declare (type word position new-position))
      (cond ((<= new-position (output-stream-buffer-end stream))
             (copy-string string position new-position
                          :buffer-char-size buffer-char-size
                          :memory-char-size memory-char-size
                          :from-memory t)
             (setf (output-stream-buffer-position stream)
                   new-position))
            ((<= length +buffer-size+)
             (write-string-boundary length string stream
                                    position buffer-char-size memory-char-size))
            (t
             (error "Strings of more than ~a are not supported yet."
                    +buffer-size+)))))
  string)

;;; reading

(defun read-string-boundary (length string stream position
                             buffer-char-size memory-char-size)
  (let ((left (- (input-stream-buffer-end stream) position)))
    (multiple-value-bind (quot rem) (floor left buffer-char-size)
      (let* ((start (input-stream-buffer-start stream))
             (end (input-stream-buffer-end stream))
             (left (- left rem))
             (left-bytes (- buffer-char-size rem))
             (left-length (- length left)))
        (when (> left-length (input-stream-left stream))
          (error "End of file ~a" stream))
        (copy-string string position (+ position left)
                     :buffer-char-size buffer-char-size
                     :memory-char-size memory-char-size)
        (incf string (* quot memory-char-size))
        (cond
          ((> rem 0)
           (let ((left-char (n-mem-ref rem (- end rem))))
             (decf left-length 3)
             (fill-buffer stream 0)
             (setf (mem-ref-32 string)
                   (logior left-char
                           (ash (n-mem-ref left-bytes start) (* rem 8))))
             (setf start (+ start left-bytes))
             (incf string memory-char-size)))
          (t
           (fill-buffer stream 0)))
        (copy-string string start (+ start left-length)
                     :buffer-char-size buffer-char-size
                     :memory-char-size memory-char-size)
        (setf (input-stream-buffer-position stream) (+ start left-length))))))

(declaim (inline read-optimized-string-generic))
(defun read-optimized-string-generic (length string stream
                                      &key (buffer-char-size 1)
                                           (memory-char-size 1))
  (declare (type sb-int:index length)
           (type (integer 1 4) buffer-char-size memory-char-size)
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (sb-sys:with-pinned-objects (string)
    (let* ((position (input-stream-buffer-position stream))
           (length (* length buffer-char-size))
           (string (vector-address string))
           (new-position (truly-the word (+ position length))))
      (declare (type word position new-position))
      (cond ((<= new-position (input-stream-buffer-end stream))
             (copy-string string position new-position
                          :buffer-char-size buffer-char-size
                          :memory-char-size memory-char-size)
             (setf (input-stream-buffer-position stream)
                   new-position))
            ((<= length +buffer-size+)
             (read-string-boundary length string stream position
                                   buffer-char-size memory-char-size))
            (t
             (error "Strings of more than ~a are not supported yet."
                    +buffer-size+)))))
  string)

(declaim (inline read-ascii-string-optimized))
(defun read-ascii-string-optimized (length string stream)
  (declare (simple-string string)
           (optimize speed))
  (read-optimized-string-generic length string stream))

(declaim (inline write-ascii-string-optimized))
(defun write-ascii-string-optimized (string stream)
  (declare (simple-string string)
           (optimize speed))
  (write-optimized-string-generic string stream))

(declaim (inline write-ascii-non-base-string-optimized))
(defun write-ascii-non-base-string-optimized (string stream)
  (declare (simple-string string)
           (optimize speed))
  (write-optimized-string-generic string stream :memory-char-size 4))

(declaim (inline read-multibyte-string-optimized))
(defun read-multibyte-string-optimized (length string stream)
  (declare (simple-string string)
           (optimize speed))
  (read-optimized-string-generic length string stream
                                 :buffer-char-size 3
                                 :memory-char-size 4))

(declaim (inline write-multibyte-string-optimized))
(defun write-multibyte-string-optimized (string stream)
  (declare (simple-string string)
           (optimize speed))
  (write-optimized-string-generic string stream
                                  :buffer-char-size 3
                                  :memory-char-size 4))
;;;

(declaim (inline optimized-ascii-string-p)
         (inline optimized-ascii-string-p-old))
(defun optimized-ascii-string-p-old (string)
  (declare (simple-string string)
           (optimize speed))
  (sb-sys:with-pinned-objects (string)
    (let* ((start (vector-address string))
           (end (truly-the word (+ start
                                   (* (length string) 4)))))
      (declare (word start end))
      (loop for address of-type word = start
            then (truly-the word (+ address sb-vm:n-word-bytes))
            while (< address end)
            never (logtest (mem-ref-word address) +ascii-mask+)))))

(defun ascii-test (string)
  (declare (simple-string string))
  (ascii-test string))

(defun optimized-ascii-string-p (string)
  (declare (simple-string string)
           (optimize speed (safety 0)))
  (ascii-test string))

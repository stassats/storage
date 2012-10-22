(in-package #:storage)

(declaim (inline read-ascii-string-optimized))
(defun read-ascii-string-optimized (length string stream)
  (loop for i below length
        do (setf (schar string i)
                 (code-char (read-n-bytes 1 stream)))))

(declaim (inline read-multibyte-string-optimized))
(defun read-multibyte-string-optimized (length string stream)
  (loop for i below length
        do (setf (schar string i)
                 (code-char (read-n-bytes +char-length+ stream)))))

(declaim (inline write-multibyte-string-optimized))
(defun write-multibyte-string-optimized (string stream)
  (loop for char across string
        do (write-n-bytes (char-code char) +char-length+ stream)))

(declaim (inline write-ascii-non-base-string-optimized))
(defun write-ascii-non-base-string-optimized (string stream)
  (loop for char across string
        do (write-n-bytes (char-code char) 1 stream)))

(defun ascii-string-p (string)
  (declare (simple-string string))
  (loop for char across string
        always (char< char +ascii-char-limit+)))

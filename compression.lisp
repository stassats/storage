(in-package #:storage)

(deftype ascii-char-code ()
  '(integer 32 126))

(declaim (inline ascii-char-p))
(defun ascii-char-p (char)
  (< 31 (char-code char) 127))

(declaim (inline relative-ascii-code))
(defun relative-ascii-code (char)
  (1+ (- (char-code char) (char-code #\Space))))

(defparameter *measurement-char-table* (make-hash-table :test #'equal))

(defun measure-string (string)
  (loop for i below (1- (length string))
        for substring = (subseq string i (+ i 2))
        do (incf (gethash substring *measurement-char-table* 0))))

(defun present-table (&key (max 128))
  (let ((alist (sort (alexandria:hash-table-alist *measurement-char-table*) #'>
                     :key #'cdr)))
    (loop repeat max
          for (key . n) in alist
          do
          (format t "~a  ~a~%" key n))))

(defun make-table ()
  (let ((alist (sort (alexandria:hash-table-alist *measurement-char-table*) #'>
                     :key #'cdr))
        (string (make-array 255)))
    (loop for i below (length string)
          do (setf (aref string i)
                   (if (ascii-char-p (code-char i))
                       (code-char i)
                       (car (pop alist)))))
    string))

(defun make-encode-table (decode-table)
  (let ((array (make-array '(95 95)
                           :element-type 'fixnum
                           :initial-element -1)))
    (loop for i from 0
          for pair across decode-table
          when (stringp pair)
          do (setf (aref array
                         (relative-ascii-code (char pair 0))
                         (relative-ascii-code (char pair 1)))
                   i))
    array))

(defun make-decode-table (table)
  (map-into (make-array 255 :element-type 'fixnum)
            (lambda (x)
              (if (stringp x)
                  (let ((a (char-code (char x 0)))
                        (b (char-code (char x 1))))
                    (+ b (* a 256)))
                  (char-code x)))
            table))

;;; 

(defparameter *char-table*
  #("er" "ed" "an" "te" "re" "e " "ar" " (" "n " "it" "on" "in" "nc" "un" "di"
    "en" "s " "r " "cr" "d)" "or" "ic" "(u" "ri" "el" "le" "y " " S" " M" "ll"
    "t " "ra" #\  #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\, #\- #\. #\/ #\0
    #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\: #\; #\< #\= #\> #\? #\@ #\A #\B #\C
    #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V
    #\W #\X #\Y #\Z #\[ #\\ #\] #\^ #\_ #\` #\a #\b #\c #\d #\e #\f #\g #\h #\i
    #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\{ #\|
    #\} #\~ " C" "es" "ne" " B" "at" "Ma" "is" "as" "al" "ce" "li" "he" "l " "ie"
    "a " "d " "nd" "ch" "st" "ng" "il" "ma" "nt" "to" " G" " P" "rt" " D" "ha"
    "la" "th" "de" "se" "am" "ro" "ol" "ge" "na" "ti" " H" "et" " A" "ia" " R"
    "ta" " W" " L" ". " " T" "ve" "om" "ur" "Co" "nn" "ea" "ni" "rd" "us" "hi"
    "ou" "ck" "me" "ss" "so" " F" "ir" "Pa" "Da" "Ca" "tt" "o " "ot" "Jo" "io"
    "Ch" "(a" "rs" "St" "ac" "Ro" "im" "oo" "lo" "Ba" "ai" "be" "ke" "rr" "ry"
    " K" "ee" "h " "Mi" "k " "os" "oi" "g " "ad" "ey" "ho" "id" " J" "ns" "rl"
    "tr" "sh" "ec" "vi" "Ja" "m " "ay" "Ha" "si" "au" "ld" "Th" "'s" "e)" "sa"
    " E" "An" "Br" "De" "vo" "Re" "ag" "mi" "Mo"))

(declaim (type (simple-array fixnum (255)) *decode-char-table*))
(defparameter *decode-char-table* (make-decode-table *char-table*))

(declaim (type (simple-array fixnum (95 95)) *encode-char-table*))
(defparameter *encode-char-table* (make-encode-table *char-table*))

(declaim (inline encode-pair))
(defun encode-pair (a b)
  (aref *encode-char-table*
        (relative-ascii-code a)
        (relative-ascii-code b)))

(declaim (inline decode-pair))
(defun decode-pair (char)
  (aref *decode-char-table* char))

(declaim (inline split-pair))
(defun split-pair (pair)
  (truncate pair 256))

(declaim (inline do-pair-encode))
(defun do-pair-encode (string function)
  (declare (simple-base-string string))
  (cond ((= (length string) 0))
        ((= (length string) 1)
         (funcall function (char string 0)))
        (t
         (loop with i = 0
               for a = (char string i)
               for b = (char string (1+ i))
               for coded = (encode-pair a b)
               do (cond ((plusp coded)
                         (funcall function (code-char coded))
                         (incf i 2))
                        (t
                         (funcall function a)
                         (incf i)))
               while (< i (- (length string) 1))
               finally
               (when (> (length string) i)
                 (funcall function (char string i)))))))

(defun pair-encode (string)
  (let ((new-string (make-string (length string)))
        (new-i -1))
    (do-pair-encode string
      (lambda (char)
        (setf (char new-string (incf new-i)) char)))
    (subseq new-string 0 (1+ new-i))))

(declaim (inline do-pair-decode))
(defun do-pair-decode (char function)
  (if (ascii-char-p (code-char char))
      (funcall function char nil)
      (funcall function (decode-pair char) t)))

(defun pair-decode (string)
  (let* ((length (+ (length string)
                    (count-if-not #'ascii-char-p string)))
         (new-string (make-string length :element-type 'base-char))
         (new-index -1))
    (loop for char across string
          do (do-pair-decode (char-code char)
               (lambda (a b)
                 (declare (type fixnum a))
                 (if b
                     (multiple-value-bind (a b) (split-pair a)
                       (setf (char new-string (incf new-index))
                             (code-char a)
                             (char new-string (incf new-index))
                             (code-char b)))
                     (setf (char new-string (incf new-index))
                                            (code-char a))))))
    new-string))

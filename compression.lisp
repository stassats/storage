(in-package #:storage)

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

(defun build-table ()
  (let ((alist (sort (alexandria:hash-table-alist *measurement-char-table*) #'>
                     :key #'cdr))
        (string (make-array 255)))
    (loop for i below (length string)
          do (setf (aref string i)
                   (if (ascii-char-p (code-char i))
                       (code-char i)
                       (car (pop alist)))))
    string))

;;;

(defvar *numeber-of-ascii-chars* 95)

(defvar *decode-char-table*
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

(defparameter *encode-char-table* (make-write-char-table))

(defun ascii-char-p (char)
  (< 31 (char-code char) 127))

(defun relative-ascii-code (char)
  (1+ (- (char-code char) (char-code #\Space))))

(defun make-write-char-table ()
  (let ((array (make-array '(95 95)
                           :element-type 'fixnum
                           :initial-element -1)))
    (loop for i from 0
          for pair across *decode-char-table*
          when (stringp pair)
          do (setf (aref array
                         (relative-ascii-code (char pair 0))
                         (relative-ascii-code (char pair 1)))
                   i))
    array))

(defun encode-pair (a b)
  (aref *encode-char-table* (relative-ascii-code a) (relative-ascii-code b)))

(defun decode-pair (char)
  (aref *decode-char-table* (char-code char)))

(defun do-pair-encode (string function)
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

(defun do-pair-decode (char function)
  (if (ascii-char-p char)
      (funcall function char nil)
      (let ((decode (decode-pair char)))
        (funcall function (char decode 0) (char decode 1)))))

(defun pair-decode (string)
  (let* ((length (+ (length string)
                    (count-if-not #'ascii-char-p string)))
         (new-string (make-string length :element-type 'base-char))
         (new-index -1))
    (loop for char across string
          do (do-pair-decode char
               (lambda (a b)
                 (setf (char new-string (incf new-index)) a)
                 (when b
                   (setf (char new-string (incf new-index)) b)))))
    new-string))

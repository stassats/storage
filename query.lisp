(in-package :storage)

(defun where-compile (&rest clauses)
  (let ((slots (loop for slot in clauses by #'cddr
                     collect slot))
        (values (loop for value in (cdr clauses) by #'cddr collect value)))
    (compile
     nil
     `(lambda (object)
        (with-slots ,slots object
          (and
           ,@(mapcar (lambda (slot value)
                       (typecase value
                         (function
                          `(funcall ,value ,slot))
                         (string
                          (let ((reversed (reverse-case value)))
                            `(and (stringp ,slot)
                                  (do-kmp ,value ,reversed
                                    ,slot ,(build-table value reversed)))))
                         (t
                          `(equalp ,value ,slot))))
                     slots values)))))))

(define-compiler-macro where (&whole form &rest clauses
                                     &environment env)
  (if (loop for clause in clauses
            always (constantp clauses env))
      `(load-time-value (where-compile ,@clauses))
      form))

;;;


(defun make-tester (slot value next)
  (declare (optimize speed)
           (function next))
  (flet ((call-next (object)
           (funcall next object))
         (slot (object)
           (slot-value object slot)))
    (declare (inline call-next slot))
    (typecase value
      (function
       (lambda (object)
        (declare (function value))
        (and (funcall value (slot object))
             (call-next object))))
      (string
       (let* ((reversed (reverse-case value))
              (table (build-table value reversed)))
         (lambda (object)
           (let ((slot (slot object)))
             (and slot
                  (do-kmp value reversed
                    slot table))))))
      (t
       (lambda (object)
         (and (equalp (slot object) value)
              (call-next object)))))))

(defun make-tester-chain (clauses)
  (if clauses
      (make-tester (car clauses) (cadr clauses)
                   (make-tester-chain (cddr clauses)))
      (constantly t)))

(defun where (&rest clauses)
  (make-tester-chain clauses))

;;;

(defun lookup (type &optional test)
  (let (results)
    (map-data (lambda (key objects)
                (when (subtypep key type)
                  (loop for object in objects
                        when (funcall test object)
                        do (push object results)))))
    (if (= (length results) 1)
        (car results)
        results)))

(defun count (type &optional test)
  (let ((count 0))
    (map-data (lambda (key objects)
                (when (subtypep key type)
                  (incf count
                        (if (null test)
                            (length objects)
                            (count-if test objects))))))
    count))

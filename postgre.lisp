
(in-package :storage)

(defvar *database*)

(defun connect ()
  (cl-postgres:open-database "storage" "stas" "" "localhost"))

(defun ensure-class-table-exists (class)
  (or (postmodern:table-exists-p (class-name class))
      (make-class-table class)))

(defun translate-type-for-sql (type)
  (if (consp type)
      (format-query "~a[]" (translate-type-for-sql (car type)))
      (case type
        (:integer 'integer)
        (:string 'text)
        (:boolean 'boolean)
        (t (unless (keywordp type)
             'integer)))))

(defun translate-name-for-sql (name)
  (substitute #\_ #\- (string name)))

(defun translate (object slot-definition)
  (translate-object (slot-value-using-class (class-of object)
                                            object
                                            slot-definition)
                    (slot-db-type slot-definition)))

(defgeneric translate-object (value type))

(defmethod translate-object (value type)
  value)

(defmethod translate-object (value (type (eql :boolean)))
  (if value
      1
      0))

(defun sql-class-slots (class)
  (loop for slot in (class-slots class)
        when (store-slot-p slot)
        collect (format nil "~a ~a" (translate-name-for-sql (slot-definition-name slot))
                        (translate-type-for-sql (slot-db-type slot)))))

(defun exec-query (query)
  (cl-postgres:exec-query *database* query))

(defun format-query (control-string &rest arguments)
  (let ((*print-case* :downcase))
    (apply #'format nil control-string arguments)))

(defun make-class-table (class)
  (when (postmodern:table-exists-p (class-name class))
   (exec-query (format-query "DROP TABLE ~a" (class-name class))))
  (exec-query
   (format-query "CREATE TABLE ~a (id integer, ~{~a~^, ~}) PRIMARY KEY (id)~
~@[ INHERITS (~{~a~^, ~})~]"
                 (class-name class)
                 (sql-class-slots class)
                 (loop for superclass in (class-direct-superclasses class)
                       unless (eql superclass (find-class 'standard-object))
                       collect (class-name superclass)
                       and do (ensure-class-table-exists superclass)))))

(defun insert-object (object)
  (let ((class (class-of object)))
    (format-query "INSERT INTO ~a (~{~a~^, ~}) VALUES (~{~s~^, ~})"
                  (class-name class)
                  (map 'list #'slot-definition-name (slots-to-store class))
                  (map 'list (lambda (slot) (translate object slot))
                       (slots-to-store class)))))

(defun store-data ()
  (map-data (lambda (class objects)
              (declare (ignore objects))
              (ensure-class-table-exists class)))
  (map-data (lambda (class objects)
              (declare (ignore class))
              (dolist (object objects)
                (insert-object object)))))

(defun save-data-postgre (storage)
  (let ((*storage* storage))
    (when (storage-data *storage*)
      (with-io-file (stream (or file (storage-file *storage*))
                            :direction :output
                            :size (measure-size))
        (dump-data stream)))))

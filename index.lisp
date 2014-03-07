(in-package :graph-utils)

(defclass index ()
  ((edge-type :initarg :edge-type :accessor edge-type)
   (key-equality-fn :initarg :key-equality-fn :accessor key-equality-fn)
   (value-equality-fn :initarg :value-equality-fn :accessor value-equality-fn)
   (index :initarg :index :accessor index)))

(defclass unique-index (index) ())

(defclass ordered-index (index)
  ((ordering-fn :initarg :ordering-fn :accessor ordering-fn)))

(defclass unique-ordered-index (ordered-index unique-index)
  ())

(defun make-index (&key (type 'index) (key-equality-fn 'equal)
                   (value-equality-fn 'equal) ordering-fn edge-type)
  (unless edge-type
    (error "You must supply an edge-type to make-index"))
  (let ((idx (make-instance type
                            :key-equality-fn key-equality-fn
                            :value-equality-fn value-equality-fn
                            :edge-type edge-type)))
    (case type
      (unique-ordered-index
       (setf (ordering-fn idx) ordering-fn
             (index idx) (cl-skip-list:make-skip-list
                          :key-equal key-equality-fn
                          :value-equal value-equality-fn
                          :duplicates-allowed? nil
                          :comparison ordering-fn)))
      (ordered-index
       (setf (ordering-fn idx) ordering-fn
             (index idx) (cl-skip-list:make-skip-list
                          :key-equal key-equality-fn
                          :value-equal value-equality-fn
                          :duplicates-allowed? t
                          :comparison ordering-fn)))
      ((index unique-index)
       (setf (index idx) (make-hash-table :test key-equality-fn :synchronized t)))
      (otherwise
       (error "Unknown index type ~A" type)))
    idx))

(defgeneric idx-insert (idx key value &key replace?))
(defgeneric idx-remove (idx key &optional value))
(defgeneric idx-get (idx key &key limit))

(defmethod idx-insert ((idx index) key value &key replace?)
  (declare (ignore replace?))
  (pushnew value (gethash key (index idx)) :test (value-equality-fn idx)))

(defmethod idx-remove ((idx index) key &optional value)
  (setf (gethash key (index idx))
        (if value
            (delete value (gethash key (index idx)) :test (value-equality-fn idx))
            nil)))

(defmethod idx-get ((idx index) key &key limit)
  (if (integerp limit)
      (let ((r (gethash key (index idx))))
        (if (> (length r) limit)
            (subseq r 0 limit)
            r))
      (gethash key (index idx))))

(defmethod idx-insert ((idx unique-index) key value &key replace?)
  (if (and (gethash key (index idx)) (null replace?))
      (error "~A is already populated" key)
      (setf (gethash key (index idx)) value)))

(defmethod idx-remove ((idx unique-index) key &optional value)
  (setf (gethash key (index idx))
        (if value
            (delete value (gethash key (index idx)) :test (value-equality-fn idx))
            nil)))

(defmethod idx-get ((idx unique-index) key &key limit)
  (if (integerp limit)
      (let ((r (gethash key (index idx))))
        (if (> (length r) limit)
            (subseq r 0 limit)
            r))
      (gethash key (index idx))))

(defmethod idx-insert ((idx ordered-index) key value &key replace?)
  (if (and replace? (skip-list-lookup (index idx) key))
      (skip-list-replace-kv (index idx) key value)
      (skip-list-add (index idx) key value)))

(defmethod idx-remove ((idx ordered-index) key &optional value)
  (skip-list-delete (index idx) key value))

(defmethod idx-get ((idx ordered-index) key &key limit)
  (skip-list-fetch-all (index idx) key))

(defmethod idx-get ((idx unique-ordered-index) key &key limit)
  (declare (ignore limit))
  (skip-list-lookup (index idx) key))

(in-package :graph-utils)

(defclass sparse-array ()
  ((dimensions :accessor dimensions :initarg :dimensions :initform 1)
   (row-count :accessor row-count :initarg :row-count :initform 0)
   (col-count :accessor col-count :initarg :col-count :initform 0)
   (matrix :accessor matrix :initarg :matrix
           :initform
           #+sbcl (make-hash-table :synchronized t)
           #-sbcl (make-hash-table))
   (adjustable? :accessor adjustable? :initarg :adjustable? :initform nil)
   (initial-element :accessor initial-element :initarg :initial-element
                    :initform nil)
   (element-type :accessor element-type :initarg :element-type :initform nil)
   (array-lock :accessor array-lock :initform (make-recursive-lock))))

(defgeneric sparse-array? (thing)
  (:documentation "sparse array predicate")
  (:method ((thing sparse-array)) t)
  (:method (thing) nil))

(defmethod print-object ((array sparse-array) stream)
  (format stream "#<SPARSE-ARRAY (~D" (row-count array))
  (when (> (col-count array) 0)
    (format stream "x~D" (col-count array)))
  (format stream ")")
  (format stream " INITIAL-ELEMENT: ~A" (initial-element array))
  (when (element-type array)
    (format stream " ~A" (element-type array)))
  (if (adjustable? array)
      (format stream " ADJUSTABLE>")
      (format stream ">")))

(defmethod incf-sarray-dimensions ((array sparse-array) &optional (delta 1))
  (with-recursive-lock-held ((array-lock array))
    (if (= 1 (dimensions array))
        (incf (row-count array) delta)
        (values
         (incf (row-count array) delta)
         (incf (col-count array) delta)))))

(defmethod adjust-sarray ((array sparse-array) &rest dimensions)
  "FIXME: handle shrinking by deleting just like adjust-array"
  (if (adjustable? array)
      (with-recursive-lock-held ((array-lock array))
        (if (= 1 (dimensions array))
            (setf (row-count array) (first dimensions))
            (setf (row-count array) (first dimensions)
                  (col-count array) (second dimensions))))
      (error "Sparse array not adjustable.")))

(defmethod make-sparse-array (dimensions &key initial-element adjustable
                              element-type)
  (when (or (> (length dimensions) 2) (= 0 (length dimensions)))
    (error "We only support 1D and 2D arrays."))
  (when element-type
    (unless (typep initial-element element-type)
      (error "initial-element ~A is not of specified type ~A"
             initial-element element-type)))
  (let ((array (make-instance 'sparse-array
                              :element-type element-type
                              :dimensions (length dimensions)
                              :row-count (first dimensions)
                              :col-count (or (second dimensions) 0)
                              :initial-element initial-element
                              :adjustable? adjustable)))
    array))

(defmethod make-sparse-vector (&key size initial-element adjustable element-type)
  (when element-type
    (unless (typep initial-element element-type)
      (error "initial-element ~A is not of specified type ~A"
             initial-element element-type)))
  (let ((array (make-instance 'sparse-array
                              :element-type element-type
                              :dimensions 1
                              :row-count (or size 0)
                              :col-count 1
                              :initial-element initial-element
                              :adjustable? adjustable)))
    array))

(defmethod sparse-vector-size ((vector sparse-array))
  (row-count vector))

(defmethod svector-push-extend ((vector sparse-array) value)
  (if (and (adjustable? vector) (= 1 (dimensions vector)))
      (prog1
          (sparse-vector-size vector)
        (when (and (element-type vector) (not (typep value (element-type vector))))
          (error "svector-push-extend: ~A is not of type ~A"
                 value (element-type vector)))
        (setf (gethash (row-count vector) (matrix vector)) value)
        (incf (row-count vector)))
      (error "~A is not an adjustable vector" vector)))

(defmethod saref ((array sparse-array) &rest indices)
  (cond ((or (null indices) (/= (length indices) (dimensions array)))
         (error "You must supply propery indices for ~A" array))
        ((and (= 1 (length indices)) (= 1 (dimensions array)))
         (when (>= (nth 0 indices) (row-count array))
           (error "~A is out of bounds for rows of ~A: max row is ~A"
                  (1- (row-count array)) array (nth 0 indices)))
         (gethash (nth 0 indices) (matrix array) (initial-element array)))
        ((and (= 2 (length indices)) (= 2 (dimensions array)))
         (when (>= (nth 0 indices) (row-count array))
           (error "~A is out of bounds for rows of ~A: max row is ~A"
                  (1- (row-count array)) array (nth 0 indices)))
         (when (>= (nth 1 indices) (col-count array))
           (error "~A is out of bounds for cols of ~A: max row is ~A"
                  (1- (col-count array)) array (nth 1 indices)))
         (let ((table (gethash (nth 0 indices) (matrix array))))
           (if (hash-table-p table)
               (gethash (nth 1 indices) table (initial-element array))
               (initial-element array))))))

(defun set-sparse-array (array indices value)
  (when (and (element-type array) (not (typep value (element-type array))))
    (error "svector-push-extend: ~A is not of type ~A" value (element-type array)))
  (with-recursive-lock-held ((array-lock array))
    (cond ((or (null indices) (/= (length indices) (dimensions array)))
           (error "You must supply propery indices for ~A" array))
          ((and (= 1 (length indices)) (= 1 (dimensions array)))
           (when (>= (nth 0 indices) (row-count array))
             (if (adjustable? array)
                 (setf (row-count array) (1+ (nth 0 indices)))
                 (error "~A is not an adjustable array: max row is ~A, you said ~A"
                        array (1- (row-count array)) (nth 0 indices))))
           (if (eq value (initial-element array))
               (remhash (nth 0 indices) (matrix array))
               (setf (gethash (nth 0 indices) (matrix array)) value)))
          ((and (= 2 (length indices)) (= 2 (dimensions array)))
           (when (>= (nth 0 indices) (row-count array))
             (if (adjustable? array)
                 (setf (row-count array) (1+ (nth 0 indices)))
                 (error "~A is not an adjustable array: max row is ~A, you said ~A"
                        array (1- (row-count array)) (nth 0 indices))))
           (when (>= (nth 1 indices) (col-count array))
             (if (adjustable? array)
                 (setf (col-count array) (1+ (nth 1 indices)))
                 (error "~A is not an adjustable array: max col is ~A, you said ~A"
                        array (1- (col-count array)) (nth 1 indices))))
           (let ((table (gethash (nth 0 indices) (matrix array))))
             (unless (hash-table-p table)
               (setq table
                     (setf (gethash (nth 0 indices) (matrix array))
                           #+sbcl
                           (make-hash-table :synchronized t)
                           #-sbcl
                           (make-hash-table))))
             (if (eq value (initial-element array))
                 (remhash (nth 1 indices) table)
                 (setf (gethash (nth 1 indices) table) value)))))))

;;(defsetf saref (a &rest indices) (value)
;;  `(set-sparse-array ,a (list ,@(mapcar #'(lambda (i) i) indices)) ,value))

(defun (setf saref) (value array &rest indices)
  (set-sparse-array array indices value))

(defun incf-svector (vector index &optional (delta 1))
  (incf (gethash index (matrix vector)) delta))

(defun incf-sarray (array indices &optional (delta 1))
  (incf (apply #'saref array indices) delta))

(defun decf-svector (vector index &optional (delta 1))
  (decf (gethash index (matrix vector)) delta))

(defun decf-sarray (array indices &optional (delta 1))
  (decf (apply #'saref array indices) delta))

(defun hash-keys (ht &optional (sort-fn #'<))
  (sort (loop for k being the hash-keys in ht collecting k) sort-fn))

(defmethod non-zero-cell-count-for-row ((array sparse-array) row)
  (let ((count 0))
    (loop for j from 0 to (1- (col-count array)) do
         (when (/= 0 (saref array row j))
           (incf count)))
    count))

(defmethod non-zero-cell-count-for-col ((array sparse-array) col)
  "Assumes initial-element is 0"
  (let ((count 0))
    (loop for i from 0 to (1- (row-count array)) do
         (when (/= 0 (saref array i col))
           (incf count)))
    count))

(defmethod sum-svector ((vector sparse-array))
  (if (= 1 (dimensions vector))
      #+sbcl
      (sb-ext:with-locked-hash-table ((matrix vector))
        (loop for v being the hash-values in (matrix vector) summing v))
      #-sbcl
      (loop for v being the hash-values in (matrix vector) summing v)
      (error "Please use sum-sarray-row for 2D sparse arrays.")))

(defmethod sum-square-svector ((vector sparse-array))
  (if (= 1 (dimensions vector))
      #+sbcl
      (sb-ext:with-locked-hash-table ((matrix vector))
        (loop for v being the hash-values in (matrix vector) summing (square v)))
      #-sbcl
      (loop for v being the hash-values in (matrix vector) summing (square v))
      (error "Please use sum-sarray-row for 2D sparse arrays.")))

(defmethod sum-sarray-row ((array sparse-array) row)
  (if (= 2 (dimensions array))
      (let ((table (gethash row (matrix array))))
        (if (hash-table-p table)
            #+sbcl
            (sb-ext:with-locked-hash-table (table)
              (loop for v being the hash-values in table summing v))
            #-sbcl
            (loop for v being the hash-values in table summing v)
            0))
      (error "Please use sum-svector for 1D sparse arrays.")))

(defmethod sum-sarray-col ((array sparse-array) col)
  (if (= 2 (dimensions array))
      (let ((total 0))
        #+sbcl
        (sb-ext:with-locked-hash-table ((matrix array))
          (maphash #'(lambda (row table)
                       (declare (ignore row))
                       (incf total (gethash col table (initial-element array))))
                   (matrix array)))
        #-sbcl
        (maphash #'(lambda (row table)
                     (declare (ignore row))
                     (incf total (gethash col table (initial-element array))))
                 (matrix array))
        total)
      (error "Please use sum-svector for 1D sparse arrays.")))

(defmethod boolean-sum-sarray-col ((array sparse-array) col)
  (if (= 2 (dimensions array))
      (let ((total 0))
        #+sbcl
        (sb-ext:with-locked-hash-table ((matrix array))
          (maphash #'(lambda (row table)
                       (declare (ignore row))
                       (when (> (gethash col table (initial-element array)) 0)
                         (incf total)))
                   (matrix array)))
        #-sbcl
        (maphash #'(lambda (row table)
                     (declare (ignore row))
                     (when (> (gethash col table (initial-element array)) 0)
                       (incf total)))
                 (matrix array))
        total)
      (error "Please use sum-svector for 1D sparse arrays.")))

(defmethod sum-square-sarray-row ((array sparse-array) row)
  (if (= 2 (dimensions array))
      (let ((table (gethash row (matrix array))))
        (if (hash-table-p table)
            #+sbcl
            (sb-ext:with-locked-hash-table (table)
              (loop for v being the hash-values in table summing (square v)))
            #-sbcl
            (loop for v being the hash-values in table summing (square v))
            0))
      (error "Please use sum-svector for 1D sparse arrays.")))

(defmethod fast-map-sarray ((fn function) (array sparse-array))
  (if (= 1 (dimensions array))
      (map nil #'(lambda (key)
                   (apply fn key (gethash key (matrix array))))
           (hash-keys (matrix array)))
      (dolist (row (hash-keys (matrix array)))
        (let ((table (gethash row (matrix array))))
          (dolist (col (hash-keys table))
            (funcall fn row col (gethash col table)))))))

(defmethod map-sarray-row ((fn function) (array sparse-array) row)
  (if (= 2 (dimensions array))
      (let ((table (gethash row (matrix array))))
        (when (hash-table-p table)
          #+sbcl
          (sb-ext:with-locked-hash-table (table)
            (loop for v being the hash-values in table using (hash-key k)
                 collecting (funcall fn k v)))
          #-sbcl
          (loop for v being the hash-values in table using (hash-key k)
             collecting (funcall fn k v))))
      (error "Cannot map rows of a single dimensional array.")))

(defmethod map-sarray-col ((fn function) (array sparse-array) col)
  (if (= 2 (dimensions array))
      (let ((result nil))
        #+sbcl
        (sb-ext:with-locked-hash-table ((matrix array))
          (maphash #'(lambda (row table)
                       (multiple-value-bind (v p?)
                           (gethash col table (initial-element array))
                         (when p?
                           (push (funcall fn row v) result))))
                   (matrix array)))
        #-sbcl
        (maphash #'(lambda (row table)
                     (multiple-value-bind (v p?)
                         (gethash col table (initial-element array))
                       (when p?
                         (push (funcall fn row v) result))))
                 (matrix array))
        (nreverse result))
      (error "Cannot map columns of single dimensional arrays.")))

(defmethod map-sarray ((fn function) (array sparse-array) &key collect?)
  (let ((result nil))
    (if (= 1 (dimensions array))
        (progn
          (dotimes (i (row-count array))
            (if collect?
                (push (apply fn (saref array i)) result)
                (apply fn (saref array i))))
          (nreverse result))
        (progn
          (dotimes (i (row-count array))
            (let ((i-result nil))
              (dotimes (j (col-count array))
                (if collect?
                    (push (apply fn (saref array i j)) i-result)
                    (apply fn (saref array i j))))
              (push (nreverse i-result) result)))
          (nreverse result)))))

(defmethod print-sparse-array ((array sparse-array) &optional (stream t))
  (if (= 1 (dimensions array))
      (progn
        (format stream "#S(")
        (dotimes (i (row-count array))
          (format stream "~A" (saref array i))
          (when (/= i (1- (row-count array)))
            (format stream " ")))
        (format stream ")~%"))
        (progn
          (format stream "#S(")
          (dotimes (i (row-count array))
            (unless (= i 0)
              (format stream "   "))
            (format stream "(")
              (dotimes (j (col-count array))
                (format stream "~A" (saref array i j))
                (when (/= j (1- (col-count array)))
                  (format stream " ")))
              (format stream ")")
              (when (/= i (1- (row-count array)))
                (format stream "~%")))
          (format stream ")~%"))))

(defmethod print-sparse-array-float ((array sparse-array) &optional (stream t))
  (if (= 1 (dimensions array))
      (progn
        (format stream "#S(")
        (dotimes (i (row-count array))
          (format stream "~F" (saref array i))
          (when (/= i (1- (row-count array)))
            (format stream " ")))
        (format stream ")~%"))
        (progn
          (format stream "#S(")
          (dotimes (i (row-count array))
            (unless (= i 0)
              (format stream "   "))
            (format stream "(")
              (dotimes (j (col-count array))
                (format stream "~F" (saref array i j))
                (when (/= j (1- (col-count array)))
                  (format stream " ")))
              (format stream ")")
              (when (/= i (1- (row-count array)))
                (format stream "~%")))
          (format stream ")~%"))))

(defun dump-sparse-matrix (m file &key (dense t) (sparse nil))
  (when dense
    (with-open-file (out file
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (format out "~D ~D~%" (row-count m) (col-count m))
      (loop for i from 0 to (1- (row-count m)) do
           (loop for j from 0 to (1- (col-count m)) do
                (format out "~F " (saref m i j)))
           (format out "~%"))))
  ;; FIXME: todo sparse
  (when sparse
    nil))

(in-package :graph-utils)

(defvar *prolog-graph* nil)

(defun init-prolog (&optional graph)
  (cond ((typed-graph? graph)
         (setq *prolog-graph* graph))
        ((null graph)
         (setq *prolog-graph* (make-typed-graph)))
        (t
         (error "Please supply a typed graph"))))

(defstruct (triple
             (:constructor %make-triple)
             (:predicate triple?)
             (:conc-name nil))
  subject predicate object (weight 1))

(defun lookup-triple (s p o)
  (let ((w (edge-exists? *prolog-graph* s o :edge-type p)))
    (when (and (numberp w) (/= 0 w))
      (%make-triple :subject s
                    :predicate p
                    :object o
                    :weight w))))

(defun make-triple (subject predicate object &optional weight)
  (or (lookup-triple subject predicate object)
      (progn
        (add-node *prolog-graph* subject)
        (add-node *prolog-graph* object)
        (add-edge-type *prolog-graph* predicate)
        (add-edge *prolog-graph* subject object :edge-type predicate :weight weight)
        (%make-triple :subject subject
                      :predicate predicate
                      :object object
                      :weight weight))))

(defun add-triple (s p o &optional (w 1))
  (make-triple s p o w))

(defun delete-triple (s p o)
  (when (edge-exists? *prolog-graph* s o :edge-type p)
    (delete-edge *prolog-graph* s o p)))

(defun triple-equal (t1 t2)
  (and (funcall (comparator *prolog-graph*) (subject t1) (subject t2))
       (eql (predicate t1) (predicate t2))
       (funcall (comparator *prolog-graph*) (object t1) (object t2))))

(defun get-triples (&key s p o)
  (cond ((and s p o)
         (list (lookup-triple s p o)))
        ((and s o)
         (let ((triples nil))
           (dolist (edge-type (edge-types *prolog-graph*))
             (let ((triple (lookup-triple s edge-type o)))
               (when (triple? triple)
                 (push triple triples))))
           triples))
        ((and s p)
         (let ((matrix (gethash p (matrix *prolog-graph*)))
               (n1 (lookup-node *prolog-graph* s)))
           (when (and matrix n1)
             (map-sarray-row
              #'(lambda (k v)
                  (%make-triple :subject s
                                :predicate p
                                :object (gethash k (ids *prolog-graph*))
                                :weight v))
              matrix n1))))
        ((and p o)
         (let ((matrix (gethash p (matrix *prolog-graph*)))
               (n2 (lookup-node *prolog-graph* o)))
           (when (and matrix n2)
             (map-sarray-col
              #'(lambda (k v)
                  (%make-triple :subject (gethash k (ids *prolog-graph*))
                                :predicate p
                                :object o
                                :weight v))
              matrix n2))))
        (s
         (mapcan #'(lambda (edge-type)
                     (get-triples :s s :p edge-type))
                 (edge-types *prolog-graph*)))
        (p
         (map-edges #'(lambda (n1 n2 w edge-type)
                        (%make-triple :subject (gethash n1 (ids *prolog-graph*))
                                      :predicate edge-type
                                      :object (gethash n2 (ids *prolog-graph*))
                                      :weight w))
                    *prolog-graph* :collect? t :edge-type p))
        (o
         (mapcan #'(lambda (edge-type)
                     (get-triples :o o :p edge-type))
                 (edge-types *prolog-graph*)))
        (t
         (map-edges #'(lambda (n1 n2 w edge-type)
                        (%make-triple :subject (gethash n1 (ids *prolog-graph*))
                                      :predicate edge-type
                                      :object (gethash n2 (ids *prolog-graph*))
                                      :weight w))
                    *prolog-graph* :collect? t))))

(defun test-prolog ()
  (init-prolog)
  (add-triple "Kevin" :likes "cats")
  (add-triple "Kevin" :likes "Dustie")
  (add-triple "Kevin" :hates "dogs")
  (add-triple "Dustie" :hates "dogs")
  (add-triple "Dustie" :likes "Kevin")
  (select (?x ?y) (q- ?x :likes ?y)))

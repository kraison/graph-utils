(in-package #:graph-utils)

;; The following queueing code was borrowed and adapted from Russell & Norvig's
;; "Introduction to AI"
(defun print-queue (q stream depth)
  (format stream "~a" (queue-elements q)))

(defstruct (queue
             (:print-function print-queue))
  (key #'identity)
  (last nil)
  (elements nil))

(defun make-empty-queue () (make-queue))

(defun empty-queue? (q)
  (= (length (queue-elements q)) 0))

(defun queue-front (q)
  (elt (queue-elements q) 0))

(defun dequeue (q)
  (when (listp (queue-elements q))
    (pop (queue-elements q))))

(defun enqueue (q &rest items)
  (cond ((null items) nil)
        ((or (null (queue-last q)) (null (queue-elements q)))
         (setf (queue-last q) (last items)
               (queue-elements q) (nconc (queue-elements q) items)))
        (t (setf (cdr (queue-last q)) items
                 (queue-last q) (last items)))))

(defun queue-length (q)
  (length (queue-elements q)))
;; End of adapted code

;;;; MINHEAP is by Stephan Frank <defclass@googlemail.com>, 2007-2012.
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files (the
;;;; "Software"), to deal in the Software without restriction, including
;;;; without limitation the rights to use, copy, modify, merge, publish,
;;;; distribute, sublicense, and/or sell copies of the Software, and to
;;;; permit persons to whom the Software is furnished to do so, subject to
;;;; the following conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be included
;;;; in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;;;  Modified by Keivn Raison to easier lookup of nodes in heap.

(defpackage :fib-heap (:use :cl)
            (:export
             #:fib-heap
             #:clear-heap
             #:empty-p
             #:insert
             #:peek-min
             #:extract-min
             #:extract-node
             #:heap-size
             #:decrease-key
             #:meld
             #:lookup-node
             ))

(in-package :fib-heap)

;;;; Fibonacci heap based on CLRS, chapter 21

                                        ;(declaim (inline link consolidate splice-lists cut cascading-cut))

(defstruct (node (:constructor %make-node (key data)))
  (key 0 :type fixnum)
  (data nil)
  (parent nil :type (or null node))
  (child nil :type (or null node))
  (left nil :type (or null node))
  (right nil :type (or null node))
  (degree 0 :type (integer 0 #.(ceiling (log most-positive-fixnum
                                             (/ (1+ (sqrt 5)) 2)))))
  (mark nil :type boolean))

(defmethod print-object ((obj node) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~4I~:_key: ~A data: ~A degree: ~A mark: ~A~:_"
            (node-key obj) (node-data obj) (node-degree obj) (node-mark obj))))

                                        ;(declaim (inline %make-node make-node))

(defun make-node (key data)
  "Return a new heap node with KEY and DATA as key/data items and set
the cycle list accordingly."
  (let ((node (%make-node key data)))
    (setf (node-right node) node
          (node-left node) node)
    node))

(defun splice-lists (list-a list-b)
  "Splice two circular lists together"
  (declare (optimize (speed 3) (space 0) (debug 0)))
  (cond
    ((null list-a) list-b)
    ((null list-b) list-a)
    (t (let ((a-pred (node-left list-a))
             (b-tail (node-left list-b)))
         (setf (node-left list-a) b-tail
               (node-right b-tail) list-a
               (node-left list-b) a-pred
               (node-right a-pred) list-b)
         list-a))))


(defclass fib-heap ()
  ((min :accessor min-node
        :type (or null node)
        :initform nil)
   (node-table :accessor node-table
               :initform (make-hash-table))
   (nodes :accessor heap-size
          :type (integer 0 #.most-positive-fixnum)
          :initform 0)))

(defmethod print-object ((obj fib-heap) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~4I~:_size: ~A~:_" (heap-size obj))))

(defun clear-heap (heap)
  (setf (min-node heap) nil
        (heap-size heap) 0)
  (clrhash (node-table heap))
  heap)

(defun cut (heap x y)
  "Remove X from the child list of Y."
  (declare (optimize (speed 3) (space 0) (debug 0))
           (type node x y))
  (let ((min (min-node heap)))
    (setf (node-right (node-left x)) (node-right x)
          (node-left (node-right x)) (node-left x))
    (if (zerop (the fixnum (decf (node-degree y))))
        (setf (node-child y) nil)
        (when (eq (node-child y) x)
          (setf (node-child y) (node-right x))))
    (setf (node-right x) min
          (node-left x) (node-left min)
          (node-left min) x
          (node-right (node-left x)) x
          (node-parent x) nil
          (node-mark x) nil)))


(defun cascading-cut (heap cnode)
  (declare (optimize (speed 3) (space 0) (debug 0))
           (type node cnode))
  (loop for node = cnode then parent
     for parent = (node-parent node)
     while parent do (if (node-mark node)
                         (cut heap node parent)
                         (progn
                           (setf (node-mark node) t)
                           (loop-finish)))))

(defun link (y x)
  "Make node Y a child of node X."
  (declare (optimize (speed 3) (space 0) (debug 0)))
                                        ;(assert (not (eq x y)))
  (setf (node-right (node-left y)) (node-right y)
        (node-left (node-right y)) (node-left y)
        (node-left y) y
        (node-right y) y
        (node-child x) (splice-lists (node-child x) y)
        (node-parent y) x
        (node-mark y) nil)
  (incf (node-degree x)))

(defun consolidate (heap)
  (declare (optimize (speed 3) (space 0) (debug 0)))
  ;; The array size constant ist $log_{phi}(m-p-f)$ where
  ;; most-positive-fixnum represents the largest number of elements we
  ;; are able to hold. The resulting constant is the largest degree of
  ;; any root list and thus our max. necessary array size which we
  ;; avoid to recalculate every time we call consolidate.
  (let ((A (make-array #.(ceiling (log most-positive-fixnum (/ (1+ (sqrt 5)) 2)))
                       :initial-element nil
                       :element-type '(or null node)))
        (w (min-node heap)))
    (declare (dynamic-extent A))
    ;; For each root list node search for others of the same degree
    (loop for next-w = (node-right w)
       for x = w
       for d fixnum = (node-degree x)
       with start = w
       with max-d fixnum = 0 ; use max-d to minimise the array scan length
       do (do ((y (aref A d) (aref A d)))
              ((null y))
            (when (< (node-key y) (node-key x))
              (rotatef x y))
            (when (eq y start)
              (setf start (node-right start)))
            (when (eq y next-w)
              (setf next-w (node-right next-w)))
            (link y x)
            (setf (aref A d) nil)
            (incf d))
         (setf (aref A d) x
               max-d (max max-d d))
       until (eq (setf w next-w) start)
       finally ;; find minimum key and its corresponding node again
         (loop for node across A
            for i to max-d
            with min = start
            while (<= i max-d)
            do (when (and node
                          (< (node-key node)
                             (node-key min)))
                 (setf min node))
            finally (setf (min-node heap) min)))))

(defun empty-p (heap)
  "Return NIL if HEAP is empty, otherwise the minnimal node."
  (zerop (heap-size heap)))

(defun insert (heap key data)
    "Insert a new node with KEY and associated DATA item into the HEAP
root-list. No consolidation is done at this time."
    (let ((node (make-node key data))
          (min (min-node heap)))
      (setf (gethash data (node-table heap)) node)
      (incf (heap-size heap))
      (cond
        (min
         (splice-lists min node)
         (if (< key (node-key min))
             (setf (min-node heap) node)
             node))
        (t
         (setf (min-node heap) node)))))

(defun peek-min (heap)
  (let ((min (min-node heap)))
    (when min
      (values (node-data min)
              (node-key min)))))

(defun extract-min (heap)
  (let ((min (min-node heap)))
    (when min
      (when (node-child min)
        (setf (node-parent (node-child min)) nil)
        (loop for x = (node-right (node-child min)) then (node-right x)
           until (eq x (node-child min))
           do (setf (node-parent x) nil))
        (setf (min-node heap)
              (splice-lists (min-node heap) (node-child min))))
      (setf (node-right (node-left min)) (node-right min)
            (node-left (node-right min)) (node-left min))
      (cond
        ((eq min (node-right min))
         (setf (min-node heap) nil))
        (t
         (setf (min-node heap) (node-right min))
         (consolidate heap)))
      (setf (node-parent min) nil
            (node-left min) nil
            (node-right min) nil)
      (decf (heap-size heap))
      (remhash (node-data min) (node-table heap))
      (values (node-data min)
              (node-key min)))))

(defun decrease-key (heap data key)
  (let ((node (gethash data (node-table heap))))
    (when (< (node-key node) key)
      (error "Cannot decrease key: new key greater than current key."))
    (let ((y (node-parent node)))
      (setf (node-key node) key)
      (when (and y (< key (node-key y)))
        (cut heap node y)
        (cascading-cut heap y))
      (if (< key (node-key (min-node heap)))
          (setf (min-node heap) node)
          node))))

(defun extract-node (heap node)
  (let ((key (node-key node))
        (value (node-data node)))
    (decrease-key heap value most-negative-fixnum)
    (extract-min heap)
    (values value key)))

(defun lookup-node (heap data)
  (let ((node (gethash data (node-table heap))))
    (when node
      (node-key node))))

(defun meld (heap-a heap-b)
    "Melds HEAP-A and HEAP-B into a new heap and returns it. HEAP-A and
HEAP-B will be empty after this operation but may be used further."
    (let ((heap (make-instance 'fib-heap)))
      (setf (min-node heap) (splice-lists (min-node heap-a)
                                          (min-node heap-b)))
      (when (and (min-node heap-a)
                 (min-node heap-b)
                 (< (node-key (min-node heap-b))
                    (node-key (min-node heap-a))))
        (setf (min-node heap) (min-node heap-b)))
      (setf (heap-size heap) (+ (heap-size heap-a) (heap-size heap-b))
            (min-node heap-a) nil
            (min-node heap-b) nil
            (heap-size heap-a) 0
            (heap-size heap-b) 0)
      heap))

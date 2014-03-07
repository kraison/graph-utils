(in-package #:graph-utils)

(defparameter *graph-utils-debug* t)
;; Prolog specials
(defparameter *occurs-check* t)
(defvar *trail* (make-array 200 :fill-pointer 0 :adjustable t))
(defvar *var-counter* 0 "Counter for generating variable names.")
(defvar *functor* nil "The Prolog functor currently being compiled.")
(defvar *select-list* nil "Accumulator for prolog selects.")
(defvar *cont* nil "Continuation container for step-wise queries.")
#+sbcl
(defvar *prolog-global-functors* (make-hash-table :synchronized t))
#-sbcl
(defvar *prolog-global-functors* (make-hash-table))
#+sbcl
(defvar *user-functors* (make-hash-table :synchronized t :test 'eql))
#-sbcl
(defvar *user-functors* (make-hash-table :test 'eql))
(defparameter *prolog-trace* nil)
(defconstant +unbound+ :unbound)
(defconstant +no-bindings+ '((t . t)))
(defconstant +fail+ nil)

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

(defun dbg (control &rest args)
  "Debug output function"
  (when *graph-utils-debug*
    (apply #'format t control args)
    (terpri)))

(defun sum (list)
  "Sum a list of numbers"
  (apply #'+ list))

(defun square (x)
  "Square a number"
  (* x x))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun reuse-cons (x y x-y)
  "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
      (cons x y)))

(defun new-interned-symbol (&rest args)
  "Concatenate symbols or strings to form an interned symbol"
  (intern (format nil "~{~a~}" args)))

(defun new-symbol (&rest args)
  "Concatenate symbols or strings to form an uninterned symbol"
  (make-symbol (format nil "~{~a~}" args)))

(defun length=1 (list)
  "Is this a list of exactly one element?"
  (and (consp list) (null (cdr list))))

(defun proper-listp (x)
  "Is x a proper (non-dotted) list?"
  (or (null x)
      (and (consp x) (proper-listp (rest x)))))

(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
  according to the keywords.  Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))

(defun find-anywhere (item tree)
  "Does item occur anywhere in tree?  If so, return it."
  (cond ((eql item tree) tree)
        ((atom tree) nil)
        ((find-anywhere item (first tree)))
        ((find-anywhere item (rest tree)))))

(defun unique-find-anywhere-if (predicate tree &optional found-so-far)
  "return a list of leaves of tree satisfying predicate, with duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
          (adjoin tree found-so-far)
          found-so-far)
      (unique-find-anywhere-if
       predicate
       (first tree)
       (unique-find-anywhere-if predicate (rest tree) found-so-far))))

(defun find-if-anywhere (predicate tree)
  "Does predicate apply to any atom in the tree?"
  (if (atom tree)
      (funcall predicate tree)
      (or (find-if-anywhere predicate (first tree))
          (find-if-anywhere predicate (rest tree)))))

(defun continue-p ()
  "Ask user if we should continue looking for solutions."
  (case (read-char)
    (#\; t)
    (#\. nil)
    (#\newline (continue-p))
    (otherwise
      (format t " Type ; to see more or . to stop")
      (continue-p))))

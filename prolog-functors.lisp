(in-package #:graph-utils)

(defvar *prolog-global-functors*
  (make-hash-table :synchronized t :test 'equalp))

(defmacro def-global-prolog-functor (name lambda-list &body body)
  `(prog1
       (defun ,name ,lambda-list ,@body)
     (export ',name)
     (setf (gethash ',name *prolog-global-functors*) #',name)))

(defun default-functor? (symbol)
  (gethash symbol *prolog-global-functors*))

(def-global-prolog-functor read/1 (exp cont)
  (if (unify exp (read)) (funcall cont)))

(def-global-prolog-functor write/1 (exp cont)
  (format t "~A" (deref-exp exp)) (funcall cont))

(def-global-prolog-functor nl/0 (cont)
  (terpri) (funcall cont))

(def-global-prolog-functor repeat/0 (cont)
  (loop (funcall cont)))

(def-global-prolog-functor fail/0 (cont)
  (declare (ignore cont))
  nil)

(def-global-prolog-functor =/2 (?arg1 ?arg2 cont)
  "Unifies two prolog variables."
  (if (unify ?arg1 ?arg2) (funcall cont)))

(def-global-prolog-functor ==/2 (?arg1 ?arg2 cont)
  "Checks equality of the values of two prolog variables."
  (if (deref-equal ?arg1 ?arg2) (funcall cont)))

(def-global-prolog-functor /=/2 (?arg1 ?arg2 cont)
  "Checks inequality of the values of two prolog variables."
  (if (not (deref-equal ?arg1 ?arg2)) (funcall cont)))

(def-global-prolog-functor >/2 (?arg1 ?arg2 cont)
  "Prolog greater than functor."
  (if (and (numberp (var-deref ?arg1)) (numberp (var-deref ?arg2))
           (> ?arg1 ?arg2))
      (funcall cont)))

(def-global-prolog-functor </2 (?arg1 ?arg2 cont)
  "Prolog less than functor."
  (if (and (numberp (var-deref ?arg1)) (numberp (var-deref ?arg2))
           (< ?arg1 ?arg2))
      (funcall cont)))

(def-global-prolog-functor >=/2 (?arg1 ?arg2 cont)
  "Prolog greater than or equal to functor."
  (if (and (numberp (var-deref ?arg1))
           (numberp (var-deref ?arg2))
           (>= ?arg1 ?arg2))
      (funcall cont)))

(def-global-prolog-functor <=/2 (?arg1 ?arg2 cont)
  "Prolog less than or equal to functor."
  (if (and (numberp (var-deref ?arg1))
           (numberp (var-deref ?arg2))
           (<= ?arg1 ?arg2))
      (funcall cont)))

;(def-global-prolog-functor member/2 (?item list cont)
;  (var-deref ?item)
;  (when (and (listp list)
;	     (member ?item list
;                    :test #'(lambda (x y) (var-deref y) (prolog-equal x y))))
;    (funcall cont)))

(def-global-prolog-functor lisp/2 (?result exp cont)
  "Call out to lisp from within a Prolog query.  Assigns result to the
 supplied Prolog var.  (lisp ?result (+ 1 2)).  Any lisp variables that you
 wish to access within a prolog query using the lisp functor should be
 declared special."
  (let ((exp (var-deref exp)))
    (when *prolog-trace* (format t "TRACE: LISP/2 ?result <- ~A~%" exp))
    (cond ((consp exp)
	   (if (unify ?result (eval exp))
	   ;;(if (unify ?result (apply (first exp) (rest exp)))
	       (funcall cont)))
	  ((and (symbolp exp) (boundp exp))
	   ;;(if (unify ?result (eval exp))
	   (if (unify ?result (funcall #'symbol-value exp))
	       (funcall cont)))
	  (t
	   (if (unify ?result exp)
	       (funcall cont))))))

(def-global-prolog-functor lispp/1 (exp cont)
  "Call out to lisp from within a Prolog query and throws away the result.
 Any lisp variables that you wish to access within a prolog query using the
 lisp functor should be declared special."
  (let ((exp (var-deref exp)))
    (when *prolog-trace* (format t "TRACE: LISPP/1 ~A~%" exp))
    (cond ((consp exp)
	   ;;(format t "applying ~A to ~A~%" (first exp) (rest exp))
	   (eval exp))
	   ;;(apply (first exp) (rest exp)))
	  ((and (symbolp exp) (boundp exp)) (funcall #'identity exp))
	  (t exp))
    (funcall cont)))

(def-global-prolog-functor regex-match/2 (?arg1 ?arg2 cont)
  "Functor that treats first arg as a regex and uses cl-ppcre:scan to check
 for the pattern in the second arg."
  (if (and (stringp (var-deref ?arg1))
	   (stringp (var-deref ?arg2))
	   (cl-ppcre:scan ?arg1 ?arg2))
      (funcall cont)))

(def-global-prolog-functor var/1 (?arg1 cont)
  (if (unbound-var-p ?arg1) (funcall cont)))

(def-global-prolog-functor is/2 (var exp cont)
  "Similar to lisp/2, but unifies instead of assigns the lisp return value."
  (if (and (not (find-if-anywhere #'unbound-var-p exp))
	   (unify var (eval (deref-exp exp))))
      (funcall cont)))

(def-global-prolog-functor call/1 (goal cont)
  "Call a prolog form."
  (var-deref goal)
  (let* ((functor (make-functor-symbol (first goal) (length (args-of goal)))))
    (let ((fn (or (gethash functor *user-functors*)
		  (gethash functor *prolog-global-functors*))))
      (if (functionp fn)
	  (apply fn (append (args-of goal) (list cont)))
	  (error 'prolog-error
		 :reason
		 (format nil "Unknown Prolog functor in call/1 ~A"
                         functor))))))

(def-global-prolog-functor if/2 (?test ?then cont)
  (when *prolog-trace* (format t "TRACE: IF/2(~A ~A)~%" ?test ?then))
  (call/1 ?test #'(lambda () (call/1 ?then cont))))

(def-global-prolog-functor if/3 (?test ?then ?else cont)
  (when *prolog-trace* (format t "TRACE: IF/3(~A ~A ~A)~%" ?test ?then ?else))
  (call/1 ?test #'(lambda ()
		    (call/1 ?then
			    #'(lambda () (funcall cont) (return-from if/3)))))
  (call/1 ?else cont))

(let ((date-regex
       "^(19|20)\\d\\d[\-\ \/\.](0[1-9]|1[012])[\-\ \/\.](0[1-9]|[12][0-9]|3[01])$"))
  (def-global-prolog-functor valid-date?/1 (date cont)
    "Date validation functor. FIXME: This needs to be fleshed out with a
 more comprehensive regex."
    (var-deref date)
    (if (and (stringp date)
             (cl-ppcre:scan date-regex date))
        (funcall cont))))

(def-global-prolog-functor trigger/1 (exp cont)
  "Call out to lisp ignoring the return value."
  (eval (deref-exp exp))
  ;;(let ((exp (deref-exp exp)))
    ;;(typecase exp
      ;;(cons   (apply (first exp) (rest exp)))
      ;;(symbol (symbol-value exp))))
  (funcall cont))

(def-global-prolog-functor not/1 (relation cont)
  "Prolog negation.  Does not retract, simply negates in the context of the
query."
  (with-undo-bindings
    (call/1 relation #'(lambda () (return-from not/1 nil)))
    (funcall cont)))

(def-global-prolog-functor or/2 (goal1 goal2 cont)
  (call/1 goal1 #'(lambda () (funcall cont) (return-from or/2 t)))
  (call/1 goal2 #'(lambda () (funcall cont) (return-from or/2 t))))

(def-global-prolog-functor bagof/3 (exp goal result cont)
  (let ((answers nil))
    (call/1 goal #'(lambda () (push (deref-exp exp) answers)))
    (if (and (not (null answers))
	     (unify result (nreverse answers)))
	(funcall cont))))

(def-global-prolog-functor setof/3 (exp goal result cont)
  "Find all unique solutions to GOAL, and for each solution,
  collect the value of EXP into the list RESULT."
  ;; Ex: Assume (p 1) (p 2) (p 3).  Then:
  ;;     (setof ?x (p ?x) ?l) ==> ?l = (1 2 3)
  (let ((answers nil))
    (call/1 goal #'(lambda ()
                     (push (deref-exp exp) answers)))
    (if (and (not (null answers))
             (unify result (delete-duplicates
                            answers
                            :test #'deref-equal)))
        (funcall cont))))

(def-global-prolog-functor show-prolog-vars/2 (var-names vars cont)
  (if (null vars)
      (format t "~&Yes")
      (loop for name in var-names
	 for var in vars do
	   (format t "~&~a = ~a" name (deref-exp var))))
  (if (continue-p)
      (funcall cont)
      (throw 'top-level-prove nil)))

(let ((graph-pkg (find-package :graph-utils)))
  (def-global-prolog-functor select/2 (var-names vars cont)
    (if (null vars)
	nil
	(push (loop for name in var-names
		 for var in vars
		 collect (let ((var (deref-exp var)))
			   (cond ((and (symbolp var)
				       (eq graph-pkg (symbol-package var)))
				  (symbol-name var))
				 ((and (consp var)
				       (eq (first var) name)
				       (symbolp (second var))
				       (eq graph-pkg
					   (symbol-package (second var))))
				  (list name (symbol-name (second var))))
				 (t var))))
	      *select-list*))
    (funcall cont))

  (def-global-prolog-functor map-query/3 (fn vars collect? cont)
    (when *prolog-trace*
      (format t "TRACE: MAP-QUERY/3 FN (~A) IS ~A~%COLLECT? is ~A~%"
	      (type-of fn) fn collect?))
    (if (null vars)
	nil
	(let ((new-vars
	       (loop
		  for var in vars
		  collect (let ((v (deref-exp var)))
			    (if (and (symbolp v)
				     (eq graph-pkg (symbol-package v)))
				(symbol-name v)
				v)))))
	  (let ((result (eval `(apply ,fn ',new-vars))))
	    (if collect? (push result *select-list*)))))
    (funcall cont)))

(def-global-prolog-functor q-/3 (s p o cont)
  (when *prolog-trace* (format t "TRACE: Q-/3(~A ~A ~A)~%" s p o))
  (let ((triples
	 (get-triples
	  :p (and (or (not (var-p p))
                      (and (var-p p) (bound-p p)))
                  (var-deref p))
	  :s (and (or (not (var-p s))
                      (and (var-p s) (bound-p s)))
                  (var-deref s))
	  :o (or (and (not (consp o))
                      (or (not (var-p o)) (and (var-p o) (bound-p o)))
		      (var-deref o))
		 (and (consp o) (cdr o))))))
    (dolist (triple triples)
      (let ((old-trail (fill-pointer *trail*)))
        (when (triple? triple)
          (when (unify p (predicate triple))
            (when (unify s (subject triple))
              (if (consp o)
                  (when (unify (car o)
                               (object triple))
                    (funcall cont))
                  (when (unify o (object triple))
                    (funcall cont)))))
          (undo-bindings old-trail))))))

(def-global-prolog-functor q-/4 (s p o w cont)
  (when *prolog-trace* (format t "TRACE: Q-/4(~A ~A ~A ~A)~%" s p o w))
  (let ((triples
	 (get-triples
	  :p (and (or (not (var-p p))
                      (and (var-p p) (bound-p p)))
                  (var-deref p))
	  :s (and (or (not (var-p s))
                      (and (var-p s) (bound-p s)))
                  (var-deref s))
	  :o (or (and (not (consp o))
                      (or (not (var-p o)) (and (var-p o) (bound-p o)))
		      (var-deref o))
		 (and (consp o) (cdr o))))))
    (dolist (triple triples)
      (let ((old-trail (fill-pointer *trail*)))
        (when (triple? triple)
          (when (unify p (predicate triple))
            (when (unify s (subject triple))
              (if (consp o)
                  (when (unify (car o)
                               (object triple))
                    (when (unify w (weight triple))
                      (funcall cont)))
                  (when (unify o (object triple))
                    (when (unify w (weight triple))
                      (funcall cont))))))
          (undo-bindings old-trail))))))

(def-global-prolog-functor assert/1 (clause cont)
  "Add a triple to the datastore."
  (when (consp clause)
    (setq clause (mapcar #'(lambda (c) (var-deref c)) clause))
    (when *prolog-trace* (format t "TRACE: Asserting ~A~%" clause))
    (if (and (or (= 3 (length clause))
		 (= 4 (length clause)))
	     (not (some #'var-p clause)))
	(let ((triple (add-triple (first clause)
                                  (second clause)
                                  (third clause)
				  (or (fourth clause) 1))))
	  (when *prolog-trace*
	    (format t "TRACE: Asserted new triple ~A~%" triple))
	  (when (triple? triple)
	    (funcall cont)))
	(error 'prolog-error
	       :reason
	       (format nil "assert is only for triples, not ~A" clause)))))

(def-global-prolog-functor subject/2 (?arg1 ?arg2 cont)
  (when (and (triple? ?arg2) (unify ?arg1 (subject ?arg2)))
    (funcall cont)))

(def-global-prolog-functor predicate/2 (?arg1 ?arg2 cont)
  (when (and (triple? ?arg2) (unify ?arg1 (predicate ?arg2)))
    (funcall cont)))

(def-global-prolog-functor object/2 (?arg1 ?arg2 cont)
  (when (and (triple? ?arg2) (unify ?arg1 (object ?arg2)))
    (funcall cont)))

(def-global-prolog-functor weight/2 (?arg1 ?arg2 cont)
  (cond ((and (triple? ?arg2) (unify ?arg1 (weight ?arg2)))
	 (funcall cont))
	((listp ?arg2)
	 (let ((triple (lookup-triple (var-deref (nth 0 ?arg2))
				      (var-deref (nth 1 ?arg2))
				      (var-deref (nth 2 ?arg2)))))
	   (when (and (triple? triple) (unify ?arg1 (weight triple)))
	     (funcall cont))))))

(def-global-prolog-functor retract/1 (clause cont)
  "Retract a fact from the datastore."
  (when (consp clause)
    (setq clause (mapcar #'(lambda (c) (var-deref c)) clause))
    (if (and (or (= (length clause) 3)
		 (= (length clause) 4))
	     (not (some #'var-p clause)))
	(handler-case
            (progn
	      (when *prolog-trace*
		(format t "TRACE: Retracting fact ~A~%" clause))
	      (let ((triple (lookup-triple (first clause)
                                           (second clause)
					   (third clause))))
		(if (triple? triple)
		    (delete-triple triple)
		    (error 'prolog-error
			   :reason
			   (format nil "clause ~A does not represent a fact"
				   clause)))))
	  (prolog-error (condition)
	    (error 'prolog-error
		   :reason
		   (format nil "Cannot retract ~A: ~A~%" clause condition)))
	  (:no-error (result)
	    (declare (ignore result))
	    (funcall cont)))
	(error 'prolog-error
	       :reason
	       (format nil "Cannot retract a clause with variables: ~A"
		       clause)))))

(def-global-prolog-functor is-valid/1 (item cont)
  "Mark a triple as VALID and remove an INVALID marker."
  (var-deref item)
  (let ((triple (lookup-triple item :has-property "invalid")))
    (when (triple? triple)
      (delete-triple triple)))
  (and (add-triple item :has-property "valid")
       (funcall cont)))

(def-global-prolog-functor is-valid?/1 (item cont)
  "Ask if a triple is valid."
  (var-deref item)
  (let ((triple (lookup-triple item :has-property "valid")))
    (when (triple? triple)
      (funcall cont))))

(def-global-prolog-functor is-invalid/1 (item cont)
  "Mark a triple as INVALID and remove a VALID marker."
  (var-deref item)
  (let ((triple (lookup-triple item :has-property "valid")))
    (when (triple? triple)
      (delete-triple triple)))
  (and (add-triple item :has-property "invalid")
       (funcall cont)))

(def-global-prolog-functor is-invalid?/1 (item cont)
  "Ask if a triple is invalid."
  (var-deref item)
  (let ((triple (lookup-triple item :has-property "invalid")))
    (when (triple? triple)
      (funcall cont))))

(def-global-prolog-functor numberp/1 (x cont)
  (when (numberp (var-deref x))
    (funcall cont)))

(def-global-prolog-functor atom/1 (x cont)
  (when (atom (var-deref x))
    (funcall cont)))

#|
(defmethod reify (node)
  (declare (special node))
  (select (?p ?o)
	  (lisp ?s node)
	  (q- ?s ?p ?o)))

(defun reify-recursive (node &key (max-levels 2) (level 0))
  (unless (>= level max-levels)
    (let ((relations (reify node)))
      (list node
	    (mapcar #'(lambda (relation)
			(if (anonymous? (second relation))
			    (nconc (list (first relation))
				   (reify-recursive (second relation)
						    :max-levels max-levels
						    :level (1+ level)))
			    relation))
		    relations)))))
|#

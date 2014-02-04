;;;-*- Mode: Lisp; Package: NONLIN -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Common Lisp NONLIN - University of Maryland at College Park 
;;;;
;;;; Version 1.2, 11/91
;;;; (email nonlin-users-request@cs.umd.edu for more info)
;;;; 
;;;; UTIL.LISP: Utility routines 
;;;;
;;;; History:

(in-package :nonlin)

; some utility macros
; since these are supposed to be general
; Both then and else are made into
; progns--these allos convenient "if" expressions

(defmacro then (&rest body)
    `(progn ,@body))

(defmacro then-let (&rest body)
    `(let ,@body))
        
(defmacro else-let (&rest body)
    `(let ,@body))    
    
(defmacro else (&rest body)
    `(progn ,@body))
        

(defmacro append1 (list onestuff)
    `(append ,list (list ,onestuff)))
    ;implements append1


(eval-when (load compile)
(defun var-form-p (x)
  (and (consp x) (= (length x) 3)
       (eq (cadr x) :in))))

(eval-when (load compile)
(defun var-form-var (x) (car x)))

(eval-when (load compile)
(defun var-form-arg (x) (caddr x)))
        
(eval-when (load compile)
(defun for-vars (l)
  (mapcan #'(lambda (x)
	      (if (var-form-p x)
		  (list (var-form-var x))
		  nil))
	  l)))

(eval-when (load compile)
(defun for-test (l)
  (cadr (for-item '(:when) l))))

(eval-when (load compile)
(defun for-type (l)
  (let ((item (for-item
		'(:do :save :splice :filter)  l)))
    (cond (item (car item))
	  (t (error "no body in for-loop"))))))

(eval-when (load compile)
(defun for-item (keywords l)
  (some #'(lambda (key) (member key l)) keywords)))

(eval-when (load compile)
(defun make-mapfn (vars test type body)
  (cond ((eq type :do) 'mapc)
	((not (eq type :save)) 'mapcan)
	((null test) 'mapcar)
	((use-remove-if-not-p vars body)
	 'remove-if-not)
	(t 'mapcan))))

(eval-when (load compile)
(defun use-remove-if-not-p (vars body)
  (and (= (length vars) 1) (equal (car vars) body))))

(eval-when (load compile)
(defun make-body (vars test type body)
  (cond ((eq type :filter)
	 `(let ((x ,body )) (if x (list x) nil)))
	((or (not (eq type :save)) (null test))
	 body)
	((use-remove-if-not-p vars body) nil)
	(t `(list ,body)))))

(eval-when (load compile)
(defun add-test (test body)
  (cond ((null test) body)
	((null body) test)
	(t `(if ,test ,body nil)))))

;;;this is form pp 446.
;;; the comment is: this is still not foolproof (lookup macrolet in the
;;; common-lisp manual), but it will cover most of the simple cases
(eval-when (load compile)
(defun make-lambda (vars body)
  (if (and (consp body)
	   (equal vars (cdr body))
	   (not (CLTL1:special-form-p (car body))) ;Added package spec 14jan99
		(not (macro-function (car body))))
      (car body)
      `(lambda ,vars ,body))))

(eval-when (load compile)
(defun for-args (l)
  (mapcan #'(lambda (x)
	      (if (var-form-p x)
		  (list (var-form-arg x))
		  nil))
	  l)))

(eval-when (load compile)
(defun for-body (l)
  (let ((item (for-item
		'(:do :save :splice :filter)
		l)))
    (cond (item (if (eql (length (cdr item)) 1)
		    ;;only one statement in the body
		    (cadr item)
		    ;;return just that statement
		    `(progn ,@(cdr item))
		    ;;else wrap the statements in a progn
		    ))
	  (t (error "No body in FOR-loop"))))))

(defmacro for-all (&rest l)
  (let ((vars (for-vars l))
	(args (for-args l))
	(body (unmarked-body l)))
    `(every #',(make-lambda vars body) ,@args)))

(defmacro there-exists (&rest l)
  (let ((vars (for-vars l))
	(args (for-args l))
	(body (unmarked-body l)))
    `(some #',(make-lambda vars body) ,@args)))

(eval-when (load compile)
(defun unmarked-body (l)
  (cond ((null l) nil)
	((var-form-p (car l)) (unmarked-body (cdr l)))
	((null (cdr l)) (car l)))))

;;; the FOR macro from charaniak and mcdermott
(defmacro for (&rest l)
  (let ((vars (for-vars l))
	(args (for-args l))
	(test (for-test l))
	(type (for-type l))
	(body (for-body l)))
    `(,(make-mapfn vars test type body)
      #',(make-lambda vars
		      (add-test test
			       (make-body vars test type body)))
      ,@args)))


(defmacro do-or (&rest list-of-forms)
    (let ((do-or-block-name (gentemp "do-or")) do-or-body final-form)
;;constructing the body
      (setq list-of-forms (reverse list-of-forms))
      (setq final-form  (pop list-of-forms))
      (for (form :in list-of-forms)
	   :do (push `(let ((result ,form))
			(unless (fail-p result)
			  (return-from ,do-or-block-name result))
			)
		     do-or-body
		     ))
      `(block ,do-or-block-name
	 ,@do-or-body (return-from ,do-or-block-name ,final-form))
      ;;final form is evaluated directly and its result sent back
      ;;so that nested do-ors will work correctly
      ))

(defmacro do-and (&rest list-of-forms)
    (let ((do-and-block-name (gentemp "do-and")) do-and-body final-form)
;;constructing the body
      (setq list-of-forms (reverse list-of-forms))
      (setq final-form  (pop list-of-forms))
      (for (form :in list-of-forms)
	   :do (push `(let ((result ,form))
			(if (fail-p result)
			  (return-from ,do-and-block-name result))
			)
		     do-and-body
		     ))
      `(block ,do-and-block-name
	 ,@do-and-body (return-from ,do-and-block-name ,final-form))
      ;;final form is evaluated directly and its result sent back
      ;;so that nested do-ands will work correctly
      ))

(defun fail ()
     '*fail*)
(defun fail-p (value)
    (eq value '*fail*))
(defconstant  *fail* '*fail
    "used by do-and"
)
    
(defmacro dremove (item place &rest keys)
  ;;for destructive removal
  `(setf ,place (remove ,item ,place ,@keys)))
    

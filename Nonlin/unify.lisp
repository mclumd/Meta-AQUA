;;;-*- Mode: Lisp; Package: NONLIN -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;    Application:  NONLIN
;;;;  Appl. Version:  1.3
;;;;     Appl. Date:  Oct. 1992
;;;;
;;;;           File:  UNIFY.LISP
;;;;       Contents:  Unification routines.
;;;;
;;;;                  Dept. of Computer Science, University of Maryland at College Park
;;;;                  email: nonlin-users-request@cs.umd.edu
;;;;
;;;;       Language:  Macintosh Common Lisp 2.0
;;;;
;;;;   Requirements:  package NONLIN 
;;;;
;;;;  History:
;;;;  Date      Who  Modification
;;;;  ---------------------------------------------------------------------------------   

(in-package :nonlin)

;;; From Charniak & McDermott (see pp. ??? for documentation)

(defstruct (pcvar (:print-function print-pcvar)) id)
(defvar *assertions* nil " list of dumb assertions")
(defvar *occurs-check-p* t)
(defvar *retrieval-methods* (make-hash-table))
(defvar *storage-methods* (make-hash-table))

(defun print-pcvar (var stream depth)
  (declare (ignore depth))
  (format stream "!~s" (pcvar-id var))
  )

(set-macro-character #\!
		     #'(lambda (stream char)
			       (declare (ignore char))
			 (make-pcvar :id (read stream t nil t)))
t)

    

(defun unify (pat1 pat2)
  (unify-1 pat1 pat2 nil))

(defun unify-1 (pat1 pat2 sub)
  (cond ((pcvar-p pat1)
	 (var-unify pat1 pat2 sub))
	((pcvar-p pat2)
	 (var-unify pat2 pat1 sub))
	((atom pat1)
	 (cond ((eql pat1 pat2) (list sub))
	       (t nil)))
	((atom pat2) nil)
	(t (for (sub :in (unify-1 (car pat1) (car pat2) sub))
		:splice
		(unify-1 (cdr pat1) (cdr pat2) sub)))))

(defun var-unify (pcvar pat sub)
  (cond ((or (eql pcvar pat)
	     (and (pcvar-p pat)
		  (eql (pcvar-id pcvar)
		       (pcvar-id pat))))
	 (list sub))
	(t (let ((binding (pcvar-binding pcvar sub)))
	     (cond (binding
		    (unify-1 (binding-value binding)
			     pat sub))
		   ((and *occurs-check-p*
			 (occurs-in-p pcvar pat sub))
		    nil)
		   (t
		    (list (extend-binding pcvar pat sub))
		    ))))))



(defun occurs-in-p (pcvar pat sub)
  (cond ((pcvar-p pat)
	 (or (eq (pcvar-id pcvar)(pcvar-id pat))
	     (let ((binding (pcvar-binding pat sub)))
	       (and binding
		    (occurs-in-p pcvar
				 (binding-value binding) sub)))))
	((atom pat) nil)
	(t (or (occurs-in-p pcvar (car pat) sub)
	       (occurs-in-p pcvar (cdr pat) sub)))))	;

(defun pcvar-binding (pcvar alist)
  (assoc (pcvar-id pcvar) alist))

(defun extend-binding (pcvar pat alist)
  (cons (list (pcvar-id pcvar) pat)
	alist))

(defun binding-value (binding) (cadr binding))

;;;from pp 463
;;;this wont be used unless we are doing backward chaining or
;;;doing "and" type retrievals (for (on ?x ?y)&(on ?y ?z)&(on ?z ?w) type)


(defun replace-variables (pat sub)
  (cond ((pcvar-p pat) (pcvar-value pat sub))
	((atom pat) pat)
	(t (cons (replace-variables (car pat) sub)
		 (replace-variables (cdr pat) sub)))))


(defun pcvar-value (pat sub)
  (let ((binding (pcvar-binding pat sub)))
    (cond ((null binding) pat)
	  (t (let ((value (binding-value binding)))
	       (cond ((eql value pat) pat)
		     (t (safe-replace-variables value sub)
			)))))))


;the retrieval section
(defun safe-replace-variables (pat sub)
  (let* ((old-renames (rename-list pat))
	 (new-pat (replace-variables pat sub))
	 (new-renames (rename-list new-pat)))
    (rename-variables new-pat
		      (for (pair :in new-renames)
			   :when (not (assoc (car pair) old-renames))
			   :save pair))))

(defun store-pattern (pat &rest keys)
  (let ((safe-pat (uniquify-variables pat))
	(method (and (consp pat)
		     (storage-method
		       (pattern-head pat)))))
    (cond (method (apply method safe-pat keys))
	  ((not
	     (there-exists
	       (assertion :in (eval `(dumb-fetch ',safe-pat ,@keys)))
	       (variants-p safe-pat assertion)))
	   (eval `(dumb-index ',safe-pat ,@keys))))
    safe-pat))


(defmacro store-pat (pat &rest keys)
  ;;this is so that the place is not evaluated
   `(apply #'store-pattern ,pat ',keys))


(defun variants-p (pat1 pat2)
  (let ((alist '()))
    (labels ((variant-ids-p (id1 id2)
			    (let ((entry1 (assoc id1 alist))
				  (entry2 (rassoc id2 alist)))
			      (if (and (null entry1)
				       (null entry2))
				  (push (cons id1 id2) alist))
			      (eq entry1 entry2)))
	     (test (pat1 pat2)
		   (or (and (pcvar-p pat1)
			    (pcvar-p pat2)
			    (variant-ids-p
			      (pcvar-id pat1)
			      (pcvar-id pat2)))
		       (eql pat1 pat2)
		       (and (consp pat1)
			    (consp pat2)
			    (test (car pat1) (car pat2))
			    (test (cdr pat1) (cdr pat2))
			    ))))
      (test pat1 pat2))))

(defun retrieve (pat &rest keyword-pairs )
  (let ((method (and (consp pat)
		     (retrieval-method (pattern-head pat)))))
    (if method
	(eval `(apply  ',method ',pat ',keyword-pairs))
	(nconc (apply #'from-assertions pat keyword-pairs)
	       nil
	       ;;(from-backward-chaining pat)
	       ;;we don't use the back-ward chaining for now
	       ))))
    

;;;this slightly complex from-assertions takes care of retrieval from
;;;specified data list (keyword :from-data-list), the key function used to
;;;get pattern from data list (:key-function)  

;;;make sure that the key-function is given as a function name, without
;;;"function" special form, if you want :with-replacement to work

;;;if :with-pat is set, then the result is a list of items of the form 
;;;(<list of data>.<list of substitutions>).  thus, (car item) gives the
;;;data list and (cadr item) gives the binding list

(defun from-assertions (pat &key with-pat with-replacement from-data-list  key-function)
  (let ((from-data-list (or from-data-list
			    (dumb-fetch pat))))
    (for (data :in from-data-list)
	 :splice (let* ((assertion (if key-function
				       (funcall key-function data)
				       data))
			(result (unify  assertion pat)))
;;; we unify the assertion with pat rather than other way round
    ;;; so that we get right binding list....
		   (if result
		       (if with-pat
			   (progn
			     (if with-replacement
				 (progn
				   (if key-function
				       (eval `(setf (,key-function ',data)
						    (safe-replace-variables
						      ',assertion (car ',result))))
				       (setf data (safe-replace-variables
						    assertion (car result))))))
			     (list (list (list data) (car result))))
			   
			   result))
		   ))))

;;;these two will go away and will be replaced by the gost and tome retrieval functions
(defmacro dumb-index (pat  &key (place '*assertions*) )
  `(progn (push ,pat ,place) 
    ,pat)) 

(defmacro dumb-fetch (pat &key (place '*assertions*))
  (declare (ignore pat))
  `,place)

;;;from pp 206

(defun uniquify-variables (pat)
  (let ((new-names (rename-list pat nil)))
    (if (null new-names)
	pat
	(rename-variables pat new-names))))

(defun rename-list (pat &optional new-names)
  (cond ((pcvar-p pat)
	 (let ((id (pcvar-id pat)))
	   (cond ((assoc id new-names) new-names)
		 (t (cons (list id (make-pcvar :id
					       (copy-symbol id)))
			  new-names)))))
	((consp pat)
	 (rename-list (cdr pat)
		      (rename-list (car pat) new-names)))
	(t new-names)))

(defun rename-variables (pat new-names)
  (cond ((pcvar-p pat)
	 (let ((entry
		 (assoc (pcvar-id pat) new-names)))
	   (if entry (cadr entry) pat)))
	((atom pat)
	 (let ((entry (assoc pat new-names)))
	   (if entry (pcvar-id (cadr entry)) pat)))
	(t
	 (cons (rename-variables (car pat) new-names)
	       (rename-variables (cdr pat) new-names)))))

;;;storage methods can be implemented using code in pp 215
;;;right now, i will use a dumb function...


(defmacro define-retrieval-method (head args . body)
  `(progn
     (setf (gethash ',head *retrieval-methods*)
	   #'(lambda ,args ,@body))
     ',head))

(defun retrieval-method (key)
  (gethash key *retrieval-methods*))



(defmacro define-storage-method (head args . body)
  `(progn
     (setf (gethash ',head *storage-methods*)
	   #'(lambda ,args ,@body))
     ',head))
(defun storage-method (key)
  (gethash key *storage-methods*))

(defun pattern-head (pat)
  (car pat))

(defun pattern-args (pat)
  (cdr pat))



;;;the storage and retrieval methods for and

(define-storage-method and (pat &rest keys)
  (for (assertion :in (pattern-args pat))
       :do (apply #'store-pattern assertion keys)))

(define-retrieval-method and (pat &rest keyword-pairs)
    (let ((results   (apply #'conjunct-retrieve 
			    (pattern-args pat) nil keyword-pairs)))
	 (if (getf keyword-pairs :with-pat)
	     (for (result :in results)
		  :save (list (progn (push 'and result) result)))
	     ;;this way, the data is always single..
	     results  		; else
	 )))
(defun partial-p (result)
    (eq (car result) '*partial*)
)

(defun partial-mapping (result)
    ;;if the result is partial, then the mapping will be in the
    ;;third position
    (if (partial-p result)
	(third result)
	result
	))    

(define-retrieval-method pand (pat &rest keyword-pairs)
    ;;this will never be called with any keywords other than
    ;;from-data-list
    (let ((results   (apply #'pconjunct-retrieve 
			    (pattern-args pat) nil keyword-pairs))
	  filtered-results best
	  sresults)
	 (for (result :in results)
	      :when (not (partial-p result))
	      :do (push result filtered-results))
	 (cond ( filtered-results)
	       ;;ie, if there are non-partial results,
	       ;;pand is equivalent to and you
	       ;;just return them
	       
	       ;;else
	       (t ;;all the results are partial matches..
		  (setq sresults (sort results #'> :key #'second))
		  (setq best (second (first sresults)))
		  ;;best is the minimum number of predicates not
		  ;;matched
		  (for (sresult :in sresults)
		       :when  (eql (second sresult) best)
		       :save sresult
		  )) ;;the best sresults will be returned..
	       )))


(defun pconjunct-retrieve (conjuncts sub &rest keyword-pairs)
    
    ;;supposed to return the best match when it fails...    
    ;;doesn't care about with-pat keyword...


    (cond ((null conjuncts) (list sub))
	  
	  (t (let ((subs  (apply #'retrieve
				 (safe-replace-variables
				     (car conjuncts)
				     sub)
				 keyword-pairs)))
		  
		  (cond ((null subs) 
			 `((*partial* ,(- 0 (length conjuncts))
			       ;;so many conjuncts are not working..
			       ,sub)))	 
			(t  (for (next-sub :in subs)
				 :splice (apply #'pconjunct-retrieve
						(cdr conjuncts)
						(append next-sub sub)
						keyword-pairs)))
		  ))))
)
    




(defun conjunct-retrieve (conjuncts sub &rest keyword-pairs)
  (cond ((null conjuncts) (list sub))
	(t (for (next-sub :in
			  (apply #'retrieve
				 (safe-replace-variables
				   (car conjuncts)
				   (if (getf keyword-pairs :with-pat)
				       ;;if the retrieval is being done with patterns,
				       ;;the result given by find-assertions will be
				       ;;in the form ( (data . sub)), so sub is 
				       (cadr  sub)
				       sub))
				 keyword-pairs))
		:splice (apply #'conjunct-retrieve
			       (cdr conjuncts)
			       (if (getf keyword-pairs :with-pat)
				   (list (append (car next-sub) (car sub))
					   (append (cadr next-sub) (cadr sub)))
				   (append next-sub sub))
			       keyword-pairs)))))

;;;for not

(define-retrieval-method  not (pat &rest keyword-pairs)
  (or (apply #'from-assertions pat keyword-pairs)
      (if (apply #'retrieve (car (pattern-args pat)) keyword-pairs )
	  nil
	  (list nil))))

;;;there is no separate storage method for not...
;;;though we can say that to store (not (on a b)) we just remove anything that
;;;unifies with (on a b)????
    
;;; my function for schema reading;;;;
    
(defun pcvar-subst (id subpcvar glist)
    ;; substitute all occurances of a pcvar with "id" by subpcvar
    ;; at all levels of the list glist
    (for (x :in glist)
	 :save (cond ((consp x)
		      (pcvar-subst id subpcvar x))
		     ((and (pcvar-p x)
			   (eql (pcvar-id x) id))
		      subpcvar)
		     (t x))))

;;;convert-pat takes a possibly negated pattern and returns its
;;; ground form and sign (multiple values) called by gost and tome
    
(defun convert-pat (pattern)
    (let (gpat sign)
	 (cond ((eql (first pattern) 'not)
		 (setf gpat (cadr pattern))
		 (setf sign :minus))
	       (t (setf gpat pattern)
		  (setf sign :plus)))
	 (values gpat sign)))
(defun sign (pat)
    (multiple-value-bind (pattern sgn) (convert-pat pat)
      (declare (ignore pattern))
	(return-from sign sgn)))
    
    
(defun negate-pat (pat)
    (if (eql (car pat) 'not)
	(cadr pat)
	;;else
	(list 'not pat)
    ))
;;;this function checks if a given pattern is ground (no variables)
;;;won't work for conjunctive patterns
        				
(defun ground-pat-p (pattern)
    (not (some #'pcvar-p pattern)))
    
	    
;;; macros for taking care of retrieval results-they correctly find the
;;; binding and data from the retrieval result.    
(defun  retrieval-result-binding (result)
    (if (eql (length result) 2)
	;;which means there is data along with binding ((data) (bindlist))
	(cadr result)
	;; else
	result))
    
(defun retrieval-result-data (result)
    (if (eql (length result) 2)
	(if (eql (length (car result)) 1)
	    ;; only one data matches
	    (caar result)
	    ;; else, for conjunctive-retrieval??
	    ;; with the change in retrieval-method of 'and
	    ;; the else part is no longer required
	    ;; (car result)
	)
	result))
;	(error "~s does not contain any retrieval data" result)))
    

(defun remove-binding (pat sub)
  (cond ((atom pat) (var-value pat sub))
	(t (cons (remove-binding (car pat) sub)
		 (remove-binding (cdr pat) sub)))))


(defun value-binding (value alist)
  (assoc value alist))

(defun var-value (pat sub)
  (let ((binding (value-binding pat sub)))
    (cond ((null binding) pat)
	  (t (make-pcvar :id (cadr binding))))))

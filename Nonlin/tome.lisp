;;;-*- Mode: Lisp; Package: NONLIN -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;    Application:  NONLIN
;;;;  Appl. Version:  1.3
;;;;     Appl. Date:  Oct. 1992
;;;;
;;;;           File:  TOME.LISP
;;;;       Contents:  Functions for interfacing with TOME.
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
;;;;  11/09/92  bpk  cleanup, doc.
;;;;  10/01/91  bpk  fixed bug in LOOKUP-TOME-PARTIAL-PAT

(in-package :nonlin)

(defun print-tome-entry (tome-entry stream depth)
  "Print tome-entry structure."

  (declare (ignore depth))
  (format stream "Eff:~s  :assert[~s ]  :delete[~s ]~%" (tome-entry-effect tome-entry)	
	  (get-tome-entry-asserts tome-entry) (get-tome-entry-deletes tome-entry))
)


(defun print-tome (&optional (stream t) (depth t))
  "Print TOME."

   (declare (ignore depth))
  (format stream "~%Tome Table Entrees:~%")
	       (for (tome-entry :in (all-tome-entrys))
                 :do
	       (format stream "~s" tome-entry))
  nil
)

    
(defun all-tome-entrys ()
   "Returns all tome-entry structures in TOME."
    (let (glist)
	 (maphash #'(lambda (key tome-entry-list)
			    (declare (ignore key))
			    (setf glist (append glist tome-entry-list))
		    ) *tome*)
	 glist)
)

    
(defun reset-tome ()
   "Initialize TOME."

    (setf *tome* (make-hash-table))
)

   
(defun lookup-tome (effect)
   "Returns tome-entry structure for effect (pattern) in TOME."

   (let (result (datalist (relevant-tome-entrys effect)))    
      (setf result (find effect datalist :key 'tome-entry-effect :test #'equal))
))


(defun index-tome  (effect)
   "Returns entry for effect in TOME (creates entry if necessary)."

   (let ((lookup-result (lookup-tome effect))
         effect-entry)

      (if lookup-result 
         ; then entry exists, return it
	    (return-from index-tome lookup-result)
         (else ; make a new entry:
	    (setf effect-entry (make-tome-entry :effect effect))
	    (enter-tome-table effect-entry)
	    (return-from index-tome  effect-entry)
      ))
))


(defun lookup-tome-partial-pat (peffect)
   "Returns all tome-entry structures whose effects unify with pattern peffect (may
    contain variables)."

    ;; Called by q&a.

    (let (results                                     
          (datalist (relevant-tome-entrys peffect)))

       (cond ((null datalist) nil) ; fixed this to Not call retrieve if no match
             (t 
                (setf results (retrieve peffect :from-data-list datalist
			                :key-function 'tome-entry-effect :with-pat t))))
))


(defun enter-tome (effect type atnode)
   "Enters effect (+/- pattern) of node atnode into TOME." ; effect-type is :assert or :delete

    (let ((effect-entry (index-tome effect))) ; existing entry for effect

	 (case type
	       (:assert 
		 (if (not (member atnode (get-tome-entry-asserts effect-entry)))
		   (then ; create a new entry for effect

		     (if (eql *current-context* (get-context (car (tome-entry-asserts effect-entry))))
		        ;contexts are same and therefore just add atnode to the car of the 
			;list
		        (setf (tome-entry-asserts effect-entry)
			      (push (append1 (car (tome-entry-asserts effect-entry)) atnode)
			            (cdr (tome-entry-asserts effect-entry))))
			;else make a new list with the current context and add to the list 
			(push (add-context (cons atnode (get-tome-entry-asserts effect-entry)))
			      (tome-entry-asserts effect-entry))))))
	       (:delete
		 (if (not (member atnode (get-tome-entry-deletes effect-entry)))
		    (then ; create a new entry for effect

		      (if (eql *current-context* (get-context (car (tome-entry-deletes effect-entry))))
			 (setf (tome-entry-deletes effect-entry)
			       (push (append1 (car (tome-entry-deletes effect-entry)) atnode)
			             (cdr (tome-entry-deletes effect-entry))))
			 ;else
			 (push (add-context (cons atnode (get-tome-entry-deletes effect-entry)))
			       (tome-entry-deletes effect-entry))))))
	 )

         ;; If deleting a fact asserted in the initial contect, need to remove that fact
         ;; from init-ctxt and put it in plan-head.
	    ;; All (not(pat)) are specified as (pat) with type :delete.
	    ;; There are no (not(pat)) in init-ctxt so modification
	    ;; needs to be done only for :delete type effects.
            ;; THIS WILL NEED TO BE CHANGED TO ALLOW NEG. FACTS IN INIT-CTXT.
	 (when (eql type :delete)
	     (modify-init-ctxt effect type)
	     ;; Now we can handle conflicts involving this fact by changing planhead if necessary,
             ;; leaving init-ctxt alone.  Also need this for linking to work since init-ctxt is not
             ;; present in allnodes.
	     )
))


(defun delete-tome (effect type atnode)
   "Delete an effect (+/- pattern) for node atnode from TOME."

   (let ((effect-entry (index-tome effect)))

        (case type
              (:assert
                 (if (eql *current-context* (get-context (car (tome-entry-asserts effect-entry))))
		    (setf (tome-entry-asserts effect-entry) 
			  (push (remove-list atnode (car (tome-entry-asserts effect-entry)))
				(cdr (tome-entry-asserts effect-entry))))
		    ;else
		    (push (add-context (remove-list atnode (get-tome-entry-asserts effect-entry)))
			  (tome-entry-asserts effect-entry))))
	      (:delete
	         (if (eql *current-context* (get-context (car (tome-entry-deletes effect-entry))))
		    (setf (tome-entry-deletes effect-entry)
			  (push (remove-list atnode (car (tome-entry-deletes  effect-entry)))
				(cdr (tome-entry-deletes effect-entry))))
		    ;else
		    (push (add-context (remove-list atnode (get-tome-entry-deletes effect-entry)))
			  (tome-entry-deletes effect-entry))))
)))


(defun tome-conflicts (effect type)
   "Returns nodes that conflict with effect (of type :assert or :delete)."

   (let ((effect-entry (lookup-tome effect)))

        (case type
	   (:assert ; return nodes that delete this effect:
     	       (return-from tome-conflicts (get-tome-entry-deletes effect-entry)))
	   (:delete ; return nodes that assert this effect:
	       (return-from tome-conflicts (get-tome-entry-asserts effect-entry)))
	)
))


(defun get-tome-entry-asserts (entry)
   "Return nodes asserting effect for tome-entry in most recent context."

   (cdar (tome-entry-asserts entry))
)


(defun get-tome-entry-deletes (entry)
   "Return nodes deleting effect for tome-entry in most recent context."

   (cdar (tome-entry-deletes entry))
)
 
   
(defun modify-init-ctxt (effect type)
   "Move a deleted condition from *init-ctxt* to *planhead*."

   ;; Called by enter-tome when some node deletes a condition in the initial context.
  
    (declare (ignore type))
    (when (find effect (node-ctxt *init-ctxt*) :test #'equal )
       ;; when effect found in *init-ctxt*:

       (dremove effect (node-ctxt *init-ctxt*) :test 'equal) ; delete it from *init-ctxt*
       (store-pat effect :place (node-ctxt *planhead*))      ; add it to *planhead*

       ;; save the information that the init-ctxt was modified (for backtracking purposes).
       (push (add-context effect) *init-ctxt-modification-list*)

       ;; enter this effect in tome under plan-head 
       (enter-tome effect :assert (node-nodenum *planhead*))

       ;; Now change any GOST or TOME entry for effect. 
       ;; Wherever init-ctxt is a contributor of effect, make planhead the contributor instead.
       (let ((gentrys (gost-entrys effect))
	     (tentry  (lookup-tome effect)))
          (for (gentry :in gentrys)
		       :do 
		       (setf (gentry-contributors gentry)
			     (push (substitute (node-nodenum *planhead*) (node-nodenum *init-ctxt*)
				               (pop (gentry-contributors gentry)))
				   (gentry-contributors gentry)))
	  )
		    #| (setf (tome-entry-asserts tentry)
			     (mapcar #'substitute-init-ctxt 
				      (tome-entry-asserts tentry)))
		       (setf (tome-entry-deletes tentry)
			     (mapcar #'substitute-init-ctxt 
                                      (tome-entry-deletes tentry))) |#
    ))
)	 
        


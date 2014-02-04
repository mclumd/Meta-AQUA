;;;-*- Mode: Lisp; Package: NONLIN -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;    Application:  NONLIN
;;;;  Appl. Version:  1.3
;;;;     Appl. Date:  Oct. 1992
;;;;
;;;;           File:  BACKTRACK.LISP
;;;;       Contents:  Backtracking Functions
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
;;;;  -----------------------------------------------------------------------------------   
;;;;  9/18/92   bpk  fixed bplanner to return status, cleanup, comments
;;;; 10/04/91   bpk  added support for batch use mode
;;;; 09/22/91   bpk  Fixed bug in BACKTRACK-THRU-CONTEXT

(in-package :nonlin)

(defun reset-context ()
  "Resets context lists.  Creates new initial context."
  (setf *context-list* nil)
  (create-new-context)
)


(defun create-new-context ()
   "Creates a new context after making a choice."
   (setf *current-context* (make-context-symbol))
)


(defun make-context-symbol ()
   "Generates and returns a context symbol."
   (make-symbol (format nil "C~D" (incf *context-counter*)))
)


(defun backtrack-thru-context ()
   "Backtrack through previous contexts (depth first)."
   ;;; Returns T if can backtrack else returns NIL.
   ;;; called by bplanner when reach a dead end or want to explore alternative plans
   (prog ()
      restart ; label
      (if *context-list*  ; if there are backtracking points (alternate contexts)
         (then
            (backup-context *current-context*)  ; backup one level (reset data structures)
            (setf *current-context* (get-context (car *context-list*))) ; get last context

            ; get an alternative at last backtracking point
            (let* ((type (backtrack-entry-type (cdar *context-list*)))  
	           (alternative (pop (backtrack-entry-alternatives (cdar *context-list*)))))
               (if (null alternative)  ; if no alternatives, backup further
	          (then
	             (pop *context-list*)
	             (go restart))
                  (else  ; there is an alternative at last backtracking point
	             (case type
	                (:expand  ; alternative linearization 
		           (create-new-context)
		           (linearize alternative) ; linearize with the new choice
	                )
	                (:establish  ; alternative contributor
		           (let* ((atnode       (first  alternative))  ; atnode needs condition
		                 (contributor   (second alternative))  ; contributor node
		                 (linearization (third  alternative))  ; alternative linearization
		                 (condition     (fourth alternative))  ; condition establishes
		                 (nchild (my-copy-node (allnodes atnode))))  ; new phantom node
 		              (create-new-context)
		              (when (not-empty linearization) (linearize linearization))
		              (setf (node-type nchild) :phantom)
		              (make-child nchild atnode)
                              (set-allnodes nchild atnode) ; FIXED BUG HERE      		    
 
		              (enter-gost (node-todo nchild) :phantom         ; update GOST
				   (node-nodenum nchild) (list contributor))
	               ))
	               (:schema  ; alternative schema
		          (let ((atnode        (first  alternative))  ; node to expand
			        (chosen-schema (second alternative))) ; alternative schema instance
			    (create-new-context)
			    ;; expand using alternative schema instance
		            (expand-node-using-schema-instance chosen-schema atnode)
	               ))
                    ) ; end case
                    (return-from backtrack-thru-context t)  ; *** RETURN *** (success)
           )))) ; end if *context-list* then
    (else ; no backtracking points
       (when (interactive-p) (format t "~&*** No more solutions."))
       (setf *terminate* t)
       (return-from backtrack-thru-context nil)  ; *** RETURN *** (failure)
    )) ; end if *context-list*
))


(defun bplanner () 
   "Plan until no more backtracking alternatives or user wants to quit."
   ;;; called by plan-for, calls planner

   (setq *nonlin-termin-status* nil)  ; initialize termination status var

   (do ((result nil) (finished nil))  ; loop until finished
       (finished :done)               ; when finished, return :done

       (setq result 
          (catch :backtrack (planner)))  ; invoke planner fn. - return here on throw :backtrack
         
       (cond ; !!! check this:
          ((equal result :failure) 
             (setq finished (not (backtrack-thru-context)))
             (when finished (setq *nonlin-termin-status* :failure)))
          ((equal result :cycle-limit)
             (setq finished t)
             (when finished (setq *nonlin-termin-status* :cycle-limit)))
          (t ; result was success
             (setq *nonlin-termin-status* :success)
             (if (and (interactive-p) ; if in interactive use mode, prompt user about continuing
                      (y-or-n-p "Try again? (~D more alternatives)" (length *context-list*)))
                (setq finished (not (backtrack-thru-context))) ; keep going
                ; else
                (setq finished t) ; quit now
          ))
       )
    ) ; end do
)


(defun backtrack () 
   "Initiate backtracking.  Throws to :backtrack in bplanner"
   (throw :backtrack :failure)
)


(defun backup-context (context)
  "Restore data structures to their values in the previous context."
  ;;; context = context symbol for current context

  (backup-allnodes   context)
  (backup-gost       context)
  (backup-tome       context)
  (backup-taskqueue  context)
  (restore-init-ctxt context)  
)


(defun backup-allnodes (context)
  "Restore ALLNODES to its value in the previous context."
  ;;; context = context symbol for current context

  ;; loop through nodes in *allnodes*
  (do ((index 0 (1+ index)))
      ((> index (1- *striplen*)))
      (let ((node-context-pair (car (aref *allnodes* index))))
         (when (eql context (get-context node-context-pair))  ; if node created in curr. context,
            (setf (aref *allnodes* index) (cdr (aref *allnodes* index)))) ; then delete it
         (backup-node-expansion-info context)

         (let* ((node (allnodes index))            
	        (expanconds (node-expanconds node))        
	        (effects    (node-ctxt node))              
                (prenodes   (car (node-prenodes node)))
                (succnodes  (car (node-succnodes node))))
            (when (eql context (get-context prenodes))                   ; backup value of prenodes
               (setf (node-prenodes node) (cdr (node-prenodes node))))
            (when (eql context (get-context succnodes))                  ; backup value of succnodes
               (setf (node-succnodes node) (cdr (node-succnodes node)))) 

	    (for (expancon :in expanconds)  ; backup bindings of use-only-for-query conds.
	       :do
	       (when (eql (scondition-type expancon) :use-only-for-query)
		  (when (eql context (caar (scondition-binding expancon)))
		     (then ; reverse of rebinding
		        (let ((new-expancon (my-copy-scondition expancon))
			      (current-binding (get-scondition-binding expancon)))
			   (setf (scondition-binding new-expancon) 
                              (cdr (scondition-binding new-expancon)))
			   (setf (scondition-pattern new-expancon)
			      (remove-binding (scondition-pattern new-expancon)
			                      (mapcar #'reverse current-binding)))
			   (destructive-replace-variables (scondition-pattern new-expancon)
							  (get-scondition-binding new-expancon))
			   (dremove expancon (node-expanconds node))
			   (pushnew new-expancon (node-expanconds node))

			   (for (effect :in effects)
			      :do
			      (setf (seffect-pattern effect)
				   (remove-binding (seffect-pattern effect) 
                                                   (mapcar #'reverse current-binding)))
			      (destructive-replace-variables (seffect-pattern effect)
							     (get-scondition-binding new-expancon))
                        ))
	   ))))
   ))) ; end do
) 
    

(defun backup-gost (context)
  "Restore GOST to its value in the previous context."
  ;;; context = context symbol for current context

  ;; check each entry in gost and removes contributors that are part of the curr. context
  (for (gost-entry :in (all-gost-entrys))
      :do
      (let ((pluses-gentrys  (gost-entry-pluses gost-entry))
            (minuses-gentrys (gost-entry-minuses gost-entry)))
         (for (pluses-gentry :in pluses-gentrys)
             :do
             (when (eql context (get-context (car (gentry-node pluses-gentry))))
                (setf (gentry-node pluses-gentry) (cdr (gentry-node pluses-gentry))))
             (when (eql context (get-context (car (gentry-contributors pluses-gentry))))
                (setf (gentry-contributors pluses-gentry) 
                      (cdr (gentry-contributors pluses-gentry))))
             (when (null (gentry-contributors pluses-gentry))
                (dremove pluses-gentry (gost-entry-pluses gost-entry)))
         )
         (for (minuses-gentry :in minuses-gentrys)
            :do
            (when (eql context (get-context (car (gentry-node minuses-gentry))))
               (setf (gentry-node minuses-gentry) (cdr (gentry-node minuses-gentry))))
	    (when (eql context (get-context (car (gentry-contributors minuses-gentry))))
	       (setf (gentry-contributors minuses-gentry) 
                     (cdr (gentry-contributors minuses-gentry))))
            (when (null (gentry-contributors minuses-gentry))
               (dremove minuses-gentry (gost-entry-minuses gost-entry)))
         )
      )
))


(defun backup-tome (context)
   "Restore TOME to its value in the previous context."
   ;;; context = context symbol for current context

   ;; check each entry in tome and remove those that are part of current context
   (for (tome-entry :in (all-tome-entrys))
      :do
      (when (eql context (get-context (car (tome-entry-asserts tome-entry))))
	 (setf (tome-entry-asserts tome-entry) (cdr (tome-entry-asserts tome-entry))))
      (when (eql context (get-context (car (tome-entry-deletes tome-entry))))
	 (setf (tome-entry-deletes tome-entry) (cdr (tome-entry-deletes tome-entry))))
   )
)


(defun restore-init-ctxt (context)
   "Undo any modifications made to initial context during the current context."
   ;;; context = context symbol for current context

   (do ((modify-context (get-context (car *init-ctxt-modification-list*)) 
 		        (get-context (car *init-ctxt-modification-list*))))
       ((not (eql modify-context context)))

       (let ((effect (cdr (car *init-ctxt-modification-list*))))
           (pop *init-ctxt-modification-list*)
	   (dremove effect (node-ctxt *planhead*) :test 'equal)
	   (store-pat effect :place (node-ctxt *init-ctxt*))
           (let ((gentrys (gost-entrys effect)))
		(for (gentry :in gentrys)
		     :do
		     (setf (gentry-contributors gentry)
			   (push (substitute (node-nodenum *init-ctxt*)
				   (node-nodenum *planhead*)
			    	   (pop (gentry-contributors gentry)))
				 (gentry-contributors gentry)))
		)
            )
       ))
)


(defun backup-taskqueue (context)
   "Restore TASKQUEUE to its value in the previous context."
   ;;; context = context symbol for current context.
   ;;; Deletes all tasks from taskqueue that were generated in context "context". 

   (setf *taskqueue* (for (node-context :in *taskqueue*)
                         :when (not (eql context (car node-context)))
                         :save node-context))

   (for (node-context :in *taskqueue*)
       :do
       ;; When node in taskqueue was expanded in or after the current context,
       ;; reset its expanded field.
       ;; The "later" condition is easy to check because the context variables are made
       ;; with monotonicity of the context-counter.
       (when (later (snode-expanded (cdr node-context)) context)  
          (setf (snode-expanded (cdr node-context)) nil))
   )
)


(defun get-context (item)
   "Returns context (symbol) for an item."
   (car item)
)


(defun add-context (node-or-nodelist)
   "Adds current context to a list."
   (cons *current-context* node-or-nodelist)
)


(defun remove-context (node-or-nodelist)
   "Remove context from a list."
   (cdr node-or-nodelist)
)


(defun deleters (entrys context)
   "Return all entries in context that do not match context."
   (for (entry :in entrys)
       :when (not (eql (get-context entry) context))
       :save entry
   )
) 

	
(defun later (context1 context2)
   "Returns T if context1 later than context2."
   (let ((string1 (string context1))
         (string2 (string context2)))

      (and (>= (length string1) (length string2))
           (string>= string1 string2))
))

;;;-*- Mode: Lisp; Package: NONLIN -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;    Application:  NONLIN
;;;;  Appl. Version:  1.3
;;;;     Appl. Date:  Oct. 1992
;;;;
;;;;           File:  EXPAND.LISP
;;;;       Contents:  Node expansion functions.
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
;;;;   9/20/92  bpk  changed store-node-expansion-info to write enode structures to 
;;;;                 *planner-expansions*.  Added *backup-node-expansion-info*.
;;;;   2/15/92  BPK  added code to output name of parent of instantiated schema to *kids*.
;;;;                 (changes to store-node-expansion-info)         
;;;;   9/22/91  bpk  added code to store node expansion info in *kids*
 
(in-package :nonlin)

(defun supersedes-p (type1 type2)
    (member type2 (get type1 'subordinates))
)
(eval-when (compile load eval)
    (setf (get :precond 'subordinates) '(:use-when))   
)


(defun expand-node-using-schema-instance  (sch expnode)
   "Expands a node using a schema instance.  Adds new nodes to ALLNODES and taskqueue.
    Returns modified schema instance."

   ;;; expnode = a node structure
   ;;; sch     = a ground schema instance (a COPY of a schema with bindings)


    ;; print debug info (if enabled)
    (if (and (or (eq (debug-p) t) (member 'schema (debug-p)))
	     (not (member (node-type expnode) '(:plan :dummy :planhead))))
	(print-schema sch))

    (let* ((expnum   (node-nodenum expnode))  ; index in ALLNODES of node being expanded
	   (anodes   (schema-strip sch))      ; array of nodes
	   (size     (schema-size  sch))      ; size of anodes
           lastnode  ; last node in anodes
           last      ; index of last node in anodes
           firstnode ; first node in anodes
           first     ; index of first node in anodes
           (current-striplen *striplen*)     ; save original length of ALLNODES
	  )

      (if (> size 0)  ;  if not null expansion 
          (then                                           ;  bug fix - 4/91 bpk 
	     (setf lastnode  (aref anodes (- size 1)))
	     (setf last      (- size 1))
	     (setf firstnode (aref anodes 0))
	    ;(first 0)	     ; anodes array goes from 0 to size-1
	
	     (flet ((relocate (nodes) ; nodes is atom or list
		       ;; A local function to convert node's anode index to ALLNODES index.
                       ;; New nodes (those from anodes) go at end of ALLNODES except the
                       ;; last node in anodes which replaces expnode at ALLNODES[expnum]
                       ;; (so that effects will be handled properly).

                       (if (not (or (null nodes) (consp nodes))) ; nodes is atomic
			   (cond ((eq nodes last) expnum)        
				 (t (+ nodes current-striplen)))
                           ;; else nodes is a list
			   (for (n :in nodes) 
			        :save
			        (cond ((eq n last) expnum)
				      (t (+ n current-striplen))))
                       )
                   ))
		
                ;; process each node in strip (anodes)
		(do ((index 0 (1+ index)))
		    ((> index (1- size)))

 		    (let ((newnode  (aref anodes index)) ; get node in anodes[index]
		     	  (newplace (relocate index))    ; get node's future index in ALLNODES
                         )

		       (set-allnodes newnode newplace)   ; put node in ALLNODES[newplace]
		       (make-child newnode expnode)      ; make node child of expanded node

                       ;; Relocate node's successor links to point to (new) node in ALLNODES.
                       ;; Store context with links.
		       (when (node-succnodes newnode)
     		   	   (setf (node-succnodes newnode)
           	                 (list (add-context (relocate (node-succnodes newnode)))))
                       )
                       ;; Relocate node's predecessor links to point to (new) node in ALLNODES.
                       ;; Store context with links.
		       (when (node-prenodes newnode)
     		   	   (setf (node-prenodes newnode)
           	     	         (list (add-context (relocate (node-prenodes newnode)))))
 		       )
                       ;; Add node to taskqueue if it's not of type :primitive.
		       (unless (eql (node-type newnode) :primitive)
		           (enter-taskqueue newnode)
                           ;; Since we remove tasks from the front of the taskqueue and
                           ;; insert at the rear in strip order, we effectively get a
                           ;; breadth-first processing of node expansions.
		       )
		))

                ;; store info about expansion of expnode
                (store-node-expansion-info expnode)

                ;; the predecessors of expnode become predecessors of first node of 
                ;; the expansion instead
	        (when (get-prenodes expnode)
 		   (for (node :in (get-prenodes expnode))
		        :do
		        (let ((succnodes (remove-list expnode (get-succnodes node))))
			   (push (add-context (append1 succnodes (node-nodenum firstnode))) 
                                 (node-succnodes node))))
  		        ;; little hack, reset the prenodes of firstnode properly:
         	   (setf (node-prenodes firstnode) (list (add-context (get-prenodes expnode)))) 
                )
	        ;; make the successor nodes of 'expnode' the successor nodes of
	        ;; lastnode of the schema (this is already the case due since lastnode 
                ;; got expnodes's ALLNODES index - so just add context here)
	        (when (get-succnodes expnode)
		    (setf (node-succnodes lastnode)
		          (list (add-context (get-succnodes expnode))))
	        )

                ;; copy all the conditions of expnode to the firstnode (i.e., firstnode inherits
                ;; its parent's conditions).
	        (for (econd :in (node-expanconds expnode))
	    	     :do		     
		     (let ((econd-copy (my-copy-scondition econd)))
		        (setf (scondition-atnode econd-copy) (node-nodenum firstnode))
		        ;; change the atnode to firstnode...
		        (pushnew econd-copy (node-expanconds firstnode))
                     )
	        )
	        ;; relocate all the conditions in the schema 'sch' and make
	        ;; appropriate GOST entries.
		(for (scondition :in (schema-conditions sch))
		     :do
		     (let* ((cond-pat  (scondition-pattern scondition))
			    (cond-type (scondition-type scondition))
			    (old-cond  (find cond-pat (node-expanconds firstnode)
					     :key #'scondition-pattern :test #'equal))
		             ;; At this step old-cond will be non-null if there
			     ;; was a condition of the same pattern at the parent node.
			    (atnode (relocate (scondition-atnode scondition)))
			     ;;atnode is the node into which this condition should go
			    (relocated-contributors (relocate (scondition-contributors scondition)))
			   )
			(if (and old-cond (supersedes-p (scondition-type old-cond) cond-type))
			       ;; If a similar condition exists in the parent node
			       ;; AND its type supersedes the type of the current
			       ;; condition, then we take this as the current 
			       ;; condition and change gost entry such that the 
			       ;; atnode is now relocated atnode of the scondition.
			       (then
			            ;; The condition is removed from expnode,such is
			            ;; that it is not tagged on to first node again.
				    (dremove old-cond (node-expanconds firstnode))
				    (setf (scondition-atnode old-cond) atnode)
				    ;; old-cond now is properly changed
				    (pushnew old-cond (node-expanconds atnode) :test #'equal)
				    (for (gentry :in (gost-entrys cond-pat))
					 ;; get the gost entrees that have this condition
					 ;; and change the node entry to the atnode.
					 :when (eql (get-gentry-node gentry) 
						    (node-nodenum expnode))
					 :do 
					 (push (add-context (node-nodenum atnode))
				 	       (gentry-node gentry))
				    )
				    ;; the condition is properly entered into GOST.
			       )
			       (else 
				    ;;  Either there is no old condition or
				    ;; the old condition does not supersede scondition.
				    (setf (scondition-contributors scondition)
				          relocated-contributors)
				    (setf (scondition-atnode scondition) atnode)
				    (enter-gost cond-pat cond-type atnode relocated-contributors)
				    (pushnew (copy-scondition scondition)
					     (node-expanconds atnode) :test #'equal)
				    ;; the condition is added to node
			    )) ; end if
		)) ; end for

		(for (fcond :in (node-expanconds firstnode))
  		      :do
		      (for (gentry :in (gost-entrys (scondition-pattern fcond)))
			      ; get the gost entrees that have this condition
			      ; and 2. change the node entry and node type to
			      ; firstnode and firstnode->type
		           :when (eql (get-gentry-node gentry) 
			              (node-nodenum expnode))
		           :do 
		           (push (add-context (node-nodenum firstnode))
			         (gentry-node gentry))
		      )
		      ;; gost also knows now that the condition is on firstnode..
	       )

               ;; relocate all effects in schema 'sch'
	       (for (seffect :in (schema-effects sch))
		    :do
		    (setf (seffect-atnode seffect) (relocate (seffect-atnode seffect)))		     
	       )
	
#|
 		(for (eff-pat :in (node-ctxt expnode))
 		     ;;lets have the patterns only in context--for uniformity
 		     ;;with init ctxt
 		     :do		     
 		     ;;(store-pat eff-pat :place (node-ctxt expnode)))
 		(pushnew eff-pat (node-ctxt expnode)))
                (for (seffect :in (schema-effects sch))
		    :do
		    (pushnew seffect (node-ctxt (seffect-atnode seffect))))
|#
	    ) 	; end flet
	  )	; end then
          (else ; size = 0 (expansion is null)
  	     ;; just add effects to expnode
             (for (seffect :in (schema-effects sch))
                :do
                (setf (seffect-atnode seffect) expnum))
	  ))  ; end if

 	  ;; store expansion
	  (setf (node-expansion expnode) sch)
          ;; add effects into context (and TOME)
	  (add-effects (schema-effects sch))

          ;; DEVISOR mods:
          ;; after node expansion, propagate window compressions
          ;;   from first node in expansion to other (colinear) nodes
          (when *devisor-mods* 
             (if (not
                (propagate-window-compressions (allnodes expnum) nil))
                (backtrack)) ; backtrack if unsuccessful
          )

      (return-from expand-node-using-schema-instance sch)
))  			


(defun add-effects (seffects)
  "Add effects to TOME and handle resulting interactions."

  ;;; seffects = list of seffect structures

  (for (seffect :in seffects)
    :do
    ;; only add effects that are not in always context
    (if (not (q&a-always-ctxt (seffect-pattern seffect)))
       (enter-tome (seffect-pattern seffect) (seffect-type seffect)
	 	   (seffect-atnode seffect)))
  )

  ;; Now try to remove all the interactions produced by the added effects. If they
  ;; can not be removed then delete all the effects from TOME and backtrack.

  (let ((suggested-net '(nil))) ; will contain all possible linearizations.
      (for (seffect :in seffects)
           :do
           (let* ((seffect-node (seffect-atnode seffect))
	          (seffect-pat  (seffect-pattern seffect))
	          (seffect-type (seffect-type seffect))
	          conflict-node-set
	          (conflict-condition ; = negation of seffect's pattern
                     (if (eql seffect-type :assert)
			(negate-pat seffect-pat)
			seffect-pat))
	          (effect-condition (negate-pat conflict-condition))
	         )
	 
              (unless (q&a-always-ctxt seffect-pat)
                 ;; find conflicting nodes using TOME (a conflicting node has opposite pattern)
	         (setf conflict-node-set (tome-conflicts seffect-pat seffect-type))
	         ;; if conflict set is not null, there are interactions so resolve them:
	         (when (and (not (null conflict-node-set)) (not (fail-p suggested-net)))
	            (setf suggested-net
                       (resolve-interaction :conflict-nodes conflict-node-set
	   		                    :offender seffect-node
			                    :offender-effect effect-condition
				            :extra-links suggested-net)))
	      )
    ))

    (if (fail-p suggested-net) ; if interactions cannot be removed,
       (then
	  (backtrack))
       (else
          ;; apply one of the linearizations suggested (if any)
	  (when (not-empty suggested-net)
	     ;; save the alternative linearizations as a backtracking point
	     (push (add-context (make-backtrack-entry :type :expand
			        :alternatives (cdr suggested-net))) 
                   *context-list*)
   	 
   	     (create-new-context)             ; create a new context
             (linearize (car suggested-net))  ; apply first linearization suggested
          )
          ;; add effects under new context
	  (for (seffect :in seffects)
	       :do
;	       (pushnew (seffect-pattern seffect) (node-ctxt (seffect-atnode seffect))))
	       (pushnew seffect (node-ctxt (seffect-atnode seffect)))
	  )
    ))
))


(defun remove-list (node list)
   "Remove node from a list of nodes (or nodenums).  Returns new list."
 
   (cond ((null list) nil)
         ((or (equal node (car list)) (equal (node-nodenum node) (car list)))
            (remove-list node (cdr list)))
         (t (cons (car list) (remove-list node (cdr list))))
))
  

(defun store-node-expansion-info (expnode)  ; rewritten 9/20/92 - bpk
   "Record info in *planner-expansions* about the expansion of a node."
   ;;; expnode : node being expanded (a node structure)

   (let (
         (children (node-children expnode))
         (context  *current-context*)
         kids e
        )

      (when children
         (dolist (c children)
            (push (make-ekid :id (snode-id c) :type (node-type c) :todo (node-todo c)) kids)
         ))

      (setq e (make-enode :context context :id (snode-id expnode) :type (node-type expnode) 
                          :todo (node-todo expnode)
                          :schema *schema-parent-name* :kids kids))
      (setq *planner-expansions* (append *planner-expansions* (list e)))
      (when (debug-ds-p 'expinfo) 
          (format t "~&Expansion info entry = ~s~&   " e))
      ;; *schema-parent-name* is set in schema.lisp
))


(defun backup-node-expansion-info (context)   ; added 9/20/92 - bpk
   "Remove obsolete node expansion info when backtracking to previous context."
   ;; context = current context

  (setq *planner-expansions* 
     (remove-if #'(lambda (e) (equal (enode-context e) context)) 
                *planner-expansions*))
)


(defun update-node-expansion-info (old-id new-node) ; added 10/11/92 - bpk
   "Updates node expansion info when a new node replaces an old one."
   ;;; called by try-to-remove-contributor (in gost.lisp) when a phantom node
   ;;; reverts to being a goal node
   
   (when (debug-ds-p 'expinfo) 
       (format t "~&Goal node ~s replaces phantom node ~s." (snode-id new-node) old-id))

   (dolist (e *planner-expansions*)
      (dolist (k (enode-kids e))
         (when (equal (ekid-id k) old-id)
            (setf (ekid-id   k) (snode-id   new-node))
            (setf (ekid-type k) (snode-type new-node))
            (setf (ekid-todo k) (snode-todo new-node))
         )))
)
   
         
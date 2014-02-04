;;;-*- Mode: Lisp; Package: NONLIN -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;    Application:  NONLIN
;;;;  Appl. Version:  1.2.1
;;;;     Appl. Date:  Sept. 1992
;;;;
;;;;           File:  PRINTPLAN.LISP
;;;;       Contents:  Functions for sorting, outputing the plan.
;;;;
;;;;         Author:  Brian Kettler
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
;;;;  11/09/92  bpk  cleanup, doc.
;;;;   9/20/92  bpk  add fn save-plan, changed print-plan to take parm :plan
;;;;   9/15/92  bpk  various changes to output
;;;;   9/22/91  bpk  fixed bug in TOPSORT, GET-PLAN
;;;;   9/13/91  bpk  added support for storing planner output in *planner-out*

(in-package :nonlin)

(defun reset-plan-list ()
   "Reset *plan-list*."

    (setf *plan-list* nil)
)
    

(defun get-plan ()
   "Do a topological sort of plan network to get a linear plan which is then returned via
    *plan-list*."

    (let ((dummynode (make-node :type :plantail)))  ; added to *plan-list* so that print routine
                                                    ; will terminate.

       (when *plan-list* (return-from get-plan *plan-list*)) ; *plan-list* already contains plan

       (unmark-allnodes)
       (topsort *planhead*) ; sets *plan-list*
       (setf *plan-list* (append1 *plan-list* dummynode)) ; terminator node

;	 (setf (node-type (car (last *plan-list*)))
;	       :plantail)
	 ;;this is so that goal state will be printed properly
	 ;;wont work if there was no dummy node in the plan schema expansion
	 ;;for stopgap it is ok

       *plan-list* ; return the plan
    )  
)


(defun get-ground-plan ()
   "Returns the nodes in the linear plan (from get-plan) that are to be printed."

    (for (node :in (get-plan))
	 :filter (if (member (node-type node) 
                    '(:planhead :plantail :primitive :event)) ; node types not printed
		     node)
    )
)
	     

(defun unmark-allnodes ()
   "Reset all node marks to 'unvisited."

  ;; for each node in ALLNODES
  (do ((index 0 (1+ index)))
      ((> index (1- *striplen*)))
      (if (not (null (aref *allnodes* index)))
        (then
          (setf (node-mark (cdar (aref *allnodes* index)))  'unvisited)
        )))
)


(defun topsort (node)
   "Recursive topographic sort algorithm for sorting partially-ordered plan network."

    ;; Net must be a DAG.  Taken from Hopcroft, Ullman, and Aho data structures book.
    ;; Sort is done IN REVERSE - i.e., visits node with no SUCCESSORS first.

    (unless (node-p node) (setf node (allnodes node))) ; make sure node is a structure
    (setf (node-mark node) :visited) ; mark node as visited

    ;; recursively visit node's unvisited successors
    (for (snode :in (get-succnodes node)) 
	 :do
         (when (not (node-p snode))         ; make sure snode is a structure
             (setf snode (allnodes snode))) ; added this to fix bug - bpk
	 (if (not (eql (node-mark snode) :visited))
              (topsort snode)))

    ;; all of node's successors have been visited so output node
    (push node *plan-list*) 
    ;; because we are using push, the reverse order becomes actual order at the end.
)

	     
(defun save-plan ()    
   "Save finished plan in *planner-out*."

    (let ((plan (get-plan)) 
	  )
          (setq *planner-out* nil)
          ;; save plan
	  (for (node :in plan)
	       :do
	       (case (node-type node)
		     ((:primitive :goal :action :event) 
                      (setf *planner-out* ; save the actions comprising the plan
                          (append *planner-out* 
                             (list (list 
                                (snode-id node) (node-type node) (node-todo node)))))
		     ))
          )
          plan
))


(defun print-plan (&key (plan nil plan-spec) ; added :plan arg
                        (with-cond nil) (with-eff nil) (long t)) ; long = long version
    "Print (linear) plan."
    (let (nodetag)

          (unless plan-spec (setq plan (get-plan)))

          (when *devisor-mods*              ; DEVISOR mod
             ;; select actual (ideal) start time for each node 
             ;;   with a time window 
             (determine-ideal-start-times))

	  (format t "~&~%*** The (current) plan is:")
	  (for (node :in plan)
	       :do
	       (case (node-type node)
		     (:phantom )
		     (:dummy )
		     (:plantail
                         (when long
			 (format t "~%==============================================")
			 (for (scond :in (find-conditions node))
			      :do (format t "~&~25T ~s" scond))))
		     (:planhead
                         (when long
			 (format t "~%===== INITIAL STATE ==========================")
                          
			 (for (pattern :in (append (node-ctxt *init-ctxt*)
						   (node-ctxt *planhead*)
						   (node-ctxt *always-ctxt*)))
			      :do
			      (format t "~&~s~%" pattern))
			 (for (scond :in (append (find-uses node)
						 (find-uses 
						     (node-nodenum 
							 *init-ctxt*))
						 (find-uses 
						     (node-nodenum
							 *always-ctxt*))))
			      :do (format t "~&~15T ~s" scond))
                         (when *devisor-mods*  ; DEVISOR mods
                            (format t "~& Planning begins at *time0* = ~s"
                               *time0*)
                            (format t "~& (Time *infinity* = ~s)"
                               *infinity*))
			 (format t "~%===== PLAN ACTIONS ==========================")))
		     
		     ((:primitive :goal :action :event) 
		      (unless (null with-cond)
			      (format t "~%~10TAPPLICABILITY CONDITIONS")
			      (for (scond :in (find-conditions node))
				   :do (format t "~&~15T ~s" scond)))
		     
                      (if (equal (node-type node) :event)
                         (setf nodetag (node-eventname node))
                      ; else
                         (setf nodetag (node-todo node)))
                              
                      ;; print action
		      (format t "~& ~% ~s: ~s ~s~%" 
   		         (node-nodenum node) (node-type node) nodetag)                     

;		      (format t "~& ~% ~s: ~s ~s ~40T[Prenodes:~s]~%"
;   		         (node-nodenum node) (node-type node)
;			 nodetag (get-prenodes node))                     
;                      (format t "~& ~40T[Succnodes:~s] ~%"
;                         (get-succnodes node))
                       
                      (when *devisor-mods* ; DEVISOR mods
                      (format t "~& ~40T[Start Time: ~s] ~60T[Duration: ~s] ~%"
                         (window-ist (node-window node)) (node-duration node)))
		      (unless (null with-eff)
			      (format t "~%~20TGOALS--EFFECTS")
			      (for (scond :in (find-uses node))
				   :do (format t "~&~25T ~s" scond)))
		      
		      (if (or with-eff with-cond)
			  (format t "~&~10T***************************************"))
		     )

    	       ))
         
    ) 
   (terpri)
   nil 
)
 
   
(defun find-conditions (node &aux node-conditions)
    "Returns the applicability conditions and their justifications from GOST (for printing)."
    (for (gost-entry :in (all-gost-entrys))
	 :do
	 (let ((scond-pat (gost-entry-condition gost-entry)))

	      (for (gentry :in (gost-entry-pluses gost-entry))
		   :when (eql (gentry-node gentry) node)
		   :do (push (make-scondition :type (gentry-type gentry)
				 :pattern scond-pat
				 :atnode node
				 :contributors (get-gentry-contributors gentry))
			     node-conditions))
	      (for (gentry :in  (gost-entry-minuses gost-entry))
		   :when (eql (gentry-node gentry) node)
		   :do (push (make-scondition :type (gentry-type gentry)
				 :pattern (negate-pat scond-pat)
				 :atnode node
				 :contributors (get-gentry-contributors gentry))
			     node-conditions))

	 ))
    node-conditions
)


(defun find-uses (node &aux node-uses)
    "Returns the actual uses of a node in the plan (for printing)." ; ???
    (for (gost-entry :in (all-gost-entrys))
	 :do
	 (let ((scond-pat (gost-entry-condition gost-entry)))

	      (for (gentry :in (gost-entry-pluses gost-entry))
		   :when (and (member node (get-gentry-contributors gentry))
			      (member (node-type (gentry-node gentry))
				      ;;WHY SHOULD WE EXCLUDE PHANTOM??
				      ;;THEY ARE ALSO USES
				       '(:primitive :action :goal :phantom :plantail))
			 )

		   :do (push (make-scondition :pattern scond-pat
				 :atnode (gentry-node gentry)
				 :contributors (list node))
			     node-uses))
	      (for (gentry :in (gost-entry-minuses gost-entry))
		   :when (and (member node (get-gentry-contributors gentry))
			      (member (node-type (gentry-node gentry))
				       '(:primitive :action :goal :plantail :phantom)))
	      
		   :do (push (make-scondition :pattern (negate-pat scond-pat)
				 :atnode (gentry-node gentry)
				 :contributors (list node))
			     node-uses))
	 ))
    node-uses
)


(defun find-effects (node &aux node-effs)
    "Returns the effects from a node (for printing). (CURRENTLY UNUSED???)." 
    (for (tome-entry :in (all-tome-entrys))
	 :do
	 (if (member node (tome-entry-asserts tome-entry))
	     (push (make-seffect :pattern (tome-entry-effect tome-entry)
		       :type :assert
		       :atnode node)
		   node-effs))
	 (if (member node (tome-entry-deletes tome-entry))
	     (push (make-seffect :pattern (tome-entry-effect tome-entry)
		       :type :delete
		       :atnode node)
		   node-effs))
    ) 
    node-effs
)

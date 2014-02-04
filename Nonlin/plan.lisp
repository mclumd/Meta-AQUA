;;;-*- Mode: Lisp; Package: NONLIN -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;    Application:  NONLIN
;;;;  Appl. Version:  1.3
;;;;     Appl. Date:  Oct. 1992
;;;;
;;;;           File:  .LISP
;;;;       Contents:  
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
;;;;  11/09/92 bpk - cleanup, doc.
;;;;  10/10/92 bpk - added call to store-node-expansion-info when expanding proto-phantom goal node
;;;;   9/15/92 bpk - added debug info, comments, cleanup
;;;;  10/04/91 bpk - added support for optional limit on number of execution cycles
;;;;   9/13/91 bpk - added support for saving planner input and output in globals
;;;;                *planner-in* and *planner-out*

(in-package :nonlin)

(defun plan-for ( &key (problem nil) (interactive t int-spec) (reuse nil) (reusing nil))
   "Invokes planner (Main Interace)."
   ;;; problem:  name of problem (a function)
   ;;;   If problem is nil, user will be prompted to specify problem interactively.
   ;;; interactive:  mode of operation 
   ;;;   If interactive = nil, no prompts will be displayed (only the first plan found
   ;;;   will be returned).
   ;;; reuse is ???!!!
   ;;; reusing is ???!!!

   (when int-spec
      (setq *nonlin-use-mode* (if interactive 'interactive 'batch)))

   (unless problem  ; if problem not specified, get it interactively from user
      (setq problem (get-problem)))  ; get-problem sets init-ctxt and plan schema

   ;; initialize program data structures
   (reset-context)
   (reset-datastructures)

   ;; initialize new plan
   (let (plannode)
      (funcall problem)  ; invoke problem function

      ;; create new :planhead node
      (setf *planhead*  
         (make-node :type :planhead :window (create-window (list 'at *time0*))))  ; DEVISER mod
      (set-allnodes *planhead*) ; add it to net
      ;; create new :plan node		 
      (setf plannode (make-node :type :plan)) 
      (set-allnodes plannode) ; add it to new
      (make-succnode plannode *planhead*)
      (make-prenode  *planhead* plannode)

      ;; store any scheduled events in net (DEVISER mod)
      (when *devisor-mods*
         (when (interactive-p) (format t "Storing Events..."))
         (store-events)
      )
         
      (enter-taskqueue plannode) ; initialize taskqueue

      (when (interactive-p)
         (terpri)
         (format t "~%UMCP NONLIN V1.2 (11/91).")
         (format t "~&***** BEGINNING PLANNING *****")
         (format t "~&The world state is ~s." (node-ctxt *init-ctxt*))
         (format t "~&The problem to be solved is:~%")
         (print-schema *planschema*)
      )

      ;; store planner input (initial-context-forms always-context-forms goal-forms)
      (setf *planner-in* 
         (list (node-ctxt *init-ctxt*) (node-ctxt *always-ctxt*) *goals*))
		 
      (bplanner) ; begin planning
   )
)
            

(defun planner ()
   "Expands nodes until taskqueue is empty (Main Planning Cycle)."
   ;;; called by bplanner

   ;; MAIN PLANNING CYCLE: 
   (do ((nexp (pick-taskqueue) (pick-taskqueue))) ; nexp is node at front of taskqueue
       ((null nexp)                               ; loop until taskqueue is empty
          (establish-unsupervised-conds)          ; on exiting loop, establish unsuperv conds
          (end-planning))                         ;    and call termination function 

      ;; update, print, and test cycle count    
      (setq *cycle-count* (+ *cycle-count* 1))
      (when (debug-p) (format t "~&~%### PLANNING CYCLE ~s ###" *cycle-count*))
      (when (and *cycle-limit* (> *cycle-count* *cycle-limit*)) 
         ; cycle count exceeds limit so terminate
         (when (interactive-p) 
            (format t "~&***** REACHED LIMIT OF ~s PLANNING CYCLES *****" *cycle-limit*))
         (throw :backtrack :cycle-limit)
      )

      ;; print debug info
      (when (and (debug-p) (not (member (node-type nexp) '(:plan :planhead :dummy))))
         (format t "~&*** EXPANDING ~s ~s ~s (Purpose ~s)." (node-nodenum nexp) (node-type nexp)
            (node-todo nexp) (purpose-nodes (node-nodenum nexp) (node-todo nexp))))
	
      (mark nexp)        ; mark the tasknet wrt nexp node
      (reset-plan-list)  ; reset the partial plan list	

      ;; Process current node being expanded (nexp)
      (case (node-type nexp) 
         (:plan      ; nexp is a :PLAN node (the initial goal, possibly in conjunctive form)
            (plan-node-expand nexp)) ; expand :plan node		
	 (:goal      ; nexp is a :GOAL node, so expand it
            (goal-node-expand nexp))
	 (:phantom   ; nexp is a :PHANTOM node, so expand it
	    (phantom-node-expand nexp))
	 (:action    ; nexp is a :ACTION node, so expand it
            (action-node-expand nexp)) 

         (:dummy nil)     ; nexp is a :DUMMY node, so do nothing
         (:planhead nil)  ; nexp is a :PLANHEAD node, so do nothing
         (step nil) 
      ) ; end case 

      ;;; print specified debug info (if expanding a goal, phantom, or action node)
      (if (and (debug-p) (not (member (node-type nexp) '(:dummy :plan :planhead))))
	 (then 
            (format t "~&After the expansion:~%")
            (when (debug-ds-p 'allnodes)   (print-allnodes))
            (when (debug-ds-p 'gost)       (print-gost))
            (when (debug-ds-p 'tome)       (print-tome))
            (when (debug-ds-p 'plan)       (print-plan :long t))
            (when (debug-ds-p 'plan-brief) (print-plan :long nil))
            (when (debug-ds-p 'taskqueue) 
	       (format t "~&The remaining task-queue is")
	       (print-taskqueue)
         )))

   ) ; end do
)


(defun plan-node-expand (nexp) 
   "Expand a plan node (holds top-level goals)."
   ;;; nexp: node structure
      
   (setf (snode-expanded nexp) *current-context*)        ; tag node with its expansion context
   (expand-node-using-schema-instance *planschema* nexp) ; expand using plan/problem schema
)


(defun goal-node-expand (nexp) 
   "Expand a goal node (holds top-level goals)."
   ;;; nexp: node structure
  
   ;; first check if goal is already true (in curr. context)
   (multiple-value-bind (result contributors)  
      ; result = T if goal is true
      ; contributors is list of nodes making goal true
      (try-to-establish (node-todo nexp) (node-nodenum nexp))  ; try-to-establish goal for node
      
      (if result  ; if goal is already established,
         (then				   
            ;; make a phantom node to replace goal node 
	    (let ((nchild (my-copy-node nexp)))  ; new phantom node

	       (setf (node-type nchild) :phantom)
	       (make-child nchild nexp)
	       (set-allnodes nchild (node-nodenum nexp)) ; nchild replaces nexp in the allnodes array
	       ;; phantom node is not entered into taskqueue

               ;; enter contributors for goal/phantom node in gost
	       (enter-gost (node-todo nchild) :phantom (node-nodenum nchild) contributors)

		   #|  (if (member (node-nodenum *init-ctxt*) contributors)
			 ;;if the establishment is from init-ctxt
			 (enter-tome (node-todo nchild) :assert (node-nodenum *init-ctxt*))
			 ;;enter that effect of init-ctxt into tome
			 ) |#
               (store-node-expansion-info nexp)

         ))
	 (else 	; goal is not already established
            ;; instantiating a schema to establish goal
	    (let* ((chosen-way (select-schema-to-expand nexp))) ; selected schema instance
	       (when chosen-way
                  (when (debug-ds-p 'schema-brief)
                      (format t "~&SCHEMA CHOSEN FOR EXPANSION: ~s (~s)." 
                                (schema-name chosen-way) (schema-todo chosen-way)))
	          (expand-node-using-schema-instance chosen-way nexp)  ; expand node
               )
         ))
     )      
))
	

(defun action-node-expand (nexp)
   "Expand a goal node (holds top-level goals)."
   ;;; nexp: node structure
   ;;; Does NOT check if pattern can be established in current context without expansion.

   (let* ((chosen-way (select-schema-to-expand nexp))) ; selected schema instance
      (when chosen-way
         (when (debug-ds-p 'schema-brief)
             (format t "~&SCHEMA CHOSEN FOR EXPANSION: ~s (~s)." 
                       (schema-name chosen-way) (schema-todo chosen-way)))
	 (expand-node-using-schema-instance chosen-way nexp)  ; expand node
      )
))


;;;; NOT USED
;;;; Shouldn't be called--the type would have been changed to :goal by the 
;;;; linking procedure itself..
(defun phantom-node-expand (nexp)
    ;; phantom nodes are entered into the task queue
    ;; by the linking procedure as a part of creative
    ;; destruction
    (multiple-value-bind  (already-true result)
	(q&a (node-todo nexp) nexp)
	(if already-true 
	    ; result contains contributors, update
	    ; gost
	    (enter-gost  (node-todo nexp)
		:phantom nexp result)
	;else change the node type and put the node back on the
	; task queue
	
	(else
	       (setf (node-type nexp) :goal)
	       (enter-taskqueue nexp :first)
	       ; put nexp in the beginning of the task queue
	       ; so that it will be considered in the next iteration
	))
))
    

(defun end-planning ()
   "Cleanup after planning is done."
   (let (plan)
 
      (setq plan (save-plan))              ; save final plan in *planner-out*
      (when (interactive-p)           
         (format t "~&***** PLANNING COMPLETED ******")
         (print-plan :plan plan :long t))  ; print final plan

      (gensym 1) ; ???
      (when (debug-ds-p 'print-final)
         (format t "~&The final data structures are")
         (print-allnodes)
         (print-gost)
         (print-tome)
      )
))

;;; Fix #1: lets NONLIN handle conditions of type :unsuper (unsupervised).
;;; B.Kettler 2/6/91
(defun establish-unsupervised-conds ()
   "Establish unsupervised conditions."
   ;;; After taskqueue is empty, try to establish all unsupervised conditions in the GOST. 
   
   (when (interactive-p) (format t "~&Attempting to establish any unsupervised conditions...~%"))

   ;; process all conditions of type :unsuperv in GOST 
   (for (gost-entry :in (all-gost-entrys))
      :do
         (for (gentry :in (gost-entry-pluses gost-entry))
            :do
               ;; if condition is of type :unsuperv then establish it
               (if (equal (gentry-type gentry) :unsuperv)
                  (then 
                     ; condition "condition" must be established at
                     ; node "node" with node number = "atnode"
                     (let* ((atnode (get-gentry-node gentry))
                           (node (allnodes atnode))
                           (condition (gost-entry-condition gost-entry)))
;                        (format t "Trying to establish cond ~s.~%"
;                           condition)

                        ; mark the net wrt atnode so that q&a will work 
                        (mark node)

                        ; try to establish condition
                        (multiple-value-bind (result contributors)
                           (try-to-establish condition atnode)

                           ;; if condition was established, update its
                           ;; GOST entry (i.e. add new contributor(s))
                           (if result
                              (then 
;                                 (format t 
;                                    ":unsuperv ~s established by ~s at ~s~%" 
;                                    condition contributors atnode)

                                 (enter-gost condition :unsuperv
                                              atnode contributors)
                              ) 
                              (else ; failed to establish condition
                                 (when (interactive-p) (format t
                               "Cannot establish ~s at ~s. Backtracking...~%"
                                    condition atnode))

                                 ; cannot establish condition so backtrack
                                 (backtrack)
                              ))))
                  ))))       
   (when (interactive-p) (format t "Done establishing any unsupervised conditions.~%"))
)
;;; end - Fix #1 mods

  

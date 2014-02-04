;;;-*- Mode: Lisp; Package: NONLIN -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;    Application:  NONLIN
;;;;  Appl. Version:  1.3
;;;;     Appl. Date:  Oct. 1992
;;;;
;;;;           File:  INIT-PLANNER.LISP
;;;;       Contents:  Initialize Planner
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
;;;;  11/09/92  bpk  cleanup, doc. (moved blocks probs. to blocks-sample-probs.lisp)
;;;;   1/29/92  BPK  fixed bug (occurring in CMU Common Lisp) in get-problem
;;;;  11/30/91  BPK  version 1.2

(in-package :nonlin)

;;;;
;;;;  INITIALIZATION ROUTINES
;;;;

(defun reset-globals ()
   "Reset Nonlin global variables (before each problem)."

   (setf *cycle-count* 0)

   (setf *goals* nil)
   (setf *planner-expansions* nil)
   (setf *planner-in* nil)
   (setf *planner-out* nil)
) 


(defun reset-terminate-flag ()
   "Resets *terminate*."
   (setf *terminate* nil)
)


(defun reset-datastructures ()
   "Reset Nonlin datastructures and global variables (before each problem)."

    (when *tasknet* ; save history from previous problem ???
       (push (list :tasknet     *tasknet* 
	           :plan        *allnodes* 
		   :size        *striplen* 
		   :tome        *TOME* 
		   :gost        *GOST*
		   :init-ctxt   *init-ctxt*
		   :always-ctxt *always-ctxt*
		   :planhead    *planhead*)
             **HISTORY**))

    (reset-allnodes)
    (reset-tome)
    (reset-gost)
    (reset-taskqueue)
    (reset-terminate-flag)
    (reset-globals)
)

;;;;
;;;;  DEBUGGING ROUTINES
;;;;

(defun debug-p ()
   "Returns T if any debugging is enabled."
   *debug*
)


(defun debug-ds-p (ds)  ; added 9/15/92 - bpk
   "Returns t if debugging set for a datastructure."
   (or (eq (debug-p) t)        ; debugging set for all datastructures
       (member ds (debug-p)))  ; else check specific datastructure
)


(defun debug (&optional (value t))
   "Enables debugging.  Argument is a list of structures to debug or T (debug all)."
   (setq *debug* value)
)


(defun interactive-p ()  ; added 9/18/92 - bpk
   "Returns t if Nonlin in interactive mode."

   (or (eq  *nonlin-use-mode* 'interactive) (eq *nonlin-use-mode* :interactive))
)


;;;;
;;;;  PROBLEM SETUP
;;;;

;;; 9/13/91 - Fixed bug in q&a-always-ctxt and q&a-initial-ctxt (both bomded on null ctxt)

(defun store-init-ctxt (patterns)
   "Store facts (list of ground, unnegated patterns) in initial context for a planning problem."

    ;; initial context is stored in a node with nodenum = -1
    (setf *init-ctxt* (make-node :type :init-ctxt :nodenum -1)) 

    ;; patterns are stored in (node-ctxt *init-ctxt*)
    ;; (?? dynamic binding trick so that store-pat works)
    (for (pat :in patterns)
	 :do (store-pat pat :place (node-ctxt *init-ctxt*)))
)


(defun store-always-ctxt (patterns)
    "Store facts (list of ground, unnegated patterns) in always context for a planning problem."

    ;; always context is stored in a node with nodenum = -2
    (setf *always-ctxt* (make-node :type :always-ctxt :nodenum -2))

    ;; patterns are stored in (node-ctxt *always-ctxt*)
    (for (pat :in patterns)
	 :do (store-pat pat :place (node-ctxt *always-ctxt*)))
)


(defun get-problem (&key name always inputs goals)
   "Get planning problem from user to solve."
    
    (let (
	  problem
	  plan-schema
	 )
	 
	 (unless name
		 (format t "~&Name of the Problem? ")
		 (setq name (read))
	 )
	 (unless always
		 (format t "~&Facts always true (as a list)")
		 (setq always (read))
	 )
	 (unless inputs
		 (format t "~&Facts of input situation (as a list)? ")
		 (setq inputs (read))
	 )
	 (unless goals
		 (format t "~&Goals to be achieved (as a list)? ")
		 (setq goals (read))
	 )
	 (setq plan-schema `(plan-schema ,name ,@(for (goal :in goals)
					       :save `(,(gensym "g") :goal ,goal)
					  )))
	 (setq problem (eval (list 'function `(lambda ()
				(store-init-ctxt ',inputs)
				(store-always-ctxt ',always)
				,plan-schema))))
))


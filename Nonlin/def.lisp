;;;-*- Mode: Lisp; Package: NONLIN -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;    Application:  NONLIN
;;;;  Appl. Version:  1.3
;;;;     Appl. Date:  Oct. 1992
;;;;
;;;;           File:  DEF.LISP
;;;;       Contents:  Definitions (Global Vars., Structures, Program Data Structures)
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
;;;;   4/29/03  cox  added eval to eval-when for define-node-function
;;;;   7/27/93  bpk  fixed package bug
;;;;  11/22/92  bpk  fixed various compile time probs. by adding misc. eval-when's
;;;;  11/09/92  bpk  cleanup, documentation
;;;;   9/20/92  bpk  added misc. vars
;;;;   2/29/92  bpk  added defvars for schema-parent-name, etc.
;;;;  10/04/91  bpk  added support for limit on planner cycles executed
;;;;   9/20/91  bpk  fixed bug in MAKE-CHILD
;;;;   9/13/91  bpk  added support for batch use mode

(in-package :nonlin)

;;;;  Variables for Planner Operation

(defvar *current-nonlin-version*      1.3)
(defvar *current-nonlin-version-date* "11/92")
(defvar *nonlin-loaded-p*             nil          "Nonlin program loaded?")
(defvar *nonlin-ops-loaded*           nil          "Set of Nonlin operators loaded")
(defvar *nonlin-use-mode*             'interactive "Nonlin use mode: interactive or batch")
(defvar *debug*                       nil          "List of current debugging options")
(defvar *cycle-count*                 0            "the number of cycles executed by NONLIN")
(defvar *cycle-limit*                 nil          "the limit on the num. of cycles to execute")
(defvar *planner-in*                  nil          "planner input")
(defvar *planner-out*                 nil          "planner output")
(defvar *planner-expansions*          nil          "Nonlin node expansion info (list of enode)")
(defvar *goals*                       nil          "goal specification for problem")
(defvar *terminate*                   nil          "flag for proper termination of the planner")
(defvar *nonlin-termin-status*        nil          "status of nonlin at termination")
       
(defvar *init-ctxt*                   nil          "the node holding the initial state facts")
(defvar *always-ctxt*                 nil          "the node holding the facts always true")
                                                   ;; The above nodes have node-nums -1 and -2, 
                                                   ;; respectively.  They are
                                                   ;; not referenced in TOME or GOST but their
                                                   ;; proxy - the planhead node - is.
(defvar *context-counter*             0            "generates monotonically increasing contexts")
(defvar *current-context*             nil          "defines the context for nodes and links")
(defvar *context-list*                nil          "Remembers the context history")
(defvar *init-ctxt-modification-list* nil          "remembers when a pattern was moved 
					            from *init-ctxt* to *planhead*")
(defvar *autocond*                    t             "automatically add preconds. to schemas?")

;;;;  Variables for Data Structures

(defvar      *allnodes*           nil "ALLNODES array - holds all nodes in network")
                                      ;; list of: (context . node)*.
(defvar      *striplen*           0   "length of ALLNODES")    
(defvar      MAXLEN               100 "max. length of ALLNODES")
(defconstant incr                 25  "the increment for increasing length of ALLNODES")
(defvar *planhead*                nil "the beginning (node) of the plan")
(defvar *plantail*                nil "the end (node) of the plan")        
(defvar *netmarked*               nil "num. of the node for which the net has been marked")   

(defvar *taskqueue*               nil "the task queue: list of: (context . node)")
                                      ;; holds both expanded and unexpanded nodes.
(defvar *gost* (make-hash-table)      "the GOST (a hash table of gost-entry structures)")
                                      ;; entries are hashed on condition (ground) patterns.
(defvar *tome* (make-hash-table)      "the TOME (a hash table of tome-entry structures)")
                                      ;; entries are hashed on effect patterns
                                      ;; (which may be contain variables).
(defvar *schematable* (make-hash-table) "the Schema Table (a hash table of schema structures).")
                                        ;; entries are hashed on :todo patterns.
(defvar *planschema*              nil "the schema which stores the planning problem")

(defvar *plan-list*               nil "contains the plan - a list of nodes (for printing)")
(defvar *plan-nodes*              nil "contains nodes for printing plan")


;; ???
(defvar **HISTORY**               nil "History persists between planning sessions!!")
(defvar *tasknet*                 nil "the tasknet")
(defvar *suggest* nil)

(defvar *schema-parent-name*      nil "name of schema being instantiated")
(defvar *schema-parent-names*     nil "names of possible schemas to be instantiated")

(defvar *infinity*     99999 "the maximum time")
(defvar *time0*        0     "plan execution start time")
(defvar *sched-events* nil   "list of node nums that are sched events")
(defvar *devisor-mods* nil   "DEVISER mods enabled?")         

;;;;
;;;;  NODES IN PLAN NETWORK
;;;;

(eval-when (load compile)
(defstruct (node 
              (:conc-name snode-) (:print-function print-node))
    "A node in the plan network."

    ;;; Generally use node- accessor functions to access these fields (see below).
    ;;; (Rather than snode- functions which take only a structure as an argument.)

    (id (gensym "ND"))          ; node id
    nodenum		        ; the number of the node in *allnodes*
    todo 			; the pattern with by which this node is found
    parent 			; the parent of this node (a node structure)
    expansion 			; the instantiated schema used to expand the node (a schema structure)
    expanconds  		; the list of conditions (scondition struct.) in the expansion
    children 			; list of children (nodes)
    prenodes 			; preceding nodes - list of: (context node-num*)
    succnodes 			; successor nodes - list of: (context node-num*)
    ctxt  			; the effects of this node
    type 			; the node type = :dummy :planhead :goal :action :phantom
    mark 			; the position of this node with respect to *netmarked* node:
    				; one of {:before :after :parallel :node}
    expanded                    ; nil (not expanded) or the context in which node was expanded
    nodevars                    ; variables used for node
    ;; DEVISER mods:
    (window (create-window nil)) ; time window
    (duration 0)                 ; time duration
    eventname                    ; event name (event-type nodes only)
))


(eval-when (load compile eval)  ; Added eval [mcox 29apr03]
(defun define-node-function (function-name sfunction-name)
   "Define a function node-X equivalent to snode-X, where X is a field in the node structure."
   
   ;;; node-X functions can take either a structure (node) as an argument or
   ;;; a number which is an index of a node in ALLNODES

   ;; define function:
   ;;    (defun node-X (nodearg) 
   ;;	    (typecase nodearg
   ;;		      (node    (snode-X nodearg))
   ;;		      (integer (snode-X (allnodes nodearg)))))
   (eval
      `(progn 
         (defun ,function-name  (nodearg)
            (typecase nodearg
		      (node (,sfunction-name  nodearg))
		      (integer (,sfunction-name (allnodes nodearg)))))

   ;; define setf macro for new function:
   ;;    (defsetf node-X (nodearg) (new-value)
   ;;       `(typecase ,nodearg
   ;;		       (node    (setf (,'snode-X ,nodearg) ,new-value))
   ;;		       (integer (setf (,'snode-X (allnodes ,nodearg)) ,new-value)))))
	 (defsetf ,function-name (nodearg) (new-value)
	    `(typecase ,nodearg
		       (node    (setf (,',sfunction-name ,nodearg) ,new-value))
		       (integer (setf (,',sfunction-name (allnodes ,nodearg)) ,new-value))))
       )) ; end progn
))


;; invoke the macro above to define a node- function for each snode- function
(eval-when (compile load eval)
  (let ((*correspondence-list* ; list of: (node-X snode-X)
	 '((node-nodenum    snode-nodenum)    (node-mark      snode-mark)
	   (node-type       snode-type)       (node-todo      snode-todo) 
	   (node-expansion  snode-expansion)  (node-parent    snode-parent)
	   (node-expanconds snode-expanconds) (node-children  snode-children)
	   (node-prenodes   snode-prenodes)   (node-succnodes snode-succnodes)
	   (node-expanded   snode-expanded)   (node-ctxt      snode-ctxt)
           ; DEVISER mods
           (node-eventname  snode-eventname)  (node-window    snode-window) 
           (node-duration   snode-duration)
          )
       ))
    
    (dolist (x *correspondence-list*)
    ;  (eval `(define-node-function ,(first x) ,(second x))))
       (define-node-function (first x) (second x)))
))
    

(defun print-node (node stream depth)
  "Print a node structure."

  (let ((*print-gensym* nil) ; print the gensym without sharpsign
	(todo      (snode-todo      node))
	(parent    (snode-parent    node))
	(type      (snode-type      node))
	(children  (snode-children  node))
        (mark      (snode-mark      node))
        (duration  (snode-duration  node))  ; DEVISER mods
        (window    (snode-window    node))
        (eventname (snode-eventname node))
       )
            
    (format stream "{")
    (format stream "<~s>" (snode-id node))

    (if (or type todo) (format stream "["))
    (if type (format stream "~s" type))
    (if todo (format stream "~s" todo))
    (if eventname (format stream "(~s)" eventname)) ; DEVISER mod
;    (if mark (format stream "mk=~s" mark))

    (when *devisor-mods*
       (format stream " dur=~s wdw=~s " duration window))
    (if (or type todo) (format stream "]"))
    (when *devisor-mods* (format t "~%~35T"))  ; DEVISER mods

    (if (not (> depth 0))
	(then
         (let ((prenodes  (get-prenodes node))
               (succnodes (get-succnodes node)))
	  (if prenodes (format stream "{PRE:~s}" prenodes))
	  (if succnodes (format stream "{SUCC:~s}" succnodes))
	  (if parent (format stream " PAR:{<~s>} " (snode-id parent)))
;	  (if children
;	      (progn (format stream "CHL:(")
;		     (for (child :in children)
;			  :do (format stream "{<~s>}"  (snode-id child)))
;		     (format stream ")")))
	  ))
    (format stream "}"))
;   (format stream "CTXT: ~s " (snode-ctxt node))
))


(defun make-succnode (snode node)
  "Install snode as a successor of node in current context. Returns successors of node."

  ;;; (node-succnodes node) is list of: (context node-num*)

  (if (member (node-nodenum snode) (get-succnodes node)) 
      ; then snode is already a successor
      (node-succnodes node)
      (else
         ;; if current context is the same as the context of the successor added most recently,
         (if (equal *current-context* (get-context (car (node-succnodes node))))
            ;; then just add snode to list of successors
            (setf (node-succnodes node) 
                     ; add snode's node num to first (context node-num*) sublist.
                     (push (append1 (car (node-succnodes node)) (node-nodenum snode)) 
                           (cdr (node-succnodes node))))
            ;; else add (new-context snode-node-num) to list of successors
            (setf (node-succnodes node)
                     (push (add-context (append1 (get-succnodes node) (node-nodenum snode))) 
                           (node-succnodes node)))
      )) ; end else
  )
)


(defun make-prenode (pnode node)
  "Install pnode as a predecessor of node in current context. Returns predecessors of node."

  ;;; (node-prenodes node) is list of: (context node-num*)

  (if (member (node-nodenum pnode) (get-prenodes node)) 
      ; then pnode is already a predecessor
      (node-prenodes node)
      (else
         ;; if current context is the same as the context of the predecessor added most recently,
         (if (equal *current-context* (get-context (car (node-prenodes node))))
            ;; then just add pnode to list of predecessors
            (setf (node-prenodes node) 
                     ; add pnode's node num to first (context node-num*) sublist.
                     (push (append1 (car (node-prenodes node)) (node-nodenum pnode)) 
                           (cdr (node-prenodes node))))
            ;; else add (new-context pnode-node-num) to list of predecessors
            (setf (node-prenodes node)
                     (push (add-context (append1 (get-prenodes node) (node-nodenum pnode))) 
                           (node-prenodes node)))
      )) ; end else
  )
)


;; FIXED 9/18/91
(defun make-child (cnode node)
  "Make cnode a child of node."

  (unless (node-p node) (setf node (allnodes node)))

  (setf (node-children node) (pushnew cnode (node-children node)))
  (setf (node-parent cnode) node)
)


(defstruct ekid  "info about child of expanded node"
   id     ; id of child node
   type   ; type of child node
   todo   ; pattern of child node
)


(defstruct enode "info about expanded node"
   context ; expansion context tag
   id     ; id of node being expanded
   type   ; type of node being expanded
   todo   ; pattern of node being expanded
   schema ; name of schema used to expand node
   kids   ; children of node (list of expkid)
)

;;;;
;;;;  ALLNODES (THE PLAN NETWORK)
;;;;

(defun allnodes (nodenum)
   "Returns node structure in ALLNODES[nodenum]."

    (cond ((not (numberp nodenum))
	     (cerror "~s" "Function allnodes is getting a non-number argument."))
	  ((eql nodenum -1) *init-ctxt*)    ; ALLNODES[-1] is the initial context
	  ((eql nodenum -2) *always-ctxt*)  ; ALLNODES[-2] is the always  context  

          ((> nodenum (1- *striplen*))      ; nodenum is out of bounds
             (cerror "~s" "The nodenumber is more than striplength."))
          ; else nodenum is valid, return ALLNODES[nodenum].
          (t (cdar (aref *allnodes* nodenum))))  ; returns latest (first) node in ALLNODES[nodenum]
)


(defun print-allnodes (&optional (stream t) (depth 0))
  "Print contents of ALLNODES for current context."

  (format stream "~&CONTENTS of ALLNODES:")
  (do ((index 0 (1+ index)))
      ((> index (1- *striplen*)))
      (unless (null (aref *allnodes* index))
         (format stream "~&~5T~s " index)
         ; print node in first (context . node) sublist in ALLNODES[index]
         (print-node (cdar (aref *allnodes* index)) stream depth)
      ))
) 

;; set-allnodes puts node at *allonodes*[nodenum]. This works in a fashion
;; similar to make-prenode/make-succnode i.e. checks the current context 
;; with the latest context and creates a new context if they differ. 

(defun set-allnodes (node &optional (nodenum *striplen*))
   "Add a node to ALLNODES[nodenum] for current context."

   ;; if nodenum not in bounds, adjust the size of the array
   (unless (array-in-bounds-p *allnodes* nodenum)
      (setf maxlen (+ maxlen incr))
      (adjust-array *allnodes* (list maxlen) :initial-element nil)
   )

   (setf (node-nodenum node) nodenum) ; set the node number for the new node

   ;; if the latest context for ALLNODES[nodenum] is the same as the current context,
   (if (equal *current-context* (get-context 
                                   (if (atom (aref *allnodes* nodenum)) 
                                       nil
                                       (car (aref *allnodes* nodenum)))))
       ;; then just add node to first sublist of ALLNODES[nodenum]
       (setf (aref *allnodes* nodenum) 
                (push (add-context node) (cdr (aref *allnodes* nodenum))))
       ;; else create a new context and add (new-context . node) to ALLNODES[nn]
       (setf (aref *allnodes* nodenum) 
                (push (add-context node) (aref *allnodes* nodenum))))
   ;; if the node number plus 1 is greater than the current strip length
   ;; (the plus 1 is to take care of 0 offset array)
   (when (> (+ nodenum 1) *striplen*)
	;; Make *striplen*gth to be nodenum.
        ;; This will ensure that the strip length is not changed when a new node is placed 
        ;; in the middle.
	(setq *striplen* (+ nodenum 1))
   )
)


(defun reset-allnodes ()
   "Initialize ALLNODES."

    (setq *striplen* 0 *MAXLEN* 100)
    (setf *allnodes* (make-array (list MAXLEN) :adjustable t :initial-element nil))    
)    

;;;;
;;;;  SCHEMAS, SCHEMA CONDITIONS, and SCHEMA EFFECTS
;;;;
        
(defstruct schema  "an opschema or actschema"
    (id (gensym "SCH"))        
    name                         ; name of opschema/actschema
    todo 	                 ; this is the pattern by which schema is fetched
    strip 			 ; array of node structures for schema expansion
    size                         ; the size of the node array
    conditions 		         ; list of scondition structures
    effects			 ; list of seffect structures
    (duration 0)                 ; duration of action/event - DEVISER mods
    (window (create-window nil)) ; window for action/event
    vars                         ; variables used in schema
)


(defun print-schema (sch &optional (stream t) (depth 0))
  "Print schema structure."

  (let ((*print-array* t)    ; print arrays readably
        (*print-gensym* nil) ; disable printing of leading #
	(conditions (schema-conditions sch))
	(effects    (schema-effects    sch))
	(strip      (schema-strip      sch))
	(size       (schema-size       sch))
	(todo       (schema-todo       sch))
	(variables  (schema-vars       sch))
        (duration   (schema-duration   sch)) ; DEVISER mods
        (window     (schema-window     sch))
       )

    (format stream "~&{~s} ~%" (schema-id sch))
    (format stream "~2T ~s::~s~%" (schema-name sch) todo)
    (when strip
	  (format stream "~5T Expansion:~%")
	  (do ((index 0 (+ index 1)))
		  ((> index (- size 1)))
		(format stream "~10T ~s " index)
		(print-node (aref strip index) stream (1+ depth))
		(format stream "~%")
		)
    )
    (when conditions
	  (format stream "~5T Conditions:~%")
	  (for (scond :in conditions)
	      :do (format stream "~10T ~s" scond))
    )
    (when effects
	  (format stream "~& ~5T Effects:~%")
	  (for (seff :in effects)
	       :do (format stream "~10T ~s" seff))
    )
    (when *devisor-mods* ; DEVISER mods
       (format stream "~& ~5T Time: dur=~s  wdw=~s ~%"
          duration window))
    (when variables
       (format stream "~5T Vars: ~s~%" variables))
))
    

(defstruct (scondition (:print-function print-scondition))
    "Schema condition."
    (id (gensym "SC"))
    type			; :use-when, :precond or :use-only-for-query
    pattern			; condition pattern
    atnode			; number of node where condition should hold at
    contributors		; numbers of nodes with effects contributing this condition
    binding			; variable bindings (kept for rebinding use-only-for-query conds)
)


(defun print-scondition (scond stream depth)
  "Print schema condition structure (scond)."

  (let ((*print-gensym* nil))
    (format stream "<<~s>>" (scondition-id scond))
    (if (< depth 1)
	(then-let ((type (scondition-type scond))
		   (pattern (scondition-pattern scond))
		   (atnode (scondition-atnode scond))
		   (contributors (scondition-contributors scond))
		   )
		  (if type (format stream " ~s " type))
		  (if pattern (format stream "~s" pattern))
		  (if atnode (format stream " :at ~s " atnode))
		  (if contributors (format stream " :from ~s " contributors))
		  (if (eql type :use-only-for-query) 
                     (format stream " :binding ~s " (scondition-binding scond)))
		  (format stream "~%")
		  ))
))
    

(defstruct (seffect (:print-function print-seffect))
    "Schema effect."

    (id (gensym "SE"))
    type                ; :assert or :delete
    pattern             ; effect pattern
    atnode              ; number of node giving this effect
)    

        
(defun print-seffect (seff stream depth)
  "Print schema effect."

  (let ((*print-gensym* nil))
    (format stream "<<~s>>" (seffect-id seff))
    (if (< depth 1)
	(then-let ((type (seffect-type seff))
		   (pattern (seffect-pattern seff))
		   (atnode (seffect-atnode seff))
		   )
		  (if type (format stream " ~s " type))
		  (if pattern (format stream "~s" pattern))
		  (if atnode (format stream " :at ~s " atnode))
		  (format stream "~%")
		  ))
))

;;;;
;;;;  TASKQUEUE
;;;;
    
(defun enter-taskqueue (node &optional first)
    "Enter a node into the taskqueue. If first = :first, insert node at front else insert at rear."

    (unless (node-p node) (setf node (allnodes node)))

    ;; insert node with a new context
    (if (eq first :first)
	(push (add-context node) *taskqueue*)                        ; insert at front
	(setf *taskqueue* (append1 *taskqueue* (add-context node)))  ; insert at rear
    )
)
    

(defun pick-taskqueue ()
   "Remove and return a pending task (unexpanded node) from the taskqueue."

   ;;; Currently this just picks the first node in the taskqueue.
   ;;; Change this function to implement different search strategies.

   (let ((tasknode (car ; get first of those nodes NOT expanded:
                        (for (node-context :in *taskqueue*)
		            :when (null (snode-expanded (cdr node-context)))
			    :save (cdr node-context)))))
       (when tasknode   ; remember context in which this node was chosen (expanded)
	   (setf (snode-expanded tasknode) *current-context*))

       (return-from pick-taskqueue tasknode)
))


(defun print-taskqueue ()
   "Prints all pending (unexpanded) nodes in taskqueue."

  (for (node-context :in *taskqueue*)
       :do (let ((node (cdr node-context)))
	        (when (null (snode-expanded node))
                         (format t "~& ~s ~s ~s" (node-nodenum node) (node-type node) 
                                                 (node-todo node)))))
)
    

(defun reset-taskqueue ()
   "Initialize taskqueue."
    (setf *taskqueue* nil)
)        

;;;;
;;;;  GOST
;;;;

;; GOST functions are in gost.lisp
  
(defstruct (gost-entry (:print-function print-gost-entry))
    "GOST table entry."
    condition  ; a ground pattern
    pluses     ; list of gentry structures where condition holds
    minuses    ; list of gentry structures where the NEGATED condition holds
)

				
(defstruct (gentry  (:print-function print-gentry))
    "GOST condition entry."
    node         ; list of: (context . nodenum), where nodenum is num. of node is where 
                 ; condition holds in the context.
    type         ; condition type - :phantom, :use-when, or :precond
    contributors ; list of: (context nodenums), where nodenums are nums. of nodes that supply
                 ; the condition in the context.
)

;;;;
;;;;  TOME
;;;;
  
;; TOME functions are in tome.lisp

(defstruct (tome-entry (:print-function print-tome-entry))
  "TOME entry."
  effect    ;  a ground pattern
  asserts   ;  list of: (context nodenums), where nodenum designates node that asserts pattern
            ;  in context.
  deletes   ;  list of: (context nodenums), where nodenum designates node that deletes pattern
            ;  in context.
)         
    
;;;;
;;;;  LINEARIZATION DATA
;;;;
        
(defstruct (suggest-entry (:print-function print-suggest-entry))
   "Suggested link to remove."
   link		  ; the link which must be removed. 
   cond		  ; the offender condition, i.e. the interacting condition.
   contributor    ; list of at most two (node node-purpose) pairs. If it
		  ; contains two pairs, then the first pair supplies the 'cond'
		  ; and the other pair supplies ~'cond'.
)


(defun print-suggest-entry (suggest-entry stream depth)
   "Print suggested link to remove."

    (declare (ignore depth))
    (format stream "link:~s  Remove-contributor:~s ~%" (suggest-entry-link suggest-entry)
            (suggest-entry-contributor suggest-entry))
)

;;;;
;;;;  BACKTRACKING ENTRIES
;;;;

(defstruct (backtrack-entry (:print-function print-backtrack-entry))
   "Backtracking point."
   type         ; :schema (alternative schemas), :expand (alternative linearizations), 
                ; or :establish (alternative contributors)
   alternatives ; list of alternative choices
)


(defun print-backtrack-entry (backtrack-entry stream depth)
   "Print backtrack entry."
   (declare (ignore depth))
   (format stream "TYPE:~s ALTERNATIVES:~s~%" 
      (backtrack-entry-type backtrack-entry)
      (backtrack-entry-alternatives backtrack-entry))
)


;; changed structure name from window to twindow to avoid conflict with CLOS class window
;; access function names remain "window-" - bpk 11/91

(defstruct (twindow (:print-function print-window)
                    (:conc-name window-) 
                    (:constructor make-window)
                    (:copier copy-window)
           )   
   "time window structure"
   (est *time0*)    ; earliest start time
   (ist nil)        ; ideal start time
   (lst *infinity*) ; latest start time
)

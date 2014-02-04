;;; -*- Mode: LISP; Syntax: Common-lisp; Package: Meta-aqua; Base: 10 -*-

(in-package :metaaqua)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	    The Meta-AQUA Introspective Multistrategy Learning System
;;;;				   Version 6
;;;;
;;;;	     Copyright (C) 1996   Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;			      File: lowlevel.lisp
;;;;
;;;;
;;;;	      *******************************************************
;;;
;;; This  program is  free  software; you can redistribute it and/or  modify it
;;; under the terms  of the GNU General Public License as published by the Free
;;; Software  Foundation;  either  version  1,  or  (at  your option) any later
;;; version.
;;; 
;;; This program is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without  even the  implied  warranty  of  MERCHANTABILITY  or
;;; FITNESS FOR  A PARTICULAR PURPOSE.  See the GNU General Public  License for
;;; more details.
;;; 
;;; You should  have received a copy of  the  GNU  General Public License along
;;; with this program; if not, write to the Free Software Foundation, Inc., 675
;;; Mass Ave, Cambridge, MA 02139, USA.  In emacs type C-h C-w to view license.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;      UTILITIES AND MISCELLANEOUS FUNCTIONS FOR META-AQUA
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;
;;; THE *Say-Way* PROPERTY
;;; 
;;; The *Say-Way* property is used to attach the English equivalent
;;; descriptions of instantiations (tokens) to the frame representations of the
;;; instantiations. The following functions support this feature.
;;; 

(defvar *say-way* 'say-way
  "Text equivalent string for frame is placed on this property.")


(defun do-say (symbol)
  (get symbol *say-way*)
  )


(defun do-add-say (symbol descr)
  (setf (get symbol *say-way*) descr)
  )


(defun say-xp (xp)
  (do-say (frame-type xp)))


;;;
;;; Function say-input is the first feedback the user receives for a new
;;; 'understands goal input into Meta-AQUA. It tells whether or not the goal
;;; originated outside the program or was a goal to understand the structure
;;; inferred by the script applier.
;;;
;;; ||||||Note that this needs to be fixed when the script applier starts to
;;; handle some of the hand-coded examples. Also, I am not sure what is
;;; happening with other modes (e.g., as with Meta-AQUA calls of ripsau) any
;;; longer. [cox 25feb95]
;;; 
(defun say-input (new-input spinqua-input?)
  ;; Was there not a function to do the following?
  ;; Yes - say-xp, but it was for xps and did not have the additional bagage.
;  (with-character-style (*Style*)
    (format
      *aqua-window*
      (if
	(and
	  spinqua-input?			; If the input from spinqua (Tale-Spin),
	  (not					; then the structure is inferred if the 
	    (member new-input			; new input is not in the story concepts.
		    *Story-Concepts*
		    :test			; Cannot use this test if hand-coded input
		    #'(lambda (x y)		; since *story-concpets* does not have instantiated
			(equal x (first y))	; frame variable, but rather, has patterns with 
			))))			; variable bindings.
	"~%Inferred Structure: ~s"
	"~%Input Structure: ~s"
	)
      new-input)
    (format
      *aqua-window*
      "~%  ~s~%"
      (or (do-say new-input)
	  (do-say (frame-type new-input))))
;    )
  
  )


;;;
;;; Function say-sub-goal is used to display the current subgoal being
;;; processed by Meta-aqua at the beginning of a new cycle of reasoning.
;;;
(defun say-sub-goal (sub-goal)
;  (with-character-style (*Style*)
    (format
      *aqua-window*
      "~%Current Sub-Goal: ")
    (format
      *aqua-window*
      "~%~a ~a~%"
      (case (first (get-abstraction
		     (goal-object sub-goal)))
	(id "  Identify interesting concepts in")
	(generate "  Generate explanation for")
	(test "  Verify hypothesis")
	(review/learn "  Review reasoning trace")
	(t "Other info "))
      (if (and
	    (eq 'generate
		(first (get-abstraction
			 (goal-object sub-goal))))
	    (isa-p 'relation (list (goal-state sub-goal))))
	  (str-concat
	    "why "
	    (string
	      (f.get (goal-state sub-goal)
		     *co-domain-slot*))
	    " decides to perform "		  
	    (string
	      (f.get (goal-state sub-goal)
		     *domain-slot*)))
	(string (goal-state sub-goal))))
;    )
  
  )


;;; 
;;; The following 4 calls of do-add-say used to be putprop calls on *say-way*
;;; in the rep_smuggle4.lisp file. The 5th was in file rep_planner.lisp. The
;;; sixth is Janis' new addition.
;;;

(do-add-say
  'XP-HUNGRY-BARK
  "Seals bark at objects when hungry.")

(do-add-say
  'XP-DEFENSIVE-BARK
  "Dogs bark at objects when threatened.")

(do-add-say
  'XP-INJURY-HIT
  "People hit animate-objects when they want to hurt them.")

(do-add-say
  'XP-INSTRUMENTAL-SCENE->ACTOR
  "Actor does action because it enables another more important action.")

(do-add-say
  'XP-GOAL-OF-OUTCOME->ACTOR
  "Actor does action because it achieves a goal the agent desires.")

(do-add-say
  'XP-TYPICAL-ACTION-ANIMATES->ACTOR
  "Actor does action because this kind of actor normally does this action.")

(do-add-say
  'XP-DISABLE-ACT-PRECONDITION
  "Actor does action because he knows that the plan which this action is an  instrumental act for will lead to satisfying the goal of disenabling the undesired action.")
 
(do-add-say
  'smoke-pipe
  "Actor smokes a pipe.")

(do-add-say
  'fill-pipe
  "Actor fills the pipe.")

(do-add-say
  'close-container
  "The actor closes the container.")

(do-add-say
  'open-container
  "The actor opens the container.")

(do-add-say
  'disembark-plane
  "Actor disembarks from plane.")




;;;
;;; MISCELLANEOUS FUNCTIONS.
;;; 


;;;
;;; Function print-goal-status simply prints the current value of the
;;; *Goal-Queue*, the goal to be processed next, and the state that the goal
;;; wishes to achieve.
;;;
;;; The function always returns true so that it will not effect the termination
;;; condition of the main body of function ripsau (the only place this function
;;; is used.
;;; 
(defun print-goal-status (next-goal goal-queue &optional calling-function)
  (if (goal-monitor-mode-p)
      (let ((previous-window *aqua-window*))
	(current-window *window3*)
	(format-if
	  calling-function
	  *aqua-window*
	  "~%Called by function ~s."
	  calling-function)
	(format
	  *aqua-window*
	  "~%~%Goal Queue  --> ~s~%"
	  (list-queue goal-queue))
	(format
	  *aqua-window*
	  "Next Goal   --> ~s~%"
	  next-goal)
	(f.pprint next-goal)
	(format
	  *aqua-window*
	  "~%Goal-State  --> ~s ~s ~s~%"
	  (goal-actor next-goal)
	  (first (get-abstraction
		   (goal-object next-goal)))
	  (goal-state next-goal
		      ))
	(current-window previous-window)))
  t)



(defun print-cycle-division (&optional title-string)
  (let ((previous-window *aqua-window*))
    (current-window *window3*)
    (format
      *aqua-window*
      (str-concat
	"-----------------"
	(or title-string
	    "new-cycle")
	"-----------------"))
    (current-window previous-window))
  t)



;;;
;;; Predicate user-quits-p returns whether or not the system should stop
;;; processing. If the system is in automatic mode (automatic? = t), nil is
;;; returned. If it is in semi mode and the next goal to be processed is a
;;; understands goal (or if automatic is another value), then the user is
;;; prompted for this information.
;;;
;;; The function always has the side effect of displaying the upcoming goal for
;;; the user to see so he can make a decision if prompted.
;;; 
(defun user-quits-p (automatic? next-goal)
  (print-goal-status				; Print system goals.
    next-goal *Goal-Queue*)
  (if (or (goal-monitor-mode-p)			; If using the internal structures window
	  (memory-monitor-mode-p))
      (print-cycle-division))			; Mark a division between processing cycles on screen.
  (not
    (or
      (eq automatic? t)				; Fully automatic  or ..
      (and (eq automatic? 'semi)		; Semi-automatic runs do not stop for 
	   (isa-p				; understand goal  or ..
	     'understands
	     (list
	       (f.get next-goal
		      'goal-object))))
      (y-or-n-p					; Allow the user to quit.
	"~%Continue ? ")))
  )


;;;
;;; Function assert-truth assigns the given truth value to the given frame and
;;; all its subframes, unless there already exists a truth value for a
;;; particular frame. The optional parameter notify determines whether or not
;;; the user is notified of these pre-existing truth slots. The frame is
;;; returned by the function.
;;;
;;; Use of this function with frames that are already hooked into either the
;;; world or reasoning models should be limited. Truth values may be propagated
;;; further than desired.
;;; 
(defun assert-truth (truth-value frame &optional notify)
    (f.traverse-frame
      frame
      #'assert-as
      truth-value
      notify)
  frame)


;;;
;;; Function assert-as is the actual helper function that does the work for
;;; function assert-truth.
;;;
(defun assert-as (current-frame parent role facet-name level truth-value notify)
  (if (not (or (literal-p current-frame)
	       (attribute-value-p current-frame)))
      (format-if
	(and (not (f.put truth-value current-frame *truth-slot*))
	     notify)
	*aqua-window*
	"Frame ~s already has ~s as value of truth slot."
	current-frame
	(f.get current-frame *truth-slot*)))
  )




;;;
;;; *INTERNAL-STRUCT-WINDOW*  FLAG
;;; 
;;; Data structure for flag to determine whether to use *window3* for
;;; displaying goal priority-queue or displaying memory. The first mode will be
;;; called goal-monitor-mode, whereas the second will be called memory-monitor
;;; mode.
;;; 



(defvar *internal-struct-window* nil
  "Determines whether the internal structures window shows behavior of goal queue or memory.")

(defconstant  *shows-goals* 'show-goal-queue)
(defconstant  *shows-memory* 'show-memory)

(defun init-struct-window-flag ()
  (setf *internal-struct-window* *shows-goals*)
  )


;;;
;;; Function toggle-structures-window is used by the system user to change the
;;; mode by which the Internal Structures Window works. The optional parameter
;;; active-window? can be used to disable the Internal Structures Window by
;;; passing it a nil value.
;;; 
(defun toggle-structures-window (&optional (active-window? t))
  (if active-window?
      (let ((previous-window *aqua-window*))
	(cond ((memory-monitor-mode-p)
	       (setf *internal-struct-window* *shows-goals*))
	      ((goal-monitor-mode-p)
	       (setf *internal-struct-window* *shows-memory*))
	      (t				; otherwise we are re-enabling the window.
	       (setf *internal-struct-window* *shows-goals*)))
	(current-window
	  *window3*
	  (str-concat
	    "Internal Structures Window  "
	    (if (goal-monitor-mode-p)
		"- Goal Priority-Queue"
		"- Memory Monitor")))
	(current-window previous-window))
      (setf *internal-struct-window* nil))
  )


(defun goal-monitor-mode-p ()
  (eq *internal-struct-window* *shows-goals*)
  )




(defun memory-monitor-mode-p ()
  (eq *internal-struct-window*
      *shows-memory*)
  )


;;;
;;; WINDOW HANDLING FUNCTIONS
;;;


;;;
;;; Function screen-init is called by init-run at the start of each program
;;; run. It exposes the two display screens (LISP-Listeners) and clears any old
;;; output. If the mode is different than the previous mode, then the function
;;; calls prep to initialize the screen, as is done by init-aqua.
;;; 
(defun screen-init (mode previous-mode)
  (cond ((equal mode previous-mode)
	 (cond ((action-mode-p mode)
		(current-window *window2*)
		(cls)))
	 (current-window *window3*)
	 (cls)
	 (current-window *window1*)
	 (cls))
  	(t
	 (prep mode)))
  )




;;;
;;; Make the global variable *aqua-window* (which points to the current output
;;; window) set to the window parameter. Bind terminal i/o and query i/o to the
;;; window. Optionally set the window label and make sure that when deexposed
;;; the screen does not grey out. Then finally expose the window for
;;; processing.
;;; 
(defun current-window (window &optional window-label do-init)
  nil
;  (setf *aqua-window* window)
; Commented out the next 3 lines.[cox 6mar97]
;  (if (not (null window-label))
;      (send *aqua-window* :set-label window-label))
;  (send *aqua-window* :expose)
;  (if do-init
;      (tv:set-screen-deexposed-gray nil))
;Commented out next6 2 [cox 6mar97]
;  (setf *terminal-io* *aqua-window*)
;  (setf *query-io* *aqua-window*)
  )




;;;
;;; STATUS AND TRUTH SLOT HANDLING
;;; 


;;;
;;; Predicate answers question:
;;; Is frame input from the story?
;;; 
(defun story-instance-p (frame)
  (equal (f.get frame *status-slot*) *story-instance*)
  )



;;;
;;; Function add-story-status marks each non-attribute subframe of and
;;; including new-story-frame as being an input concept from a story.
;;; 
(defun add-story-status (new-story-frame)
  (f.traverse-frame
    new-story-frame
    #'(lambda (frame parent slot facet level)
	(if (not (or (visited-p frame)
		     (attribute-value-p frame)))
	    (mark-as-story-instance frame))))
  new-story-frame
  )


;;; |||||
;;; In this simple version, all story-instances are naively believed,
;;; ie. truth slot marked as in, as in the original version of AQUA.
;;; However we eventually want to be able to override this or have suspicions 
;;; about this assumption. 
;;;
;;; BEWARE that this function does NOT check first to see whether the
;;; new-instance is an attribute or not.
;;; 
(defun mark-as-story-instance (new-instance)
  (when (and (frame-var-p new-instance)
	     (not (literal-p new-instance)))
    (f.put *story-instance* new-instance *status-slot*)
    (mark-in new-instance))
  new-instance
  )



;;;
;;; Function mark-as-believed recursively marks each subframe of
;;; frame (as well as frame itself) as being in the set of beliefs.
;;; Exceptions not marked are attribute values and fillers having a
;;; list as their value. ||||| This should be changed so that the
;;; function runs down the frmae lists recursively also. There is a
;;; possible conflict with the cycles when doing this however.
;;; 
(defun mark-as-believed (frame)
  (f.traverse-frame
    frame
    #'(lambda (frame parent slot facet level)
	(if (not (or (visited-p frame)
		     (attribute-value-p frame)
		     (frame-list-p frame)))
	    (mark-in frame))))
  frame)



;;;
;;; Function mark-in simply assign the *in* value to the *truth-slot*
;;; of the given frame.
;;; 
(defun mark-in (frame)
  (f.put *in* frame *truth-slot*)
  )




;;;
;;; ANOMALY DETECTION
;;; 
;;; The following routines support anomaly detection and indexing.
;;; 


;;;
;;; Function declare-anomaly simply prints the discovery of an anomaly.
;;; 
(defun declare-anomaly (concept anomaly-type path)
;  (with-character-style (*Style*)
    (format
      *aqua-window*
      (str-concat
	"~%~%Anomaly detected: "
	"Odd for a ~s~%  to be in "
	"path ~s of a ~s.")
      anomaly-type path concept)
;    )
  
  )


;;;
;;; Function anomaly-check acts as a predicate to determine the presence of an
;;; anomaly. Currently, an anomaly exists if the actual filler in a frame is
;;; not of the same type as the proto-type declared in concept definitions for
;;; that filler by the Representation files (rep_*.lisp). If the prototype is
;;; an attribute value, then the two are checked for strict equivalence. If
;;; the two are literals, then their frame values are tested for equivalence
;;; using equal (this is so that two copies of the list '(literal) will
;;; equate). Otherwise the actual filler must be isa the prototype's type and
;;; the actual filler must not itself be anomalous. These kinds of anomalies
;;; generate the Incorporation Failure type when reading a story.
;;;
;;; The return value is a list of anomaly paths if the actual filler is
;;; anomalous, t if any of the other tests fail, nil otherwise. If the
;;; function returns t, then the anomaly is caused by the actual filler. If a
;;; list is returned, then the anomaly is deeper in the actual filler's frame
;;; structure.
;;;
;;; |||||| NOTE that the actual filler may be anomalous with respect to the
;;; definition of a frame it is contained in, *AND* the filler itself may be
;;; anomalous with respect to its own definition. In this case, the function
;;; will return the list, but will not signal the former anomaly. Need to fix
;;; this. [11apr94]
;;; 
(defun anomaly-check (prototype-filler actual-filler check-list)
  (let* ((anomalous-paths nil)
	 (exists-anomaly
	   (not
	     (if (attribute-value-p
		   prototype-filler)
		 (eq prototype-filler actual-filler)
		 (if (and (literal-p prototype-filler)
			  (literal-p actual-filler))
		     (or (equal (*FRAME* prototype-filler)
				literal)	; Dummy literal
			 (equal (*FRAME* actual-filler)
				literal)	; Dummy literal
			 (equal (*FRAME* prototype-filler)
			    (*FRAME* actual-filler)))
		     (and
		       (or
			 (isa-p
			   (frame-type prototype-filler)
			   (list actual-filler))
			 (isa-p
			   (frame-type actual-filler)
			   (list prototype-filler)))
		       (not (setf anomalous-paths
				  (is-anomalous
				    actual-filler check-list)))))))))
    (or anomalous-paths
	exists-anomaly))
  )




;;;
;;; Predicate test-criteria-p returns true if it is OK to check for anomalies
;;; between the given filler and the proto-type, nil otherwise. This occurs if
;;; the filler is a non-list value filler and the prototype has a corresponding
;;; non-nil filler.
;;; 
(defun test-criteria-p (facet filler prototype-filler)
  (and
    (equal *value-facet*
	   (facet->facet-name
	     facet))
    (not (frame-list-p filler))
    (not (null prototype-filler)))
  )


;;;
;;; Function append-each-in-result-to appends the path prefix to each list in
;;; the list-of-suffixes.  The resulting list is returned.
;;; For instance,
;;;        (append-each-in-result-to
;;;           '(actor domain)
;;;           '((at-location co-domain) (object color)))
;;;    --> '((actor domain at-location co-domain) (actor domain object color))
;;; 
(defun append-each-in-result-to (path-prefix list-of-suffixes)
  (let ((result nil))
    (dolist (each-suffix list-of-suffixes)
      (setf result
	    (cons (append
		    path-prefix
		    each-suffix)
		  result)))
    result)
  )


;;;
;;; Function recursive-check is called by is-anomalous in order to search for
;;; anomalies in sub-frames of the high-level concept passed to is-anomalous.
;;; It must be able to construct the paths to the anomalies returned by
;;; anomaly-check.
;;;
;;; The input prototype is an instantiated copy of the frame definition of the
;;; original concept being checked for anomalies. The path is a list of roles
;;; that traverse the path from the original concept, into its hierarchical
;;; structure to the current-concept being checked. Thus, the first time
;;; recursive-check is called (called by is-anomalous) the current-concept is
;;; the original concept, the prototype is the instantiation of the frame type
;;; of the original concept, and the path is nil.
;;;
;;; The function checks for anomalies by calling anomaly-check on each filler
;;; from the current-concept that passes the test criteria predicate against
;;; the corresponding proto-type-filler, and then recursively calling itself
;;; on each of these current-concept fillers with the path extended to point
;;; at the new current-concept.
;;; 
;;; 
(defun recursive-check (prototype path current-concept check-list)
  (do-break recursive-check)
  (let ((anomalies nil))
    (dolist (each-slot (f.slot-list current-concept))
      (let ((role (slot->role each-slot)))
	(dolist (each-facet (slot->facets each-slot))
	  (let* ((filler (facet->filler each-facet))
		 (new-path (append path (list role)))
		 (prototype-filler
		   (apply #'f.chase-path
			  (cons prototype
				new-path))))
	    (cond ((test-criteria-p
		     each-facet filler prototype-filler)
		   (let ((anomaly-check-result
			   (anomaly-check prototype-filler filler check-list)))
		     (cond (anomaly-check-result
			    (setf anomalies
				  (cond ((listp anomaly-check-result)
					 (append
					   (append-each-in-result-to
					     new-path
					     anomaly-check-result)
					   anomalies))
					(t
					 (declare-anomaly
					   (frame-type prototype)
					   (frame-type filler)
					   new-path)
					 (cons
					   new-path
					   anomalies))))
			    )))
		   ;; The if test also avoids infinite loops. There is no need to
		   ;; check for anomalies within attribute values anyhow. [cox 1may94]
		   (if (not (attribute-value-p filler))	
		       (let ((sub-roles (recursive-check
					  prototype
					  new-path
					  filler
					  check-list)))
			 (if sub-roles
			     (setf anomalies
				   (append
				     anomalies
				     sub-roles)))))))))))
    anomalies))


;;;
;;; Function is-anomalous is the main anomaly detection function. It checks to
;;; see if the input concept is filled with normally expected values by
;;; calling function recursive-check, passing it an instantiated copy of the
;;; concept's frame definition (to act as prototype), nil (signaling a null
;;; path into the structure searched so far), and the concept to be checked
;;; for anomalies. It returns a list of paths into the input concept where
;;; anomalies occur, with duplicate paths removed. Note that literals are
;;; never considered anomalous.
;;; 
;;; Is-anomalous acts as a predicate, returning nil if no anomalies exist
;;; within the concept, or returning the list of paths to the anomalies
;;; otherwise.
;;;
;;; The check-list is to avoid checking for anomalies repeatedly in the same
;;; concept.  Recursive-check calls anomaly-check, which calls is-anomalous.
;;; If it comes back here with the same concept, return nil. This avoids
;;; infinite loops.
;;; 
(defun is-anomalous (concept &optional check-list)
  (do-break is-anomalous)
  (if (or (literal-p concept)
	  (attribute-value-p concept)
	  (member concept check-list))
      nil
      (remove-duplicates
	(recursive-check
	  (f.instantiate-frame
	    ;; The concept's frame definition.
	    (frame-def concept))
	  nil
	  concept
	  (cons concept check-list))
	:test #'equal))
  )




;;;
;;; Function complain-missing-xp prints a message to the user that there exists
;;; an error in function index-anomaly because there is no explanation for the
;;; concept. The routine then executes a break. If show-bugs flag is nil then
;;; we do nothing.
;;;
(defun complain-missing-xp (concept show-bugs)
  (when show-bugs
    (format
      *aqua-window*
      (str-concat
	"~%ERROR in index-anomaly:"
	"No xps in concept ~s")
      concept)
    (break))
  )


;;;
;;; Function index-anomaly places in memory any explanation (usually is the
;;; question) associated with the input concept so that it can be retrieved at
;;; a later time. If the concept is a relation, the anomaly is passed directly
;;; to the function do-index. However, if it is not a relation, then the actor
;;; relation of the concept is passed to do-index. Do-index requires a
;;; relation, and at this time, it indexes by actor relations of actions.
;;;
;;; |||||| Is this function redundant with the index-question function?
;;; [28nov93]
;;; 
(defun index-anomaly (concept anomaly-list)
  (let ((xps (f.get concept
		    *explanations-slot*)))
    (do-break index-anomaly)
    (if xps
	(dolist (each-path anomaly-list)
	  (let ((anomaly
		  (apply
		    #'f.chase-path
		    (cons concept
			  each-path))))
	    (if (isa-p 'relation
		       (list anomaly))
		(do-index xps
			  'question-type.0
			  anomaly)
		(if (f.get anomaly
			   *actor-slot*)
		    (do-index xps
			      'question-type.0
			      (f.get-relation
				anomaly
				*actor-slot*))
		    ))))
	(complain-missing-xp concept
			     *Show-Bugs*)))
  )

(defun index-anomaly2 (concept anomaly-list)
  (let ((anomalous-frame (f.get concept *domain-slot*)))
    (do-break index-anomaly2)
    (if anomalous-frame
	(dolist (each-path anomaly-list)
	  (let ((anomalous-node
		  (apply
		    #'f.chase-path
		    (cons anomalous-frame
			  (butlast each-path)))))
	    (if (isa-p 'relation
		       (list anomalous-node))
		(do-index concept
			  'question-type.0
			  anomalous-node)
		(if (f.get anomalous-node
			   *actor-slot*)
		    (do-index concept
			      'question-type.0
			      (f.get-relation
				anomalous-node
				*actor-slot*))
		    ))))
	(format *aqua-window*
		"~%ERROR in index-anomaly2~%")))
  )


;;;
;;; STORY COHERENCE
;;; 

;;;
;;; The following code was an initial beginning to compute the coherence of the
;;; story. See comment on *Story-Tokens*.
;;; 

(defvar *reference-counter* 0)

(defun story-references-in (frame)
  "Count the number of input story concepts references within the frame."
  (terpri *aqua-window*)
  (setf *reference-counter* 0)
  (f.traverse-frame
    frame
    #'sentences-touched)
  (format
    *aqua-window*
    (str-concat
      "~%~%~s story concept(s) "
      "referenced within frame ~s.~%~%"  )
    *reference-counter*
    frame)
  )


(defun sentences-touched (current-frame parent role facet-name level)
  (cond ((and
	   (not (visited-p current-frame *traverse-marker*))
	   (member current-frame
		   (get-model *Story-Tokens*)))
	 (setf *reference-counter*
	       (+ 1 *reference-counter*))
	 (format
	   *aqua-window*
	   "~%Sentence token: ~s"
	   current-frame))))
 



;;;
;;; Function return-path-to returns a path from the root frame to the target
;;; frame if it exists. This is really inefficient.
;;; 
(defun return-path-to (target-frame root-frame)
  (if (equal root-frame target-frame)
      nil
      (f.traverse-frame
	root-frame
	#'(lambda (current-frame
		   parent
		   role
		   facet-name
		   level)
	    (if (equal current-frame target-frame)
		(print
		  (append
		    (return-path-to
		      parent root-frame)
		    (list (if (equal facet-name *value-facet*)
			      role
			      (list role facet-name)))))))))
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;           HIGH LEVEL PREDICATES
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;
;;; Predicate action-mode-p returns true if Meta-AQUA is being run in
;;; problem-solving mode, false if story understanding mode.
;;; 
(defun action-mode-p (mode)
  (equal mode 'act-out-story)
  )


;;; 
;;; Predicate same-results-p returns true if the main-result of both nodes
;;; passed to it are of the same type, nil otherwise. The function returns as a
;;; second value:  the common type if the types are the same, otherwise the
;;; second value returned is a list of the main-result type of the first node
;;; followed by the second.
;;; 
(defun same-results-p (node1 node2)
  (let ((first-type (get-abstraction
		      (f.get node1
			     'main-result
			     *value-facet*)))
	(second-type (get-abstraction
		       (f.get node2
			      'main-result
			      *value-facet*)))) 
    (if (and (not (or (null first-type)
		      (null second-type)))
	     (equal first-type second-type))
	(values t first-type)
	(values nil (list first-type second-type)))))


;;; 
;;; The predicate siblings-p is a slight mis-nomer.  Sibling are defined in
;;; this sense as nodes in which one is not the direct ancestor of the other
;;; not including entity, the root of the isa hierarchy.
;;;
;;; ||||| Look more closely at common-ancestor-p and lowest-common-ancestor-p
;;; for details since this explanation is off somewhat. they are in frame.lisp.
;;; 
(defun siblings-p (node1 node2)
  (let ((lca-type nil))
    (if (and (not (f.common-ancestor-p node1 node2))
	     (not (equal '(entity)
			 (setf lca-type (f.lowest-common-ancestor node1 node2)))))
	(values t lca-type)
	(values nil lca-type))))



;;; 
;;; Currently the function checks all slot's relation facets of the concept for
;;; an indexed question. If any of the co-domain slots of the relations has an
;;; actor slot, then it also checks the relation facet of these actor
;;; relations.
;;; 
(Defun potential-answer-p (concept)
  (do-break potential-answer-p)
  (let ((question
	  (retrieve-memory
	    'question-type.0
	    concept))
	(sub-questions
	  (mapcan
	    #'(lambda (each-slot)
		(let*
		  ((relation-filler
		     (slot->filler		; ||||||Turn this into a call of f.get-relation?
		       each-slot
		       *relation-facet*))
		   (return-val
		     (or
		       (and (not
			      (listp relation-filler))
			    (retrieve-memory
			      'question-type.0
			      relation-filler))
		       (let ((co-domain-filler	; This is really the value filler of the slot (each-slot)
			       (f.get
				 relation-filler
				 *co-domain-slot*)))
			 (if (and
			       co-domain-filler
			       (f.get
				 co-domain-filler
				 *actor-slot*))
			     (retrieve-memory
			       'question-type.0
			       (f.get-relation
				 co-domain-filler
				 *actor-slot*)))))))
		  (if return-val
		      (list return-val))))
	    (f.slot-list concept))))
    (if (or (null question)
	    (listp question))
	(append question sub-questions)
	(cons question sub-questions))
    )
  )






;;; 
;;; A concept is determined to be interesting if it is anomalous or if it is an
;;; explanation. As Schank has noted, it is also inherently interesting if it
;;; involves violence, sex, or loud noises. Finally, it is interesting if it is
;;; a concept about which the reasoner has learned something lately (in this
;;; case the concept will be marked as personally interesting).
;;;
;;; For example, if input concept is DOG-BARKS.25745 then anomaly-list would be
;;; '((OBJECT) (TO DOMAIN))
;;; 
(defun interesting-p (concept)
  (let ((anomaly-list (is-anomalous concept)))
    (do-break interesting-p)
    (cond (anomaly-list
	   (index-anomaly concept anomaly-list)
	   (let ((anomaly-frame
		   (f.instantiate-frame
		     `(anomaly
			(expected-outcome
			  (,*value-facet*
;			   ,(f.instantiate-frame
;			      (apply
;				#'f.chase-path
;				`(,(frame-type concept)
;				  ,@(first anomaly-list))))
			   ,(apply
			      #'f.chase-path
			      `(,(f.instantiate-frame
				   (frame-def concept))
				,@(first anomaly-list)))))
			(actual-outcome
			  (,*value-facet*
			   ,(apply
			      #'f.chase-path
			      `(,concept
				,@(first anomaly-list)))))
			(action
			  (,*value-facet* ,concept))
			(paths
			  (,*value-facet*
			   (literal ,anomaly-list)))))))
; 	     (f.set-literal
;	       (f.get anomaly-frame
;		      'paths)
;	       anomaly-list)
	     (set-anomaly-list
	       *Current-Result-Record*
	       anomaly-frame)
	     anomaly-frame))
	  (t
	   (or (and (isa-p 'violent-mop (list concept))
		    (null
		      (format
			*aqua-window*
			"~%~s is a violent action ... interesting.~%" concept))
		    (f.instantiate-frame
		      `(characterization
			 (,*domain-slot* (,*value-facet* ,concept))
			 (,*co-domain-slot* (,*value-facet* violent-mop.0)))))
	       (and (isa-p 'sexual-mop (list concept))
		    (null
		      (format
			*aqua-window*
			"~%~s is a sexual action ... interesting.~%" concept))
		    (f.instantiate-frame
		      `(characterization
			 (,*domain-slot* (,*value-facet* ,concept))
			 (,*co-domain-slot* (,*value-facet* sexual-mop.0)))))
	       (and (isa-p 'noisy-mop (list concept))
		    (null
		      (format
			*aqua-window*
			"~%~s is a noisy action ... interesting.~%" concept))
		    (f.instantiate-frame
		      `(characterization
			 (,*domain-slot* (,*value-facet* ,concept))
			 (,*co-domain-slot* (,*value-facet* noisy-mop.0)))))
	       (and (get (frame-type concept) 'personally-interesting)
		    (null
		      (format
			*aqua-window*
			"~%~s is personally interesting.~%" concept))
		    (f.instantiate-frame
		      `(characterization
			 (,*domain-slot* (,*value-facet* ,concept))
			 (,*co-domain-slot* (,*value-facet* personally-interesting.0)))))
	       ;; Commented out so that explanatyions themselves are not explained. [cox 19mar95]
	       ;; |||||| May want to set back later. 
;	       (and (isa-p 'xp (list concept))
;		    (null
;		      (format
;			*aqua-window*
;			"~%~s is an explanation ... interesting.~%" concept))
;		    (f.instantiate-frame
;		      `(characterization
;			 (,*domain-slot* (,*value-facet* ,concept))
;			 (,*co-domain-slot* (,*value-facet* explanatory-struct.0))))
;;;; 		    (list concept (list 'xp))
;		    )
	       ))))
  )



;;;
;;; Function show-indexes is a utility to print the indexes for explanations
;;; about dog barking. Used by the programmer, not the program.
;;; 
(defun show-indexes (&optional (deep t))
  (if deep
      (describe (get (get (get 'actor 'dog-barks) 'dog) 'to))
      (describe (get (get 'actor 'dog-barks) 'dog)))
  )


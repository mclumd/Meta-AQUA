;;; -*- Mode: LISP; Syntax: Common-lisp; Package: Framesystem; Base: 10 -*-

(in-package :frames)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	 A Frame System for conceptual construction
;;;;
;;;;	   Copyright (C) 1994   Michael T. Cox   (cox@cc.gatech.edu)
;;;;
;;;;				   20 May 1994
;;;;
;;;;				File: internals.lisp
;;;;
;;;;
;;;;   This file contains code that is local to the FrameSystem. All functions
;;;;   that are in the external interface are contained in the files frame.lisp
;;;;   and isa.lisp. Externally exported identifiers are in
;;;;   exported-symbols.lisp. Local variables and parameters are contained
;;;;   in file local-constants.lisp.
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
;;           INTERNALLY USED FRAME FUNCTIONS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; 
;;; The dummy frame is used to hold places in the global binding
;;; list during frame instantiation. PLace holders are necessary since
;;; some frame variables will refer to slots occuring later in the
;;; frame definition. This cannot always be avoided since some slots
;;; will refer to other slots that refer back to the first. Thus some
;;; forward reference will occur.
;;;
;;; Dummy frames are also used in indexing. See function do-index.
;;;
;;; NOTE that the function init-dummy is called automatically at the
;;; end of this file (frame.lisp). If the dummy is already defined,
;;; then no action will occur.
;;; 
(defun init-dummy ()
  (cond ((or (not
	       (boundp *dummy*))
	     (not
	       (equal (list *dummy*)
		      (symbol-value *dummy*))))
	 (f.define-frame (list *dummy*))))
  )


;;; NOTE that the call to instantiate-frame must not be changed to
;;; a call of f.instantiate-frame. This function is called internally by
;;; instantiate-filler only. The *GLOBAL-Bindings* must not be affected.
;;; 
(defun create-dummy-frame (status-filler)
  (let ((dummy-var
	  (instantiate-frame dummy status-filler t)))
    (f.put *var-type*
	   dummy-var
	   *status-slot*)
    dummy-var)
  )



;;;
;;; Function instantiate-frame is the actual work horse of function
;;; f.instantiate-frame. It is also called by functions f.instantiate-literal,
;;; create-dummy-frame and instantiate-filler. Note that the pattern property
;;; is actually used by function f.put-all! to determine the scope of the
;;; changes made. [cox 27jan95]
;;; 
;;; note: an application should never call instantiate-frame.  use
;;; f.instantiate-frame or define-frame instead.
;;;
;;; status-filler is the designated status type for the status slot.
;;; 
; is a difference between the pattern and the structure being built.
; put fillers of all slots on the binding-list?
;;; 
(defun instantiate-frame (pattern &optional status-filler subframe-p)
;;;   (declare (special *existing-frames* ))	; Removed for efficiency [cox 25feb95]
  (cond ((frame-var-p pattern)
	 pattern)
	((and (listp pattern)
	      (not (eq
		     (length pattern)
		     1))
	      (equal (first pattern)
		     *literal*))
	 (when (not (eq
		      (length pattern)
		      2))
	   (format *frame-stream*
		   "error in instantiate-frame: bad literal.")
;;; 	   (break)				; Removed for efficiency [cox 25feb95]
	   )
	 (f.instantiate-literal (first (rest pattern))))
	(t
	 (let ((frame-name (gen (car pattern))))
	   (put-isas frame-name (car pattern))
	   (if (not subframe-p)
	       (setf  *global-bindings*
		      (acons *self* frame-name *global-bindings*))
	       (cond ((var-binding-p (second pattern))
		      (setf  *global-bindings* 
			     (acons (second pattern) frame-name *global-bindings*))
		      (setf pattern
			    (cons (car pattern) (cddr pattern))))))	; remove variable marker.
	   (putprop frame-name 'pattern pattern)
	   (mapcar
	     #'(lambda (each-slot)
		 (let ((current-role (slot->role each-slot)))
		   (mapcar
		     #'(lambda (each-facet)
			 (let ((facet-name (facet->facet-name each-facet)))
			   (format-if 
			     *debug-on*
			     t 
			     "~%facet = ~s;slot = ~s" 
			     each-facet each-slot)
			   ;; test to make sure that the facet has not already been
			   ;; established as when variables pointing to it instantiate the slot.
			   (cond ((not (f.get frame-name current-role facet-name))  
				  ;; |||||| should (*frame* frame-name) be used below?
				  ;; note that the facet is filled at the same time
				  ;; that current-filler assignment happens below.
				  (let ((dummy-var (cdr (assoc 
							  (create-self-ref-var 
							   current-role 
							   'reps
							   )
							  *global-bindings*)))
					(current-filler (f.put (instantiate-filler 
								 (facet->filler
								   each-facet) 
								 current-role
								 facet-name
								 status-filler)
							       frame-name 
							       current-role
							       facet-name)))
				    (if (and dummy-var 
					     (not (equal dummy-var current-filler)))
					(switch-back-ptrs-of 
					  dummy-var current-filler)))))))
		     (slot->facets each-slot))))
	     (frame->slots pattern))
	   (if status-filler
	       (f.put status-filler frame-name *status-slot*))
	   frame-name))))



;;; 
;;; |||||| Need to remove all list., [, & ]s from rep_* files.  Or could just
;;; work down the cdrs of lists.  By doing appends to results in lists we avoid
;;; problems with nils being returned.
;;; 
;;; Parameters role and facet-name are the names of the slot and facet which
;;; instantiate-frame is trying to instantiate.  Status-filler is the
;;; designated status type for the status slot.
;;; 
(defun instantiate-filler (filler-pattern role facet-name status-filler)
  (cond ((null filler-pattern)
	 nil)
	;; Diff between list of bindings and another mop.
	((listp filler-pattern)
	 (cond
	   ;; If the filler is a literal specification ...
	   ((and (listp filler-pattern)
		 (eq 2 (length filler-pattern))
		 (eq (first filler-pattern) *literal*)
		 (not (var-binding-p (second filler-pattern))))
	    (f.instantiate-literal (first (rest filler-pattern)) status-filler))
	   ;; If the filler is a list of frames or frame specs ...
	   ((frame-list-p filler-pattern)
	    ;; |||||| NOTE that .list will bomb!
	    ;; It bombs probably because it tries to make call of
	    ;; (instantiate-filler '.list) in the following statement.
;	    (cons (if (frame-var-p (first filler-pattern))
;		      (first filler-pattern)
;		      (instantiate-filler (car filler-pattern) role facet-name status-filler))
;		  (instantiate-filler (cdr filler-pattern) role facet-name status-filler)))
	    ;; NOTE that I commented out the above and un-commented the mapcar. Will there be change? 19Dec91
	    (mapcar #'(lambda (variable)
		       (instantiate-filler variable role facet-name status-filler))
		    filler-pattern))
	   ;; Otherwise it must be a single subframe
	   ((var-binding-p (second filler-pattern))	; Is it of the form (X =Y (..)(..) ..) ?
	    ; Add the facet to *GLOBAL-Bindings*.
	    ; and remove the variable identifier from the filler-pattern.
	    (let* ((var-name (second filler-pattern))
		   (dummy-var (cdr (assoc 
				     var-name
				     *GLOBAL-Bindings*)))
		   (new-pattern (cons (car filler-pattern)
				      (cddr filler-pattern)))
		   (new-filler (instantiate-frame new-pattern status-filler t)))
	      (setf *GLOBAL-Bindings* 
		    (acons var-name 
			   new-filler 
			   *GLOBAL-Bindings*))
	      (if dummy-var
		  (switch-back-ptrs-of 
		    dummy-var 
		    new-filler))
	      new-filler
		    ))
	   (t
	    (instantiate-frame filler-pattern status-filler t)))	; t -> sub-frame
	 )
	((frame-var-p filler-pattern)
	 filler-pattern
;	 (if (instance-p filler-pattern)
;	     ()
;	     ())
	 )
	((var-binding-p filler-pattern)
	 ;; Return the binding if it exists or the slot's actual value if it exists or
	 ;; instantiate the slot and return that to be placed in the variable position by instantiate-frame.
	 (or (f.get
	       (cdr (assoc *self* *GLOBAL-Bindings*))
	       (var->role filler-pattern)
	       *value-facet*)
	     (cdr (assoc filler-pattern *GLOBAL-Bindings*))
	     ;; This next step depends on f.put returning the filler generated by 
	     ;; the instantiate-filler call. Thus the structure is put into both locations.
	     ;; Looks like the above comment refers to the commented-out code below. [cox 29apr94]
	     (let ((dummy-frame (create-dummy-frame status-filler)))
	       (setf  *GLOBAL-Bindings* 
		      (acons filler-pattern dummy-frame *GLOBAL-Bindings*))
	       dummy-frame
;	     (f.put (cdr (assoc *self* *GLOBAL-Bindings*))
;		    (var->role filler-pattern) 
;		    *value-facet*
;		    dummy-frame 
;;		    (instantiate-filler 
;;		      (frame->filler
;;			(get (cdr (assoc *self* *GLOBAL-Bindings*))
;;			     'pattern)
;;			(var->role filler-pattern) 
;;			*value-facet*
;;	                status-filler)
;;		      (var->role filler-pattern)
;;		      *value-facet*)
;		    )
	     )))
	((atom filler-pattern)
	 ;; |||||| Eventually want to put back in requirement for being an entity.
;;; 	 (if (isa-p *entity* filler-pattern)
;	       (instantiate-frame (list filler-pattern) status-filler t)
;	       (format *frame-stream* "~%Not an entity."))
	 (instantiate-frame (list filler-pattern) status-filler t)
	 )
	(t
	 (format *frame-stream*
		 "~%Error in instantiate-filler.")
	 (break))))




;;; 
;;; Function merge-nodes takes all common slot-facets between 
;;; node1 and node2 and assigns the unification of the 2 onto
;;; node1. Then all remaining slot-facets in node2 not in node1
;;; are put in node1. Of course any filler in node1 not in node2
;;; will remain. The unification of the two is then node1. It is
;;; returned as the value of the function.
;;; |||||| Are we sure that node1 is returned as the value of the function?
;;; 
;;; Node2 is removed from the *existing-frames* list.
;;; 
(defun merge-nodes (node1 node2 &optional lazy?)
;;;   (declare (special *existing-frames* ))	; Commented out [cox 25feb95]
;  (if (or (equal (frame-type node2) 'dog)
;	  (equal (frame-type node2) '(dog)))
;      (format *frame-stream*
;      "~%HELLO Merging: ~s ~s~%List: ~s" node1 node2 *existing-frames*))
;  (setf *existing-frames*			; Commented out [cox 25feb95]
;	(delete node2 *existing-frames* :test #'equal))
;  (if (or (equal (frame-type node2) 'dog)
;	  (equal (frame-type node2) '(dog)))
;      (format *frame-stream* "~%List: ~s" *existing-frames*))
  (every #'(lambda (slot)
	     (let ((slot-name (slot->role slot)))
	       (every
		 #'(lambda (facet)
		     (let* ((facet-name (facet->facet-name facet))
			    (filler (do-unify (facet->filler facet)
					      (f.get node2 slot-name facet-name)
					      lazy?)))
		       (if filler 
			   (f.put! filler
				   node1
				   slot-name
				   facet-name)
			   t)))
		 (slot->facets slot))))
	 (f.slot-list node1))
  ; Now attempt to put in node1 all fillers of node2.
  ; Because f.put is not destructive only the facets not
  ; in node1 will be assigned.
  (dolist (each-slot (f.slot-list node2))
    (dolist (each-facet (slot->facets each-slot))
      (f.put (facet->filler each-facet)
	     node1 
	     (slot->role each-slot) 
	     (facet->facet-name each-facet))))
  )



;;; 
;;; Can there ever be a case where both are nil?
;;; Yes when we are unifying two frames, one of which has a slot/facet
;;; with no filler and the other does not have the coresponding slot/facet.
;;; 
;;; If 2 fillers are to be unified and one is an abstraction of the
;;; other, then take the more specialized one.
;;; 
;;; |||||| Really if node1 is a more specialized version of node2
;;; and they share some slot X, if the X slot on node2 is more 
;;; refined than the X slot on node1, then node1's X slot should
;;; be overwritten by node2. Currently not done. Or is it? Look later.
;;; 
;;; ||||||This will bomb if it ever has to try to f.unify a list of nodes with
;;; a single node. I think that can-unify-p will avoid this situation.
;;; Better check some day to make sure.
;;; 
(defun do-unify (node1 node2 &optional lazy?)
  (cond ((and (null node1) (null node2))
	 nil)
	((and (eq node1 *nil*)
	      (eq node2 *nil*))
	 *nil*)
	;; Since literals are never traversed internally, it is not necessary to mark the node.
	;; Note that can-unify-p and do-unify both consider that both nodes may be literals, but
	;; one may be the unassigned literal as in the conceptual definition of literal.
	((literal-p node1)
	 (if (literal-p node2)			;Both nodes are literals.
	     (cond ((equal
		      (symbol-value
			node1)			;Node1 is an unnassigned literal.
		      literal)
		    (switch-back-ptrs-of	;So make all node1 backpointers 
		      node1 node2)		;point to node2
		    node2)			;and return node2
		   (t
		    (switch-back-ptrs-of	;otherwise make all node2 
		      node2 node1)		;backpointers point to node1
		    node1))			;and return node1
	     node1))
	((literal-p node2)
	 (break)
	 node2)
	((or (null node1) (equal node1 *nil*))
	 (mark-as-visited node2)
	 node2)
	((or (null node2) (equal node2 *nil*))
	 (mark-as-visited node1)
	 node1)
	;; |||||| Why is neither node marked below?
	((equal node1 node2)
	 node1)
	;; Handle list fillers.
	((and (frame-list-p node1) (frame-list-p node2))
	 (do-unify (car node1) (car node2) lazy?)
	 (do-unify (cdr node1) (cdr node2) lazy?))
	;; Allow list to be unified with entity.
	((and (equal (frame-type node1)
		*entity*)
	      (frame-list-p node2))
	 (switch-back-ptrs-of node1 node2)
	 node2)
	((and (equal (frame-type node2)
		*entity*)
	      (frame-list-p node1))
	 (switch-back-ptrs-of node2 node1)
	 node1)
	;; The next clause used to be the second clause.
	((and (visited-p node1)(visited-p node2))
	 nil)
	;; NOTE that the following has been moved to above location.
;;; 	((equal node1 node2)
;;; 	 node1)
	;; |||||| The handling of truth value unification should involve inference and be
	;; recorded in trace, not simply unified. [cox 6may94]
	((and (isa-p 'truth-value
		     (list node1))
	      (isa-p 'truth-value
		     (list node2)))
	 (format *frame-stream*
		 "Unification of truth values ~s and ~s"
		 node1
		 node2)
	 (cond ((eq node1 *hypothesized*)
		node2)
	       ((eq node2 *hypothesized*)
		node1)
	       ((or
		  (and (eq node1 *in*)
		       (eq node2 *hypothesized-in*))
		  (and (eq node2 *in*)
		       (eq node1 *hypothesized-in*)))
		*in*)
	       ((or
		  (and (eq node1 *out*)
		       (eq node2 *hypothesized-out*))
		  (and (eq node2 *out*)
		       (eq node1 *hypothesized-out*)))
		*out*)
	       ;; ||||||I still am not handling the cases such as
	       ;; one being in while the other is out. [cox 6may94]
	       (t
		(break "ERROR: truth value problem in do-unify")
	       node1)				; Something besides nil so program can continue. [25may94]
	       ))
	((or (and
	       (eq node1 *story-instance*)
	       (eq node2 *attribute-value*))
	     (and
	       (eq node2 *story-instance*)
	       (eq node1 *attribute-value*)))
	 *attribute-value*)
	((or (and
	       (eq node1 *story-instance*)
	       (eq node2 *question*))
	     (and
	       (eq node2 *story-instance*)
	       (eq node1 *question*)))
	 *question*)
	((or (and
	       (eq node1 *story-instance*)
	       (eq node2 *learned*))
	     (and
	       (eq node2 *story-instance*)
	       (eq node1 *learned*)))
	 *story-instance*)
	;; The following will work if (isa-p (frame-type node2) (list node1))
	;; or if (eq (frame-type node1) (frame-type node2)); e.g., person.12 and person.21
	((f.common-ancestor-p node1 node2)
	 (mark-as-visited node1)
	 (mark-as-visited node2)
	 ;; This case works also for siblings.
	 (merge-nodes node1 node2 lazy?)
	 ;; Node2 is old-location, node1 is new-location.
	 (switch-back-ptrs-of node2 node1)
	 node1)
	((f.common-ancestor-p node2 node1)
	 (mark-as-visited node1)
	 (mark-as-visited node2)
	 (merge-nodes node2 node1 lazy?)
	 ;; Node1 is old-location, node2 is new-location.
	 (switch-back-ptrs-of node1 node2)
	 node2)
	(t
	 (cond (lazy?
		(format *frame-stream*
			"~%Lazy unification replacing ~s with sibling ~s."
			node2 node1)
		(if (isa-p 'marijuana (list node2))
		    (break)))
	       (t
		(format *frame-stream*
			"~%ERROR in do-unify on nodes ~s and ~s."
			node1 node2)
		(break)))
	 node1					; Something besides nil so program can continue. [25may94]
	 )))



;;; 
;;; Function switch-back-ptrs-of makes any node that refers to old-location
;;; point instead to new-location.  Backpointers are triples: <frame slot
;;; filler> which get bound to local variables b1, b2, & b3.  Some backpointers
;;; are the triple <'index symbol property>.  This is because frame variables
;;; are indexed in memory.  When a unification occurs, we want to replace any
;;; frame indexed variables with the newly merged node.  The pointers are
;;; created when any filler is placed in a slot facet by f.put!.  Since f.put
;;; calls f.put!  all extant fillers have backpointers.
;;;
;;; If the old-location was the nil filler *nil*, then the function is not
;;; appropriate. This could happen when do-unify is unifying a nil filler with
;;; an actual filler.
;;;
;;; |||||| Are there conflicts possible with other attribute values?
;;; 
(defun switch-back-ptrs-of (old-location new-location)
  (if (not (equal old-location *nil*))
      (dolist (each-back-ptr (get old-location *Back-Ptrs*))
	(let ((b1 (first each-back-ptr))
	      (b2 (second  each-back-ptr))
	      (b3 (third  each-back-ptr)))
	  (cond ((equal b1 'index)
		 (let ((current-value (get b2 b3)))
		   (cond ((atom current-value)
			  (putprop b2 b3 new-location))
			 (t
			  (putprop
			    b2 b3
			    (subst new-location
				   old-location
				   current-value))))))
		(t
		 (let ((current-filler 
			 (f.get b1 b2 b3)))
		   (f.put! (if (listp current-filler)
			       (subst new-location
				      old-location
				      current-filler)
			       new-location)
			   b1 b2 b3)))))))
  )



(defun get-lowest (candidate-list)
  (let ((lowest (first candidate-list))
	(not-unique nil))
    (dolist (each-candidate (rest candidate-list))
      (if (< (first each-candidate) (first lowest))
	  (setf lowest each-candidate)
	  (if (eq (first each-candidate) (first lowest))
	      (setf not-unique t))))
    (if not-unique
	(setf lowest (select-most-specialized candidate-list lowest))
	lowest))
  )


;;;
;;; |||||| Return to write this later.
;;; Also need to perform error checking upon the condition that
;;; definitional constraints at the same level do not contradict
;;; each other. Eg., X requires the filler to be male while Y
;;; requires that it be female. What to return when contradictory
;;; defaults are not compatible?
;;; 
(defun select-most-specialized (candidate-list lowest)
  lowest)



(defun lowest-not-isa-remainder-p (lowest remainder-list)
  (notany #'(lambda (list-item)
	      (isa-p (third lowest) (list (third list-item))))
	  remainder-list)
  )


(defun inherit (frame slot facet level)
  (let ((default (f.get frame slot facet)))
    (if default
	(list (list level default frame))
	(let ((abstractions (get-abstraction frame)))
	  (if (null abstractions)
	      nil
	      (let ((return-list nil)
		    (level-num (+ 1 level)))
		(dolist (each-abstraction abstractions)
		  (setf return-list
			(append return-list
				(inherit each-abstraction
					 slot
					 facet
					 level-num))))
		return-list)))))
  )
	       


; Function may be more efficient with delete instead of putprop - remove - get.
;
(defun f.rem-back-ptrs (filler frame slot &optional facet)
  (if (frame-var-p filler)
      (putprop filler 
	       *Back-Ptrs* 
	       (remove (list frame slot (or facet *value-facet*))
		       (get filler *Back-Ptrs*) 
		       :test #'equal)))
  )


(defun f.put-back-ptrs (filler frame slot &optional facet)
  (if (frame-var-p filler)
      (addprop filler
	       *Back-Ptrs*
	       (list frame
		     slot
		     (or facet *value-facet*))))
  )


;;;
;;; Function gen is used by instantiate-frame to create
;;; a unique object identifier (token). It takes as input
;;; a form representing a concept (eg.,
;;; (ptrans (actor (constraint (person))))). It then
;;; creates a unique symbol based on the frame-type of the
;;; input concept (eg., PTRANS -> PTRANS.12) and binds the
;;; token to that concept. Compare to gen0. NOTE that
;;; use of the special handling was never implemented.
;;;
;;; Commented out assignment to *existing-frames* and removed the cond clause.
;;; [cox 25feb95]
;;; 
;(defun gen (concept)
;  (let ((a-concept (if (atom concept)
;		       (list concept)
;		       concept)))
;    (cond ((needs-special-handling-p a-concept)	; Always returns nil. [cox 25feb95]
;   ; ||||||Do special handling. Do not forget to remove nil.
;	   a-concept)
;	  (t
;	   (let ((x (list 
;		      (create-new-var (frame-type a-concept)) 
;		      a-concept)))
;	     (set (car x) (cadr x))
;	     (setf *existing-frames*
;		   (cons (car x) *existing-frames* ))
;;    (if (or (equal (frame-type (car x)) 'dog)
;;	  (equal (frame-type (car x)) '(dog)))
;;      (format *frame-stream*
;;	     "~%HELLO Gensyming: ~s onto ~s." (car x) *existing-frames*))
;	     (car x))))))
(defun gen (concept)
  (let ((a-concept (if (atom concept)
		       (list concept)
		       concept)))
    (let ((x (list 
	       (create-new-var
		 (frame-type a-concept)) 
	       a-concept)))
      (set (car x) (cadr x))
      (car x))))

;;; (proclaim '( special *existing-frames*))	; Commented out assignment [cox 25feb95]


(defparameter *print-switch* nil
  "If non-nil print frame definitions to screen during gen0")

;;; 
;;; Function gen0 is called by f.define-frame.
;;; The purpose of this routine is to bind the value
;;; of the car of the input list concept to the list
;;; itself. Thus the value of the symbol 'PTRANS is
;;; the definition of the primitive PTRANS after
;;; calling (gen0 '(ptrans (actor (constraint (person)))...))
;;; 
(defun gen0 (concept)
  (let* ((a-concept (if (atom concept)
			(list concept)
			concept))
	 (x (list
	      ;; Had to put this condition in because frame-type tests for literals now. 23nov93
	      ;; Therefore we could not properly call define-frame on the definition of literal itself.
	      (print
		(if (literal-p a-concept)
		    (first a-concept)
		    (frame-type a-concept))
;;; 		(frame-type a-concept)
		)
	      a-concept)))
    (format-if
      *print-switch*
      *frame-stream* "~s~%" a-concept)
    (set (car x) (cadr x))
    (car x))
  )



;(value-default-facet (some #'(lambda (slot)
;				 (if (equal (car slot)
;					    *isa-property*)
;				     slot))
;			    (cdr pattern))
;)
;;; |||||| Should we allow (isa (constriant ....)) ?
;;; 
;;; Also handle where there is overlap in isas already defined and specified in
;;; pattern.  For define-frame you need not gen a variable, just put the
;;; pattern on the symbol-value of the car of the parameter.
;;;
;;; NOTE that the isa slot is required for every frame definition. 
;;; 
(defun f.define-frame (pattern &optional status-filler)
  (if (and status-filler
	   (recursive-member *status-slot* pattern))	; Then there already exists a status slot.
      (break "Error (f.define-frame): two status slots defined.")
      (if status-filler
	  (setf pattern
		(append pattern
			`((,*status-slot*
			   (,*value-facet*
			    ,status-filler)))))))
  (let* ((new-frame (gen0 pattern))
	 (isa-slot (frame->slot pattern *isa-property*))
	 (slot-slot (frame->slot pattern *slot-prop*))
	 (abstractions (get-abstraction new-frame)))
    ;; Creates bad cycle if frame isa itself.
    (when (equal (first (slot->filler isa-slot))
		 (frame-type pattern))
      (format *frame-stream*
	      "~%ERROR: in f.define-frame.")
      (break))
    (do-break f.define-frame
	      (str-concat
		"New frame: ~s~%isa-slot:~s"
		"~%slot-slot:~s~%abstractions:~s~%")
	      new-frame
	      isa-slot
	      slot-slot
	      abstractions)
    (and slot-slot
	 (putprop (frame-type new-frame)
		  *slot-prop*
		  (first (slot->filler slot-slot
				       *value-facet*)))
	 (setf
	   (symbol-value new-frame)
	   (delete-if #'(lambda (each-slot)
			  (if (listp each-slot)
			      (equal slot-slot 
				     each-slot)))
		      (symbol-value new-frame))))
    ;; Set instances property for parents.
    (dolist (each-parent
	      (slot->filler isa-slot
			    *value-facet*))
      (addprop each-parent 'instances new-frame t))	; Add properties, avoid duplicates.
    (cond ((and (null isa-slot) (null abstractions))
	   nil)
	  ((null abstractions)
	   (format-if
	     *Debug-On*
	     *frame-stream*
	     "~%(null abstractions)")
	   (if (put-isas
		 (if (literal-p new-frame)
		     new-frame
		     (frame-type new-frame))
;;; 		 (frame-type new-frame)
		 (slot->filler isa-slot *value-facet*))
	       ;; Now remove the isa slot from the newly defined frame.
	       (setf
		 (symbol-value new-frame)
		 (delete-if #'(lambda (each-slot)
				(if (listp each-slot)
				    (equal isa-slot 
					   each-slot)))
			    (symbol-value new-frame)))
						;Case missing value facet in extant slot.
	       (format *frame-stream*
		       "~%ERROR in f.define-frame: ~s has no abstraction."
		       isa-slot)))
	  ;; |||||| The following is redundant?
	  ((null isa-slot)
	   ;; Entity & literal are the only frames allowed not to have isa link.
	   (when (not (or (equal new-frame 'entity)
			  (equal new-frame 'literal)))
	     (format *frame-stream*
		     "~%Error: No isa slot in frame definition for ~s."
		     new-frame)
	     (break))
	   (format-if
	     *Debug-On*
	     *frame-stream*
	     "~%(null isa-slot)")
	   (putprop new-frame *isa-property* abstractions))
	  (t
	   
	   (format-if
	     *Debug-On*
	     *frame-stream*
	     "~%(else t)")
						;Case where there are existing abstractions and possibly new ones.
	   (put-isas (frame-type new-frame)
		     (slot->filler isa-slot *value-facet*))		     
	   (setf
	     (symbol-value new-frame)
	     (delete-if #'(lambda (each-slot)
			    (if (listp each-slot)
				(equal isa-slot
				       each-slot)))
			(symbol-value new-frame)))))
;    (setf (symbol-value new-frame)
;	  (add-constraint-facet-names (*FRAME* new-frame)))
    (undefined-frames new-frame)
    (setf *extant-frames* (cons new-frame *extant-frames*))
    new-frame))




;;;
;;; The function undefined frames is an internal routine used
;;; in function f.define-frame. It is the main function used in
;;; bookkeeping registering those frames made reference to by
;;; some frame definition but never defined themselves. The print
;;; routine above then reports these undefined frames.
;;; 
(defun undefined-frames (new-frame)
  (process-facets (symbol-value new-frame)
		  new-frame)
  (setf *undefined-frames*
	(del-prev-undefined new-frame))
  )



(defun del-prev-undefined (newly-defined-frame)
  (mapcan #'(lambda (each-pair)
	      (and (not (equal (car each-pair)
			       newly-defined-frame))
		   (list each-pair)))
	  *undefined-frames*))

  
;;; 
;;; The frame-var is the original frame variable which node
;;; is defined within.
;;; Eg. If frame-var = PTRANS.12, then
;;; (*FRAME* frame-var) -> (PTRANS (Actor (value node))).
;;; 
(defun categorize (node frame-var)
  (cond ((var-binding-p node)
	 nil)
	((not (boundp node))
	 (setf *undefined-frames*
	       (cons (list node frame-var) *undefined-frames*)))
	((not (listp (symbol-value node)))
	 (setf *num-non-lists* (+ *num-non-lists* 1)))
	))



;;; The frame will be a form such as (PTRANS (Actor (value X))..)
;;; and the frame-var will be the symbol with such binding
;;; eg. PTRANS.101.
;;; 
(defun process-facets (frame frame-var)
  (mapcar #'(lambda (slot)
	      (if (not (var-binding-p slot))
		  (mapcar #'(lambda (facet)
			      (let ((filler (facet->filler facet)))
				(cond ((null filler)
				       (setf *num-nulls* (+ *num-nulls* 1)))
				      ((atom filler)
				       (categorize filler frame-var))
				      ((listp filler)
				       (if (frame-list-p filler)
					   ;; If filler is frame list, assume it has
					   ;; no sublists other than frames.
					   (dolist (each-filler filler)
					     (if (listp each-filler)
						 ;; then it is a frame.
						 (and (categorize
							(first each-filler)
							frame-var)
						      (process-facets each-filler
								      frame-var))
						 (categorize each-filler frame-var)))
					   ;; otherwise it is a frame.
					   (and (categorize (first filler)
							    frame-var)
						(process-facets filler frame-var))
					   ))
				      (t (format *frame-stream*
						 "ERROR in known-frames"))
				      ))
			      )
			  (slot->facets slot))))
	  (frame->slots frame))
  )


(defun f.define-relation (pattern)
  (f.define-frame pattern)
  )
(proclaim '(inline f.define-relation))


(defun value-default-facet (slot)
  (cons
    (car slot)
	 (mapcar 
	   #'(lambda (facet)
		     (if (atom facet)
			 `(value ,facet)
			 (if (not (equal (facet->facet-name facet) 'value))
			     `(value ,@facet))))
	       (slot->facets slot))))




(defun get-from-list (old-item)
  (nth
    (- (length old-list)
       (length (member old-item old-list)))
    new-list)
  )


;;;
;;; Function blend takes as argumenmts two facet fillers
;;; that are lists. Each filler in the lists are then
;;; unified.
;;; 
(defun blend (old-filler-list new-filler-list &optional filter)
  (cond ((null old-filler-list)
	 nil)
	(t
	 (let ((old-head (first old-filler-list))
	       (new-head (first new-filler-list)))
	   ;; The following arguments to the f.unify functions MUST be in
	   ;; the current order. The function is not debugged or
	   ;; verified. Careful with use of this function!!
	   (if (member old-head old-list)
	       (f.unify
		 (get-from-list old-head)
		 new-head)
	       (f.unify
		 (copy-instantiated-frame
		   old-head
		   filter)
		 new-head)))
	 (blend (rest old-filler-list)
		(rest new-filler-list)
		filter))))



;;;
;;; Function modify is called by f.modify. See its definition for
;;; some comments.
;;; 
(defun modify (frame new-filler &rest slot-names)
  (setf frame (*FRAME* frame))
  (cond ((null slot-names) nil)
	(t
	 (let ((slot-name (if (listp (first slot-names))
			      (first (first slot-names))
			      (first slot-names)))
	       (facet-name (if (listp (first slot-names))
			       (second (first slot-names))
			       *value-facet*)))
	   (cons (frame->frame-name frame)
		 (mapcar
		   #'(lambda (each-slot) 
		       (let ((current-slot-name (slot->role each-slot)))
			 (if (equal slot-name current-slot-name)
			     (cons current-slot-name
				   (mapcar
				     #'(lambda (each-facet) 
					 (let ((current-facet-name
						 (facet->facet-name each-facet)))
					   (cond ((equal facet-name current-facet-name)
						  (list
						    current-facet-name
						    (if (eq (length slot-names) 1)
							new-filler
							(apply
							  #'f.modify
							  (cons
							    (facet->filler each-facet)
							    (cons
							      new-filler
							      (rest slot-names)))))))
						 (t
						  each-facet)))
					 )
				     (slot->facets each-slot)))
			     each-slot)))
		   (frame->slots frame))))))
  )



(defun copy-instantiated-frame (frame &optional filter)
  (cond ((null frame)
	 nil)
	((and (functionp filter)
	      (funcall filter frame))
	 *nil*)
	;; If frame is just an attribute-value like in.0, then do not copy, just return.
	((attribute-value-p frame) frame)
	((literal-p frame)
	 (setf old-list (cons frame old-list))
	 (let ((new-literal (f.instantiate-literal
			      (symbol-value frame))))
	   (setf new-list (cons new-literal new-list))
	   new-literal))
	((frame-var-p frame)
	 (setf old-list (cons frame old-list))
	 (let ((old-frame-slots (f.slot-list frame))
	       (new-frame
		 (f.instantiate-frame
		   (list (frame-type frame)))))
	   (setf new-list (cons new-frame new-list))
	   (dolist (each-slot old-frame-slots)
	     (let ((current-role (slot->role each-slot)))
	       (dolist (each-facet (slot->facets each-slot))
		 (let ((current-facet-name
			 (facet->facet-name each-facet))
		       (current-filler
			 (facet->filler each-facet)))
		   (if (frame-list-p current-filler)
		       (let ((new-filler nil))
			 (dolist (each-filler current-filler)
			   (setf new-filler
				 (append
				   new-filler
				   (list
				     (cond ((attribute-value-p each-filler)
					    each-filler)
					   ((literal-p each-filler)
					    (f.instantiate-literal
					      (symbol-value each-filler)))
					   (t
					    (f.instantiate-frame
					      (list
						(frame-type
						  each-filler)))))))))
			 (f.put new-filler
				new-frame
				current-role
				current-facet-name)
			 (blend current-filler
				new-filler
				filter))
		       (let ((new-filler
			       (cond ((attribute-value-p current-filler)
				      current-filler)
				     ((literal-p  current-filler)
				      (f.instantiate-literal
					(symbol-value current-filler)))
				     (t
				      (f.instantiate-frame
					(list
					  (frame-type
					    current-filler)))))))
			 (f.put new-filler
				new-frame
				current-role
				current-facet-name)
			 (blend (list current-filler)
				(list new-filler)
				filter)))))))
	   new-frame))))

	
(defun lca (abstraction/s frame)
  (cond ((null abstraction/s)
	 nil)
	((atom abstraction/s)
	 (if (isa-p abstraction/s frame)
	     abstraction/s
	     (lca (get-abstraction abstraction/s) frame)))
	((listp abstraction/s)
	 ; Need to filter out the nils in the returned list.
	 (mapcar #'(lambda (each-abstraction)
		     (lca each-abstraction frame))
		 abstraction/s))))

; Fix HACK later.
; As presented here we are taking the atoms in the lowest sublists.
; EG. (my-filter '(((a(b))((c))((d)e)))) --> (a e).
;
(defun my-filter (abstr-list)
  (cond ((null abstr-list)
	 nil)
	((atom abstr-list)
	 (list abstr-list))
	((listp abstr-list)
	 (let ((results nil))
	   (dolist (each-list-item abstr-list)
	     (if (atom each-list-item)
		 (setq results (cons each-list-item results))))
	   (cond ((null results)
		  (dolist (each-sublist abstr-list)
		    (setf results (append each-sublist results)))
		  (my-filter results))
		 (t
		  results))))))


(proclaim '(inline un-mark1))

;;;
;;; Function un-mark removes the visited marker of a given frame (node) and all
;;; subframes unless the frame is a literal. Marking is performed in order to
;;; avoid cycles in memory structures during search or memory traversal. Note
;;; that the exact marker is dependent on the caller, but defaults to the
;;; standard marker *visited-prop*. Various markers may be used to avoid
;;; conflicts. For example, traverse-frame uses a non-default marker because
;;; the function passed to it may itself use the standard marker (e.g.,
;;; f.unify).
;;; 
(defun un-mark (node &optional (marker *visited-prop*))
  (cond ((null node)
	 nil)
	((and (atom node) (visited-p node marker))
	 (remove-mark node marker)
	 (if (and (frame-var-p node)
		  (not (literal-p node)))
	     (un-mark1 node marker)))
	((listp node)
	 (un-mark (car node) marker)
	 (un-mark (cdr node) marker))
	))


;;;
;;; Function un-mark1 is used by function un-mark to removed visited markers
;;; for all subnodes of the given frame (node).
;;; 
(defun un-mark1 (node marker)
  (mapcar
    #'(lambda (each-slot)
	(mapcar
	  #'(lambda (each-facet)
	      (un-mark (facet->filler
			 each-facet)
		       marker))
	  (slot->facets each-slot)))
    (f.slot-list node)))



;;;
;;; Function mark-as-visited marks a given frame (node) as visited unless that
;;; frame is a literal. If the marker parameter is ommitted in the function
;;; call, the standard marker is used.
;;; 
(defun mark-as-visited (node &optional (marker *visited-prop*))
  (cond ((or (literal-p node)
	     (null node))
	 nil)
	((atom node)
	 (set-mark node marker))
	((listp node)
	 (mark-as-visited (car node) marker)
	 (mark-as-visited (cdr node) marker))))


;;; 
;;; Function remove-mark unsets a given marker on the frame (node) passed to
;;; it. Markers are implemented as symbol properties that are set to t
;;; (set)/nil (unset).
;;; 
(defun remove-mark (node marker)
  (putprop node marker nil)
  )


;;; 
;;; Function set-mark sets a given marker on the frame (node) passed to it.
;;; Markers are implemented as symbol properties that are set to t (set)/nil
;;; (unset).
;;; 
(defun set-mark (node marker)
  (putprop node marker t)
  )



;;;
;;; Function can-unify-p1 is the predicate that actually does the work for
;;; predicate can-unify-p.
;;; 
(defun can-unify-p1 (node1 node2 lazy? &optional error-path)
  (cond ((or (null node1) (null node2)
	     (eq node1 *nil*) (eq node2 *nil*)
	     (equal node1 node2))
	 (values t nil nil))
	((and (symbolp node1)
	      (symbolp node2)
	      (attribute-value-p node1)
	      (attribute-value-p node2))
	 (if (or (eq node1 node2)
		 ;; |||||| See comments on do-unify for this special case. [cox 6may94]
		 (and (isa-p 'truth-value
			     (list node1))
		      (isa-p 'truth-value
			     (list node2))
		      (not (or (and (equal node1 *in*)(equal node2 *out*))
			       (and (equal node2 *in*)(equal node1 *out*)))))
		 (and
		   (eq node1 *story-instance*)
		   (eq node2 *attribute-value*))
		 (and
		   (eq node2 *story-instance*)
		   (eq node1 *attribute-value*))
		 (and
		   (eq node1 *story-instance*)
		   (eq node2 *question*))
		 (and
		   (eq node2 *story-instance*)
		   (eq node1 *question*))
		 (and
		   (eq node1 *story-instance*)
		   (eq node2 *learned*))
		 (and
		   (eq node2 *story-instance*)
		   (eq node1 *learned*))
		 (and
		   lazy?
		   error-path			; To make sure they are not top level nodes.
		   (intersection (get-abstraction node1)
				 (get-abstraction node2))))
	     (values t nil nil)
	     (values nil (list node1 node2) error-path)))
	;; If either is a literal, then to unify both must be a literal
	;; (since we know that neither is *nil* or null) and the literal values must be equal
	;; or at least one must be unassigned, i.e., have the value (literal) as in the concept
	;; definition of a literal.
	((or (literal-p node1)
	     (literal-p node2))
	 (if (and (literal-p node1)
		  (literal-p node2)
		  (or (equal (symbol-value node1)
			     (symbol-value node2))
		      (equal (symbol-value node1) literal)
		      (equal (symbol-value node2) literal)))
	     (values t nil nil)
	     (values nil (list node1 node2) error-path)))
	;; Allow entities to be unified with lists.
	((or (and (equal (frame-type node1)
		*entity*)
	      (frame-list-p node2))
	     (and (equal (frame-type node2)
		*entity*)
	      (frame-list-p node1)))
	 (values t nil nil))
	((or
	   (and (listp node1) (atom node2))
	   (and (listp node2) (atom node1)))
	 (values nil (list node1 node2) error-path))
	((and (frame-list-p node1) (frame-list-p node2))
	 (multi-val-every #'can-unify-p1
			  node1
			  node2
			  (make-sequence
			    'list
			    (length node1)
			    :initial-element lazy?)
			  (make-sequence
			    'list
			    (length node1)
			    :initial-element error-path)))
	((and (visited-p node1)(visited-p node2))
	 (values t nil nil))
	((not (or (f.common-ancestor-p node1 node2)
		  (f.common-ancestor-p node2 node1)))
	 (if (and
	       lazy?
	       error-path			; To make sure they are not top level nodes.
	       (intersection (get-abstraction (frame-type node1))
			     (get-abstraction (frame-type node2))))
	     (values t nil nil)
	     (values nil (list node1 node2) error-path)))
	(t
	 (let ((success t) (failures nil) (far-error nil))
	   (mark-as-visited node1)
	   (mark-as-visited node2)
	   ;; One only cares about the intersection of the two node's
	   ;; fillers so we need only check one node.
	   (every #'(lambda (slot)
		      (every
			#'(lambda (facet)
			    (multiple-value-setq
			      (success failures far-error)
			      (can-unify-p1 
				(facet->filler facet)
				(frame->filler (*FRAME* node2)
					       (slot->role slot)
					       (facet->facet-name facet))
				lazy?
				(append error-path (list (slot->role slot))))))
			(slot->facets slot)))
		  (f.slot-list node1))
	   (values success failures far-error)))
	))



(defun f.instantiate-attribute (pattern)
  (let ((new-attribute (f.define-frame
			 pattern
			 *attribute-value*)))
    (setf (first (symbol-value
		   new-attribute))
	  (strip-dot-zero
	    (first (symbol-value
		     new-attribute))))
    (putprop new-attribute
	     *attribute-value-prop*
	     t)
    new-attribute))



;;;
;;; The following function were developed to test f.traverse-frame function.
;;; |||||| Remove eventually.
;;;

(defun test-print (filler parent role facet-name level)
  (let ((old-level 0))
    (test-print1 filler
		 parent
		 role
		 facet-name
		 level))
  (format *frame-stream* ")~%")
  t)


(setf old-level 0)
(defun test-print1 (filler parent role facet-name level)
  (cond ((equal 0 level)
	 (format *frame-stream*
		 "~%Frame:~%~s~%"
		 filler ))
	(t
	 (if (> level old-level)
	     (setf old-level level)
	     (cond ((< level old-level)
		    (format *frame-stream*
			    "~%" )
		    (setf old-level level))))
	 (do ((counter (* level level) (- counter 1)))
	     ((equal counter 0) nil)
	   (format *frame-stream*
		   "      "))
	 (format *frame-stream*
		 "~s ~s ~s (~s)~%"
		 role
		 facet-name
		 filler
		 level)))
  (format *frame-stream*
	  "~%")
  t
  )

(defun test2 (filler parent role facet-name level)
  (if (not (visited-p filler))
      (format *frame-stream*
	      "~%Filler: ~s"
	      filler)))


(defun traverse-frame1 (frame funct parent slot facet level &rest args)
  (apply funct `(,frame ,parent ,slot ,facet ,level ,@args))
  (cond ((and (atom frame)
	      (not (literal-p frame))
	      (not (visited-p frame *traverse-marker*)))
	 (mark-as-visited frame *traverse-marker*)
	 (mapcar
	   #'(lambda (each-slot)
	       (mapcar
		 #'(lambda (each-facet)
		     (let ((filler (facet->filler
				     each-facet))
			   (role (slot->role each-slot))
			   (facet-name (facet->facet-name
					 each-facet))
			   (new-level (+ level 1)))
		     (if (frame-list-p filler)
			 (dolist (each-filler filler)
			   (apply
			     #'traverse-frame1
			     `(,each-filler
			       ,funct
			       ,frame
			       ,role
			       ,facet-name
			       ,new-level
			       ,@args)))
			 (apply
			   #'traverse-frame1
			   `(,filler
			     ,funct
			     ,frame
			     ,role
			     ,facet-name
			     ,new-level
			     ,@args)))))
		 (slot->facets each-slot)))
	   (f.slot-list frame))))
  )

	  

;;; ;;; |||||| FINISH.
(defun f.bound-put! (frame slot facet filler)
  )


;
;-> ((OBJECT VALUE))  . . . .  (PHYSICAL-OBJECT)
;==>> PHYSICAL-OBJECT
;
;-> ((FROM VALUE))  . . . .  (AT-LOCATION (DOMAIN (VALUE =OBJECT)))
;-> ((FROM VALUE) (DOMAIN VALUE))  . . . .  =OBJECT
;==>> AT-LOCATION
;
;-> ((TO VALUE))  . . . .  (AT-LOCATION (DOMAIN (VALUE =OBJECT)))
;-> ((TO VALUE) (DOMAIN VALUE))  . . . .  =OBJECT
;==>> AT-LOCATION
;
;==>> PTRANS
;(PTRANS
;  (OBJECT (VALUE (PHYSICAL-OBJECT)))
;  (FROM   (VALUE (AT-LOCATION
;		   (DOMAIN (VALUE =OBJECT)))))
;  (TO     (VALUE (AT-LOCATION
;		   (DOMAIN (VALUE =OBJECT))))))
;;; 
;;; This still does not handle the type of bindings
;;; such as filler like (ptrans =x ...).
;;; 
(defun tag-bindings (frame-form &optional path)
  (mapcar #'(lambda (slot)
	      (mapcar #'(lambda (facet)
			  (format *frame-stream*
				  "~%-> ~s  . . . .  ~s"
				  (append path
					  (list
					    (list
					      (slot->role slot)
					      (facet->facet-name facet))))
				  (facet->filler facet))
			  (if (listp (facet->filler facet))
			      (tag-bindings
				(facet->filler facet)
				(append path
					(list
					  (list
					    (slot->role slot)
					    (facet->facet-name facet))))))
			  )
		      (slot->facets slot)))
	  (frame->slots frame-form))
  (format *frame-stream*
	  "~%==>> ~s~%" (car frame-form))
  )






;;;
;;; Function pprint1 actually pretty prints a single frame. It is called by
;;; function f.pprint. The function prints a header identifying the frame to be
;;; printed and the left paren followed by the frame type, it prints all slots,
;;; and then it closes with a right paren.
;;; 
(defun pprint1 (frame stream)
  (print-header frame stream)
  (let ((frame-slots
	  (f.slot-list frame)))
    (mapcar
      #'print-slot
      frame-slots
      (make-sequence
	'list
	(length frame-slots)
	:initial-element stream)))
  (format
    stream
    ")")
  )


(defun print-header (frame stream)
  (format
    stream
    "~%Frame ~s has the following value:~%~%"
    frame)
  (format
    stream
    "(~s"
    (first
      (get-abstraction
	(first
	  (get-abstraction frame)))))
  )


(defun print-slot (slot stream)
  (format stream
	  "~%   (~s ~s)"
	  (slot->role slot)
	  (slot->filler slot))
  )



(defvar *printed-already-list* nil)

(defun t-print1 (each-node parent role facet-name level target)
  (cond ((not (member 'each-node *printed-already-list*))
	 (format *frame-stream*
		 "~%~%~%For the frame ~s:"
		 each-node)
	 (if (null parent)
	     (format *frame-stream*
		     "~%")
	     (format *frame-stream*
		     (str-concat
		       "~% (Which is in "
		       "the ~s slot of frame ~s)~%")
		     role
		     parent))
	 (if (literal-p each-node)
	     (format *frame-stream*
		     "~%The value of the literal is ~s."
		     (*FRAME* each-node))
	     (dolist (each-role (f.role-list
				  each-node))
	       (dolist (each-facet-name
			 (f.facet-list each-node
				       each-role))
		 (format
		   *frame-stream*
		   "~%The ~s facet of the ~s role is ~s."
		   each-facet-name
		   each-role
		   (f.get each-node
			  each-role
			  each-facet-name)))))
	 (setf *printed-already-list* 
	       (cons each-node
		     *printed-already-list*)))
	(t))
  )



;;;
;;; Test print??
;;; 
(defun t-print (test-token)
  (setf *printed-already-list* nil)
  (f.traverse-frame
   test-token
   #'t-print1
   nil))

;;;
;;; Function return-path-to1 is the main code for function f.return-path-to.
;;; 
(defun return-path-to1 (target-frame current-frame &optional path)
  (cond ((equal target-frame current-frame)
	 path)
	((null current-frame)
	 nil)
	((listp current-frame)
	 (or
	   (return-path-to1
	     target-frame (first current-frame) path)
	   (return-path-to1
	     target-frame (rest current-frame) path)
	     ))
	((or (equal current-frame *nil*)
	     (literal-p current-frame)
	     (visited-p current-frame))
	 nil)
	(t
	 (mark-as-visited current-frame)
	 (some #'(lambda (slot)
		   (let ((role (slot->role slot)))
		     (some
		       #'(lambda (facet)
			   (return-path-to1
			     target-frame
			     (facet->filler facet)
			     (cons
			       (if (equal
				     (facet->facet-name
				       facet)
				     *value-facet*)
				   role
				   (list role
					 (facet->facet-name
					   facet)))
			       path)))
		       (slot->facets slot))))
	       (f.slot-list current-frame))
	 ))
  )









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;           DO NOT REMOVE THE FOLLOWING CALL!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Do NOT remove the following call!
(init-dummy)



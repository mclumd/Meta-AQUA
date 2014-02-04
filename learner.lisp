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
;;;;			      File: learner.lisp
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
;;;;      
;;;;                  LEARN  AND REVIEW PHASE
;;;;                  AND LEARNING STRATEGIES 
;;;;       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;
;;; Support Functions
;;;


;;;
;;; Predicate not-trivial-frame-p returns t if the input frame is a trivial
;;; frame, nil otherwise. A frame is trivial if it has only a status slot and a
;;; truth slot. Many (most?) entity frames suit this characterization. This
;;; predicate is used during EBG.
;;; 
(defun not-trivial-frame-p (frame)
  (if (remove
	*status-slot*
	(remove
	  *truth-slot*
	  (f.role-list frame)))
      t)
  )

(defun test-p (list)
  (if (not (equal 'instance-of (first list)))
      (let ((found nil))
	(dolist (each-item (rest list))
	  (if (and (not (not-trivial-frame-p each-item))
		   (not (attribute-value-p each-item)))
	      (setf found t)))
	found))
  )

;;;
;;; Function remove-truth removes truth-slot value fillers from all decendents
;;; of the given frame. The use of this function above may not be necessary
;;; because of the conditional check of not-trivial-frame-p.
;;; 
(defun remove-truth (frame)
  (f.traverse-frame
    frame
    (lambda (current-frame parent role facet-name level)
      (if (and (not (visited-p current-frame *traverse-marker*))
	       (equal role *truth-slot*)
	       (equal facet-name *value-facet*))
	  (f.remove-filler! parent *truth-slot*))))
  frame
  )


;;;
;;; Function change-status alters the status slot of fillers from all
;;; decendents of the given frame. The argument new-status replaces the old
;;; value if there was one. A frame's status changes when it is acquired as a
;;; case, generalized, or otherwise brought into a program from input. The
;;; changes is usually from story-instance to learned.
;;;
;;; |||||| Should we change the status if a frame is a predefined instance?
;;; Or an attribute value?
;;; 
(defun change-status (frame new-status)
  (f.traverse-frame
    frame
    (lambda (current-frame parent role facet-name level)
      (if (and (non-attribute-val-status-p
		 current-frame
		 role
		 facet-name)
	       (not (equal current-frame *question*)))
	  (f.put! new-status parent *status-slot*))))
  frame
  )


;;;
;;; Predicate non-attribute-status-filler-p returns t when the current-frame is
;;; a filler for a status slot other than the status-slot filler
;;; 'attribute-value.0 (which signifies that the parent is an attribute filler
;;; itself, such as in.0 or learned.0). Such values are not subject to change.
;;; Visited frames have already been changed.
;;; 
(defun non-attribute-val-status-p (current-frame role facet-name)
  (do-break non-attribute-val-status-p)
  (and (not (visited-p current-frame *traverse-marker*))
       (equal role *status-slot*)
       (equal facet-name *value-facet*)
       (not (equal *attribute-value*
		   current-frame)))
  )



(defun remove-status (frame)
  (f.traverse-frame
    frame
    (lambda (current-frame parent role facet-name level)
      (if (and
	    (non-attribute-val-status-p
	      current-frame
	      role
	      facet-name)
	    (not (equal current-frame *question*)))
	  (f.remove-filler! parent *status-slot*))))
  frame
  )


;;;
;;; ||||| Note that funcall may be appropriate to use
;;; because of the problems encountered below.
;(defun test (p1 p2)
;  (format t "~%The number is ~s." (eval p2))
;  )
;
;(dolist (each-num '(1 2 3 4)) (test each-num '(+ 1 p1)))



; |||||
; Function old-review used this. What will be done?
;
(defun retrieve-case-from-META-XP (concept)
  (first (get-model *World-Model*))
)



;;;
;;; Function adjust-path changes the path-list so that as f.traverse-frame
;;; traverses the node network, the path-list contracts to remain pointing
;;; into the net. A call of f.chase-path will then be able to get to the
;;; current node being operated on.
;;; 
(defun adjust-path (path-list level)
  (cond ((equal level *prev-level*)
	 (butlast path-list))
	((> level *prev-level*)
	 path-list)
	((< level *prev-level*)
	 (dotimes (x  (+ 1 (- *prev-level* level)))
	   (setf path-list (butlast path-list)))
	 path-list))
  )




;;;
;;; Explanation-Based Generalization (EBG) Learning Strategy
;;;

;;;
;;; |||||Note that in the current version this will produce extra slots if the
;;; ones in the xp do not have as many slots as the definition of a frame.
;;;
;;; NOTE that this is not the current definition used by the program. Look
;;; below for the right one.
;;; 
;(defun generalize-node (current-frame parent role facet-name level parent-struct)
;  (cond ((not (equal level 0))
;	 (setf *path-list* (append
;			     (adjust-path *path-list* level)
;			     `((,role ,facet-name)) ))
;	 (setf *prev-level* level)
;	 (when (and (not (visited-p current-frame))
;		    ;; The frame-body call makes sure that this is not a terminal.
;		    (frame-body current-frame)
;		    (not-trivial-frame-p current-frame))	
;	   (f.unify current-frame
;		    ;; |||||Literals are not handled properly below.
;		    (let ((new-frame
;			    (frame-def
;			      (apply #'f.chase-path
;				     *new-struct*
;				     *path-list*))))
;		      (if (or (null new-frame)
;			      ;; |||||| Eventually we want to unify all items in a list.
;			      (frame-list-p (f.get parent role facet-name)))
;			  *nil*
;			  (if (listp new-frame)
;			      (if (attribute-value-p (frame-type new-frame))
;				  new-frame
;				  (if (literal-p (frame-type new-frame))
;				      (f.set-literal
;					(f.instantiate-frame (*FRAME* *literal*))
;					(symbol-value new-frame))
;				      (f.instantiate-frame new-frame)))
;			      (f.instantiate-frame (list new-frame))))))
;	   (format-if
;	     (and *Debug-On* (f.where-bound current-frame))
;	     t 
;	     "~%Backpointers of ~s is ~s.~%Location is ~s-~s-~s.~%"
;	     current-frame
;	     (f.where-bound current-frame)
;	     parent role facet-name)
;	   )))
;  (when *Debug-On*
;    (format t "~%*path-list*: ~s." *path-list*)
;    (format t "~%*prev-level*: ~s.~%" *prev-level*))  
;  )





(defun generalize-node (current-frame parent role facet-name level parent-struct)
;;;(if (isa-p 'fridge (list current-frame))
;;;    (do-break generalize-node))
  (cond ((not (equal level 0))
	 (setf *path-list* (append
			     (adjust-path *path-list* level)
			     `((,role ,facet-name)) ))
	 (setf *prev-level* level)
	 (when (and (not (visited-p current-frame *traverse-marker*))
;;; 		    (not (tmxp-filter current-frame))
		    ;; The frame-body call makes sure that this is not a terminal.
		    (frame-body current-frame)
		    (not-trivial-frame-p current-frame))	
	   (f.unify
	     (apply #'f.chase-path *new-struct* *path-list*)
	     ;; ||||||Literals are not handled properly below.
	     ;; ||||||Is it because f.set-literal returns the value rather than the literal? [cox 24apr94]
	     (let ((new-frame (frame-def current-frame)))
	       (if (or (null new-frame)
		       ;; |||||| Eventually we want to unify all items in a list.
		       (frame-list-p (f.get parent role facet-name)))
		   *nil*
		   (if (listp new-frame)
		       (if (attribute-value-p (frame-type new-frame))
			   new-frame
			   (if (literal-p (frame-type new-frame))
			       (f.set-literal
				 (f.instantiate-frame (*FRAME* *literal*))
				 (symbol-value new-frame))
			       (f.instantiate-frame new-frame)))
		       (f.instantiate-frame (list new-frame)))))
	     :notify-but-no-break)
	   (if (and *Debug-On* (f.where-bound current-frame))
	       (format t "~%Backpointers of ~s is ~s.~%Location is ~s-~s-~s.~%"
		       current-frame
		       (f.where-bound current-frame)
		       parent role facet-name))
	   )))
  (when *Debug-On*
    (format t "~%*path-list*: ~s." *path-list*)
    (format t "~%*prev-level*: ~s.~%" *prev-level*))
  )

;;;
;;; Function do-ebg should really be doing this.
;;;
 (defun perform-EBG-on (xp)
  ;; Set-up for function generalize-node.
  ;; |||||Why is this not done with let?
  (setf *path-list* nil)
  (setf *prev-level* 0)
;;;   (setf *new-struct*
;;; 	(f.instantiate-frame  (*FRAME* (frame-type xp))))
  (setf *new-struct* xp)
  (do-break perform-EBG-on)
  (f.traverse-frame
    (f.instantiate-frame  (*FRAME* (frame-type xp)))
    #'generalize-node nil)
  ;; ||||| Really need to just make sure that tokens are unified
  ;; after fixing backptr problem, instead of the following hack.
  (f.unify (f.chase-path *new-struct* 'consequent 'object)
	   (f.chase-path *new-struct*
			 'main-precondition
			 *co-domain-slot*
			 *co-domain-slot*
			 *co-domain-slot*))
  (remove-truth *new-struct*)
  )

;(defun count-nodes (current-frame parent role facet-name level)
;   (if (not (visited-p current-frame *traverse-marker*))
;       (setf *x* (+ 1 *x*)))
;   )
;
;(defun find-it (current-frame parent role facet-name level)
;   (if (equal 'drug (frame-type current-frame))
;       (break))
;   )
;
;
;(setf *x* 0)


(defvar *path-list* nil)
(defvar *prev-level* 0)
(defvar *new-struct* nil)

(defun perform-EBG-on (xp)
;  (with-character-style (*Style*)
    (format
      *aqua-window*
      "~%~%Performing EBG on explanation ~s."
      xp)
    ;; Set-up for function generalize-node.
    ;; |||||Why is this not done with let?
    (setf *path-list* nil)
    (setf *prev-level* 0)
    (setf *new-struct*
	  (f.instantiate-frame (frame-def xp)))
    (do-break perform-EBG-on)
    (f.traverse-frame xp #'generalize-node nil)
    ;; ||||| Really need to just make sure that tokens are unified
    ;; after fixing backptr problem, instead of the following hack.
    (f.unify (f.chase-path *new-struct* 'conseq 'object)
	     (f.chase-path *new-struct*
			   'main-precondition
			   *co-domain-slot*
			   *co-domain-slot*
			   *co-domain-slot*)
	     :notify-but-no-break)
    (let ((generalized-xp 
	    (change-status
	      (remove-truth *new-struct*)
	      *learned*)))
      (format
	*aqua-window*
	"~%  resulting in general explanation ~s.~%"
	generalized-xp)
      generalized-xp)
;    )
  
  )





;;;;
;;;; Review - Learn Phase
;;;;



;;;
;;; ||||| NOTE the perform-EBG-on function that really does the EBG in
;;; function specialize. Change soon. 18 May 93. 
;;; 
(defun do-EBG (concept)
  (format
    *aqua-window*
    "~%Execute EBG on ~s."
    concept)
  )


;;; ||||||Must complete.
(defun new-concept (concept)
  t)


;;; 
;;; |||||| WIll this involve a differentiate plan?
;;;
;;; Function differentiate creates subgoals to achieve a given
;;; knowledge-differentiation-goal. If either of the objects to be
;;; differentiated are new concepts, then we spawn a sub-goal to expand them.
;;; The subgoal will have a higher priority than the following organization
;;; goal so that the concepts will be ready to be indexed.
;;; 
(defun differentiate (differentiate-goal)
  (let* ((g-actor (goal-actor differentiate-goal))
	 (g-object (goal-object differentiate-goal))
	 (obj1 (f.get *domain-slot* g-object))
	 (obj2 (f.get *co-domain-slot* g-object))
	 (goal-priority (f.get differentiate-goal 'priority))
	 (higher-priority
	   (inverse-trans
	     (+ 2 (trans goal-priority))))
	 )
    (do-break differentiate)
    (if (new-concept obj1)
	(spawn-sub-goal
	  g-actor
	  obj1
	  knowledge-expansion-goal.0
	  higher-priority
	  differentiate-goal))
    (if (new-concept obj2)
	(spawn-sub-goal
	  	  g-actor
	  obj2
	  knowledge-expansion-goal.0
	  higher-priority
	  differentiate-goal))
    (spawn-sub-goal
      g-actor
      `(reindex-with-respect-to
	 (,*domain-slot* (,*value-facet* ,obj1))
	 (,*co-domain-slot* (,*value-facet* ,obj2)))
      knowledge-reorganization-goal.0
      (inverse-trans
	(- (trans higher-priority)
	   1))
      differentiate-goal)
    ;; Also need to place these subgoals on the subgoal slot of the differentiation goal.
    )
  )



;(defun generalize (xp-token imxp role-literal)
;  (let ((generalized-xp (perform-ebg-on xp-token)))
;    (break)
;    (f.unify generalized-xp
;	     (f.get imxp
;		    (symbol-value
;		      role-literal))))
;  )


;;; 
;;; Function generalize calls ebg on the xp token. The resultant generalized
;;; explanation is then unified with m-prime. M-prime is the node in the imxp
;;; that registers the new explanation created during learning.
;;;
;;; If various generalization routine were available to choose from, this
;;; function might decide which was most appropriate. At least it would have
;;; the appropriate algorithm passed to it.
;;; 
(defun generalize (xp-token memory-item)
  (let ((generalized-xp				;Generalize the token
	  (perform-ebg-on			;using EBG.
	    xp-token)))
    (do-break generalize)
    ;; Reversed order or arguments below [cox 30jun95]
    (f.unify
      memory-item			;with the imxp node m' or m.
      generalized-xp			;Unify the result
	     ))
  )



;;;
;;; Function abstract currently is used to handle the bad constraints on the
;;; value of at-locations on a bark.  This is done by abstracting to the common
;;; parents of what was expected (the constraint) to happen and what actually
;;; happened.
;;;
;;; ||||||Note that this is passed =A1 and =Anomaly as sibling1 and sibling2
;;; respectively. Which-sibling will then be assigned from sibling2 and A1
;;; (sibling1) will never be used in the function.
;;; 
(defun abstract (sibling1 sibling2)
  (let ((which-sibling				;Find the parameter which 
	  (cond ((isa-p 'anomaly		;represents the original anomaly.
			(list sibling1))
		 sibling1)
		((isa-p 'anomaly
			(list sibling2))
		 sibling2)
		)))
    (do-break abstract)
    (if which-sibling
	(let* ((action (f.get which-sibling 'action))
	       (path (first (symbol-value (f.get which-sibling 'paths))))
	       ;; ||||| Hack: f.chase-path needs to be modified rather than having the
	       ;; following conditional. See comments on f.chase-path.
	       (constraint (let ((temp
				   (if (listp path)
				       (apply #'f.chase-path
					      (frame-def action)
					      path))))
			     (if (var-binding-p temp)
				 (f.get (frame-def action)
					(var->role temp))
				 temp)))
	       (actual
		 (f.get which-sibling 'actual-outcome)
;		 (if (listp path)
;		     (apply #'f.chase-path
;			    (cons
;			      action
;			      path)))
		 )
	       (common-parent (f.lowest-common-ancestor 
				(frame-type constraint)
				(frame-type actual))))
;	  (format
;	    *aqua-window*
;	    "~%~%Concept: ~s~%Slots: ~s~%Constraint: ~s~%"
;	    (first (get-abstraction action))
;	    (symbol-value (f.get which-sibling 'paths))
;	    constraint)
;	  (format
;	    *aqua-window*
;	    (str-concat
;	      "Actual Filler: ~s "
;	      "= ~s~%Common-Parent: ~s~%")
;	    actual
;	    (get-abstraction
;	      (first
;		(get-abstraction
;		  (first (get-abstraction actual)))))
;	    common-parent)
	  (if (listp path)
;	      (with-character-style (*Style*)	    
		(cond (common-parent
		       (format
			 *aqua-window*
			 "~%Perform abstraction to ~s~%  on conceptual definition of ~s.~%"
			 common-parent
			 (first (get-abstraction action))
			 )
		       (set (frame-type action)
			    (apply #'f.modify
				   `(,(frame-type action)
				     ,common-parent
				     ,@path)))
		       ;; The action this learning was about is interesting,
		       ;; because it is newly learned.
		       (setf
			 (get (first (get-abstraction action))
			      'personally-interesting)
			 t)
		       common-parent)		; return value if successful.
		      (t
		       (format
			 *aqua-window*
			 "Abstraction fails: No common parent.")))
;		)
	    
	      (format *aqua-window*
		      "~%Error in function ABSTRACT.~%")))))
  )



;;;
;;; Predicate current-frame-more-general-than returns t if the current frame is
;;; more general than the frame it is compared to, nil otherwise.
;;; 
(defun current-frame-more-general-than (frame-compared-to current-frame)
  (and (not (eq (frame-type
		  frame-compared-to)
		(frame-type
		  current-frame)))
       (isa-p (frame-type
		current-frame)
	      (list
		frame-compared-to)))
  )




;;;
;;; Function get-corresponding-frame returns the frame in the new structure
;;; being created by function replace-with-general-filler corresponding to the
;;; corresponding-filler argument. If the corresponding-filler is nil or a
;;; non-list, then it is returned. The real problem solved by this function is
;;; the case when corresponding-filler is a list. That is, the current-frame
;;; may be a list element in a filler of a role of the parent frame. So the
;;; problem is to find the location in the list where the current frame exists,
;;; and then to return the corresponding frame from the list represented by
;;; corresponding filler.
;;;
;;; parent -> (frame (role (facet-name (q current-frame r s))))
;;; corresponding-filler -> (a b c d)
;;; (get-corresponding-frame
;;;    current-frame parent role facet-name corresponding-filler)
;;;  -> b
;;; 
(defun get-corresponding-frame
       (current-frame parent role facet-name corresponding-filler)
  (cond ((null corresponding-filler)
	 nil)
	((not (listp corresponding-filler))
	 corresponding-filler)
	(t
	 (let ((list-filler (f.get parent role facet-name)))
	   (nth (- (length list-filler)
		   (length (member current-frame
				   list-filler)))
		corresponding-filler))))
  )


;;;
;;; Function replace-with-general-filler is the function passed to
;;; f.traverse-frame by function merge-concepts. As each sub-frame (the
;;; current-frame parameter) is encountered during the traversal, it checks to
;;; see if the sub-frame is more general than the corresponding sub-frame in
;;; the new concept (global *new-struct*) being constructed. If this is true,
;;; it replaces the more specific frame with the more general one. This is
;;; performed, not only on the current frame, but all locations where the
;;; current frame is bound. This information is obtained from the specific
;;; frame's back-pointer list by calling f.where-bound.
;;; 
(defun replace-with-general-filler (current-frame parent role facet-name level parent-struct)
  (cond ((not (equal level 0))
	 (setf *path-list* (append
			     (adjust-path *path-list* level)
			     `((,role ,facet-name)) ))
	 (setf *prev-level* level)
	 (let ((corresponding-frame
		 (get-corresponding-frame
		   current-frame parent role facet-name
		   (apply #'f.chase-path
			*new-struct*
			*path-list*))))
	   (when (and (not (visited-p
			     current-frame
			     *traverse-marker*))
		      (current-frame-more-general-than
			corresponding-frame
			current-frame))
	     (let ((general-frame
		     (f.copy-instantiated-frame
		       current-frame
;;; 		       #'tmxp-filter
		       )))
	       (dolist (each-back-ptr
			 (f.where-bound
			   corresponding-frame))
		 (f.put!
		   general-frame
		   (first each-back-ptr)
		   (second each-back-ptr)
		   (third each-back-ptr))
		 ))))
	 ))
  )


;;; 
;;; Function merge-concepts takes two concepts and returns the most general
;;; unification of the two. It works by unifying copies of the two in order to
;;; produce a copy that is guaranteed to contain all slots and facets of both
;;; concepts. Subsequently, the new concept is traversed twice; once for each
;;; parent concept, and any facet that is more general than a corresponding
;;; filler in the unified concept is put on the new frame in its place.
;;; Finally, all truth slots are removed. The global variables *path-list*,
;;; *prev-level*, and *new-struct* are used by function
;;; replace-with-general-filler to manipulate the new frame as the traversal is
;;; performed.
;;; 
(defun merge-concepts (concept1 concept2)
  (do-break merge-concepts)
  (setf *path-list* nil)
  (setf *prev-level* 0)
  (setf *new-struct*
	(f.unify (f.copy-instantiated-frame
		   (remove-status concept1)
;;; 		   #'tmxp-filter
		   )
		 (f.copy-instantiated-frame
		   (remove-status concept2)
;;; 		   #'tmxp-filter
		   )))
  (f.traverse-frame concept1 #'replace-with-general-filler nil)
  (f.traverse-frame concept2 #'replace-with-general-filler nil)
  (let ((generalized-xp 
	  (change-status
	    (remove-truth *new-struct*)
	    *learned*)))
    (format
      *aqua-window*
      "~%  resulting in general explanation ~s.~%"
      generalized-xp)
    generalized-xp)
  )





;;;
;;; Function index-new-xp indexes a newly generalized explanation pattern in
;;; memory.
;;;
;;; The function do-index will return an index to the memory when it stores the
;;; item, unless a similar item already exists in memory. If this occurs,
;;; do-index will return the similar item. Therefore, the first cond clause
;;; represents a successful store of a new memory; whereas, the second clause
;;; is the case of finding the item already in memory. This case is covered in
;;; Cox (1994), when Meta-AQUA forgets the detection explanation, but is
;;; reminded of it as it tries to store a newly generalized explanation from
;;; the story.
;;; 
(defun index-new-xp (generalized-xp)
  (let ((new-index
	  (do-index generalized-xp		;Index the because-xp off dogs barking at containers.
		    'xp-type.0
		    (f.get generalized-xp
			   *explains-node*)
		    t
		    )))
    (do-break index-new-xp)
    (cond ((isa-p 'index (list new-index))
	   (format
	     *aqua-window*
	     (str-concat
	       "~%~%Indexing new explanation "
	       "with index ~s.~%")
	     new-index)
	   new-index)
	  (t
	   (format
	     *aqua-window*
	     "~%Indexing aborted because of reminding.~%")
	   (format
	     *aqua-window*
	     (str-concat
	       "~%Generalizing the similar memories "
	       "instead of storing separate items.~%"))
;;; 	   (break "index-new-xp")
	   (let ((merged-xp
		   (merge-concepts generalized-xp new-index)))
	     ;; Otherwise new-index is the conflicting memory item from the memory-list.
	     (do-index merged-xp
		       'xp-type.0
		       (f.get merged-xp
			      *explains-node*)
		       t
		       t			; Forced indexing true
		       (list
			 (f.chase-path
			   merged-xp
			   'explains
			   'domain '(to relation))))
	     )
	   )) )
  )



;;;
;;; Function list-relations returns a list of relation frames along a path list
;;; starting at the current frame. The first element in the path list provides
;;; a pointer into the current frame, determining the slot from which to
;;; extract the current relation. The filler of this slot then provides the
;;; next frame from which to recursively extract more relations.
;;; 
(defun list-relations (current-frame path-list)
  "Extract a list of relations along a path list starting with the current frame."
  (cond ((null path-list)
	 nil)
	(t
	 (cons
	   (f.make-relation
	     current-frame
	     (first path-list))
	   (list-relations
	     (f.get current-frame
		    (first path-list))
	     (rest path-list)))))
  )

;;;
;;; Function specialize is currently used to learn from the expectation
;;; failure. It differentiates the indices used to retrieved the incorrectly
;;; expected explanation and the actual explanation for the dog barking events.
;;; The differentiation is accomplished by indexing the generalized-xp using
;;; relations obtained from the anomaly path. Then the function removes the
;;; index for the old-xp. Finally, with the help of a small hack, the old-xp is
;;; reindexed using the anomaly path.
;;; 
;;; The old-index parameter is the overly general index that incorrectly
;;; retrieved the defensive-bark explanation (local variable old-xp). The
;;; generalized version of the proper explanation is passed to the function as
;;; parameter generalized-xp. The last three function parameters are from the
;;; IMXP: (=i =anomaly =m-prime)
;;;
;;; ||||||To perform the index differentiation, an algorithm must be developed
;;; that reindexes the two explanations with respect to each other and the
;;; existing indexes (in the case of a novel situation there is no index
;;; though). The current algorithm depends on the anomaly, rather than the xps
;;; themselves. The small hack mentioned above must go.
;;; 
(defun specialize (learning-node old-index anomaly generalized-xp)
  (let* ((anomalous-concept			; e.g., Copy of dog-barks or hit. 
	   (f.copy-instantiated-frame
	     (f.get anomaly 'action)))
	 (anomaly-path				; e.g., (TO DOMAIN) or
	   (second				; (INSTRUMENTAL-SCENE TO DOMAIN) 
	     (symbol-value
	       (f.get anomaly 'paths))))
	 (index-type (f.get old-index 'type))	; e.g., xp-type.0
	 (index-relation (f.get old-index	; e.g., an actor frame
				'relation)))
    (do-break specialize)
    (setf (get (frame-type anomalous-concept)	; The action this learning was about is
	       'personally-interesting)		; interesting because it is newly learned.
	  t)					; |||||| But need to make this more principled.
    ;; Index the xp off dogs barking at
    ;; containers or people hitting what?
    (print-indexing-event
      (do-index generalized-xp
		index-type
		(f.get generalized-xp
		       *explains-node*)		; Pass some explained relation
		t				; Non-destructive as true
		nil				; Forced-indexing as false
		(list-relations
		  (f.chase-path generalized-xp
				*explains-node*
				*domain-slot*)
		  (butlast anomaly-path))))
    (print-specialization-event old-index)	; Simple program feedback.
    (do-index nil index-type index-relation)	; Removes the current indexing of old-xp.
    ;; ||||| Hack to get old threaten-xp indexed off
    ;; animate objects in the "to" slot.
    (f.put-all!
      (f.instantiate-frame animate-object)	; Filler
      (f.get anomalous-concept 'to)		; Frame
      *domain-slot*)				; Slot
    ;; The old threatening bark explanation is indexed off
    ;; dogs barking at animate objects.
    (do-index (first				; Old-xp, e.g., xp-defensive-bark.X or xp-injury-hit.Y
		(f.get old-index
		       *co-domain-slot*))
	      index-type
	      (f.get anomalous-concept
		     *actor-slot*
		     *relation-facet*)
	      t
	      t					; Forced indexing [cox 27jun95]
	      (list-relations anomalous-concept
			      (butlast anomaly-path)))
    )
  )



(defun print-indexing-event (memory-index
			     &optional
			     (stream *aqua-window*))
  "Program feedback during re-indexing."
;  (with-character-style (*Style*)
    (format
      stream
      "~%~%Indexing new explanation with index ~s.~%"
      memory-index)
;    )
  
  )


(defun print-specialization-event (memory-index
				   &optional
				   (stream *aqua-window*))
  "Program feedback during re-indexing."
;  (with-character-style (*Style*)
    (format
      stream
      "~%~%Execute specialize on ~s.~%"
      memory-index)
;    )
  
  )

;;;
;;; BLAME ASSIGNMENT
;;;


;;;
;;; Function l.strategy-decision uses the outcome of the verify stage (||||||
;;; NOTE that it may also use the outcome of the generation-phase) as an index
;;; to retrieve an introspective explanation of the reasoning trace. If the
;;; memory retrieval is successful, then the index is formally represented as a
;;; frame and then stored in a newly created knowledge state frame. This state
;;; frame is then recorded as the decision basis for the learning strategy
;;; decision. Since many strategies may be chosen as part of an overall
;;; learning plan, the function does not choose a meaningful value to return as
;;; the result of the function. Explanation.0 is returned as a generic value. I
;;; suppose that we are using introspective explanation with the IMXP in
;;; further learning steps, so the value is appropriate, but many algorithms
;;; may be selected at a lower level, so it may be misleading too..
;;; 
(defun l.strategy-decision (decision-basis reasoning-failure learning-node k-goal)
  (let* ((imxp-candidates 
	   (retrieve-xps
	     reasoning-failure))
	 (k-state (f.instantiate-frame
		    knowledge-state)))
    (do-break l.strategy-decision)
    (cond (imxp-candidates
	   (f.unify
	     (make-index reasoning-failure	; Result should be of xp-type.0
			 imxp-candidates)
	     (f.get k-state
		    'believed-item)))
	  (t
	   (format *aqua-window*
		   "~%No introspective XP retrieved.~%")))
    (f.put! (list k-state)
	    (f.get
	      decision-basis
	      'knowledge)
	    'members))
  'explanation.0)				; Return value




;;;
;;; STRATEGY EXECUTION
;;;




;;; 
;;; ||||| Because l.runstrategy is performed multiple times, only the last
;;; f.put! will have an effect. Should add each one to a list instead.
;;; 
(defun l.runstrategy (learning-node strategy-choice parameters &optional add-break?)
  (do-break l.runstrategy)
  (case strategy-choice
    (generalization.0
      (if add-break?
	  (add-break generalize))
      (f.put! (list (apply #'generalize
			   parameters))
	      (f.get learning-node
		     'main-result)
	      'members)
      (if add-break?
	  (un-break generalize))
      'generalize.0)
    (abstraction.0
      (if add-break?
	  (add-break abstract))
      (f.put! (list (apply #'abstract
			   parameters))
	      (f.get learning-node
		     'main-result)
	      'members)
      (if add-break?
	  (un-break abstract))
      'abstract.0)
    (specialization.0
      (if add-break?
	  (add-break specialize))
      (f.put! (list (apply #'specialize
			   (cons learning-node parameters)))
	      (f.get learning-node
		     'main-result)
	      'members)
      (if add-break?
	  (un-break specialize))
      'specialize.0)
    (conditionalization.0
      (if add-break?
	  (add-break index-new-xp))
      (f.put! (list (apply #'index-new-xp
			   parameters))
	      (f.get learning-node
		     'main-result)
	      'members)
      (if add-break?
	  (un-break index-new-xp))
      'index-new-xp.0)
    ;; ||||||The following is redundant. Remove. [5dec93]
    (EBG.0
      (if add-break?
	  (add-break do-EBG))
      (f.put! (list (do-EBG parameters))
	      (f.get learning-node
		     'main-result)
	      'members)
      (format *aqua-window*
	      "Performing do-EBG. Is this correct?")
      (if add-break?
	  (un-break do-EBG))
      (break)
      'Do-EBG.0)
    ( t (format
	  *aqua-window*
	  (str-concat
	    "ERROR: unknown learning strategy - "
	    "~s." )
	  strategy-choice)))
  )



;;;
;;; Function execute-strategies runs the learning algorithms chosen by function
;;; select-learning-strategies. The learning-type is the algorithm selection
;;; whereas the credit-blame slot is the arguments to the strategy.
;;; 
(defun execute-strategies (strategy-list imxp learning-node)
  (do-break execute-strategies)
;  (with-character-style (*Style*)
    (cond (strategy-list
	   (format
	     *aqua-window*
	     "~%~%~%Executing strategies.~%")
	   (dolist (each-learning-strategy
		     strategy-list)
	     ;; ||||| The following code is currently dependent on the order of parameters. ??
	     (l.runstrategy learning-node
			    (f.get each-learning-strategy
				   'learning-type)
			    (f.get each-learning-strategy
				   'credit-blame))))
	  (t
	   (format
	     *aqua-window*
	     "~%~%~%No learning performed.~%")
	   ))
;    )
  
  )




;;;
;;; STRATEGY SELECTION
;;;


;;; 
;;; Strategy selection: Take as input a trace of how and why a failure occurred
;;; and a list of learning goals along with their dependencies; produce as
;;; output an ordered set of learning strategies to apply in order to
;;; accomplish the goals along with updated dependencies on the set of goals.
;;; These learning strategies are organized as plans to accomplish the goals.
;;; The plans are sequences of steps representing calls to specific learning
;;; algorithms.
;;;
;;; The function returns the ordered list of learning strategies. At the
;;; current time [cox 8jun95], this function does not support partial ordering.
;;; The list returned is considered a fully ordered list.
;;;
;;; NOTE that if the optional argument randomize is true, then the order is
;;; random. This parameter is used in order to empirically demonstrate the
;;; advantage of mediation by learning goals.
;;; 
(defun select-learning-strategies  (learning-goal-list
				    imxp
				    &optional randomize	; T -> shuffle strategies local var
				    )
  (do-break select-learning-strategies)
;  (with-character-style (*Style*)
    (cond (learning-goal-list
	   (format
	     *aqua-window*
	     "~%~%~%Selecting learning strategies.~%")
	   (let ((strategies
		   (f.get
		     imxp
		     'learning-algorithm)))
	     (format
	       *aqua-window*
	       (str-concat
		 "~%The following algorithms "
		 "are selected:~%  ~s")
	       (if randomize
		   (setf strategies
			 (shuffle strategies))
		   strategies))
	     strategies))
	  (t
	   (format
	     *aqua-window*
	     "~%~%~%Cannot choose a strategy.~%")))
;    )
  
  )



;;;
;;; Initialized by function init-aqua.
;;; 
(defvar *shuffle-state* nil
  "Random state variable used by the function shuffle.")


;;;
;;; Function shuffle returns a list with the elements in random order. this
;;; task is accomplished by choosing a random element of the list to be the new
;;; first element , and shuffling the remainder. [cox 6jun95]
;;;
;;;
;;; The random function call uses a separate random state variable so that the
;;; system random state variable is not changed. If it was to be changed, then
;;; the behavior of the system under replication conditions (i.e., when
;;; re-running stories generated in previous experiemtns) would not be the same.
;;; 
;;; ||||| Need to save random state so that we can recreate the random bahavior
;;; for replication purposes.
;;; 
(defun shuffle (list)
  (if (null list)
      nil
      (let ((random-position (random (length list)
				     *shuffle-state*)))
	(cons
	  (nth random-position list)
	  (shuffle
	    (append
	      (subseq list
		      0
		      random-position)
	      (subseq list
		      (+ 1 random-position)
		      (length list)))))))
  )


   
;;;
;;; DECIDING WHAT TO LEARN
;;;


;;;
;;; Predicate passes-similarity-criteria-p is used to test for similarity of
;;; objects being reconciled or differentiated during learning.
;;;
(defun passes-similarity-criteria-p (goal-object)
  (do-break passes-similarity-criteria-p)
  (let* ((object1 (f.get goal-object *domain-slot*))
	 (object1 (f.get goal-object *co-domain-slot*)))
    t)
  )



;;;
;;; Predicate reasonable-goals-p determines whether or not the learning goals
;;; suggested by the IMXP created in blame assignment are indeed the proper
;;; goals to pursue. The currents tests are on binary goals. Reconciliation
;;; goals demand that the objects to be reconcilable are fairly similar,
;;; whereas a differentiation goal requires that they be somewhat different.
;;; The similarity criteria is in predicate passes-similarity-criteria-p.
;;; 
(defun reasonable-goals-p (learning-goals)
  (do-break reasonable-goals-p)
  (every
    #'(lambda (each-goal)
	(case (f.get each-goal 'goal-type)
	  (knowledge-differentiation-goal-0
	    (not
	      (passes-similarity-criteria-p
		(f.get each-goal
		       'goal-object)))
	    ;;||||| For now must make certain this goes thru. Change after completing criteria
	    ;;predicate.
	    t
	    )
	  (knowledge-reconciliation-goal-0
	    (passes-similarity-criteria-p
	      (f.get each-goal
		     'goal-object)))
	  (t
	    t)))
    learning-goals)
  )

(defun announce-new-goals (new-goals)
  (format
    *aqua-window*
    (str-concat
      "~%Goals are adjusted to better "
      "reflect what needs to be learned."
      "~%New Goals: ~s~%")
    new-goals)
  )


;;;
;;; Function change-goals is invoked if the goals contained in the given imxp
;;; are not reasonable. The goals are adjusted and the changes are announced.
;;; The new goals are returned.
;;;
;;; |||||| Still needs to be finished.
;;; 
(defun change-goals (learning-goals imxp)
  (announce-new-goals learning-goals)
  learning-goals
  )


;;;
;;; Function announce-preliminary-goal prints a message listing the goals input
;;; into the decide what to learn stage. It also lists the corresponding
;;; priorities associated with each.
;;; 
(defun announce-preliminary-goals (learning-goals)
  (format
    *aqua-window*
    (str-concat
      "~%Posting the following"
      " learning goals:~%  ~s~%"
      "  with priorities ~s")
    learning-goals
    (mapcar
      #'(lambda (each-goal)			;Run down list of learning goals
	  (f.get each-goal			;and collect a list of priorities.
		 'priority))
      learning-goals))
  )


;;; 
;;; Deciding what to learn: Take as input a causal explanation of how and why a
;;; failure occurred; generate as output a list of learning goals which, if
;;; achieved, can reduce the likelihood of the failure repeating. The
;;; previously instantiated explanation-pattern assists in this process by
;;; specifying points in the reasoning trace most likely to be responsible for
;;; the failure. Include with the output both tentative goal-dependencies and
;;; priority orderings on the goals.
;;; 
(defun decide-what-2-learn (imxp)
  (do-break decide-what-2-learn)
;  (with-character-style (*Style*)
    (cond
      (imxp
       (format
	 *aqua-window*
	 "~%~%Deciding what to learn.~%")
       (let ((l-goals (f.get imxp 'potential-learning-goals)))
	 (announce-preliminary-goals l-goals)
	 (if (not (reasonable-goals-p l-goals))
	     (change-goals l-goals imxp)
	     l-goals)))
      (t
       (format
	 *aqua-window*
	 "~%~%Cannot decide what to learn.~%")
       nil))
;    )
  
  )

;;; 
;;; |||||| Should rewrite this so that it traverses the phases of the trace
;;; until it finds an failure-type (or non sucessful-prediction) or hits the
;;; phase equal to current-phase.
;;; 
(defun return-failure (trace)
  (do-break return-failure)
  (let ((verify-outcome				; verification phase outcome
	  (first (f.chase-path
		   trace
		   'examination
		   'main-result
		   'members))))
    (if (equal 'successful-prediction
	       (frame-type
		 (f.get verify-outcome
			*co-domain-slot*)))
	(let ((generate-outcome			; generation phase outcome
		(first (f.chase-path
			 trace
			 'generation
			 'main-result
			 'members))))
	  (if (equal 'retrieval-failure
		     (frame-type
		       (f.get generate-outcome
			      *co-domain-slot*)))
	      generate-outcome			; Recovered Impasse
	      verify-outcome))
	verify-outcome))
  )



;;;
;;; Function learn (previously called function review) performs step 2 of the
;;; following learning algorithm:
;;; 
;;; 0. Perform and Record Reasoning in TMXP
;;; 1. Failure Detection on Reasoning Trace 
;;; 2. If Failure Then 
;;;       Learn from Mistake:
;;;           Blame Assignment
;;;              Compute index as characterization of failure
;;;              Retrieve Introspective Meta-XP
;;;              Apply IMXP to trace of reasoning in TMXP
;;;              If Successful XP-Application then 
;;;                 Check XP-ASSERTED-NODES
;;;                 If one or more nodes not believed then
;;;                    Introspective questioning
;;;                    GOTO step 0
;;;              Else GOTO step 0
;;;           Post Learning Goals
;;;           Choose Learning Algorithm(s)
;;;           Apply Learning Algorithm(s)
;;; (3. If Learning Then
;;;        Evaluate Learning)
;;; 
;;; 
;;; ||||| Need to unify the interdiction-act of the bust-act with the
;;; interdiction-act that the sniff is related to off the precondition XP.
;;; 
;;; ||||| Change the sniff1 binding.  The HACK can be removed by implementing
;;; the review process.
;;; 
;;; How to handle the problem of which actor relation to explain. The specific
;;; dog sniffing the luggage or the more general dog being the actor of the
;;; detection MOP.
;;; 
;;; ||||| Need to put the results of each learning strategy applied in the
;;; main-result slot of the learning-node.
;;;
;;; Because this function performs a f.unify on the parameter, it must return
;;; the correct value of trace, and the calling function must setf the return
;;; value to the returned value.
;;; 
(defun learn (k-goal learning-node &aux (dummy-var (f.instantiate-frame dummy)))
  (do-break learn)
  (set-learning-episodes *Current-Result-Record*)
  ;; The k-goal is saved on an arbitrary slot of this dummy frame so that it will remain updated.
  ;; Otherwise the blame-assignment phase will change the value when it performs a f.unify
  ;; operation and the remove-achieved-goal call at the end will not work.  [cox 27jun95]
  (f.put k-goal dummy-var 'goal-var)
;  (with-character-style (*Style*)
    (format *aqua-window* "~%~%LEARNING PHASE . . .~%")
;    )
  
  (let* ((trace (goal-state k-goal))
	 (reasoning-failure
	   (return-failure trace))
	 (imxp nil))
    (f.unify (f.get learning-node 'strategy-choice)
	     ;; Automatically returns explanation.0
	     (l.strategy-decision		; Looks for an IMXP & sets index if found.
	       (return-decision-basis
		 learning-node)
	       reasoning-failure
	       learning-node
	       k-goal))
    (f.unify (f.get learning-node 'main-result)
	     (f.instantiate-frame outcome))
;      ((imxp (f.unify (explain reasoning-failure
;				  learning-node
;				  k-goal)
;			 (f.get trace 'introspection))))
    ;; Unify the Introspective-Meta-XP with the result of the verify phase.
    ;; This result was the mentally-initiates from <> to expectation-failure.
    ;; The following action also binds the A & E nodes to the rest of the imxp structure.
    ;; |||||| This is now part of the IMXP structures and need not be done. Will check to make sure before deleting
    ;; altogether [29dec93]
;     (f.unify
;	(f.get imxp 'link4)
;	(first (return-result
;		 (return-d-c-node
;		   trace
;		   'examination))))
;;;   (setf trace (f.unify (f.get imxp 'rc) trace))
    (execute-strategies
      (select-learning-strategies
	(decide-what-2-learn
	  (setf imxp (blame-assignment reasoning-failure
				       learning-node
				       k-goal
				       trace)))
	imxp)
      imxp
      learning-node)
    (set-model
      *Reasoning-Model*
      (cons imxp (get-model *Reasoning-Model*)))
    (remove-achieved-goal (f.get dummy-var 'goal-var))
    trace)
  )


;;;
;;; Function blame-assignment explains the reasoning failure with the imxp
;;; retrieved during l.strategy-decision. Just as with explanation of pyhsical
;;; events in the world, the explain function binds the imxp to the
;;; representation of the reasoning failure to produce an instantiated causal
;;; pattern for the mental events leading up to the reasoning failure. The
;;; instantiated imxp is returned as the value of the function.
;;; 
(defun blame-assignment (reasoning-failure learning-node k-goal trace)
  (let* ((explanation (explain (f.get reasoning-failure *co-domain-slot*)
			       learning-node
			       k-goal))
	 (imxp (if explanation
		   (f.unify explanation
			    (f.get trace 'introspection)))))
  (do-break blame-assignment)
  (format *aqua-window* "~%~s" (eval *goal-Queue*))
    (f.unify (f.get k-goal 'mxp)
	     (f.get imxp 'rc))
  (format *aqua-window* "~%~s" (eval *goal-Queue*))    
    (cond (imxp
	     (format
	       *aqua-window*
	       (str-concat
		 "~%~%Blame assignment has produced "
		 "explanation of reasoning failure:~%  ~s~%")
	       imxp)
	   )
	  (t
	   (format
	     *aqua-window*
	     "~%~%Blame assignment cannot explain reasoning failure.~%")))
    imxp)
  )



(defun oldreview (learning-node)  
  (let* ((xp nil)
	 (current-case (retrieve-case-from-META-XP nil))
	 ;HACK. See below.
	 (sniff1 (return-last-element (get-model *World-Model*)))
	 (old-xps (f.get current-case *explanations-slot*))
	 (merged-node nil)
	 (failures nil))
;    (with-character-style (*Style*)
      (cond (current-case
	     (cond ((not (multiple-value-setq
			   (merged-node failures)
			   (f.unify sniff1
				    (get-scene
				      1
				      current-case))))
		    (format *aqua-window* 
			    "~%Match fails.~%~s and ~s would not unify"
			    sniff1  (get-scene 1 current-case))
		    (format *aqua-window*
			    "~%because incompatibility in subnodes ~s."
			    failures)
		    (cond ((similar-p failures)
			   (tweak failures)
;			   (f.put! (cons instantiated-xp
;					 (f.get instance
; 			                        *explanations-slot*))
;				   instance 
;				   *explanations-slot*)
			   ))
		    ;; And here is the rest of the HACK.
		    (format *aqua-window*
			    "~%~%Found a better explanation for action of ~s"
			    (f.get sniff1 *actor-slot* *relation-facet*))
		    (format *aqua-window* "~%because ~s is a ~s.~%"
			    (second
			      (f.get (f.get sniff1 *actor-slot* *relation-facet*)
				     *explanations-slot*))
			    (get-abstraction
			      (frame-type 
				  (first
				    (f.get (f.get sniff1 
						  *actor-slot* 
						  *relation-facet*)
					   *explanations-slot*))))
			    )
		    (f.put! old-xps
			    (f.get sniff1 *actor-slot* *relation-facet*)
			    *explanations-slot*)
		    (format *aqua-window* "~%New explanation is ~s.~%"
			    (say-xp (first old-xps)))
		    (format *aqua-window* "~%~%Removing old explanation.~%"))
		   ))
	     (t
	      (if (null xp)
		  (format *aqua-window* "~%No action taken on review." )
		(format *aqua-window* "~%New XP: ~s." xp))))
;      )
    
	      ))

;;; Used to be major part of inner cond statement in the review function above.
    ;	     ((setf xp (apply-xp (frame-type (first old-xps))
				       ;; Here comes a major HACK.
;				       (f.get sniff1 *actor-slot* *relation-facet*)))
;		    (format *aqua-window* "~%Just before the unify.")
;		    (break)
;		    ;; Another major hack here: How does this know that the interdiction-acts
;		    ;; must be unified?
;		    ;; Also this is the location that f.unify fails because of attempt on
;		    ;; dog-authority merging.
;		    (f.unify (f.get current-case 'instrumental-scene)
;			   (f.get xp 'main-action))
;		    (manage-hypotheses (f.get xp *explains-node*)) ;manage-hypotheses is now
						;		    gen-questions.
;	     )

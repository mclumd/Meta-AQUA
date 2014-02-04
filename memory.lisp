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
;;;;			       File: memory.lisp
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



;;;; 
;;;; HISTORY
;;;; 
;;;; Changed the value of "memory-list" from a symbol to a string in function
;;;; store-memory, because it is passed as an argument to gentemp. In Allegro
;;;; 8.0 the function no longer takes symbol parameters. [mcox 3aug06]
;;;; 
;;;; Removed an extraneous ~s from last format statement in function
;;;; announce-index. [mcox 1nov05]
;;;; 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;       CONCEPTUAL MEMORY FUNCTIONS AND UTILITIES
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;
;;;; Implementational description of indexing structure & representation
;;;;
;;; 
;;; The first level of indexes is specified by a triple: relation (e.g.,
;;; actor), predicate (relation's domain such as bark) and value (co-domain of
;;; relation such as dog). In fact this relation (who did what?), that is, the
;;; actor of a action, is a salient feature.
;;; 
;;; A second level of indexing is currently specified by one (or more) slots of
;;; the original relation's predicate. An example is what a dog barks at. The
;;; value of the "to" relation is an at-location relation. So indexing as it
;;; stands searches the domains of each slot of the predicate. This is thus a
;;; tuple: role (e.g., to) and type of the domain of the value of the role
;;; (e.g., container).
;;;
;;; At the end of a sequence, the retrieve or index routine looks at or sets
;;; the memory-type: either question-type.0, xp-type.0, plan-type.0, or
;;; case-type.0.
;;;
;;;
;;;                  actor
;;;                  |     \ sniff
;;;           bark   |      MORE indexing ...
;;;                  V
;;;             micro-index.15
;;;                  |         \
;;;                  | dog       \ seal
;;;                  V             MORE indexing ...
;;;              micro-index.16
;;;                  |
;;;                  | to
;;;                  V
;;;            micro-index.24123
;;;                |             \
;;;    container  |                \ animate-object
;;;              V                  V
;;;     micro-index.24124        micro-index.24127
;;;             |                         |
;;;  xp-type.0  |                         | xp-type.0
;;;             V                         V
;;;         (BECAUSE.49207)       (XP-DEFENSIVE-BARK)
;;;
;;; 
;;; |||||| NOTE that in some instances the explanation will be a list of
;;; specific explanations (e.g., BECAUSE.49207), whereas other times it will be
;;; a list of types (e.g., XP-DEFENSIVE-BARK). Needs to be made completely
;;; consistent.
;;; 


;;; 
;;; Main Indexing Function.
;;; 
;;; Function do-index is the principle indexing function of Meta-AQUA. It takes
;;; a memory item to be indexed, the item's type classification, and a relation
;;; that serves a context for the mapping, and returns an instantiated index
;;; frame that represents the index. As side-effect, it places the memory item
;;; in conceptual memory via the micro-indexing implementational scheme
;;; described above. If the optional parameter not-destructive is non-nil then
;;; the memory item is placed in conceptual memory along with any other
;;; structures that happen to be there already, otherwise the previous items
;;; are overwritten.
;;;
;;; In addition, every memory item added to conceptual memory through indexing
;;; is placed on a retrieval list for finding similar items when storing. Thus,
;;; before an new item is indexed, the system checks the list (based on the
;;; kind of memory item it is) to see if there already exists an item that is
;;; of this type (see function "remindings"). We can therefore find forgotten
;;; or lost memories this way at storage time. This feature is needed for the
;;; forgotten goal and forgotten BK error.  [12dec93]
;;;
;;; NOTE that I removed the memory check referred to in the above paragraph for
;;; question memory-types, since these are suspended questions, rather than
;;; stored changes to the memory. We may wish to reinstate this feature to
;;; check for similar questions that remain from past stories. We can then use
;;; analogy for possible processing. The example is the question "Why did the
;;; authorities arrest the passenger?" It is repeated in subsequent stories.
;;; Right now I am not doing anything with the previous question when a new one
;;; is formed, so I need to remove the remindings check in the first cond
;;; clause. [5apr94]
;;; 
;;; |||||| Should put info on symbol-value of dummy frames concerning what the
;;; index represents, ie. how one gets to the index.
;;;
;;; Changed from using dummy nodes to micro-index nodes [nov93]
;;;
(defun do-index (memory-item memory-type relation
 		 &optional
		 not-destructive
		 forced-indexing
;;;  		 &rest
		 more-relations)
  "Index memory-type memory-item by relation."
;;;   (when (equal memory-type 'question-type.0)
  (do-break do-index)
;;;     (add-break main-indexing-loop))
  (cond ((and (not forced-indexing)
	      (not (equal memory-type		; If there is a reminding of an item already
 			  'question-type.0))	; in memory similar to the memory-item parameter, then 
	      (remindings memory-item))		;it is returned
	 )
	(t					;otherwise store the memory-item under some index.
	 (if (not (null memory-item))
	     (if (listp memory-item)
		 (mapcar #'store-memory
			 memory-item)
		 (store-memory memory-item)))
	 (let* ((secondary-index 
		  (main-indexing-loop 
		    relation
		    more-relations))
		(temp more-relations))
	   (announce-index
	     memory-item relation
	     secondary-index memory-type)
	   (if (null memory-item)		; We are removing some earlier memory-item, 
	       (and				; presumably for re-indexing.
		 t 				; Was (break "Do index") bfore t
		 (remove-from-index-list	; so try to remove the item from *indices*
		   (retrieve-memory
		     memory-type relation)))
	       (set-model
		 *indices*			; The following use of a temp variable is required because
		 (cons				; of the way LISP handles &rest arguments. 
		   (append			; |||||| But I commented the &rest out above.
		     (list memory-type
			   relation)
		     temp)
		   (get-model *indices*))))
	   (last-linkage relation
			 memory-item
			 secondary-index
			 memory-type
			 not-destructive))
	 )))


;;; 
;;; It is important to return the index since do-index MUST return it also.
;;; 
(defun last-linkage (context memory-item secondary-index memory-type not-destructive)
  (if (frame-var-p memory-item)
      (f.put-back-ptrs-for-index
	secondary-index
	memory-type
	memory-item))
  (if not-destructive
      (addprop secondary-index
	       memory-type
	       memory-item)
      (putprop secondary-index
	       memory-type
	       memory-item))
  ;; Return an instantiated frame representation of the index.
  (make-index context memory-item memory-type)
  )


;;;
;;; Function make-index assembles an instantiated frame representation of an
;;; index. Conceptually, an index is a mapping from a context to a memory item
;;; (implementationally, however, the index is composed of chains of micro
;;; indexes). Depending on the memory-type (question-type.0, xp-type.0,
;;; case-type.0, or plan-type.0), indexes currently can map to questions, xps,
;;; cases and plans. The instantiated index frame is returned by the function.
;;; 
(defun make-index (context
		   memory-item
		   &optional
		   (memory-type
		     (attempt-to-infer-type
		       memory-item)))
  "Return a frame representation of an index given a context and a memory item."
  (f.unify
    ;; |||||| The order of the following is important to the success of the unification.
    ;; Otherwise index-value-type.xxx and xp.type.0 will unify to  index-value-type.xxx.
    ;; Need to fix f.unify.
    (f.instantiate-frame
      `(index 
	 (type (value ,memory-type))
	 (relation (value ,context))
	 (memory-item (value ,memory-item))
	 ))
    (f.instantiate-frame index))
  )


;;;
;;; Function attempt-to-infer-type is used by function make-index in order to
;;; derive the memory-type of an item indexed in memory. It first looks to see
;;; if the item has a question status associated with it, otherwise it checks
;;; the isa link in the conceptual hierarchy to make such an inference. Nil is
;;; returned if none of these heuristics work.
;;; 
(defun attempt-to-infer-type (memory-item)
  (if (listp memory-item)
      (setf memory-item
	    (first memory-item)))
  (cond ((or (eq (f.get memory-item *status-slot*)
		 *question*)
	     (eq (f.chase-path memory-item
			       *explanations-slot*
			       '(memory-item *status-slot*))
		 *question*))
	 'question-type.0)
	((isa-p 'xp (list memory-item))
	 'xp-type.0)
	((isa-p 'plan (list memory-item))
	 'plan-type.0)
	((isa-p 'mop (list memory-item))
	 'case-type.0)
	(t nil))
  )


;;;
;;; Function main-indexing-loop is the loop that processes the first-level of
;;; indexing (as explained above) and the more-relations attributes when
;;; forming an index in the do-index function. For example, when indexing the
;;; detection explantion by dog-barks-at-container, the loop first places a
;;; micro-index (primary) on a link from the 'actor symbol off its 'bark
;;; property and another micro-index (secondary) on this symbol off the 'dog
;;; property. The second time through the loop initialization more-relations
;;; is empty (if only one item was on the more-relations list), so the loop
;;; exists. As it exists, however, the function handles the 'to-container
;;; relation. A micro-index (prime) is placed on the 'to property of the input
;;; secondary index and a micro-index (a new secondary index) is placed on the
;;; 'container property of the prime-index.  With no remaining relations to
;;; process, it then halts, returning the final secondary index.
;;;
;;; The function returns the changed value of the last secondary-index so that
;;; do-index can reset its value.  Main-indexing-loop assumes that
;;; more-relations is non-nil upon entry.
;;;
;;; The comments on the variables represent typical values. On the local
;;; variables, typical values are provided for both the initial time and
;;; second time through the loop.
;;; 
(defun main-indexing-loop (relation		; actor.XXXX
			   more-relations)	; (to.52015)
  (do-break main-indexing-loop)
  (do* ((current-index				
	  (frame-type relation)			; e.g., actor
	  secondary-index)			; e.g., micro-index.16
	(current-relation			
	  (first more-relations)		; e.g., to.52015
	  (first remaining-relations))		; e.g., nil
	(link1				        
	  (frame-type				; e.g., bark
	    (f.get relation *domain-slot*))	
	  (frame-type current-relation))	; e.g., to
	(primary-index 
	  (or (get current-index		; e.g., micro-index.15
		   link1)
	      (f.instantiate-frame
		micro-index))
	  (or (get current-index
		   link1)
	      (f.instantiate-frame		; e.g., new frame
		micro-index)))
	(link2					
	  (frame-type				; e.g., dog
	    (f.get relation *co-domain-slot*))
	  (frame-type				; e.g., container
	    (f.chase-path
	      current-relation
	      *co-domain-slot*
	      *domain-slot*)))
	(secondary-index 
	  (or (get primary-index		; e.g., micro-index.16
		   link2)
	      (f.instantiate-frame
		micro-index))
	  (or (get primary-index
		   link2)
	      (f.instantiate-frame		; e.g., new frame
		micro-index)))
	(remaining-relations			
	  more-relations			; e.g., (to.52015)
	  (rest remaining-relations))		; e.g., nil
	)
       ((null remaining-relations)
	(putprop current-index
		 link1
		 primary-index)
	(putprop primary-index
		 link2
		 secondary-index)
	;; Secondary-index is returned.
	secondary-index)
    (putprop current-index
	     link1
	     primary-index)
    (putprop primary-index
	     link2
	     secondary-index))
  )



;;;
;;; Function remove-from-index-list is called by do-index when a nil value is
;;; being indexed in memory. This represents an attempt to remove an item from
;;; memory. Therefore, this function is used to also remove the item from the
;;; global list of indices.
;;; 
(defun remove-from-index-list (old-memory-item
			       &optional
			       (index-list (get-model *indices*)))
  (cond ((null index-list)
	 nil)
	((equal old-memory-item (second (first index-list)))
	 ;; (break)
	 ;; Remove the matching item from *indices*
	 (set-model
	   *indices*
	   (append (set-difference
		     (get-model
		       *indices*)
		     index-list)
		   (rest index-list))))
	(t					; Try the next item on the list.
	 (remove-from-index-list
	   old-memory-item
	   (rest index-list))))
  )



;;;
;;; Function remindings checks a memory item before it is indexed into memory
;;; in order to see if it causes a reminding. This reminding represents
;;; something similar that is already in memory. If there is a reminding, then
;;; the similar item is returned, nil otherwise.
;;; 
(defun remindings (memory-item)
  (do-break remindings)
  (let ((old-memory
	  (some
	    #'(lambda (each-memory)
		(if
		  (and (not (eq memory-item each-memory))	;Self-reminings do not count.
		       (can-unify-p each-memory
				    memory-item))
		  each-memory))
	    (if (listp memory-item)
		(mapcan
		  #'(lambda (each-memory-store)
		      (let ((returned-item
			      (get-model
				(symbol-value
				  (get
				    (if (frame-var-p
					  each-memory-store)
					(first
					  (get-abstraction
					    each-memory-store))
					each-memory-store)
				    'memory-list)))))
			(if (not (listp returned-item))
			    (list returned-item)
			    returned-item)))
		  memory-item)
		(get-model
		  (symbol-value (get (frame-type memory-item)
				     'memory-list)))))))
    (if old-memory
	(forgotten-memory memory-item old-memory)))
  )



;;;
;;; Function forgotten-memory simply prints an appropriate statement to the
;;; user when the remindings function (called by do-index) runs into a similar
;;; memory item to the one that is being indexed. The similar old-memoryis
;;; returned as the value of the function.
;;; 
(defun forgotten-memory (memory old-memory)
;  (with-character-style (*Style*)
    (format
      *aqua-window*
      (str-concat
	"~%Reminded of old memory (~s) already "
	"~%  in memory which is similar to new item "
	"~%  (~s) to be stored.")
      old-memory
      memory)
;    )
  
  old-memory)



;;;
;;; Function store-memory adds a new memory to the memory-list of the the
;;; concept type of New-memory. If there is not already a memory list, a new
;;; one is created.
;;; 
(defun store-memory (new-memory)
  (announce-storage new-memory)
  (let ((concept-type
	  (first
	    (get-abstraction
	      new-memory))))
;;;     (break "store memory")
    (add-to-model
      new-memory
      (or
	(if
	  (get
	    concept-type
	    'memory-list)
	  (symbol-value
	    (get
	      concept-type
	      'memory-list)))
	(let ((new-list-name
	       ;; Changed from symbol to string argument. [mcox 3aug06]
	       (gentemp "memory-list")))
	  (setf (symbol-value  new-list-name)
		nil)
	  (setf
	    (get
	      concept-type
	      'memory-list)
	    new-list-name)
	  (new-model new-list-name)))))
  )


;;;
;;; Function announce-storage prints to the internal-structures window if in
;;; memory printing mode. It shows the new item being stored in memory. The
;;; function always returns t.
;;;
;;; |||||| Should possibly show the memory-list that it is going into.
;;; 
(defun announce-storage (memory-item)
  "Print memory item as they are stored in memory."
  (if (memory-monitor-mode-p)
      (let ((previous-window *aqua-window*))
	;; Change to structure window.
	(current-window *window3*)
	(format
	  *aqua-window*
	  "~%~%Storing new memory  --> ~s~%"
	  memory-item)
	;; Change back to output window.
	(current-window previous-window)))
  t
  )



;;;
;;; Function announce-index prints to the internal-structures window if in
;;; memory printing mode. It shows the index of the new item being stored in
;;; memory, the new item's type, and the relation which under which the
;;; indexing was performed. The function always returns t.
;;; 
(defun announce-index (memory-item relation index memory-type)
  "Print memory activity as indexes are added/changed in memory."
  (if (memory-monitor-mode-p)
      (let ((previous-window *aqua-window*))
	;; Change to structure window.
	(current-window *window3*)
	(cond ((null memory-item)
	       (format
		 *aqua-window*
		 "~%~%Index is being removed on memory item ~s.~%"
		 memory-item)
	       (format
		 *aqua-window*
		 "~%~%Item was indexed by relation ~s.~%~%"
		 relation))
	      (t
	       (format
		 *aqua-window*
		 "~%~%Memory item ~s (of type ~s) stored under index ~s.~%"
		 memory-item
		 memory-type
		 index)
	       (format
		 *aqua-window*
		 "~%~%Relation ~s was used for full indexing.~%~%"
		 relation)))
	;; Change back to output window.
	(current-window previous-window)))
  t
  )


;;;
;;; Function announce-retrieval prints to the internal-structures window if in
;;; memory printing mode. It shows ...
;;;
;;; NOTE that it is imperative that announce-retrieval return the
;;; retrieved-item passed to it.
;;; 
(defun announce-retrieval (retrieved-item memory-type relation)
  "Print memory activity as new items are retrieved from memory."
  (if (memory-monitor-mode-p)
      (let ((previous-window *aqua-window*))
	;; Change to structure window.
	(current-window *window3*)
	(cond ((null retrieved-item)
	       (format
		 *aqua-window*
		 "~%~%No item "
		 memory-type
		 relation))
	      (t
	       (format
		 *aqua-window*
		 "~%~%Memory item ~s "
		 retrieved-item)))
	(format
	  *aqua-window*
	  "was retrieved given memory type ~s and cue ~s.~%~%"
	  memory-type
	  relation)
	;; Change back to output window.
	(current-window previous-window)))
  retrieved-item
  )


;;; 
;;; NOTE search is bfs going upwards towards root (entity).  Or is it really
;;; bfs? Check ||||||
;;; 
(defun find-secondary-index (frame-type prime-index restrict-search)
 (if (not (equal 'entity frame-type))
     (let ((parent-list (get-abstraction frame-type)))
      ;; The following will try to use one of the main abstractions which isa the frame-type.
      (or (some #'(lambda (each-abstraction)
		   (get prime-index each-abstraction))
		parent-list)
	  (if (not restrict-search)
	      (some #'(lambda (each-abstraction)
		       (find-secondary-index
			each-abstraction
			prime-index
			restrict-search))
		    parent-list)))))
 )



;;; 
;;; Note that the index returned will be a list.
;;;
;;; ||||| Note also that this routine depends on frames having a
;;; static slot ordering. This is not guaranteed when f.put and
;;; f.put! is used.
;;;
;;; Optional parameter restrict-search limits the search for
;;; secondary indexes to the parent types of the current
;;; concept's type. That is, the search is one ply, rather than
;;; going all of the way to the root (entity). In effect it
;;; forces retrieve-memory to use the pre-6may94 search-method.
;;; 
(defun retrieve-memory (memory-type relation &optional restrict-search)
  (let* ((main-act (frame-type (f.get relation *domain-slot*)))
	 (prime-index
	   (get (frame-type relation) main-act))
	 (type-for-co-domain-of-relation
	   (frame-type (f.get relation *co-domain-slot*)))
	 (secondary-index
	   (or
	     (get prime-index
		  type-for-co-domain-of-relation)
	     (find-secondary-index
	       type-for-co-domain-of-relation 
	       prime-index
	       restrict-search)))
	 (last-secondary secondary-index))
    (do-break
      retrieve-memory
      "~%Main-act: ~s~%~%prime-index: ~s~%~%secondary-index: ~s~%~%last-secondary: ~s~%"
      main-act
      prime-index
      secondary-index
      last-secondary)
    (dolist (each-role (f.role-list main-act))
;      (format *aqua-window*
;	      "~%~%ROLE: ~s ~%" each-role)
      (cond ((setf prime-index
		   (get secondary-index each-role))
	     (setf secondary-index
		   (let* ((type-for-domain-of-role-slot
			    (frame-type
			      (f.chase-path 
				relation
				*domain-slot*
				each-role
				*domain-slot*))))
		     (or
		       (get prime-index
			    type-for-domain-of-role-slot)
		       (find-secondary-index
			 type-for-domain-of-role-slot
			 prime-index
			 restrict-search)))))))
    (announce-retrieval
      (or (get secondary-index memory-type)
	  (get last-secondary memory-type))
      memory-type relation))
  )


;;; 
;;; Print local variables as they would be inside a call of retrieve-memory.
;;; This function is useful to explore various relations in memory to see what
;;; indexes are associated with them.
;;; 
(defun print-locals (relation &optional (stream *aqua-window*))
  (let* ((main-act
	   (frame-type
	     (f.get relation
		    *domain-slot*)))
	 (prime-index
	   (get (frame-type relation)
		main-act))
	 (secondary-index
	   (get prime-index
		(frame-type
		  (f.get relation
			 *co-domain-slot*))))
	 (last-secondary secondary-index))
    (format stream
	    "~%Main-act: ~s~%" main-act)
    (format stream
	    "~%Prime-index: ~s~%" prime-index)
    (format stream
	    "~%Secondary-index: ~s~%" secondary-index)
    (format stream
	    "~%Last-secondary: ~s~%" last-secondary)
    ))




;;;;
;;;; Reset Functions for Memory Re-Initialization
;;;;

;;;
;;; Function reset-memory is used to remove the changes to memory performed by
;;; the dog-barking scenario #1. It is used when the user wishes to rerun the
;;; scenario during testing or run another mode without side-effects from the
;;; earlier runs. The function re-establishes the original (flawed) definition
;;; of bark, undoes the personally-interesting property on barking, and undoes
;;; the various indexing changes to questions and explanations performed by the
;;; learing algorithms executes in the scenario. An optionbal parameter allows
;;; the user to reset the *Story-Concepts* to the original scenario. This is
;;; used if one has already run scenario #2 (established by the call of
;;; set-story-4).
;;;
;;; The optional parameter which-scenario can be used to control which parts
;;; of memory is affected. The default is the original dog barking scenario,
;;; signalled by 'dog-bark, whereas a passed value of 'hit will modify only
;;; the memories associated with the handball scenario. The constant 'all will
;;; modify all memory.
;;; 
(defun reset-memory (&optional  reset-story (which-scenario 'dog-bark))
  (if reset-story
      (init-story *init*))
  (case which-scenario
    (dog-bark
      (reset-bark-scenario))
    (hit
      (reset-hit-scenario))
    (all
      (reset-bark-scenario)
      (reset-hit-scenario))
    (t
      (format
	*aqua-window*
	"ERROR: Bad arg, ~s, passed to reset-memory."
	which-scenario)))
  ;; Reset the memory lists for the items that are stored in memory.
  (setf (get 'actor 'memory-list) nil
	(get 'self-cause 'memory-list) nil
	(get 'xp-defensive-bark 'memory-list) nil
	(get 'imxp-anomaly-and-baffled 'memory-list) nil
	(get 'imxp-baffled-and-resolved 'memory-list) nil
	(get 'xp-injury-hit 'memory-list) nil)
;;; (setf *indices* nil)
  )


;;;
;;; Function remove-prior-IMXPs will remove all of the indexed IMXPs created
;;; during init-aqua. The main purpose is to disable learning. If no imxps can
;;; be found then no learning occurs. This is useful, for instance, when trying
;;; to establish a no-learning base-line of behavior in order to comare the
;;; performance of the system later during learning. [cox 5mar95]
;;; 
(defun remove-prior-IMXPs ()
  (do-index
    nil
    'xp-type.0
    (f.instantiate-frame
      `(mentally-initiates
	 (domain
	   (,*value-facet*
	    (not-equal-relation)))
	 (co-domain
	   (,*value-facet*
	    (expectation-failure))))
      *predefined-instance*))
  (do-index
    nil
    'xp-type.0
    (f.instantiate-frame
      `(mentally-initiates
	 (domain
	   (,*value-facet*
	    (truth)))
	 (co-domain
	   (,*value-facet*
	    (retrieval-failure))))
      *predefined-instance*)))



(defun reset-bark-scenario ()
  "Reset memory to initial conditions during the dog-barking scenario."
  (setf dog-barks
	(f.modify 'dog-barks
		  '(animate-object)
		  'to
		  'domain))
  (setf (get 'dog-barks 'personally-interesting) nil)
  ;; Is the following necessary considering that init-global-vars (?) does
  ;; it at the beginning of function meta-aqua?
  (let* ((new-bark
	   (f.instantiate-frame
	     `(dog-barks
		(,*actor-slot*
		 (,*value-facet*
		  (dog)))
		(to (,*value-facet*
		     (at-location
		       (,*domain-slot*
			(,*value-facet*
			 (container)))))))
	     *predefined-instance*))
	 (actor-relation
	   (f.make-relation
	     new-bark
	     *actor-slot*)))
    (do-index (list (f.instantiate-frame XP-DEFENSIVE-BARK))
	      'xp-type.0
	      actor-relation)
    (do-index nil
	      'question-type.0
	      actor-relation)
    (do-index nil
	      'xp-type.0
	      actor-relation
	      nil
	      nil
	      (list
		;; Note that the make-relation call must be done during the first do-index 
		;; call in order to establish the 'to relation facet. Then any remaining 
		;; do-index calls can simply perform a f.get.
		(f.make-relation
		  new-bark
		  'to
		  )))
    (dolist (each-object '(animate-object physical-object hiding-object))
      (f.put! (f.instantiate-frame
		`(,each-object)
;;; 		(animate-object)
		*predefined-instance*)
	      (f.get new-bark 'to)
	      *domain-slot*)
      (do-index nil
		'xp-type.0
		actor-relation
		nil
		nil
		(list
		  (f.get-relation
		    new-bark
		    'to))))
    )
  (do-index nil
	    'question-type.0
	    (f.make-relation
	      (f.instantiate-frame
		`(arrest
		   (,*actor-slot*
		    (,*value-facet*
		     (authority))))
		*predefined-instance*)
	      *actor-slot*))
  )


;;;
;;; Started the following but got tripped up when the get-sub-indexes hit
;;; special read macro definitions or something on symbol plists. Forget
;;; the next 3 functions for now.
;;;

(defun destroy-micro-index (m-index)
  (setf (symbol-plist m-index) nil)
  )

(defun destroy-micro-index-chain (m-index)
  (cond ((null m-index)
	 nil)
	(t
	 (dolist (each-sub-index (get-sub-indexes m-index))
		  (destroy-micro-index-chain each-sub-index))
	 (destroy-micro-index m-index)))
  )


(defun get-sub-indexes (m-index)
  (mapcan #'(lambda (each-plist-item)
	      (if (isa-p 'micro-index (list each-plist-item))
		  (list each-plist-item)))
	  (symbol-plist m-index))
  )

	      
(defun reset-hit-scenario ()
  "Reset memory to initial conditions during the handball scenario."
  (setf hit
	(f.modify 'hit
		  '(animate-object)
		  'object))
  (setf (get 'hit 'personally-interesting) nil)
  ;; Is the following necessary considering that init-global-vars (?) does
  ;; it at the beginning of function meta-aqua?
  (let* ((new-hit
	   (f.instantiate-frame
	     `(hit
		(,*actor-slot*
		 (,*value-facet*
		  (person)))
		(to (,*value-facet*
		     (at-location
		       (,*domain-slot*
			(,*value-facet*
			 (toy)))))))
	     *predefined-instance*))
	 (actor-relation
	   (f.make-relation
	     new-hit
	     *actor-slot*)))
    (do-index (list (f.instantiate-frame XP-INJURY-HIT))
	      'xp-type.0
	      actor-relation)
    (do-index nil
	      'question-type.0
	      actor-relation)
    (do-index nil
	      'xp-type.0
	      actor-relation
	      nil
	      nil
	      (list
		;; Note that the make-relation call must be done during the first do-index 
		;; call in order to establish the 'to relation facet. Then any remaining 
		;; do-index calls can simply perform a f.get.
		(f.make-relation
		  new-hit
		  'to
		  )))
    (f.put! (f.instantiate-frame
	      '(animate-object)
	      *predefined-instance*)
	    (f.get new-hit 'to)
	    *domain-slot*)
    (do-index nil
	      'xp-type.0
	      actor-relation
	      nil
	      nil
	      (list
		(f.get-relation
		  new-hit
		  'to)))
    )
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; REMOVING INDEXED MEMORY ITEMS
;;;
;;; The following set of functions are a down a dirty method for removing
;;; indexed questions during memory reset. I still need to adjust *indices*
;;; itself as they are removed. [cox 27feb95]
;;;




;;;
;;; Function remove-old-questions will remove all indexed memory items that are
;;; of type question (from *indices* by default; that is, the indices created
;;; during story processing, rather than indices created during Meta-AQUA
;;; initialization).
;;; 
(defun remove-old-questions (&optional
			     (index-list-name
			       *indices*))
  (remove-indices
    'question-type.0
    index-list-name)
  )


;;;
;;; Function remove-indices will remove any type of indexed memory items
;;; (xp-type.0, question-type.0, etc.). The function returns the number of
;;; items removed from memory and adjusts the model named by index-list-name to
;;; no longer contain the index.  The call to function remove-index actually
;;; removes the item from memory, however.
;;;
(defun remove-indices (index-type
		       &optional
		       (index-list-name *indices*)
		       &aux
		       (index-list (get-model index-list-name))
		       processed-list
		       (number-removed 0))
  (dolist (each-index index-list)
    (when (and (eq (first each-index) index-type)
	       (not (member (second each-index)
			    processed-list)))
      (setf processed-list
	    (cons (second each-index)
		  processed-list))	   
      (when
	(remove-index				; Remove index acts as a predicate
	  (second each-index)			; and actually removes the item from memory.
	  index-type)
	(set-model				; Here we remove the index from the global list
	  *indices*				; of indices created during story processing.
	  (set-difference
	    (get-model index-list)
	    (list each-index)
	    :test #'equal))
	(setf number-removed (+ 1 number-removed))
	(format					; User feedback.
	  *aqua-window*
	  "~%Removed question ~s.~%~%"
	  each-index))))
  number-removed
  )

(defun remove-index (index index-type &aux removed-a-link)
  (dolist (each-micro-index
	    (extract-micro-indices
	      (get index 'bkptrs)
	      index-type))
    (when (get each-micro-index
	       index-type)
      (format
	*aqua-window*
	"~%Index ~s Micro-index ~s Memory item ~s~%"
	index
	each-micro-index
	(get each-micro-index
	     index-type))
      (setf
	(get each-micro-index
	     index-type)
	nil)
      (setf removed-a-link t)))
  removed-a-link
  )


(defun extract-micro-indices (back-pointer-list index-type)
  (cond ((null back-pointer-list)
	 nil)
	(t
	 (append
	   (extract-micro-index
	     (first back-pointer-list)
	     index-type)
	   (extract-micro-indices
	     (rest back-pointer-list)
		   index-type))))
  )


(defun extract-micro-index (back-pointer index-type)
  (if (member index-type back-pointer)
      (return-if-micro-index back-pointer))
  )


(defun return-if-micro-index (back-pointer)
  (cond ((null back-pointer)
	 nil)
	((isa-p 'micro-index
		(list (first back-pointer)))
	 (list					; Must be a list (of one element) 
	   (first back-pointer)))		; for append to work in function extract-micro-indices
	(t
	 (return-if-micro-index
	   (rest back-pointer))))
  )



;;; 
;;; A useful non-destructive utility [cox 5mar95]
;;; 
(defun rem-any-questions (alist)
  (cond ((null alist)
	 nil)
	((eq (first alist)
	     'question-type.0)
	 (rem-any-questions (rest alist)))
	(t
	 (rem-any-questions (rest alist))))
  )



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
;;;;			    File: understander.lisp
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





;;;
;;; Some possibilities for addition to standard Aqua:
;;;   Creating tests to determine the best XP 
;;;   - this  will allow detection of disinfo, lies, partial truths, 
;;;     or otherwise false explanations. Play what is wrong with this
;;;     explanation. Integrate with detective domain.
;;;
;;;   Adding recency and frequency control. 
;;;     Integrate with key domain.
;;;
;;;   Meta-XPs.
;;;

;;;
;;; Can we find old Meta-XPs during processing to use as shortcuts by successive
;;; refinement of the current Meta-XP in memory? We then should run into other 
;;; similar past traces.
;;;

;;;
;;; Mention how a vaiable points to its instantiation, then stress how
;;; the car of this instantiation points to the frame pattern with all
;;; its bindings. Eg PTRANS.666 -> (PTRANS (actor (value PERSON.23))...)
;;;     (eval (car PTRANS.666)) -> (PTRANS (actor (value =X))...).
;;;

;;|||||  (NOTE: 5 pipes mark comments that eventually must be addressed.)
;;; Need to uncomment the entity test eventually.
;;; 
;;; Will we ever need the special handling test?
;;;
;;; Need to modify the search-by2 and other routines make to search for cases.
;;;
;;; ISA hiearchy is still incomplete. Include other definitions from Ashwin's
;;; files.
;;;
;;; Need function to take a defined frame (w/out X.12 type form) and return 
;;; its symbol-value. Could just modify *FRAME* to do it, but we should be 
;;; careful of how to assure that frame variable was indeed passed. So modify 
;;; frame-var-p also? No, I just modified *FRAME*. If a non frame-var-p atom is
;;; passed and the atom is bound, then the symbol value is returned. No other checking
;;; is performed. 25 Oct 91
;;;
;;; Make sure that the lowest-common-ancestor function will work on isa 
;;; hiearchies as in paper notes.
;;;
;;; ALL OF THE CANNED FORMAT STATEMENTS REALLY NEED TO BE GENERATED FROM A 
;;; META-XP REPRESENTING THE CHAIN OF REASONING!!! But this is not central to the research!
;;;
;;; HAVE TWO RUNS THAT CAN DEMONSTRATE THE SPEEDUP OR OTHER BENEFITS OF LEARNING
;;; WITH AND WITHOUT META-XPS.
;;; 
;;; Problem with AGENCY: The dog is not the authority, only the agent of the 
;;; police.
;;;
;;; When f.put! is used on a facet which already has a filler there may be a 
;;; problem with the backlinks and future processing. Will other nodes be 
;;; updated so that they do not point any longer to the location? If it is not 
;;; done right then when these other nodes are changed or unified with another,
;;; then the algorithm may update the new change made by f.put! The example 
;;; which made me think of this possibility is having the program try to recover
;;; from overgeneralization. The program believes that the dog is the agent of
;;; the detection leads to the faulty conclusion the dog is the agent of the
;;; interdiction act. Later we want to learn that the dog was but an agent so 
;;; the program must change the extant instantiations of dog outside detection 
;;; to authority. f.put! may cause the problem discussed above.
;;;
;;; The above discussion is a little off, but the first line is most important. The 
;;; problem was really that if A is a filler to be replaced by B, then if f.put! did
;;; not change the backpointers of A, any process that subsequently did a unify with
;;; A will affect B, since the location is still referenced in A's list of 
;;; backpointers. -This is now fixed. 23 Apr 91.
;;;
;;; When reading the arrest scene, need to represent its larger context: The BUST. 
;;; The interdiction-act can be a part of the bust. We instantiate the bust, then 
;;; try to relate the sniff with review process. How is the reminding done so that
;;; the PRECONDITON XP is properly found and applied?
;;;
;;; Make the program more similar to the reasoning model presented in the SAIC paper.
;;; We demand a separate decision process which select the reasoning strategy to use,
;;; eg. CBR or explanation or analogy. How is this decision made if for example both 
;;; CRB and explanation have potential. In the XP sketched in the SAIC paper the 
;;; reasons for performing explanation was that there existed an index to an XP. 
;;; However there could also be an index to a case as well. Does the system use the
;;; first strategy that "comes to mind" or does it arbitrate between all 
;;; possibilities. Here is the serial vs. parallel issue among others.
;;;
;;; Interesting. Indexes can be variable. To index an explanation for why an agent does X
;;; one uses X and the agent, but to index a case of arrest we do not refer to the agent.
;;; I suppose that this is because the agent is usually an authority. Instead we use what
;;; the crime is. This is the variable which varies to produce dependent cases? Should 
;;; consider this more.
;;;
;;; NOTE that when the program instantiates the interdiction-act in the bust because it
;;; reads about the arrest, the system implicitly infers the existence of the confiscation
;;; even though it is not mentioned in the story. This will not be registered in the 
;;; Trace-Meta-XP, but that may not be wrong. Many inferences humans make surely are
;;; subtle and unconscious whereas others will be deliberate. The META-XP registers the
;;; deliberate ones.
;;; 
;;; By making the processing explicit using Trace-Meta-XPs we produce side effect
;;; of making parameter passing and return values implicit from the view of
;;; a single function. Especially parameter passing. Function returns and 
;;; function side effect is now a slot in a structure, so I guess it is more
;;; explicit. However instead of the programmer being able to point to the 
;;; formal parameter list, we instead have to assume knowledge concerning where 
;;; in a Trace-Meta-XP the  is. What are the implications of this trade-off?
;;;
;;; After the program hypothesizes that sniffing can be viewed as a detection,
;;; it then reviews other detection methods that it knows about, to see if they
;;; can fit the description (is this a conscious review or a reminding; how are
;;; reminding and review related?). Then the program can post a specific
;;; hypothesis that it needs to have a new type of detection-method and thereby
;;; later modifying the isa-hierarchy. ???
;;;
;;; NEED TO CONSIDER THE DIAGRAM AND COMMENTS FROM BLUE NOTEBOOK 22FEB91 !!!
;;; These comments show many more representational features for a META-XP. 
;;; Where are the limits of an META-XP? What is the functional role of these 
;;; structures? Do we need to divide the functions, instead of having such a 
;;; large and global structure?
;;;
;;; When f.unify fails it generates a failures list. This represents an anomaly!
;;; Generates additional questions. Both in sniff-detection anomaly and in 
;;; dog-authority anomaly.
;;; 
;;; Also need to generate a specific question asking how the dog physically 
;;; accomplished the detection.
;;;
;;; The frame system DOES have to use the .list designator. Either that or
;;; require all facet fillers to be lists, even if they have only a single 
;;; item. The problem lies in the ambiguity between the following:
;;;   (dummy.101 (ptrans (actor (value =x))))
;;; and
;;;   (ptrans (actor (value =x))).
;;; For the time we use a hack. A list of frames is defined to be when
;;; the first item is either a listr, a frame identifier (ie. ptrans.101)
;;; or a variable identifier (ie. =X).
;;;
;;; Need function f.bound-put! in frame.lisp which destructively adds a filler
;;; to a frame. It differs from the normal f.put! though since if the facet is
;;; bound to other facets in the same frame, then it changes their value also.
;;; To do this we need to keep track of the bindings better in the frame
;;; definitions.
;;; 
;;;  Still seem to have a problem with backpointers.
;;;
;;; When unifying two nodes there is a problem with system slots such as a 'slot slot.
;;; I was creating one in f.make-relation and subsequently using it (or planning to).
;;; However even though 'actor is a bound symbol, the program crashes when it hits
;;; (slot (value actor)) slot.
;;;
;;; Came up againt the problem again. I suppose this is related to the fact that actor is not
;;; a frame variable as actor.23 is. I now need system slots so I am starting to implement
;;; some of the suggestions below. The slot slot will be a property like isa. Will worry about
;;; defining global system slots later. 21 Aug 91. NOTE though that we must deciode how to
;;; specify a slot slot when defining particular relations,
;;; e.g., (define-relation X (domain...)(co-domain...)(slot x)). 26 Aug.91


;;; All system slots should be made properties of the variables instead of sublist as is now
;;; the case with the isa slot. This would make unification easier. How will the slots be
;;; defined? Since system slots should be common to all frames, we could have a special
;;; definition of the frame 'entity' which all frames share the slots of. Also realize that
;;; the function f.unify may be affected. When two slots are unified and they are not of the
;;; same type, we need to make sure that the result has the correct slot property. But since
;;;  the result of the unify is guaranteed to be the most specific frame, it will have the
;;; right slot property, a new one will not be created.

;;; Another problem with lists of frames: FACETS. If a slot is a list of frames, then the
;;; order must be significant since there could be an associated list of relation facets, etc.
;;; The order of the list itself may NOT be significant if one views the associated lists as
;;; having a corespondance, e.g., (slot1 (value (v1 v2 v3))(relation (r1 r2 r3))). However if
;;; one one is provided with a special facet, all others must at least be given a dummy. For
;;; instance notice what happens if slot1 has values v1..v3 and v2 is given a relation facet.
;;; Without some kind of placeholder we do not know what value the relation coresponds to.
;;; Dummies must be padded for the longest list. Consider a slot with three constraint facets.
;;; One may have only one value known, but may later add a fourth. When adding the first
;;; value the system needs to pad two dummies in the value facet list. However when adding
;;; a fourth value the system must pad an extra dummy constraint. Notice also that there is
;;; no way the frame systems provides to modify the ith facet in a list. The user must read
;;; the value and insert a new value in the appropriate place. The frame system should then
;;; be responsible for updating (adding of deleting) the remaining facets when a change occurs
;;; by a f.put ? 4 Sep 91
;;; 
;;; There is a choice to be made in the manner in which one indexes memory:
;;; When the program learns that there are distinctions to be made between
;;; dogs barking at animate objects (threaten explanation) and dogs barking
;;; at inanimate objects (detection explanation), and that dogs barking in
;;; general is not specific enough, there are two possibilities for indexing.
;;; AQUA would handle this by creating new categories of barking to index
;;; the separate explanations. Meta-AQUA will for the time use indexes that
;;; have the additional features on barking, while barking is still the category.
;;; What is the trade-off between the two methods? 20 Oct 91
;;; 
;;; How to represent questions and index answers such as
;;; Why did the dog bark?
;;; Why was there barking?
;;; Why did the dog bark at x?
;;; Why does the dog bark at red objects?
;;;   20 Oct 91
;;;



(defun tweak (failure-list)
  ; tweak-node will be a (node slot filler) triple returned by view-as.
  ; The facet is guaranteed to be *value-facet*.
  (let ((tweak-node (apply #'view-as failure-list)))
    (cond (tweak-node				; if view-as returned something
	   (format *aqua-window*
		   "~%~%~s can be viewed as ~s."
		   (second failure-list)
		   (first failure-list))
	   (format *aqua-window*
		   "~%because both ~s and ~s are ~s."
		   (second failure-list)
		   (third tweak-node)
		   (get-abstraction 
		     (frame-type
		       (second failure-list))))
	   t)
	  ((setf tweak-node			; else set it with the reverse of failure-list.
		 (apply #'view-as
			(reverse failure-list)))
	   (format *aqua-window*
		   "~%~%~s can be viewed as ~s."
		   (first failure-list)
		   (second failure-list))
	   (format *aqua-window*
		   "~%because both ~s and ~s are ~s."
		   (first failure-list)
		   (third tweak-node)
		   (get-abstraction 
		     (frame-type (first failure-list))))
	   t))
    (cond (tweak-node
	   (format *aqua-window*
		   "~%~s is the value of the ~s slot of ~s.~%"
		   (third tweak-node) 
		   (second tweak-node)
		   (first tweak-node))
	   (format *aqua-window*
		   "~%Tweak succeeds.")
	   t)))
    )



;;;
;;; ||||| NOTE that it does not make sense to check for same main-result
;;; unless both nodes are MOPs or specializations of MOPs. Fix this.
;;; 
(defun similar-p (failures)
  (let ((lca-type nil)
	(result-type nil)
	(success nil))
    (cond ((multiple-value-setq 
	     (success result-type) 
	     (apply #'same-results-p failures))
	   (format
	     *aqua-window*
	     "~%~%However they are somewhat similar.")
	   (format
	     *aqua-window*
	     "~%~s and ~s both have ~s as a main-result."
	     (first failures)
	     (second failures)
	     result-type)
	   (cond ((multiple-value-setq 
		    (success lca-type)
		    (apply #'siblings-p failures))
		  (format
		    *aqua-window*
		    "~%Also they are siblings at the ~s level."
		    lca-type)
		  (values t result-type lca-type))
		 (t
		  (values nil result-type lca-type))))
	  (t
	   (format
	     *aqua-window*
	     (str-concat
	       "~%~%~s and ~s are not similar. "
	       "They have different main-results.")
	     (first failures)
	     (second failures))
	   (values nil result-type lca-type)))))




;;;
;;; Function outcome-template creates a representation to be placed on the
;;; outcome of the hypo-node. It says that knowing that there was no xp in the
;;; foreground knowledge initiates the knowledge of retrieval failure. That
;;; is, the reasoner is stumped.
;;; 
(defun outcome-template (answer hypo-node)
  `(mentally-initiates
     (results- (,*value-facet*
		,hypo-node))
     (,*domain-slot*
      (,*value-facet*
       (truth =truth-relation
	      (,*domain-slot*
	       (,*value-facet* =xp))
	      (,*co-domain-slot*
	       (,*value-facet* ,*out*)))))
     (,*co-domain-slot*
      (,*value-facet* 
       (retrieval-failure
	 (initiates- (,*value-facet* =domain))
	 (expected-outcome
	   (,*value-facet*
	    (xp =xp
		(truth (value ,*out*)
		       (relation =truth-relation))
 		(results- (,*value-facet* ,hypo-node))
		)))
	 (actual-outcome
	   (,*value-facet* ,answer))))))
  )



;;;
;;; Function exists-in-story is a predicate that returns t if the input
;;; assertion can unify with a concept in the world model or a concept
;;; reachable from some concept in the world model.
;;; 
(defun exists-in-story (assertion)
  (let ((found nil))
    (or
      (some
	#'(lambda (each-world-concept)
	    (when
	      (can-unify-p
		each-world-concept
		assertion)
	      (if *Show-Bugs*
		  (format
		    *aqua-window*
		    "~%Asserted node ~s already known"
		    assertion))
	      (if *Show-Bugs*
		  (format
		   *aqua-window*
		   "~%In first conditional test."
		   assertion))
	      (if *Show-Bugs*
		  (format
		   *aqua-window*
		   "~%each-world-concept:~s."
		   each-world-concept))
	      (setf found t)
	      ))
	(get-model *World-Model*))
      (some
	#'(lambda (each-world-concept)
	    (f.traverse-frame
	      each-world-concept
	      #'(lambda (current-frame parent role facet-name level)
		  (when
		    (and
		      (not (visited-p current-frame))
		      (can-unify-p current-frame
				   assertion))
		    (if *Show-Bugs*
			(format
			  *aqua-window*
			  "~%Asserted node ~s already known"
			  assertion))
		    (if *Show-Bugs*
			(format
			 *aqua-window*
			 "~%In second conditional test."
			 assertion))
		    (if *Show-Bugs*
			(format
			 *aqua-window*
			 "~%Current frame:~s~%parent:~s~%role:~s~%facet-name:~s~%level:~s."
			 current-frame parent role facet-name level))
		    (setf found t)))))
	(get-model *World-Model*)))
    found)
  )




;;;
;;; Want to view the source as the target concept. To do this we search for a
;;; value facet in the target which is the same type as the source. Returns the
;;; node (as slot filler pair) at which source was unified, or nil if no
;;; unifiaction is possible.
;;; 
;;; ||||| This heuristic will not work very well if there are multiple
;;; source-types within target. For example there could have been many mtrans
;;; inside of an interdiction-act.
;;;
(defun view-as (target source)
  (some #'(lambda (each-slot)
	    ;; Oh the HACKery!
	    (if (not (equal 'instrumental-scene-of
			     (slot->role each-slot)))
	    (some #'(lambda (each-facet)
		      (let ((filler (facet->filler each-facet)))
			; ||||| Do I really want to check for filler being an instance?
			; Ie. one with a period imbedded in it. We may allow abstractions.
			(if (and (frame-var-p filler)
				 (equal (facet->facet-name each-facet)
					*value-facet*))
			    (or (cond ((intersection (get-abstraction filler)
						     (get-abstraction (frame-type source)))
				      ;Question this relation as only hypothesized to be true.
				       (mark-as-question
					 ;; ||||| Since all input has relations explicitly added now,
					 ;; is the call of f.mmake-relation necessary (harmful)?
					 (f.make-relation target (slot->role each-slot))
					 *hypothesized-in*)
				       ;; ||||| Do we really need this here?
;				       (with-character-style (*Style*)
					 (format
					   *aqua-window*
					   (str-concat
					     "~%Trying to understand why actor "
					     "performs this act.~%"))
;					 )
				       
				       (setf filler
					     (f.unify filler
						    source))
				       (setf source filler)
				       ; ||||| Is filler and source now guaranteed to be same?
				       (list target (slot->role each-slot) filler)))
				(view-as filler source)))))
		  (slot->facets each-slot))))
	(f.slot-list target))
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;           THREE PHASES OF UNDERSTANDING 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;
;;;; Question Identification Phase
;;;;
;;;
;;; Currently two strategies exist to choose from in the Question
;;; Identification phase: Questioning (function pose-question) and
;;; skimming (function skim).
;;; 



;;;
;;; Function pose-question actually poses the question "Why did the actor of
;;; the event passed as the concept parameter perform the action?" Modified so
;;; that it will also ask why xps explain their explains node, and more
;;; generally, why values are domains of relations. [30oct94]
;;;
;;; How to mark this as a question explicitly. It is a knowledge goal so we
;;; know this, but how did Ashwin do it? Consider that the *explanations-slot*
;;; is really supposed to be a facet on the actor slot. Now how do we say "Why
;;; did the actor choose to enter into this relation. Tag the XP with status =
;;; question!
;;;
;;; For example, the function parameters (and local var) might be as follows:
;;; action    = DOG-BARKS.25745
;;; anomaly   = ANOMALY.26311 -> (ANOMALY
;;; 				   (EXPECTED-OUTCOME (VALUE ANIMATE-OBJECT.26303))
;;; 				   (ACTUAL-OUTCOME (VALUE LUGGAGE.25748))
;;; 				   (ACTION (VALUE DOG-BARKS.25745))
;;; 				   (PATHS (VALUE LITERAL.26312)))
;;;             LITERAL.26312 -> ((OBJECT) (TO DOMAIN))
;;; supergoal = GOAL.26225 -> (GOAL (GOAL-OBJECT (VALUE ID.26224)))
;;;             ID.26224 -> (ID (CO-DOMAIN (VALUE DOG-BARKS.25745)))
;;; relation  = ACTOR.25747 -> (ACTOR (DOMAIN (VALUE DOG-BARKS.25745))
;;; 				      (CO-DOMAIN (VALUE DOG.25746)))
;;; 
(defun pose-question (action anomaly supergoal)
  (let* ((relation
	   (f.get-relation
	     action (map-2-slot action t)))	; NOTE THAT MAP-2-SLOT MAY SET ADD-BREAK
	 (tmxp (processing-trace supergoal))
	 (is-repeat-question?
	   (not (null (f.get relation *explanations-slot*))))
	 )
    (do-break pose-question			; Add-break can be set by map-2-slot
	      (str-concat
		"Action arg for pose-question "
		"neither mop nor xp"))
    (if (not is-repeat-question?)
	(f.put!
	  (list
	    (mark-as-question
	      (f.instantiate-frame
		`(xp
		   (,*explains-node*
		    (,*value-facet*
		     ,relation
		     ;; (entity)
		     ))
		   (mxp
		     (,*value-facet* ,tmxp))))
	      *hypothesized*))
	  relation
	  *explanations-slot*))
    (print-question-posed
      relation
      action
      is-repeat-question?) 
    (when (not is-repeat-question?)
      (index-question
	relation 
	is-repeat-question?)
      (index-anomaly2 relation
		      (symbol-value		; Paths are in a literal.
			(f.get anomaly 'paths))))
    ;; Post goal to generate hypothesis.
    (spawn-sub-goal
      *current-character*
      (make-goal-state 'generate
		       *current-character*
		       relation)
      'knowledge-acquisition-goal
      'seven.0
      supergoal)
    relation)
  )



(defun print-question-posed (relation
			     action
			     &optional
			     is-repeat-question?
			     (stream *aqua-window*))
  "Provide English feedback for the questions posed."
;  (with-character-style (*Style*)
    (format
      stream
      (str-concat
	(if is-repeat-question?
	    "~%~%Repeating Old Question: ~s~%  "
	    "~%~%Posing Question: ~s~%  ")
	(cond ((isa-p 'mop (list action))
	       "Why did the ~s ~s perform the ~s?~%")
	      ((isa-p 'xp (list action))
	       "Why is the ~s ~s explained by this ~s explanation?~%")
	      (t
	       "Why is ~s ~s the domain of the ~s relation?~%" )))
      relation
      (frame-type
	(f.get action
	       (map-2-slot action)))
      (f.get action
	     (map-2-slot action))
      (frame-type action))
;    )
  
  )



;;; 
;;; [30oct94]
;;; 
(defun map-2-slot (action &optional break)
  "Map an action to a slot for question posing."
  (cond ((isa-p 'mop (list action))
	 *actor-slot*)
	((isa-p 'xp (list action))
	 *explains-node*)
	(t
	 (if break
	     (add-break pose-question))
	 *domain-slot*))
  )


;;;
;;; Function skim is a function which basically makes a minimal set of
;;; inferences such as those involved with story coherence.  The function
;;; attempts to understand the input concept through simple script application.
;;; It also performs a print operation and deletes the current goal from the
;;; *Goal-Queue* (which is a small hack).
;;;
(defun skim (concept id-node k-goal)
;  (assert (eq concept
;	      (first
;		(get-model
;		  *World-model*)))
;	  (*World-Model*)
;	  "Error in Skim")
;  (with-character-style (*Style*)
    (format
      *aqua-window*
      (str-concat
	"~%~s is not a very interesting "
	"concept.~%  Skimming . . .~%")
      concept )
;    )
  
  (remove-achieved-goal k-goal)
  (scriptify *World-Model*)
  )




(defun q.strategy-decision (decision-basis concept)
  (let ((reason (interesting-p concept))
	(k-state (f.instantiate-frame
		   knowledge-state)))
    ;; The following is the reason we will generate a question.
    (f.put-all!
      (or reason
	  (list 'noreason nil))
      k-state
      'believed-item
;	    (f.instantiate-frame
;	      (if reason
;		  (list concept reason)
;		  (list 'noreason nil)))
      )
    (f.put!
      (list k-state)
      (f.get
	decision-basis
	'knowledge)
      'members)
    (if reason
	'questioning.0
	'skimming.0)
    ))
  


(defun q.runstrategy (id-node choice concept k-goal)
  (if (equal choice
	     'pose-question.0)
      (do-break q.runstrategy))
  (f.unify (f.get id-node 'main-result)
	   (f.instantiate-frame outcome))
  (case choice
    (questioning.0
      (f.put! (list (pose-question
		      concept
		      (f.get
			(first (return-decision-basis
			id-node))
			'believed-item)
		      k-goal))
	      (f.get id-node 'main-result)
	      'members)
      'pose-question.0)
    (skimming.0
      (values 'skim.0
	      (skim concept id-node k-goal)))
    ( t (format
	  *aqua-window*
	  "ERROR: unknown q-strategy - ~s."
	  choice)))
  )


;;;
;;; The function identify-question not only performs the primary role of
;;; determining whether there is a question that must be posed, but if there is
;;; not (and thus the program skims the input), then it also is the function
;;; that actually updates the script match number and passes it all the way
;;; back to Meta-AQUA to be returned (and if spinning automatic story
;;; generation then passing it back to spinqua).
;;; 
(defun identify-question (k-goal id-node script-match-number)
  (let ((new-input (goal-state k-goal))
	(which-strategy nil)			; Note that which-strategy is really a dummy variable.
	(new-script? nil))
    (f.unify
      (multiple-value-setq
	(which-strategy
	  new-script?)
	(q.runstrategy
	  id-node
	  (f.unify
	    (f.get id-node 'strategy-choice)
	    (q.strategy-decision
	      (return-decision-basis
		id-node)
	      new-input))
	  new-input
	  k-goal))
      (f.get id-node 'strategy-execution))
    (if new-script?
	(+ 1 script-match-number)
	script-match-number))
  )



;;;;
;;;; Hypothesis Generation Phase
;;;;
;;;
;;; Currently four strategies exist to choose from in the Hypothesis
;;; Generation phase: Suspension (function suspend-task), explanation
;;; (function explain), analogy (function (analogize) and case-based
;;; reasoning (function episodize). Analogy is not actually implemented.
;;; 

;;;
;;; Function resume-generation-phase is called by function answer-old-questions
;;; when opportunistically picking up on old processing that may now be
;;; completable. See resume-examination-phase.
;;;
;;; If answer is not an XP, then the function returns immediately with a value
;;; of nil (and no side-effect), otherwise it executes the body and returns t.
;;; 
(defun resume-generation-phase
       (answer questions old-goal current-goal question old-answer phase
	&optional automatic?)
  (do-break resume-generation-phase)
  (when (not (isa-p 'xp (list answer)))
;    (with-character-style (*Style*)
      (format
	*aqua-window*
	"~%But input does not help answer question.~%")
;      )
    
    (return-from resume-generation-phase nil))
  ;; This is the case when the system was stumped (could not previously generate
  ;; an answer to the question) and now is presented a possible answer.
  (format
    *aqua-window*
    "~%Found answer for previous baffling question.~%")
  (let* ((hypo-node (f.get phase *co-domain-slot*))
	 (hypo-goal (return-phase-goal hypo-node)))
    ;; Must remove the suspend strategy-choice.
    (f.put-all! *nil* hypo-node 'strategy-choice)
    ;; And the indication that suspension was done.
    (f.put-all! *nil* hypo-node 'strategy-execution)
    (resume-goal-hierarchy hypo-goal)
    ;; |||||| This hack is temporary in oder to get the data. [cox 12jun95]
    ;; Come back to allow the system to reject an inappropriate IMXP.
    (if (isa-p  'xp (list old-answer))
	(let ((current-val
		(get
		  (get
		    (get
		      'mentally-initiates
		      'truth)
		    'retrieval-failure)
		  'xp-type.0)))
	  (if (contains-previous-anomaly-p old-goal)
	      (if (not
		    (f.get
		      (first
			current-val)
		      'anomaly))
		  (setf (get
			  (get
			    (get
			      'mentally-initiates
			      'truth)
			    'retrieval-failure)
			  'xp-type.0)
			(reverse
			  current-val)))
	      (if (f.get
		    (first
		      current-val)
		    'anomaly)
		  (setf (get
			  (get
			    (get
			      'mentally-initiates
			      'truth)
			    'retrieval-failure)
			  'xp-type.0)
			(reverse
			  current-val)))
	      )))
    (cond ((dummy-explanation-p old-answer)
	   (explain question hypo-node old-goal answer automatic?)
	   (remove-indices-to-answered-question question)
	   ;; Place the stumped representation on the outcome of the generation phase.
;;;  	   (break "resume-generation-phase")
	   (f.unify
	     (assert-truth
	       *in*
	       (f.instantiate-frame
		 (outcome-template answer hypo-node))
	       )
	     (first (return-result hypo-node)))
	   ;; Spawn a goal to learn.
	   (spawn-sub-goal
	     *current-character*
	     (make-goal-state
	       'review/learn
	       *current-character*
	       ;; Review the reasoning which led to the faulty conclusions.
	       (processing-trace old-goal))
	     'knowledge-acquisition-goal
	     'three.0
	     old-goal))
	  (t
	   (format
	     *aqua-window*
	     "ERROR in resume-generation-phase.")))    
    t)
  ;; Then need to place the retrieval failure representation for stumped.
  ;; Place question on it to make sure that there exists no item in memory?
  )



;;;
;;; Function contains-previous-anomaly-p acts as a predicate that returns
;;; non-nil whenever the reasoning trace that pursued the old goal detected an
;;; anomaly during the identification phase to initiate the reasoning, rather
;;; than some other reason such as the original input being interesting.
;;; 
(defun contains-previous-anomaly-p (old-goal)
  (isa-p
    'anomaly
    (list
      (f.get
	(first
	  (return-decision-basis
	    (f.get
	      (processing-trace old-goal)
	      'identification)))
	'believed-item)))
  )

;;; 
;;; Function suspend-task is called by the hypothesis-generation and
;;; verification phases of both understanding and planning. Suspension occurs
;;; when the goal cannot be accomplished, so the task to achieve the goal is
;;; suspended until later opportunities arise.
;;; 
(defun suspend-task (dummy goal)
;  (with-character-style (*Style*)
    (format
      *aqua-window*
      (str-concat
	"~%Cannot achieve ~s at this time."
	"~%  Suspend ~s task . . .~%")
      goal
      (frame-type (goal-object goal))
      )
;    )
  
  ;; Remove the goal and all the superordinate goals above
  ;; it in the hierarchy from the goal queue.
  (suspend-goal-hierarchy goal)
  dummy)


;;; |||||
;;; 
(defun analogize (question k-goal)
  (format
    *aqua-window*
    "~%Should not have tried to run analogize.~%")
  question)



;;;
;;; ||||| Eventually make more systematic, using all available info.
;;; See SAIC paper.
;;; 
;;; The basis for the decision is now the value pointed to by the index.
;;; The index currently has no name; it is an indirect reference so has
;;; two indexes really. Fix eventualy. For now the strategy execution
;;; functions can just use the value placed in the processing trace
;;; associated with k-goal, not the index (sic).
;;; 
;;; ||||| This function has become too long. Clean it up.  Should we
;;; really be putting the case on the tmxp slot associated with the
;;; k-goal? This is supposed to be a decision phase not a strategy
;;; execution.  I had to put the k-goal parameter on here just to do
;;; this. Check what happens in other strategy decision routines. 3 May
;;; 93.
;;; 
(defun h.strategy-decision (decision-basis questions hypo-node k-goal)
  (let ((xps (retrieve-xps questions))
	(k-state (f.instantiate-frame
		   knowledge-state))
	(return-val nil))
    (do-break h.strategy-decision "Retrieved XPs: ~s" xps)
    (cond (xps
	   ;; The following is the reason we will generate a hypothesis.
	   (f.unify (make-index
		      questions xps)
		    (f.get
		      k-state
		      'believed-item))
	   (setf return-val 'explanation.0))
	  (t
	   (let ((case (retrieve-case questions)))
	     (cond (case
		    (f.unify (make-index
			       questions case)
			     (f.get
			       k-state
			       'believed-item))
		     (f.put! case
			     (processing-trace k-goal)
			     'cases)
		     (setf return-val 'CBR.0))
		   (t
		    ;; ||||||Perhaps should place stumped (baffled?) representation on d-c-node at this point.
;		    (format *aqua-window*
;		      (str-concat
;			"~%Suspension in h.strategy-decision."
;			"~%Called with ~s and ~s.")
;		      hypo-node
;		      questions)
		    (setf return-val 'suspension.0))))
	   ))
    (f.put! (list k-state)
	    (f.get
	      decision-basis
	      'knowledge)
	    'members)
    return-val)
  )



;;;
;;; ||||| Instead of passing the choice perhaps runstrategy should find
;;; the choice in the current trace of the processing attached to
;;; k-goal.
;;; 
(defun h.runstrategy (hypo-node choice question k-goal)
  (f.unify (f.get hypo-node 'main-result)
	   (f.instantiate-frame outcome))
  (if
    (and
      (not (eq choice 'suspension.0))
	;; Then this is an original question; i.e., one without a previous explanation.
      (dummy-explanation-p
	  (first
	    (f.get question *explanations-slot*))))
    (set-self-gen-answers
      *Current-Result-Record*))
  (case choice
    (explanation.0
;      (f.put! (list (explain question hypo-node k-goal))
;	      (f.get hypo-node 'main-result)
;	      'members)
      (f.unify
	(explain question hypo-node k-goal)
	(first (return-result hypo-node)))
;;;       (break "Check hypo node main result")
      'explain.0)
    (analogy.0
      (f.unify
	(analogize question k-goal)
	(first (return-result hypo-node)))
      'analogize.0)
    (CBR.0
      (f.unify
	(episodize question k-goal)
	(first (return-result hypo-node)))
      'episodize.0)
    (suspension.0
      (suspend-task question k-goal)
      'suspend-task.0)
    ( t (format
	  *aqua-window*
	  "ERROR: unknown h-strategy - ~s." choice))))



(defun generate-hypothesis (k-goal hypo-node)
  (let ((questions (goal-state k-goal)))
    ;; ||||| The following code is currently dependent on the order of parameters.
    (f.unify (h.runstrategy
	       hypo-node
	       (f.unify (f.get hypo-node 'strategy-choice)
			(h.strategy-decision
			  (return-decision-basis
			    hypo-node)
			  questions
			  hypo-node
			  k-goal))
	       questions
	       k-goal)
	     (f.get hypo-node 'strategy-execution))
    ;; ||||| This is a case for use of f.bound-put! when finished.
    ;; Also, all strategy choices are guaranteed currently to return an XP.
    ;; This may not always be so.
    (if (not
	  (equal 'suspension.0
		 (f.get hypo-node 'strategy-choice)))
	(let ((resulting-xp
		(first (return-result hypo-node))))
	  (cond (resulting-xp
		 (f.put-all!
		   resulting-xp
		   (processing-trace k-goal)
		   'main-xp))
		(t
		 ;; ||||||If I get to the following, shouldn't I be suspending the task? 
		 (break "ERROR: No xp results"))))))
  )



;;;;
;;;; Verification Phase
;;;;
;;;
;;; Currently three strategies exist to choose from in the Verification
;;; phase: Suspension (function suspend-task), comparison (function
;;; compare), and devising a test (function devise-test). Devise-test is
;;; not actually implemented.
;;; 



;;;
;;; Function resume-examination-phase is called by function
;;; answer-old-questions when opportunistically picking up on old processing
;;; that may now be completable. See resume-generation-phase.
;;;
;;; The function is not guaranteed to return t any longer. If the answer does
;;; not match to the question and the answer is not an alternative xp, then the
;;; function prints a message that the input does not help answer the old
;;; question and it returns nil. This forces function answer-old-questions to
;;; also return nil, which causes function do-understand to call reason-about.
;;; 
(defun resume-examination-phase
       (answer questions old-goal question old-answer phase)
  (do-break resume-examination-phase)
  (if (and (isa-p 'xp (list answer))
	   (equal (frame-type (f.get answer *explains-node*))
		  (frame-type question)))
      ;; ||||| Probably should really do an apply-xp here. This would generate hvqs
      ;; (and if apply-xp is fixed) make sure that the pre-xp-nodes are in.
      (let ((merged-node (f.unify (f.get answer *explains-node*)
				  question t nil t))
	    ;; |||||| Cannot perform the following because there is already 
	    ;; a different explanation on the slot rather than a dummy.
;;; 	    (merged-node (f.unify (f.get question *explanations-slot*) answer))
	    )
	(f.put! (if old-answer			;||||||Possibly will always be an old answer, even if dummy.
		    (list answer old-answer)
		    (list answer))
		merged-node
		*explanations-slot*)))
  ;; ||||| Eventually we should make sure first that there was no test of the
  ;; hypothesis being done first. We must look at the reason for the original suspension
  ;; to see that there was no reason yet to decide on a test method. Now we can do
  ;; the compare
  (cond ((or (try-matching2 answer questions nil)	; Try matching strictly.
	     (try-matching2 answer questions t))	; Now try lazy matching.
	 (if (and (isa-p  'xp (list old-answer))
		  (contains-previous-anomaly-p old-goal))
	     ;; The current-phase is the test relation, thus the co-domain is the verify d-c-node.
	     (cond
	       ((isa-p 'xp (list answer))	; Added [cox 25def95]
		(let* ((verify-node (f.get phase *co-domain-slot*))
		       (verify-goal (return-phase-goal verify-node)))
		  ;; Must remove the suspend strategy-choice.
		  (f.put! *nil* verify-node 'strategy-choice)
		  ;; And the indication that suspension was done.
		  (f.put! *nil* verify-node 'strategy-execution)
		  (resume-goal-hierarchy verify-goal)
		  (verify verify-goal
			  verify-node)
		  (remove-indices-to-answered-question question)
		  (spawn-sub-goal
		    *current-character*
		    (make-goal-state
		      'review/learn
		      *current-character*
		      ;; Review the reasoning which led to the faulty conclusions.
		      (processing-trace old-goal))
		    'knowledge-acquisition-goal
		    'ten.0
		    old-goal)
		  t
		  ))
	       (t
		(format
		  *aqua-window*
		  "~%~%New input does not help answer old question.~%~%")
		nil))))
	(t
	 ;; |||||| This hack is temporary in oder to get the data. [cox 14jun95]
	 ;; Come back to allow the system to reject an inappropriate IMXP.
	 (if (isa-p  'xp (list old-answer))
	     (let ((current-val			; Current value of the IMXPs stored at this location.
		     (get
		       (get
			 (get
			   'mentally-initiates
			   'not-equal-relation)
			 'expectation-failure)
		       'xp-type.0)))
	       (if (contains-previous-anomaly-p old-goal)
		   (if (not
			 (f.get
			   (first
			     current-val)
			   'anomaly))
		       (setf (get
			       (get
				 (get
				   'mentally-initiates
				   'not-equal-relation)
				 'expectation-failure)
			       'xp-type.0)
			     (reverse
			       current-val)))
		   (if (f.get
			 (first
			   current-val)
			 'anomaly)
		       (setf (get
			       (get
				 (get
				   'mentally-initiates
				   'not-equal-relation)
				 'expectation-failure)
			       'xp-type.0)
			     (reverse
			       current-val)))
		   )))
	 (cond
	   ((isa-p 'xp (list answer))		; Added [cox 25def95]
	    (let* ((verify-node (f.get phase *co-domain-slot*))
		   (verify-goal (return-phase-goal verify-node)))
	      ;; Must remove the suspend strategy-choice.
	      (f.put! *nil* verify-node 'strategy-choice)
	      ;; And the indication that suspension was done.
	      (f.put! *nil* verify-node 'strategy-execution)
	      (resume-goal-hierarchy verify-goal)
	      (verify verify-goal
		      verify-node)
	      (big-hack old-answer answer)
	      (remove-indices-to-answered-question question)
	      t
	      ))
	   (t
;;; 	 (break "Odd that ~s is not an xp." answer)
	    (format
	      *aqua-window*
	      "~%~%New input does not help answer old question.~%~%")
	    nil))))
  )



;;;
;;; This routine is the one that (when implemented) will link understanding
;;; with problem-solving; that is, in order to verify a hypothetical
;;; interpretation of some input, a reasoner may have to devise a test and then
;;; plan the test.
;;; 
(defun devise-test (hypothesis k-goal)
  (format
    *aqua-window*
    "~%Execution of devise-test should not have occurred.~%")
  )



;;;
;;; ||||| NOTE THAT BECAUSE THE FUNCTION COMPARE (or is it big-hack now?)
;;; PERFORMS A F.UNIFY WITH THE HYPOTHESIS PARAMETER, IT MAY INADVERTENTLY
;;; CHANGE WHAT THE FRAME VARIABLE POINTED-TO BY IT CONTAINS. TO CHANGE THIS IS
;;; NOT AS EASY AS THE CHANGES WE DID TO FUNCTION REVIEW. THE REASON IS THAT
;;; COMPARE IS ALREADY RETURNING A VALUE TO BE SET TO SOMETHING BY THE CALLING
;;; FUNCTION. ALSO WE CANNOT JUST USE MULTI-VALUE RETURN VALUES SINCE THE
;;; PARAMETER IS OBTAINED FROM THE PARAMETER OF THE CALLING FUNCTION.  NEED TO
;;; FIX THIS ASAP.
;;;
;;; Since the learning does not really depend on this hack, I do not think that
;;; it is a real fudge about which to be worried. [30oct94]
;;; 
(defun big-hack (alternative hypothesis)
 ;; Big HACKs. This is the unification of the sniff with the explanation of the bark.
 ;; Since the story does not explicitly say that the detection is the sniff, we need
 ;; infer it by backtracking. The system then needs to notice that the detection must
 ;; be the one in the bust scene.
  (let ((d-method (f.instantiate-frame tip-off)))
;	   (format *aqua-window*
;		   "~%Unify goal-scene of tip-off with goal-scene of bark.")
;	   (f.unify (f.get d-method 'goal-scene)
;		  (f.chase-path alternative 'consequent 'goal-scene))
;	   (format *aqua-window*
;		   "~%Unify bark from because statement with previous bark.")
;	   (f.unify (f.get alternative 'consequent)
;		  (fourth (get-model *World-Model*)))
;	   (format *aqua-window*
;		   "~%Add sniff as instrumental scene of detection-method.")
    ;; The following puts the sniff on the instrumental-scene slot of the detection method.
    (f.put (return-last-element (get-model *World-Model*))
	   d-method
	   'instrumental-scene)
;	   (format
;	     *aqua-window*
;	     "~%F.unify tip-off with detection-method of detection scene of bust.")
    (f.unify
      d-method
      (f.get
	(get-scene 1
		   (second (get-model *World-Model*))	; Returns a case.
		   )
	'method))
;	     (f.unify (f.get alternative 'antecedent)
;		    (return-last-element (get-model *World-Model*)))
    (let ((question (f.get hypothesis *explains-node*)))
;      (with-character-style (*Style*)
	(format *aqua-window*
		"~%~%Found a better explanation for action of ~s.~%"
		(f.get question *domain-slot*))
	(format *aqua-window*
		"~%The explanation links together more of the story.~%"
;;; 		(f.get question *domain-slot*)
		)
;	)
      
      (f.put! (list alternative) question *explanations-slot*)))
    )



;;;
;;; Function compare in this form is truer to the way in which it was presented
;;; in the MSL-91 paper (see page 224). Think of the evidence as node A and the
;;; hypothesis as node E. The function returns either a successful prediction
;;; node or an expectation failure node.
;;;
;;; Return the conclusion: Successful-Prediction or Expectation-Failure.
;;; Side-effect: if expectation-failure, then spawn a goal that initiates
;;; learning.
;;; 
(defun compare (evidence hypothesis k-goal verify-node)
  (do-break compare)
  (format
    *aqua-window*
    (str-concat
      "~%Compare strategy applied to "
      "evidence ~s for hypothesis ~s~%")
    evidence
    hypothesis)
  (cond ((can-unify-p evidence hypothesis)
	 (remove-achieved-goal k-goal)		; Successful hypothesis, so remove knowledge goal.
	 (assert-truth
	   *in*
	   (f.instantiate-frame
	     `(mentally-initiates
		(results- (,*value-facet*
			   ,verify-node))
		(,*domain-slot*
		 (,*value-facet*
		  (equal-relation
		    (,*domain-slot*
		     (,*value-facet* ,evidence))
		    (,*co-domain-slot*
		     (,*value-facet* ,hypothesis)))))
		(,*co-domain-slot*
		 (,*value-facet* 
		  (successful-prediction
		    (initiates- (,*value-facet* =domain))
		    (expected-outcome
		      (,*value-facet* ,hypothesis))
		    (actual-outcome
		      (,*value-facet* ,evidence)))))
		))))
	(t					; Post immediate goal to review process.
	 (spawn-sub-goal
	   *current-character*
	   (make-goal-state
	     'review/learn
	     *current-character*
	     ;; Review the reasoning which led to the faulty conclusions.
	     (f.get hypothesis 'mxp))
	   'knowledge-acquisition-goal
	   'ten.0
	   k-goal)
	 (assert-truth
	   *in*
	   (f.instantiate-frame
	     `(mentally-initiates
		(results- (,*value-facet*
			   ,verify-node))
		(,*domain-slot*
		 (,*value-facet*
		  (not-equal-relation
		    (,*domain-slot*
		     (,*value-facet* ,evidence))
		    (,*co-domain-slot*
		     (,*value-facet* ,hypothesis)))))
		(,*co-domain-slot*
		 (,*value-facet* 
		  (expectation-failure
		    (initiates- (,*value-facet* =domain))
		    (expected-outcome
		      (,*value-facet* ,hypothesis))
		    (actual-outcome
		      (,*value-facet* ,evidence)))))
		)))))
  )



;;; 
;;; Function v.strategy-decision looks to see if there are any
;;; compatible concepts that could verify the hypothesis. If so it tries
;;; to compare these with the hypothesis. Otherwise it suspends the
;;; verification till a later time when more output may be available.
;;;
;;; ||||| Currently there is never a decision made to try to test a
;;; hypothesis. We usually suspend. Compares are then done when a later
;;; input can answer the question.  The hypothesis is then checked for
;;; agreement.
;;; 
(defun v.strategy-decision (decision-basis hypothesis verify-node k-goal)
  (let ((relevant-input (find-relevant-input
			  hypothesis))
	(k-state (f.instantiate-frame
		   knowledge-state))
	(return-val nil))
    (cond (relevant-input
	   (format
	     *aqua-window*
	     (str-concat
	       "~%Found relevant input "
	       "for verifying hypothesis: ~s~%")
	     relevant-input)
	   (format
	     *aqua-window*
	       "~%Comparison strategy selected ~%")
	   (do-break v.strategy-decision)
	   (f.unify
	     relevant-input
	     (f.get
	       k-state
	       'believed-item))
	   (setf return-val 'comparison.0))
	  ((test-applicable-p hypothesis)
	   (setf return-val 'devise-test.0))
	  (t
	   (setf return-val 'suspension.0)))
    (f.unify k-state
	     (or
	       (first
		 (f.chase-path
		   decision-basis
		   'knowledge
		   'members))
	       (first decision-basis)))
    return-val)
  )



;;;
;;; Function find-relevant-input takes as input a hypothetical explanation, and
;;; returns as output the first item it finds in the world model that is of the
;;; same type as the object (explains node) of the hypothetical explanation
;;; input to the function. The explains node represents what the explanation is
;;; explaining.
;;;
;;; This function is called by function v.strategy-decision. The purpose is to
;;; retrieve a relevant piece of new input from the story that corroborates an
;;; old explanation.
;;; 
(defun find-relevant-input (hypothesis)
  (let* ((hypo-explains (f.get hypothesis
			       *explains-node*))
	 (hypo-type (frame-type
		      hypo-explains)))
    (do-break find-relevant-input)
    (if (not (null hypo-explains))
	(some
	  #'(lambda (each-world-concept)
	      (if
		(equal
		  hypo-type
		  (frame-type
		    (f.get each-world-concept
			   *explains-node*)))
		each-world-concept))
;;; 	  (get-model *World-Model*)	  
	  (list (first (get-model *World-Model*)))	; |||||| Temporary
	  )))
  )


;;;
;;; Predicate test-applicable-p takes a hypothesis and returns whether or not a
;;; test can be devised (or is it worth for the system to try an devise a test)
;;; for the hypothesis.
;;;
;;; ||||| Currently NOT implemented.
;;; 
(defun test-applicable-p (hypothesis)
  nil)



(defun v.runstrategy (verify-node choice hypothesis k-goal)
  (f.unify (f.get verify-node 'main-result)
	 (f.instantiate-frame outcome))
  (case choice
    (comparison.0
      (f.put! (list (compare
		      (f.get
			(first
			  (return-decision-basis
			    verify-node))
			'believed-item)
		      hypothesis
		      k-goal
		      verify-node))
	      (f.get verify-node 'main-result)
	      'members)
      'compare.0)
    (devise-test.0
      (f.put! (list (devise-test hypothesis k-goal))
	      (f.get verify-node 'main-result)
	      'members)
      'devise-test.0)
    (suspension.0
      (suspend-task hypothesis k-goal)
      'suspend-task.0)
    ( t (format
	  *aqua-window*
	  "ERROR: unknown v-strategy - ~s." choice))))



;;;
;;; ||||| See comment below for why the callers of f.put! is important.
;;;
(defun verify (k-goal verify-node)
 ;; The following calling sequence works because f.put will return the filler
 ;; passed to it. The filler is the value returned by v.strategy-decision.
 ;; ||||| NOTE that by using f.put! strategy decision will be assigned the value returned by
 ;; v.strategy-decision (a string). However the slots in the d-c-node which are bound to the
 ;; strategy-choice slot (the co-domain of link2 and the domain of link3) will not be changed.
 ;; This is why I need to finish f.bound-put!. Same event happens in function generate-hypothesis.
 ;;
 ;; I commented out the code the remarks above refer to and use f.unify until a more general
 ;; function like f.boutd-put! is finished.
  
;    (v.runstrategy (f.put! verify-node 
;			   'strategy-choice
;			   *value-facet*
;			   (v.strategy-decision hypothesis))
;		   hypothesis)
  (let ((hypothesis (goal-state k-goal)))
    (do-break verify)
    ;; ||||| The following code is currently dependent on the order of parameters.
    (f.unify
      (v.runstrategy
	verify-node
	(f.unify
	  (f.get
	    verify-node
	    'strategy-choice)
	  (v.strategy-decision
	    (return-decision-basis verify-node)
	    hypothesis
	    verify-node
	    k-goal))
	hypothesis
	k-goal)
      (f.get verify-node 'strategy-execution)))
  )





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;           MAIN CONTROL FUNCTIONS 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;
;;; Function boot-understanding is the routine to handle
;;; understanding goals in the dispatch call of function
;;; reason-about. It spawns a goal to identify any questions
;;; concerning the desired goal-state of the k-goal.
;;; 
(defun boot-understanding (k-goal)
  ;; Post goal to identify the question.
  (spawn-sub-goal
    *current-character*
    (make-goal-state
      'id
      *current-character*
      (goal-state k-goal))
    'knowledge-acquisition-goal
    'seven.0
    k-goal)
  )



;;;
;;; Function dispatch-knowledge-goal calls particular reasoning
;;; functions depending on the type of goal-object in the knowledge
;;; goal.  It returns a phase identifier that corresponds to the
;;; goal-object type.  The counterpart function in the
;;; problem-solving process is dispatch-world-goal.
;;;
;;; The function returns two values. The main (first) value is the phase
;;; identifier (goal type?) of the routine dispatched. The second value is the
;;; (possibly updated) script match number.
;;; 
(defun dispatch-knowledge-goal (k-goal d-c-node script-match-number)
  (values
    (case (frame-type (goal-object k-goal))
      (understands
	(boot-understanding k-goal)
	'understands)
      (id
	(setf script-match-number
	      (identify-question
		k-goal
		d-c-node
		script-match-number))
	'identification)
      (generate
	(generate-hypothesis k-goal d-c-node)
	'generation)
      (test
	(verify k-goal d-c-node)
	'examination)
      (review/learn
	(learn k-goal d-c-node)
	'review)
      (t
	(format *aqua-window*
		"ERROR: unknown goal type in function reason-about.")
	nil))
    script-match-number)
  )

;;; 
;;; Function reason-about looks at the mxp slot of the knowledge goal
;;; (this is returned by function processing-trace)
;;; and determines where in the reasoning chain the sequence is. It then
;;; calls dispatches the proper routine to pick up the reasoning by
;;; calling dispatch-goal. It also passes the proper parameter info
;;; stored in the mxp.
;;;
;;; Parameter k-goal is the knowledge-goal.
;;;
;;; NOTE that reason-about is the primary dispatching function for
;;; calling the reasoning processes. It also manages the Trace-Meta-XP
;;; that records such reasoning. However the function
;;; answer-old-questions also performs this role when resuming suspended
;;; reasoning tasks.
;;;
;;; Returns script-match-number. It is (possibly) updated when passed
;;; through dispatch-knowledge-goal.
;;; 
(defun reason-about (k-goal script-match-number)
  (let ((d-c-node (gen-d-c-node k-goal)) 
	(phase nil))
    (do-break reason-about)
    (multiple-value-setq
      (phase script-match-number)
      (dispatch-knowledge-goal
	k-goal
	d-c-node
	script-match-number))
    (if (and phase
	     (not (equal phase 'understands))
	     (not (equal phase 'wants)))
	(let ((which-tmxp
		(if (equal phase 'review)
		    (goal-state k-goal)
		    (processing-trace k-goal))))
	  ;; NOTE that because D-C-Node is passed to dispatch-goal
	  ;; and is possibly unified with other frames it might be
	  ;; no longer point to the correct node.
	  (f.unify
	    d-c-node 
	    (f.get which-tmxp phase))
	  (f.put! (f.make-relation
		    which-tmxp phase)
		  which-tmxp
		  'current-phase)))
    script-match-number)
  )


;;; 
;;; Function do-understand processes a knowledge goal by checking to see if the
;;; current input (new input) possibly addresses a previously suspended
;;; question. If so it attempts to answer the question with the input, else it
;;; processes the input normally via the reason-about function.
;;;
;;; The function returns the script-match-number. It is (possibly) updated if
;;; passed through reason-about.
;;; 
(defun do-understand (k-goal script-match-number &optional automatic?)
  (let* ((new-input				; Note that this is not necessarily new input.
	   (goal-state k-goal))
	 (old-questions
	   (and
	     (is-goal-object-p			; Goal is to understand a new input.
	       'understands
	       k-goal)
	     (potential-answer-p new-input)
	     )))
    (do-break do-understand)
    (notice new-input				; Notice is currently used to unify a 
	    k-goal				; posted goal with its equivalent.
	    (get-model *World-Model*))
    (cond ((and
	     old-questions			; If old questions exist
	     (not				; and not a reassertion of the action being questioned.
	       (reasserts-act-being-questioned-p
		 new-input
		 (first old-questions)))
	     (print-remembers-old-question	; {this line guaranteed true}
	       old-questions)			; {in order to print        }
	     (cond
	       ((or				; and either
		  automatic?			; on automatic run or
		  (y-or-n-p "Answer this?"))	; user wants these questions answered
		t)
	       (t
		(if (y-or-n-p			; Prompt the user.
		      "Try to purge old question?")
		    (if (retrieve-memory
			  'question-type.0
			  new-input)
			(do-index		; This will work for simply indexed questions.
			  nil
			  'question-type.0
			  new-input)))
		nil))
	     (answer-old-questions		; then answer them
	       new-input old-questions
	       (get-old-goal old-questions)
	       k-goal automatic?))
	   script-match-number)			; Return script match number unchanged.
	  (t
	   (reason-about			; else pursue the reasoning goal, returning
	     k-goal				; the script-match-number which may be changed by 
	     script-match-number))))		; function reason-about.
  )



(defun print-remembers-old-question (question)
  "Simple feedback to user about reminding."
;  (with-character-style (*Style*)
    (format
      *aqua-window*
      "~%~%Input triggers reminding of old question:~%  ~s~%"
      question)
;    )
  
  t
  )
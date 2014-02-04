;;; -*- Mode: LISP; Syntax: Common-lisp; Package: Representations; Base: 10 -*-

(in-package :reps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	      Meta-AQUA Background Knowledge Represented as Frames
;;;;
;;;;	    Copyright (C) 1996   Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;                     File: rep_meta-xps.lisp
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
;;;; Changed XP-DISBLE-ACT-PRECONDITION. [mcox 4nov05]
;;;; 



;;; Trace-Meta-XPs will not record all computations of a system. Only the explicit
;;; "conscious" processes are so noted. Many other functions are not captured.
;;; As previously mentioned, the implicit inferences from concept instantiations
;;; need not be represented. The classic example is to infer eating when reading
;;; a brief story (that mentions only ordering and paying) about eating at a
;;; restaurant by script instantiation.
;;; 
;;; Will predicate logic be needed in the representation to capture the 
;;; computations? CYC found it necessary.
;;; 
;;; |||||
;;; Will I need to represent properties with collections and members? 
;;; What are the semantics of these?
;;; 
;;; NEED TO CONSIDER THE DIAGRAM AND COMMENTS FROM BLUE NOTEBOOK 22FEB91 !!!
;;; These comments show many more representational features for a META-XP. 
;;; Where are the limits of an META-XP? What is the functional role of these 
;;; structures? Do we need to divide the functions, instead of having such a 
;;; large and global structure?
;;; 
;;; Need to declaratively represent the major functions, eg. explain.
;;; Each function frame should have precondition, main-result (return value),
;;; parameter, and side-effect slots. The function will still be on the 
;;; function property of the symbol.
;;; 
;;; Do we need to have specialized d-c-nodes to represent the differences 
;;; between hypothesis generation, verification, learning, etc.?
;;;




; |||||
; If I define ENTITY formally will this present side-effect
; for the functions I have defined that traverse or otherwise
; access the isa hierarchy? Actually it is defined without slots
; in internals.lisp. Then it is given an isa property of nil 
; in function init-isa-hierarchy in isa.lisp.
;
;(define-frame ENTITY
;   (truth             [truth-frame])
;   (incompatible-with (list. =opposite-of))
;   (opposite-of       [(opposite-of =self)])
;   )


(define-frame PROCESS
    (isa            (value (entity)))
    (at-location    (value (relation)))	; ||||||???? 19oct94
    )



(define-frame PHYSICAL-PROCESS
    (isa            (value (process)))
    (at-location    (value (physical-location)))
    )


(define-frame MENTAL-PROCESS
    (isa            (value (process)))
    )


(define-frame RELATION
    (isa            (value (state)))
    (domain         (value (entity)))
    (co-domain      (value (entity)))
)


;;; |||||Should state be a relation or vice versa?
(define-frame STATE
    (isa                (value (entity)))
    (co-domain          (value (state-value)))
    )


(define-frame STATE-VALUE
    (isa                (value (entity)))
)


(define-relation PROCESS-RELATION
    (isa            (value (relation)))
    (domain         (value (process)))
)


(define-relation SPATIAL-RELATION
    (isa            (value (relation)))
)


(define-relation EVENT-RELATION
    (isa            (value (process-relation)))
    (domain         (value (event)))
)


(define-relation MOP-RELATION
    (isa            (value (event-relation)))
    (domain         (value (mop)))
)


;;; These are STATEs in that they can be RESULTS of MOPs
;;; (e.g., RECRUIT RESULTS in ACTOR), etc.
;;; 
(define-relation VOLITIONAL-ROLE-RELATION
    (isa            (value (mop-relation state)))
    (domain         (value (mop)))
    (co-domain      (value (volitional-agent)))
    ) ;; WHO IS THE ?slot OF ?co-domain


;;;
;;; ||||| This definition may be superfluous.
;;; 
(define-relation TRUTH-RELATION
    (isa            (value (relation)))
    (domain         (value (entity)))
    (co-domain      (value (truth-value)))
    ) 


(define-frame MENTAL-STATE
    (isa            (value (state)))
    (domain         (value (volitional-agent)))  ;;Only VOLITIONAL-AGENTs think in this world
;    (needs-explanation? (value true.0))
    )


(define-frame CAUSAL-RELATION
  (isa                (value (xp relation)))
  (domain             (value (entity)))
  (co-domain          (value (entity)))
  (pre-xp-nodes       (value =co-domain))
  (xp-asserted-nodes  (value =domain))
  (internal-xp-nodes  )
  (links              )
    )


;;; A why question really. 
(define-frame QUESTION
  (isa                (value (entity)))
  (explanations       (value ((xp		; A list of explanations.
				(status (value question.0))))))
  
    )



;;; ||||| XP has alot commented out. Why?  Is it because the explains node
;;; needs to be unified with one of the pre-xp-nodes and we do not know why
;;; until a particular one is instantiated?
;;; 
(define-frame XP
    ;; Essentially an XP isa relation between the pre-xp-nodes and the explains node.
    ;; ||||| Should not the above be xp-asserted nodes and the explains node?
    (isa                (value (relation)))
;;  (pre-xp-nodes       (value (entity)))              ;; A list of ENTITYs.
    (explains           (value (question)))            ;; One of the PRE-XP-NODES.
;;  (xp-asserted-nodes  (value (entity)))              ;; A list of ENTITYs.
;;  (internal-xp-nodes  (value (entity)))              ;; A list of ENTITYs.
;;  (links              (value (xp)))                  ;; A list of CAUSAL-RELATIONs or XPs.
;    (slot               (value (explanations))) ;; XPs are stored in the EXPLANATION slot.
)


;;;
;;; Most basic causal patterns that link states and/or actions. For example,
;;; to say that "John's arm is sore, because the pitcher hit him with the
;;; ball" is a basic causal statement action X -> state Y ; that is,
;;; (results-in Y X), where Y, (sore (arm-of John)), is the consequent, and X,
;;; (hit (actor pitcher) (object (arm-of John)) (instrument ball)), is the
;;; antecedent. See results-in definition below.
;;; 
(define-frame PRIMITIVE-XP
	      (isa (value (xp)))
  )


;;;
;;; Explains how and why reasoning works (fails or succeeds)
;;; 
(define-frame META-XP
  (isa (value (xp)))
  )


;;;
;;; Explains why volitional agents decide to do particular actions.
;;; 
(define-frame VOLITIONAL-XP
    (isa                (value (xp)))
    (explains           (value (volitional-role-relation)))
)


(define-frame SHALLOW-VOLITIONAL-XP
		(isa (value  (volitional-xp)))
		)



;;;
;;;
;;; ACTION OF SOME AGENT CAUSES ACTOR TO PERFORM SOME ACTION.
;;; 
;;; A backlink from an action to an earlier action that resulted
;;; in its preconditions. This is actually a specialization of
;;; BECAUSE. If one was to say "Action1 because State1"
;;; then we have a normal enables relation. If "State1 because
;;; Action1" then it is a normal results relation being expressed.
;;; ||||| Should implement this type of inference in the future.
;;;
;;;
;;; Changed to CAUSE and added the synonym slots ante conseq. [cox 9feb95]
;;; Because of problems, changes antecedent and consequent to be the synonyms.
;;; Note that the reason synonyms are used is that Tale-Spin uses the short
;;; names whereas Meta-AQUA uses the longer. [cox 10feb95]
;;; 
(define-frame CAUSE
	      (isa (value (primitive-xp)))
;;;   (domain (value (mop)))
;;;   (co-domain (value (mop)))
;;;   (antecedent (value =co-domain))
;;;   (consequent (value =domain))
  (actor (value (volitional-agent)))
  (ante
    (value (mop
	     (actor (value
 		      (volitional-agent)))
	     (main-result (value =main-precondition)))))
  (conseq
    (value (mop
	     (actor
	       (value =actor)
	       (relation =role))
	     (main-precondition (value =main-precondition)))
	   ))
  (antecedent (value =ante))
  (consequent (value =conseq))
  (role (value (actor
		 (domain (value =conseq))
		 (co-domain (value =actor)))))
  (main-precondition (value (state
;;;  			      (domain (value =actor))
			      )))
  (explains (value =role))
  (pre-xp-nodes (value (=conseq =actor =role)))
  (internal-nodes (value (=main-precondition)))
  (xp-asserted-nodes (value (=ante)))
  (link1 (value (results
		  (domain (value =ante))
		  (co-domain (value =main-precondition)))))
  (link2 (value (enables
		  (domain (value =main-precondition))
		  (co-domain (value =conseq)))))
  (links (value (=link1 =link2)))
  )



;;;
;;; John does consequent action because of antecedent action he performed
;;; earlier. This is the XP to represent "The dog barked because the dog
;;; detected the marijuana."
;;;
(define-frame SELF-CAUSE
	      (isa (value (primitive-xp)))
;;;   (domain (value (mop)))
;;;   (co-domain (value (mop)))
;;;   (antecedent (value =co-domain))
;;;   (consequent (value =domain))
  (actor (value (volitional-agent)))
  (ante
    (value (mop
	     (actor (value
		      =actor
			   ))
	     (main-result (value =main-precondition)))))
  (conseq
    (value (mop
	     (actor
	       (value =actor)
	       (relation =role))
	     (main-precondition (value =main-precondition)))
	   ))
  (antecedent (value =ante))
  (consequent (value =conseq))
  (role (value (actor
		 (domain (value =conseq))
		 (co-domain (value =actor)))))
  (main-precondition (value (state
 			      (domain (value =actor))
			      )))
  (explains (value =role))
  (pre-xp-nodes (value (=conseq =actor =role)))
  (internal-nodes (value (=main-precondition)))
  (xp-asserted-nodes (value (=ante)))
  (link1 (value (results
		  (domain (value =ante))
		  (co-domain (value =main-precondition)))))
  (link2 (value (enables
		  (domain (value =main-precondition))
		  (co-domain (value =conseq)))))
  (links (value (=link1 =link2)))
  )


;;;
;;;
;;; ACTION OF ACTOR CAUSES STATE.
;;;
;;; Answers "How did state (conseq) come about?" or "Why does this state
;;; exist?" Answer: "State conseq came about because it resulted from action
;;; ante." But what would be the xp for answering "What state, Y, did action X
;;; result in?" Is this the same xp? No, the answer is just a filler, not an
;;; xp.
;;;
;;; NOTE that unlike the other primitive xps here, the explains node is a state
;;; (the consequent), rather than the role of an action (i.e., why did actor
;;; perform some action).
;;;
;;; |||||| Especially since there exists the results causal-relation below, I
;;; suppose that there should be some reordering of the ontology. Are XPs
;;; really mental-causal-relations? Or are structures like "results" below
;;; really a kind of xp? Also, we still need the variation on cause above in
;;; which the action of one actor determined the action of another, rather than
;;; one's previous action determining one's own action. [cox 12feb95]
;;; 
(define-frame RESULTS-IN
	      (isa (value (primitive-xp)))
  (actor (value (volitional-agent)))
  (ante (value (mop
		 (actor (value =actor))
		 (main-result (value =conseq)))))
  (conseq (value (state
		   (domain (value =actor)))))
  (antecedent (value =ante))
  (consequent (value =conseq))
  (explains (value =conseq))
  (pre-xp-nodes (value (=conseq =actor)))
  (internal-nodes (value ))
  (xp-asserted-nodes (value (=ante)))
  (link1 (value (results
		  (domain (value =ante))
		  (co-domain (value =conseq)))))
  (links (value (=link1)))
  )



;;;
;;;
;;; STATE CAUSES ACTION BY ACTOR.
;;;
;;; For example, "Because Mary was dead, John committed suicide." What about
;;; "Because John was depressed, John committed suicide"?
;;;
(define-frame FORCED-BY
	      (isa (value (primitive-xp)))
  (actor (value (volitional-agent)))
  (ante (value (state)))
  (conseq (value (mop
		   (actor (value =actor))
		   (main-result (value =conseq)))))
  (antecedent (value =ante))
  (consequent (value =conseq))
  (role (value (actor
		 (domain (value =conseq))
		 (co-domain (value =actor)))))
  (explains (value =role))
  (pre-xp-nodes (value (=conseq =actor =role)))
  (internal-nodes (value nil.0))
  (xp-asserted-nodes (value (=ante)))
  (link1 (value (results
		  (domain (value =ante))
		  (co-domain (value =conseq)))))
  (links (value (=link1)))
  )



;;;
;;; This is straight from Ashwin's definition (see file
;;; ~/Aqua/Concepts/rep_xps.lisp), modified to remove the ".list" markers and
;;; replace the "[]" macros with explicit "value slot" identifiers.  I also
;;; made it isa volitional-xp, rather than xp-normal-plan-choice (which, for
;;; Ashwin, was isa volitional-xp). [12feb95]
;;; 
;;
;;-----------------------------------------------------------------------------
;; People participate in actions that result in states that they want.
;;-----------------------------------------------------------------------------
;; Given a person doing something that results in a state, assert that he wants
;; that state, via a rational plan choice.
;;
;; This is the representation of the PAM chain.  People have a goal to achieve
;; a state, so they consider their goal, decide to do an action that they know
;; will result in that state, and do it.  To expand this further, we must
;; explain how they knew that the action would result in that state, and how
;; they selected this action over the others.
;;
;;; ||||||Danger - this definition has transitive variable bindings. [24feb95]
;;;
;;; ||||||This xp had main-results bound to both an xp-asserted-node and a
;;; pre-xp-node. Is this ever possible? [cox 25feb95]
;;; 
(define-frame XP-GOAL-OF-OUTCOME->ACTOR
	      (isa (value (volitional-xp)
;;;                       (xp-normal-plan-choice)
			  ))
  ;; NOTE that this is not the actor of the explanation. Used to be agent I think.
  (actor (value (volitional-agent)))
  (ante (value (state				; Was relation [cox 25feb95]
		 ;; Commented out because of is-anomalous intolerance for cycles. [cox 26feb95]
;;; 		 (main-result- (value =conseq))
		 )))
  (conseq (value (mop
		   (actor (value =actor)
			  (relation =role))
		   (main-result (value =ante)
				(relation =main-result)))))
  (antecedent (value =ante))
  (consequent (value =conseq))
  (action
    (value =conseq))   ;; Should be any RESULT, not just the MAIN-RESULT.
  (role
    (value (actor
	     (domain (value =conseq))
	     (co-domain (value =actor)))))
  (good-state
    (value =ante))
  (main-result
    (value (main-result
	     (domain (value =conseq))
	     (co-domain (value =ante)))))
  (goal
    (value (achievement-goal
	     (goal-actor (value =actor))
	     (goal-object (value =ante)))))
  (plan-choice
    (value (plan-selection
	     (actor (value =actor))
	     (plan (value =conseq))
	     (goal (value =goal))
	     (role (value =role)))))
  (pre-xp-nodes       (value (=actor =conseq =role
;;; 				     =ante ; [cox ~25feb95]
;;; 				     =main-result ; [cox ~25feb95]
				     )))
  (explains           (value =role))
  (xp-asserted-nodes  (value (=main-result =goal
					   =ante	; Added [cox 25feb95]
					   )))
  (internal-xp-nodes  (value (=plan-choice)))
  (links (value (=link1)))
  (link1
    (value (mentally-results
	     (domain    (value =plan-choice))
	     (co-domain (value =role)))))
  )



(define-frame XP-NORMAL-PLAN-CHOICE
		(isa (value  (volitional-xp)))
		)



;;-------------------------------------------------------------------------------------
;;; Typical or thematic action for animate-objects, not just volitional-agents.
;;-------------------------------------------------------------------------------------

(define-frame XP-TYPICAL-ACTION-ANIMATES->ACTOR
    (isa                   (value (shallow-volitional-xp)))
    (actor                 (value (animate-object)))
    (action                (value (mop (actor (value =actor)
					      (relation =role)))))
    (role                  (value (actor (domain (value =action))
					 (co-domain (value =actor)))))
    (pre-xp-nodes          (value (=role =actor =action)))
    (explains              (value =role))
    (xp-asserted-nodes     )
    ;; |||||| This filler should really be a list and should be on the xp-asserted-nodes slot instead.
    (internal-xp-nodes     (value  =plan-choice))
    (plan-choice           (value (plan-selection (actor (value =actor))
                                           (plan (value =action))
                                           (role (value =role)))))
    (link1                 (value (mentally-results (domain (value =plan-choice))
                                             (co-domain (value =role)))))
    (links                 (value (=link1)))
)





;;;
;;; Actor performs the action because it results in the precondition for the
;;; main-action. The actor has the goal of achieving the precondition.
;;; 
(define-frame XP-INSTRUMENTAL-SCENE->ACTOR
    (isa                  (value (xp-normal-plan-choice)))
    (actor                (value (volitional-agent)))
    (conseq               (value (mop (actor (value =actor)
					     (relation =role))
				      ;; Avoid cycles in is-anomalous [cox 1jul95]
;;; 				      (instrumental-scene-of (value =ante))
				      (main-result (value =good-state)))))
    (ante          (value (mop (actor (value =actor))
				      (instrumental-scene (value =conseq))
				      (main-precondition (value =good-state)))))
    (consequent (value =conseq))
    (antecedent (value =ante))    
    (action
      (value =conseq))
    (main-action
      (value =ante))
    (good-state (value (state)))
    (role                 (value (actor (domain (value =conseq))
					(co-domain (value =actor)))))
;    (scene                (value (instrumental-scene (domain (value =main-action))
;                                              (co-domain (value =conseq)))))
    (goal   ; Added [cox 30jun95]
      (value (achievement-goal
	       (goal-actor (value =actor))
	       (goal-object (value =good-state)))))
    (plan-choice          (value (plan-selection
				   (actor (value =actor))
				   (plan (value =conseq))
				   ;; Achieve a precondition of main-action
				   (goal (value =goal))
				   (role (value =role)))))
    (pre-xp-nodes         (value (=role =actor =conseq
;;; 					=scene
					)))
    (explains             (value =role))
    (xp-asserted-nodes    (value (=plan-choice)))
    (internal-xp-nodes    (value (=good-state =ante)))
    (link1                (value (mentally-results (domain    (value =plan-choice))
						   (co-domain (value =role)))))
    (links                (value (=link1)))
    )




;;;
;;; Dogs bark when there exists some state (caused by some action) that
;;; threatens it.
;;; 
(define-frame XP-DEFENSIVE-BARK
  (isa (value (shallow-volitional-xp)))
  (actor (value (dog)))
  (object (value (physical-object)))		; Changed from threatening-object [cox 28feb95]
  (barking (value (dog-barks
		    (actor (value =actor))
		    (to (value (at-location
				 (domain (value =object))))))))
  (role (value (actor
		 (domain (value =barking))
		 (co-domain (value =actor)))))
  (threat (value (state)))
  (threatening-action (value (mop
			       (main-result (value =threat)))))
  (explains (value =role))
  (pre-xp-nodes (value (=actor =barking =object =role )))
  (internal-nodes (value (=threat)))
  (xp-asserted-nodes (value (=threatening-action)))
  (link1 (value (results
		  (domain (value =threatening-action))
		  (co-domain (value =threat)))))
  (link2 (value (enables
		  (domain (value =threat))
		  (co-domain (value =barking)))))
  (links (value (=link1 =link2)))
  )



;;;
;;; This xp is not quite right. The action is not the precondition for the
;;; main-action in the sense of resulting in some state that is necessary for
;;; the main-action; that is, the seal does not bark to produce the food that
;;; then can be eaten. Instead it is a contingency set up by the trainer,
;;; rather than some goal to be achieved by the seal as a volitional agent.
;;; How to represent this instead, if we want to be exact?
;;; 
(define-frame XP-HUNGRY-BARK
	      (isa (value (xp-instrumental-scene->actor)))
  (actor (value (seal)))
  (action (value (seal-barks
		   (actor (value =actor)
			  (relation =role))
		   (to (value (at-location
				(domain (value =consumable-object)))))
		   (instrumental-scene-of (value =main-action)))))
  (main-action (value (ingest
			(actor (value =actor))
			(object (value =consumable-object))
			(instrumental-scene (value =action)))))
  (consumable-object (value (food)))
  (role (value (actor (domain (value =action))
		      (co-domain (value =actor)))))
  (scene                (value (instrumental-scene (domain (value =main-action))
						   (co-domain (value =action)))))
  (plan-choice          (value (plan-selection
				 (actor (value =actor))
				 (plan (value =action))
;;                                 (goal <achieve a precondition of main-action>)
				 (role (value =role)))))
  (explains (value =role))
  (pre-xp-nodes (value (=actor =barking =action =scene
			       =main-action =consumable-object =role )))
  (internal-nodes (value *nil*))
  (xp-asserted-nodes (value (=plan-choice)))
  (link1 (value (results
		  (domain (value =threatening-action))
		  (co-domain (value =threat)))))
  (link2 (value (enables
		  (domain (value =threat))
		  (co-domain (value =barking)))))
  (link3 (value (mentally-results (domain (value =plan-choice))
				  (co-domain (value =role)))))
  (links (value (=link1 =link2 =link3)))
  )



;;; ||||||
;;; What to do with prevented-mop slot?
;;; Is it an asserted node or an internal node?
;;; After all, it is the act prevented and so will not 
;;; occur. But this is the inference we will try to 
;;; generalize on. Detecting explosives must be to 
;;; prevent terrorism since we know that detecting 
;;; drugs prevent drug-smuggling.
;;;

;;; Want this to be belief that plan will result in goal (disable SMUGGLING-ACT).
;;; Not actually. The goal is not prevent the state of drugs being in the country.
;;; 
;;; Actor does action because he knows that the plan which this action is an
;;; instrumental act for will lead to satisfying the goal and disenabling the 
;;; prevented-mop.
;;;
;;; Is the main-result slot of the XP really a pre-xp node? The result is a
;;; future result anticipated by the actor of the interdiction act. It cannot be
;;; matched against when checking for XP applicability.  And beware that we are
;;; using a slot-name here that has a very special meaning for actions and 
;;; events. A function that looks at structural info in memory items might make
;;; some dreadful conclusions just because of this naming convention.
;;; 
(define-frame XP-DISABLE-ACT-PRECONDITION
    (isa                (value (volitional-xp)) )
    (actor              (value (volitional-agent)))
    (enemy              (value (volitional-agent)))
    (hidden-item        (value (physical-object)))
    (good-state         (value (controls     
				 (domain (value =actor))	
				 (co-domain (value =hidden-item)))))
    (bad-state          (value (at-location 
				 (domain (value =hidden-item))
				 (co-domain (value (physical-location
						     (location-of (nation))))))))
    (action             (value (detection
			         (actor  (value =actor)
                                         (relation =role))
                                 (object (value =hidden-item))
                                 (main-result 
				   (value (knowledge-state 
					    (domain (value =actor))
					    (believed-item	
					      (value (at-location
						       (domain 
							(value =hidden-item))
						       ;; Commented out co-dom and value
						       ;; because p-loc was already 
						       ;; commented out. [mcox 4nov05]
;			                              (co-domain (value
;;; 						                   (physical-location
;;; 						                     (location-of
;;; 								       (value =hidden-item)))))
;								  ))
						      ))))
;					      (relation =main-result)
					      ))
				 (method (value (detection-method
						  (hidden-item (value =hidden-item))
						  (goal-scene (value
								(mtrans
								  (main-result
								    ;; Really should be ks above.
								    (value (knowledge-state)))
								  ))))))
                                 (side-effect (value =good-state)
                                              (relation =side-effect))
				 (instrumental-scene-of (value =main-action)))))
    (prevented-mop      (value (smuggling-act
			  (actor (value =enemy))
			  (object (value =hidden-item))
			  (main-result (value =bad-state))
			  )))
    ;; ||||||
    ;; Should this be confiscation? Or should the confiscation at least be represented?
    ;; The detection enables the confiscation, not the interdiction-act itself.
    (main-action (value (interdiction-act
			  (actor (value =actor))	; used to be (actor (value =actor))
			  (object (value =hidden-item))
			  (opponent (value =enemy))
			  (instrumental-scene (value =action))
			  (prevented-mop (value =prevented-mop))
			  (main-result (value =good-state)
				       (relation =main-result))
			  )))
    (role               (value (actor 
				 (domain    (value =action))
				 (co-domain (value =actor))
				 (slot (value (actor))))))  ; Careful the slot slot does not cause grief.
    (main-result        (value (main-result 
				 (domain (value =main-action))
				 (co-domain (value =good-state)))))
    (satisfied-goal     (value (prevention-goal
			             (goal-actor  (value =actor))
                                     (goal-object (value =bad-state)))))
    (side-effect        (value (side-effect (domain (value =action))
                                     (co-domain (value =good-state)))))
    (knowledge-state    (value (knowledge-state 
			  (domain (value =actor))
			  (believed-item (value =side-effect))
			  )))
    (plan-choice        (value (plan-selection (actor (value =actor))
                                        (plan (value =main-action))
                                        (goal (value =satisfied-goal))
                                        (role (value =role)))))
    (xp-type            (value (xp-instrumental-scene->actor)))
    (pre-xp-nodes       (value (	;Commented out all but actor 
					;node temporarily [mcox 4nov05]
;				=role 
				=actor 
;				=action =main-action
;                               =main-result =good-state
;                               =side-effect 
			       )))	; =bad-state was a pre-xp-node.
    (explains           (value =role))
    (xp-asserted-nodes  (value (=knowledge-state =satisfied-goal)))
    (internal-xp-nodes  (value (=enemy =hidden-item =plan-choice =bad-state)))
    (link1              (value (mentally-initiates 
			    (domain (value =knowledge-state))
			    (co-domain (value =satisfied-goal)))))
    (link2              (value (mentally-results 
			    (domain    (value =plan-choice))
			    (co-domain (value =role)))))
    (link3              (value (disenables 
			    (domain (value =main-result)) ;Changed from main-action [mcox 4nov05]
			    (co-domain (value =prevented-mop)))))
    ;; The following links should be a relation between a distinguished node in the XP-ASSERTED-NODES
    ;; and the EXPLAINS node of the sub-xp. |||||| Is this true though? The xp-asserted node is the
    ;; plan-choice which is not in this list. Also the scene slot of the xp is not passed. How can the
    ;; sub-xp be viewed as a link then?
    (link4              (value (xp-instrumental-scene->actor
				 (actor (value =actor))
				 (action (value =action))
				 (main-action (value =main-action))
				 (role (value =role)))))
    (links              (value (=link1 =link2 =link3 =link4)))
)




(define-frame MENTAL-CAUSAL-RELATION
    (isa            (value (causal-relation))))


(define-relation RESULTS
    (isa            (value (causal-relation)))
    (domain         (value (process)))
;;;     (co-domain      (value (relation))) ;I think that this used to be Ashwin's def.
    (co-domain      (value (state)))         ;; List of STATEs (or RELATIONs?).
)


(define-relation ENABLES
    (isa            (value (causal-relation)))
;;;     (domain         (value (relation))) ;I think that this used to be Ashwin's def.
    (domain         (value (state)))
    (co-domain      (value (process)))
    )


(define-frame MENTALLY-RESULTS
    (isa            (value (results mental-causal-relation)))
    (domain         (value (process)))
    (co-domain      (value (state)))
)


(define-frame MENTALLY-ENABLES
    (isa            (value (enables mental-causal-relation)))
    (domain         (value (state)))
    (co-domain      (value (process)))
)


;;;
;;; ||||| What are the lists doing on the domain and
;;; co-domain slots' value facets? 16 May 93
;;; 
(define-frame INITIATES
    (isa            (value (mentally-results)))
    (domain         (value (process state)))
    (co-domain      (value (state goal)))  ;; List of GOALs (GOAL ISA STATE ISA RELATION)
    )

(define-relation MENTALLY-INITIATES
    (isa            (value (initiates)))
  ;; |||||Domain really should be limited to mental-processes or mental-states.
    (domain         (value (entity)))
)


(define-relation DISENABLES
    (isa            (value (causal-relation)))
    (domain         (value (process)))
    (co-domain      (value (process)))
    )


(define-frame STRATEGY-CHOICE-VALUE
    (isa (value (attribute-value)))
  )

(define-frame QUESTION-IDENTIFICATION-CHOICE-VALUE
    (isa (value (strategy-choice-value)))
  )

(define-attribute-value QUESTIONING.0
    (isa (value (question-identification-choice-value)))
  )

(define-attribute-value SKIMMING.0
    (isa (value (question-identification-choice-value)))
  )



(define-frame HYPOTHESIS-GENERATION-CHOICE-VALUE
    (isa (value (strategy-choice-value)))
  )

(define-attribute-value EXPLANATION.0
    (isa (value (hypothesis-generation-choice-value)))
  )

(define-attribute-value CBR.0
    (isa (value (hypothesis-generation-choice-value)))
  )

(define-attribute-value ANALOGY.0
    (isa (value (hypothesis-generation-choice-value)))
  )

(define-attribute-value SUSPENSION.0
    (isa (value (hypothesis-generation-choice-value
		 hypothesis-verification-choice-value
		 plan-generation-choice-value
		 plan-evaluation-choice-value)))
  )


(define-frame HYPOTHESIS-VERIFICATION-CHOICE-VALUE
    (isa (value (strategy-choice-value)))
  )

(define-attribute-value COMPARISON.0
    (isa (value (hypothesis-verification-choice-value)))
  )


;;; |||||| NOTE that there is another devise-test.0 below
;;; which isa mental-state. 18nov93.
;;; 
(define-attribute-value DEVISE-TEST.0
    (isa (value (hypothesis-verification-choice-value)))
  )


;;; 
;;; The frames that represent the processes to implement the
;;; above choices.
;;;

(define-attribute-value POSE-QUESTION.0
    (isa (value (mental-process)))
  )

(define-attribute-value SKIM.0
    (isa (value (mental-process)))
  )

(define-attribute-value EXPLAIN.0
    (isa (value (mental-process)))
  )

(define-attribute-value ANALOGIZE.0
    (isa (value (mental-process)))
  )

(define-attribute-value EPISODIZE.0
    (isa (value (mental-process)))
  )

(define-attribute-value SUSPEND-TASK.0
    (isa (value (mental-process)))
  )

(define-attribute-value COMPARE.0
    (isa (value (mental-process)))
  )

(define-attribute-value DEVISE-TEST.0
    (isa (value (mental-process)))
  )

(define-attribute-value KNOWLEDGE-STATE.0
    (isa (value (knowledge-state)))
  )




(define-attribute-value INFERENCE.0
    (isa (value (inference)))
  )

(define-attribute-value HEURISTIC.0
    (isa (value (heuristic)))
  )




;;; 
;;; The decision should have access to and store knowledge 
;;; and inferences from memory. The considerations do not.
;;; 
;;; Processes can have side-effects, decisions do not, but
;;; other processes which result from decision choices do.
;;; ||||| Sould basis-of-decision be an XP or META-XP itself?
;;; This would be basing a decision on precedence. How is that?
;;; 
(define-frame DECISION-PROCESS
    (isa                     (value (mental-process)))
    (goals (value (considerations)))
    (agent                   (value (volitional-agent))) ; The agent is the system.
    ;; Could also be another Trace-Meta-XP, which would be saying I made this decision because
    ;; I always do it this way. Precedence as an explanation of one's reasoning.
    (basis-of-decision       (value (basis))) 
    ;; Following was mop-considered.
    (process-considered          (value (mental-process)))
    ;; This being the local outcome of this particular process stemming from this decision.
    (outcome-considered      (value (outcome 
				      (object (value =agent))
				      ;; ||||| NOTE that there is NO role-considered slot here!
				      (volitional-role-of-object (value =role-considered))
				      (results- (value =process-considered)))))
    ;; Just like physical actions such as MOPs, a mental process has a main-result.
    (main-result (value (strategy-choice-value)))
;;;     (mentally-results        (value =role-considered))
)

;;; 
;;; The outcomes of the processes chosen wil be either a question, hypothesis,
;;; verification success, failure or partial success, or learning sucess,
;;; failure or partial success. ???  What about review?
;;; 
;;; ||||| Or is the outcome an arbitrary entity? A relation? No could return a
;;; MOP.  What are possible values that strategies can return?  Maybe should
;;; force all high level "conscious" functions to return something isa outcome.
;;; Now some executions will return a question, some hypotheses, some success,
;;; etc.  What do these have in common? We thought about having them all as
;;; goals.  What is the META-XP glue?
;;;
;;; |||||| I changed members to be a list of entities rather than states. The
;;; reason is that episodize returns a case or mop to be put on the outcome of
;;; a d-c-node during function generate hypothesis (if strategy = cbr). So this
;;; results in a unification failure. Fix eventually. [cox 22feb95]
;;; 
(define-frame OUTCOME
  (isa                      (value (collection mental-state)))
  (natural-category         (value (state)))
  (collection-of            (value (mental-state)))
  (results-                 (value (process))) ;; Back pointer for RESULTS slot.
  (pos                      (value (state)))   ;; List of instances of positive members.
  (neg                      (value (state)))   ;; List of instances of negative members.
  (members                  (value ((entity)))) ;; List of all outcome states.
  )


;;; This is the old definition of outcome (if above slots are included
;;; by default) used by AQUA for the outcomes of a volitional agent
;;; taking on a role of a MOP. The definitions which depended on it
;;; should be updated.
;;; 
(define-frame MOP-OUTCOME
  (isa                       (value (outcome)))
  (object                    (value (volitional-agent)))
  (volitional-role-of-object (value (volitional-role-relation
				      (domain =results-)
				      (co-domain =object))))
  )


;;;
;;; ||||| Is it necessary to have an agent on this frame and on
;;; the decision process? What function does it play?
;;; 
(define-frame CONSIDERATIONS
    (isa                     (value (mental-state)))
    (agent                   (value (volitional-agent)))
    (process-considered      (value (mental-process)))
    (expected-outcome        (value (outcome 
				      (results-
					(value =process-considered)))))
    ;; Is this the place for the mentally-enables slot. Should not it be in the Trace-Meta-XP alone? Or no?
    (mentally-enables        (value (decision-process 
				      (agent (value =agent))
				      (process-considered
					(value =process-considered))
				      (outcome-considered
					(value =outcome-considered))
				      (self-outcome-considered 
					(value =self-outcome-considered))
				      (mop-component-considered 
					(value =mop-component-considered))
				      (mentally-results (value =role-considered)))))
    (prime-state             (value (goal)))	; Main goal.
    (states                  (value (state)))	; List of states.
    (state-orderings         (value (state-ordering)))
)


;;; 
;;; Definition for Decide-Compute-Node.
;;; 
;;; Probably will need a META-XP- (wherebound) slot.
;;; 
(define-frame D-C-NODE
  ;; ||||| Isa xp instead?
  (isa                (value (meta-xp)))
  (agent (value (volitional-agent)))
  ;; Probably wrong slot name but this points to the previous 
  ;; d-c-node in the global Trace-Meta-XP chain.
  ;; ||||| This will not be needed since we can assume a knowledge of the global 
  ;; Trace-Meta-XP structure including the slots which have the previous node. However
  ;; since Trace-Meta-XPs can assume different forms, there may not be a static order.
  (enables-           (value (d-c-node)))
  (initial-state      (value (considerations)))
  (strategy-choice    (value (strategy-choice-value)))
  (strategy-decision  (value (decision-process
			       (agent (value =agent)
				      (relation =role))
			       (basis-of-decision       (value (basis)))
			       (main-result (value =strategy-choice)))))
  (role (value (actor
		 (domain (value =strategy-decision))
		 (co-domain (value =agent)))))
  (strategy-execution (value (mental-process)))
  (side-effect        (value (considerations)))
  ;; ; The main-result is the return values from the strategy-execution.
  (main-result        (value (outcome
			       (results-
				 (value =strategy-execution)	; Added [cox 18feb95]
				 ))))
  (explains (value =role))			; Explains why agent picks strategy-execution [cox 22feb95]
  (pre-xp-nodes (value (=explains)))
  (link1              (value (mentally-enables
			       (domain (value =initial-state))
			       (co-domain (value =strategy-decision))
			       )))
  (link2              (value (mentally-results
			       (domain (value =strategy-decision))
			       (co-domain (value =strategy-choice))
			       )))
  (link3              (value (mentally-enables
			       (domain (value =strategy-choice))
			       (co-domain (value =strategy-execution))
			       )))
  (link4              (value (mentally-results
			       (domain (value =strategy-execution))
			       (co-domain (value =main-result))
			       )))
  (link5              (value (mentally-results
			       (domain (value =strategy-execution))
			       (co-domain (value =side-effect))
			       )))
  (links              (value (=link1 =link2 =link3 =link4 =link5)))
)



(define-frame REASONING-PHASE
  (isa (value (relation)))
  (domain (value (trace-meta-xp)))
  (co-domain (value (d-c-node)))
  )



;(define-frame REASONING-PHASE-VALUE
;  (isa (value (attribute-value)))
;  )

(define-frame IDENTIFICATION
  (isa (value (reasoning-phase)))
  (slot (value (identification)))
  )

(define-frame GENERATION
  (isa (value (reasoning-phase)))
  (slot (value (generation)))
  )

(define-frame EXAMINATION		; TEST.0
  (isa (value (reasoning-phase)))
;;;   (slot (value (test)))  
 (slot (value (examination)))
  )

(define-frame REVIEW
  (isa (value (reasoning-phase)))
  (slot (value (review)))
  )


;;; 
;;; |||||
;;; NOTE comment above concerning enables-. 
;;; The following static structure may not be appropriate.
;;; 
;;; Need to develop the question-identification d-c-node and have appropriate
;;; slots in the Trace-Meta-XP body.
;;; (The following is from blue notebook 14Feb91)
;;; When representing the META-understanding of a question we need to
;;; record a number of items:
;;;   1. What generated this question?
;;;   2. What do I do when it is answered?
;;;   3. Do I need a hard answer or just an indication?
;;;   4. How important is this question?
;;;   5. Are hypotheses mutually exclusive?
;;;   6. Is this a META-question?
;;; 
(define-frame TRACE-META-XP
  (isa                (value (meta-xp)))
;;; --------------------------------------------------------------------------
  ;; |||||
  ;; Currently I am sticking the old case in here. 
  ;; What about the new case?
  (cases              (value (mop)))
;;;   (decision           (value (basis)))
;;;   (confidence         (value (confidence-val)))
  (main-xp            (value (xp)))
  (XPs                (value =main-xp))
  (introspection (value (introspective-meta-xp)))
;;;   (prev-states         (value (mental-state)))
  (main-goal     (value (goal)))
  ;; Points to the last phase of reasoning in a Trace-Meta-XP
  ;; where any work was done. Used when resuming a suspended reasoning task.
  (current-phase (value (reasoning-phase
			  (domain (value =self)))))
;;; --------------------------------------------------------------------------
  ;; |||||
  ;; Should the Mental State glue in these transitions be Goals????
  ;; ||||| Note that the preconditions slot should be a list. See def. for MOP.
  (preconditions           (value (considerations)))
  (unknowns                (value (considerations))) ; Either questions or problems.
  (predictions             (value (considerations))) ; Either hypotheses or solutions (plans).
  (outcomes                (value (considerations)))
  (main-result             (value (mental-state)))
;;; --------------------------------------------------------------------------
  ;; The four phases of planning or understanding:
  (identification          (value (d-c-node
				    (initial-state (value =preconditions))
				    (side-effect (value =unknowns))
				    )))
  (generation              (value (d-c-node
				    (enables- (value =identification))
				    (initial-state (value =unknowns))
				    (side-effect (value =predictions))
				    )))
  (examination             (value (d-c-node
				    (enables- (value =generation))
				    (initial-state (value =predictions))
				    (side-effect (value =outcomes))
				    )))
  (review                  (value (d-c-node
				    (enables- (value =examination))
				    (initial-state (value =outcomes))
				    )))
;;; --------------------------------------------------------------------------
  (link1              (value (mentally-enables
			       (domain (value =preconditions))
			       (co-domain (value =identification))
			       )))
  (link2              (value (mentally-results
			       (domain (value =identification))
			       (co-domain (value =unknowns))
			       )))
  (link3              (value (mentally-enables
			       (domain (value =unknowns))
			       (co-domain (value =generation))
			       )))
  (link4              (value (mentally-results
			       (domain (value =generation))
			       (co-domain (value =predictions))
			       )))
  (link5              (value (mentally-enables
			       (domain (value =predictions))
			       (co-domain (value =examination))
			       )))
  (link6              (value (mentally-results
			       (domain (value =examination))
			       (co-domain (value =outcomes))
			       )))
  (link7              (value (mentally-enables
			       (domain (value =outcomes))
			       (co-domain (value =review))
			       )))
  (link8              (value (mentally-results
			       (domain (value =review))
			       (co-domain (value =main-result))
			       )))
  (links              (value (=link1 =link2 =link3 =link4 =link5 =link6 =link7 =link8)))
)



;;; 
;;; A COLLECTION is a set of entities.
;;; 
(define-frame COLLECTION
  (isa            (value (entity)))
  (collection-of  (value (entity)))   ;; all members are ISA this.
  (members        (value ((entity))))   ;; a list of entities. 
    )


(define-relation MEMBERS
    (isa            (value (attribute)))
    (co-domain      (value (entity)))
    )


(define-relation COLLECTION-OF
    (isa            (value (attribute)))
    (co-domain      (value (entity)))
    )


;;; 
;;; We should use the truth slot or 
;;; 
(define-relation NOT-EQUAL-RELATION
  (isa (value (relation)))
  (domain (value (entity)))
  (co-domain (value (entity)))
  (truth (value (truth-value)))
  )


(define-relation EQUAL-RELATION
  (isa (value (relation)))
  (domain (value (entity)))
  (co-domain (value (entity)))
  (truth (value (truth-value)))
  )


(define-frame BASE-TYPE
  (isa (value (mental-state)))
  )

(define-frame OMISSION-ERROR
  (isa (value (base-type)))
  )


(define-frame COMMISSION-ERROR
  (isa (value (base-type)))
  )


;;;
;;; Failures of commission.
;;; 
;;; ||||| Could just have a generic failure-type isa mental-state
;;; which both expectation-failure and retrieval-failure isa since
;;; they have the same named slots.
;;; 
(define-frame EXPECTATION-FAILURE
  (isa (value (commission-error)))
  ;||||| Should be relation. Could be equals or not-equals.
  ;; Backpointer to the relation which initiated the failure.
  (initiates- (value (state)))			; This state will result from a comparison in some GMXP.
  (expected-outcome (value (state)))		; ||||| Really should be entity. Could have specializations
  (actual-outcome (value (state)))		; of ef which deal with XPs (states) and others that deal with
  )						; efs on mops (such as input-anomaly).


;;; ||||||Note how similar the following is from the definition of anomaly.
;;; 
(define-frame INCORPORATION-FAILURE
  (isa (value (commission-error)))
  ;; Backpointer to the relation which initiated the failure.
  (initiates- (value (state)))
  (computed-occurence (value (entity)))		; Obtained from constraint on some conceptual definition.
  (actual-occurence (value (entity)))		; Obtained from some input.
  ; ||||| Eventually use actual-occurence in functions that operate on anomaly and remove action slot.
  (action (value (mop)))
  (paths (value (literal)))			; Points to sub-frame where the contradiction occured.
  )



;;;
;;; Failures of omission.
;;; 
;;; ||||| The names of these slots are odd.
;;; What are the semantics. Is a retrieval failure
;;; mental-state represented like this afterall?
;;;
;;; Also NOTE piped comments above. Apply here also.
;;;
(define-frame RETRIEVAL-FAILURE
  (isa (value (omission-error)))
  (initiates- (value (state)))	    ;Backpoinmter to the state which initiated the failure.
  (expected-outcome (value (state)))
  (actual-outcome (value (state)))
  )



(define-frame SUCCESSFUL-PREDICTION
  (isa (value (base-type)))
  (initiates- (value (state)))	    ;Backpoinmter to the state which initiated the success.
  (expected-outcome (value (state)))
  (actual-outcome (value (state)))
  )



(define-attribute-value XP-TYPE.0
  (isa (value (index-type-value)))
  )

(define-attribute-value CASE-TYPE.0
  (isa (value (index-type-value)))
  )

(define-attribute-value PLAN-TYPE.0
  (isa (value (index-type-value)))
  )

(define-attribute-value QUESTION-TYPE.0
  (isa (value (index-type-value)))
  )

(define-frame INDEX-TYPE-VALUE
  (isa (value (attribute-value)))
  )


;;; 
;;; Note that we will have to accomodate arbitrarily complex indices. For
;;; example, not just (bark (actor dog)), but also (bark (actor dog)(at
;;; animate-object)).
;;;
;;; See function remove-indices-to-answered-question to see how this frame is
;;; currently used. It is also used as a convenient symbol to instantiate when 
;;; indexing an item in memory, but the slots are not currently used when in
;;; this capacity. If changed later, keep consistent with the semantics
;;; remove-indices-to-answered-question depends on.
;;
;;; Now index is seen as a relation (I have been painting them as such in all
;;; figures within papers) mapping from relation to memory-item. They are
;;; implemented as micro-indexes (e.g., the BECAUSE explanation is indexed
;;; along actor -> bark -> dog -> to -> container -> xp-type.0), but
;;; conceptually they are relations, from concept to memory, that is, from
;;; domain to co-domain. Note that all memory-items correspond to the type
;;; (xp-type.0 -> memory-item isa xp, case-type.0 -> memory-item isa case,
;;; plan-type.0 -> memory-item isa plan, and question-type.0 -> memory-item isa
;;; question.) See comments in file memory.lisp (especially above the function
;;; do-index) for additional implementational details. [1dec93]
;;; 
(define-frame INDEX 
  (isa (value (relation)))
  (type (value (index-type-value)))
  (domain (value (relation)))
  (co-domain  (value (entity)))
  (relation (value =domain))
  (memory-item (value =co-domain))
  )


;;;
;;; This is the micro-index discussed above and in file memory.lisp.
;;; 
(define-frame micro-index
  (isa (value (entity)))
  )


;;;
;;; This frame is defined for those values which will be interpreted
;;; as is with no further level of inspection by frame traversal or
;;; unification routines. Examples are names of people
;;; (eg., (name (value literal.101))
;;;       literal.101 -> "Michael" )
;;; or paths in an anomaly
;;; (eg., (anomaly (paths (value literal.201)))
;;;       literal.201 -> (co-domain domain)  )
;;; 
(define-frame LITERAL
  (isa (value (entity)))
  )


;;;
;;; ||||| Eventually need to represent the paths slot with frames
;;; instead of just shoving the path chasing list on it.
;;; NOTE that when this occurs we must eliminate the redefined function
;;; f.copy-instantiated-frame (and its associated functions:
;;; blend, get-from-list,copy-instantiated-frame and the variables
;;; new-list and old-list) in file new-funs.lisp.
;;;
(define-frame ANOMALY				; CONSTRAINT-ANOMALY
  (isa (value (commission-error)))
  (expected-outcome (value (entity)))		; Obtained from some conceptual definition.
  (actual-outcome (value (entity)))		; Obtained from some input.
  ; ||||| Eventually make actual-outcome in functions that operate on anomaly and remove action slot.
  (action (value (mop)))
  (paths (value (literal)))			; Points to sub-frame where the contradiction occured.
  )



;;;
;;; ||||| I am not too sure about this definition.
;;; Should the derivation actually be on a facet?
;;; It might be better to allow any frame to have a derivation slot.
;;; 
(define-frame FALSIFIES
  (isa (value (anomaly)))
  (domain (value (entity))
	  (derivation (derivation-type)))
  (co-domain (value (entity))
	     (derivation (derivation-type)))
  (paths (value (literal)))
  )


(define-frame DERIVATION-TYPE
	      (isa (value (process)))
  )


;;; Rule-like mental process resulting in a choice.
;;; 
(define-frame HEURISTIC
  (isa (value (mental-process)))
  (preconditions (value (relation)))
  (main-result (value (strategy-choice-value)))
  )


;;; Rule-like mental process resulting in additional knowledge.
;;; 
(define-frame INFERENCE
  (isa (value (mop)))
  (preconditions (value (relation)))
  (main-result (value (knowledge-state)))
  )


;;; The basis of a decision is composed of three types of factors.
;;; A collection of knowledge assertions, a collection of inferencess
;;; which produce some assertion, and a collection of decision heuristics.
;;; 
(define-frame BASIS
  (isa (value (entity)))
  (knowledge (value (collection
		      (collection-of (value knowledge-state.0)))))
  (inferences (value (collection
		       (collection-of (value inference.0)))))
  (decision-heuristics (value (collection
				(collection-of (value heuristic.0)))))
  )


(define-frame LEARN/REVIEW-CHOICE-VALUE
    (isa (value (strategy-choice-value)))
  )

(define-attribute-value EBG.0
  (isa (value (learn/review-choice-value)))
  )


(define-attribute-value GENERALIZATION.0
  (isa (value (learn/review-choice-value)))
  )


(define-attribute-value ABSTRACTION.0
  (isa (value (learn/review-choice-value)))
  )


(define-attribute-value SPECIALIZATION.0
  (isa (value (learn/review-choice-value)))
  )


(define-attribute-value CONDITIONALIZATION.0
  (isa (value (learn/review-choice-value)))
  )


;;; The frames that represent the processes to implement the
;;; above choices.

(define-attribute-value DO-EBG.0
    (isa (value (mental-process)))
  )

(define-attribute-value GENERALIZE.0
    (isa (value (mental-process)))
  )

(define-attribute-value ABSTRACT.0
    (isa (value (mental-process)))
  )

(define-attribute-value SPECIALIZE.0
    (isa (value (mental-process)))
  )


(define-attribute-value INDEX-NEW-XP.0
    (isa (value (mental-process)))
  )



(define-frame LEARNING-PROCESS
  (isa (value (mental-process)))
  (learning-type (value (learn/review-choice-value)))
  (credit-blame (value (entity)))
;;;   (algorithm (value (mental-process)))
  )


;;; Keep in mind that the "entity" filling the new-input slot must be a mental action
;;; and the A slot must be a state for the mentally-results links to hold to the syntax.
;;; A and E can then be states such as xps and knowledge states.
;;; 
;;; ||||| Is the RC node really a link; if it is a Trace-Meta-XP
;;; then it is a relation between the M and E nodes. This is like
;;; the sub-XPs in structures like XP-Religious-Fanatic.

(define-frame INTROSPECTIVE-META-XP
  (isa (value (meta-xp)))
  (a (value (entity
;;; 	      (explains (value (entity)))
	      )))
  (e (value (entity)))
  (e-prime (value (entity)))
  (m (value (entity)))
  (m-prime (value (entity)))
  (rc (value (trace-meta-xp)))
  (not-equals (value (not-equal-relation
		       (domain (value =a))
		       (co-domain (value =e)))))
  (ef (value (expectation-failure
	       (initiates- (value =not-equals))
	       (expected-outcome (value =e))
	       (actual-outcome (value =a)))))
  (equals (value (equal-relation
		   (domain (value =a))
		   (co-domain (value =e-prime)))))
  (sp (value (successful-prediction
	       (initiates- (value =equals))
	       (expected-outcome (value =e))
	       (actual-outcome (value =a)))))
  (bad-state (value (truth
		      (domain (value =e-prime))
		      (co-domain (value out.0)))))
  (rf (value (retrieval-failure
	       (initiates- (value =bad-state))
	       (expected-outcome (value =e-prime))
	       (actual-outcome (value =a)))))
  (new-input (value (entity)))
  ;; The reason that the reasoning chain was begun.
  (reasoning-goal (value (goal)))
  ;; The nodes are the nodes listed in Table1 of (Cox & Ram, 1991) + a and new-input.
  (nodes (value (=a =e =e-prime =m =m-prime =rc =sp =ef =rf =new-input)))
;  (link1 (value (trace-meta-xp
;		  (domain (value =reasoning-goal))
;		  (co-domain (value =e)))))
  (link1 (value (mentally-initiates
		  (domain (value =m))
		  (co-domain (value =rc)))))
  (link2 (value (mentally-results
		  (domain (value =rc))
		  (co-domain (value =e)))))
  (link3 (value (mentally-results
		  (domain (value =new-input))
		  (co-domain (value =a)))))
  (link4 (value (mentally-initiates
		  (domain (value =not-equals))
		  (co-domain (value =ef)))))
  (link5 (value (mentally-initiates
		  (domain (value =equals))
		  (co-domain (value =rf)))))
  (links (value (=link1 =link2 =link3 =link4 =link5)))
  (learning-algorithm      (value ((learning-process))))
  )



(define-frame IMXP-SUCCESSFUL-PREDICTION
  (isa (value (introspective-meta-xp)))
  (a (value (entity
	      (explains (value (entity))))))
  (e (value (entity
	      (truth (value hypothesized-in.0)))))
  (m (value (entity)))
  (rc (value (trace-meta-xp)))
  (sp (value (successful-prediction
	       (initiates- (value =equals))
	       (expected-outcome (value =e))
	       (actual-outcome (value =a)))))
  (equals (value (equal-relation
		   (domain (value =a))
		   (co-domain (value =e)))))
  (new-input (value (entity)))
  (nodes (value (=a =e =m =rc =sp =new-input)))
  (pre-xp-nodes (value (=a =e =sp)))
  (explains (value =sp))
  (xp-asserted-nodes (value (=m =equals)))
  (internal-nodes )
  (link1 (value (mentally-results
		  (domain (value =rc))
		  (co-domain (value =e)))))
  (link2 (value (mentally-results
		  (domain (value =new-input))
		  (co-domain (value =a)))))
  (link3 (value (mentally-initiates
		  (domain (value =equals))
		  (co-domain (value =sp)))))
  (links (value (=link1 =link2 =link3)))
  )



(define-frame IMXP-EXPECTATION-FAILURE
  (isa (value (introspective-meta-xp)))
  (a (value (entity
	      (explains (value (entity))))))
  (e (value (entity
	      (truth (value hypothesized-in.0)))))
  (m (value (entity)))
  (rc (value (trace-meta-xp)))
  (not-equals (value (not-equal-relation
		       (domain (value =a))
		       (co-domain (value =e)))))
  (ef (value (expectation-failure
	       (initiates- (value =not-equals))
	       (expected-outcome (value =e))
	       (actual-outcome (value =a)))))
  (new-input (value (entity)))
  (nodes (value (=a =e =m =rc =ef =new-input)))
  (pre-xp-nodes (value (=a =e =ef)))
  (explains (value =ef))
  (xp-asserted-nodes (value (=m =not-equals)))
  (internal-nodes )
  (link1 (value (mentally-results
		  (domain (value =rc))
		  (co-domain (value =e)))))
  (link2 (value (mentally-results
		  (domain (value =new-input))
		  (co-domain (value =a)))))
  (link3 (value (mentally-initiates
		  (domain (value =not-equals))
		  (co-domain (value =ef)))))
  (links (value (=link1 =link2 =link3)))
  )



(define-frame IMXP-RETRIEVAL-FAILURE
  (isa (value (introspective-meta-xp)))
  ;; ||||| This may be a poor name for the relation since other XPs use it.
  ;; The inferences that may eventually go with it may conflict. What is a better name?
  (a (value (entity
	      (explains (value (entity))))))
  (e (value (entity
	      (truth (value out.0))
	      (relation (value =bad-state)))))
  (m (value (entity)))
  (rc (value (trace-meta-xp
	       (truth (value out.0)))))
  (equals (value (equal-relation
		   (domain (value =a))
		   (co-domain (value =e)))))
  (bad-state (value (truth
		      (domain (value =e))
		      (co-domain (value out.0)))))
  (rf (value (retrieval-failure
	       (initiates- (value =bad-state))
	       (expected-outcome (value =e))
	       (actual-outcome (value =a)))))
  (new-input (value (entity)))
  (nodes (value (=a =e =m =rc =rf =new-input)))
  ;; ||||| At first I was tempted to place =e below. However because the truth is out,
  ;; the function xp-applicable-p would fail. Perhaps I should modify such a function
  ;; to make exceptions which specify the truth value of the node since we want to be able
  ;; to deal with negative info. Eg. some event NOT happening being the reason for
  ;; disappointment states.
  (pre-xp-nodes (value (=a =rf)))
  (explains (value =rf))
  (xp-asserted-nodes (value (=m =equals)))
  (internal-nodes (value (=rc)))
  (link1 (value (mentally-initiates
		  (domain (value =m))
		  (co-domain (value =rc))
		  (truth (value out.0)))))
  (link2 (value (mentally-results
		  (domain (value =rc))
		  (co-domain (value =e))
		  (truth (value out.0)))))
  (link3 (value (mentally-results
		  (domain (value =new-input))
		  (co-domain (value =a)))))
  (link4 (value (mentally-initiates
		  (domain (value =bad-state))
		  (co-domain (value =rf)))))
  (links (value (=link1 =link2 =link3 =link4)))
  )



(define-frame IMXP-NOVEL-SITUATION
  (isa (value (imxp-retrieval-failure)))
  (m (value (entity
	      (truth (value out.0)))))
  )

;;; No explains slot on this XP. [mcox 19oct06]
(define-frame IMXP-NOVEL-SITUATION
  (isa (value (introspective-meta-xp)))
  (a (value (entity
;;; 	      (explains (value (entity)))
	      )))
  (e (value (entity
	      (truth (value out.0)))))
  (m (value (entity
	      (truth (value out.0)))))
  (rc (value (trace-meta-xp
	       (truth (value out.0)))))
  (ef (value (expectation-failure
	       (truth (value out.0)))))
  (equals (value (equal-relation
		   (domain (value =a))
		   (co-domain (value =e)))))
  (rf (value (retrieval-failure
	       (initiates- (value =equals))
	       (expected-outcome (value =e))
	       (actual-outcome (value =a)))))
  (new-input (value (entity)))
  (nodes (value (=a =e =m =rc =sp =ef =rf =new-input)))
  (link1 (value (mentally-initiates
		  (domain (value =m))
		  (co-domain (value =rc))
		  (truth (value out.0)))))
  (link2 (value (mentally-results
		  (domain (value =rc))
		  (co-domain (value =e))
		  (truth (value out.0)))))
  (link3 (value (mentally-results
		  (domain (value =new-input))
		  (co-domain (value =a)))))
  ;; ||||| NOTE that there does NOT exist a not-equals slot yet.
  (link4 (value (mentally-initiates
		  (domain (value =not-equals))
		  (co-domain (value =ef)))))
  (link5 (value (mentally-initiates
		  (domain (value =equals))
		  (co-domain (value =rf)))))
  (links (value (=link1 =link2 =link3 =link4 =link5)))
  )



;(define-frame XP-NOVEL-SITUATION
;  (isa (value (introspective-meta-xp)))
;  (decision-basis (value (basis)))
;  (q (value (entity)))
; ;; ||||| Is the following important?
;  (satisfied-goal (value (knowledge-acquisition-goal
;			   (goal-object (value =q)))))
;  (wrong-xp (value (xp)))			; = E
;  (right-xp (value (xp				; = A
;		     (explains (value =q)))))
;  (difference (value (not-equal-relation
;		       (domain (value =right-xp))
;		       (co-domain (value =wrong-xp))
;		       (truth (value in.0)))))
;  (failure (value (expectation-failure
;		    (initiates-  (value =difference))
;		    (expected-outcome (value =wrong-xp))
;		    (actual-outcome (value =right-xp)))))
;  (decision (value (decision-process
;		     (basis-of-decision (value =decision-basis)))))
;  (hypothesis-gen (value (d-c-node
;			   (initial-state      (value (considerations
;							;; ||||| Should this be =satisfied-goal?
;							(prime-state (value =q)))))
;			   (strategy-choice    (value explanation.0))
;			   (strategy-decision  (value =decision))
;			   (strategy-execution (value explain.0))
;			   (main-result        (value (outcome
;							(prime-state (value =wrong-xp)))))
;			   )))
;  (input-context (value (state)))
;  (new-input (value (process)))
;  (answers (value (relation
;		    (domain (value =right-xp))
;		    (co-domain (value =q)))))
;  (pre-xp-nodes (value (=failure =wrong-xp =right-xp)))
;  (explains (value =failure))
;  (internal-nodes )
;  (xp-asserted-nodes (value (=difference =q =new-input)))
;  (link1 (value (mentally-initiates
;		  (domain (value =difference))
;		  (co-domain (value =failure)))))
;  (link2 (value (mentally-results
;		  (domain (value =new-input))
;		  (co-domain (value =right-xp)))))
;  
;  (link3 (value (mentally-results
;		  (domain (value =new-input))
;		  (co-domain (value =satisfied-goal)))))
;  (links (value (=link1 =link2 =link3 =hypothesis-gen)))
;
;  (learning-algorithm (value (learning-process)))
;  )



;;; 
;;; There used to be another definition of IMXP-NOVEL-SITUATION-ALTERNATIVE-REFUTED
;;; at this location (just before the following definition). It is now in file
;;; old-fragments.lisp The next one is old. The second one is the current one.
;;; 


(define-frame IMXP-NOVEL-SITUATION-ALTERNATIVE-REFUTED
  (isa (value (introspective-meta-xp)))
  ;; ||||| For now this gets bound to the actor slot (somehow),
  ;; but in reality the question should be the relation:
  ;; (explanations
  ;;   (domain (actor (domain bark)
  ;; 	             (co-domain dog)))
  ;;   (co-domain (=a))
  ;;   (status question)).
  (q (value (relation
	      (explanations (value (=a))))))
  ;; The actual explanation which answers the question.
  (a (value (xp)))
  ;; The node e is the hypothesis (expected answer to the question).
  (e (value (xp
	      (explains (value =q))
;;; 	      (truth (value hypothesized-in.0))
	      )))
  (e-prime (value (entity
		    (truth (value out.0)))))
  ;; Will be bound to the index used to retrieve e.
  (m (value (entity)))
  (m-prime (value (entity
		    (truth (value out.0)))))
  ;; Will be bound to the anomaly causing the question.
  ;; ||||| Should be the constraint.
  (c (value (entity)))
  (h-decision-basis (value
		      (basis
			(knowledge
			  (value (collection
				   (members (value
					       ((knowledge-state
						  (co-domain (value =m))
						  (believed-item (value =m))))))))))))
  (q-decision-basis (value
		      (basis
			(knowledge
			  (value (collection
				   (members (value
					      ((knowledge-state
						 (co-domain (value =c))
						 ;; ||||| How to represent the path
						 ;; information to the anomaly?
						 (believed-item (value =c))))))))))))
;  (satisfied-goal (value (knowledge-acquisition-goal
;			   (goal-object (value =q)))))
  (rc (value (trace-meta-xp
	       (identification
		 (value =q-id))
	       (generation
		 (value =hypo-gen)))))
  (not-equals (value (not-equal-relation
		       (domain (value =a))
		       (co-domain (value =e)))))
  (ef (value (expectation-failure
	       (initiates- (value =not-equals))
	       (expected-outcome (value =e))
	       (actual-outcome (value =a)))))
  (equals (value (equal-relation
		   (domain (value =a))
		   (co-domain (value =e-prime)))))
  (rf (value (retrieval-failure
	       (initiates- (value =equals))
	       (expected-outcome (value =e-prime))
	       (actual-outcome (value =a)))))
  (new-input (value (entity)))
  (nodes (value (=a =e =e-prime =m =m-prime =rc =ef =rf =new-input)))
  (link1 (value (mentally-initiates
		  (domain (value =m))
		  (co-domain (value =rc)))))
  (link2 (value (mentally-results
		  (domain (value =rc))
		  (co-domain (value =e)))))
  (link3 (value (mentally-results
		  (domain (value =new-input))
		  (co-domain (value =a)))))
  (link4 (value (mentally-initiates
		  (domain (value =not-equals))
		  (co-domain (value =ef)))))
  (link5 (value (mentally-initiates
		  (domain (value =equals))
		  (co-domain (value =rf)))))
  (links (value (=link1 =link2 =link3 =link4 =link5
;;; ?			 =hypothesis-gen
;;; ?			 =q-id
;;; ?			 =rc
			 )))
  (k-goal (value
	    (knowledge-acquisition-goal
	      (goal-object (value (generate
				    (co-domain (value =q))))))))
  (h-decision (value (decision-process
		      (basis-of-decision (value =h-decision-basis)))))
  (hypo-gen (value (d-c-node
		     (initial-state      (value (considerations
						  ;; ||||| Should this be =satisfied-goal?
						  (prime-state (value =k-goal)))))
		     (strategy-choice    (value explanation.0))
		     (strategy-decision  (value =h-decision))
		     (strategy-execution (value explain.0))
		     )))
  (q-decision (value (decision-process
		      (basis-of-decision (value =q-decision-basis)))))
  (q-id (value (d-c-node
		 (strategy-choice    (value questioning.0))
		 (strategy-decision  (value =q-decision))
		 (strategy-execution (value pose-question.0))
		 (side-effect        (value (considerations
					      (prime-state (value =k-goal)))))
		 )))
;;;   (input-context (value (state)))
  (new-input (value (process)))
;  (answers (value (relation
;		    (domain (value =right-xp))
;		    (co-domain (value =q)))))
  (pre-xp-nodes (value (=ef =a =e)))
  (explains (value =ef))
  (internal-nodes (value (=q =hypo-gen =e-prime =rf)))
  ;; Very important for m and m-prime to be in the list of xp-asserted-nodes
  ;; since this allows reasoning questions to be posted on the introspective-meta-xp.
  (xp-asserted-nodes (value (=not-equals =q-id =c =new-input =m =m-prime)))
;  (link3 (value (mentally-results
;		  (domain (value =new-input))
;		  (co-domain (value =satisfied-goal)))))
  (learning-algorithm (value ((learning-process	; Handles Incorrect BK.
			       (learning-type (value generalization.0))
			       (credit-blame (value (=a =c)))
			       )
			      (learning-process	; Handles Mis-indexed Structure.
			       (learning-type (value specialization.0))
			       (credit-blame (value (=m =a =c)))
			       )
			      )))
  )



;;; 
;;; This is the Latest Version.
;;; 
;;; 
;;; ||||| Should this really be a predefined instance?  Much more work needs to
;;; go into this representation.  For example the representation of the lower
;;; part of Fig.7: What is in and out, etc. Another possible problem:  E node
;;; has hypothesized-in truth slot, but the program will have made it a
;;; question with what kind of truth?
;;; 
;;; ||||| Another potential major problem is unification with the inclusion of
;;; lists. These occur in the collection frames of both decision-basis frames.
;;; Will unify work?
;;; 
(define-frame IMXP-NOVEL-SITUATION-ALTERNATIVE-REFUTED
	      (isa (value (composite-introspective-meta-xp)))
  (failure-cause (value (incorrect-domain-knowledge.0
			  novel-situation.0
			  erroneous-association.0)))
  ;; ||||| For now this gets bound to the actor slot (somehow),
  ;; but in reality the question should be the relation:
  ;; (explanations
  ;;   (domain (actor (domain bark)
  ;; 	             (co-domain dog)))
  ;;   (co-domain (=a2))
  ;;   (status question)).
  (q (value (relation
;;; 	      (explanations (value (=a2)))
	      )))
  ;; The actual explanation which answers the question.
  (a2 (value (xp)))
  ;; The node e is the hypothesis (expected answer to the question).
  (e (value (xp
	      (explains (value =q))
;;; 	      (truth (value hypothesized-in.0))
	      )))
  (e-prime (value (xp
;;; 		    (truth (value out.0))
	      )))
  ;; Will be bound to the index used to retrieve e.
  (i (value (index
	      (domain (value =q))
	      (co-domain (value (=m)))
	      (relation (value =q))
	      (memory-item (value (=m))))))
  (i-prime (value (index
		    (domain (value =q))
		    (co-domain (value =m-prime))
		    (relation (value =q))
		    (memory-item (value =m-prime)))))
  ;; The abstract xp retrieved with i to produce e.
  (m (value (xp)))
  (m-prime (value (entity
		 ;;If this is made out.0 and m-prime is unified with A2 (which will be in.0) the unification fails.
		    ;; (truth (value out.0))
		    )))
  (a1 (value (entity)))
  (c (value (entity)))
  ;; Will be bound to the anomaly causing the question.
  ;; ||||| Should be the constraint.
  (anomaly (value (anomaly
		      (expected-outcome (value =c))
		      (actual-outcome (value =a1))
		      (action (value (mop =a-mop)))
		      (paths (value (literal =a-literal))))))
  (i-f (value (incorporation-failure
		(initiates- (value (state)))
		(computed-occurence (value =c))
		(actual-occurence (value =a1))
		(action (value =a-mop))
		(paths (value =a-literal)))))
  (h-decision-basis
    (value
      (basis
	(knowledge
	  (value (collection
		   (members (value
			      ((knowledge-state
				 (co-domain (value =i))
				 (believed-item (value =i))))))))))))
  (q-decision-basis
    (value
      (basis
	(knowledge
	  (value (collection
		   (members (value
			      ((knowledge-state
				 (co-domain (value =anomaly))
				 ;; ||||| How to represent the path
				 ;; information to the anomaly?
				 (believed-item (value =anomaly))))))))))))
;  (satisfied-goal (value (knowledge-acquisition-goal
;			   (goal-object (value =q)))))
  (rc (value (trace-meta-xp
	       (identification
		 (value =q-id))
	       (generation
		 (value =hypo-gen))
	       (examination
		 (value =verify-node)))))
  (not-equals (value (not-equal-relation
		       (domain (value =a2))
		       (co-domain (value =e)))))
  (ef (value (expectation-failure
	       (initiates- (value =not-equals))
	       (expected-outcome (value =e))
	       (actual-outcome (value =a2)))))
  (equals (value (equal-relation
		   (domain (value =a2))
		   (co-domain (value =e-prime)))))
  (rf (value (retrieval-failure
	       (initiates- (value =equals))
	       (expected-outcome (value =e-prime))
	       (actual-outcome (value =a2)))))
  (new-input (value (entity)))
  (nodes
    (value
      (=a2 =e =e-prime =i =m =m-prime =rc =ef =rf =new-input)))
  (link1 (value (mentally-initiates
		  (domain (value =i))
		  (co-domain (value =rc)))))
  (link2 (value (mentally-results
		  (domain (value =rc))
		  (co-domain (value =e)))))
  (link3 (value (mentally-results
		  (domain (value =new-input))
		  (co-domain (value =a2)))))
  (link4 (value (mentally-initiates
		  (domain (value =not-equals))
		  (co-domain (value =ef)))))
  (link5 (value (mentally-initiates
		  (domain (value =equals))
		  (co-domain (value =rf)))))
  (links (value (=link1 =link2 =link3 =link4 =link5
;;; ?			 =hypothesis-gen
;;; ?			 =q-id
;;; ?			 =rc
			)))
  ;; The goal to answer the question?
  (k-goal (value
	    (knowledge-acquisition-goal
	      (goal-object
		(value
		  (generate
		    (co-domain (value =q))))))))
  (h-decision (value (decision-process
		       (basis-of-decision (value =h-decision-basis)))))
  (hypo-gen (value (d-c-node
		     (initial-state      (value (considerations
						  ;; ||||| Should this be =satisfied-goal?
						  (prime-state (value =k-goal)))))
		     (strategy-choice    (value explanation.0))
		     (strategy-decision  (value =h-decision))
		     (strategy-execution (value explain.0))
		     (main-result (value (outcome
					   (members (value (=e))))))
		     )))
  (q-decision (value (decision-process
		       (basis-of-decision (value =q-decision-basis)))))
  (q-id (value (d-c-node
		 (strategy-choice    (value questioning.0))
		 (strategy-decision  (value =q-decision))
		 (strategy-execution (value pose-question.0))
		 (side-effect        (value (considerations
					      (prime-state (value =k-goal)))))
		 )))
  (verify-node (value (d-c-node
			(strategy-choice    (value compare.0))
			(strategy-execution (value comparison.0))
			(main-result (value (outcome
					      (members (value (=link4))))))
			)))
;;;   (input-context (value (state)))
;;;   (new-input (value (process)))	   ; This IMXP has 2 new-input slots. Removed [cox 30jun95]
;  (answers (value (relation
;		    (domain (value =right-xp))
;		    (co-domain (value =q)))))
  (pre-xp-nodes (value (=ef =a2
;;; 			    =e			; Cannot test for this since it is unverified [cox 22feb95]
			    )))
  (explains (value =ef))
  (internal-nodes (value (=q =hypo-gen =e-prime =rf)))
  ;; Very important for m and m-prime to be in the list of xp-asserted-nodes
  ;; since this allows reasoning questions to be posted on the introspective-meta-xp.
  ;; Does i belong here not that m is the item retrieved rather than the index.
  ;; Also there is anomaly and c confusion.
  (xp-asserted-nodes
    (value
      (=not-equals =q-id =anomaly =c =new-input =i =m =m-prime)))
;  (link3 (value (mentally-results
;		  (domain (value =new-input))
;		  (co-domain (value =satisfied-goal)))))
  (learning-algorithm (value (
			      (learning-process	           ; Handles Incorrect BK.
				(learning-type (value abstraction.0))
				(credit-blame (value (=a1 =anomaly)))	; |||||| Shoud this be =a1 ??
				)
			      (learning-process	           ; Handles Novel Situation.
				(learning-type (value generalization.0))
;;; 				(credit-blame (value (=a2 =self (literal m-prime))))
				(credit-blame (value (=a2 =m-prime)))
				)
			      (learning-process	           ; Handles Mis-indexed Structure.
				(learning-type (value specialization.0))
				(credit-blame (value (=i =anomaly =m-prime)))
				)
			      )))
  (potential-learning-goals (value ((knowledge-reconciliation-goal
			    (domain         (value person.0))
			    (co-domain      (value (merged =g-o1
						     (domain (value =a1))
						     (co-domain (value =c)))))
			    (goal-actor     (value person.0))
			    (goal-object    (value =g-o1))
			    (supergoal      (value (goal)))
			    (subgoals       (value ))
			    (goal-type      (value knowledge-reconciliation-goal.0))
			    (priority       (value eight.0))
			    (goal-value     (value (amount-value)))
			    (achieved       (value false.0))
			    (backptr (value (plan)))
			    (MXP (value =rc)))
			  (knowledge-differentiation-goal
			    (domain         (value person.0))
			    (co-domain      (value (distinguishable =g-o2
						     (domain (value =m))
						     (co-domain (value =m-prime)))))
			    (goal-actor     (value person.0))
			    (goal-object    (value =g-o2))
			    (supergoal      (value (goal)))
			    (subgoals       (value ))
			    (goal-type      (value knowledge-differentiation-goal.0))
			    (priority       (value seven.0))
			    (goal-value     (value (amount-value)))
			    (achieved       (value false.0))
			    (backptr (value (plan)))
			    (MXP (value =rc)))
			  )))
  )



;;;
;;; Just like Novel-Sit-Alt-Refuted, but with no initial anomaly.
;;;
(define-frame IMXP-NOVEL-SITUATION-ALTERNATIVE-REFUTED-NO-ANOMALY
	      (isa (value (composite-introspective-meta-xp)))
  (failure-cause (value (novel-situation.0
			  erroneous-association.0)))
  ;; ||||| For now this gets bound to the actor slot (somehow),
  ;; but in reality the question should be the relation:
  ;; (explanations
  ;;   (domain (actor (domain bark)
  ;; 	             (co-domain dog)))
  ;;   (co-domain (=a2))
  ;;   (status question)).
  (q (value (relation
;;; 		     (explanations (value (=a2))))))
	      )))
  ;; The actual explanation which answers the question.
  (a2 (value (xp)))
  ;; The node e is the hypothesis (expected answer to the question).
  (e (value (xp
	      (explains (value =q))
;;; 	      (truth (value hypothesized-in.0))
	      )))
  (e-prime (value (xp
;;; 		    (truth (value out.0))
	      )))
  ;; Will be bound to the index used to retrieve e.
  (i (value (index
	      (domain (value =q))
	      (co-domain (value (=m)))
	      (relation (value =q))
	      (memory-item (value (=m))))))
  (i-prime (value (index
		    (domain (value =q))
		    (co-domain (value =m-prime))
		    (relation (value =q))
		    (memory-item (value =m-prime)))))
  ;; The abstract xp retrieved with i to produce e.
  (m (value (xp)))
  (m-prime (value (entity
		 ;;If this is made out.0 and m-prime is unified with A2 (which will be in.0) the unification fails.
		    ;; (truth (value out.0))
		    )))
  (h-decision-basis
    (value
      (basis
	(knowledge
	  (value (collection
		   (members (value
			      ((knowledge-state
				 (co-domain (value =i))
				 (believed-item (value =i))))))))))))
  (q-decision-basis
    (value
      (basis
	(knowledge
	  (value (collection
		   (members (value
			      ((knowledge-state
				 (co-domain (value (characterization =charact)))
				 (believed-item (value =charact))))))))))))
  (rc (value (trace-meta-xp
	       (identification
		 (value =q-id))
	       (generation
		 (value =hypo-gen))
	       (examination
		 (value =verify-node)))))
  (not-equals (value (not-equal-relation
		       (domain (value =a2))
		       (co-domain (value =e)))))
  (ef (value (expectation-failure
	       (initiates- (value =not-equals))
	       (expected-outcome (value =e))
	       (actual-outcome (value =a2)))))
  (equals (value (equal-relation
		   (domain (value =a2))
		   (co-domain (value =e-prime)))))
  (rf (value (retrieval-failure
	       (initiates- (value =equals))
	       (expected-outcome (value =e-prime))
	       (actual-outcome (value =a2)))))
  (new-input (value (entity)))
  (nodes
    (value
      (=a2 =e =e-prime =i =m =m-prime =rc =ef =rf =new-input)))
  (link1 (value (mentally-initiates
		  (domain (value =i))
		  (co-domain (value =rc)))))
  (link2 (value (mentally-results
		  (domain (value =rc))
		  (co-domain (value =e)))))
  (link3 (value (mentally-results
		  (domain (value =new-input))
		  (co-domain (value =a2)))))
  (link4 (value (mentally-initiates
		  (domain (value =not-equals))
		  (co-domain (value =ef)))))
  (link5 (value (mentally-initiates
		  (domain (value =equals))
		  (co-domain (value =rf)))))
  (links (value (=link1 =link2 =link3 =link4 =link5)))
  ;; The goal to answer the question?
  (k-goal (value
	    (knowledge-acquisition-goal
	      (goal-object
		(value
		  (generate
		    (co-domain (value =q))))))))
  (h-decision (value (decision-process
		       (basis-of-decision (value =h-decision-basis)))))
  (hypo-gen (value (d-c-node
		     (initial-state      (value (considerations
						  ;; ||||| Should this be =satisfied-goal?
						  (prime-state (value =k-goal)))))
		     (strategy-choice    (value explanation.0))
		     (strategy-decision  (value =h-decision))
		     (strategy-execution (value explain.0))
		     (main-result (value (outcome
					   (members (value (=e))))))
		     )))
  (q-decision (value (decision-process
		       (basis-of-decision (value =q-decision-basis)))))
  (q-id (value (d-c-node
		 (strategy-choice    (value questioning.0))
		 (strategy-decision  (value =q-decision))
		 (strategy-execution (value pose-question.0))
		 (side-effect        (value (considerations
					      (prime-state (value =k-goal)))))
		 )))
  (verify-node (value (d-c-node
			(strategy-choice    (value compare.0))
			(strategy-execution (value comparison.0))
			(main-result (value (outcome
					      (members (value (=link4))))))
			)))
;;;   (input-context (value (state)))
;;;   (new-input (value (process)))	  ; This IMXP has 2 new-input slots. Removed [cox 30jun95]
;  (answers (value (relation
;		    (domain (value =right-xp))
;		    (co-domain (value =q)))))
  (pre-xp-nodes (value (=ef =a2
;;; 			    =e			; Cannot test for this since it is unverified [cox 22feb95]
			    )))
  (explains (value =ef))
  (internal-nodes (value (=q =hypo-gen =e-prime =rf)))
  ;; Very important for m and m-prime to be in the list of xp-asserted-nodes
  ;; since this allows reasoning questions to be posted on the introspective-meta-xp.
  ;; Does i belong here not that m is the item retrieved rather than the index.
  (xp-asserted-nodes
    (value
      (=not-equals =q-id =new-input =i =m =m-prime)))
;  (link3 (value (mentally-results
;		  (domain (value =new-input))
;		  (co-domain (value =satisfied-goal)))))
  (learning-algorithm (value (
			      (learning-process	           ; Handles Novel Situation.
				(learning-type (value generalization.0))
;;; 				(credit-blame (value (=a2 =self (literal m-prime))))
				(credit-blame (value (=a2 =m-prime)))
				)
			      (learning-process	; Further Handles Novel Situation.
				(learning-type (value conditionalization.0))
				(credit-blame (value (=m-prime)))
				)
;			      (learning-process	           ; Handles Mis-indexed Structure.
;				(learning-type (value specialization.0))
;				(credit-blame (value (=i =charact =m-prime)))
;				)
			      )))
  (potential-learning-goals (value (
			  (knowledge-differentiation-goal
			    (domain         (value person.0))
			    (co-domain      (value (distinguishable =g-o2
						     (domain (value =m))
						     (co-domain (value =m-prime)))))
			    (goal-actor     (value person.0))
			    (goal-object    (value =g-o2))
			    (supergoal      (value (goal)))
			    (subgoals       (value ))
			    (goal-type      (value knowledge-differentiation-goal.0))
			    (priority       (value seven.0))
			    (goal-value     (value (amount-value)))
			    (achieved       (value false.0))
			    (backptr (value (plan)))
			    (MXP (value =rc)))
			  )))
  )

(define-frame COMPOSITE-INTROSPECTIVE-META-XP
	      (isa (value (introspective-meta-xp)))
  )



(define-frame FAILURE-CAUSE-TYPE-VALUE
	      (isa (value (attribute-value)))
  )


(define-attribute-value INCORRECT-DOMAIN-KNOWLEDGE.0
			(isa (value (failure-cause-type-value)))
  )

(define-attribute-value NOVEL-SITUATION.0
			(isa (value (failure-cause-type-value)))
  )

(define-attribute-value MISSING-ASSOCIATION.0
			(isa (value (failure-cause-type-value)))
  )

(define-attribute-value ERRONEOUS-ASSOCIATION.0
			(isa (value (failure-cause-type-value)))
  )

(define-attribute-value MISSING-BEHAVIOR.0
			(isa (value (failure-cause-type-value)))
  )

(define-attribute-value FLAWED-BEHAVIOR.0
			(isa (value (failure-cause-type-value)))
  )

(define-attribute-value MISSING-HEURISTIC.0
			(isa (value (failure-cause-type-value)))
  )

(define-attribute-value FLAWED-HEURISTIC.0
			(isa (value (failure-cause-type-value)))
  )

(define-attribute-value MISSING-GOAL.0
			(isa (value (failure-cause-type-value)))
  )

(define-attribute-value POOR-GOAL.0
			(isa (value (failure-cause-type-value)))
  )

(define-attribute-value FORGOTTEN-GOAL.0
			(isa (value (failure-cause-type-value)))
  )

(define-attribute-value POOR-PRIORITY.0
			(isa (value (failure-cause-type-value)))
  )

(define-attribute-value MISSING-INPUT.0
			(isa (value (failure-cause-type-value)))
  )

(define-attribute-value NOISE.0
			(isa (value (failure-cause-type-value)))
  )



;;;
;;; This IMXP represents a kind of normal gap-filling, only the gap is an
;;; entire explanation instead of a filler of some larger structure. An
;;; explanation is missing, and the system acquires a new one from the input
;;; (or some expert).
;;; 
(define-frame IMXP-BAFFLED-AND-RESOLVED
  (isa (value (composite-introspective-meta-xp)))
  ;; ||||||The following slot is not really the failure type,
  ;; rather its the failure cause. The type would be
  ;; impasse. See forgetting paper Cogsci-94.
  (failure-cause (value (novel-situation.0 missing-association.0)))
  ;; The baffling question.
  (q (value (relation
;;; 	      (explanations (value (=a)))
	      )))
  ;; The actual explanation which answers the question.
  (a (value (xp
 	      (explains (value =q))
	      )))
  ;; E is not in the set of beliefs.
  (truth-value (value (truth
			(domain (value =e))
			(co-domain (value out.0))
			(initiates (value =rf)))))
  ;; The node e is the missing answer.
  (e (value (xp
	      (results- (value =hypo-gen))
	      (explains (value =q))
 	      (truth (value out.0)
		     (relation =truth-value)))))
  ;; Will be bound to the index used to retrieve e.
  (i (value (index
	      (domain (value =q))
	      (co-domain (value =m))
	      (relation (value =q))
	      (memory-item (value =m)))))
  ;; The abstract xp that should have been retrieved with i to produce e.
  (m (value (xp)))
  ;; The missing indexes.
  (h-decision-basis
    (value
      (basis
	(knowledge
	  (value (collection
		   (members (value
			      ((knowledge-state
				 (co-domain (value =i))
				 (believed-item (value =i))))))))))))
  (rc (value (trace-meta-xp
	       (identification
		 (value =q-id))
	       (generation 
		 (value =hypo-gen))
	       (link3 (value =link2))
	       (link4 (value (mentally-results
;;; 			       (truth (value out.0))  ;;Resolve this one soon. [26may94]
			       )))
	       )))
  (equals (value (equal-relation
		   (domain (value =a))
		   (co-domain (value =e)))))
  (rf (value (retrieval-failure
	       (initiates- (value =truth-value))
	       (expected-outcome (value =e))
	       (actual-outcome (value =a)))))
  (new-input (value (entity)))
  (nodes
    (value
      (=a =e =i =m =rc =rf =new-input =later-process =truth-value)))
  (link1 (value (mentally-results
		  (domain (value pose-question.0))
		  (co-domain (value (outcome
				      (members (value (=q)))))))
;		(mentally-initiates
;		  (domain (value =i))
;		  (co-domain (value =rc)))
		))
  (link2 (value (mentally-enables
		  (domain (value =con))
		  (co-domain (value =hypo-gen)))))
  (link3 (value (mentally-results
		  (domain (value =rc))
		  (co-domain (value =e)))))
  (link4 (value (mentally-initiates
		  (results- (value =hypo-gen))
		  (domain (value =truth-value))
		  (co-domain (value =rf)))))
  (link5 (value (mentally-enables
		  (domain (value =new-input))
		  (co-domain (value =later-process)))))
  (link6 (value (mentally-results
		  (domain (value =later-process))
		  (co-domain (value =a)))))
  (links (value (=link1 =link2 =link3 =link4 =link5 =link6)))
  ;; The goal to answer the question?
  (k-goal (value
	    (knowledge-acquisition-goal
	      (goal-object
		(value
		  (generate
		    (co-domain (value =q))))))))
  (h-decision (value (decision-process
		       (basis-of-decision (value =h-decision-basis)))))
  (hypo-gen (value (d-c-node
		     (enables- (value =q-id))
		     (initial-state      (value (considerations
						  ;; ||||| Should this be =satisfied-goal?
						  (prime-state (value =k-goal)))))
		     (strategy-choice    (value suspension.0))
		     (strategy-decision  (value =h-decision))
		     (strategy-execution (value suspend-task.0))
		     (side-effect        (value (considerations)))
		     (main-result        (value (outcome =o
							 (members (value (=link4))))))
		     (link4 (value (mentally-results
				     (co-domain (value =o))
;;; 				     (truth (value out.0))
				     )))
		     )))
  (q-decision-basis
    (value
      (basis
	(knowledge
	  (value (collection
		   (members (value
			      ((knowledge-state
				 (co-domain (value (characterization =charact)))
				 (believed-item (value =charact))))))))))))
  (q-decision (value (decision-process
		       (basis-of-decision (value =q-decision-basis)))))
  (q-id (value (d-c-node
		 (strategy-choice    (value questioning.0))
		 (strategy-decision  (value =q-decision))
		 (strategy-execution (value pose-question.0))
		 (side-effect        (value (considerations =con
							    (prime-state (value =k-goal)))))
		 (link4 (value =link1))
		 )))
;;;   (input-context (value (state)))
  (later-process (value (process)))
;  (answers (value (relation
;		    (domain (value =right-xp))
;		    (co-domain (value =q)))))
  (pre-xp-nodes (value (=a =e =rf)))
  (explains (value =rf))
  (internal-nodes (value (=q =hypo-gen =later-process =i)))
  (xp-asserted-nodes
    (value
      (=q-id =m =new-input)))
  (learning-algorithm (value ((learning-process	; Handles Supposed Novel Situation.
				(learning-type (value generalization.0))
				(credit-blame (value (=a =m)))
				)
			      (learning-process	; Handles Supposed Novel Situation.
				(learning-type (value conditionalization.0))
				(credit-blame (value (=m)))
				)
			      )))
  (potential-bugs (value (=a =i)))
  (potential-learning-goals (value ((knowledge-expansion-goal
				      (domain         (value person.0))
				      (co-domain      (value =a))
				      (goal-actor     (value person.0))
				      (goal-object    (value =a))
				      (supergoal      (value (goal)))
				      (subgoals       (value ))
				      (goal-type      (value knowledge-expansion-goal.0))
				      (priority       (value eight.0))
				      (goal-value     (value (amount-value)))
				      (achieved       (value false.0))
				      (backptr (value (plan)))
				      (MXP (value =rc)))
				    (knowledge-reorganization-goal
				      (domain         (value person.0))
				      (co-domain      (value =i))
				      (goal-actor     (value person.0))
				      (goal-object    (value =i))
				      (supergoal      (value (goal)))
				      (subgoals       (value ))
				      (goal-type      (value knowledge-reorganization-goal.0))
				      (priority       (value seven.0))
				      (goal-value     (value (amount-value)))
				      (achieved       (value false.0))
				      (backptr (value (plan)))
				      (MXP (value =rc)))
				    )))
  )




;;;
;;; This IMXP represents the case where an anomaly occurs which cannot be
;;; explained by the system because it does not have the sufficient background
;;; information with which to reason.
;;; 
(define-frame IMXP-ANOMALY-AND-BAFFLED
	      (isa (value (composite-introspective-meta-xp)))
  (failure-cause (value (incorrect-domain-knowledge.0
			  novel-situation.0
			  missing-association.0)))
  ;; The baffling question.
  (q (value (relation
;;; 	      (explanations (value (=a2)))
	      )))
  ;; The actual explanation which answers the question.
  ;; ||||||Should be a2 ? See what was dome w/IMXP-NOVEL-SIT-ALT-REFUTED
  (a2 (value (xp				; Made a2 and commented out explains [cox 28feb95]
;;;  	      (explains (value =q))
	      )))
  ;; E is not in the set of beliefs.
  (truth-value (value (truth
			(domain (value =e))
			(co-domain (value out.0))
			(initiates (value =rf)))))
  ;; The node e is the missing answer.
  (e (value (xp
	      (results- (value =hypo-gen))
	      (explains (value =q))
 	      (truth (value out.0)
		     (relation =truth-value)))))
  ;; Will be bound to the index used to retrieve e.
  (i (value (index
	      (domain (value =q))
	      (co-domain (value =m))
	      (relation (value =q))
	      (memory-item (value =m)))))
  ;; The abstract xp that should have been retrieved with i to produce e.
  (m (value (xp)))
  ;; Actual input (actual is relative; with noise repr. this will not be the "actual" true value.
  (a1 (value (entity)))
  (c (value (entity)))				; Constraint from conceptual def. in memory.
  ;; The missing indexes.
  (h-decision-basis
    (value
      (basis
	(knowledge
	  (value (collection
		   (members (value
			      ((knowledge-state
				 (co-domain (value =i))
				 (believed-item (value =i))))))))))))
  (rc (value (trace-meta-xp
	       (identification
		 (value =q-id))
	       (generation 
		 (value =hypo-gen))
	       (link3 (value =link2))
	       (link4 (value (mentally-results
;;; 			       (truth (value out.0))  ;;Resolve this one soon. [26may94]
			       )))
	       )))
  (equals (value (equal-relation
		   (domain (value =a2))
		   (co-domain (value =e)))))
  (rf (value (retrieval-failure
	       (initiates- (value =truth-value))
	       (expected-outcome (value =e))
	       (actual-outcome (value =a2)))))
  (new-input (value (entity)))
  (nodes
    (value
      (=a2 =e =i =m =rc =rf =new-input =later-process =truth-value)))
  (link1 (value (mentally-results
		  (domain (value pose-question.0))
		  (co-domain (value (outcome
				      (members (value (=q)))))))
;		(mentally-initiates
;		  (domain (value =i))
;		  (co-domain (value =rc)))
		))
  (link2 (value (mentally-enables
		  (domain (value =con))
		  (co-domain (value =hypo-gen)))))
  (link3 (value (mentally-results
		  (domain (value =rc))
		  (co-domain (value =e)))))
  (link4 (value (mentally-initiates
		  (result- (value =hypo-gen))	; ||||||??????
		  (domain (value =truth-value))
		  (co-domain (value =rf)))))
  (link5 (value (mentally-enables
		  (domain (value =new-input))
		  (co-domain (value =later-process)))))
  (link6 (value (mentally-results
		  (domain (value =later-process))
		  (co-domain (value =a2)))))
  (links (value (=link1 =link2 =link3 =link4 =link5 =link6)))
  ;; The goal to answer the question?
  (k-goal (value
	    (knowledge-acquisition-goal
	      (goal-object
		(value
		  (generate
		    (co-domain (value =q))))))))
  (h-decision (value (decision-process
		       (basis-of-decision (value =h-decision-basis)))))
  (hypo-gen (value (d-c-node
		     (enables- (value =q-id))
		     (initial-state      (value (considerations
						  ;; ||||| Should this be =satisfied-goal?
						  (prime-state (value =k-goal)))))
		     (strategy-choice    (value suspension.0))
		     (strategy-decision  (value =h-decision))
		     (strategy-execution (value suspend-task.0))
		     (side-effect        (value (considerations)))
		     (main-result        (value (outcome =o
							 (members (value (=link4))))))
		     (link4 (value (mentally-results
				     (co-domain (value =o))
;;; 				     (truth (value out.0))
				     )))
		     )))
  ;; Will be bound to the anomaly causing the question.
  ;; ||||| Should be the constraint.
  (anomaly (value (anomaly
		    (expected-outcome (value =c))
		    (actual-outcome (value =a1))
		    (action (value (mop =a-mop)))
		    (paths (value (literal =a-literal))))))
  (i-f (value (incorporation-failure
		(initiates- (value (state)))
		(computed-occurence (value =c))
		(actual-occurence (value =a1))
		(action (value =a-mop))
		(paths (value =a-literal)))))
  (q-decision-basis
    (value
      (basis
	(knowledge
	  (value (collection
		   (members (value
			      ((knowledge-state
				 (co-domain (value =anomaly))
				 ;; ||||| How to represent the path
				 ;; information to the anomaly?
				 (believed-item (value =anomaly))))))))))))
  (q-decision (value (decision-process
		       (basis-of-decision (value =q-decision-basis)))))
  (q-id (value (d-c-node
		 (strategy-choice    (value questioning.0))
		 (strategy-decision  (value =q-decision))
		 (strategy-execution (value pose-question.0))
		 (side-effect        (value (considerations =con
							    (prime-state (value =k-goal)))))
		 (link4 (value =link1))
		 )))
;;;   (input-context (value (state)))
  (later-process (value (process)))
;  (answers (value (relation
;		    (domain (value =right-xp))
;		    (co-domain (value =q)))))
  (pre-xp-nodes (value (=a2 =e =rf =anomaly)))
  (explains (value =rf))
  (internal-nodes (value (=q =hypo-gen =later-process =i)))
  (xp-asserted-nodes
    (value
      (=q-id =m =new-input)))
  (learning-algorithm (value ((learning-process	; Handles Incorrect BK.
				(learning-type (value abstraction.0))
				(credit-blame (value (=a2 =anomaly)))
				)
			      (learning-process	; Handles Supposed Novel Situation.
				(learning-type (value generalization.0))
				(credit-blame (value (=a2 =m)))
				)
			      ;; |||||| Why was the learning-process below commented out? [cox 11jun95]
			      (learning-process	; Handles Supposed Novel Situation.
				(learning-type (value conditionalization.0))
				(credit-blame (value (=m)))
				)
			      )))
  (potential-bugs (value (=a2 =i)))
  (potential-learning-goals (value ((knowledge-reconciliation-goal
				      (domain         (value person.0))
				      (co-domain      (value (merged =g-o1
								     (domain (value =a1))
								     (co-domain (value =c)))))
				      (goal-actor     (value person.0))
				      (goal-object    (value =g-o1))
				      (supergoal      (value (goal)))
				      (subgoals       (value ))
				      (goal-type      (value knowledge-reconciliation-goal.0))
				      (priority       (value eight.0))
				      (goal-value     (value (amount-value)))
				      (achieved       (value false.0))
				      (backptr (value (plan)))
				      (MXP (value =rc)))
				    (knowledge-expansion-goal
				      (domain         (value person.0))
				      (co-domain      (value =a2))
				      (goal-actor     (value person.0))
				      (goal-object    (value =a2))
				      (supergoal      (value (goal)))
				      (subgoals       (value ))
				      (goal-type      (value knowledge-expansion-goal.0))
				      (priority       (value eight.0))
				      (goal-value     (value (amount-value)))
				      (achieved       (value false.0))
				      (backptr (value (plan)))
				      (MXP (value =rc)))
				    (knowledge-reorganization-goal
				      (domain         (value person.0))
				      (co-domain      (value =i))
				      (goal-actor     (value person.0))
				      (goal-object    (value =i))
				      (supergoal      (value (goal)))
				      (subgoals       (value ))
				      (goal-type      (value knowledge-reorganization-goal.0))
				      (priority       (value seven.0))
				      (goal-value     (value (amount-value)))
				      (achieved       (value false.0))
				      (backptr (value (plan)))
				      (MXP (value =rc)))
				    )))
  )



;;;
;;; This IMXP represents the case where an anomaly is discovered, and the
;;; reasoner sucessfully explains it.
;;; 
(define-frame IMXP-ANOMALY-EXPLAINED
	      (isa (value (composite-introspective-meta-xp)))
  (failure-cause (value (incorrect-domain-knowledge.0)))
  ;; The question.
  (q (value (relation
;;; 	      (explanations (value (=a2)))
	      )))
  ;; The actual explanation which answers the question.
  (a2 (value (xp
;;;  	      (explains (value =q))
	      )))
  ;; E is in the set of beliefs.
  (truth-value (value (truth
			(domain (value =e))
			(co-domain (value in.0))
			(initiates (value =sp)))))
  ;; The node e is the answer.
  (e (value (xp
	      (results- (value =hypo-gen))
	      (explains (value =q))
 	      (truth (value in.0)
		     (relation =truth-value)))))
  ;; Will be bound to the index used to retrieve e.
  ;; ||||| Do we need this in the explanation?
  (i (value (index
	      (domain (value =q))
	      (co-domain (value =m))
	      (relation (value =q))
	      (memory-item (value =m)))))
  ;; The abstract xp that should have been retrieved with i to produce e.
  (m (value (xp)))
  ;; Actual input (actual is relative; with noise repr. this will not be the "actual" true value.
  (a1 (value (entity)))
  (c (value (entity)))				; Constraint from conceptual def. in memory.
  ;; The missing indexes.
  (h-decision-basis
    (value
      (basis
	(knowledge
	  (value (collection
		   (members (value
			      ((knowledge-state
				 (co-domain (value =i))
				 (believed-item (value =i))))))))))))
  (rc (value (trace-meta-xp
	       (identification
		 (value =q-id))
	       (generation 
		 (value =hypo-gen))
	       (link3 (value =link2))
	       (link4 (value (mentally-results
;;; 			       (truth (value out.0))  ;;Resolve this one soon. [26may94]
			       )))
	       )))
  (sp (value (successful-prediction
	       (initiates- (value =equals))
	       (expected-outcome (value =e))
	       (actual-outcome (value =a2)))))
  (equals (value (equal-relation
		   (domain (value =a2))
		   (co-domain (value =e)))))
  (rf (value (retrieval-failure
	       (initiates- (value =truth-value))
	       (expected-outcome (value =e))
	       (actual-outcome (value =a2)))))
  (new-input (value (entity)))
  (nodes
    (value
      (=a2 =e =i =m =rc =sp =new-input =later-process =truth-value)))
  (link1 (value (mentally-results
		  (domain (value pose-question.0))
		  (co-domain (value (outcome
				      (members (value (=q)))))))
;		(mentally-initiates
;		  (domain (value =i))
;		  (co-domain (value =rc)))
		))
  (link2 (value (mentally-enables
		  (domain (value =con))
		  (co-domain (value =hypo-gen)))))
  (link3 (value (mentally-results
		  (domain (value =rc))
		  (co-domain (value =e)))))
  (link4 (value (mentally-initiates
		  (result- (value =hypo-gen))	; ||||||??????
		  (domain (value =truth-value))
		  (co-domain (value =sp)))))
  (link5 (value (mentally-enables
		  (domain (value =new-input))
		  (co-domain (value =later-process)))))
  (link6 (value (mentally-results
		  (domain (value =later-process))
		  (co-domain (value =a2)))))
  (links (value (=link1 =link2 =link3 =link4 =link5 =link6)))
  ;; The goal to answer the question?
  (k-goal (value
	    (knowledge-acquisition-goal
	      (goal-object
		(value
		  (generate
		    (co-domain (value =q))))))))
  (h-decision (value (decision-process
		       (basis-of-decision (value =h-decision-basis)))))
  (hypo-gen (value (d-c-node
		     (enables- (value =q-id))
		     (initial-state      (value (considerations
						  ;; ||||| Should this be =satisfied-goal?
						  (prime-state (value =k-goal)))))
 		     (strategy-choice    (value explanation.0))
		     (strategy-decision  (value =h-decision))
		     (strategy-execution (value explain.0))
		     (side-effect        (value (considerations)))
		     (main-result        (value (outcome =o
							 (members (value (=e))))))
;;; 		     (link4 (value (mentally-results   ;commented out 14jun95]
;;; 				     (co-domain (value =o))
;;; 				     (truth (value out.0))
;;; 				     )))
		     )))
  ;; Will be bound to the anomaly causing the question.
  ;; ||||| Should be the constraint.
  (anomaly (value (anomaly
		    (expected-outcome (value =c))
		    (actual-outcome (value =a1))
		    (action (value (mop =a-mop)))
		    (paths (value (literal =a-literal))))))
  (i-f (value (incorporation-failure
		(initiates- (value (state)))
		(computed-occurence (value =c))
		(actual-occurence (value =a1))
		(action (value =a-mop))
		(paths (value =a-literal)))))
  (q-decision-basis
    (value
      (basis
	(knowledge
	  (value (collection
		   (members (value
			      ((knowledge-state
				 (co-domain (value =anomaly))
				 ;; ||||| How to represent the path
				 ;; information to the anomaly?
				 (believed-item (value =anomaly))))))))))))
  (q-decision (value (decision-process
		       (basis-of-decision (value =q-decision-basis)))))
  (q-id (value (d-c-node
		 (strategy-choice    (value questioning.0))
		 (strategy-decision  (value =q-decision))
		 (strategy-execution (value pose-question.0))
		 (side-effect        (value (considerations =con
							    (prime-state (value =k-goal)))))
		 (link4 (value =link1))
		 )))
;;;   (input-context (value (state)))
  (later-process (value (process)))
;  (answers (value (relation
;		    (domain (value =right-xp))
;		    (co-domain (value =q)))))
  (pre-xp-nodes (value (=a2 =e =sp =i-f =anomaly)))
  (explains (value =sp))			; was =i-f
  (internal-nodes (value (=q =hypo-gen =later-process =i)))
  (xp-asserted-nodes
    (value
      (=q-id =m =new-input)))
  (learning-algorithm (value ((learning-process	; Handles Incorrect BK.
				(learning-type (value abstraction.0))
				(credit-blame (value (=a2 =anomaly)))
				)
			      )))
  (potential-bugs (value (=a2)))
  (potential-learning-goals (value ((knowledge-reconciliation-goal
				      (domain         (value person.0))
				      (co-domain      (value (merged =g-o1
								     (domain (value =a1))
								     (co-domain (value =c)))))
				      (goal-actor     (value person.0))
				      (goal-object    (value =g-o1))
				      (supergoal      (value (goal)))
				      (subgoals       (value ))
				      (goal-type      (value knowledge-reconciliation-goal.0))
				      (priority       (value eight.0))
				      (goal-value     (value (amount-value)))
				      (achieved       (value false.0))
				      (backptr (value (plan)))
				      (MXP (value =rc)))
				    )))
  )




(define-frame XP-VIEW
  (isa                (value (meta-xp)))
  (XPs                (value (=main-xp)))
  (main-xp            (value (XP-DISABLE-ACT-PRECONDITION)))
  (learning-algorithm (value (modify-isa-hiearchy)))
  ; This represents the goal-scene of the detection method.
  ; We must be able to recover from the modifications?
  ; So do we also need the old back-ptr list before the unify?
  (prev-state         (value (mop)))
  )


(define-frame MODIFY-ISA-HIEARCHY
	      (isa (value (entity)))
  )



;;;
;;; Characterizations are used to categorize inputs with respect to
;;; interestingness (see function interesting-p). The input concept
;;; may be characterized as violent, sexy, loud, or personally relevant.
;;; 
(define-relation CHARACTERIZATION
  (isa (value (relation)))
  (domain (value (mop)))
  (co-domain (value (mop-type)))
  )



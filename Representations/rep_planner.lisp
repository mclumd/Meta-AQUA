;;; -*- Mode: LISP; Syntax: Common-lisp; Package: Representations; Base: 10 -*-

(in-package :reps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	      Meta-AQUA Background Knowledge Represented as Frames
;;;;
;;;;	    Copyright (C) 1996   Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;                     File: rep_planner.lisp
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
;;;; Most of the representations, in this file are used by
;;;; Meta-AQUA in problem-solving mode (mode='act-out-story).
;;;; The goal representations are an exceptation.
;;;; 
;;;; What kind of backpointers will be necessary to link goals, plans,
;;;; subgoals, and subplans?
;;;;
;;;; What was the slot I thought of at breakfast. It had to do with
;;;; maintaining anonymity and the action used to carry it out? Was it
;;;; secondary goal slot?  Subplan slot?
;;;;
;;;;
;;;; We need to define goal types:
;;;;
;;;; World Goals
;;;;  1. States an agent wants to achieve.
;;;;  2. States an agent wants to prevent.
;;;;  3. States an agent wants to maintain.
;;;;  4. Do we need Adjunct Goals? NO. Make the arg.
;;;;
;;;; Knowledge Goals
;;;;  1. Knowledge Acquisition
;;;;  2. Knowledge Reorganization
;;;;
;;;; Note that the Knowledge Goals may apply to structures in the
;;;; Reasoning Model as well as the World Model.
;;;;
;;;; Also need a goal-memory or goal-list. This would store the current
;;;; goals of the system so that goal interaction can be analysed.  Goals
;;;; need backpointers to what plans are trying to achieve them. If a plan
;;;; is then rejected, the system can delete goals it generated, but not
;;;; those which have another plan that is dependent of the goal too.
;;;; This is necessitated by the presence of goal overlap.
;;;;
;;;; ||||| Do we need to have a slot to show that when they become
;;;; achieved instead of being worked towards?
;;;;

;;; Instead of the Knowledge Goals listed above, the following is the current
;;; taxonomy of Knowledge or Learning Goals (see AAAI GDL Symposium, Cox & Ram,
;;; 1994):
;;; 
;;;  Learning Goals [27nov93]
;;; 
;;;     --- Unary Goals ---
;;;  1. Knowledge Acquisition Goal  - seeks answer to question or fill a gap.
;;;  2. Knowledge Refinement Goal   - seeks to specialize an interpretation of
;;;                                   an object.
;;;  3. Knowledge Expansion Goal    - seeks to broaden an interpretation of
;;;                                   an object.
;;;  4. Knowledge Organization Goal - reindex an object with respect to its
;;;                                   context.
;;;     --- Binary Goals ---
;;;  5. Knowledge Differentiation Goal - conceptually separate two objects.
;;;  6. Knowledge Reconciliation Goal  - conceptually merge two objects.
;;;  7. Knowledge Organization Goal    - reindex two objects with respect to
;;;                                      each other.

; |||||
; Should be actor and object so we can treat arbitrary frame uniformly?
; Or should we be careful to distinguish  between actors of actions and an "actor" slot of 
; what is essentially another slot?
;;;
;;; NOTE that currently subgoals is empty since there is conflict telling
;;; the dummy goal (if we specify ((goal)) as filler here) from a real goal in
;;; calls of function spawn-sub-goal that try to add a subgoal to this list filler.
;;; 
(define-frame GOAL
    (isa            (value (mental-state)))
    (domain         (value (volitional-agent)))  ; Inherited from MENTAL-STATE.
    (co-domain      (value (relation)))
    (goal-actor     (value =domain))             ; Useful synonym.
    (goal-object    (value =co-domain))          ; Useful synonym.
    (supergoal      (value (goal)))		 ; Links to goal hierarchy.
    (subgoals       (value ))		         ; Will be a list of goals in hierarchy.
    (goal-type      (value (goal-type-value)))   ; Taxonomic tag.
    (priority       (value (integer-value)))     ; Priorities vary, goal-values are constant?
    (goal-value     (value (amount-value)))      ; Crude measure of absolute usefulness.
    (achieved       (value (boolean-value)))	 ; Goals begin as unachieved (false.0),
                                                 ; becomes true.0 when satisfied.
    (backptr (value (plan)))                     ; Back pointer to the plan 
                                                 ; which spawned this goal. If was created
                                                 ; for another reason will be nil
    ;; I suppose that I was thinking of goals to understand events in the world as having 
    ;; nil backpointers, however what is the counterpart to understanding tasks?
    (MXP (value (trace-meta-xp)))	        ; Points to the Trace-Meta-XP to which this goal is
                                                ; associated with.
						; When the goals are pursued, we can find the place in the
  			                        ; chain where we are to pick up.	
)


;;; 
;;; ||||| isa attribute-value-type?
;;;
;;; |||||| Perhaps should be changed so that it distinguishes unary from
;;; non-unary learning goals.
;;; 
(define-frame GOAL-TYPE-VALUE
    (isa            (value (entity)))
)


(define-frame WORLD-GOAL
    (isa            (value (goal)))
)


;;; 
;;; In order to reject plans (if simulation rejects them; how else are they
;;; rejected?) the system posts the goal to prevent it. A meta-plan will then 
;;; find and delete such. Is this a good idea, or is it too indirect?
;;; 
(define-frame KNOWLEDGE-GOAL
    (isa            (value (goal)))
)


(define-frame KNOWLEDGE-ACQUISITION-GOAL
    (isa            (value (knowledge-goal goal-type-value)))
)


(define-frame KNOWLEDGE-REFINEMENT-GOAL
    (isa            (value (knowledge-goal goal-type-value)))
)


(define-frame KNOWLEDGE-EXPANSION-GOAL
    (isa            (value (knowledge-goal goal-type-value)))
    (domain         (value (volitional-agent)))
    (co-domain      (value (entity)))
    (goal-actor     (value =domain))
    (goal-object    (value =co-domain))
    (supergoal      (value (goal)))
    (subgoals       (value ))
    (goal-type      (value knowledge-expansion-goal.0))
    (priority       (value (integer-value)))
    (goal-value     (value (amount-value)))
    (achieved       (value (boolean-value)))
    (backptr (value (plan)))
    (MXP (value (trace-meta-xp)))
)


;;;
;;; NOTE that if the co-domain is nil or *nil*, then the system is reindexing
;;; with respect to nothing. That is, indexing a single concepts, rather than
;;; indexing a concept with respect to another concept.
;;; 
(define-frame INDEX-WITH-RESPECT-TO
    (isa            (value (relation)))
  )

;;; This must be changed to knowledge-organization goal to be consistent with
;;; papers, but will this have side-effect? [28nov93]
;;; 
(define-frame KNOWLEDGE-REORGANIZATION-GOAL
    (isa            (value (knowledge-goal goal-type-value)))
    (domain         (value (volitional-agent)))
    (co-domain      (value (reindex-with-respect-to
			     (domain (value (entity))))))
    (goal-actor     (value =domain))
    (goal-object    (value =co-domain))
    (supergoal      (value (goal)))
    (subgoals       (value ))
    (goal-type      (value knowledge-reorganization-goal.0))
    (priority       (value (integer-value)))
    (goal-value     (value (amount-value)))
    (achieved       (value (boolean-value)))
    (backptr (value (plan)))
    (MXP (value (trace-meta-xp)))
)


(define-frame REINDEX-WITH-RESPECT-TO
	      (isa (value (relation)))
  )



(define-frame DISTINGUISHABLE
    (isa            (value (relation)))
  )

(define-frame KNOWLEDGE-DIFFERENTIATION-GOAL
    (isa            (value (knowledge-goal goal-type-value)))
    (domain         (value (volitional-agent)))
    (co-domain      (value (distinguishable
			     (domain (value (entity)))
			     (co-domain (value (entity))))))
    (goal-actor     (value =domain))
    (goal-object    (value =co-domain))
    (supergoal      (value (goal)))
    (subgoals       (value ))
    (goal-type      (value knowledge-differentiation-goal.0))
    (priority       (value (integer-value)))
    (goal-value     (value (amount-value)))
    (achieved       (value (boolean-value)))
    (backptr (value (plan)))
    (MXP (value (trace-meta-xp)))
)


(define-frame MERGED
    (isa            (value (relation)))
  )

(define-frame KNOWLEDGE-RECONCILIATION-GOAL
    (isa            (value (knowledge-goal goal-type-value)))
    (domain         (value (volitional-agent)))
    (co-domain      (value (merged
			     (domain (value (entity)))
			     (co-domain (value (entity))))))
    (goal-actor     (value =domain))
    (goal-object    (value =co-domain))
    (supergoal      (value (goal)))
    (subgoals       (value ))
    (goal-type      (value knowledge-reconciliation-goal.0))
    (priority       (value (integer-value)))
    (goal-value     (value (amount-value)))
    (achieved       (value (boolean-value)))
    (backptr (value (plan)))
    (MXP (value (trace-meta-xp)))
)



(define-attribute-value   KNOWLEDGE-ACQUISITION-GOAL.0
    (isa            (value (goal-type-value knowledge-acquisition-goal)))
    )

(define-frame KNOWLEDGE-REFINEMENT-GOAL.0
    (isa            (value (goal-type-value knowledge-refinement-goal )))
)


(define-frame KNOWLEDGE-EXPANSION-GOAL.0
    (isa            (value (goal-type-value knowledge-expansion-goal)))
)

(define-attribute-value   KNOWLEDGE-REORGANIZATION-GOAL.0
    (isa            (value (goal-type-value knowledge-reorganization-goal)))
    )

(define-frame KNOWLEDGE-DIFFERENTIATION-GOAL.0
    (isa            (value (goal-type-value knowledge-differentiation-goal)))
)


(define-frame KNOWLEDGE-RECONCILIATION-GOAL.0
    (isa            (value (goal-type-value knowledge-reconciliation-goal)))
)



;;; 
;;; ||||| Why are these isa goal-type-value?
;;; 
(define-frame ACHIEVEMENT-GOAL
    (isa            (value (world-goal goal-type-value)))
)


(define-frame MAINTENANCE-GOAL
    (isa            (value (world-goal goal-type-value)))
)


;;; ||||| How is this goal-type business going to work? We have a prevent.0 in
;;; the type slot. This one was taken from Ashwin's definitions I think.  What
;;; syntax do we want?
;;;
;;; I changed prevent.0 to prevention-goal0 so it is at least defined. I still
;;; need to decide what to do here though. [29oct94] 
;;; 
(define-frame PREVENTION-GOAL
    (isa            (value (world-goal goal-type-value)))
    (goal-type      (value prevention-goal.0))
)


(define-attribute-value  ACHIEVEMENT-GOAL.0
    (isa            (value (goal-type-value achievement-goal)))
    )


(define-attribute-value  MAINTENANCE-GOAL.0
    (isa            (value (goal-type-value maintenance-goal)))
    )


(define-attribute-value  PREVENTION-GOAL.0
    (isa            (value (goal-type-value prevention-goal)))
    )






(define-frame PROBLEM-IDENTIFICATION-CHOICE-VALUE
    (isa (value (strategy-choice-value)))
  )


(define-attribute-value FORMULATION.0
    (isa (value (problem-identification-choice-value)))
  )

(define-attribute-value GTM.0
    (isa (value (problem-identification-choice-value)))
  )



(define-frame PLAN-GENERATION-CHOICE-VALUE
    (isa (value (strategy-choice-value)))
  )


(define-attribute-value MBR.0
    (isa (value (plan-generation-choice-value)))
  )

(define-attribute-value MEA.0
    (isa (value (plan-generation-choice-value)))
  )



(define-frame PLAN-EVALUATION-CHOICE-VALUE
    (isa (value (strategy-choice-value)))
  )


(define-attribute-value EXECUTION.0
    (isa (value (plan-evaluation-choice-value)))
  )

(define-attribute-value REPLANNING.0
    (isa (value (plan-evaluation-choice-value)))
  )


;;; 
;;; The frames that represent the processes to implement the
;;; above choices.
;;; 
(define-attribute-value FORMULATE-PROBLEM.0
    (isa (value (mental-process)))
  )

(define-attribute-value GO-THRU-MOTION.0
    (isa (value (mental-process)))
  )

(define-attribute-value DO-MBR.0
    (isa (value (mental-process)))
  )

(define-attribute-value DO-MEA.0
    (isa (value (mental-process)))
  )

(define-attribute-value EXECUTE.0
    (isa (value (mental-process)))
  )

(define-attribute-value REPLAN.0
    (isa (value (mental-process)))
  )




;;; 
;;; ||||| Need FROM and TO (ENTITY) or (LOCATION)
;;; for EVENTs  (or even PROCESSes)?
;;; 
(define-frame EVENT
  (isa                (value (process)))
  (actor              (value (volitional-agent)))
  (preconditions      (value (state)))
  (instrumental-scene (value (event)))
  (goal-scene         (value (event)))
  (scenes             (value (=instrumental-scene =goal-scene)))
  (side-effect+       (value (state)))
  (side-effect-       (value (state)))
  (main-result        (value (state)))
)


;;; 
;;; Note that because a plan isa EVENT it also shares the actor, precondition, 
;;; etc. slots.
;;; Removed main-goal since plans do not have goals, agents do.
;;; Plans have results which can be the indexes to plans to achieve
;;; a given goal. 
;;; 
;;; ||||| Is the adjunct-plans slot necessary. They might be tracked through
;;; gen-goals, eg. a given plan may spawn the maintain-anonimity goal to 
;;; increase the chances of success.
;;; ;; ||||| Do we need good-state and bad-state slots? These would represent key 
;;; states which the planner must keep in mind. 
;;; 
;;; How to associate what we planned and what actually happened?
;;; Likewise how to associate a question's expected answer with its actual
;;; answer? USE the Trace-Meta-XP!!!!!!! There is expected-solution and
;;; expected-answer slots.
;;; 
(define-frame PLAN
  (isa (value (event)))          ; Plan shares all slots in event.
  (cost (value (integer-value))) ; Crude measure of resource cost.
  (gen-goals (value (goal)))     ; A list of goals generated by this plan.
  (adjunct-plans (value (plan))) ; A list of plans that do not achieve the goal
                                 ; this plan is meant to achieve, but instead 
                                 ; support the plan. Eg maintain-anonimity.
  (subplans (value (plan)))      ; Children.
  (super-plans (value (plan)))   ; Parents.
  (success (value (boolean-value))); For recording an actual plan: did it work.
  (contingencies (value (plan))) ; Based on possible or predicted outcomes 
				 ; of yet to be executed events.
)


;;;
;;; |||||| Was never defined. Decide what to do with it when continuing the
;;; problem solving version of Meta-AQUA (ripsau). [cox 26feb95]
;;; 
(define-frame META-PLAN
	      (isa (value (entity)))
  )

;;; 
;;; ||||| This is not really a META-plan, is it?
;;;
;;; |||||Also I do not know if meta-plan is defined. 29 May 93
;;; 
(define-frame ANTI-PLAN
  (isa (value (meta-plan)))
  (opponent (value (volitional-agent)))
  (good-state (value (state))) ; ?
  (bad-state (value (state)))  ; ?
  (conflicting-goal (value (goal)))
)

;;; The following six frame definitions correspond to the alternatives in
;;; the cases statements of functions dispatch-world-goal and
;;; dispatch-knowledge-goal. That is, to the frame-type of the goal-object
;;; of the goal passed to these routines.

;;; 
;;; Wants is the analog to understands.
;;; Wants is interpreted in this program as a
;;; world goal.
;;; 
(define-frame WANTS
  (isa (value (relation)))
  (domain (value (volitional-agent)))
  (co-domain (value (entity))))


(define-frame UNDERSTANDS
  (isa (value (relation)))
  (domain (value (volitional-agent)))
  (co-domain (value (entity))))


(define-frame ID
  (isa (value (relation)))
  (domain (value (volitional-agent)))
  (co-domain (value (entity))))


(define-frame GENERATE
  (isa (value (relation)))
  (domain (value (volitional-agent)))
  (co-domain (value (entity))))


(define-frame TEST
  (isa (value (relation)))
  (domain (value (volitional-agent)))
  (co-domain (value (entity))))


(define-frame REVIEW/LEARN
  (isa (value (relation)))
  (domain (value (volitional-agent)))
  (co-domain (value (entity))))



;;; 
;;; Iteration: Act continues until test state is achieved.
;;; Test is not necessarily a goal, since loop may be
;;; function of thermostat where test is temperature threshhold.
;;; 
(define-frame LOOP
  (isa (value (process)))
  (act (value (event)))
  (test (value (state)))
  )



;;; 
;;; Should there be a larger scope to this strategy, and should there be
;;; narrower pieces that compose it? For example should we include a goto
;;; where object is expected to be/arrive as an instrumental scene with
;;; something like this as a goal-scene? Should there be a detection-method 
;;; like look-out-for which is a smaller part of the  goal scene? I think 
;;; the answers are affirmative. But what level of detail is necessary for 
;;; the project. It would be nice to have a new detection-method to round
;;; out the ones available to the authorities. There is also the variant
;;; whereby one moves while one is scanning for an object, instead of
;;; simply waiting for it to come to you. This would be a strategy of the
;;; authorities.
;;; 
;;; What is the difference between plans and conceptual structures like 
;;; detection-methods. Plans have contingencies and are templates for performing	
;;; actions. These actions are not yet implemented though. Detection-methods
;;; are both general descriptions of the actions which compose plans and 
;;; memories of actual events. Also in trying to understand what another
;;; agent is ging to do, we may not represent his contingencies explicitly since
;;; they are beyond our inspection. Do we infer them though?
;;; 
;;; Note that this representation does not have a contingency for what to do if
;;; the object does not arrive before a reasonable time. This is beyond the
;;; details and scope we wish to pursue. 
;;; 
;;; Should the selection of such a plan cause the generation of a goal like 
;;; maintaining one's position until the plan succeeds, or is the goal generated
;;; separately? It might be nice to have the goal generated by the plan.
;;; 
(define-frame WAIT-FOR-PLAN
  (isa (value (plan)))
  (actor (value (volitional-agent
		 (at-location (value =position)))))
  (object (value (physical-object)))
  ;; The preconditions are that the actor be at the location the object is 
  ;; suppose to arrive at, and that the actor can recognize the object when 
  ;; it comes.
  (preconditions (value ((at-location
			  (domain (value =actor))
			  (co-domain (value =position)))
			 (knowledge-state
			  (domain (value =actor))
			  (believed-item
			    (value (shape
				     (domain =object))))))))
  ;; ||||| I am not sure that both location-of slots are right in this schema.
  (position (value (physical-location
		     (location-of (value =actor)))))
  (expected-location (value (near
			      (domain (value =object))
			      (co-domain (value =actor))
			      (location-of (value =object)))))
  ;; The test condition is some state, thus is a relation 
  ;; with a domain and co-domain.
  (condition (value (at-location
		     (domain (value =object))
		     (co-domain (value =expected-location)))))
  (main-result (value (knowledge-state
		       (domain (value =actor))
		       (believed-item
			 (value =condition)))))
  (instrumental-scene (value (loop
			      (act 
			       (value (attend
				       (actor (value =actor))
				       (object (value (eyes)))
				       (to (value =expected-location)))))
			      (test 
			       (value =condition)))))
  ;; Should the goal-scene itself really be a detection mop?
  (goal-scene (value (mtrans
		      (actor (value =actor))
		      (m-object (value =main-result))
		      (instrument (value =instrumental-scene)))))

  (scenes (value (=instrumental-scene =goal-scene)))
  (success (value (boolean-value))); For recording an actual plan: did it work.
  (contingencies (value (watchful-eye-plan))) 
  )



(define-frame WATCHFUL-EYE-PLAN
	      (isa (value (plan)))
  )


;(define-frame SMUGGLING-PLAN
;  (actor              (value (criminal-volitional-agent)))
;  (object             (value (contraband)))
;  (preconditions      (value (possess
;			       (domain (value =actor))
;			       (co-domain =object))))
;  (instrumental-scene (value (pick-up)))
;  (goal-scene         (value (smuggling-act)))
;  (scenes             (value (=instrumental-scene =goal-scene)))
;  (side-effect+       (value (state)))
;  (side-effect-       (value (state)))
;  (main-result        (value (state)))
;  (cost (value (integer-value)))       
;  (gen-goals (value (goal)))     
;  (adjunct-plans (value (keep-low-profile-plan)))
;  (subplans (value (plan)))      
;  (super-plans (value (plan)))   
;  (contingencies (value (look-out-for-plan wait-for-plan))) 
;)



;;; Changed from physical-object. [mcox 31oct06]
(define-frame VEHICLE
  (isa (value (inanimate-object)))
  )


(define-frame PLANE
  (isa (value (vehicle)))
  )





(define-frame DISEMBARK-PLANE
  (isa (value (mop)))
  (actor (value (volitional-agent)))
  ;; ||||| Again this should not be an instance with 0 suffix, will fix later.
  (object (value plane))
  (preconditions (value (speed
			  (domain (value =object))
			  (co-domain (value zero.0)))))
  (main-result (value (outside
			(domain (value =actor))
			(co-domain (value =object)))))
  (goal-scene (value (mtrans
		       (actor (value =actor))
		       (object (value =actor))
		       (from (value (inside
				      (domain (value =actor))
				      (co-domain (value =object)))))
		       (to (value =main-result)))))
  (scenes (value (=goal-scene)))
  (success (value (boolean-value))) ; For recording whether the action met expectations.
  )


;;; |||||| This is definitely NOT right. 19oct94
(define-attribute-value LUGGAGE-AREA.0
  (isa (value (location)))
  )


(define-frame GOTO-LUGGAGE-AREA
  (isa (value (mop)))
  (actor (value (volitional-agent)))
  ;; |||||| Again this should not be an instance with 0 suffix, will fix later.
  (object (value luggage-area.0))
  (preconditions (value (knowledge-state
			  (domain (value =actor))
			  (believed-item (value (at-location 
						  (co-domain 
						    (value =object))
						  ))))))
  (main-result (value (at-location
			(domain (value =actor))
			(co-domain (value =object)))))
  (goal-scene (value (mtrans
		       (actor (value =actor))
		       (object (value =actor))
		       (from (value (inside
				      (domain (value =actor))
				      (co-domain (value =object)))))
		       (to (value (at-location
				    (domain (value =actor))
				    (co-domain (value =object))))))))
  (scenes (value (=goal-scene)))
  (success (value (boolean-value))) ; For recording whether the action met expectations.
  )



(define-frame PICK-UP
  (isa (value (mop)))
  (actor (value (volitional-agent)))
  (object (value (physical-object)))
  (preconditions (value (near
			  (domain (value =object))
			  (co-domain (value =actor)))))
  (goal-scene (value (atrans
		       (actor (value =actor))
		       (object (value =object))
		       (to (value =main-result)))))
  (main-result (value (possess
			(domain (value =actor))
			(co-domain (value =object)))))
  (scenes (value (=goal-scene)))
  (success (value (boolean-value))); For recording whether the action met expectations.
  )



;;; 
;;; Another idea: should the plans generate the goals of executing the actions, 
;;; thereby controlling execution, or do we allow higher processes to decide
;;; when to execute the action.
;;; 
;;; ||||| NOTE that we are using indirect variables for =sub1.
;;; Need to fix the frame instantiation routine.
;;; 
(define-frame SMUGGLING-PLAN
  (isa (value (plan)))
  (actor              (value (criminal-volitional-agent)))
  (object             (value (contraband)))
  (destination        (value (nation)))
  (preconditions      (value (possess
			       (domain (value =actor))
			       (co-domain (value =object)))))
  (scene1             (value (disembark-plane
			      (actor (value =actor)))))
  (scene2             (value (goto-luggage-area
			      (actor (value =actor))
			      (assoc (value =sub1)))))
  (instrumental-scene (value (pick-up
			      (actor (value =actor))
			      (object (value =object))
			      (main-result (value =preconditions)))))
  (goal-scene         (value (smuggling-act
			      (actor (value =actor))
			      (object (value =object))
			      (main-result (value =main-result)))))
  (scenes             (value (=scene1 =scene2 =instrumental-scene =goal-scene)))
  (side-effect+       (value (state)))
  ;; Remove the keep low profile goal and find luggage?
  (side-effect-       (value (		; =kgoal 18Dec91: I commented this binding out today
			        ;; because the goal had been commented out some time ago. Why was kgoal
			      ;; commented out along with subplans and adjunct plans? 
			       =mgoal))) 
  (main-result    (value (at-location 
			   (domain (value =object))
			   (co-domain (value =destination)))))
  (cost (value low.0))   ; Cost of the plan is low as far as effort goes, 
			 ; but the risk is high. How to represent?
  (gen-goals (value (
		     ;; |||||Uncomment the appearance goal later. 23 May 93.
;		     (goal =mgoal
;		       (goal-actor (value =actor))
;		       (goal-object (value (appearance	;|||||This relation is not defined. 23 May 93
;					     (domain (value =actor)) 
;					     (co-domain (value (anonymous))))))
;		       (priority (value six.0))
;		       (goal-value (value high.0))
;		       (goal-type (value (maintenance-goal)))
;		       (backptr (value nil.0)))
;		     (goal =kgoal
;		       (goal-actor (value =actor))
;		       (goal-object (value (knowledge-state =kstate
;					     (domain (value =actor))
;					     (believed-item 
;					      ;should facet of believed-item
;					      ;be question instead of value?
;					       (value (at-location 
;							(domain 
;							  (value =object))
;							)))
;					     )))
;		       (priority (value six.0))
;		       (goal-value (value med-high.0))
;		       (goal-type (value knowledge-acquisition-goal.0)))
		    )))
  (adjunct-plans (value (plan)))
;;;   (adjunct-plans (value (keep-low-profile-plan)))
  ;; ||||| Should the following subplan be generated dynamically?
  (subplans (value (plan)))
;  (subplans (value (find-luggage-area =sub1
;		     (main-result (value =kstate))))
;	    (assoc =scene1))
  ;; ||||| NOTE that this must be nil.0 or the slot is not instantiated.
  (super-plans (value nil.0)) 
  (contingencies (value (plan)))
;;; 		   (look-out-for-plan)))
)



;;;
;;; Frame bust-plan is a slightly modified version of bust-act
;;; (see file rep_smuggle4.lisp).
;;; 
(define-frame BUST-PLAN
  (isa (value (plan))) ; See murder.
  (actor (value (authority)))
  (planner (value (authority-group)))
  (object (value (criminal-volitional-agent)))
  (illegal-object (value (contraband)))
  ;; The authorities control the smuggled contraband as a result of the interdiction.
  (good-state (value (controls
		       (domain (value =actor))
		       (co-domain (value =illegal-object)))))
  ;; Lack of freedom.
  (main-result 
    (value (controls
	    (domain (value =actor))
	    (co-domain (value =object)))))
  (cost (value med-high.0))   ; Cost of the plan is low as far as effort goes, 
			 ; but the risk is high. How to represent?
  (side-effect (value =good-state))
  ;; Is this really instrumental? Perhaps it is enabling.
  ;; Does there exist a distinction between physical causation 
  ;; and legal enabling in MOP theory?
  ;; The main-result of the Interdiction-act is control of the contraband,
  ;; which is not the precondition of the arrest.
  (instrumental-scene (value (interdiction-act
                        (actor (value =actor))
                        (object (value =illegal-object))
			(opponent (value =object))
			(main-result (value =good-state))
			(instrumental-scene  (value (detection =i1)))
			(goal-scene (value (confiscation =i2))))))
  (goal-scene 
   (value (ARREST
	    (actor (value =actor))
	    (object (value =object))
	    (charge (value (crime)))
	    (preconditions (value =good-state))
	    (main-result (value =main-result))
	    )))
  (scenes (value (=i1 =i2 =goal-scene)))
  ;; ||||| NOTE that this must be nil.0 or the slot is not instantiated.
  (super-plans (value nil.0)) 
  (contingencies (value (plan)))
  )

;;;
;;; See comments on *World-Model* for the usefulness 
;;; of this frame type.
;;; 
(define-frame MODEL
  (isa (value (entity)))
  (frame-list (value nil.0))
  )

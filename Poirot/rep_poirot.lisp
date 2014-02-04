;;; -*- Mode: LISP; Syntax: Common-lisp; Package: Meta-aqua; Base: 10 -*-

(in-package :meta-aqua)

;;; 
;;; THIS FILE IS NOW OBSOLETE. 
;;; 
;;; THE CURRENT CODE IS REP_POIROT.LISP IN META-AQUA PROPER (SEE
;;; REPRESENTATIONS DIR).
;;; 



;;;; 
;;;; This file defines the Meta-AQUA frame representations to support
;;;; interpretation and explanation of the POIROT trace.
;;;; 

;;;(in-package :reps) Eventually fold into Representation package

;;; The following should eventually go into the Meta-AQUA represenations
;;; proper. Although an abstract-object was defined similar to physical-object,
;;; no abstract-state paralleled physical-state. The following defs fill this
;;; gap.

(define-relation ABSTRACT-STATE
    (isa            (value (state)))
    (domain         (value (abstract-object)))
    (co-domain      (value (abstract-state-value)))
    (slot           (value (abstract-state)))
    )


(define-frame ABSTRACT-STATE-VALUE
    (isa            (value (state-value)))
  )


(define-frame ABSTRACT-CONTROL-STATE           ;; domain has ownership of co-domain.
    (isa            (value (abstract-state)))
  )


;;; 
;;; Now the reason we need abstract-state-values.
;;; 
;;; A triage-code is an abstract-state relationship between the RequirementRec
;;; abstract-object and a triage-value such as URGENT.0. Thus the frame
;;; definition for RequirementRec has a slot called triage-code. See below.
;;; 

(define-frame triage-value
    (isa (value (abstract-state-value)))
  )

(define-attribute-value PRIORITY.0
    (isa (value (triage-value)))
  )
(setf priority 'PRIORITY.0)


(define-attribute-value URGENT.0
    (isa (value (triage-value)))
  )
(setf urgent 'URGENT.0)


(define-attribute-value ROUTINE.0
    (isa (value (triage-value)))
  )
(setf routine 'ROUTINE.0)




;;; 
;;; New relations
;;; 

(define-relation DISTANCE-FROM
    (isa     (value (spatial-relation)))
  (domain    (value (physical-object)))
  (co-domain (value (integer-value)))
  )

(define-relation IS-RESERVED
    (isa       (value (unary-relation)))
  (domain    (value (volitional-agent)))
  (co-domain (value (abstract-object)))
  )


(define-relation IS-BOOKED
    (isa       (value (unary-relation)))
  (domain    (value (physical-object)))
  (co-domain (value (boolean-value)))
  )


;;; 
;;; New attribute value instances. When a constant such as the symbol C17-002
;;; is input from POIROT, it will be interned and mapped into a Meta-AQUA frame
;;; representation by the following operation. (symbol-value (intern C17-002)).
;;; This will return a value such as the attribute-value C17-002.0.
;;; 

(define-attribute-value C17-002.0
    (isa (value (plane)))
  )
(setf C17-002 'C17-002.0)


(define-attribute-value M-001.0
    (isa (value (abstract-object)))
  )
(setf M-001 'M-001.0)


(define-attribute-value S42.0
    (isa (value (abstract-object)))
  )
(setf S42 'S42.0)


(define-attribute-value ORBI.0
    (isa (value (airport)))
  )
(setf ORBI 'ORBI.0)


(define-attribute-value ETAR.0
    (isa (value (airport)))
  )
(setf ETAR 'ETAR.0)


(define-attribute-value SBAGH.0
    (isa (value (staging-area)))
  )
(setf SBAGH 'SBAGH.0)


(define-attribute-value HLSTU.0
    (isa (value (hospital)))
  )
(setf HLSTU 'HLSTU.0)



;;; Do not really need a separate category of health-state and
;;; health-state-value.

;;; Need to do condition codes yet. e.g., C1

(define-attribute-value C869.0
    (isa (value (physical-state-value)))
  )
(setf C869 'C869.0)


(define-attribute-value LIGHT-BURNS.0
    (isa (value (physical-state-value)))
  )
(set (intern "Light Burns") 'LIGHT-BURNS.0)


(define-attribute-value SEVERE-BURNS.0
    (isa (value (physical-state-value)))
  )
(set (intern "Severe Burns") 'SEVERE-BURNS.0)

(define-attribute-value PATIENT1.0
    (isa (value (patient)))
  )
(setf P1 'PATIENT1.0)

(define-attribute-value PATIENT4.0
    (isa (value (patient)))
  )
(setf P4 'PATIENT4.0)

;;; Using location as type may be problematic.
(define-attribute-value LOCATION-P__2.0
    (isa (value (location)))
  )
(setf P__2 'LOCATION-P__2.0)

(define-attribute-value LOCATION-H__1.0
    (isa (value (location)))
  )
(setf H__1 'LOCATION-H__1.0)

(define-attribute-value LOCATION-H__3.0
    (isa (value (location)))
  )
(setf H__3 'LOCATION-H__3.0)


;;; How to handle integers needs to be determined.
(define-attribute-value FIFTEEN.0
    (isa (value (integer-value)))
  )

(define-attribute-value TWENTY.0
    (isa (value (integer-value)))
  )

(define-attribute-value TWENTY-FOUR.0
    (isa (value (integer-value)))
  )

(define-attribute-value THIRTY.0
    (isa (value (integer-value)))
  )

(define-attribute-value ONE-HUNDRED.0
    (isa (value (integer-value)))
  )

(define-attribute-value THREE-HUNDRED.0
    (isa (value (integer-value)))
  )


;;; 
;;; New object types
;;; 

(define-frame PATIENT
    (isa (value (person)))
  (condition (value (physical-state-value)))
  )


(define-frame HOSPITAL
    (isa (value (building)))
  )


(define-frame AIRPORT
  (isa (value (inanimate-object)))
  )


(define-frame STAGING-AREA
  (isa (value (inanimate-object)))
  )


;;; 
;;; The big question is whether it is cheating to have an a priori
;;; representation of mission or should it be part of the learning?
;;; 
(define-frame MISSION
    (isa (value (event)))
  (id (value (abstract-object)))
  (instrument (value (plane)))
  (arrival-time (value (integer-value))) ;end time
  )



;;; 
;;; New action types
;;; 

(define-frame SWS-CALL
    (isa  (value (MOP)))
  )


;;; ((lookupAirport SBAGH 300) 
;;;  ((ORBI 20 "Baghdad International Airport")))
;;; ;;Set mission destination
;;; ((lookupAirport HLSTU 300) 
;;;  ((ETAR 4 "Ramstein AB")))
;;; ;;Link mission
;;; ((lookupMission ORBI ETAR 24h 30) 
;;;  ((M-001 C17-002 54 0 ORBI ETAR 291 10h 20h null)))
;;; ((reserveSeat M-001 887924789)(R3))


(define-frame RequirementRec
    (isa (value (abstract-object)))
  (patient-slot (value (patient)))
  (triage-code (value (triage-value)))
  (wound-type (value (physical-state-value)))
  (LAT-slot (value (integer-value)))
  (from-slot (value (inanimate-object)))
  (to-slot (value (inanimate-object)))
  )


;;; The suite of Semantic Web Service Calls

(define-frame LookupRequirements
    (isa  (value (SWS-call)))
  (actor (value (volitional-agent)))
  (object (value (abstract-object)))
  (main-result (value ((RequirementRec))))
  )


(define-frame LookupAirport
    (isa  (value (SWS-call)))
  (actor (value (volitional-agent)))
  (object (value (inanimate-object)))
  (radius (value (integer-value)))
 #|
  (from (value (at-location 
		(domain (value =object))
		(co-domain (value (location =ploc)))
		)))
  (to (value (distance-from 
	      (domain (value =ploc))
	      (co-domain (value (integer-value)))
	      )))
 |#
  (main-result (value (distance-from)))
  )


(define-frame LookupMission
    (isa  (value (SWS-call)))
  (actor (value (volitional-agent)))
  (instrument (value (plane)))
  (object (value (mission
		  (id (value M-001.0))
		  (instrument 
		   (value =instrument))
		  )))
  (from (value (at-location)))
  (to (value (at-location)))
  (LAT-slot (value (integer-value)))
  (main-result (value 
		(instrument
		       (domain (value =object))
		       (co-domain (value =instrument)))
		))
  (side-effect (value 
		(arrival-time
		 (domain (value =object))
		 (co-domain (value (integer-value))))
		))
  )


;;; Modelled after the members slot of the collection frame.
(define-relation INSTRUMENT
    (isa            (value (attribute)))
    (co-domain      (value (entity)))
    )


(define-relation ARRIVAL-TIME
    (isa            (value (attribute)))
    (co-domain      (value (integer-value)))
    )




(define-frame ReserveSeat
    (isa (value (SWS-call)))
  (actor (value (volitional-agent)))
  (object (value (abstract-object)))
  (recipient (value (volitional-agent)))  
  (main-result (value (is-reserved 
		       (domain (value =recipient))
		       (co-domain (value =object)
				 ))))
  )



;;; 
;;; The following three definitions were for the original homework problem.
;;; 

(define-frame PatientPortType_PatientLookup
    (isa  (value (SWS-call)))
  (actor (value (volitional-agent)))
  (object (value (patient)))
  )


(define-frame HospitalLookupPortType_HospitalLookup
    (isa  (value (SWS-call)))
  (actor (value (volitional-agent)))
  (object (value (hospital)))
  (from (value (at-location 
		(domain (value =object))
		(co-domain (value (location =ploc)))
		)))
  (to (value (distance-from 
	      (domain (value =ploc))
	      (co-domain (value (integer-value)))
	      )))
  )


(define-frame TripReservePortType_TripReserve
    (isa (value (SWS-call)))
  (actor (value (volitional-agent)))
  (object (value (person)))
  (from (value (at-location 
		(domain (value =object))
		(co-domain (value (location)))
		)))
  (to (value (at-location
	      (domain (value (hospital)))
	      (co-domain (value (location)))
	      )))
  (main-result (value (is-booked (domain (value =object))
				 ;(co-domain (value true.0))
				 )))
  )




;;; 
;;; This representation redefines the flawed XP from rep_meta-xps.lisp.
;;; 
;;; Actor performs the action because it results in the precondition for the
;;; main-action. The actor has the goal of achieving the state resulting from
;;; the main-action.
;;; 
#|
(define-frame XP-INSTRUMENTAL-SCENE->ACTOR
    (isa                  (value (xp-normal-plan-choice)))
    (actor                (value (volitional-agent)))
    (conseq               (value (mop (actor (value =actor)
					     (relation =role))
				      (main-result (value =precondition)))))
    (ante          (value (mop (actor (value =actor))
				      (instrumental-scene (value =conseq))
				      (main-precondition (value =precondition))
				      (main-result (value =good-state)))))
    (consequent (value =conseq))
    (antecedent (value =ante))    
    (action
      (value =conseq))
    (main-action
      (value =ante))
    (precondition (value (state)))
    (good-state (value (state)))    
    (role              (value (actor (domain (value =conseq))
				     (co-domain (value =actor)))))
    (goal
      (value (achievement-goal
	       (goal-actor (value =actor))
	       (goal-object (value =good-state)))))
    (plan-choice       (value (plan-selection
				(actor (value =actor))
				(plan (value =conseq))
			        ;; Achieve a state accomplished by main-action
				(goal (value =goal))
				(role (value =role)))))
    (pre-xp-nodes      (value (=role =actor =conseq =precondition 
				     )))
    (explains          (value =role))
    (internal-xp-nodes (value (=plan-choice)))
    ;; We might add =goal to asserted nodes. For XPs like
    ;; xp-goal-outcome->actor we need to have such in the set to verify the XP
    (xp-asserted-nodes (value (=good-state =ante)))
    (link1             (value (mentally-enabless (domain    (value =goal))
						 (co-domain (value =plan-choice)))))
    (link2             (value (mentally-results (domain    (value =plan-choice))
						(co-domain (value =role)))))
    (links             (value (=link1 =link2)))
    )
|#

(define-frame XP-INSTRUMENTAL-SCENE->ACTOR2
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
;;; Actor performs the action because it results in the reservation for the
;;; script The actor has the goal of achieving the state resulting from the
;;; script.
;;; 
(define-frame XP-RESERVATION-PRECOND->ACTOR
    (isa                  (value (xp-instrumental-scene->actor)))
    (actor                (value (volitional-agent)))
    (conseq               (value (mop (actor (value =actor)
					     (relation =role))
				      (main-result (value =precondition)))))
    (ante          (value (script (actor (value =actor))
				      (instrumental-scene (value =conseq))
				      (main-precondition (value =precondition))
				      (main-result (value =good-state)))))
    (consequent (value =conseq))
    (antecedent (value =ante))    
    (action
      (value =conseq))
    (main-action
      (value =ante))
    (precondition (value (is-reserved
			  (domain (value =actor)))))
    (good-state (value (state)))    
    (role                 (value (actor (domain (value =conseq))
					(co-domain (value =actor)))))
    (goal
      (value (achievement-goal
	       (goal-actor (value =actor))
	       (goal-object (value =good-state)))))
    (plan-choice       (value (plan-selection
				(actor (value =actor))
				(plan (value =conseq))
			        ;; Achieve a state accomplished by main-action
				(goal (value =goal))
				(role (value =role)))))
    (pre-xp-nodes      (value (=role =actor =conseq =precondition 
				     )))
    (explains          (value =role))
    (internal-xp-nodes (value (=plan-choice)))
    ;; We might add =goal to asserted nodes. For XPs like
    ;; xp-goal-outcome->actor we need to have such in the set to verify the XP
    (xp-asserted-nodes (value (=good-state =ante)))
    (link1             (value (mentally-enabless (domain    (value =goal))
						 (co-domain (value =plan-choice)))))
    (link2             (value (mentally-results (domain    (value =plan-choice))
						(co-domain (value =role)))))
    (links             (value (=link1 =link2)))
    )






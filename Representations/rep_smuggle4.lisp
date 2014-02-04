;;; -*- Mode: LISP; Syntax: Common-lisp; Package: REPRESENTATIONS; Base: 10 -*-

(in-package :reps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	      Meta-AQUA Background Knowledge Represented as Frames
;;;;
;;;;	    Copyright (C) 1996   Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;                     File: rep_smuggle4.lisp
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


;;****************************************************************************
;;
;;                              REP_SMUGGLE4.LISP
;;                           (for Meta-Aqua version 6)
;;
;;****************************************************************************

;;; 
;;; Changed weapon definition to isa physical-device from device. 
;;; Device is not defined [mcox 29apr03]
;;; 

;;; 
;;; Postthesis Update History
;;; Removed theme from isa val on SMUGGLER and AUTHORITY definitions [mcox 29apr03]
;;; 

;;;
;;; ||||||
;;; Define add all atrans-mop, etc. 
;;; 

;;; 
;;; Also need to have XP for Detection. A general one and a 
;;; particular one for smuggling-detection. This will point
;;; to the goal of PREVENTION of smuggling. This will aid 
;;; the system in making the cross-domain inference that 
;;; since drug-detection is used to prevent drug smuggling
;;; bomb detection is used to prevent terrorism. See Cross-
;;; domain learning section on p. 6 of Ashwin's proposal.
;;; 
;;; ||||||
;;; Where is XP-Type slots in Ashwin's XPs 
;;; -they are in major XPs such as RELIGIOUS-FANATIC.
;;; 


(define-attribute-value SMUGGLER.0
  (isa (value (smuggler))
))


(define-attribute-value AUTHORITY.0
  (isa (value (authority))
))


(define-attribute-value PERSON.0
  (isa (value (person))
))






;;;
;;; MOP and RELATION is now defined in file rep_meta-xps.lisp.
;;; RELATION used to be double-defined, here and in rep_meta-xps.
;;; [cox 17apr94]
;;;



;Problem is there is a difference between take and give.
(define-frame ATRANS
	      (isa (value (mop)))
  (actor (value (volitional-agent)))
  (from (value (control-state)))
  (to (value (control-state)))
  )


(define-frame PTRANS
  (isa (value (mop)))
  (actor (value (volitional-agent)))
  (object (value (physical-object)))
  (from (value (at-location (domain (value =object))
	)))
  (to   (value (at-location (domain (value =object))
	)))
)


;;; 
;;; |||||| Like odor is a physical-object-attribute, should not to, from and
;;; others be a process-attribute or evbent-attribute instead of a simple
;;; relation? [23oct94]
;;; 
(define-relation TO
  (isa (value (relation)))
  (domain (value (entity)))
  (co-domain (value (at-location)))
  )


(define-relation FROM
  (isa (value (relation)))
  (domain (value (entity)))
  (co-domain (value (at-location)))
  )


(define-relation DOMAIN
  (isa (value (relation)))
  (domain (value (entity)))
  (co-domain (value (entity)))
  )


(define-relation SLOT
  (isa (value (relation)))
  (domain (value (entity)))
  (co-domain (value (entity)))
  )


(define-relation LIST-ROLE?
  (isa (value (relation)))
  (domain (value (entity)))
  (co-domain (value (entity)))
  )


;;;
;;; Hard to redefine because it was defined as
;;; having the domain of only mtrans-mop.
;;; 
;;; Actually should be just object anyhow.
;;; This would be more uniform for all frames with objects.
;;; Whether it is mental or physical is defined by the semantics
;;; of the slot, not the slot name.
;;; 
(define-relation MOBJECT
    (isa            (value (mop-relation)))
;    (domain         (value (mtrans-mop)))
    (co-domain      (value (entity))) ;; any fact
    (list-role?     (value false.0)))


;;;
;;; Changes mobject to object slot. [cox 25aug93]
;;; 
;;; Changes to from and to slots (were both =object) & added actor and
;;; receiver slots. [cox 24apr94]
;;;
(define-frame MTRANS
  (isa (value (mop)))
  (actor (value (volitional-agent)))
  (receiver (value (volitional-agent)))
  (object (value (entity)))
  (main-result (value (knowledge-state
			(co-domain (value =object)))))
  )



(define-relation PHYSICAL-STATE
    (isa            (value (state)))
    (domain         (value (physical-object)))
    (co-domain      (value (physical-state-value)))
    (slot           (value (physical-state)))
;;  (inst-role?     (value true.0))
    )


(define-frame PHYSICAL-STATE-VALUE
    (isa            (value (state-value)))
;;  (value-of       [physical-state]))
  )


(define-frame PHYSICAL-CONTROL-STATE           ;; domain has possession of co-domain.
    (isa            (value (physical-state)))
  )


;;; This is the way it used to be before the change below. 9 Sep 92
;;; (define-frame POSSESS
;;;     (isa            (value (physical-control-state)))     ;; DOMAIN owns CO-DOMAIN.
;;;     (co-domain      (constraint (inanimate-object (possessed-by (value =domain)))))  ; |||||| ?
;    (say-ways       (noun ((OWNERSHIP)))
;                    (sentence ((?domain OWNS ?co-domain))))
;;; )

(define-frame POSSESS
    (isa            (value (control-state physical-control-state)))     ;; DOMAIN owns CO-DOMAIN.
    (domain (value (physical-object)))	;||||||For now tables can possess. [cox 25aug93]
;    (domain (value (animate-object)))
    (co-domain      (value (physical-object 
;			    (possessed-by (value =domain)) ; |||||| ? Removed for now. [cox 25aug93]
			    )))
;    (say-ways       (noun ((OWNERSHIP)))
;                    (sentence ((?domain OWNS ?co-domain))))
)

(define-relation SOCIAL-STATE
    ;; Changed from isa state, otherwise a controls instance will not
    ;; unify with a generic relation during make-index. [cox 17apr94]
    ;; But if we made state isa relation, instead of relation isa state 
    ;; (see file rep_meta-xps.lisp), then  this would not be necessary, no? [cox 23oct94]
    (isa            (value (relation)))
    (domain         (value (group)))
    (slot           (value (social-state)))
;;;     (list-role?     (value true.0))
;;  (inst-role?     (value true.0))
    )


(define-frame SOCIAL-CONTROL-STATE            ;; DOMAIN has control over CO-DOMAIN.
    (isa            (value (social-state)))
  )


;;;
;;; |||||| CONTROLS has contraint instead of value facet. How will this
;;; affect the program behavior?  I also changed owner slot to controller
;;; slot, so this definition must have been "OWNS" in Ashwin's files.
;;; Interesting because I had not owns defintion and had to make one up
;;; below. [cox 23oct94]
;;; 
(define-relation CONTROLS
    (isa            (value (control-state social-control-state)))     ;; DOMAIN owns CO-DOMAIN.
    (co-domain      (constraint (inanimate-object (controller (value =domain)))))
;    (say-ways       (noun ((OWNERSHIP)))
;                    (sentence ((?domain OWNS ?co-domain))))
)



;;;
;;; Control states are for atrans. They are controls, possess, and owns.
;;; [cox 23oct94]
;;; 
(define-relation CONTROL-STATE
		 (isa (value (relation)))
  )

(define-relation LEGAL-STATE
    (isa            (value (state)))
    (domain         (value (entity)))
    (co-domain (value (entity)))
    )


(define-frame LEGAL-CONTROL-STATE            ;; DOMAIN has control over CO-DOMAIN.
    (isa            (value (legal-state)))
  )


(define-relation OWNS
    (isa            (value (control-state legal-control-state)))     ;; DOMAIN owns CO-DOMAIN.
  (domain (value (group)))
    (co-domain      (constraint (inanimate-object (owner (value =domain)))))
;    (say-ways       (noun ((OWNERSHIP)))
;                    (sentence ((?domain OWNS ?co-domain))))
)




(define-frame HIDING-PLACE
  (isa (value (inanimate-object)))
  (hides (value (physical-object)))
  )


(define-frame HIDING-OBJECT
  (isa (value (hiding-place)))
  (hides (value (physical-object)))
  )


(define-frame CONTAINER
  (isa (value (inanimate-object hiding-object)))
  (contains (value (physical-object)))
  )


;;;
;;; |||||| Eventually need to have an open-container. Have to watch the
;;; side-effects though because luggage would be a closed container, and then
;;; the larger paths through the hierarchy may cause certain overly-specific
;;; code not to work. [cox 24oct94]
;;; 
(define-frame CLOSED-CONTAINER
  (isa (value (container)))
  (contains (value (physical-object)))
  (door-for (value (door)))
  )


(define-frame LUGGAGE
  (isa (value (container))))

;;;
;;; Really a garbage pail.
;;; 
(define-frame PAIL
  (isa (value (container))))


(define-frame COMPOST-PILE
  (isa (value (inanimate-object hiding-object))))


(define-frame LAUNDRY-PILE
  (isa (value (inanimate-object hiding-object))))


;;; Notice that mark devaney allowed contraband to be hid under the rug in the
;;; tale-spin generator.
;;; 
(define-frame RUG
  (isa (value (inanimate-object hiding-object))))

(define-frame HOUSE
  (isa (value (building))))




(define-frame ATTRIBUTE-VALUE
  (isa (value (entity))))


(define-frame COLOR-VALUE
    (isa    (value  (physical-object-attribute-value)))
)


(define-attribute-value GREEN.0   
  (isa  (value (color-value)))
;;;   (incompatible-with (value (RED.0 BLACK.0 BLUE.0 BROWN.0)))
;  (say-ways (adjective ((GREEN))))
)


(define-attribute-value BLACK.0   
  (isa  (value (color-value)))
;;;   (incompatible-with (value (green.0 red.0 BLUE.0 BROWN.0)))
)


;;;
;;;  Elvis' hair color.
;;;
(define-attribute-value BROWN.0   
  (isa  (value (color-value)))
;;;   (incompatible-with (value (green.0 red.0 BLUE.0 BLACK.0)))
)


(define-attribute-value RED.0   
  (isa  (value (color-value)))
;;;   (incompatible-with (value (green.0 BLACK.0 BLUE.0 BROWN.0)))
)


(define-attribute-value BLUE.0   
  (isa  (value (color-value)))
;;;   (incompatible-with (value (green.0 BLACK.0 RED.0 BROWN.0)))
)


(define-attribute-value AUBURN.0   
  (isa  (value (color-value)))
;;;   (incompatible-with (value (RED.0 BLACK.0 green.0 BROWN.0)))
)


(define-attribute-value WHITE-AND-BLACK.0   
  (isa  (value (color-value)))
;;;   (incompatible-with (value (RED.0 BLACK.0 green.0 BROWN.0)))
)


(define-attribute-value BLOND.0   
  (isa  (value (color-value)))
;;;   (incompatible-with (value (RED.0 BLACK.0 green.0 BROWN.0)))
)


;;;
;;; Slots on physical objects.
;;; 
(define-frame PHYSICAL-OBJECT-ATTRIBUTE
  (isa (value (attribute)))
  )


(define-relation ODOR
    (isa            (value (physical-object-attribute)))
    (domain         (value (physical-object)))
    (co-domain      (value (odor-value)))
    (slot           (value (odor)))
;;;     (list-role?     (value false.0))
)


(define-relation COLOR
    (isa            (value (physical-object-attribute)))
    (domain         (value (physical-object)))
    (co-domain      (value (color-value)))
    (slot           (value (color)))
;;;     (list-role?     (value false.0))
)


(define-frame   ODOR-VALUE
    (isa            (value  (physical-object-attribute-value)))
)



(define-attribute-value BURNING-ROPE-ODOR.0   
		 (isa  (value (odor-value)) )
)


(define-frame BELIEF-STRENGTH-VALUE
    (isa            (value (attribute-value)))
  )


(define-frame STRONG-BELIEF-STRENGTH-VALUE
    (isa            (value (belief-strength-value)))
)


(define-attribute-value BELIEVES-TRUE.0
  (isa (value (strong-belief-strength-value)))
)




(define-frame BELIEF-STATE
    (isa            (value (mental-state)))
;;;     (incompatible-with (constraint (list. not-belief-state)))
    (co-domain      (value (belief
			     (believed-item   (value =believed-item))
			     (belief-strength (value =belief-strength)))))
    (believed-item   (value (entity)))               ;; For say-ways
    (belief-strength (value (belief-strength-value)))   ;; For say-ways
    ;; All BELIEF-STATEs stored in the BELIEFS slot.
;;; ||||||    (slot           (value (beliefs)))
;;;     (list-role?     (value true.0))
;;  (inst-role?     (value true.0))
    )


(define-frame PHYSICAL-OBJECT-ATTRIBUTE-VALUE
    (isa            (value (attribute-value)))
)


(define-frame VOLITIONAL-AGENT-ATTRIBUTE-VALUE
    (isa            (value (physical-object-attribute-value)))
)


(define-frame BELIEF
    (isa                (value (volitional-agent-attribute-value)))
;;;     (natural-category   (value [belief]))
;;;     (natural-category?  (value true.0))
    (believed-item      (value (entity)))
    (belief-strength    (value (belief-strength-value)))
    )


(define-frame GROUP
    (isa                (value (volitional-agent)))
;;  (number             (number.)) ;; works, but can't match
;;;     (natural-category   (value (group)))
;;;     (natural-category?  (value true.0))
    )


(define-frame NAME-CATEGORY
    (isa            (value (attribute-value)))
    )



(define-frame NATION-NAME
    (isa            (value (name-category)))
  )

(define-frame NATION
    (isa               (value (group)))   ;; Should be viewable as a group.
;;;     (natural-category? (value (true.0)))       ;; A non-NATION can never match a NATION.
    (name              (value (nation-name)))
    )


;;; From rep_meta-xps.lisp
;;; 
;(define-relation SPATIAL-RELATION
;    (isa            (value (relation)))
;)


(define-relation COMPARATIVE-RELATION
    (isa            (value (spatial-relation)))
)


(define-relation LARGER
    (isa            (value (comparative-relation)))
)


(define-relation FATTER
    (isa            (value (comparative-relation)))
)


(define-relation TALLER
    (isa            (value (comparative-relation)))
)


(define-frame PHYSICAL-LOCATION
  (isa            (value (spatial-relation)))
  (domain (value (physical-object)))
  (location-of (value =domain))
  )


;;; 
;;; |||||| Should not this be physical-location isa location
;;; and on, near, inside, etc. isa physical-location?
;;; 
(define-frame LOCATION
  (isa               (value (physical-location)))
  (domain (value (physical-object)))
  (location-of (value =domain))
  (co-domain (value (co-ordinates)))
;;;     (natural-category? (value true.0))
;;;     (nationality       [nationality (domain =self)])
    )


(define-frame CO-ORDINATES
  (isa (value (entity)))
  (x-cordinate (value (entity)))
  (y-cordinate (value (entity)))
  )

(define-frame  KNOWLEDGE-STATE
    (isa            (value (belief-state)))
    (believed-item (value (entity)))
    (co-domain (value =believed-item))
    (belief-strength (value believes-true.0))
)

;(define-frame KNOWLEDGE-STATE
;    (isa            (belief-state))
;    (co-domain (value (belief
;                 (belief-strength (value believes-true.0)))))
;)


;;;
;;; Changed the isa values of the following definitions: on, inside, under, and
;;; outside. The reason is that all of these are more specialized instances of
;;; nearness; that is to say, if something is inside of something else, then it
;;; is also near it; whereas the reverse is not true. [cox 9jan95]


(define-relation ON
    (isa            (value (near)))
    (domain         (value (physical-object)) )
    (co-domain      (value (physical-object)))
    (location-of    (value =domain))
    (slot           (value (on)))
;;;     (list-role?     (value false.0))  
)



;;; 
;;; One inference from this relation should be that the location
;;; of the two objects (container and contained) are the same. The 
;;; granularity problem arrises since these two loactions are 
;;; only approximately the same. This is fine for saying that the 
;;; loacation of drugs within a suitcase is the same as the location
;;; of the suitcase itself. However if we assert that the Earth is 
;;; within/inside the solar system, followed by a query on the 
;;; loaction of the Earth, the inference only holds if the agent
;;; asking the question is an alien from accross the universe. The 
;;; question would probably demand a different answer otherwise, eg.
;;; 24 million miles from the center of the solar system.
;;; 
;;; ||||||
;;; Should this frame be changed to within so that the concept of
;;; outside can be subsumed as double atrans subsumes buy/sell?
;;; 
(define-relation INSIDE
    ;; Isa location so that search and detection-method can unify.
    (isa            (value (near)))
    (domain         (value (physical-object)) )
    (co-domain      (value (physical-object)))
    (location-of    (value =domain))
    (slot           (value (inside)))
;;;     (list-role?     (value false.0))  
    )

;;;
;;; NOTE that this is to be the sense of inside that refers to
;;; X being inside a container (as opposed to X being inside a room).
;;; For the time being we do NOT have multiple word senses. Instead
;;; this version will overwrite the other.
;;; ||||||Fix later.
;;;
;;; Made isa inside. [cox 9feb95]
;;; 
(define-relation INSIDE-CONTAINER
    ;; Isa location so that search and detection-method can unify.
    (isa            (value (inside)))
    (domain         (value (physical-object)) )
    (co-domain      (value (container)))
    (location-of    (value =domain))
    (slot           (value (inside-container)))
;;;     (list-role?     (value false.0))  
    )


(define-relation UNDER
    (isa            (value (near)))
    (domain         (value (physical-object)) )
    (co-domain      (value (physical-object)))
    (slot           (value (under)))
    )

(define-relation OUTSIDE
    (isa            (value (near)))
    (domain         (value (physical-object)) )
    (co-domain      (value (physical-object)))
    (location-of    (value =domain))
    (slot           (value (outside)))
    (list-role?     (value false.0))  
    )


(define-relation NEAR
    ;; Isa location also.
    (isa            (value (physical-location)))
    (domain         (value (physical-object)) )
    (co-domain      (value (physical-object)))
    (location-of    (value =domain))
    (slot           (value (near)))
    )


;;;
;;; ||||||
;;; This must be changed eventually.
;;; 
(define-frame ELSEWHERE
  (isa (value (physical-location)))
  (location-of    (value (physical-object)))
  )


(define-relation SPEED
    (isa            (value (relation)))
    (domain         (value (vehicle)) )
    (co-domain      (value (integer-value)))
    (slot           (value (speed)))
    )


(define-frame COVER
  (isa (value (physical-object)))
  )
              

;;; This should really be a relation, but .... simplify, simplify.
(define-frame BODY-PART
  (isa (value (physical-object)))
  (body-part-of (value (animate-object)))
  )


(define-frame ANATOMICAL-ORGAN
	      (isa (value (body-part)))
  )


(define-frame STOMACH
  (isa (value (anatomical-organ)))
)


(define-frame LUNGS
  (isa (value (anatomical-organ)))
)


(define-frame SENSE-ORGAN
	      (isa (value (body-part)))
  )

(define-frame EYES
  (isa (value (sense-organ)))
)


(define-frame EARS
  (isa (value (sense-organ)))
)
 

(define-frame NOSE
  (isa (value (sense-organ)))
)



(define-frame SOUNDS
  (isa (value (entity)))
  )


(define-frame WORDS
  (isa (value (sounds)))
  )


(define-frame ANIMAL-NOISES
  (isa (value (sounds)))
  )


;;; Primitives should be isa (primitive).
(define-frame SPEAK
  (isa (value (mop)))
  (actor (value (volitional-agent)))	; Changed filler from volitional-agent so dogs cannot speak. [cox 15apr94]
  (object (value (sounds)))
)

(define-frame TALK
  (isa (value (speak)))
  (actor (value (person)))
  (object (value (words)))
  )



(define-frame ATTEND
  (isa (value (mop)))
  (actor (value (animate-object)))
  (object (value (sense-organ)))
  (to (value (physical-object)))
)

(define-frame STREET
  (isa (value (inanimate-object)))
  )


(define-frame BUILDING
  (isa (value (inanimate-object)))
  )


(define-frame JAIL
  (isa (value (building)))
)


(define-frame DOG
  (isa (value (volitional-agent animate-object)))
)



(define-frame SEAL
  (isa (value (volitional-agent animate-object)))
)




;;; 
;;; Do we need a main-result to specify that we now know something
;;; or is it implied simply in the object slot?
;;; 
(define-frame SNIFF
	      (isa (value (mop)))
  (actor (value (animate-object)))
  (object (value (physical-object)))		; The object of the sniff event is the item sniffed.
  (goal-scene 
    (value (mtrans
	     (actor (value =actor))
	     (object (value =main-result))	; The object of the mtrans is knowledge.
	     )))
  (instrumental-scene
    (value (attend
	     (actor (value =actor))
	     (object (value (nose
			      (body-part-of (value =actor)))))
	     (to (value =object)))))
  (scenes (value (=instrumental-scene =goal-scene)))
  (main-result 
    (value 
      (knowledge-state
	(domain (value =actor))
;;; 		   (believed-item (value (odor))); |||||| Deleted temporarily because of conflict with
	;; merging sniff & detection. Problem is that detection-method's goal-scene has a main-result
	;; whose knowledge-state's believed item is the location of the contraband. Interesting conflict!
	)))
  )




;;;
;;; |||||| Notice that by changing the bark definition to include the object
;;; slot (what the dog barks at), we present the problem of modifying both
;;; this new slot as well as the domain of the at-location subframe. Thus frame
;;; modifying functions need to be able to process variable bindings, not just
;;; fillers.
;;; 
(define-frame DOG-BARKS			;Changed from BARK. [cox 25aug93]
  (isa (value (bark)))
  (actor (value (dog)))
  (object (value (animate-object)))
  (to (value (at-location
	       (domain (value =object)))))
  ;; Had to commetn out the instrumental-scene because dogs cannot speak.  They make noise or signal
  ;; instead. We want Tale-Spin to generate an anomaly when it output the concept of dog speaking. 
  ;; [cox 17apr94]
  ;; No, we now have talk frame for persons and speak with object animal-noises for animals.
  ;; [cox 25apr94]
  (instrumental-scene
    (value (speak
	     (actor (value =actor))
	     (object (value (animal-noises)))
	     )))
  (goal-scene
    (value
      (mtrans
	(actor (value =actor))
;;; 	(receiver (value =object))
	(object (value (knowledge-state =ks
			 (domain (value =actor)))))
	(from (value (at-location
		       (domain (value =actor)))))
	(to (value (at-location
		    (domain (value =object))))))))
  (main-result (value =ks))
  (scenes (value (=instrumental-scene  
		  =goal-scene)))
  )


(define-frame SEAL-BARKs
  (isa (value (bark)))
  (actor (value (seal)))
  (object (value (animal-noises)))
  )


(define-frame BARK			;Changed from BARK. [cox 25aug93]
  (isa (value (mop)))
  (actor (value (volitional-agent)))
  (instrumental-scene (value (speak
			       (actor (value =actor)))))
  (scenes (value (=instrumental-scene)))
  )



(define-frame AUTHORITY-VOLITIONAL-AGENT
    (isa             (value (volitional-agent)))
    (occupation      (value (police-act)))
    )



(define-frame POLICE-ACT
  (isa (value (mop)))
  (actor (value (authority-volitional-agent)))
  (planner (value (authority-group)))
  (opponent       (value (volitional-agent)))
  )

(define-frame AUTHORITY-GROUP
    (isa            (value (group)))
    (primes         (value (detection-method)))
    (occupation     (value (interdiction-act (planner (value =self)))))
;    (typical-mops   (constraint (interdiction-act detection)))
    )



(define-frame AUTHORITY
  (isa (value (person))) ; Removed theme from isa val [mcox 29apr03]
;;;   (primes         (value (detection-method)))
;;;   (occupation     (value (interdiction-act (actor (value =self)))))
;;;   (typical-mops   (constraint (interdiction-act)))
  )




;;;
;;; NOTE that Meta-AQUA should be able to learn that children can commit crimes
;;; too.
;;; 
(define-frame CRIMINAL-VOLITIONAL-AGENT
    (isa             (value (adult)))
    (occupation      (value (criminal-act)))
;    (say-ways        (noun ((MILITARY VOLITIONAL AGENT))))
)



(define-frame CRIMINAL-GROUP
    (isa            (value (group)))
    (occupation     (value (criminal-act (planner (value =self)))))
;    (typical-mops   (constraint (interdiction-act detection)))
    )


(define-frame CRIMINAL-ACT
  (isa (value (mop)))
  (actor (value (criminal-volitional-agent)))
  (planner (value (criminal-group)))
  (opponent       (value (volitional-agent)))
  (instrument (value (physical-object))))



(define-frame SMUGGLING-GROUP
  (isa (value (group)))
;  (primes (value (smuggling-method)))
  (occupation     (value (smuggling-act (planner (value =self)))))
  (typical-mops   (constraint (smuggling-act)))
)



;;;
;;; ||||||
;;; What is the function of =self.
;;; Should he also be isa criminal?
;;; 
(define-frame SMUGGLER
; ||||||
; instead of person should this be criminal-volitional-agent?
    (isa            (value (person criminal-volitional-agent))) ; Removed theme from isa val [mcox 29apr03]
;    (primes         (value (smuggling-method)))
    (occupation     (value (smuggling-act (actor (value =self)))))
    (typical-mops   (constraint (smuggling-act)))
)


;;;
;;; ||||||
;;; Make TERRORIST isa CRIMINAL too.
;(define-frame CRIMINAL
;  (isa (person)))


;;; ||||||
(define-frame SNITCH
  (isa (value (criminal-volitional-agent)))
)



(define-frame CONTRABAND
  (isa (value (inanimate-object))))


;;; |||||| Changed to drug instead of drugs. [cox 19aug93]
(define-frame DRUG
  (isa (value (contraband))))


(define-frame MARIJUANA
  ;; |||||| Changed to drug instead of drugs. [cox 19aug93]
  ;; Added plan to make sibling with tobacco. [cox 24feb95]
  (isa (value (drug plant))))



(define-frame TOBACCO
  ;;  [cox 23feb95]
  (isa (value (plant))))



(define-relation RISKED-MOP
    (isa            (value (mop-relation)))
    (co-domain      (value (event)))
    (list-role?     (value false.0)))


(define-frame CONFISCATION
  (isa (value (police-act))) ; Should also be a police-act.
  (actor (value (authority)))
  (object (value (physical-object)))
  (opponent (value (criminal-volitional-agent)))
  (main-result (value (controls (domain (value =actor))
				(co-domain (value =object)))))
  (goal-scene
    (value (atrans
	     (actor (value =actor))
	     (object (value =object))
	     (from  (value (controls
			     (domain (value =opponent))
			     (co-domain (value =object)))))
	     (to (value =main-result)))))
  )


;;;
;;; Modeled after Terrorist-Act.
;;; 
(define-frame SMUGGLING-ACT
  (isa (value  (criminal-act crime))) ; See murder.
  (actor (value (smuggler)))
  (planner (value (smuggling-group)))
  (object (value (contraband =p)))
  (instrument (value (physical-object))) ; What is used to hide the object.
  (opponent (value (authority)))
  (goal-scene
  ;; change definition of this mop to have physical object as well.
   (value (ptrans
	    (actor (value =actor))
	    (object (value =object))
	    (to  (value (nation =l))))))
  (risked-mop 
    (value (detection
	     (actor (value =opponent))
	     (object (value =p))
	     (main-result (value =bad-state)))))
  (main-result    (value (at-location 
			   (domain (value =object))
			   (co-domain (value =l)))))
  (bad-state 
    (value
      (knowledge-state
	(domain (value =opponent))
	(believed-item 
	  (value (at-location
		   (domain (value =p))
		   ;; |||||| Perhaps this should be (near (co-domain =instrument)).
		   (co-domain (value =instrument)) 
		       ))))))
  ;; Want to avoid another's knowledge-state.
  (not-want-bad-state 
    (value (prevention-goal 
	     (goal-actor (value =actor))
	     (goal-object (value =bad-state)))))
)




(define-relation PREVENTED-MOP
    (isa            (value (mop-relation)))
    (co-domain      (value (event)))
    (list-role?     (value false.0))
)



;;; 
;;; Should the result of this act be couched in positive or negative terms?
;;; Do we want a good state of disabling the preconditions of Smuggling-Act
;;; or avoid the bad state of the precondition remaining?
;;; Secondly does the disabling link belong in the explanation of this act
;;; or can it be represented here?
;;; 
(define-frame INTERDICTION-ACT
  (isa (value (police-act))) ; See murder.
  (actor (value (authority)))
  (planner (value (authority-group)))
  (object (value (contraband)))
;  (instrument (value (physical-object))) ; Weapon?
  (opponent (value (smuggler)))
  (good-state (value (controls (domain (value =actor))
                               (co-domain (value =object)))))
  (main-result (value =good-state))
  (item-location (value (physical-location
			  (location-of (value =object)))))
  (m-object (value (knowledge-state
		     (domain (value =actor))
		     (believed-item
		       (value (at-location
				(domain (value =object))
				(co-domain (value =item-location))))))))
  (instrumental-scene (value
			(detection
			  (actor (value =actor))
			  (object (value =object))
			  (item-location (value =item-location))
			  (main-result (value =m-object))
			  (instrumental-scene-of (value =self)))))
  (goal-scene 
    (value (confiscation
	     (actor (value =actor))
	     (object (value =object))
	     (opponent (value =opponent))
	     (preconditions (value =m-object))
	     (main-result (value =good-state)))))
  (scenes (value (=instrumental-scene =goal-scene)))
  (prevented-mop 
    (value (smuggling-act
	     (actor (value =opponent))
	     (object (value =object))
	     (opponent (value =actor)))))
;  (risked-mop (shooting
;                (actor =opponent)
;                (object =actor)
;                (main-result =bad-state)))
;  (bad-state (value
;             (death-state
;              (domain (value =actor))
;              (co-domain (value
;                          dead.0)
;             )))
;  (not-want-bad-state (prevention-goal (goal-actor (value =actor))
;                                       (goal-object (value =bad-state)))
)



;;;
;;; Should the confiscation and detection be represented with their own slots
;;; in this structure?
;;; The main-result of the interdiction-act is the possession of the contraband
;;; whereas the main-result of the bust-act is the legal possession of the criminal 
;;; by the authorities. This is one reason why the representation is complicated. 
;;; Another is that there may be a bust without a confiscation. Would this be another
;;; variant structure though. The reasoning process could maintain an abstract 
;;; representation, refining the structure when more info comes in. Many subtleties...
;;;
;;; Note that this definition of bust involves arrest for smuggling or possession
;;; of the contraband. This is determined by the fact that the arrest scene has
;;; the precondition that the authority controls the contraband seized through the
;;; interdiction. There could be other definitions and variants on the bust act. This
;;; is simply one version of the concept. Thus should actually be a specialization of
;;; bust-act caslled smuggling-bust or something similar. 8 May 93
;;; 
(define-frame BUST-ACT
  (isa (value (police-act))) ; See murder.
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
  )



(define-relation HIDDEN-ITEM
    (isa            (value (mop-relation)))
    (co-domain      (value (physical-object)))
    (list-role?     (value false.0))
;;  (needs-explanation? (value true.0))
)



;;; 
;;; |||||| This is not really an Mtrans-MOP since there is no
;;;       object. Or actually this is a receiving MTRANS, not a trans-
;;;       mitting MTRANS such as talking, threatening, etc.
;;; 
;;; Ideally we want a generic detection mop which covers detections
;;; by actors other than authorities.
;;; 
 (define-frame DETECTION
  (isa (value (mop))) ; isa investigation scene?
;;;   (actor (value (authority)))
  (actor (value (volitional-agent)))
;;;   (object (value (contraband)))
  (object (value (physical-object)))
  (item-location (value (physical-location)))
  (m-object (value
             (knowledge-state
              (domain (value =actor))
	      (believed-item (value (at-location =l
				       (domain (value =object))
				       (co-domain (value =item-location))
					    )))
                           )))
  (main-result (value =m-object))
  ;; |||||| Method should really be goal-scene or instrument slot.
  (method (value (detection-method
		   (actor (value =actor))
		   ;; |||||| Should simply be object slot.
		   (hidden-item (value =object))
		   (item-location (value =l))
		   ;; Is it necessary to have mtrans? I think so, in
		   ;; order for the understander to reason without having to 
		   ;; delve deeper into the particulars of the mtrans. Yet if
		   ;; I fix the f.get function then we can inspect w/out instantiating.
		   (goal-scene (value (mtrans
					(main-result (value =main-result)))))
		   (main-result (value =main-result)))))
)


(define-frame DETECTION
	      (isa (value (mop)))		; isa investigation scene?
;;;   (actor (value (authority)))
  (actor (value (volitional-agent)))
  (object (value (contraband)))
;;;   (object (value (physical-object)))
  (item-location (value
		   (inside
		     (domain (value =object))
		     (co-domain (value (container))))
;		   (physical-location
;		     (location-of (value =object)))
		   ))
  (m-object (value
	      (knowledge-state
		(domain (value =actor))
		(believed-item (value (at-location
					(domain (value =object))
					(co-domain (value =item-location))
					)))
		)))
  (main-result (value =m-object))
  ;; |||||| Method should really be goal-scene or instrument slot.
  (method (value (detection-method
		   (actor (value =actor))
		   ;; |||||| Should simply be object slot.
		   (hidden-item (value =object))
		   (item-location (value =item-location))
		   ;; Is it necessary to have mtrans? I think so, in
		   ;; order for the understander to reason without having to 
		   ;; delve deeper into the particulars of the mtrans. Yet if
		   ;; I fix the f.get function then we can inspect w/out instantiating.
		   (goal-scene (value (mtrans
					(main-result (value =main-result)))))
		   (main-result (value =main-result)))))
  )



;;; 
;;; Can detect something by looking at it, being told.
;;; Both have preconditions, especially since the object
;;; is presumed to be hidden. Should how hidden and the
;;; preconditions, etc. be part of DETECTION?
;;; 
 (define-frame DETECTION-METHOD
  (isa (value (mop))) ;?
  (actor (value (volitional-agent)))
  ;; |||||| Should simply be object slot.
  (hidden-item (value (physical-object)))
  (item-location (value (at-location)))
  ;; This is really an XP?  (how hidden (?))
  ;; Revealing-scene
  (goal-scene (value (mtrans)))
  (scenes (value (=goal-scene)))
  (main-result (value (knowledge-state)))
  )

(define-frame DETECTION-METHOD
  (isa (value (mop))) ;?
  (actor (value (volitional-agent)))
  ;; |||||| Should simply be object slot.
  (hidden-item (value (physical-object)))
  (item-location (value (physical-location
			  (location-of (value =hidden-item)))))
  ;; This is really an XP?  (how hidden (?))
  ;; Revealing-scene
  (goal-scene (value (mtrans)))
  (scenes (value (=goal-scene)))
  (main-result (value (knowledge-state)))
  )



;;;
;;; The TIP-OFF is detection represented by 
;;; the authority HEARING the info from another person.
;;; 
;;; This needs to cover the concerned citizen informing as well
;;; as the snitch or informant telling. XPs then will distinguish the
;;; reasons why one happens as opposed to the other. One has a belief in
;;; law-and-order like the fanatic has religious beliefs, one has a grudge,
;;; and one gets paid.
;;; 
(define-frame TIP-OFF
  (isa (value (detection-method)))
  ;; |||||| This should be generalize to animate object once
  ;; we learn that dogs can play this role too?
  (actor (value (authority)))
  (source (value (volitional-agent)))
  (hidden-item (value (contraband)))
  (item-location (value
		   (at-location
		     (domain (value =hidden-item))
		     (co-domain (value (physical-location))))))
  (m-object (value (knowledge-state
		     (domain (value =actor))
		     (believed-item 
		       (value =item-location)))))
  (main-result (value =m-object))
  ;; Revealing-scene.
  (goal-scene  (value (mtrans
			(actor (value =source))
			(object (value =actor))
			(mobject (value =m-object))
			(instrumental-scene 
			  (value (speak
				   (actor (value =source)))))
			(from (value (at-location)))
			(to (value (at-location)))
			(main-result (value =m-object)))))
  (scenes (value (=goal-scene)))
  )


(define-frame TIP-OFF
  (isa (value (detection-method)))
  ;; |||||| This should be generalize to animate object once
  ;; we learn that dogs can play this role too?
  (actor (value (authority)))
  (source (value (volitional-agent)))
  (hidden-item (value (contraband)))
  (item-location (value
		   (physical-location
		     (location-of (value =hidden-item)))))
  (m-object (value (knowledge-state
		     (domain (value =actor))
		     (believed-item 
		       (value (at-location
				(domain (value =hidden-item))
				(co-domain (value =item-location))))))))
  (main-result (value =m-object))
  ;; Revealing-scene.
  (goal-scene  (value (mtrans
			(actor (value =source))
			(object (value =actor))
			(mobject (value =m-object))
			(instrumental-scene 
			  (value (speak
				   (actor (value =source)))))
			(from (value (at-location)))
			(to (value (at-location)))
			(main-result (value =m-object)))))
  (scenes (value (=goal-scene)))
  )





;;;
;;; ||||||
;;; One question is where to put the (inside) relation?
;;; Could be filler of item-location or part of the
;;; m-object knowledge-state or even inside the mtrans.
;;; 
;;; There should be a representation somewhere that this
;;; event needs to be iteratively applied until the main-
;;; result is successful.
;;; 
(define-frame SEARCH
  (isa (value (detection-method)))
  ;; XP needs to bind this with the authority that knows.
  ;; Or will inferences get us this since there is a binding here?
  (actor (value (authority)))
  (object (value =actor))
  (container (value (container
		      (contains (value =hidden-item)))))
  (hidden-item (value (contraband)))
  (item-location (value (inside
			  (domain (value =hidden-item))
			  (co-domain (value =container))
			  (location-of (value =hidden-item)))))
  ;; Actually I think that inside should be isa at-location.
  ;; We then do not have the double reference below. Inside
  ;; is just more specific than at.
  (m-object 
    (value (knowledge-state
	     (domain (value =actor))
	     (believed-item 
	       (value (at-location
			(domain (value =hidden-item))
			(co-domain (value =item-location)))))
		     )))
  ;; The opens-scene is an instrumental action for the
  ;; revealing-scene to take place. Is that fact represented
  ;; here or only in the XP which explains this method?
  (instrumental-scene 
    (value (ptrans
                 (actor (value =actor))
                 (object (value (cover)))
                 (from (value (on
				(domain (value =object))
				(co-domain (value =container)))))
                 (to (value (elsewhere))) ;what will this become?
                 )))
  ;; Look in object.
  ;; Revealing scene.
  (goal-scene 
    (value (mtrans
	     (actor (value =actor))
	     (object (value =actor))
	     (mobject (value =m-object))
	     (instrumental-scene
	       (value (attend
			(actor (value =actor))
			(object (value (eyes)))
			(to (value =hidden-item)))))
	     )))
  (scenes (value (=instrumental-scene =goal-scene)))

  (main-result (value =m-object))
  )



(define-frame CRIME
  (isa (value (action))))



(define-frame SMUGGLING-CRIME
  (isa (value (crime))))



;;;
;;; Really should be isa smuggling-crime-value ?
;;; 
(define-attribute-value SMUGGLING-CRIME.0
  (isa (value (smuggling-crime))
))



(define-frame ARREST
  (isa (value (police-act violent-mop)))
  (actor (value (authority)))
  (object (value (criminal-volitional-agent)))
  (charge (value (crime)))

  (proclaim-arrest 
    (value (mtrans
	     (actor (value =actor))
	     (object (value =object))
	     (mobject)
	     (instrumental-scene
	       (value (speak
			(actor (value =actor))
			(object)
			(to))))
	     )))
  (read-rights 
    (value (mtrans
	     (actor (value =actor))
	     (object (value =object))
	     (mobject)
	     (instrumental-scene
	       (value (speak
			(actor (value =actor))
			(object)
			(to))))
		 )))
  ;; Really should be filled with an incarcerate mop that has the 
  ;; same main-result as the main-result of the arrest mop.
  (take-to-jail 
    (value (ptrans
	     (actor (value =actor))
	     (object (value =object))
	     (to (value (at-location
			  (domain (value =object))
			  (co-domain (value (physical-location
					   (location-of (value (jail)))))))))
                  )))
  ;; Lack of freedom.
  (main-result 
    (value (controls
	    (domain (value =actor))
	    (co-domain (value =object)))))
  (scenes (value (=proclaim-arrest =read-rights =take-to-jail)))
  )




;;; 
;;; |||||| NOTE now that the decision-process defined for d-c-nodes
;;; do not correcpond to the decisionpprocess used in volitional
;;; decisions to enter an actor role etc.
;;; 
(define-frame DECISION-PROCESS-FOR-ENTER
    (isa                     (value (decision-process)))
;;;     (mop-component-considered(value [enter-mop.0]))
  )



(define-relation SCENES
    (isa            (value (causal-relation)))
    (domain         (value (mop)))
    (co-domain      (value (mop)))
    (slot           (value (scenes)))
;;;     (list-role?     (value true.0))
    )



;;; 
;;; The instrumental scene of a MOP is a MOP whose main result is a state that satisfies the
;;; main precondition of the goal scene of that MOP.  (I use main result to avoid the hassle of
;;; variable binding across list slots.  This seems ok.)
;;; 
(define-relation INSTRUMENTAL-SCENE
    (isa            (value (scenes))) ;; causal-relation
    (domain         (value (mop (instrumental-scene (value =co-domain)))))
    (co-domain      (value (mop (instrumental-scene-of (value =domain)))))
    (slot           (value (instrumental-scene)))
;;;     (list-role?     (value false.0))
    )



;;; 
;;; Should really be instrumental-scene-.
;;; 
(define-relation INSTRUMENTAL-SCENE-OF
    (isa            (value (mop-relation)))
    (domain         (value (mop (instrumental-scene-of (value =co-domain)))))
    (co-domain      (value (mop (instrumental-scene (value =domain)))))
;;;     (list-role?     (value false.0))
    )



;;; 
;;; The goal scene of a MOP is a MOP whose main result is the main result of
;;; the overall MOP. The variable bindings in the following may not work.
;;; In Ashwin's rep_causal.lisp similar bindings were commented out
;;; in instrumental-scene and replaced by the pattern above. I put
;;; the relation frame in for the first =m. Does is work? I know it
;;; would if I put in a domain or co-domain slot in the relation frame.
;;; 
(define-relation GOAL-SCENE
    (isa            (value (scenes))) ;; causal-relation
    (domain         (value (mop (main-result (value (relation =m)))
				(goal-scene (value =co-domain)))))
    (co-domain      (value (mop (main-result (value =m))
				(goal-scene-of (value =domain)))))
    (slot           (value (goal-scene)))
;;;     (list-role?     (value false.0))
    )



(define-relation SIDE-EFFECT
    (isa            (value (results)))
    (co-domain      (value (relation)))
    (domain         (value (process
			     (side-effect (value =co-domain)))))
    (slot           (value (side-effect)))
    ;; for now (maybe there should be an "other-results" slot.
;;;     (list-role?     (value false.0))
    )
    


(define-relation MAIN-RESULT
    (isa            (value (results)))
    (co-domain      (value
;;; 		      (relation)	; Commented out because of generalize-node failure 
		      (state)		; during baffled&resolved example (story 9). [cox 27feb95]
			   ))
    (domain         (value (process
			     (main-result (value =co-domain)))))
    (slot           (value (main-result)))
    ;; for now (maybe there should be an "other-results" slot.
;;;     (list-role?     (value false.0))
    )
    


(define-frame PLAN-SELECTION
    (isa               (value (decision-process-for-enter)))
    (actor             (value (volitional-agent)))
    ;; |||||| Ashwin had plan slot defaulted to being a MOP.
    (plan              (value (event)))		; Changed from (plan) [26feb95]
    (goal              (value (goal (goal-actor (value =actor)))))
    (role              (value (volitional-role-relation)))
    )




(define-frame INTEGER-VALUE
	      (isa (value (attribute-value)))
)


(define-attribute-value ZERO.0
		 (isa (value (integer-value)))
)

(define-attribute-value ONE.0
		 (isa (value (integer-value)))
)

(define-attribute-value TWO.0
		 (isa (value (integer-value)))
)

(define-attribute-value THREE.0
		 (isa (value (integer-value)))
)

(define-attribute-value FOUR.0
		 (isa (value (integer-value)))
)

(define-attribute-value FIVE.0
		 (isa (value (integer-value)))
)

(define-attribute-value SIX.0
		 (isa (value (integer-value)))
)

(define-attribute-value SEVEN.0
		 (isa (value (integer-value)))
)

(define-attribute-value EIGHT.0
		 (isa (value (integer-value)))
)

(define-attribute-value NINE.0
		 (isa (value (integer-value)))
)

(define-attribute-value TEN.0
		 (isa (value (integer-value)))
)

(define-attribute-value ELEVEN.0
		 (isa (value (integer-value)))
)



(define-frame AMOUNT-VALUE
	      (isa (value (attribute-value)))
)


(define-attribute-value LOW.0
		 (isa (value (amount-value)))
)

(define-attribute-value MED-HIGH.0
		 (isa (value (amount-value)))
)

(define-attribute-value HIGH.0
		 (isa (value (amount-value)))
)


(define-frame BOOLEAN-VALUE
	      (isa (value (attribute-value)))
)

(define-attribute-value TRUE.0 
		 (isa (value (boolean-value)))
)

(define-attribute-value FALSE.0 
		 (isa (value (boolean-value)))
)


(define-frame ABSTRACT-OBJECT
  (isa (value (entity)))
  )

;;; Changed [cox 20aug93]
(define-frame PHYSICAL-OBJECT
;;;  Did the definition from Ashwin's files actually have physical-object
;;;  isa physical location?
;;; (isa            (value  (physical-location)))
    (isa            (value  (entity)))
    ;; Was constraint as below. [cox 20aug93]
    (at-location    (value (physical-location
				  (location-of (value =self)))))
;    (at-location    (constraint (location
;				  (location-of (value =self)))))
;;  (weight         [weight-value])
    (color          (value color-value))
    (odor           (value odor-value))
    (composition    (value compositional-value))
    (age            (value integer-value)) ;|||||| Could make a test during unification 
					   ;with procedural knowledge for integers. 
;;  (size               [size-value])
    (name           (constraint (name-category)))
;    (nationality    (constraint (nationality (domain =self))))
    (life-state     (constraint (life-state-value)))
;    (say-ways       (noun (list. (PHYSICAL OBJECT))))
)


(define-frame ANIMATE-OBJECT
	      (isa (value (physical-object)))
  (gender          (value (gender-value)))
  (life-state      (value animate.0))
  (body-part       (value (physical-object)))
  )



(define-frame INANIMATE-OBJECT
    (isa                (value (physical-object)))
;    (natural-category   (value [inanimate-object]))
;    (natural-category?  (value true.0))	
    (owner              (constraint (volitional-agent)))
;;  (creator            [volitional-agent])
    (purpose            (constraint (mop (instrument (constraint =self)))))
    (life-state         (value inanimate.0))
;    (journalism-roles   (list. =owner))
)



(define-frame ATTRIBUTE
    (isa                (value  (relation)))
)


(define-relation LIFE-STATE
    (isa            (value (attribute)))
    (co-domain      (constraint (life-state-value)))
    (slot           (value (life-state)))
    (list-role?     (value false.0))
)


(define-frame LIFE-STATE-VALUE
    (isa            (value (attribute-value)))
)


(define-attribute-value ANIMATE.0
    (isa             (value (life-state-value)))
;;;     (incompatible-with (value INANIMATE.0))
)


(define-attribute-value INANIMATE.0
    (isa             (value (life-state-value)))
)
 


(define-relation AT-LOCATION
    (isa            (value (physical-object-attribute)))
    (domain         (value (physical-object)))
    (co-domain      (value  (physical-location
			      ;; Commented out since sometimes we want to talk about relative locations
			        ;; or co-locations. I.e.,
			      ;; (at-location (domain (value hand.12))
			        ;;              (co-domain (value (physical-location
			        ;;                                  (location-of (value ball.31))))))
			      ;; when the hand and the ball connect in a hit. [cox 4may94]
;;; 			      (location-of (value =domain))	
			      )))
    (slot           (value (at-location)))
;;;     (list-role?     (value false.0))
)


;;; Added gender to volitional agents and the following to go with it. [cox 20aug93]
(define-attribute-value MALE.0
  (isa (value (gender-value)))
  )

(define-attribute-value FEMALE.0
  (isa (value (gender-value)))
  )

(define-frame GENDER-VALUE
  (isa (value (attribute-value)))
  )


(define-frame VOLITIONAL-AGENT
	      (isa (value (animate-object)))
  (gender          (value (gender-value)))
  )



(define-frame PERSON
  (isa (value (volitional-agent)))
  (name (value (literal)))
  (hair (value (color-value)))			; Defined here because mammal is not defined.
  (body-part (value (physical-object)))
  )





(define-relation ACTOR
		 (isa (value (volitional-role-relation)))
                 (domain (value (mop)))
		 (co-domain (value (animate-object)))
)


(define-relation TRUTH
		 (isa (value (truth-relation)))
                 (domain (value (entity)))
		 (co-domain (value (truth-value)))
)


(define-frame WEAPON
    ;; Changed to physical-device from device. 
    ;; Device is not defined [mcox 29apr03]
	      (isa (value (contraband physical-device))))



(define-frame EXPLOSIVES
	      (isa (value (contraband weapon))))


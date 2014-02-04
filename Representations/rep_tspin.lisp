;;; -*- Mode: LISP; Syntax: Common-lisp; Package: REPRESENTATIONS; Base: 10 -*-

(in-package :reps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	      Meta-AQUA Background Knowledge Represented as Frames
;;;;
;;;;	    Copyright (C) 1996   Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;                     File: rep_tspin.lisp
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
;;                              REP_TSPIN.LISP
;;
;;****************************************************************************

;;; 
;;; The frame definitions in this file are added for those objects and acts not
;;; in Meta-AQUA frame representation format yet, but necessary to support the
;;; kinds of entities existing in Tale-Spin's ontology.
;;; 
;;;



(define-frame COMPOSITIONAL-VALUE 
    (isa    (value  (physical-object-attribute-value)))
    )


(define-frame PLASTIC.0
  (isa  (value (compositional-value)))
  )

(define-frame GLASS.0
  (isa  (value (compositional-value)))
  )

(define-frame TILE.0
  (isa  (value (compositional-value)))
  )

(define-frame WOOD.0
  (isa  (value (compositional-value)))
  )



(define-frame FOOD 
  (isa (value (physical-object)))
  (component (value (physical-object)))		; So oranges and bananas can have peels. [cox 24oct94]
  )



;;; The following are the repsentations for the location items in *all-locations*.

(define-frame KITCHEN
  (isa (value (physical-object container))))

(define-frame SINK
  (isa (value (physical-object))))

(define-frame SMOKE
  (isa (value (physical-object))))

(define-frame AIR
  (isa (value (physical-object))))

(define-frame GARAGE
  (isa (value (physical-object container))))

(define-frame CUPBOARD
	      (isa (value (physical-object closed-container)))
  (door-for (value (door)))
  )

;;; May not be needed.
(define-frame FRIDGE
  (isa (value (physical-device closed-container)))
  (door-for (value (door)))
  )

(define-frame TABLE
  (isa (value (physical-device)))
  (purpose (value supporting.0))
  )

(define-frame STOVE
  (isa (value (physical-device)))
  (purpose (value cooking.0))
  )

	



;;; |||||| Container to stash things, but one that does not need to be opened?? [cox 19aug93]
(define-frame VASE
  (isa (value (physical-object container)))
  )


(define-frame HAMMER
  (isa (value (tool))))

(define-frame PAN
  (isa (value (tool))))

(define-frame TOOL
  (isa (value (physical-device)))
  (purpose (value nil.0))		;|||||| ???
  )


(define-frame DRUG
  (isa (value (physical-object))))


;;; |||||| Container because it contains air? [cox 19aug93]
(define-frame BALLOON
  (isa (value (toy))))


(define-frame CUP
  (isa (value (physical-object container)))
  )


;;; |||||| The following ontological items are from Pazzani and are flawed.
;;; The reason he set it up this way may have to do with avoiding multiple
;;; inheritance in Tale-Spin and Occam. [cox 20aug93]

(define-frame LIQUID
  (isa (value (physical-object)))	;; ||||||It may be a state also. [cox 20aug93]
  )

(define-frame DRINK
  (isa (value (liquid)))
  )

;;; ||||||Could be (define-frame WATER ((isa (value H2O))) (state (value (liquid))))
;;; [cox 20aug93]
;;;
(define-frame WATER
  (isa (value (liquid)))
  )

;;; |||||| What about temperature of items that take on attribute-values
;;; such as hot.0, cold.0 & warm.0 etc. ? [cox 20aug93]
;;;
;;; ||||||Could be (define-frame HOT-WATER ((isa (value H2O))) (state (value (liquid)))
;;;                                              (temperature (value hot.0))))
;;;
(define-frame HOT-WATER
  (isa (value (water)))
  )

(define-frame MILK
  (isa (value (drink)))
  )

(define-frame SWITCH
  (isa (value (physical-device)))
  (purpose (value enabling.0))
  (actuator-of (value (electrical-device)))
  )

(define-frame LIGHT
  (isa (value (electrical-device)))
  (purpose (value illuminating.0))
  (actuator (value (switch)))
  )



(define-frame PURPOSE-VALUE
    (isa    (value  (physical-object-attribute-value)))
    )

(define-attribute-value SMOKING.0   
  (isa  (value (purpose-value)))
  )

(define-attribute-value COOKING.0   
  (isa  (value (purpose-value)))
  )

(define-attribute-value IGNITING.0   
  (isa  (value (purpose-value)))
  )

;;; NOTE abiguity of lighting as in light and lighter.
(define-attribute-value ILLUMINATING.0   
  (isa  (value (purpose-value)))
  )

;;;|||||| Are all of the types with this purpose reasonable. 
;;; Should be things that are on/off & such.??
(define-attribute-value ENABLING.0   
  (isa  (value (purpose-value)))
  )

;;;|||||| Are all of the types with this purpose reasonable. 
(define-attribute-value HOLDING.0   
  (isa  (value (purpose-value)))
  )

;;;
;;; This is a bit contrived, no?
(define-attribute-value APPLYING-LEVERAGE.0   
  (isa  (value (purpose-value)))
  )

(define-attribute-value COMMUNICATING.0   
  (isa  (value (purpose-value)))
  )

(define-attribute-value SUPPORTING.0   
  (isa  (value (purpose-value)))
  )


;;; |||||| Of course not all physical devices have actuators (e.g., the pipe).
;;; [cox 24oct94]
;;; 
(define-frame PHYSICAL-DEVICE
  (isa (value (physical-object)))
  (purpose (value (purpose-value)))
  (actuator (value (physical-object)))		; To turn it on or make it function.
  )



;;; [cox 24feb95]
(define-frame SMOKING-DEVICE
  (isa (value (physical-device)))
  (purpose (value smoking.0))
  (actuator (value (physical-object)))
  )



(define-frame ELECTRICAL-DEVICE
  (isa (value (physical-device)))
  (purpose (value (purpose-value)))
  (actuator (value (switch)))
  )



(define-frame LEVERAGE-DEVICE
  (isa (value (physical-device)))
  (purpose (value (purpose-value)))
  (actuator (value (switch)))
  )



;;; Something that is used to gain access to something else.
(define-frame ACCESS-DEVICE
  (isa (value (physical-device)))
  (purpose (value (purpose-value)))
  (actuator (value (switch)))
  )



;;; |||||| Record composition (glass or plastic) properties and others? 
;;; Modify physical-object definition? [cox 19aug93]
(define-frame PIPE
	      ;; Physical-device -> smoking-device to avoid poor lazy unification [cox24feb95]
	      (isa (value (smoking-device container)))	
  (purpose (value smoking.0))
  )


(define-frame IGNITION-DEVICE
  (isa (value (physical-device)))
  (purpose (value igniting.0))
  )

(define-frame DOOR-BELL
  (isa (value (electrical-device)))
  (purpose (value nil.0)) ;;|||||| ???
  (actuator (value (switch)))
  )

;;; Pazzani's ontology considers a faucet just a container 
;;; in that it "contains" water as a glass does.
;;;
;;; Changed the ontology by removing container from isa properties to avoid
;;; lazy unification of opening containers and "opening" a faucet to turn on
;;; the water. This error of inference would be a very interesting, albeit
;;; exceedingly difficult, scenario from which to learn though. Perhaps in
;;; future research. [cox 23feb95]
;;; 
(define-frame FAUCET
  (isa (value (physical-device
;;; 		container
		)))
  (purpose (value enabling.0)) ;;|||||| ???
  (actuator (value (handle)))
  )

(define-frame PHONE
  (isa (value (physical-device)))
  (purpose (value communicating.0))
  (actuator (value (receiver)))
  )

(define-frame DOOR
  (isa (value (access-device)))
  (purpose (value enabling.0))			; Gaining access to instead?
  (door-for-of (value (closed-container)))
  )

(define-frame HANDLE
  (isa (value (leverage-device)))		; Changed from physical-device [cox 25feb95]
  (purpose (value applying-leverage.0))         ; Changed from holding.0 [cox 25feb95]
  ;; |||||| We are mixing representations for pot handles and faucet handles I believe. [cox 24oct94]
  (actuator-for (value (faucet)))
  )

(define-frame RECEIVER
  (isa (value (physical-device)))
  (purpose (value holding.0)) ;;|||||| ???
  (actuator-of (value (phone)))	; ||||||Like handle, this probably should not be device.  [cox 24oct94]
  )



(define-frame LIGHTER
  (isa (value (ignition-device))))

(define-frame WINDOW
  (isa (value (physical-device)))
  (purpose (value nil.0)) ;; |||||| ???
  )

(define-frame PEEL
  (isa (value (physical-object)))
  (component-of (value (food)))
  )

(define-frame TOY
  (isa (value (inanimate-object)))
  )

;;; Made ball isa toy to facilitate the generaization in the handball scenario.
(define-frame BALL
  (isa (value (toy)))
  )

(define-frame FLOOR
  (isa (value (physical-object)))
  )

;;; 
;;; |||||| Living as opposed to FLOOR above. But not really animate, huh?
;;; ||||||Watch out for this inference, since we use tobacco isa plant and
;;; the context of tobacco is usually not living. [cox 21feb95]
;;; 
(define-frame PLANT
  (isa (value (physical-object)))
  )

(define-frame BOARD
  (isa (value (physical-object)))
  )

(define-frame NAIL
  (isa (value (physical-object)))
  )

(define-frame CAT
  (isa (value (volitional-agent animate-object)))
)


(define-frame HAND
  (isa (value (body-part)))
  (body-part-of (value (person)))
)


(define-attribute-value NATURAL.0 
  (isa  (value (color-value)))
)

(define-attribute-value CLEAR.0 
  (isa  (value (color-value)))
)

(define-attribute-value ORANGE.0 
  (isa  (value (color-value)))
)

(define-attribute-value YELLOW.0 
  (isa  (value (color-value)))
)


(define-attribute-value WHITE.0 
  (isa  (value (color-value)))
)




(define-relation FILLED
  (isa       (value (relation)))
  (domain    (value (container)))
  (co-domain (value (physical-object)))
  )


(define-relation ATTACHED
  (isa       (value (relation)))
  (domain    (value (physical-object)))
  (co-domain (value (physical-object)))
  )


;;; 
;;; The three mental-state relations defined implicitly by Tale-Spin.
;;; 

(define-relation LIKES
  (isa       (value (relation)))
  (domain    (value (volitional-agent)))
  (co-domain (value (volitional-agent)))
  )


(define-relation FEARS
  (isa       (value (relation)))
  (domain    (value (volitional-agent)))
  (co-domain (value (volitional-agent)))
  )


(define-relation TRUSTS
  (isa       (value (relation)))
  (domain    (value (volitional-agent)))
  (co-domain (value (volitional-agent)))
  )




;;; Really play with toy, as opposed to play a game or play with another
;;; person. [cox 9feb95]
;;; 
(define-frame PLAY
	      (isa (value (mop)))
  (actor (value (child)))
  (object (value (toy)))
  )

(define-relation LESS-THAN
		 (isa (value (comparative-relation)))
  )


;;;
;;; What is need is a way to lpace literal values inside of frame definitions.
;;; If the commented out code is used, the form (f.instantiate-literal 18) is
;;; placed on the constraint. [cox 10feb95]
;;; 
(define-frame CHILD
	      (isa (value (person)))
  (age (value (literal))
;;;    (constraint (less-than
;;; 		     (domain (value =age))
;;; 		     (co-domain (value (f.instantiate-literal 18)))))
       )
  )

(define-frame ADULT
	      (isa (value (person)))
  )

			  
;;; |||||| Look at what Ashwin did with Propel in AQUA rep files. [cox 24aug93]
;;; |||||| Decide what to do with mode slots, both here and in general.
;;;
(define-frame PROPEL
  (isa (value (mop)))
  ;; Gravity might propel and it is not animate or volitional.
  (actor (value (entity)))			; Added 12apr94. 
  (object (value (physical-object)))
  (from (value (at-location
		 (domain (value =object))	; Changed back to =object 3may94.
			 ;;   =actor))	; Changed from =object 12apr94.
	)))
  (to   (value (at-location
;;; 		 (domain (value =object))	; Commented out 12apr94
	)))
)


(define-frame TOY-PROPEL
  (isa (value (propel)))
  ;; Gravity might propel and it is not animate or volitional.
  (actor (value (person)))			; Added 12apr94. 
  (object (value (hand)))
  (from (value (at-location
		 (domain (value =object))	; Changed back to =object 3may94.
			 ;;   =actor))	; Changed from =object 12apr94.
	)))
  (to   (value (at-location
		 (domain (value =object))
;;; 		 (domain (value =object))	; Commented out 12apr94
		 (co-domain (value (location
				     (location-of (value (toy))))))
	)))
)

;;; |||||| Look at what Ashwin did with Ingest in AQUA rep files. [cox 24aug93]
;;;
(define-frame INGEST
  (isa (value (mop)))
;  (actor (value (volitional-agent)))
  (object (value (physical-object)))
  (from (value (at-location (domain (value =object))
	)))
  (to   (value (at-location (domain (value =object))
	)))
)

(define-frame TIME-VALUE
    (isa    (value  (physical-object-attribute-value)))
    )

(define-attribute-value PAST.0 
  (isa  (value (time-value)))
)


;;; 
;;; |||||| Added 28oct93. Much of Tspin assumes that officer1 isa police.
;;; So instead of changing all of the entries there, I add a new level. It makes
;;; more sense anyhow, since there exists different subtypes of authorities.
;;; Need to eventually go through the old Meta-AQUA code to update.
;;; 
(define-frame POLICE
  (isa (value (authority)))
  (name (value (literal)))
  (body-part (value (physical-object)))
  )


(define-frame POLICE-ARRIVE
  (isa (value (mop)))
  (actor (value (authority)))
  (time (value (time-value)))
  )


(define-frame K-9-SQUAD-ARRIVE
  (isa (value (mop)))
  (actor (value (authority)))
  (time (value (time-value)))
  )


;;; |||||| Look at what Ashwin did with expel in AQUA rep files. [cox 24aug93]
;;;
(define-frame EXPEL
  (isa (value (mop)))
  (actor (value (volitional-agent)))
  (object (value (physical-object)))
  (to   (value (at-location
		(domain (value =object)))))
	)


(define-frame GRASP
  (isa (value (mop)))
  (actor (value (volitional-agent)))
  (object (value (physical-object)))
  )


(define-frame UNGRASP
  (isa (value (mop)))
  (actor (value (volitional-agent)))
  (object (value (physical-object)))
  )


(define-frame TIE
  (isa (value (mop)))
  (actor (value (volitional-agent)))
  (object (value (physical-object)))
  )


;(defun TURN (actor object state)
;  (make-old-cd :head 'TURN :actor actor :object object :to state))
;
;(defun DOOR (object)
;  (make-old-cd :head 'DOOR :object object))
;
;(defun WANTS (ACTOR GOAL)
;  (make-old-cd :head 'WANT :ACTOR ACTOR :OBJECT GOAL))
;
;(defun achieve (actor goal)
;  (make-old-cd :head 'achieve :actor actor :object goal))


(define-frame CALL-ON-PHONE
  (isa (value (mop)))
  (actor (value (person)))
  (object (value (phone)))
  )


;;; There should be a to direction of the kiss. However, instead of
;;; specifying some location of the object, it specifies a part of
;;; the object where the touching of the lips occurred. [cox 2sep93]
;;; 
(define-frame KISS
  (isa (value (mop)))
  (actor (value (volitional-agent)))
  (object (value (physical-object)))		;Should be default of person.
;  (to (value (at-location
;		(domain (value =object))
;		(co-domain (value (inside
;				   (domain (value =object))
;				   (co-domain (value (container))))))
;		)))
  )


;;;
;;; Really a "pour into" action rather than a tilt.
;;;
(define-frame TILT
  (isa (value (mop)))
  (actor (value (volitional-agent)))
  (object (value (physical-object)))
  (to (value (at-location
		(domain (value =object))
		(co-domain (value (inside
				   (domain (value =object))
				   (co-domain (value (container))))))
		)))
  )





(define-frame UNARY-RELATION
  (isa (value (relation)))
  )

;;; The following was created by the following code:
;;; (dolist (x *property-states* ) 
;;;   (format 
;;;    t 
;;;    "~%(define-relation ~s~%  
;;;         (isa       (value (unary-relation)))~%  
;;;         (domain    (value (physical-object)))~%  
;;;         (co-domain (value (boolean-value))))~%"  x))
;;;
;;; The returns were left out of course. [cox 24aug93]


(define-relation OPEN
  (isa       (value (unary-relation)))
  (domain    (value (physical-object)))
  (co-domain (value (boolean-value))))

(define-relation BROKEN
  (isa       (value (unary-relation)))
  (domain    (value (physical-object)))
  (co-domain (value (boolean-value))))

(define-relation RING
  (isa       (value (unary-relation)))
  (domain    (value (physical-object)))
  (co-domain (value (boolean-value))))

;;; On is an exception since there already exists an on relation.
;;; It is the spatial relation between to physical objects. 
;;; So we rename it to turned-on. [cox 24aug93]
;;;
(define-relation TURNED-ON
  (isa       (value (unary-relation)))
  (domain    (value (physical-object)))
  (co-domain (value (boolean-value))))

(define-relation FLOWING
  (isa       (value (unary-relation)))
  (domain    (value (physical-object)))
  (co-domain (value (boolean-value))))

(define-relation TIED
  (isa       (value (unary-relation)))
  (domain    (value (physical-object)))
  (co-domain (value (boolean-value))))

(define-relation SHARP
  (isa       (value (unary-relation)))
  (domain    (value (physical-object)))
  (co-domain (value (boolean-value))))

(define-relation INFLATED
  (isa       (value (unary-relation)))
  (domain    (value (physical-object)))
  (co-domain (value (boolean-value))))

(define-relation BURNING
  (isa       (value (unary-relation)))
  (domain    (value (physical-object)))
  (co-domain (value (boolean-value))))

(define-relation BARKING
  (isa       (value (unary-relation)))
  (domain    (value (physical-object)))
  (co-domain (value (boolean-value))))



;;; These were not in *property-states*
(define-relation DIRTY
  (isa       (value (unary-relation)))
  (domain    (value (physical-object)))
  (co-domain (value (boolean-value))))


(define-relation FLYING
  (isa       (value (unary-relation)))
  (domain    (value (physical-object)))
  (co-domain (value (boolean-value))))

(define-relation BURSTED
  (isa       (value (unary-relation)))
  (domain    (value (physical-object)))
  (co-domain (value (boolean-value))))

(define-relation SHATTERED
  (isa       (value (unary-relation)))
  (domain    (value (physical-object)))
  (co-domain (value (boolean-value))))

;;; 
;;; The following are the representations for the *problem-states*.
;;; 
(define-relation CONCERNED
  (isa       (value (unary-relation)))
  (domain    (value (person)))
  (co-domain (value (boolean-value))))


(define-relation THIRSTY
  (isa       (value (unary-relation)))
  (domain    (value (volitional-agent)))
  (co-domain (value (boolean-value))))


(define-relation SAD
  (isa       (value (unary-relation)))
  (domain    (value (volitional-agent)))
  (co-domain (value (boolean-value))))


(define-relation HUNGRY
  (isa       (value (unary-relation)))
  (domain    (value (volitional-agent)))
  (co-domain (value (boolean-value))))


;;; In the sense of the term used in drug bust stories, the domain of this
;;; relation should probably be limited to person since some of the
;;; inferences may eventually be farfetched to ascribe to animals, even
;;; though they too can become addicted.
;;; 
(define-relation JONESING
  (isa       (value (unary-relation)))
  (domain    (value (volitional-agent)))
  (co-domain (value (boolean-value))))


(define-relation BORED
  (isa       (value (unary-relation)))
  (domain    (value (person)))
  (co-domain (value (boolean-value))))


;;; In response to the following:
;;; Cannot handle conversion of 
;;; #{CAUSE ANTE=#{MTRANS ACTOR=DAD 
;;;                       OBJECT=#{LOC ACTOR=GANJA1 
;;;                                    VAL=?UNSPEC 
;;;                                    MODE=(POS)} 
;;;                       TO=ELVIS 
;;;                       FROM=DAD 
;;;                       MODE=(NEG)} 
;;;         CONSEQ=#{PROPEL ACTOR=ELVIS 
;;;                         OBJECT=HAND 
;;;                         TO=DAD 
;;;                         TIME=FUTURE}} 
;;; yet.
;;;
;;; Spoken: If Dad doesn't tell Elvis where the ganja1 was then Elvis
;;;  will push hand to him.  
;;; Someone else's action (actor2's) causes a person (actor1) to do some 
;;; act (consequent). This is an explanation of why actor1 chooses to 
;;; take on the role of actor in the consequent mop. [cox 27aug93]
;;;
;;; Commented out this definition because I renamed the Because definition in
;;; file rep_meta-xps.lisp simply Cause. They were so very similar, there was
;;; no sense to have two. The main difference was the actor2 slot in this
;;; definition of Cause, but I removed it. [cox 9feb95]
;;; 
;(define-frame CAUSE
;  (isa (value (xp)))
;  (ante (value =antecedent))
;  (conseq (value =consequent))
;  (actor1 (value (volitional-agent)))
;  (actor2 (value (volitional-agent)))
;  (antecedent (value (mop
;		       (actor (value =actor1))  ; This had been actor2 I think.
;		       (main-result (value =main-precondition)))))
;  (consequent (value (mop
;		       (actor
;			 (value =actor1)
;			 (relation =role))
;		       (main-precondition (value =main-precondition)))))
;  (role (value (actor
;		 (domain (value =consequent))
;		 (co-domain (value =actor1)))))
;  (main-precondition (value (state
;			      (domain (value =actor1)))))
;  (explains (value =role))
;  (pre-xp-nodes (value (=consequent =actor1 =role)))
;  (internal-nodes (value (=main-precondition)))
;  (xp-asserted-nodes (value (=antecedent)))
;  (link1 (value (results
;		  (domain (value =antecedent))
;		  (co-domain (value =main-precondition)))))
;  (link2 (value (enables
;		  (domain (value =main-precondition))
;		  (co-domain (value =consequent)))))
;  (links (value (=link1 =link2)))
;  )


(define-frame time-attribute-value
    (isa            (value (attribute-value)))
)

;;;
;;; Needed for stories like police-brutality story. [cox 16feb95]
;;; 
(define-attribute-value FUTURE.0
	      (isa (value (time-attrubute-value)))
  )

(define-attribute-value PAST.0
	      (isa (value (time-attrubute-value)))
  )

(define-attribute-value PRESENT.0
	      (isa (value (time-attrubute-value)))
  )


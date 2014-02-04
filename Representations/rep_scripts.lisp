;;; -*- Mode: LISP; Syntax: Common-lisp; Package: REPRESENTATIONS; Base: 10 -*-

(in-package :reps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	      Meta-AQUA Background Knowledge Represented as Frames
;;;;
;;;;	    Copyright (C) 1996   Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;                     File: rep_scripts.lisp
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
;;; NOTE that primitive actions such as CDs must have states for from and to
;;; slots. Thus the fillers must be states not objects. This conflicts with
;;; old CD sloppiness where we could have 
;;;   (PTRANS (actor John.1)
;;;           (from House.12)
;;;           (to Bar.5))
;;; I think that Schank actually noted this simplification once, but a good 
;;; implementation should not neglect it.
;;; 
;;; From rep_mops.txt :
;;; 
;;; Try to stick to the semantics of goal-scene etc. (if the goal-scene's
;;; main-result isn't the main-result of the mop, funny things will get merged.)
;;; If the representation is too restrictive, let me know. --Ashwin
;;; 
(define-frame MOP
    (isa                 (value (event)))
    (planner             (value (volitional-agent)))
    (actor               (value (volitional-agent)))
    (object              (value (physical-object)))
    (instrument          (value (inanimate-object)))
    ;; Used to be relation instead of state. Was Ashwion's def.
    (main-precondition   (value (state (enables (value =self)))))    ;; A state.
    (roles               (value ( =actor =planner =instrument =object)))
    (preconditions       (value ( =main-precondition)))
    (basic-roles         (value ( =actor)))
    (instrumental-scene  (value (mop (instrumental-scene-of (value =self)))
                              (main-result =p)))      ; In which the set-up occurs.
    (goal-scene          (value (mop (goal-scene-of (value =self))
				     (main-precondition (value =p))
				     ;; In which the main action occurs.
				     (main-result (value =main-result)))))
    (scenes              (value ( =instrumental-scene =goal-scene)))
    (instrumental-scene-of (value (mop (instrumental-scene (value =self)))))
    (goal-scene-of         (value (mop (goal-scene (value =self)))))
    ;; Desired outcome (a state that the PLANNER wants). Used to be relation instead of state here.   
    (main-result         (value (state)))
    ;; Should be all other outcomes (a list of states), but for now it's one side effect.
    (side-effect         (value (state)))    
    (results             (value ( =main-result =side-effect)))
    (enables-            (value ( =main-precondition)))
)



;;; NOTE the difference between the isa relation and the type slot. This
;;; was confusing in Ashwin's representation. The anomaly is still not
;;; resolved.

(define-frame VIOLENT-MOP
    (isa                 (value (mop)))
  )

(define-frame SEXUAL-MOP
    (isa                 (value (mop)))
  )

;;; This category seems ridiculous though.
(define-frame NOISY-MOP
    (isa                 (value (mop)))
  )




(define-frame MOP-TYPE
  (isa (value (attribute-value)))
  )


;;; 
;;; Actions which are violent-mops are interesting.  Sexual acts and loud
;;; noises are also inherently interesting according to Schank.
;;; 
;;; Interesting concepts deserve deeper processing.

(define-attribute-value VIOLENT-MOP.0
    (isa                 (value (mop-type)))
  )

(define-attribute-value SEXUAL-MOP.0
    (isa                 (value (mop-type)))
  )

;;; This category seems ridiculous though.
(define-attribute-value NOISY-MOP.0
    (isa                 (value (mop-type)))
  )


;;;
;;; If the reasoner has learned about a concept recently, then it remains
;;; interesting in the future, even if it is no longer anomalous. Actually this
;;; is required in the original example, since the system learns about what
;;; dogs will bark at, and therefore if the same or similar story is run, then
;;; the program will not try to explaion why the dog is barking this second
;;; time. We need it to do so in order to demonstrate that the program learns.
;;; Moreover, it makes sense that someone will be interested in something that
;;; it has recently learned about.
;;; 
(define-attribute-value PERSONALLY-INTERESTING.0
    (isa                 (value (mop-type)))
  )


;;;
;;; Explanations are interesting by default when input into a story regardless
;;; of all other attributes above. Note that even though this is called a mop
;;; type, xps are not of that type. It should really be struct-type I
;;; suppose....
;;; 
(define-attribute-value EXPLANATORY-STRUCT.0
    (isa                 (value (mop-type)))
  )



;;;
;;; Scripts are special types of mops that represent stereotypical events with
;;; defined and regular sequences of actions. The classic script is the
;;; restaurant script (see Schank & Abelson, 1977).
;;; 
(define-frame SCRIPT
	      (isa (value (mop)))
  )




(define-frame PLAYING-CATCH-SCRIPT
	      (isa (value (script)))
  (actor (value (volitional-agent)))
  (partner (value (volitional-agent)))
  (object (value (toy)))			; Could be either a ball or a baloon.
  (instrumental-scene
    (value (grasp				; Should actually be an atrans.
	     ;; Also, it is always the partner who does the grasping in Tale-Spin.
	     (actor (value =partner))
	     (object (value =object)))
	   ))
  (goal-scene
    (value (propel
	     (actor (value =partner))
	     (object (value =object))
	     (from
	       (value (at-location
			(domain (value =object))
			(co-domain
			  (value (near
				   (domain (value =object))
				   (co-domain (value =partner))))))))
	     (to
	       (value (at-location
			(domain (value =object))
			(co-domain
			  (value (near
				   (domain (value =object))
				   (co-domain (value =actor)))))))))))
  (scenes (value (=instrumental-scene =goal-scene)))	  
  )


(define-frame GET-A-DRINK
	      (isa (value (script)))
  (actor
    (value (volitional-agent)))
  (object
    (value (drink)))
  (instrumental-scene
    (value (gain-control-of-contained-object
	     (actor
	       (value =actor))
	     (object
	       (value (cup =cup)))
	     (containing-object
	       (value (cupboard))))
;			     (get-beverage-container
;			       (actor (value =actor))
;			       (object (value (cup =cup))))
	   ))
  (scene1
    (value (fill-cup
	     (actor (value =actor)))))
  (goal-scene
    (value (ingest
	     (actor
	       (value =actor))
	     (object
	       (value =object))
	     (from
	       (value (at-location
			(domain (value =object))
			;; Can't use the following because tspin creates
			;; ingests from cups and from pipes. Removed others in remaining ingests.
;;; 				      (co-domain (value (lips)))
			)))
	     (to
	       (value (at-location
			(domain
			  (value =object))
			(co-domain
			  (value (inside
				   (domain
				     (value =object))
				   (co-domain
				     (value (stomach))))))))))))
  (post-completion-scene
    (value (wash-item
	     (actor (value =actor))
	     (object (value =cup)))))
  (scenes
    (value (=instrumental-scene =scene1 =goal-scene =post-completion-scene)))	  
  )


;;;
;;; Added get-a-bite to replace eat-something. Note that if the food is peeled
;;; there may be a problem. The scene will most likely be skipped.
;;; [cox 28dec94]
;;; 
(define-frame GET-A-BITE-FROM-FRIDGE
	      (isa (value (script)))
  (actor
    (value (volitional-agent)))
  (object
    (value (food)))
  (instrumental-scene
    (value (gain-control-of-contained-object
	     (actor
	       (value =actor))
	     (object
	       (value =object))
	     (containing-object
	       (value (fridge))))))
  (goal-scene
    (value (ingest
	     (actor
	       (value =actor))
	     (object
	       (value =object))
	     (from
	       (value (at-location
			(domain
			  (value =object)))))
	     (to
	       (value (at-location
			(domain
			  (value =object))
			(co-domain
			  (value (inside
				   (domain
				     (value =object))
				   (co-domain
				     (value (stomach))))))))))))
  (scenes
    (value (=instrumental-scene =goal-scene)))	  
  )


(define-frame GET-A-BITE-FROM-TABLE
	      (isa (value (script)))
  (actor
    (value (volitional-agent)))
  (object
    (value (food)))
  (instrumental-scene
    (value (atrans
	     (actor
	       (value =actor))
	     (object
	       (value =object))
	     (to
	       (value (control-state
			(domain
			  (value =actor))
			(co-domain
			  (value =object))))))))
  (goal-scene
    (value (ingest
	     (actor
	       (value =actor))
	     (object
	       (value =object))
	     (from
	       (value (at-location
			(domain
			  (value =object))
			)))
	     (to
	       (value (at-location
			(domain
			  (value =object))
			(co-domain
			  (value (inside
				   (domain
				     (value =object))
				   (co-domain
				     (value (stomach))))))))))))
  (scenes
    (value (=instrumental-scene =goal-scene)))	  
  )



(define-frame GET-BEVERAGE-CONTAINER
	      (isa (value (mop)))
  (actor (value (volitional-agent)))
  (object (value (cup)))
  (instrumental-scene
    (value (propel
	     (actor (value =actor))
	     (object (value (door =x)))
	     (from (value (at-location
			    (domain (value =x))
			    (co-domain (value (physical-location
						(location-of (value cupboard)))))))))))
  (goal-scene
    (value (atrans
	     (actor (value =actor))
	     (object (value =object))
;;; 		      (from (value (cupboard)))
	     (to (value =main-result))
	     (main-result (value =main-result)))))
  (post-completion-scene
    (value (propel
	     (actor (value =actor))
	     (object (value =x))
	     (to (value (at-location
			  (domain (value =x))
			  (co-domain (value (physical-location
					      (location-of (value cupboard)))))))))))
  (scenes (value (=instrumental-scene =goal-scene =post-completion-scene)))
  (main-result 
    (value (controls
	     (domain (value =actor))
	     (co-domain (value =object)))))
  )



(define-frame FILL-CUP
	      (isa (value (mop)))
  (actor (value (volitional-agent)))
  (object (value (drink)))
  (instrumental-scene
    (value (propel
	     (actor (value =actor))
	     (object (value (handle =handle)))
	     (from
	       (value (at-location
			(domain (value =handle))
			(co-domain
			  (value (near
				   (domain (value =handle))
				   (co-domain (value (faucet =faucet))))))))))))
  (goal-scene
    (value (ptrans
	     (actor (value =actor))
	     (object (value cup))
	     (to
	       (value (at-location
			(domain (value cup))
			(co-domain
			  (value (physical-location
				   (location-of (value =faucet)))))))))))
  (post-completion-scene
    (value (propel
	     (actor (value =actor))
	     (object (value =handle))
	     (to (value (at-location
			  (domain (value =object))
			  (co-domain (value (near
					      (domain (value =handle))
					      (co-domain (value =faucet)))))))))))
  (scenes (value (=instrumental-scene =goal-scene =post-completion-scene)))
  (main-result
    ;; |||||| Should be on (although conflicts with spatial-relation)
    ;; or flowing. [cox 27oct94]
    (value (controls
	     (domain (value =actor))
	     (co-domain (value =object))))))



(define-frame WASH-ITEM
	      (isa (value (mop)))
  (actor (value (volitional-agent)))
  (object (value (physical-object)))
  (instrumental-scene
    (value (propel
	     (actor (value =actor))
	     (object (value (handle =handle)))
	     (from
	       (value (at-location
			(domain (value =handle))
			;; |||||| Shouldn't the following be a near? [cox 27oct94]
			(co-domain
			  (value (near
				   (domain (value =handle))
				   (co-domain (value (faucet =faucet)))))))))
	     )))
  (goal-scene
    (value (ptrans
	     (actor (value =actor))
	     (object (value =object))
	     (to
	       (value (at-location
			(domain (value =object))
			(co-domain
			  (value (near
				   (domain (value =object))
				   (co-domain (value =faucet)))))))))))
  (post-completion-scene
    (value (propel
	     (actor (value =actor))
	     (object (value =handle))
	     (to
	       (value (at-location
			(domain (value =handle))))))))
  (scenes (value (=instrumental-scene =goal-scene =post-completion-scene)))
  ;; |||||| Needs a main-result dirty  false or truth=out [cox 27oct94]
  )



;;; EAT SCRIPT

(define-frame EAT-SOMETHING
	      (isa (value (script)))
  (actor
    (value (volitional-agent)))
  (object
    (value (food)))
  (instrumental-scene
    (value (get-food-by-asking
	     (actor
	       (value =actor))
	     (object
	       (value =object)))))
  (goal-scene
    (value (ingest
	     (actor
	       (value =actor))
	     (object
	       (value =object))
	     (from
	       (value (at-location
			(domain
			  (value =object)))))
	     (to
	       (value (at-location
			(domain
			  (value =object))
			(co-domain
			  (value (inside
				   (domain
				     (value =object))
				   (co-domain
				     (value (stomach))))))))))))
  (scenes
    (value (=instrumental-scene  =goal-scene)))	  
  )


(define-frame GET-FOOD-BY-ASKING
	      (isa (value (mop)))
  (actor (value (volitional-agent)))
  (giver (value (volitional-agent)))
  (object (value (food)))
  (instrumental-scene
    (value (mtrans
	     (actor (value =actor))
	     (object (value (atrans)))		; Should the atrans be =goal-scene
	     (from
	       (value (at-location
			(domain (value =actor)))))
	     (to
	       (value (at-location
			(domain (value atrans))))))))
  (instrumental-scene2
    (value (grasp
	     (actor (value =giver))
	     (object (value =object)))))
  (goal-scene
    (value (atrans
	     (actor (value =giver))
	     (object (value =object))
	     (from (value (controls
			    (domain (value =giver))
			    (co-domain (value =object)))))
	     (to (value =main-result))
	     (main-result (value =main-result)))))
  (scenes
    (value (=instrumental-scene =instrumental-scene2  =goal-scene)))
  (main-result 
    (value (controls
	     (domain (value =actor))
	     (co-domain (value =object)))))
  )




;;; SMOKE SCRIPT


(define-frame SMOKING-SCRIPT
	      (isa (value (script)))
  (actor (value 
 	   (volitional-agent)
))
  (object (value (tobacco)))
  (instrumental-object (value (pipe)))
  (lighting-object (value (ignition-device)))
  (instrumental-scene
    (value (gain-control-of-contained-object
	     (actor (value
		      (volitional-agent)	; Changed from -actor so script will be triggered [cox 14jul]
			   ))
	     (object (value =instrumental-object))
	     (containing-object (value (cupboard))))))
  ;; The second instrumental scene is subsumed by fill-pipe.
; (instrumental-scene2
;   (value (gain-control-of
;	    (actor (value =actor))
;	    (object (value =object)))))
  (scene1
    (value (fill-pipe
	     (actor (value =actor))
	     (object (value =instrumental-object)))))
; (scene2
;   (value (gain-control-of			;redundant. Part of light-object
;           (actor (value =actor))
;	    (object (value =lighting-object)))))
  (goal-scene
    (value (smoke-pipe
	     (actor (value =actor))
	     (object (value =object))
	     (instrumental-object (value =instrumental-object))
	     (lighting-object (value =lighting-object))
	     (goal-scene
	       (value (ingest
			(actor (value =actor))
			(object (value =object)))))
	     )))
  (post-completion-scene
    (value (wash-item
	     (actor (value =actor))
	     (object (value =instrumental-object)))))
  (scenes
    (value (=instrumental-scene  =scene1
;;; 			    =scene2
				 =goal-scene =post-completion-scene)))	  
  )


(define-frame SMOKE-PIPE
	      (isa (value (mop)))
  (actor (value
;;; 	   (volitional-agent)
	   (adult)		     ; Changed to adult to test child smoking anomaly [cox 16mar95]
	   ))
  (object (value (tobacco)))
  (instrumental-object (value (pipe)))
  (lighting-object (value (ignition-device)))
  (instrumental-scene
    (value (light-object
	     (actor (value =actor))
	     (object (value =object))
	     (instrumental-object
	       (value =lighting-object)))))
  (goal-scene
    (value (ingest
	     (actor (value =actor))
	     (object (value =object))
	     (from
	       (value (at-location
			(domain (value =object))
			(co-domain
			  (value (physical-location
				   (domain (value =object))
				   (co-domain
				     (value =instrumental-object))))))))
	     (to
	       (value (at-location
			(domain (value =object))
			(co-domain
			  (value (physical-location
				   (domain (value =object))
				   (co-domain
				     (value (lungs
					      (body-part-of
						(value =actor))))))))))))))
  (post-completion-scene
    (value (expel
	     (actor (value =actor))
	     (object (value (smoke =smoke)))
	     (from
	       (value (at-location
			(domain (value =smoke))
			(co-domain
			  (value (physical-location
				   (domain (value =smoke))
				   (co-domain
				     (value (lungs
					      (body-part-of
						(value =actor)))))))))))
	     (to
	       (value (at-location
			(domain (value =smoke))
			(co-domain
			  (value (physical-location
				   (domain (value =smoke))
				   (co-domain
				     (value (air))))))))))))
  (scenes
    (value (=instrumental-scene =goal-scene =post-completion-scene)))
  )


(define-frame TURN-ON
	      (isa (value (mop)))
  (actor
    (value (volitional-agent)))
  (object
    (value (physical-device)))
  (goal-scene
    (value (propel
	     (actor (value =actor))
	     (object (value =object)))))
  (scenes
    (value (=goal-scene)))
  )



(define-frame TURN-OFF
	      (isa (value (mop)))
  (actor
    (value (volitional-agent)))
  (object
    (value (physical-device)))
  (goal-scene
    (value (propel
	     (actor (value =actor))
	     (object (value =object)))))
  (scenes
    (value (=goal-scene)))
  )



(define-frame LIGHT-OBJECT
	      (isa (value (mop)))
  (actor
    (value (volitional-agent)))
  (object
    (value (physical-object)))
  (instrumental-object
    (value (ignition-device)))
  (instrumental-scene
    (value (gain-control-of
	     (actor
	       (value =actor))
	     (object
	       (value =instrumental-object)))))
  (scene1
    (value (turn-on
	     (actor
	       (value =actor))
	     (object
	       (value =instrumental-object)))))
  (goal-scene
    (value (ptrans
	     (actor
	       (value =actor))
	     (object
	       (value =instrumental-object))
	     (to
	       (value (at-location
			(domain
			  (value =instrumental-object))
			(co-domain
			  (value (physical-location
				   (domain
				     (value =instrumental-object))
				   (co-domain
				     (value =object)))))))))))
  (post-completion-scene
    (value (turn-off
	     (actor
	       (value =actor))
	     (object
	       (value =instrumental-object)))))
  (scenes
    (value (=instrumental-scene =scene1 =goal-scene =post-completion-scene)))
  )


(define-frame GET-OBJECT
	      (isa (value (mop)))
  (actor
    (value (volitional-agent)))
  (object
    (value (physical-object)))
  (goal-scene
    (value (atrans
	     (actor
	       (value =actor))
	     (object
	       (value =object))
;;; 	     (from
;;; 	       (value (cupboard)))
;;; 	     (to
;;; 	       (value =actor))
	     (main-result
	       (value =main-result)))))
  ;; |||||| Why no scenes slot?
;(scenes
;  (value (=goal-scene)))
 (main-result 
   (value (controls
	    (domain
	      (value =actor))
	    (co-domain
	      (value =object)))))
 )




;;; 
;;; ||||||This funcion is obsolete I think.  [cox 26oct94]
;;; 
(define-frame GET-PIPE-CONTAINER
	      (isa (value (get-object)))
  (actor
    (value (volitional-agent)))
  (object
    (value (pipe)))
  (instrumental-scene
    (value (propel
	     (actor
	       (value =actor))
	     (object
	       (value (door =x)))
	     (from
	       (value (at-location
			(domain
			  (value =x))
			(co-domain
			  (value (physical-location
				   (location-of
				     (value cupboard)))))))))))
  (goal-scene
    (value (atrans
	     (actor
	       (value =actor))
	     (object
	       (value =object))
;;; 	     (from
;;; 	       (value (cupboard)))
;;; 	     (to
;;; 	       (value =actor))
	     (main-result (value =main-result)))))
  (post-completion-scene
    (value (propel
	     (actor
	       (value =actor))
	     (object
	       (value =x))
	     (to
	       (value (at-location
			(domain
			  (value =x))
			(co-domain
			  (value (physical-location
				   (location-of
				     (value cupboard)))))))))))
  (scenes
    (value ( =instrumental-scene  =goal-scene =post-completion-scene)))
  (main-result 
    (value (controls
	     (domain (value =actor))
	     (co-domain (value =object)))))  )



(define-frame FILL-PIPE
	      (isa (value (mop)))
  (actor
    (value (volitional-agent)))
  (object
    (value (pipe)))
  (ingredients
    (value (inanimate-object)))
  (instrumental-scene
    (value (gain-control-of
	     (actor
	       (value =actor))
	     (object
	       (value =ingredients))))) 
  (goal-scene
    (value (tilt
	     (actor
	       (value =actor))
	     (object
	       (value =ingredients))
	     (to
	       (value (at-location
			(domain
			  (value =ingredients))
			(co-domain
			  (value =main-result))))))))
  (scenes
    (value (=instrumental-scene =goal-scene)))
  (main-result 
    (value (inside
	     (domain
	       (value =ingredients))
	     (co-domain
	       (value =object)))))
  )


(define-frame GAIN-CONTROL-OF
	      (isa (value (mop)))
  (actor
    (value (volitional-agent)))
  (object
    (value (entity)))
  )


;;; |||||| For the time being we are ignoring the fact that dcont is different
;;; for children under the age of 10 (they ask mom or dad for assistance).
;;; 
(define-frame GAIN-CONTROL-OF-CONTAINED-OBJECT
	      (isa (value (gain-control-of)))
  (actor
    (value (volitional-agent)))
  (object
    (value (physical-object)))
  (containing-object
    (value (closed-container
	     (door-to
	       (value (door =door))))))
  ;; |||||| We are also ignoring that precondition of having to know where the location of the object is.
;  (instrumental-scene
;    (value (gain-proximity-to
;	     (actor (value =actor))
;	     (object (value =actor))
;	     (to
;              (value (at-location
;			  (domain (value =object))))))))
  (instrumental-scene
    (value (open-container
	     (actor
	       (value =actor))
	     (object
	       (value =containing-object)))))
  (goal-scene
    (value (atrans
	     (actor
	       (value =actor))
	     (object
	       (value =object))
	     (to
	       (value =main-result)))))
  (post-completion-scene
    (value (close-container
	     (actor
	       (value =actor))
	     (object
	       (value =containing-object)))))
  (scenes
    (value (=instrumental-scene =goal-scene =post-completion-scene)))
  (main-result
    (value (control-state
	     (domain
	       (value =actor))
	     (co-domain
	       (value =object)))))
  )



(define-frame CLOSE-CONTAINER
	      (isa (value (mop)))
  (actor
    (value (volitional-agent)))
  (object
    (value (container)))
  (goal-scene
    (value (propel
	     (actor
	       (value =actor))
	     (object
	       (value (door =door)))
	     (to
	       (value (at-location
			(domain
			  (value =door))
			(co-domain
			  (value (near
				   (domain
				     (value =door))
				   (co-domain
				     (value =object)))))))))))
  (scenes
    (value (=goal-scene)))
  (main-result
    (value (open
	     (domain
	       (value =object))
	     (co-domain
	       (value false.0)))))
  )



(define-frame OPEN-CONTAINER
	      (isa (value (mop)))
  (actor
    (value (volitional-agent)))
  (object
    (value (container)))
  (goal-scene
    (value (propel
	     (actor
	       (value =actor))
	     (object
	       (value (door =door)))
	     (from
	       (value (at-location
			(domain
			  (value =door))
			(co-domain
			  (value (near
				   (domain
				     (value =door))
				   (co-domain
				     (value =object)))))))))))
  (scenes
    (value (=goal-scene)))
  (main-result
    (value (open
	     (domain (value =object))
	     (co-domain (value true.0)))))
  )


;;;
;;; |||||| Finish. Too simple right now. [23oct94]
;;; 
(define-frame GAIN-PROXIMITY-TO
	      (isa (value (mop)))
  (actor
    (value (volitional-agent)))
  (object
    (value (physical-object)))
  (goal-scene
    (value (ptrans
	     (actor
	       (value =actor))
	     (object
	       (value =actor))
	     (to
	       (value (at-location
			(domain
			  (value =actor))
			(co-domain
			  (value =main-result))))))))
  (scenes
    (value (=goal-scene)))
  ;; |||||| Should the main-result be the physical-location or the at-location.
  (main-result
    (value (physical-location
	     (domain
	       (value =actor))
	     (co-domain
	       (value =object)))))
  )



(define-frame GAIN-CONTROL-OF-OBJECT
	      (isa (value (gain-control-of)))
  (actor
    (value (volitional-agent)))
  (object
    (value (physical-object)))
  (instrumental-scene
    (value (gain-proximity-to
	     (actor
	       (value =actor))
	     (object
	       (value =object))
	     (to
	       (value (at-location
			(domain
			  (value =object))))))))
  (goal-scene
    (value (atrans
	     (actor
	       (value =actor))
	     (object
	       (value =object))
	     (to
	       (value =main-result)))))
  (scenes
    (value (=instrumental-scene =goal-scene)))
  (main-result
    (value (control-state
	     (domain
	       (value =actor))
	     (co-domain
	       (value =object)))))
  )




;;; -*- Mode: LISP; Syntax: Common-lisp; Package: REPRESENTATIONS; Base: 10 -*-

(in-package :reps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	      Meta-AQUA Background Knowledge Represented as Frames
;;;;
;;;;	    Copyright (C) 2013   Michael T. Cox   (mcox@cs.umd.edu)
;;;;
;;;;
;;;;                           File: rep_fire.lisp
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

;;; Note that in rep_scripts.lisp there exists a frame for lighting
;;; objects with a lighter.  This LIGHT-OBJECT frame would work well with an 
;;; arsonist XP.

(define-attribute-value GUI_MONTAG
  (isa (value (person))
       ))

(define-frame A_
  (isa (value (square))
       ))

(define-frame B_
  (isa (value (square))
       ))

(define-frame C_
  (isa (value (square))
       ))

(define-frame D_
  (isa (value (square))
       ))

(define-frame T_
  (isa (value (triangle))
       ))

(define-attribute-value NATURE.0
  (isa (value (nature))
       ))

(define-frame NATURE
  (isa (value (non-volitional-agent)))
  )

(define-attribute-value VERY-HOT.0   
  (isa  (value (temperature-value)))
  )

(define-attribute-value HOT.0   
  (isa  (value (temperature-value)))
  )

(define-attribute-value MEDIUM-TEMPERATURE.0   
  (isa  (value (temperature-value)))
  )

(define-attribute-value COLD.0   
  (isa  (value (temperature-value)))
  )

(define-attribute-value VERY-COLD.0   
  (isa  (value (temperature-value)))
  )


(define-frame NON-VOLITIONAL-AGENT
	      (isa (value (abstract-object)))
  )

(define-frame TEMPERATURE-VALUE
    (isa    (value  (physical-object-attribute-value)))
  )

(define-relation TEMPERATURE
    (isa            (value (physical-object-attribute)))
    (domain         (value (physical-object)))
    (co-domain      (value (temperature-value)))
    (slot           (value (temperature)))
    )


;;; Should be isa chemical substance I suppose, but this will do.
(define-frame OXYGEN
  (isa (value (physical-object)))
  )

;;; Not used yet.
(define-frame ARSON
    (isa (value (crime))))


(define-frame BLOCK
  (isa (value (toy)))
  )

(define-frame SQUARE
  (isa (value (block)))
  )

(define-frame TRIANGLE
  (isa (value (block)))
  )

;;; Placeholder for now. 
(define-frame PUTOUTFIRE
  (isa (value (mop)))
  (actor (value (volitional-agent)))
  (object (value (physical-object)))
  )

;;; Placeholder for now. 
(define-frame APPREHEND
  (isa (value (mop)))
  (actor (value (volitional-agent)))
  (object (value (volitional-agent)))
  )

;;; This definition is the same one as pick-up in rep_planner.lisp. I
;;; just removed the dash in the name.
(define-frame PICKUP
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


(define-frame PUTDOWN
  (isa (value (mop)))
  (actor (value (volitional-agent)))
  (object (value (physical-object)))
  (preconditions (value (possess
			  (domain (value =actor))
			  (co-domain (value =object)))))
  (goal-scene (value (ptrans
		       (actor (value =actor))
		       (object (value =object))
		       (to (value (at-location
				   (domain (value =object))
				   (co-domain
				    (value =main-result))))))))
  (main-result (value (near
			(domain (value =object))
			(co-domain (value =actor)))))
  (scenes (value (=goal-scene)))
  (success (value (boolean-value))); For recording whether the action met expectations.
  )

(define-frame STACK
  (isa (value (mop)))
  (actor (value (volitional-agent)))
  (object (value (block)))
  (recipient (value (block)))
  (preconditions (value (possess
			  (domain (value =actor))
			  (co-domain (value =object)))))
  (goal-scene (value (ptrans
		       (actor (value =actor))
		       (object (value =object))
		       (from (value (at-location
				   (domain (value =object))
				   ;;(co-domain
				   ;; (value =preconditions))
				   )))
		       (to (value (at-location
				   (domain (value =object))
				   (co-domain
				    (value =main-result))))))))
  (main-result (value (on
			(domain (value =object))
			(co-domain (value =recipient)))))
  (scenes (value (=goal-scene)))
  (success (value (boolean-value))); For recording whether the action met expectations.
  )



(define-frame UNSTACK
  (isa (value (mop)))
  (actor (value (volitional-agent)))
  (object (value (block)))
  (recipient (value (block)))
  (preconditions (value (on
			  (domain (value =object))
			  (co-domain (value =recipient)))))
  (goal-scene (value (ptrans
		       (actor (value =actor))
		       (object (value =object))
		       (from (value (at-location
				     (domain (value =object))
				     (co-domain
				      (value =preconditions)))))
		       (to (value (at-location
				     (domain (value =object))
				     ;;(co-domain
				     ;; (value =main-result))
				     ))))))
  (main-result (value (possess
			(domain (value =actor))
			(co-domain (value =object)))))
  (scenes (value (=goal-scene)))
  (success (value (boolean-value))); For recording whether the action met expectations.
  )






;;;
;;;
;;; STATES CAUSE ACTION BY NATURE.
;;;
;;; For example, "Heat + oxygen + fuel cause fire." 
;;;
(define-frame FORCED-BY-STATES
	      (isa (value (primitive-xp)))
  (actor (value (non-volitional-agent)	;Was nature.0
		))
  (object (value (physical-object)))
  (ante (value (model
		(frame-list (value (=heat =element =fuel))))))
  (conseq (value (burns
		  (actor (value =actor))
		  (object (value =object))
		  (main-result (value =result)))))
  (fuel (value (at-location (domain (value =object))
			    (co-domain (value (physical-location =l))))))
  (element (value (at-location (domain (value (oxygen)))
			    (co-domain (value =l)))))
  (heat (value (temperature (domain (value =object))
			    (co-domain (value very-hot.0)))))
  (result (value (burning (domain (value =object))
			  (co-domain (value true.0)))))
  (goal (value (burning 
		(domain (value =object))
		(co-domain (value false.0)))))
  (antecedent (value =ante))
  (consequent (value =conseq))
  (role (value (actor
		 (domain (value =conseq))
		 (co-domain (value =actor)))))
  (explains (value =role))
  (pre-xp-nodes (value (=actor =conseq =object =role)))
  (internal-nodes (value nil.0))
  (xp-asserted-nodes (value (=ante)))
  (link1 (value (results
		  (domain (value =ante))
		  (co-domain (value =conseq)))))
  (link2 (value (results
		  (domain (value =heat))
		  (co-domain (value =conseq)))))
  (link3 (value (results
		  (domain (value =element))
		  (co-domain (value =conseq)))))
  (link4 (value (results
		  (domain (value =fuel))
		  (co-domain (value =conseq)))))
  (links (value (=link1 =link2 =link3 =link4)))
  )


;;;
;;;
;;; Lighting an object with an ignition device results in the object
;;; having a high temperature. That is the action causes heat for the
;;; object.
;;;
(define-frame IGNITION-XP
	      (isa (value (primitive-xp)))
  (actor (value (volitional-agent)))
  (object (value (physical-object)))
  (ante (value (light-object
		(actor (value =actor))
		(instrumental-object
		 (value (ignition-device)))
		)))
  (conseq (value =heat))
  (heat (value (temperature (domain (value =object))
			    (co-domain (value very-hot.0)))))
  (antecedent (value =ante))
  (consequent (value =conseq))
  (role (value (actor
		 (domain (value =conseq))
		 (co-domain (value =actor)))))
  (explains (value =role))
  (pre-xp-nodes (value (=actor =conseq =object =role)))
  (internal-nodes (value nil.0))
  (xp-asserted-nodes (value (=ante)))
  (link1 (value (results		;results-in instead? If so fix the domain of the state that is the conseq of the results-in
		  (domain (value =ante))
		  (co-domain (value =conseq)))))
  (links (value (=link1)))
  )

(define-frame ARSONIST-XP
	      (isa (value (xp)))
  (actor (value (criminal-volitional-agent)))
  (object (value (physical-object)))
  (ante (value (ignition-xp
		(actor (value =actor))
		(object
		 (value =object))
		(ante (value (light-object =l-o
			      (actor (value =actor))
			      (instrumental-object
			       (value (ignition-device)))
			      )))
		(conseq (value =heat))
		)))
  (conseq (value (forced-by-states
		  (object (value =object))
		  (heat (value =heat))
		  (conseq (value (burns =b
				  (object (value =object))
				  )))
		  )))
  (heat (value (temperature (domain (value =object))
			    (co-domain (value very-hot.0)))))
  ;; Added [mcox 19-21nov13]
  ;; Note that the controlsstate is the main result of an arrest mop in rep_smuggle4
  ;; Commented out so GDA goal gen can handle instead [mcox 27jan14]
  ;;(goal (value (controls
		;(domain (value (authority)))
;;		(co-domain (value =actor))
;;		)))
  (antecedent (value =ante))
  (consequent (value =conseq))
  (role (value (actor
		 (domain (value =b))
		 ;;(co-domain (value =actor))
		 )))
  (explains (value =role))
  (pre-xp-nodes (value (=actor =conseq =object =role)))
  (internal-nodes (value nil.0))
  (xp-asserted-nodes (value (=ante)))
  (link1 (value (results
		  (domain (value =ante))
		  (co-domain (value =conseq)))))
  (link2 (value (xp-instrumental-scene->actor
		 (actor (value =actor))
		 (action (value =l-o))
		 (main-action (value =b))
		 (role (value =role)))))
  (links (value (=link1 =link2)))
  )

(define-frame BURNS
	      (isa (value (violent-mop)))
  (actor
    (value (non-volitional-agent)))
  (object
    (value (physical-object)))
  (goal-scene
    (value (ingest
	     (actor
	       (value =actor))
	     (object
	       (value =object))
	     )))
  (scenes
    (value (=goal-scene)))
  (main-result
    (value (burning
	     (domain (value =object))
	     )))
  )


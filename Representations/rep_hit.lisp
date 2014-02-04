;;; -*- Mode: LISP; Syntax: Common-lisp; Package: REPRESENTATIONS; Base: 10 -*-

(in-package :reps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	      Meta-AQUA Background Knowledge Represented as Frames
;;;;
;;;;	    Copyright (C) 1996   Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;                     File: rep_hit.lisp
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



(define-frame HIT				; Really punch a specialty of hit.
  (isa (value (violent-mop)))
  (actor (value (person)))
  (object (value (animate-object
		   (at-location (value
				  =l)))))
  (to (value (at-location
	       (domain (value =object)))))			; Added this slot. [cox]
  (instrumental-scene            ;;;overly-simplified
   (value (propel
	   (object (value (hand =h
				(body-part-of (value =actor)))))
	   (from (value (at-location
			  (domain (value =h))
			  (co-domain (value (location
					      (location-of (value =actor))))))))
	   (to (value 
 		 (at-location
		   (domain (value =h))
		   (co-domain (value
				(location =l
				       (location-of (value =object)))
				     )))
		 )))))
  (scenes (value (=instrumental-scene))))	; Really should be goal-scene.

;;; Placed in file 
;(putprop 'XP-INJURY-HIT
;	 *say-way*
;	 "People hit animate-objects when they want to hurt them.")

(define-frame XP-INJURY-HIT
  (isa (value (shallow-volitional-xp)))
  (actor (value (person)))
  (injured-object (value (physical-object)))
  (hitting (value (hit
		   (actor (value =actor))
		   (to (value (at-location
			       (domain (value =injured-object))))))))
  (role (value (actor
		(domain (value =hitting))
		(co-domain (value =actor)))))
  (cause-pain (value (hurt
		      (domain (value =injured-object))	; Added facet id value "value". [cox]
		      (co-domain (value true.0)))))	; Was (true). Also added facet id value "value".[cox]
  (explains (value =role))
  (pre-xp-nodes (value (=hitting =role)))
  (internal-nodes (value (=actor =injured-object)))	;Was not a list. [cox]
  (xp-asserted-nodes (value (=cause-pain)))	; Was a sigle item rather than a list. [cox]
  (link1 (value (results
		 (domain (value =hitting))
		 (co-domain (value =cause-pain)))))
  (links (value (=link1)))
)







(define-frame HANDBALL-COURT
  (isa (value
;;; 	 (physical-location)
	 (inanimate-object)			; Substituted physical-object so it would not be anomalous as
						; co-domain of outside frame.
	 )))

(define-frame HAVE-FUN
  (isa (value
	 (mop)					;Changed temporarily.
;;; 	 (mental-state)
	      )))

(define-relation HURT
  (isa (value (unary-relation)))
  (domain (value (volitional-agent)))
  (co-domain (value (boolean-value))))


(define-frame BREAK-OBJECT
	      (isa (value (violent-mop)))
  (actor (value (person)))
  (object (value (inanimate-object)))
  (instrumental-scene
    (value (propel
	     (actor (value =actor))
	     (object (value (hand =h
				  (body-part-of (value =actor)))))
	     (from (value (at-location
			    (domain (value =h))
			    (co-domain (value (near
						(domain (value =h))
						(co-domain (value =actor))
						))))))
	     (to (value (at-location
			  (domain (value =h))
			  (co-domain (value (physical-location
					      (domain (value =h))
					      (co-domain (value =object))))))))
	     )))
  (main-result (value (physical-state
			(domain (value =object))
			(co-domain (value broken.0))))))

(define-attribute-value BROKEN.0
			(isa (value (physical-state-value))))


(define-frame HOUSEHOLD-OBJECT
   (isa (value (inanimate-object))))

(define-frame LAMP
   (isa (value (household-object))))



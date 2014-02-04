;;; -*- Mode: LISP; Syntax: Common-lisp; Package: REPRESENTATIONS; Base: 10 -*-

(in-package :reps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	      Meta-AQUA Background Knowledge Represented as Frames
;;;;
;;;;	    Copyright (C) 1996   Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;                     File: rep_burglarscript.lisp
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


;;; GOBAT/D
;;;
;;; BURGLAR SCRIPT

(define-frame VALUABLES
    (isa (value (household-object)))
  (worth (value high.0)))


;;;
;;; Modeled after confiscation. See rep_smuggle4.lisp
(define-frame ROB
  (isa (value (criminal-act))) ; Should also be a police-act.
  (actor (value (criminal-volitional-agent)))
  (object (value (valuables)))
  (opponent (value (volitional-agent)))
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


(define-frame BURGLARIZE-SCRIPT
	      (isa (value (script)))
  (actor (value 
 	   (criminal-volitional-agent)
))
  (object (value (valuables)))
;  (instrumental-object (value (feet))) ; So what is the instrumental object of get near valuables
  (threatening-object (value (weapon)))
  (target (value (house)))
  (victim (value (volitional-agent))) ;Grandma
  ;; Get close to the target.
  (instrumental-scene
    (value (ptrans
	     (actor (value
		      (volitional-agent)	; Changed from =actor so script will be triggered [cox 14jul]
			   ))
	     (object (value =actor))
	     (to  (value (at-location
			(domain (value =actor))
			(co-domain
			  (value (near
				  (domain (value =actor))
				  (co-domain (value =target)))))))))))
  (scene1
    (value (ptrans 
	    (actor (value =actor))
	    (object (value =actor))
	    (from (value (at-location
			  (domain (value =actor))
			  (co-domain (value
				      (outside
				       (domain (value =actor))
				       (co-domain (value =target))))))))
	    (to (value (at-location
			(domain (value =actor))
			(co-domain (value
				    (inside
				     (domain (value =actor))
				     (co-domain (value =target))))))))
	    )
	   ))
; (scene2
;   (value (gain-control-of			;redundant. Part of light-object
;           (actor (value =actor))
;	    (object (value =lighting-object)))))
  (goal-scene
    (value (rob
	     (actor (value =actor))
	     (object (value =object))
	     (opponent (value =victim))
;	     (instrumental-object (value =threatening-object))
	     (main-result (value (controls =contr 
					   (domain (value =actor))
					   (co-domain (value =object)))))
	     (goal-scene
	      (value (atrans
		      (actor (value =actor))
		      (object (value =object))
		      (from  (value (controls
				     (domain (value =victim))
				     (co-domain (value =object)))))
		      (to (value =contr))
		      )))
;;;	     (goal-scene
;;;	       (value (gain-control-of
;;;			(actor (value =actor))
;;;			(object (value =object)))))
	     )))

  

  (post-completion-scene
    (value (ptrans
	    (actor (value =actor))
	    (from  (value (at-location
			(domain (value =actor))
			(co-domain
			  (value (inside
				  (domain (value =actor))
				  (co-domain (value =target))))))))
	    (to  (value (at-location
			(domain (value =actor))
			(co-domain
			  (value (outside
				  (domain (value =actor))
				  (co-domain (value =target))))))))
	    )))
  (scenes
    (value (=instrumental-scene  =scene1
;;; 			    =scene2
				 =goal-scene =post-completion-scene)))	  
  )



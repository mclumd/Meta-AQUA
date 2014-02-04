;;; -*- Mode: LISP; Syntax: Common-lisp; Package: REPRESENTATIONS; Base: 10 -*-

(in-package :reps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	      Meta-AQUA Background Knowledge Represented as Frames
;;;;
;;;;	    Copyright (C) 1996   Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;                     File: rep_cop_scripts.lisp
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
;;; DRUG BUST SCRIPTS
;;; COPS only (no k-9)
;;;
;;; Created 1-11-95 Mark Devaney
;;;
;;; Four separate "tracks" for bust stories to follow, each is own script
;;; ALL COP-AND-POTHEAD stories start out as :
;;;  1- cop enters kitchen from outside (ptrans (a cop) (t suspect-loc) (f outside))
;;;  2- cop asks elvis for pot          (mtrans (a cop) (t suspect) (o atrans)))

;;; Script #1 :  Suspect complies w/ request, hands over contraband and is arrested
;;; Script #2 :  Suspect complies w/ request, hands over contraband and is NOT arrested
;;; Script #3 :  Suspect does not comply, cop searches and finds contraband and suspect
;;;              is arrested.
;;; Script #4 :  Suspect does not comply, cop searches unsuccessfully and no arrest is made.

;;; COP-AND-POTHEAD   #1  - simple 
;;; Suspect hands over pot and  gets busted
(define-frame COPS-EASY-BUST-SCRIPT
	      (isa (value (script)))
  (actor (value (authority)))
  (object (value (contraband)))
  (suspect (value (volitional-agent)))
;;; ENTER LOCATION OF SUSPECT
;  (instrumental-scene
;    (value (ptrans
;	     (actor (value =actor))
;	     (object (value =actor))
;	     (from (value (at-location
;			    (domain (value =actor))
;			    (co-domain (value (outside)))
;			    )))
;	     (to (value (at-location
;			  (domain (value =actor))
;			  (co-domain (value (near
;					      (domain (value =actor))
;					      (co-domain (value =suspect)))))))))))
;;; ASK FOR CONTRABAND
  ;; Actually the cop asks him to hand it over (which is the act of scene1) [cox 16jan94]
  (instrumental-scene
    (value (mtrans
	     (actor (value =actor))
	     (object (value =scene1))
;;; 	     (from (value (at-location
;;; 			    (domain (value =actor)))))
;	     (to (value (at-location
;			  (domain (value (atrans))))))
	     )))
  
;;; SUSPECT HANDS OVER CONTRABAND
  (scene1
    (value (atrans
	     (actor (value =suspect))
	     (object (value =object))
	     (from (value (possess
			    (domain (value =suspect))
			    (co-domain (value =object)))))
	     (to (value (possess
			  (domain (value =actor))
			  (co-domain (value =object))))))))
  
;;; COP ARRESTS SUSECT
  (goal-scene
    (value (arrest
	     (actor (value =actor))
	     (object (value =suspect)))))
  
  (scenes (value (=instrumental-scene  =scene1 =goal-scene))))



;;; COP-AND-POTHEAD   #2  - simple 
;;; Suspect hands over contraband and doesn't get busted (not a large enough amount)
(define-frame COPS-LET-GO
	      (isa (value (script)))
  (actor (value (volitional-agent)))
  (object (value (contraband)))
  (suspect (value (volitional-agent)))
;;; ENTER LOCATION OF SUSPECT
  (instrumental-scene
    (value (ptrans
	     (actor (value =actor))
	     (object (value =actor))
	     (from (value (at-location
			    (domain (value =actor))
			    (co-domain (value (outside)))
			    )))
	     (to (value (at-location
			  (domain (value =actor))
			  (co-domain (value =suspect))))))))
;;; ASK FOR CONTRABAND
  (scene
    (value (mtrans
	     (actor (value =actor))
	     (object (value (atrans)))
	     (from (value (at-location
			    (domain (value =actor)))))
	     (to (value (at-location
			  (domain (value atrans))))))))
  
;;; ELVIS HANDS OVER CONTRABAND
  (scene2
    (value (atrans
	     (actor (value =suspect))
	     (object (value =object))
	     (from (value (at-location
			    (domain (value =object))
			    (co-domain (value =suspect)))))
	     (to (value (at-location
			  (domain (value =object))
			  (co-domain (value =actor))))))))
  
;;; COP LEAVES
  (goal-scene
    (value (ptrans
	     (actor (value =actor))
	     (object (value =actor))
	     (from (value (at-location
			    (domain (value =actor))
			    (co-domain (value =suspect)))))
	     (to (value (at-location
			  (domain (value =actor))
			  (co-domain (value outside))))))))
  (scenes (value (=instrumental-scene =scene1 =scene2 =goal-scene))))



;;; COP-AND-POTHEAD   #3
;;; Suspect does not hand over contraband, cop searches and finds it then arrests suspect

(define-frame COPS-SEARCH-AND-ARREST
	      (isa (value (script)))
  (actor (value (volitional-agent)))
  (object (value (contraband)))
  (suspect (value (volitional-agent)))
;;; ENTER LOCATION OF SUSPECT
  (instrumental-scene
    (value (ptrans
	     (actor (value =actor))
	     (object (value =actor))
	     (from (value (at-location
			    (domain (value =actor))
			    (co-domain (value (outside)))
			    )))
	     (to (value (at-location
			  (domain (value =actor))
			  (co-domain (value =suspect))))))))
;;; ASK FOR CONTRABAND
  (scene1
    (value (mtrans
	     (actor (value =actor))
	     (object (value (atrans)))
	     (from (value (at-location
			    (domain (value =actor)))))
	     (to (value (at-location
			  (domain (value atrans))))))))
;;; SEARCH FOR CONTRABAND (successfully)
  (scene2
    (value (ptrans
	     (actor (value =actor))
	     (object (value =actor))
	     (from (value (at-location
			    (domain (value =actor))
			    (co-domain (value =actor)))))
	     (to (value (at-location
			  (domain (value =actor))
			  (co-domain (value =object))))))))
	     
;;; COP ARRESTS SUSECT
  (goal-scene
    (value (arrest
	     (actor (value =actor))
	     (object (value =suspect)))))

  (scenes (value =instrumental-scene =scene1 =scene2 =goal-scene)))

;;; COP-AND-POTHEAD   #4
;;; Suspect does not hand over contraband, cop searches unsuccesfully and leaves

(define-frame COPS-SEARCH-DONT-FIND
	      (isa (value (script)))
  (actor (value (volitional-agent)))
  (object (value (contraband)))
  (suspect (value (volitional-agent)))
;;; ENTER LOCATION OF SUSPECT
  (instrumental-scene
    (value (ptrans
	     (actor (value =actor))
	     (object (value =actor))
	     (from (value (at-location
			    (domain (value =actor))
			    (co-domain (value (outside)))
			    )))
	     (to (value (at-location
			  (domain (value =actor))
			  (co-domain (value =suspect))))))))
;;; ASK FOR CONTRABAND
  (scene1
    (value (mtrans
	     (actor (value =actor))
	     (object (value (atrans)))
	     (from (value (at-location
			    (domain (value =actor)))))
	     (to (value (at-location
			  (domain (value atrans))))))))
  
;;; SEARCH FOR CONTRABAND (unsuccessfully)
  (scene2
    (value (ptrans
	     (actor (value =actor))
	     (object (value =actor)))))
  	     
;;; COP ARRESTS SUSECT
  (goal-scene
    (value (ptrans 
	     (actor (value =actor))
	     (object (value =suspect))
	     (to (value (at-location
			  (domain (value =actor))
			  (co-domain (value outside))))))))

  (scenes (value =instrumental-scene =scene1 =scene2 =goal-scene)))

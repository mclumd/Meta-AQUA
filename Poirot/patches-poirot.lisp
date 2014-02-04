;;; -*- Mode: LISP; Syntax: Common-lisp; Package: Meta-aqua; Base: 10 -*-

(in-package :meta-aqua)

;;; Add the scripts from rep_poirot.lisp.
(setf *known-script-names* 
  (cons 'vacation-script 
	(cons 'dining-script 
	      *known-script-names*)))


;;;; 
;;;; This file represents a set of modifications to existing Meta-AQUA code to
;;;; enable the POIROT/Meta-AQUA component.
;;;; 


;;; 
;;; Added underscore divisions to visually separate each story input from the
;;; rest of the output Meta-AQUA produces. [mcox 13nov06]
;;; 
;;; Function say-input is the first feedback the user receives for a new
;;; 'understands goal input into Meta-AQUA. It tells whether or not the goal
;;; originated outside the program or was a goal to understand the structure
;;; inferred by the script applier.
;;;
;;; ||||||Note that this needs to be fixed when the script applier starts to
;;; handle some of the hand-coded examples. Also, I am not sure what is
;;; happening with other modes (e.g., as with Meta-AQUA calls of ripsau) any
;;; longer. [cox 25feb95]
;;; 
(defun say-input (new-input spinqua-input?)
  ;; Was there not a function to do the following?
  ;; Yes - say-xp, but it was for xps and did not have the additional bagage.
  (format
   *aqua-window*
   "~%____________________________________________________________________________________________~%"
   )
  (format
   *aqua-window*
   (if
       (and
	spinqua-input?			; If the input from spinqua (Tale-Spin),
	(not				; then the structure is inferred if the 
	 (member new-input		; new input is not in the story concepts.
		 *Story-Concepts*
		 :test			; Cannot use this test if hand-coded input
		 #'(lambda (x y)	; since *story-concpets* does not have instantiated
		     (equal x (first y)) ; frame variable, but rather, has patterns with 
		     ))))		; variable bindings.
       "~%Inferred Structure: ~s"
     "~%Input Structure: ~s"
     )
   new-input)
  (format
   *aqua-window*
   "~%  ~s~%"
   (or (do-say new-input)
       (do-say (frame-type new-input))))
  (format
   *aqua-window*
   "~%____________________________________________________________________________________________~%"
   )
  )




(defun index-xp-prep ()
  (make-predefined-explanation
    'XP-INJURY-HIT
    'hit
    'person)
  (make-predefined-explanation
    'XP-INJURY-HIT
    'hit
    'adult)
  (make-predefined-explanation
    'XP-INJURY-HIT
    'hit
    'child)
  (make-predefined-explanation
    'XP-TYPICAL-ACTION-ANIMATES->ACTOR
    'sniff
    'dog)
  (make-predefined-explanation
    'XP-DEFENSIVE-BARK
    'dog-barks
    'dog)
  (make-predefined-explanation
    'XP-GOAL-OF-OUTCOME->ACTOR
    'smoke-pipe
    'adult)
  (make-predefined-explanation
    'XP-HUNGRY-BARK
    'seal-barks
    'seal)
  ;; Indexed the following XPs in memory for the midterm [mcox 25oct06]
  (make-predefined-explanation
    'XP-TYPICAL-ACTION-ANIMATES->ACTOR
    'LookupRequirements
    'person)
  (do-add-say
  'XP-INFO-PRECOND->ACTOR
  "Actor does action because it determines information needed by another action.")
  (make-predefined-explanation
    'XP-INFO-PRECOND->ACTOR
    'LookupAirport
    'person)
  (make-predefined-explanation
    'XP-INFO-PRECOND->ACTOR
    'LookupMission
    'person)
  (do-add-say
  'XP-RESERVATION-PRECOND->ACTOR
  "Actor does action because it achieves a reservation precondition of a subsequent action.")
  (make-predefined-explanation
    'XP-RESERVATION-PRECOND->ACTOR
    'ReserveSeat
    'person)
  ;; Indexed the following 3 XPs in memory for the homework [mcox 11aug06]
  (make-predefined-explanation
    'XP-TYPICAL-ACTION-ANIMATES->ACTOR
    'PatientPortType_PatientLookup
    'person)
  (do-add-say
  'XP-INSTRUMENTAL-SCENE->ACTOR2
  "Actor does action because it enables another more important action.")
  (make-predefined-explanation
    'XP-INSTRUMENTAL-SCENE->ACTOR2
    'HospitalLookupPortType_HospitalLookup
    'person)
  (make-predefined-explanation
    'XP-GOAL-OF-OUTCOME->ACTOR
    'TripReservePortType_TripReserve
    'person)
  (do-index
    (list
      (f.instantiate-frame IMXP-NOVEL-SITUATION-ALTERNATIVE-REFUTED)
      (f.instantiate-frame IMXP-NOVEL-SITUATION-ALTERNATIVE-REFUTED-NO-ANOMALY))
    'xp-type.0
    (f.instantiate-frame
      `(mentally-initiates
	 (domain
	   (,*value-facet*
	    (not-equal-relation)))
	 (co-domain
	   (,*value-facet*
	    (expectation-failure))))
      *predefined-instance*))
  (do-index
    (list (f.instantiate-frame IMXP-ANOMALY-EXPLAINED))
    'xp-type.0
    (f.instantiate-frame
      `(mentally-initiates
	 (domain
	   (,*value-facet*
	    (not-equal-relation)))
	 (co-domain
	   (,*value-facet*
	    (incorporation-failure))))
      *predefined-instance*))
  (do-index
    (list (f.instantiate-frame IMXP-ANOMALY-EXPLAINED))
    'xp-type.0
    (f.instantiate-frame
      `(mentally-initiates
	 (domain
	   (,*value-facet*
	    (equal-relation)))
	 (co-domain
	   (,*value-facet*
	    (successful-prediction))))
      *predefined-instance*))
  (do-index
    (list
       (f.instantiate-frame IMXP-ANOMALY-AND-BAFFLED)
      (f.instantiate-frame IMXP-BAFFLED-AND-RESOLVED)
;;;       (f.instantiate-frame IMXP-ANOMALY-AND-BAFFLED)
      )
    'xp-type.0
    (f.instantiate-frame
      `(mentally-initiates
	 (domain
	   (,*value-facet*
	    (truth)))
	 (co-domain
	   (,*value-facet*
	    (retrieval-failure))))
      *predefined-instance*))
  )



;;; 
;;; A concept is determined to be interesting if it is anomalous or if it is an
;;; explanation. As Schank has noted, it is also inherently interesting if it
;;; involves violence, sex, or loud noises. Finally, it is interesting if it is
;;; a concept about which the reasoner has learned something lately (in this
;;; case the concept will be marked as personally interesting).
;;;
;;; For example, if input concept is DOG-BARKS.25745 then anomaly-list would be
;;; '((OBJECT) (TO DOMAIN))
;;; 
(defun interesting-p (concept)
  (let ((anomaly-list (is-anomalous concept)))
    (do-break interesting-p)
    (cond (anomaly-list
	   (index-anomaly concept anomaly-list)
	   (let ((anomaly-frame
		   (f.instantiate-frame
		     `(anomaly
			(expected-outcome
			  (,*value-facet*
;			   ,(f.instantiate-frame
;			      (apply
;				#'f.chase-path
;				`(,(frame-type concept)
;				  ,@(first anomaly-list))))
			   ,(apply
			      #'f.chase-path
			      `(,(f.instantiate-frame
				   (frame-def concept))
				,@(first anomaly-list)))))
			(actual-outcome
			  (,*value-facet*
			   ,(apply
			      #'f.chase-path
			      `(,concept
				,@(first anomaly-list)))))
			(action
			  (,*value-facet* ,concept))
			(paths
			  (,*value-facet*
			   (literal ,anomaly-list)))))))
; 	     (f.set-literal
;	       (f.get anomaly-frame
;		      'paths)
;	       anomaly-list)
	     (set-anomaly-list
	       *Current-Result-Record*
	       anomaly-frame)
	     anomaly-frame))
	  (t
	   (or (and (isa-p 'violent-mop (list concept))
		    (null
		      (format
			*aqua-window*
			"~%~s is a violent action ... interesting.~%" concept))
		    (f.instantiate-frame
		      `(characterization
			 (,*domain-slot* (,*value-facet* ,concept))
			 (,*co-domain-slot* (,*value-facet* violent-mop.0)))))
	       (and (isa-p 'sexual-mop (list concept))
		    (null
		      (format
			*aqua-window*
			"~%~s is a sexual action ... interesting.~%" concept))
		    (f.instantiate-frame
		      `(characterization
			 (,*domain-slot* (,*value-facet* ,concept))
			 (,*co-domain-slot* (,*value-facet* sexual-mop.0)))))
	       (and (isa-p 'noisy-mop (list concept))
		    (null
		      (format
			*aqua-window*
			"~%~s is a noisy action ... interesting.~%" concept))
		    (f.instantiate-frame
		      `(characterization
			 (,*domain-slot* (,*value-facet* ,concept))
			 (,*co-domain-slot* (,*value-facet* noisy-mop.0)))))
	       (and (get (frame-type concept) 'personally-interesting)
		    (null
		      (format
			*aqua-window*
			"~%~s is personally interesting.~%" concept))
		    (f.instantiate-frame
		      `(characterization
			 (,*domain-slot* (,*value-facet* ,concept))
			 (,*co-domain-slot* (,*value-facet* personally-interesting.0)))))
	       ;; Modified function to determine SWS calls personally interesting. [mcox 11aug06]
	       (and (isa-p 'SWS-call (list concept))
		    (null
		      (format
			*aqua-window*
			"~%~s is a Semantic Web Service call.~%" concept))
		    (f.instantiate-frame
		      `(characterization
			 (,*domain-slot* (,*value-facet* ,concept))
			 (,*co-domain-slot* (,*value-facet* personally-interesting.0)))))
	       ;; Commented out so that explanatyions themselves are not explained. [cox 19mar95]
	       ;; |||||| May want to set back later. 
;	       (and (isa-p 'xp (list concept))
;		    (null
;		      (format
;			*aqua-window*
;			"~%~s is an explanation ... interesting.~%" concept))
;		    (f.instantiate-frame
;		      `(characterization
;			 (,*domain-slot* (,*value-facet* ,concept))
;			 (,*co-domain-slot* (,*value-facet* explanatory-struct.0))))
;;;; 		    (list concept (list 'xp))
;		    )
	       ))))
  )



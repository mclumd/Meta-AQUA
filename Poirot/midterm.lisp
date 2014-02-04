;;; -*- Mode: LISP; Syntax: Common-lisp; Package: Meta-aqua; Base: 10 -*-

(in-package :metaaqua)

;;;; 
;;;; This is the POIROT/Meta-AQUA limited Midterm example (story 22). This file
;;;; represents a stub that creates a Meta-AQUA frame representation for the
;;;; compact semantic trace listed in the variable *base-limited-midterm*.
;;; 


;;; Nethier this variable nor the next is actually used by Meta-AQUA
;;; yet. Instead the content of *base-limited-midterm* is encoded in the
;;; set-story-22 function that assigns the input to Meta-AQUA. Set-story
;;; functions are the normal way hand-coded examples get into Meta-AQUA for
;;; interpretation.
;;; 

;;; 
;;; The following is the basic, limited, midterm, example trace.  
;;; 
(defvar *base-limited-midterm*
  '(;;Prep
    ((lookupRequirements S42) 
     ((887924789 Priority 869 null US 24h SBAGH HLSTU)))
    ;;Set mission start
    ((lookupAirport SBAGH 300) 
     ((ORBI 20 "Baghdad International Airport")))
    ;;Set mission destination
    ((lookupAirport HLSTU 300) 
     ((ETAR 4 "Ramstein AB")))
    ;;Link mission
    ((lookupMission ORBI ETAR 24h 30) 
     ((M-001 C17-002 54 0 ORBI ETAR 291 10h 20h null)))
    ((reserveSeat M-001 887924789)(R3)))
  )

(defvar *limited-midterm*
  '(;;Prep
    ((lookupRequirements S42) 
     ((887924789 Priority 869 null US 24h SBAGH HLSTU)))
    ;;Set mission start
    ((lookupAirport SBAGH 300) 
     ((ORBI 20 "Baghdad International Airport")))
    ((setPatientAPOE 887924789 ORBI) 
     (True))
    ((getArrivalTime 887924789 SBAGH ORBI) 
     (15 30))
    ((setPatientAvailable 887924789 30)
     (True))
    ;;Set mission destination
    ((lookupAirport HLSTU 300) 
     ((ETAR 4 "Ramstein AB")))
    ((setPatientAPOD 887924789 ETAR) 
     (True))
    ;;Link mission
    ((lookupMission ORBI ETAR 24h 30) 
     ((M-001 C17-002 54 0 ORBI ETAR 291 10h 20h null)))
    ((getArrivalTime 887924789 ETAR HLSTU 20h) 
     (1205 1215)) ;; 1205 minutes
    ((setPatientMission 887924789 M-001)
     (True))
    ((reserveSeat M-001 887924789)(R3)))
  )


;;; POIROT
;;; 
;;; Midterm Example 
;;; 
;;; This will be the Ramstein mission example in isolation. 
;;; 
(defun set-story-22 ()
  (init-story
    '(
      ((LookupRequirements
	(actor (value (person)))
	(object (value S42.0))
	;;Only one result for now.
	(main-result (value ((RequirementRec
			      (patient-slot (value PATIENT4.0))
			      (triage-code (value PRIORITY.0))
			      (wound-type (value C869.0))
			      (LAT-slot (value TWENTY-FOUR.0))
			      (from (value SBAGH.0))
			      (to (value HLSTU.0))
			      ))))
	     )
       "The expert looks up the requirements")

      ((achievement-goal
	(goal-actor (value (person)))
	(goal-object (value (at-location
			     (domain (value PATIENT4.0))
			     (co-domain (value (inside
						(domain (value PATIENT4.0))
						(co-domain (value HLSTU.0)))))))))
       "The goal is to get the patient to the destination")

      ((LookupAirport
	(actor (value (person)))
	(object (value SBAGH.0))
	(radius (value THREE-HUNDRED.0))
	(main-precondition (value 
			    (from
			     (domain (value (RequirementRec)))
			     (co-domain (value =object)))
;;			    (near 
;;			     (domain (value PATIENT4.0))
;;			     (co-domain (value SBAGH.0)))
			    ))
	(main-result (value (distance-from
			     (domain (value ORBI.0))
			     (co-domain (value TWENTY.0)))))
	     )
       "The expert performs an airportLookup on the Baghdad staging area")

      ((LookupAirport
	 (actor (value (person)))
	 (object (value HLSTU.0))
	(radius (value THREE-HUNDRED.0))
	(main-precondition (value 
			    (to
			     (domain (value (RequirementRec)))
			     (co-domain (value =object)))
			    ))
	(main-result (value (distance-from
			      (domain (value ETAR.0))
			      (co-domain (value FOUR.0)))))
	     )
       "Then the expert performs an airportLookup on Landstuhl Regional Medical Center")

      ((LookupMission
	(actor (value (person)))
	(instrument (value c17-002.0))
	;; Object here is odd role, because it is the result of the SWS call 
	;; as well as the direct object of the "verb." Ditto on instrument.
	(object (value (mission
			(id (value M-001.0))
			(instrument (value =instrument)))))
	(from (value (at-location
		      (domain (value ORBI.0))
		      (co-domain (value (physical-location))))
		     ))
	(to (value (at-location
		    (domain (value ETAR.0))
		    (co-domain (value (physical-location))))
		   ))
	(LAT-slot (value TWENTY-FOUR.0))
	;; Needs to return a complex object with C17-002.0 etc.
	(main-result (value (instrument 
			     (domain (value =object))
			     (co-domain (value= instrument)))
			    ))
	(side-effect (value (arrival-time
			     (domain (value =object))
			     (co-domain (value twenty.0)))
			    ))
	)
       "Next she looks for a mission that linked the two")

      ;; Really object should be R3 abstract object as per semantic trace.
      ((ReserveSeat
	(actor (value (person)))
	(object (value M-001.0))
	(recipient (value PATIENT4.0))
	(main-result (value (is-reserved 
			     (domain (value =recipient))
			     (co-domain (value =object)))))
	)
       "Finally she reserves a seat for the patient on the mission")
      )
    ))



;;; 
;;; This is simply the final service call in story 22. I use it for debugging.
;;; 
(defun set-story-23 ()
  (init-story
    '(
      ((achievement-goal
	(goal-actor (value (person)))
	(goal-object (value (at-location
			     (domain (value PATIENT4.0))
			     (co-domain (value (inside
						(domain (value PATIENT4.0))
						(co-domain (value HLSTU.0)))))))))
       "The goal is to get the patient to the destination")

      ;; Really object should be R3 abstract object as per semantic trace.
      ((ReserveSeat
	(actor (value (person)))
	(object (value M-001.0))
	(recipient (value PATIENT4.0))
	(main-result (value (is-reserved 
			     (domain (value =recipient))
			     (co-domain (value =object)))))
	)
       "Finally she reserves a seat for the patient on the mission")
      )
    ))





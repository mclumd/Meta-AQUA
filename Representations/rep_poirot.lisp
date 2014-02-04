;;; -*- Mode: LISP; Syntax: Common-lisp; Package: Representations; Base: 10 -*-

(in-package :reps)

;;;; 
;;;; This file defines the Meta-AQUA frame representations to support
;;;; interpretation and explanation of the POIROT trace.
;;;; 


;;; 
;;; The NULL value seems to arise when service calls fail and at other weird
;;; times. For now use the following as modeled on nil.0. See the value of
;;; *nil* in file exported-symbols.lisp in the Frames Package.
;;; 
(define-attribute-value NULL.0 
    (isa (value (attribute-value))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Additions to Meta-AQUA's upper-level ontology
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
;;; Misc New relations
;;; 

(define-relation DISTANCE-FROM
    (isa     (value (spatial-relation)))
  (domain    (value (physical-object)))
  (co-domain (value (integer-value)))
  )

(define-relation IS-RESERVED
    (isa       (value (unary-relation)))
  (domain    (value (volitional-agent)))
  (co-domain (value (integer-value)))
  )


(define-relation IS-BOOKED
    (isa       (value (unary-relation)))
  (domain    (value (physical-object)))
  (co-domain (value (boolean-value)))
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Additional attribute value instances
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; 
;;; Now the reason we need abstract-state-values above.
;;; 
;;; A triageCode is an abstract-state relationship between the PatientReqRecord
;;; abstract-object and a triage-value such as URGENT.0. Thus the frame
;;; definition for PatientReqRecord has a slot called triageCode. See below.
;;; 

(define-frame triage-value
    (isa (value (abstract-state-value)))
  )

(define-attribute-value PRIORITY.0
    (isa (value (triage-value)))
  )


(define-attribute-value URGENT.0
    (isa (value (triage-value)))
  )


(define-attribute-value ROUTINE.0
    (isa (value (triage-value)))
  )


(define-attribute-value SURGICAL.0
    (isa (value (triage-value)))
  )


(define-attribute-value CONVENIENCE.0
    (isa (value (triage-value)))
  )



;;; 
;;; Patients identifications
;;; 

(define-attribute-value |3_PBAG.0|
    (isa (value (patient)))
  )

(define-attribute-value |2_PBAG.0|
    (isa (value (patient)))
  )

(define-attribute-value |1_TMP2.0|
    (isa (value (patient)))
  )

(define-attribute-value |1_PBAG.0|
    (isa (value (patient)))
  )

(define-attribute-value |1_IRS2.0|
    (isa (value (patient)))
  )

(define-attribute-value |1_HBAGH.0|
    (isa (value (patient)))
  )

;;; Patients identified by social security number.
(define-attribute-value |812876409.0|
    (isa (value (patient)))
  )

(define-attribute-value |814411200.0|
    (isa (value (patient)))
  )

(define-attribute-value |821882113.0|
    (isa (value (patient)))
  )

(define-attribute-value |822988466.0|
    (isa (value (patient)))
  )

(define-attribute-value |823665454.0|
    (isa (value (patient)))
  )

(define-attribute-value |823827847.0|
    (isa (value (patient)))
  )

(define-attribute-value |823948883.0|
    (isa (value (patient)))
  )

(define-attribute-value |829873498.0|
    (isa (value (patient)))
  )

(define-attribute-value |834465277.0|
    (isa (value (patient)))
  )

(define-attribute-value |865774332.0|
    (isa (value (patient)))
  )

(define-attribute-value |888399444.0|
    (isa (value (patient)))
  )

(define-attribute-value |888766322.0|
    (isa (value (patient)))
  )

(define-attribute-value |898247923.0|
    (isa (value (patient)))
  )

(define-attribute-value |898594432.0|
    (isa (value (patient)))
  )

;;; Original 6 patients follow.
(define-attribute-value |887924789.0|
    (isa (value (patient)))
  )

(define-attribute-value |887927651.0|
    (isa (value (patient)))
  )

(define-attribute-value |829876593.0|
    (isa (value (patient)))
  )

(define-attribute-value |838765532.0|
    (isa (value (patient)))
  )

(define-attribute-value |898287654.0|
    (isa (value (patient)))
  )

(define-attribute-value |867773214.0|
    (isa (value (patient)))
  )


;;; 
;;; Where was this used again?
;;; 
(define-attribute-value US.0
    (isa (value (nation)))
  )


;;; 
;;; Wound Types.
;;; 

;;; This numerical coding is no longer valid.
(define-attribute-value |869.0|
    (isa (value (physical-state-value)))
  )

;;; These types spelled out are also obsolete but kept for backward
;;; compatibility with older tests.
(define-attribute-value Laceration.0
    (isa (value (physical-state-value)))
  )

(define-attribute-value Fracture.0
    (isa (value (physical-state-value)))
  )

(define-attribute-value Internal.0
    (isa (value (physical-state-value)))
  )


;;; Amputation
(define-attribute-value AMP.0
    (isa (value (physical-state-value)))
  )

;;; Second Degree Burns
(define-attribute-value BURN2.0
    (isa (value (physical-state-value)))
  )

;;; Third Degree Burns
(define-attribute-value BURN3.0
    (isa (value (physical-state-value)))
  )

;;; Cholera
(define-attribute-value CHOL.0
    (isa (value (physical-state-value)))
  )

;;; Contusion
(define-attribute-value CNT.0
    (isa (value (physical-state-value)))
  )

;;; Concussion
(define-attribute-value CONC.0
    (isa (value (physical-state-value)))
  )

;;; Medical-Psych-Dental Exam
(define-attribute-value EXAM.0
    (isa (value (physical-state-value)))
  )

;;; Cold-Flu
(define-attribute-value FLU.0
    (isa (value (physical-state-value)))
  )

;;; Bone Fracture
(define-attribute-value FRAC.0
    (isa (value (physical-state-value)))
  )

;;; GastroIntestinal
(define-attribute-value GAS.0
    (isa (value (physical-state-value)))
  )

;;; Gunshot Wound
(define-attribute-value GSW.0
    (isa (value (physical-state-value)))
  )

;;; Internal Injury
(define-attribute-value INT.0
    (isa (value (physical-state-value)))
  )

;;; Laceration
(define-attribute-value LAC.0
    (isa (value (physical-state-value)))
  )

;;; Pregnancy-Delivery
(define-attribute-value PREG.0
    (isa (value (physical-state-value)))
  )

;;; Tuberculosis
(define-attribute-value TB.0
    (isa (value (physical-state-value)))
  )

;;; Combinations.

(define-attribute-value |BURN3, CHOL.0|
    (isa (value (physical-state-value)))
  )

(define-attribute-value |INT, TB.0|
    (isa (value (physical-state-value)))
  )


;;; 
;;; Nationalities. 
;;; 

(define-attribute-value MIL.0
    (isa (value (attribute-value)))
  )

(define-attribute-value DIP.0
    (isa (value (attribute-value)))
  )

(define-attribute-value CON.0
    (isa (value (attribute-value)))
  )

(define-attribute-value COA.0
    (isa (value (attribute-value)))
  )

(define-attribute-value EPW.0
    (isa (value (attribute-value)))
  )

(define-attribute-value TCN.0
    (isa (value (attribute-value)))
  )

(define-attribute-value CIV.0
    (isa (value (attribute-value)))
  )


;;; 
;;; Special Needs values
;;; 

;;; Altitude Restriction 10000ft
(define-attribute-value ALT10.0
    (isa (value (attribute-value)))
  )

;;; Altitude Restriction 15000ft
(define-attribute-value ALT15.0
    (isa (value (attribute-value)))
  )

;;; Altitude Restriction 20000ft
(define-attribute-value ALT20.0
    (isa (value (attribute-value)))
  )

;;; Altitude Restriction 5000ft
(define-attribute-value ALT5.0
    (isa (value (attribute-value)))
  )

;;; Orthopedic Braces
(define-attribute-value BRC.0
    (isa (value (attribute-value)))
  )

;;; Ditto
(define-attribute-value BRACE.0
    (isa (value (attribute-value)))
  )

;;; Critical Care Air Transport Team
(define-attribute-value CCATT.0
    (isa (value (attribute-value)))
    )

;;; Defibrillator
(define-attribute-value DEF.0
    (isa (value (attribute-value)))
  )

;;; Incubator
(define-attribute-value INC.0
    (isa (value (attribute-value)))
  )

;;; IV Controller
(define-attribute-value IVC.0
    (isa (value (attribute-value)))
  )

;;; Vital Signs Monitor
(define-attribute-value MON.0
    (isa (value (attribute-value)))
  )

;;; Oxygen Analyzer
(define-attribute-value OAN.0
    (isa (value (attribute-value)))
  )

;;; Pulse Oximeter
(define-attribute-value POX.0
    (isa (value (attribute-value)))
  )

;;; Restraints
(define-attribute-value RES.0
    (isa (value (attribute-value)))
  )

;;; Stryker Frame
(define-attribute-value STR.0
    (isa (value (attribute-value)))
  )

;;; Suction Apparatus
(define-attribute-value SXN.0
    (isa (value (attribute-value)))
  )

;;; Traction Appliance
(define-attribute-value TRK.0
    (isa (value (attribute-value)))
  )

;;; Ventilator
(define-attribute-value VEN.0
    (isa (value (attribute-value)))
  )

;;; Combos

(define-attribute-value |MON, RES.0|
    (isa (value (attribute-value)))
  )

(define-attribute-value |TRK, VEN.0|
    (isa (value (attribute-value)))
  )

(define-attribute-value |IVC, DEF, ALT5.0|
    (isa (value (attribute-value)))
  )



;;; Note that both time-value and time-attribute-value exists in tspin.lisp.
;;; Comment these out soon because now defined dynamically.

;;; The indeterminate time used in AssetQueryRecords.
(define-attribute-value |--:--.0|
    (isa (value (time-value)))
  )

(define-attribute-value |2 00:00.0|
    (isa (value (time-value)))
  )

(define-attribute-value |1 00:00.0|
    (isa (value (time-value)))
  )

(define-attribute-value |1 06:00.0|
    (isa (value (time-value)))
  )

(define-attribute-value |0 24:00.0|
    (isa (value (time-value)))
  )

(define-attribute-value |0 04:51.0|
    (isa (value (time-value)))
  )

(define-attribute-value |0 10:00.0|
    (isa (value (time-value)))
  )

(define-attribute-value |0 14:51.0|
    (isa (value (time-value)))
  )


(define-attribute-value C130J-001.0
    (isa (value (plane)))
  )

(define-attribute-value C130J-002.0
    (isa (value (plane)))
  )

(define-attribute-value C130J-003.0
    (isa (value (plane)))
  )

(define-attribute-value C130J-004.0
    (isa (value (plane)))
  )

(define-attribute-value C130J-005.0
    (isa (value (plane)))
  )

(define-attribute-value C17-001.0
    (isa (value (plane)))
  )

(define-attribute-value C17-002.0
    (isa (value (plane)))
  )

(define-attribute-value C23A-001.0
    (isa (value (plane)))
  )

(define-attribute-value C23A-002.0
    (isa (value (plane)))
  )

(define-attribute-value C23A-003.0
    (isa (value (plane)))
  )

(define-attribute-value C23A-006.0
    (isa (value (plane)))
  )

(define-attribute-value C23A-009.0
    (isa (value (plane)))
  )

(define-attribute-value C5-001.0
    (isa (value (plane)))
  )


;;; Note plane is defined likewise in rep_planner.lisp
(define-frame HELICOPTER
  (isa (value (vehicle)))
  )

;;; Blackhawk really
(define-attribute-value UH60A-001.0
    (isa (value (helicopter)))
  )

(define-attribute-value UH60A-002.0
    (isa (value (helicopter)))
  )

(define-attribute-value UH60A-003.0
    (isa (value (helicopter)))
  )



(define-attribute-value M-001.0
    (isa (value (abstract-object)))
  )



(define-attribute-value S42.0
    (isa (value (abstract-object)))
  )



;;; Strings such as "KARLSRUHE AAF" should really be literal frames. Change
;;; eventually. [mcox 14dec06]
;;; 
;;;(define-attribute-value |KARLSRUHE AAF.0|
;;;    (isa (value (name-category)))
;;;  )

(define-attribute-value EDIL.0
    (isa (value (airport)))
  )

;;;(define-attribute-value |RAMSTEIN AB.0|
;;;    (isa (value (name-category)))
;;;  )

(define-attribute-value ETAR.0
    (isa (value (airport)))
  )

;;;(define-attribute-value |HEIDELBERG AAF.0|
;;;    (isa (value (name-category)))
;;;  )

(define-attribute-value ETIE.0
    (isa (value (airport)))
  )

;;;(define-attribute-value |ANDREWS AFB.0|
;;;    (isa (value (name-category)))
;;;  )

(define-attribute-value KADW.0
    (isa (value (airport)))
  )

;;;(define-attribute-value |PHILLIPS AAF.0|
;;;    (isa (value (name-category)))
;;;  )

(define-attribute-value KAPG.0
    (isa (value (airport)))
  )

;;;(define-attribute-value |DAVISON AAF.0|
;;;    (isa (value (name-category)))
;;;  )

(define-attribute-value KDAA.0
    (isa (value (airport)))
  )

;;;(define-attribute-value |ALI AL SALEM AIR BASE.0|
;;;    (isa (value (name-category)))
;;;  )

(define-attribute-value OKAS.0
    (isa (value (airport)))
  )
 
;;;(define-attribute-value |KUWAIT INTL.0|
;;;    (isa (value (name-category)))
;;;  )

(define-attribute-value OKBK.0
    (isa (value (airport)))
  )

;;;(define-attribute-value |ERBIL NORTHWEST.0|
;;;    (isa (value (name-category)))
;;;  )

(define-attribute-value ORBA.0
    (isa (value (airport)))
  )

;;;(define-attribute-value |BALAD SE.0|
;;;    (isa (value (name-category)))
;;;  )

(define-attribute-value ORBD.0
    (isa (value (airport)))
  )

;;;(define-attribute-value HABBANIYAH.0
;;;    (isa (value (name-category)))
;;;  )

(define-attribute-value ORBH.0
    (isa (value (airport)))
  )

;;;(define-attribute-value |BAGHDAD INTL.0|
;;;    (isa (value (name-category)))
;;;  )

(define-attribute-value ORBI.0
    (isa (value (airport)))
  )

;;;(define-attribute-value KIRKUK.0
;;;    (isa (value (name-category)))
;;;  )

(define-attribute-value ORBK.0
    (isa (value (airport)))
  )

;;;(define-attribute-value MOSUL.0
;;;    (isa (value (name-category)))
;;;  )

(define-attribute-value ORBM.0
    (isa (value (airport)))
  )

;;;(define-attribute-value |BASRAH INTL.0|
;;;    (isa (value (name-category)))
;;;  )

(define-attribute-value ORMM.0
    (isa (value (airport)))
  )

;;;(define-attribute-value |TIKRIT EAST.0|
;;;    (isa (value (name-category)))
;;;  )

(define-attribute-value ORTK.0
    (isa (value (airport)))
  )

;;;(define-attribute-value |ALI BASE.0|
;;;    (isa (value (name-category)))
;;;  )

(define-attribute-value ORTL.0
    (isa (value (airport)))
  )


;;; Really the following are strings that represent values for hasResultMessage
;;; parts of success records.
#|
(define-attribute-value |APOE SET SUCCESSFULLY.0|
    (isa (value (name-category)))
  )

(define-attribute-value |AVAILABLE SET SUCCESSFULLY.0|
    (isa (value (name-category)))
  )

(define-attribute-value |APOD SET SUCCESSFULLY.0|
    (isa (value (name-category)))
  )

(define-attribute-value |RESERVATION SUCCESSFULLY MADE.0|
    (isa (value (name-category)))
  )

(define-attribute-value |NO MATCHING MISSIONS FOUND..0|
    (isa (value (name-category)))
  )

(define-attribute-value |MISSION ID SET SUCCESSFULLY.0|
    (isa (value (name-category)))
  )

(define-attribute-value |MISSION 5 CREATED SUCCESSFULLY.0|
    (isa (value (name-category)))
  )
|#


;;; Baghdad Irish Road Zone 1
(define-attribute-value IRS1.0
    (isa (value (staging-area)))
  )

;;; Baghdad Irish Road Zone 2
(define-attribute-value IRS2.0
    (isa (value (staging-area)))
  )

;;; Baghdad Irish Road Zone 3
(define-attribute-value IRS3.0
    (isa (value (staging-area)))
  )

;;; Baghdad Patient Holding
(define-attribute-value PBAG.0
    (isa (value (staging-area)))
  )

;;; Fallujah Patient Holding
(define-attribute-value PFAL.0
    (isa (value (staging-area)))
  )

;;; Titrik Patient Holding
(define-attribute-value PTIK.0
    (isa (value (staging-area)))
  )

;;; Baghdad Tampa Road Zone 1
(define-attribute-value TMP1.0
    (isa (value (staging-area)))
  )

;;; Baghdad Tampa Road Zone 2
(define-attribute-value TMP2.0
    (isa (value (staging-area)))
  )

;;; Baghdad Tampa Road Zone 3
(define-attribute-value TMP3.0
    (isa (value (staging-area)))
  )



;;; Which hospital is this???
(define-attribute-value HLSTU.0
    (isa (value (hospital)))
  )

;;; Ali Base Combat Support Hospital
(define-attribute-value HALIB.0
    (isa (value (staging-area)))
  )

;;; Arifjan Navy Expeditionary Medical Facility
(define-attribute-value HARIF.0
    (isa (value (staging-area)))
  )

;;; Baghdad Army Combat Support Hospital
(define-attribute-value HBAGH.0
    (isa (value (hospital)))
  )

;;; Balad Air Force Theater Hospital
(define-attribute-value HBALAD.0
    (isa (value (hospital)))
  )

;;; Baghdad Emergency Medical Services
(define-attribute-value HBIAP.0
    (isa (value (hospital)))
  )

;;; Kuwait City Hospital
(define-attribute-value HKUWT.0
    (isa (value (hospital)))
  )

;;; Landstuhl Regional Medical Center
(define-attribute-value HLMRC.0
    (isa (value (hospital)))
  )

;;; Mosul Army Combat Support Hospital
(define-attribute-value HMOSL.0
    (isa (value (hospital)))
  )

;;; Tikrit East Army Combat Support Hospital
(define-attribute-value HTIKR.0
    (isa (value (hospital)))
  )

;;; Walter Red Army Medical Center
(define-attribute-value HWRAMC.0
    (isa (value (hospital)))
  )



;;; Do not really need a separate category of health-state and
;;; health-state-value.


(define-attribute-value C869.0
    (isa (value (physical-state-value)))
  )


(define-attribute-value LIGHT-BURNS.0
    (isa (value (physical-state-value)))
  )


(define-attribute-value SEVERE-BURNS.0
    (isa (value (physical-state-value)))
  )

(define-attribute-value PATIENT1.0
    (isa (value (patient)))
  )

(define-attribute-value PATIENT4.0
    (isa (value (patient)))
  )

;;; Using location as type may be problematic.
(define-attribute-value LOCATION-P__2.0
    (isa (value (location)))
  )

(define-attribute-value LOCATION-H__1.0
    (isa (value (location)))
  )

(define-attribute-value LOCATION-H__3.0
    (isa (value (location)))
  )


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

(define-attribute-value FIFTY-THREE.0
    (isa (value (integer-value)))
  )

(define-attribute-value FIFTY-FOUR.0
    (isa (value (integer-value)))
  )

(define-attribute-value ONE-HUNDRED.0
    (isa (value (integer-value)))
  )

(define-attribute-value THREE-HUNDRED.0
    (isa (value (integer-value)))
  )

;;; Now to handle the real-valued numbers input as strings... argh!!
(define-attribute-value |100.0|
    (isa (value (attribute-value)))
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


(define-frame RESTAURANT
    (isa (value (building))))


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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The definitions for records returned by various services
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Returned by LookupRequirements
(define-frame PatientReqRecord
    (isa (value (abstract-object)))
  (patientID (value (patient)))
  (origin (value (inanimate-object)))
  (destination (value (inanimate-object)))
  (triageCode (value (triage-value)))
  ;;Added [mcox 23jan07]
  (specialNeedsCode (value (attribute-value)))
  (woundType (value (physical-state-value)))
  (LAT (value (time-value)))
  ;;Added [mcox 20feb07]
  (readyForTransport (value (time-value)))
  ;;Should have a more specific type [mcox 15dec06]
  (nationality (value (attribute-value)))
  )

;;; Returned by LookupAirport
(define-frame LUAirportRecord
    (isa (value (abstract-object)))
  (airportName (value 
		(literal)
		;;(name-category)
		      ))
  (distance (value 
	     ;;Hack for floats
	     (entity)
	     ;;(integer-value)
	     ))
  (airportLocationID (value (inanimate-object)))
  )


;;; Returned by lookupAsset
(define-frame AssetQueryRecord
    (isa (value (abstract-object)))
  (airportName (value 
		(literal)
		;;(name-category)
		))
  (assetID (value (plane)))		;"C17-002"
  (APOD (value (airport)))		; "KADW"
  (APOE (value (airport)))		; "ORBD"
  (startsFromLocation (value (airport))) ; "ETAR"
  (endsAtLocation (value (airport)))	; "ETAR"
  (mustDepartBy (value (time-value)))	; "--:--"
  (endsAtTime (value (time-value)))	; "--:--"
  (travelTime (value (time-value)))	; "0 12:04"
  (availableToDepart (value (time-value))) ; "0 04:19"
  (PAX (value (integer-value)))		; 0
  (maxPAX (value (integer-value)))	; 54
  )

;;; Returned by lookupMission
(define-frame MissionInfoRecord
    (isa (value (abstract-object)))
  (missionID (value (integer-value)))
  (APOD (value (airport)))
  (APOE (value (airport)))
  (departureTime (value (time-value)))
  (arrivalTime (value (time-value)))
  (assetID (value (plane)))
  (specialNeeds (value (attribute-value)))
  (PAX (value (integer-value)))
  (maxPAX (value (integer-value)))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The suite of Semantic Web Service Calls
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-frame lookupRequirements
    (isa  (value (SWS-call)))
  (actor (value (volitional-agent)))
  (object (value (abstract-object)))
  (main-result (value ((PatientReqRecord))))
  ;;Perhaps make =main-result instead?
  (return-value (value ((PatientReqRecord))))
  )


(define-frame lookupAirport
    (isa  (value (SWS-call)))
  (actor (value (volitional-agent)))
  (object (value =locationID))
  (locationID (value (inanimate-object)))
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
 (main-result (value (distance-from)
		     ))
 (return-value (value ((LUAirportRecord))))
  )


;;; 
;;; Success records are value pairs used in many service results. The following
;;; is a current list. setPatientAPOE, setPatientAPOD, setPatientAvailable, 
;;; setPatientMission, createMission, initializeTentativeMission, and
;;; setTentativeMissionDepartureTime.
;;; 
(define-frame Success
    (isa  (value (abstract-object)))
 (hasResultType (value (integer-value)))
 (hasResultMessage (value 
		    (literal)
		    ;;(name-category)
			  )))


(define-frame setPatientAPOE
    (isa  (value (SWS-call)))
  (actor (value (volitional-agent)))
  (object (value =locationID))
  (locationID (value (inanimate-object)))
  (patientID (value (patient)))
  (return-value (value (Success)))
  )


(define-frame setPatientAPOD
    (isa  (value (SWS-call)))
  (actor (value (volitional-agent)))
  (object (value =locationID))
  (locationID (value (inanimate-object)))
  (patientID (value (patient)))
  (return-value (value (Success)))
  )


(define-frame setPatientAvailable
    (isa  (value (SWS-call)))
  (actor (value (volitional-agent)))
  (object (value =time))
  (time (value (time-value)))
  (patientID (value (patient)))
  (return-value (value (Success)))
  )


(define-frame setPatientMission
    (isa  (value (SWS-call)))
  (actor (value (volitional-agent)))
  (object (value =missionID))
  (missionID (value (integer-value)))
  (patientID (value (patient)))
  (return-value (value (Success)))
  )


;;; 
;;; The problem with this service is that the missionID value is part of the
;;; string that is value of the hasResultMessage field of the success
;;; output. E.g., for mission ID = "5" the string is "Mission 5 created
;;; successfully"
;;; 
(define-frame createMission
    (isa  (value (SWS-call)))
  (actor (value (volitional-agent)))
  (object (value =missionID))
  (missionID (value (integer-value)))
  (return-value (value (Success)))
  )


;;; All other services have return value with both name and type success. This
;;; one has missionID that is a success. But the way I treat the translation
;;; the following representation is required. Fix soon.
;(define-frame missionID
;    (isa  (value (abstract-object)))
; (hasResultType (value (integer-value)))
; (hasResultMessage (value (integer-value))))

;;; 
;;; Really the object of the service should be the returned mission id. For the
;;; examle in side comments below, the hasResultMessage is the integer 5. Note
;;; that this is not a string (i.e., "5") as is the case with setpatientMission
;;; 
(define-frame initializeTentativeMission
    (isa  (value (SWS-call)))
  (actor (value (volitional-agent)))
  (object (value =return-value))
  (assetID  (value (plane)))		; "C17-002"
  (EDT (value (time-value)))		; "0 01:10"
  (fromLocationID (value (airport)))	; "ORBD"
  (PAX (value (integer-value)))		; 0
  (toLocationID (value (airport)))	; "KADW"
  (maxPAX (value (integer-value)))	; 54
  (travelTime (value (time-value)))	; "0 12:04"
  (return-value (value (integer-value))); five.0
  )


;(define-frame arrivalTime
;    (isa  (value (abstract-object)))
; (hasResultType (value (integer-value)))
; (hasResultMessage (value (time-value))))


;;; 
;;; depTime and return-value's hasResultMessage vary. The former is the time to
;;; leave whereas the latter is the time to arrive after adding departure time
;;; with mission travel time.
;;; 
(define-frame setTentativeMissionDepartureTime
    (isa  (value (SWS-call)))
  (actor (value (volitional-agent)))
  (object (value =missionID))
  (depTime (value (time-value)))	; "0 01:10"
  (missionID (value (integer-value)))	; "5"
  (return-value (value (time-value)))	; "0 13:14" ??
  )

(define-frame DepartureArrivalPair
    (isa  (value (abstract-object)))
 (departureTime (value (time-value)))
 (arrivalTime (value (time-value))))


;;; Not sure what the object of the service call is. Look up soon.
(define-frame getArrivalTime
    (isa  (value (SWS-call)))
  (actor (value (volitional-agent)))
  (object (value =return-value))
  (fromLocationID (value (inanimate-object))) ; "HBAGH"
  (toLocationID (value (inanimate-object))) ; "ORBI"	
  (patientID (value (patient)))		; "829876593"
  (earliestDeparture (value (time-value))) ; "0 01:00"	  
  (return-value (value (DepartureArrivalPair)))	; "0 01:21" and "0 01:37"
  )


(define-frame lookupAsset
    (isa  (value (SWS-call)))
  (actor (value (volitional-agent)))
  (object (value =???))			;???
  (fromLocationID (value (inanimate-object))) ; "ORBD"
  (toLocationID (value (inanimate-object))) ;"KADW"
  (EDT (value (time-value)))		; "0 01:10"
  (LAT (value (time-value)))		; "2 00:00"
  (return-value (value ((AssetQueryRecord))))  
  )



(define-frame lookupMission
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
  (latestArrival (value (time-value)))
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
  (return-value (value ((MissionInfoRecord))))
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


(define-frame ID
    (isa (value (abstract-object)))
  (literal-value (value (literal)))
  )

(define-frame reservationID
    (isa (value (ID)))
  )

;;; Right now missionIDs in researveSeat, setPatientMission, createMission, and
;;; setTentativeMissionDepartureTime are still integer values. Change eventually.
;;; 
(define-frame missionID
    (isa (value (ID)))
  )


(define-frame ReserveSeat
    (isa (value (SWS-call)))
  (actor (value (volitional-agent)))
  (object (value =missionId
		 ;;(abstract-object)
		 ))
  (recipient (value (volitional-agent)))
  ;;Mapping for poirot patientID slot
  (patientID (value =recipient))
  (missionId (value (integer-value)))
  ;; The return-value identifier really should be the frame name
  ;; of the is-reserved relation between the object and recipient.
  (main-result (value (is-reserved 
		       (domain (value =recipient))
		       (co-domain (value =object)
				 ))))
  (return-value (value (reservationID
			(literal-value (value (literal))))))
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
(define-frame XP-INSTRUMENTAL-SCENE->ACTOR2
    (isa                  (value (xp-instrumental-scene->actor)))
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


(define-frame XP-INFO-PRECOND->ACTOR
    (isa                  (value (xp-instrumental-scene->actor)))
    (actor                (value (volitional-agent)))
    (conseq               (value (SWS-call (actor (value =actor)
						  (relation =role))
					   (main-result (value =precondition)))))
    (ante          (value (SWS-call (actor (value =actor))
				    (instrumental-scene (value =conseq))
				    (main-precondition (value =precondition))
				    (main-result (value =good-state)))))
    (consequent (value =conseq))
    (antecedent (value =ante))    
    (action
      (value =conseq))
    (main-action
     (value =ante))
    ;; Should this not be a knowledge state?
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


 ;;; 
;;; Actor performs the action because it results in the reservation for the
;;; script The actor has the goal of achieving the state resulting from the
;;; script.
;;; 
;;; Changed to include recipient (i.e., the patient) who is different from
;;; actor (i.e., the expert) so that they are not unified at explanation time.
;;; Do I need to include =recipient in the pre-xp-nodes or where? Is the domain
;;; of achievement-goal =actor or =recipient? [mcox 16feb07]
;;; 
(define-frame XP-RESERVATION-PRECOND->ACTOR
    (isa                  (value (xp-instrumental-scene->actor)))
  (actor                (value (volitional-agent)))
  (recipient            (value (volitional-agent)))
  (conseq               (value (SWS-call (actor (value =actor)
						(relation =role))
					 (main-result (value =precondition)))))
  (ante          (value (script 
			 (actor (value =recipient))
			 ;;			   (instrumental-scene (value =conseq))
			 (main-precondition (value =precondition))
			 (main-result (value =good-state)))))
  (consequent (value =conseq))
  (antecedent (value =ante))    
  (action
   (value =conseq))
  (main-action
   (value =ante))
  (precondition (value (is-reserved
			(domain (value =recipient)))))
  ;; The state resulting from the script that requires the reservation
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
  (xp-asserted-nodes (value (=goal =ante)))
  (link1             (value (mentally-enabless (domain    (value =goal))
					       (co-domain (value =plan-choice)))))
  (link2             (value (mentally-results (domain    (value =plan-choice))
					      (co-domain (value =role)))))
  (links             (value (=link1 =link2)))
  )




(define-frame FLY 
    (isa (value (mop)))
  (actor (value (volitional-agent)))
  (object (value (physical-object)))
  (instrument (value (plane)))
  ;; Precondition here so that vacation-script can assert that vacationer must 
  ;; have a reservation to fly. But really it is a precondition on boarding 
  ;; the plane and that extra detail is not currently represented in the 
  ;; script. The precondition should probably be have-fuel, but ignored.
  (main-precondition (value (state)))
  (from
   (value (at-location
	   (domain (value =actor))
	   )))
  (to
   (value (at-location
	   (domain (value =actor))
	   )))
  (main-result (value (at-location 
		       (co-domain (value (physical-location))))))
  )



(define-frame VACATION-SCRIPT
	      (isa (value (script)))
  (actor (value (volitional-agent)))
  (flight (value (integer-value)))	;e.g., m-001
  (instrument (value (plane)))
  (start (value (house)))		;Home
  (destination (value (nation)))	;Vacation dest
  (airport1 (value (airport)))		;Local airport
  (airport2 (value (airport)))		;airport should be within destination
  (lodging (value (building)))		;lodging should be within destination
  (from (value (at-location
		(domain (value =actor))
		(co-domain (value (inside
				   (domain (value =actor))
				   (co-domain (value =start))))))))
  (to (value (at-location
		(domain (value =actor))
		(co-domain (value (inside
				   (domain (value =actor))
				   (co-domain (value =lodging))))))
       ))
		;;  (object (value ))
  (main-precondition (value (is-reserved
			     (domain (value =actor))
			     (co-domain (value =flight)))))
  ;; Get within airport
  (instrumental-scene
    (value (ptrans
	     (actor (value =actor))
	     (object (value =actor))
	     (from (value =from))
	     (to (value (at-location
			 (domain (value =actor))
			 (co-domain
			  (value (inside
				  (domain (value =actor))
				  (co-domain (value =airport1))))))))
	     )
	   ))
  ;; Fly to destination. Actor is un-named airline or pilot.
  (goal-scene
   ;; NOTE that this fly could not be directly defined as a mop. 
   ;; An error resulted in the script init expansion of scenes.
    (value (fly 
	    (object (value =actor))
	    (instrument (value =instrument))
	    (main-precondition (value =main-precondition))
	     (from
	       (value (at-location
			(domain (value =actor))
			(co-domain
			  (value (inside
				  (domain (value =actor))
				  (co-domain (value =airport1))))))))
	     (to
	       (value (at-location
			(domain (value =actor))
			(co-domain
			  (value (inside
				  (domain (value =actor))
				  (co-domain (value =airport2))))))))
	     (main-result (value =main-result)))))
  (post-completion-scene
   (value (ptrans
	   (actor (value =actor))
	   (object (value =actor))
	   (from (value (at-location
			 (domain (value =actor))
			 (co-domain
			  (value (inside
				  (domain (value =actor))
				  (co-domain (value =airport2))))))))
	   (to (value =to))
	   )
	  ))
  (scenes
   (value (=instrumental-scene =goal-scene =post-completion-scene)))
  ;; Main result is to be at the destination which the goal-scene accomplished.
  ;; It is not to be at the airport which is the to slot of the goal-scene.
  ;; Yes but to bind with the goal of the midterm example we need to have the 
  ;; main-result to inside the lodging. This is analogous to inside the 
  ;; hospital.
  (main-result (value 
		=to
;;		(at-location 
;;		 (domain (value =actor))
;;		 (co-domain (value (physical-location
;;				    (domain (value =actor))
;;				    (co-domain (value =destination))))))
		      ))
  )





(define-frame DINING-SCRIPT
	      (isa (value (script)))
  (actor (value (volitional-agent)))
  (object (value (food)))
;  (flight (value (abstract-object)))	;e.g., m-001
;  (instrument (value (plane)))
  (start (value (house)))		;Home
  (destination (value (restaurant)))	;Vacation dest
  (main-precondition (value (is-reserved
			     (domain (value =actor))
			     ;;Should probably reserve a time-slot instead. 
			     ;; Must be an abstract-object.
			     ;(co-domain (value =destination))
			     )))
  ;; Get within airport
  (instrumental-scene
    (value (ptrans
	     (actor (value =actor))
	     (object (value =actor))
	     (from (value (at-location
			   (domain (value =actor))
			   (co-domain (value (inside
					      (domain (value =actor))
					      (co-domain (value =start))))))))
	     (to (value (at-location
			 (domain (value =actor))
			 (co-domain
			  (value (inside
				  (domain (value =actor))
				  (co-domain (value =destination))))))))
	     )
	   ))
  ;; Do the dining
  (goal-scene
   ;; NOTE that this is actually an instance of an older script.
   (value (eat-something
	   (actor (value =actor))
	   (object (value =object))
	   (main-precondition (value =main-precondition))
	   (main-result (value =main-result)))))
  ;;Leave restaurant
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
   (value (=instrumental-scene =goal-scene =post-completion-scene)))
  ;; Main result is not to be hungry.
  (main-result (value (hungry
			(domain (value =actor))
			(co-domain (value false.0)))
		      ))
  )



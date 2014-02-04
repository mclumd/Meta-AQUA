;;; -*- Mode: LISP; Syntax: Common-lisp; Package: Meta-aqua; Base: 10 -*-

(in-package :metaaqua)

(use-package :net.uri)


;;;; 
;;;; POIROT/Meta-AQUA Simple Homework Example (story 21). This file
;;;; represents a stub that creates a Meta-AQUA frame representation
;;;; for the old homework problem. The trace is PatientLookup(P4),
;;;; HospitalLookup(P__2,1000,C2), Reserve(P4,H__3).
;;; 



;;; POIROT
;;; 
;;; Simple Homework Example 
;;; 
;;; Note that when I chose to insert person.0 as the actor of the story
;;; instances in stead of letting the system gensym an actor as below, the
;;; pre-XP node check failed when applying each of the three XPs to the story
;;; instances. 
;;; 
(defun set-story-21 ()
  (init-story
    '(
      ((PatientPortType_PatientLookup
	 (actor (value (person)))
	 (object (value patient4.0))
	     )
       "The expert performed a PatientLookup on P4")

      ((HospitalLookupPortType_HospitalLookup
	(actor (value (person)))
	(object (value (Hospital)))
	(from (value (at-location
		      (domain (value =object))
		      (co-domain (value (location =P-loc))))))
	(to (value (distance-from
		    (domain (value =P-loc))
		    (co-domain (value
				thirty.0)))))
	)
       "She looked for a hospital within 30 miles of the patient")
 
      ((TripReservePortType_TripReserve
	(actor (value (person)))
	(object (value patient4.0))
	(from (value (at-location
		      (domain (value =object))
		      (co-domain (value (location =P-loc))))))
	(to (value (at-location
		    (domain (value (hospital)))
		    (co-domain (value (location =H-loc))))))
	(main-result (value (is-booked (domain (value =object)))))
	)
       "The expert reserved a trip for the patient")

      
      )
    ))

#|

THE FOLLOWING IS THE COMPLICATED FIRST SET OF CALLS I MADE ON 8AUG06. SEE NOTES ON PAPER.

CL-USER(23): (main-loop)
Lisp Hypothesizer
Processing...
Result: (http://poirot.bbn.com/2006/05/core#semanticTrace_6)
Subject URI is NIL
Predicate URI is NIL
Object is NIL
Context is http://poirot.bbn.com/2006/05/core#semanticTrace_6
Query Result: ((http://localhost:8008/owl-s/domain/ConditionLookupProcessModel.owl#ServiceInvocation2
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasOutput
v                http://localhost:8008/owl-s/domain/ConditionLookupProcessModel.owl#OutputParam2)
               (http://localhost:8008/owl-s/domain/ConditionLookupProcessModel.owl#ServiceInvocation2
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasInput
                http://localhost:8008/owl-s/domain/ConditionLookupProcessModel.owl#InputParam2)
               (http://localhost:8008/owl-s/domain/ConditionLookupProcessModel.owl#ServiceInvocation2
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/ConditionLookupProcessModel.owl#ConditionPortType_ConditionLookup)
               (http://localhost:8008/poirot-servlet http://www.w3.org/2002/07/owl#imports
                http://www.daml.org/services/owl-s/1.1/Grounding.owl)
               (http://localhost:8008/poirot-servlet http://www.w3.org/2002/07/owl#imports
                http://www.daml.org/services/owl-s/1.1/Process.owl)
               (http://localhost:8008/poirot-servlet http://www.w3.org/2002/07/owl#imports
                http://www.daml.org/services/owl-s/1.1/Profile.owl)
               (http://localhost:8008/poirot-servlet http://www.w3.org/2002/07/owl#imports
                http://www.daml.org/services/owl-s/1.1/Service.owl)
               (http://localhost:8008/poirot-servlet http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://www.w3.org/2002/07/owl#Ontology)
               (http://localhost:8008/owl-s/domain/ConditionLookupProcessModel.owl#OutputParam2
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue
                http://localhost:8008/owl-s/domain/ConditionLookupConcept.owl#OutputData2)
               (http://localhost:8008/owl-s/domain/ConditionLookupProcessModel.owl#OutputParam2
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType
                http://localhost:8008/owl-s/domain/ConditionLookupConcept.owl#ConditionLookupResult)
               (http://localhost:8008/owl-s/domain/ConditionLookupProcessModel.owl#OutputParam2
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/ConditionLookupProcessModel.owl#ConditionPortType_ConditionLookup_ConditionRecord_OUT)
               (http://localhost:8008/owl-s/domain/ConditionLookupConcept.owl#OutputData2
                http://localhost:8008/owl-s/domain/ConditionLookupConcept.owl#ConditionName Infection)
               (http://localhost:8008/owl-s/domain/ConditionLookupConcept.owl#OutputData2
                http://localhost:8008/owl-s/domain/ConditionLookupConcept.owl#ConditionId C3)
               (http://localhost:8008/owl-s/domain/ConditionLookupConcept.owl#OutputData2
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/ConditionLookupConcept.owl#ConditionRecord)
               (http://localhost:8008/owl-s/domain/ConditionLookupProcessModel.owl#InputParam2
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue C3)
               (http://localhost:8008/owl-s/domain/ConditionLookupProcessModel.owl#InputParam2
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType
                http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#ConditionId)
               (http://localhost:8008/owl-s/domain/ConditionLookupProcessModel.owl#InputParam2
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/ConditionLookupProcessModel.owl#ConditionPortType_ConditionLookup_ConditionId_IN)
               (http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#InputParam3
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue P4)
               (http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#InputParam3
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType
                http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#PatientId)
               (http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#InputParam3
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#PatientPortType_PatientLookup_PatientId_IN)
               (http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#OutputParam3
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue
                http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#OutputData3)
               (http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#OutputParam3
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType
                http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#PatientLookupResult)
               (http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#OutputParam3
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#PatientPortType_PatientLookup_PatientRecord_OUT)
               (http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#OutputData3
                http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#PatientLoccode P__2)
               (http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#OutputData3
                http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#ConditionId C2)
               (http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#OutputData3
                http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#PatientId P4)
               (http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#OutputData3
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#PatientRecord)
               (http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#ServiceInvocation3
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasOutput
                http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#OutputParam3)
               (http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#ServiceInvocation3
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasInput
                http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#InputParam3)
               (http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#ServiceInvocation3
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#PatientPortType_PatientLookup)
               (http://localhost:8008/owl-s/domain/ConditionLookupProcessModel.owl#ServiceInvocation4
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasOutput
                http://localhost:8008/owl-s/domain/ConditionLookupProcessModel.owl#OutputParam4)
               (http://localhost:8008/owl-s/domain/ConditionLookupProcessModel.owl#ServiceInvocation4
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasInput
                http://localhost:8008/owl-s/domain/ConditionLookupProcessModel.owl#InputParam4)
               (http://localhost:8008/owl-s/domain/ConditionLookupProcessModel.owl#ServiceInvocation4
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/ConditionLookupProcessModel.owl#ConditionPortType_ConditionLookup)
               (http://localhost:8008/owl-s/domain/ConditionLookupProcessModel.owl#OutputParam4
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue
                http://localhost:8008/owl-s/domain/ConditionLookupConcept.owl#OutputData4)
               (http://localhost:8008/owl-s/domain/ConditionLookupProcessModel.owl#OutputParam4
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType
                http://localhost:8008/owl-s/domain/ConditionLookupConcept.owl#ConditionLookupResult)
               (http://localhost:8008/owl-s/domain/ConditionLookupProcessModel.owl#OutputParam4
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/ConditionLookupProcessModel.owl#ConditionPortType_ConditionLookup_ConditionRecord_OUT)
               (http://localhost:8008/owl-s/domain/ConditionLookupProcessModel.owl#InputParam4
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue C2)
               (http://localhost:8008/owl-s/domain/ConditionLookupProcessModel.owl#InputParam4
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType
                http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#ConditionId)
               (http://localhost:8008/owl-s/domain/ConditionLookupProcessModel.owl#InputParam4
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/ConditionLookupProcessModel.owl#ConditionPortType_ConditionLookup_ConditionId_IN)
               (http://localhost:8008/owl-s/domain/ConditionLookupConcept.owl#OutputData4
                http://localhost:8008/owl-s/domain/ConditionLookupConcept.owl#ConditionName Severe Burns)
               (http://localhost:8008/owl-s/domain/ConditionLookupConcept.owl#OutputData4
                http://localhost:8008/owl-s/domain/ConditionLookupConcept.owl#ConditionId C2)
               (http://localhost:8008/owl-s/domain/ConditionLookupConcept.owl#OutputData4
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/ConditionLookupConcept.owl#ConditionRecord)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam5
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue 30)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam5
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType
                http://www.w3.org/2001/XMLSchema#double)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam5
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#HospitalPortType_HospitalLookup_Range_IN)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam7
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue P__2)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam7
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType
                http://www.w3.org/2001/XMLSchema#string)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam7
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#HospitalPortType_HospitalLookup_Loccode_IN)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#ServiceInvocation5
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasOutput
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#OutputParam5)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#ServiceInvocation5
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasInput
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam8)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#ServiceInvocation5
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasInput
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam7)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#ServiceInvocation5
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasInput
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam6)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#ServiceInvocation5
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasInput
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam5)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#ServiceInvocation5
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#HospitalLookupPortType_HospitalLookup)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam8
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue )
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam8
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType
                http://localhost:8008/owl-s/domain/ConditionLookupConcept.owl#ConditionId)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam8
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#HospitalPortType_HospitalLookup_ConditionId_IN)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam6
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue kilometers)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam6
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#RangeUnits)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam6
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#HospitalPortType_HospitalLookup_Units_IN)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#OutputParam5
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#NoHospitalsFound)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#OutputParam5
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#HospitalLookupResult)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#OutputParam5
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#HospitalPortType_HospitalLookup_HospitalRecord_OUT)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam12
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue )
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam12
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType
                http://localhost:8008/owl-s/domain/ConditionLookupConcept.owl#ConditionId)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam12
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#HospitalPortType_HospitalLookup_ConditionId_IN)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#OutputParam6
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#NoHospitalsFound)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#OutputParam6
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#HospitalLookupResult)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#OutputParam6
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#HospitalPortType_HospitalLookup_HospitalRecord_OUT)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam9
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue 100)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam9
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType
                http://www.w3.org/2001/XMLSchema#double)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam9
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#HospitalPortType_HospitalLookup_Range_IN)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam10
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue kilometers)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam10
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#RangeUnits)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam10
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#HospitalPortType_HospitalLookup_Units_IN)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam11
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue P__2)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam11
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType
                http://www.w3.org/2001/XMLSchema#string)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam11
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#HospitalPortType_HospitalLookup_Loccode_IN)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#ServiceInvocation6
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasOutput
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#OutputParam6)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#ServiceInvocation6
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasInput
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam12)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#ServiceInvocation6
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasInput
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam11)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#ServiceInvocation6
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasInput
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam10)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#ServiceInvocation6
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasInput
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam9)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#ServiceInvocation6
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#HospitalLookupPortType_HospitalLookup)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam16
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue )
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam16
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType
                http://localhost:8008/owl-s/domain/ConditionLookupConcept.owl#ConditionId)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam16
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#HospitalPortType_HospitalLookup_ConditionId_IN)
               (http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#OutputData5_1
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#Distance 107.0)
               (http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#OutputData5_1
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#Loccode H__1)
               (http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#OutputData5_1
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#HospitalName Theater Mobile Unit)
               (http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#OutputData5_1
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#HospitalID H1)
               (http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#OutputData5_1
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#HospitalRecord)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#ServiceInvocation7
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasOutput
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#OutputParam7)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#ServiceInvocation7
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasInput
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam16)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#ServiceInvocation7
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasInput
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam15)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#ServiceInvocation7
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasInput
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam14)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#ServiceInvocation7
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasInput
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam13)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#ServiceInvocation7
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#HospitalLookupPortType_HospitalLookup)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam13
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue 1000)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam13
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType
                http://www.w3.org/2001/XMLSchema#double)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam13
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#HospitalPortType_HospitalLookup_Range_IN)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam14
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue kilometers)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam14
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#RangeUnits)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam14
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#HospitalPortType_HospitalLookup_Units_IN)
               (http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#OutputData5_3
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#Distance 865.0)
               (http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#OutputData5_3
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#Loccode H__3)
               (http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#OutputData5_3
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#HospitalName Medical Ship)
               (http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#OutputData5_3
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#HospitalID H3)
               (http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#OutputData5_3
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#HospitalRecord)
               (http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#OutputData5
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#HospitalRecordElement
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#OutputData5_3)
               (http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#OutputData5
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#HospitalRecordElement
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#OutputData5_2)
               (http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#OutputData5
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#HospitalRecordElement
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#OutputData5_1)
               (http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#OutputData5
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#HospitalList)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam15
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue P__2)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam15
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType
                http://www.w3.org/2001/XMLSchema#string)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#InputParam15
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#HospitalPortType_HospitalLookup_Loccode_IN)
               (http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#OutputData5_2
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#Distance 173.0)
               (http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#OutputData5_2
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#Loccode H__2)
               (http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#OutputData5_2
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#HospitalName Bagdhad)
               (http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#OutputData5_2
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#HospitalID H2)
               (http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#OutputData5_2
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#HospitalRecord)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#OutputParam7
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#OutputData5)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#OutputParam7
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType
                http://localhost:8008/owl-s/domain/HospitalLookupConcept.owl#HospitalLookupResult)
               (http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#OutputParam7
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#HospitalPortType_HospitalLookup_HospitalRecord_OUT)
               (http://poirot.bbn.com/2006/05/core#semanticTrace_6
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://poirot.bbn.com/2006/05/core#SemanticTrace)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_6_1
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://poirot.bbn.com/2006/05/core#SemanticTraceElement)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_6_1
                http://poirot.bbn.com/2006/05/core#index 1)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_6_1
                http://poirot.bbn.com/2006/05/core#elementOf
                http://poirot.bbn.com/2006/05/core#semanticTrace_6)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_6_1
                http://poirot.bbn.com/2006/05/core#serviceInvocation
                http://localhost:8008/owl-s/domain/ConditionLookupProcessModel.owl#ServiceInvocation2)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_6_2
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://poirot.bbn.com/2006/05/core#SemanticTraceElement)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_6_2
                http://poirot.bbn.com/2006/05/core#index 2)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_6_2
                http://poirot.bbn.com/2006/05/core#elementOf
                http://poirot.bbn.com/2006/05/core#semanticTrace_6)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_6_2
                http://poirot.bbn.com/2006/05/core#serviceInvocation
                http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#ServiceInvocation3)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_6_3
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://poirot.bbn.com/2006/05/core#SemanticTraceElement)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_6_3
                http://poirot.bbn.com/2006/05/core#index 3)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_6_3
                http://poirot.bbn.com/2006/05/core#elementOf
                http://poirot.bbn.com/2006/05/core#semanticTrace_6)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_6_3
                http://poirot.bbn.com/2006/05/core#serviceInvocation
                http://localhost:8008/owl-s/domain/ConditionLookupProcessModel.owl#ServiceInvocation4)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_6_4
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://poirot.bbn.com/2006/05/core#SemanticTraceElement)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_6_4
                http://poirot.bbn.com/2006/05/core#index 4)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_6_4
                http://poirot.bbn.com/2006/05/core#elementOf
                http://poirot.bbn.com/2006/05/core#semanticTrace_6)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_6_4
                http://poirot.bbn.com/2006/05/core#serviceInvocation
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#ServiceInvocation5)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_6_5
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://poirot.bbn.com/2006/05/core#SemanticTraceElement)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_6_5
                http://poirot.bbn.com/2006/05/core#index 5)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_6_5
                http://poirot.bbn.com/2006/05/core#elementOf
                http://poirot.bbn.com/2006/05/core#semanticTrace_6)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_6_5
                http://poirot.bbn.com/2006/05/core#serviceInvocation
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#ServiceInvocation6)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_6_6
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://poirot.bbn.com/2006/05/core#SemanticTraceElement)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_6_6
                http://poirot.bbn.com/2006/05/core#index 6)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_6_6
                http://poirot.bbn.com/2006/05/core#elementOf
                http://poirot.bbn.com/2006/05/core#semanticTrace_6)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_6_6
                http://poirot.bbn.com/2006/05/core#serviceInvocation
                http://localhost:8008/owl-s/domain/HospitalLookupProcessModel.owl#ServiceInvocation7))
Publishing ((http://poirot.bbn.com/2006/05/hyp#hypothesis-lisp-1
             http://www.w3.org/1999/02/22-rdf-syntax-ns#type http://poirot.bbn.com/2006/05/hyp#hypothesis)
            (http://poirot.bbn.com/2006/05/hyp#thesis-lisp-1 http://www.w3.org/1999/02/22-rdf-syntax-ns#type
             http://poirot.bbn.com/2006/05/hyp#thesis)
            (http://poirot.bbn.com/2006/05/hyp#thesis-lisp-2 http://www.w3.org/1999/02/22-rdf-syntax-ns#type
             http://poirot.bbn.com/2006/05/hyp#thesis)
            (http://poirot.bbn.com/2006/05/hyp#hypothesis-lisp-1 http://poirot.bbn.com/2006/05/hyp#hasThesis
             http://poirot.bbn.com/2006/05/hyp#thesis-lisp-1)
            (http://poirot.bbn.com/2006/05/hyp#hypothesis-lisp-1 http://poirot.bbn.com/2006/05/hyp#hasThesis
             http://poirot.bbn.com/2006/05/hyp#thesis-lisp-2))
Finished
Finished
NIL
|#

;;; 
;;; Test for the original Homework
;;; 
(defvar *test-results* 
    '(("http://localhost:8008/poirot-servlet"
       "http://www.w3.org/2002/07/owl#imports"
       "http://www.daml.org/services/owl-s/1.1/Grounding.owl")
      ("http://localhost:8008/poirot-servlet" 
       "http://www.w3.org/2002/07/owl#imports"
       "http://www.daml.org/services/owl-s/1.1/Process.owl")
      ("http://localhost:8008/poirot-servlet"
       "http://www.w3.org/2002/07/owl#imports"
       "http://www.daml.org/services/owl-s/1.1/Profile.owl")
      ("http://localhost:8008/poirot-servlet"
       "http://www.w3.org/2002/07/owl#imports"
       "http://www.daml.org/services/owl-s/1.1/Service.owl")
      ("http://localhost:8008/poirot-servlet"
       "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
       "http://www.w3.org/2002/07/owl#Ontology")
      ("http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#OutputData6"
       "http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#PatientLoccode"
       P__2)
      ("http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#OutputData6"
       "http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#ConditionId"
       C2)
      ("http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#OutputData6"
       "http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#PatientId" 
       P4)
      ("http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#OutputData6"
       "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
       "http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#PatientRecord")
      ("http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#ServiceInvocation8"
       "http://www.daml.org/services/owl-s/1.1/Process.owl#hasOutput"
       "http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#OutputParam8")
      ("http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#ServiceInvocation8"
       "http://www.daml.org/services/owl-s/1.1/Process.owl#hasInput"
       "http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#InputParam17")
      ("http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#ServiceInvocation8"
       "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
       "http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#PatientPortType_PatientLookup")
      ("http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#OutputParam8"
       "http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue"
       "http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#OutputData6")
      ("http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#OutputParam8"
       "http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType"
       "http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#PatientLookupResult")
      ("http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#OutputParam8"
       "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
       "http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#PatientPortType_PatientLookup_PatientRecord_OUT")
      ("http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#InputParam17"
       "http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue"
       P4)
      ("http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#InputParam17"
       "http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType"
       "http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#PatientId")
      ("http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#InputParam17"
       "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
       "http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#PatientPortType_PatientLookup_PatientId_IN")
      ("http://poirot.bbn.com/2006/05/core#semanticTrace_8"
       "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
       "http://poirot.bbn.com/2006/05/core#SemanticTrace")
      ("http://poirot.bbn.com/2006/05/core#semanticTraceElement_8_1"
       "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
       "http://poirot.bbn.com/2006/05/core#SemanticTraceElement")
      ("http://poirot.bbn.com/2006/05/core#semanticTraceElement_8_1"
       "http://poirot.bbn.com/2006/05/core#index" 
       1)
      ("http://poirot.bbn.com/2006/05/core#semanticTraceElement_8_1"
       "http://poirot.bbn.com/2006/05/core#elementOf"
       "http://poirot.bbn.com/2006/05/core#semanticTrace_8")
      ("http://poirot.bbn.com/2006/05/core#semanticTraceElement_8_1"
       "http://poirot.bbn.com/2006/05/core#serviceInvocation"
       "http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#ServiceInvocation8)")
      )
  "Test for the original Homework")


#|
NOTE the following capabilities for URI when using the :net.uri package.

METAAQUA(11): (setf x (parse-uri (first (first *test-results*))))
#<URI http://localhost:8008/poirot-servlet>
METAAQUA(13): (net.uri:uri-host x)
"localhost"
METAAQUA(14): (net.uri:uri-scheme x)
:HTTP
METAAQUA(15): (net.uri:uri-port x)
8008
METAAQUA(16): (net.uri:uri-path x)
"/poirot-servlet"
METAAQUA(19): (net.uri:uri-string x)
Error: The symbol "URI-STRING" is not external in the NET.URI package.
  [condition type: READER-ERROR]
METAAQUA(22): (net.uri::uri-string x)
"http://localhost:8008/poirot-servlet"
METAAQUA(25): (setf y (parse-uri "http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#OutputData6"))
#<URI http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#OutputData6>
METAAQUA(26): (describe y)
#<URI http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#OutputData6> is an instance of
    #<STANDARD-CLASS URI>:
 The following slots have :INSTANCE allocation:
  SCHEME        :HTTP
  HOST          "localhost"
  PORT          8008
  PATH          "/owl-s/domain/PatientLookupConcept.owl"
  QUERY         NIL
  FRAGMENT      "OutputData6"
  PLIST         NIL
  ESCAPED       NIL
  STRING        "http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#OutputData6"
  PARSED-PATH   NIL
  HASHCODE      NIL


THE FOLLOWING IS THE bare SEMANTIC TRACE FOR THE PLookup(P4) CALL with URI prefixes
stripped out, with type identifying triples deleted, with servlet info deleted, 
and with extraneous output statements deleted.

CL-USER(25): (main-loop)
Result: (core#semanticTrace_8)
Context is core#semanticTrace_8
Query Result: ((PatientLookupConcept.owl#OutputData6
                PatientLookupConcept.owl#PatientLoccode P__2)
               (PatientLookupConcept.owl#OutputData6
                PatientLookupConcept.owl#ConditionId C2)
               (PatientLookupConcept.owl#OutputData6
                PatientLookupConcept.owl#PatientId P4)
               (PatientLookupProcessModel.owl#ServiceInvocation8
                Process.owl#hasOutput
                PatientLookupProcessModel.owl#OutputParam8)
               (PatientLookupProcessModel.owl#ServiceInvocation8
                Process.owl#hasInput
                PatientLookupProcessModel.owl#InputParam17)
               (PatientLookupProcessModel.owl#OutputParam8
                Process.owl#parameterValue
                PatientLookupConcept.owl#OutputData6)
               (PatientLookupProcessModel.owl#InputParam17
                Process.owl#parameterValue P4)
               (core#semanticTraceElement_8_1
                core#index 1)
               (core#semanticTraceElement_8_1
                core#elementOf
                core#semanticTrace_8)
               (core#semanticTraceElement_8_1
                core#serviceInvocation
                PatientLookupProcessModel.owl#ServiceInvocation8))
Publishing ((hyp#hypothesis-lisp-1 hyp#hasThesis
             hyp#thesis-lisp-1)
            (hyp#hypothesis-lisp-1 hyp#hasThesis
             hyp#thesis-lisp-2))



THE FOLLOWING IS THE full verbose SEMANTIC TRACE FOR THE PLookup(P4) CALL.

CL-USER(25): (main-loop)
Lisp Hypothesizer
Waiting for data
Waiting for data
Waiting for data
Waiting for data
Waiting for data
Waiting for data
Waiting for data
Waiting for data
Waiting for data
Waiting for data
Waiting for data
Waiting for data
Waiting for data
Waiting for data
Processing...
Result: (http://poirot.bbn.com/2006/05/core#semanticTrace_8)
Subject URI is NIL
Predicate URI is NIL
Object is NIL
Context is http://poirot.bbn.com/2006/05/core#semanticTrace_8
Query Result: ((http://localhost:8008/poirot-servlet http://www.w3.org/2002/07/owl#imports
                http://www.daml.org/services/owl-s/1.1/Grounding.owl)
               (http://localhost:8008/poirot-servlet http://www.w3.org/2002/07/owl#imports
                http://www.daml.org/services/owl-s/1.1/Process.owl)
               (http://localhost:8008/poirot-servlet http://www.w3.org/2002/07/owl#imports
                http://www.daml.org/services/owl-s/1.1/Profile.owl)
               (http://localhost:8008/poirot-servlet http://www.w3.org/2002/07/owl#imports
                http://www.daml.org/services/owl-s/1.1/Service.owl)
               (http://localhost:8008/poirot-servlet http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://www.w3.org/2002/07/owl#Ontology)
               (http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#OutputData6
                http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#PatientLoccode P__2)
               (http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#OutputData6
                http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#ConditionId C2)
               (http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#OutputData6
                http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#PatientId P4)
               (http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#OutputData6
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#PatientRecord)
               (http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#ServiceInvocation8
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasOutput
                http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#OutputParam8)
               (http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#ServiceInvocation8
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasInput
                http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#InputParam17)
               (http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#ServiceInvocation8
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#PatientPortType_PatientLookup)
               (http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#OutputParam8
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue
                http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#OutputData6)
               (http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#OutputParam8
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType
                http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#PatientLookupResult)
               (http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#OutputParam8
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#PatientPortType_PatientLookup_PatientRecord_OUT)
               (http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#InputParam17
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue P4)
               (http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#InputParam17
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType
                http://localhost:8008/owl-s/domain/PatientLookupConcept.owl#PatientId)
               (http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#InputParam17
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#PatientPortType_PatientLookup_PatientId_IN)
               (http://poirot.bbn.com/2006/05/core#semanticTrace_8
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://poirot.bbn.com/2006/05/core#SemanticTrace)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_8_1
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://poirot.bbn.com/2006/05/core#SemanticTraceElement)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_8_1
                http://poirot.bbn.com/2006/05/core#index 1)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_8_1
                http://poirot.bbn.com/2006/05/core#elementOf
                http://poirot.bbn.com/2006/05/core#semanticTrace_8)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_8_1
                http://poirot.bbn.com/2006/05/core#serviceInvocation
                http://localhost:8008/owl-s/domain/PatientLookupProcessModel.owl#ServiceInvocation8))
Publishing ((http://poirot.bbn.com/2006/05/hyp#hypothesis-lisp-1
             http://www.w3.org/1999/02/22-rdf-syntax-ns#type http://poirot.bbn.com/2006/05/hyp#hypothesis)
            (http://poirot.bbn.com/2006/05/hyp#thesis-lisp-1 http://www.w3.org/1999/02/22-rdf-syntax-ns#type
             http://poirot.bbn.com/2006/05/hyp#thesis)
            (http://poirot.bbn.com/2006/05/hyp#thesis-lisp-2 http://www.w3.org/1999/02/22-rdf-syntax-ns#type
             http://poirot.bbn.com/2006/05/hyp#thesis)
            (http://poirot.bbn.com/2006/05/hyp#hypothesis-lisp-1 http://poirot.bbn.com/2006/05/hyp#hasThesis
             http://poirot.bbn.com/2006/05/hyp#thesis-lisp-1)
            (http://poirot.bbn.com/2006/05/hyp#hypothesis-lisp-1 http://poirot.bbn.com/2006/05/hyp#hasThesis
             http://poirot.bbn.com/2006/05/hyp#thesis-lisp-2))
Finished
Finished
NIL
CL-USER(26): 
|#

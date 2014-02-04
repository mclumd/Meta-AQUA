(in-package :poirot-connector)


;;; Not implemented yet.
(defparameter *verbose* t
  "If nil, print only fragments of uris")

(defvar *break-on* nil)


;;; Eventually make these 3 access functions inline.

(defun triple-subject (triple)
  "Return subject of rdf triple"
  (first triple)
  )

(defun triple-predicate (triple)
  "Return predicate of rdf triple"
  (second triple)
  )

(defun triple-object (triple)
  "Return object of rdf triple"
  (third triple)
  )



;;; Assume input bb is a flat list.
(defun parse-trace (bb)
  "Converts any uri to its equivalent class structure."
  (if *break-on*
      (break))
  (let ((first-element 
	(string-trim 
	 "\"" 
	 (if (consp bb)
	     (first bb)
	   bb))))
    (cond ((null bb) nil)
	  ((consp bb)
	   (cons (if (is-uri 
		      first-element)
		     (parse-uri 
		      first-element)
		   first-element)
		 (parse-trace 
		  (rest bb))))
	  (t 
	   (if (is-uri bb)
	       (parse-uri bb)
	     bb)
	   )))
  )


 ;;; Assumes a clean (divisible by 3) list as input
(defun convert2triples (flat-list)
  (cond ((null flat-list) nil)
	(t
	 (cons 
	  (list (first flat-list)
		(second flat-list)
		(third flat-list))
	  (convert2triples 
	   (rest (rest (rest flat-list))))))
	)
  )



;;; Start of a set of routine to extract the output of sws calls and create the
;;; Meta-AQUA frames for input. Ran into trouble with URI encodings. ACL sees
;;; http:// as a package label and barfs on # syntax.

(defun get-output-param (service-call 
			 &optional 
			 (triple-list (first *bb*)))
  "Return the output parameter of a service call"
  (let ((first-triple (first triple-list)))
    (cond ((null first-triple)
	   nil)
	  ((string-equal service-call 
		  (net.uri:uri-fragment 
		   (triple-subject first-triple)))
	   (if (string-equal (net.uri:uri-fragment 
		       (triple-predicate first-triple))
		      "hasOutput")
	       (triple-object first-triple)
	     (get-output-param service-call 
			       (rest triple-list))))
	  (t
	   (get-output-param service-call 
f			     (rest triple-list)))))
  )


;;; More general definition, although I do not like doing string comparison as
;;; opposed to symbol (address) comparisons.
(defun get-object-value (subject
			 predicate
			 &optional 
			 (triple-list (first *bb*)))
  "Return the object value of a given triple"
  (let ((first-triple (first triple-list)))
    (cond ((null first-triple)
	   nil)
	  ((and (string-equal subject
			      (net.uri:uri-fragment 
			       (triple-subject first-triple)))
		(string-equal predicate
			      (net.uri:uri-fragment 
			       (triple-predicate first-triple))))
	   (triple-object first-triple))
	  (t
	   (get-object-value subject
			     predicate
			     (rest triple-list)))))
  )


;;; More parsimonious definition.
(defun get-output-param (service-call 
			 &optional 
			 (triple-list (first *bb*)))
  "Return the output parameter of a service call"
  (get-object-value 
   service-call
   "hasOutput")
  )


;;; Yet more general, but should implement with find or other sequence function
(defun get-query (&optional 
		  subject
		  predicate
		  object
		  (triple-list (first *bb*)))
  "Return a query match on a given triple"
  (let ((first-triple (first triple-list)))
    (cond ((null first-triple)
	   nil)
	  ((and (or (null subject)
		    (string-equal subject
				  (if (uri-p (triple-subject first-triple))
				      (net.uri:uri-fragment 
				       (triple-subject first-triple))
				    (triple-subject first-triple))))
		(or (null predicate)
		    (string-equal predicate
				  (if (uri-p (triple-predicate first-triple))
				      (net.uri:uri-fragment 
				       (triple-predicate first-triple))
				    (triple-predicate first-triple))))
		(or (null object)
		    (string-equal object
				  (if (uri-p (triple-object first-triple))
				      (net.uri:uri-fragment 
				       (triple-object first-triple))
				    (triple-object first-triple)))))
	   (cons first-triple 
		 (get-query subject
			    predicate 
			    object
			    (rest triple-list))))
	  (t
	   (get-query subject
		      predicate 
		      object
		      (rest triple-list)))))
  )


;;; If caller passes nil as type-id, all types are returned.
(defun get-type (type-id 
		 &optional 
		 (triple-list (first *bb*)))
  "Access function to get a triple having a specific type."
  (get-query nil "type" type-id triple-list)
  )



;;; Test that only one trace exists. 
;;; 
;;; The trace itself is somewhat redundant. It has no value and is simply an
;;; identifier to which elements can point.
;;; 
(defun get-trace (&aux
		  (trace
		   (get-type 
		    "SemanticTrace")))
  "Return the trace triple."
  (if (not (eql 1 (length trace)))
      (format t "~%Exists multiple traces.~%")
    (first (first trace))
    )
  )


;;; 
;;; If we can assume that only one semantic trace is one the blackboard, then
;;; we do not really need the get-trace function. We can just collect all trace
;;; elements.
;;; 
(defun get-elements ()
  (mapcar #'(lambda (trip)
	      (triple-subject trip))
	  (get-type 
	   "semanticTraceElement")
	  )
  )


;;; These really need to be ordered using the indexes in the triples.
(defun collect-SWSs (&aux
		   SWSs)
  (dolist (each-element (get-elements)) 
    ;;(format t "~%~s~%" (uri-fragment each-element))
    (setf SWSs
      (append
       SWSs
       (get-query 
	(uri-fragment each-element) 
	"serviceInvocation")))
    )
  SWSs
  )


(defun show-SWSs ()
  (mapcar #'(lambda (each-trip)
	      (triple-object 
	       each-trip))
	  (collect-SWSs))
  )


;;; Try (mapcar #'show-SWS-call (show-SWSs))
;;; 
(defun show-SWS-call (SWS
		      &aux
		      (trips
		       (get-query 
			(uri-fragment SWS)
			)))
  (format t "~%Name: ~S~%"
	  (uri-fragment 
	   SWS))
  (format 
   t 
   "Type: ~S~%"
   (uri-fragment 
    (triple-object
     (first 
      (get-type nil trips)))))
  ;;Handle Input
  (format 
   t 
   "Input: ~%")
  (print-io "hasInput" trips)
  ;;Handle output
  (format 
   t 
   "Output: ~%")
  (print-io "hasOutput" trips)
  (terpri)
  )


;;; Assumes that parameter identifiers are unique.
;;; 
;;; predicate parameter is either "hasInput" or "hasOutput".
;;; trips parameter is list of triples all whose subject is equal to a 
;;;       particular SWS invocation.
;;;
(defun print-io (predicate trips)
  (dolist (each-element			;Especially input can have mult vals
	      (get-query 
	       nil predicate 
	       nil trips))
    (let* ((io-id			;e.g., InputParam10
	    (uri-fragment 
	     (triple-object 
	      each-element)))
	   (io-vals			;Some list with a triple 
	    (get-query 
	     io-id 
	     "parameterValue")))
      (format 
       t "      ~S" 
       io-id)
      (if io-vals
	  (dolist (each-val io-vals)
	    (let ((val-object
		   (triple-object	;e.g., H__3 or ns11:OutputData4
		    each-val)))
	      (format t " = ~s" 
		      val-object)
	      (if (uri-p val-object)	;e.g., ns11:OutputData4
		  (format 
		   t
		   "~%   ~s"
		   (mapcar
		    #'(lambda 
			  (triple)
			(mapcar
			 #'(lambda	;Function will shorten uris
			       (uri)
			     (if (uri-p 
				  uri)
				 (uri-fragment
				  uri)
			       uri))
			 triple))
		    (get-query		;Will return a list of triples
		     (uri-fragment
		      val-object))
		    ))
		)
	      )
	    )
	)
      (terpri)))  
  )

#|

(triple-object 
 (first 
  (get-query 
   (uri-fragment 
    (triple-object 
     (first 
      (get-query nil "hasInput" nil trips))))
   "parameterValue")))
;;
;; -->"C1"

The Semantic Trace as seen by Meta-AQUA is :
(("\"http://poirot.bbn.com/2006/05/core#semanticTrace_12\""))
Processing...
Result: ("http://poirot.bbn.com/2006/05/core#semanticTrace_12")
Subject URI is NIL
Predicate URI is NIL
Object is NIL
Context is "http://poirot.bbn.com/2006/05/core#semanticTrace_12"
Query Result: ((http://poirot.bbn.com/owl-s/domain/PatientLookupProcessModel.owl#OutputParam16
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue
                http://poirot.bbn.com/owl-s/domain/PatientLookupConcept.owl#OutputData14)
               (http://poirot.bbn.com/owl-s/domain/PatientLookupProcessModel.owl#OutputParam16
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType
                http://poirot.bbn.com/owl-s/domain/PatientLookupConcept.owl#PatientLookupResult)
               (http://poirot.bbn.com/owl-s/domain/PatientLookupProcessModel.owl#OutputParam16
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://poirot.bbn.com/owl-s/domain/PatientLookupProcessModel.owl#PatientPortType_PatientLookup_PatientRecord_OUT)
               (http://poirot.bbn.com/owl-s/domain/PatientLookupProcessModel.owl#ServiceInvocation16
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasOutput
                http://poirot.bbn.com/owl-s/domain/PatientLookupProcessModel.owl#OutputParam16)
               (http://poirot.bbn.com/owl-s/domain/PatientLookupProcessModel.owl#ServiceInvocation16
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasInput
                http://poirot.bbn.com/owl-s/domain/PatientLookupProcessModel.owl#InputParam35)
               (http://poirot.bbn.com/owl-s/domain/PatientLookupProcessModel.owl#ServiceInvocation16
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://poirot.bbn.com/owl-s/domain/PatientLookupProcessModel.owl#PatientPortType_PatientLookup)
               (http://poirot.bbn.com/owl-s/domain/PatientLookupProcessModel.owl#InputParam35
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue P4)
               (http://poirot.bbn.com/owl-s/domain/PatientLookupProcessModel.owl#InputParam35
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType
                http://poirot.bbn.com/owl-s/domain/PatientLookupConcept.owl#PatientId)
               (http://poirot.bbn.com/owl-s/domain/PatientLookupProcessModel.owl#InputParam35
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://poirot.bbn.com/owl-s/domain/PatientLookupProcessModel.owl#PatientPortType_PatientLookup_PatientId_IN)
               (http://poirot.bbn.com http://www.w3.org/2002/07/owl#imports http://www.daml.org/services/owl-s/1.1/Grounding.owl)
               (http://poirot.bbn.com http://www.w3.org/2002/07/owl#imports http://www.daml.org/services/owl-s/1.1/Process.owl)
               (http://poirot.bbn.com http://www.w3.org/2002/07/owl#imports http://www.daml.org/services/owl-s/1.1/Profile.owl)
               (http://poirot.bbn.com http://www.w3.org/2002/07/owl#imports http://www.daml.org/services/owl-s/1.1/Service.owl)
               (http://poirot.bbn.com http://www.w3.org/1999/02/22-rdf-syntax-ns#type http://www.w3.org/2002/07/owl#Ontology)
               (http://poirot.bbn.com/owl-s/domain/PatientLookupConcept.owl#OutputData14
                http://poirot.bbn.com/owl-s/domain/PatientLookupConcept.owl#PatientLoccode P__2)
               (http://poirot.bbn.com/owl-s/domain/PatientLookupConcept.owl#OutputData14
                http://poirot.bbn.com/owl-s/domain/PatientLookupConcept.owl#ConditionId C2)
               (http://poirot.bbn.com/owl-s/domain/PatientLookupConcept.owl#OutputData14
                http://poirot.bbn.com/owl-s/domain/PatientLookupConcept.owl#PatientId P4)
               (http://poirot.bbn.com/owl-s/domain/PatientLookupConcept.owl#OutputData14
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://poirot.bbn.com/owl-s/domain/PatientLookupConcept.owl#PatientRecord)
               (http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#ServiceInvocation17
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasOutput
                http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#OutputParam17)
               (http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#ServiceInvocation17
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasInput
                http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#InputParam39)
               (http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#ServiceInvocation17
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasInput
                http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#InputParam38)
               (http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#ServiceInvocation17
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasInput
                http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#InputParam37)
               (http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#ServiceInvocation17
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasInput
                http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#InputParam36)
               (http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#ServiceInvocation17
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#HospitalLookupPortType_HospitalLookup)
               (http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#OutputParam17
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue
                http://poirot.bbn.com/owl-s/domain/HospitalLookupConcept.owl#OutputData15)
               (http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#OutputParam17
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType
                http://poirot.bbn.com/owl-s/domain/HospitalLookupConcept.owl#HospitalLookupResult)
               (http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#OutputParam17
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#HospitalPortType_HospitalLookup_HospitalRecord_OUT)
               (http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#InputParam37
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue kilometers)
               (http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#InputParam37
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType
                http://poirot.bbn.com/owl-s/domain/HospitalLookupConcept.owl#RangeUnits)
               (http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#InputParam37
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#HospitalPortType_HospitalLookup_Units_IN)
               (http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#InputParam39
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue C2)
               (http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#InputParam39
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType
                http://poirot.bbn.com/owl-s/domain/ConditionLookupConcept.owl#ConditionId)
               (http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#InputParam39
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#HospitalPortType_HospitalLookup_ConditionId_IN)
               (http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#InputParam38
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue P__2)
               (http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#InputParam38
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType http://www.w3.org/2001/XMLSchema#string)
               (http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#InputParam38
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#HospitalPortType_HospitalLookup_Loccode_IN)
               (http://poirot.bbn.com/owl-s/domain/HospitalLookupConcept.owl#OutputData15
                http://poirot.bbn.com/owl-s/domain/HospitalLookupConcept.owl#HospitalRecordElement
                http://poirot.bbn.com/owl-s/domain/HospitalLookupConcept.owl#OutputData15_1)
               (http://poirot.bbn.com/owl-s/domain/HospitalLookupConcept.owl#OutputData15
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://poirot.bbn.com/owl-s/domain/HospitalLookupConcept.owl#HospitalList)
               (http://poirot.bbn.com/owl-s/domain/HospitalLookupConcept.owl#OutputData15_1
                http://poirot.bbn.com/owl-s/domain/HospitalLookupConcept.owl#Distance 865.0)
               (http://poirot.bbn.com/owl-s/domain/HospitalLookupConcept.owl#OutputData15_1
                http://poirot.bbn.com/owl-s/domain/HospitalLookupConcept.owl#Loccode H__3)
               (http://poirot.bbn.com/owl-s/domain/HospitalLookupConcept.owl#OutputData15_1
                http://poirot.bbn.com/owl-s/domain/HospitalLookupConcept.owl#HospitalName Medical Ship)
               (http://poirot.bbn.com/owl-s/domain/HospitalLookupConcept.owl#OutputData15_1
                http://poirot.bbn.com/owl-s/domain/HospitalLookupConcept.owl#HospitalID H3)
               (http://poirot.bbn.com/owl-s/domain/HospitalLookupConcept.owl#OutputData15_1
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://poirot.bbn.com/owl-s/domain/HospitalLookupConcept.owl#HospitalRecord)
               (http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#InputParam36
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue 1000)
               (http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#InputParam36
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType http://www.w3.org/2001/XMLSchema#double)
               (http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#InputParam36
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#HospitalPortType_HospitalLookup_Range_IN)
               (http://poirot.bbn.com/owl-s/domain/TripReserveProcessModel.owl#InputParam42
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue P__2)
               (http://poirot.bbn.com/owl-s/domain/TripReserveProcessModel.owl#InputParam42
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType http://www.w3.org/2001/XMLSchemastring)
               (http://poirot.bbn.com/owl-s/domain/TripReserveProcessModel.owl#InputParam42
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://poirot.bbn.com/owl-s/domain/TripReserveProcessModel.owl#TripReservePortType_TripReserve_OriginLoccode_IN)
               (http://poirot.bbn.com/owl-s/domain/TripReserveProcessModel.owl#ServiceInvocation18
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasOutput
                http://poirot.bbn.com/owl-s/domain/TripReserveProcessModel.owl#OutputParam18)
               (http://poirot.bbn.com/owl-s/domain/TripReserveProcessModel.owl#ServiceInvocation18
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasInput
                http://poirot.bbn.com/owl-s/domain/TripReserveProcessModel.owl#InputParam42)
               (http://poirot.bbn.com/owl-s/domain/TripReserveProcessModel.owl#ServiceInvocation18
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasInput
                http://poirot.bbn.com/owl-s/domain/TripReserveProcessModel.owl#InputParam41)
               (http://poirot.bbn.com/owl-s/domain/TripReserveProcessModel.owl#ServiceInvocation18
                http://www.daml.org/services/owl-s/1.1/Process.owl#hasInput
                http://poirot.bbn.com/owl-s/domain/TripReserveProcessModel.owl#InputParam40)
               (http://poirot.bbn.com/owl-s/domain/TripReserveProcessModel.owl#ServiceInvocation18
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://poirot.bbn.com/owl-s/domain/TripReserveProcessModel.owl#TripReservePortType_TripReserve)
               (http://poirot.bbn.com/owl-s/domain/TripReserveProcessModel.owl#InputParam40
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue P4)
               (http://poirot.bbn.com/owl-s/domain/TripReserveProcessModel.owl#InputParam40
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType
                http://poirot.bbn.com/owl-s/domain/PatientLookupConcept.owl#PatientId)
               (http://poirot.bbn.com/owl-s/domain/TripReserveProcessModel.owl#InputParam40
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://poirot.bbn.com/owl-s/domain/TripReserveProcessModel.owl#TripReservePortType_TripReserve_PatientId_IN)
               (http://poirot.bbn.com/owl-s/domain/TripReserveProcessModel.owl#InputParam41
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue H__3)
               (http://poirot.bbn.com/owl-s/domain/TripReserveProcessModel.owl#InputParam41
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType http://www.w3.org/2001/XMLSchemastring)
               (http://poirot.bbn.com/owl-s/domain/TripReserveProcessModel.owl#InputParam41
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://poirot.bbn.com/owl-s/domain/TripReserveProcessModel.owl#TripReservePortType_TripReserve_DestLoccode_IN)
               (http://poirot.bbn.com/owl-s/domain/TripReserveProcessModel.owl#OutputParam18
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterValue
                http://poirot.bbn.com/owl-s/domain/TripReserveConcepts.owl#SuccessfulReservation)
               (http://poirot.bbn.com/owl-s/domain/TripReserveProcessModel.owl#OutputParam18
                http://www.daml.org/services/owl-s/1.1/Process.owl#parameterType
                http://poirot.bbn.com/owl-s/domain/TripReserveConcept.owl#TripReserveResult)
               (http://poirot.bbn.com/owl-s/domain/TripReserveProcessModel.owl#OutputParam18
                http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://poirot.bbn.com/owl-s/domain/TripReserveProcessModel.owl#TripReservePortType_TripReserve_Result_OUT)
               (http://poirot.bbn.com/2006/05/core#semanticTrace_12 http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://poirot.bbn.com/2006/05/core#SemanticTrace)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_12_1 http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://poirot.bbn.com/2006/05/core#SemanticTraceElement)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_12_1 http://poirot.bbn.com/2006/05/core#index 1)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_12_1 http://poirot.bbn.com/2006/05/core#elementOf
                http://poirot.bbn.com/2006/05/core#semanticTrace_12)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_12_1 http://poirot.bbn.com/2006/05/core#serviceInvocation
                http://poirot.bbn.com/owl-s/domain/PatientLookupProcessModel.owl#ServiceInvocation16)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_12_2 http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://poirot.bbn.com/2006/05/core#SemanticTraceElement)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_12_2 http://poirot.bbn.com/2006/05/core#index 2)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_12_2 http://poirot.bbn.com/2006/05/core#elementOf
                http://poirot.bbn.com/2006/05/core#semanticTrace_12)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_12_2 http://poirot.bbn.com/2006/05/core#serviceInvocation
                http://poirot.bbn.com/owl-s/domain/HospitalLookupProcessModel.owl#ServiceInvocation17)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_12_3 http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                http://poirot.bbn.com/2006/05/core#SemanticTraceElement)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_12_3 http://poirot.bbn.com/2006/05/core#index 3)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_12_3 http://poirot.bbn.com/2006/05/core#elementOf
                http://poirot.bbn.com/2006/05/core#semanticTrace_12)
               (http://poirot.bbn.com/2006/05/core#semanticTraceElement_12_3 http://poirot.bbn.com/2006/05/core#serviceInvocation
                http://poirot.bbn.com/owl-s/domain/TripReserveProcessModel.owl#ServiceInvocation18))

Name: "ServiceInvocation16"
Type: "PatientPortType_PatientLookup"
Input: 
      "InputParam35" = "P4"
Output: 
      "OutputParam16" = #<URI http://poirot.bbn.com/owl-s/domain/PatientLookupConcept.owl#OutputData14>
   (("OutputData14" "PatientLoccode" "P__2") ("OutputData14" "ConditionId" "C2") ("OutputData14" "PatientId" "P4")
    ("OutputData14" "type" "PatientRecord"))


Name: "ServiceInvocation17"
Type: "HospitalLookupPortType_HospitalLookup"
Input: 
      "InputParam39" = "C2"
      "InputParam38" = "P__2"
      "InputParam37" = "kilometers"
      "InputParam36" = "1000"
Output: 
      "OutputParam17" = #<URI http://poirot.bbn.com/owl-s/domain/HospitalLookupConcept.owl#OutputData15>
   (("OutputData15" "HospitalRecordElement" "OutputData15_1") ("OutputData15" "type" "HospitalList"))


Name: "ServiceInvocation18"
Type: "TripReservePortType_TripReserve"
Input: 
      "InputParam42" = "P__2"
      "InputParam41" = "H__3"
      "InputParam40" = "P4"
Output: 
      "OutputParam18" = #<URI http://poirot.bbn.com/owl-s/domain/TripReserveConcepts.owl#SuccessfulReservation>
   NIL

Finished Input Processing

|#


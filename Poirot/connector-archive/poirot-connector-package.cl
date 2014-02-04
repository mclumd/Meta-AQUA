;;; This file contains only the package definition for the poirot lisp
;;; connector. For a lisp component to use the connector package, it should
;;; have a statement such as the following in the file .clinit.cl.
;;; 
;;; (unless (find-package "POIROT-CONNECTOR")
;;;   (load (concatenate 'string *poirot-java-home*
;;; 		     "\\lisp\\src\\lisp\\poirot-connector-package")))
;;; 
(require :jlinker)
(defpackage "POIROT-CONNECTOR"
  (:nicknames :pc)
  (:use "COMMON-LISP" "JAVATOOLS.JLINKER" "NET.URI")
  (:export "*POIROT-JAVA-HOME*" 
	   "*POIROT-PORT*" 
	   "*POIROT-COMPONENT-NAMESPACE*"
	   "*POIROT-COMPONENT-NAME*"
	   "*POIROT-COMPONENT-DESCRIPTION*"
	   "*LOADED-CALLBACK*"
	   "*EXECUTE-CALLBACK*"
	   "*POIROT-SAMPLE-QUERY*"
	   "*NEW-SUBSCRIPTION-DATA*"
	   "DONE-PROCESSING"
	   "START-POIROT-CONNECTION"
	   "GET-COMPONENT-NAME"
	   "GET-COMPONENT-STATE"
	   "GET-COMPONENT"
	   "GET-NEW-SUBSCRIPTION-RESULTS"
	   "CONVERT-SUBSCRIPTION-RESULT-LIST"
	   "EXECUTE-QUERY"
	   "GET-FULL-URI"
	   "IS-URI"
	   "GET-CLASS-URI"
	   "GET-PROPERTY-URI"
	   "PUBLISH-ADD"
	   "PUBLISH-ADDLIST"
	   "CREATE-INSTANCE-TRIPLE"
	   "ADD-PROPERTY-TRIPLE"
	   "PARSE-TRACE"
	   "CONVERT2TRIPLES"
	   "SHOW-SWS-CALL"
	   "SHOW-SWSS"
	   "*BB*"
	   )
  )

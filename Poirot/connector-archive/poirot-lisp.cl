;; Poirot Lisp Connector using jLinker
;;

(in-package :poirot-connector)



;; -----------------------------------------------------------------
;; Edit these 5 variables as needed. See loader.cl for instructions.
(defvar *poirot-java-home* "c:\\poirot\\coretrunk")

;;; Changing the port requires changing the port in the Tomcat config file
(defvar *poirot-port* 9090)

;; If no namespace is specified for a subject or predicate, this namespace is used
(defvar *poirot-component-namespace* "http://poirot.bbn.com/2006/05/hyp#")
(defvar *poirot-component-name* "TestLispHypothesizer")
(defvar *poirot-component-description* "Dumb hypothesizer for testing the lisp connector")
;; ----------------------------------------------------------------


;; CLASSPATH setup
(setf *poirot-jars* '("lisp\\build\\lib\\poirot-lisp.jar"
		      "core\\build\\lib\\poirot-core.jar"
		      "common\\build\\lib\\poirot-common.jar"
		      "extlib\\sesame\\lib\\sesame.jar"
		      "extlib\\sesame\\lib\\openrdf-model.jar"
		      "extlib\\sesame\\lib\\rio.jar"
		      "extlib\\other\\lib\\log4j-1.2.13.jar"
		      "core\\config" ;; for the log4j.configuration
		      ))

;; These variables will be set when the component connects
(defvar *poirot-component* nil)
(defvar *poirot-component-class* nil)

;; All in one method to start the Java VM, register subscriptions
;;   and open the connection to the poirot core.
(defun start-poirot-connection (query-list)
  (when (start-poirot-java)
    (if (not (listp query-list))
	(setf query-list (list query-list)))
    (dolist (query query-list)
      (add-subscription query))
    (start-client-connector *poirot-port*)))

;; This method starts the Java JVM and the jlinker connection to it.
(defun start-poirot-java ()
  (setf *poirot-java-classpath* nil)
  (dolist (jar *poirot-jars*)
    
    (if (null *poirot-java-classpath*)
	(setf *poirot-java-classpath* 
	  (format nil "~A\\~A" *poirot-java-home* jar))
      (setf *poirot-java-classpath* 
	(format nil "~A;~A\\~A" *poirot-java-classpath* 
		*poirot-java-home* jar))))
    
  (format t "Poirot Classpath: ~A~%" *poirot-java-classpath*)
        
  (jlinker-init :start-java :classpath *poirot-java-classpath*)
  
  (setf *poirot-component-class* 
    (JCLASS "com.bbn.poirot.lisp.RemoteLispComponent"))
  
  (if *poirot-component-class*
      t
    nil))

;; This is an example Poirot subscription query
(defvar *poirot-sample-query* 
    "SELECT x FROM {x} rdf:type {poirot:SemanticTrace} USING NAMESPACE poirot = <http://poirot.bbn.com/2006/05/core#>, rdf = <http://www.w3.org/1999/02/22-rdf-syntax-ns#>")

;; Add a subscription query to your poirot component.
;; Currently all subscriptions must be added before the component gets
;; loaded.  
(defun add-subscription (query)
  (let ((addsub-method (JMETHOD *poirot-component-class* "addSubscription" 1)))
    (jcall addsub-method nil query))
  t)

;; Set the use abbreviated URI flag.
;; If true, you'll received abbreviated URIs from poirot, with the namespace
;;   replaced with an abbreviation, i.e. ns1:OutputData
;; If false, you'll get the full valid URI, i.e. http://www.daml.org/.../#OutputData
;; Default is false.
(defun set-abbreviated-namespace-flag (flag)
  (let ((ab-method (JMETHOD *poirot-component-class* "setAbbreviatedUri" 1)))
    (jcall ab-method nil flag))
  t)

;; Start the poirot component's connection to the poirot meta controller.
;; Currently this method must be called before the poirot core tomcat server
;;  has been started. 
;; This component should be listed in the poirot-config.xml file:
;;  <component class="com.bbn.poirot.lisp.RemoteLispComponent">
;;    <connector type="remote" host="localhost" port="9090" />
;;</component>

(defun start-client-connector (port)
  
  (format t "Trying to start Poirot Client Connector on ~D~%" port)
  
  (let* ((rcc-class (JCLASS "com.bbn.poirot.common.remote.ClientConnector"))
	 (sc-method (JMETHOD RCC-CLASS "startConnector" 1)))
    
    (jcall sc-method nil port))
  t)

(defun get-component-name ()
  (if *poirot-component*
      (let ((cname-method 
	     (JMETHOD *poirot-component-class* "getComponentName")))
	(jcall cname-method *poirot-component*))
    (format nil "Poirot Core has not yet loaded your component. No status information is available.")))

(defun get-component-state ()
  (if *poirot-component*
      (let ((state-method (JMETHOD *poirot-component-class* "getState")))
	(jcall state-method *poirot-component*))
    "Unloaded"))

;; Remote Accessed Methods
;; These methods are called by the Java code, from the class
;;  com.bbn.poirot.lisp.RemoteLispConnection

(defun get-component ()
  (let* ((grlc-method (JMETHOD *poirot-component-class* "getRemoteLispComponent")))
    (setf *poirot-component* (jcall grlc-method nil)))
  
  (set-namespace *poirot-component-namespace*)
  (set-name *poirot-component-name*)
  (set-description *poirot-component-description*)
  
  (if (not (null *loaded-callback*))
      (funcall *loaded-callback*))
   
  *poirot-component*)

;; Callback function you can define that will be called when the component
;;  gets loaded by the core
(defvar *loaded-callback* nil)

;; Set the default namespace
(defun set-namespace (new-ns) 
  (if *poirot-component*
      (let ((setns-method 
	     (JMETHOD *poirot-component-class* "setDefaultNamespace" 1)))
	(jcall setns-method *poirot-component* new-ns)
	t)
    (format nil "Poirot Core has not yet loaded your component.~% State variable setting is unavailable until your component has been loaded.")))

;; Set the component name
(defun set-name (new-name) 
  (if *poirot-component*
      (let ((setns-method 
	     (JMETHOD *poirot-component-class* "setComponentName" 1)))
	(jcall setns-method *poirot-component* new-name)
	t)
    (format nil "Poirot Core has not yet loaded your component.~% State variable setting is unavailable until your component has been loaded.")))

;; Set the component description
(defun set-description (new-desc) 
  (if *poirot-component*
      (let ((setns-method 
	     (JMETHOD *poirot-component-class* "setComponentDescription" 1)))
	(jcall setns-method *poirot-component* new-desc)
	t)
     (format nil "Poirot Core has not yet loaded your component.~% State variable setting is unavailable until your component has been loaded.")))

;; There are two methods for handling the execute method, which 
;; gets called by the poirot meta controller.
;; First, you can specify a callback method
;; The execute callback should take one single argument,
;;  which will be the subscription result list.
(defvar *execute-callback* nil)

;; The second method is to have another thread poll for
;; new results, which are accessed through this variable
(defvar *new-subscription-data* nil)

;; Get new results given by the execute method
;; This method can be polled by a lisp process
;; This version resets the result global variable when they
;;  are returned, so only one process can get one set of results
(defun get-new-subscription-results ()
  (if (null *new-subscription-data*) 
      nil
    (progn
      (let ((results *new-subscription-data*))
	(setf *new-subscription-data* nil)
	results))))

;; The execute method will block until your lisp process
;; has finished, allowing the lisp code to perform additional
;; blackboard queries or post results to the blackboard.
;; Informing the execute method that you're done by setting 
;; this variable back to t.  It gets set to nil during poirot-execute.
(defvar *execute-finished* t)

;; This is the method that will be called from the Poirot meta
;;   controller whenever we have a result from a new subscription.
;; The arguments are:
;;  flat-result-list: a flattened array of the subscription results
;;  sub-list-length: length of sub lists embedded in result-list
;; Example: result-list #(a b c d e f) and sub-list-length 2 
;;          should be converted to ((a b) (c d) (e f))

(defun poirot-execute (flat-result-array sub-list-length) 
  (format t "Java Calling [t] ~a ~a ~%" flat-result-array
	  sub-list-length)
  (format *standard-output* "Java Calling [*standard-output*] ~a ~a~%" flat-result-array sub-list-length)

  (let ((result-list (convert-subscription-result-list 
		      flat-result-array sub-list-length)))
    
    (setf *execute-finished* nil)
    
    (setf *new-subscription-data* result-list)
 
    (if (not (null *execute-callback*))
	(funcall *execute-callback* result-list))
    
    (loop while (null *execute-finished*) do
	  (progn
	    (sleep 1)
	    (format t "Waiting for execution to finish~%"))))
  
  (format t "Execute finished.~%")
  
  t)

(defun done-processing ()
  (setf *execute-finished* t))

;; Helper method to convert the flattened result list back into a
;;   multi dimensional list.

(defun convert-subscription-result-list (result-array sub-list-length)
  (let ((conv-list nil)
	(sub-list nil)
	(result nil))
    (dotimes (i (length result-array))
      (setf result (aref result-array i))
      (if (< (length sub-list) sub-list-length)
	  (setf sub-list (append sub-list (list result)))
	(progn
	  (setf conv-list (append conv-list (list sub-list)))
	  (setf sub-list (list result)))))
    (if (> (length sub-list) 0)
	(setf conv-list (append conv-list (list sub-list))))
	
    conv-list))

;; Query helper methods

;; Get the full URI for a prefix abbreviated URI
(defun get-full-uri (str)
  (if *poirot-component*
      (let ((uri-method (JMETHOD *poirot-component-class* "getFullUri" 1)))
	(jcall uri-method *poirot-component* str))
    (format nil "Poirot Core has not yet loaded your component.~% Get Full URI function is unavailable until your component has been loaded.")))


;; is-uri
;; Determines whether the parameter string is a URI or not by checking
;;  for a preceding http://
(defun is-uri (str) 
  (if (> (length str) 6)
      (if (string-equal (subseq str 0 7) "http://")
	  t nil) 
    nil))

;; This method gets the URI for a class in the poirot core ontology
(defun get-class-uri (cls)  
  (if *poirot-component*
      (let ((cls-method (JMETHOD *poirot-component-class* "getClassUri" 1)))
	(jcall cls-method *poirot-component* cls))
    (format nil "Poirot Core has not yet loaded your component.~% Get Class URI function is unavailable until your component has been loaded.")))

;; This method gets the URI for a property in the poirot core ontology
(defun get-property-uri (prop)
  (if *poirot-component*
      (let ((cls-method (JMETHOD *poirot-component-class* "getPropertyUri" 1)))
	(jcall cls-method *poirot-component* prop))
    (format nil "Poirot Core has not yet loaded your component.~% Get Property URI function is unavailable until your component has been loaded.")))

;; Query methods
(defun execute-query (subject predicate object context) 
  (if *execute-finished*
      (format nil "You can only execute a blackboard query while the component is in the executing state.")
    
    (let ((subUri (if (null subject) nil (get-class-uri subject)))
	  (predUri (if (null predicate) nil (get-property-uri predicate)))
	  (queryMethod (JMETHOD *poirot-component-class* "executeQuery" 4))
	  (contextUri context)
	  (retval nil))
      
      (when (and (not (null subject)) (null subUri))
	(setf retval (format nil "Unable to locate class for ~A~%" subject)))
      (when (and (not (null predicate)) (null predUri))
	(setf retval (format nil "Unable to locate uri for ~A~%" predicate)))
      (when (null retval)
	(format t "Subject URI is ~A~%Predicate URI is ~A~%Object is ~A~%Context is ~A~%"
		subUri predUri object contextUri)
	(setf retval 
	  (jcall 
	   queryMethod *poirot-component* subUri predUri object contextUri)))
      retval)))

;; QueryLanguage is the form of RDQL query language you want to use
;;  Possible values are "SERQL" and "SPARQL"
;; SERQL: http://www.openrdf.org/doc/sesame/users/ch06.html
;; SPARQL: http://www.w3.org/TR/rdf-sparql-query/
(defun execute-tuple-query (queryLanguage query)
  (if *execute-finished*
      (format nil "You can only execute a blackboard query while the component is in the executing state.")
    
    (let ((queryMethod (JMETHOD *poirot-component-class* "executeTupleQuery")))
      (jcall queryMethod *poirot-component* queryLanguage query))))

;; Post to blackboard methods
;; Query methods
(defun publish-add (subject predicate object context) 
  (if *execute-finished*
      (format nil "You can only publish to the blackboard while the component is in the executing state.")
    (let ((subUri (if (null subject) nil (get-class-uri subject)))
	  (predUri (if (null predicate) nil (get-property-uri predicate)))
	  (pMethod (JMETHOD *poirot-component-class* "publishAdd" 4))
	  (contextUri context)
	  (retval nil))
      
      (when (and (not (null subject)) (null subUri))
	(setf retval (format nil "Unable to locate class for ~A~%" subject)))
      (when (and (not (null predicate)) (null predUri))
	(setf retval (format nil "Unable to locate uri for ~A~%" predicate)))
      (when (null retval)
	(setf retval 
	  (jcall pMethod *poirot-component* subUri predUri object contextUri)))
      retval)))

;; Statement list should be in the form
;; ((subject predicate object) (subject predicate object) ...)

(defun publish-addlist (statement-list context)
  (if *execute-finished*
      (format nil "You can only publish to the blackboard while the component is in the executing state.")
    (let ((statement-array (make-array (* (length statement-list) 3)))
	  (pMethod (JMETHOD *poirot-component-class* "publishAdd" 2))
	  (index 0))
      
      (dolist (statement statement-list)
	(setf (aref statement-array index) (first statement))
	(setf (aref statement-array (+ index 1)) (second statement))
	(setf (aref statement-array (+ index 2)) (third statement))
	(setf index (+ index 3)))
      
      (jcall pMethod *poirot-component* statement-array context))))

;; quick helper functions for adding triples to a list of statements
(defun create-instance-triple (class-name instance-name)
  (let ((cls-uri (if (is-uri class-name) class-name 
		   (get-class-uri class-name)))
	(inst-uri (if (is-uri instance-name) instance-name
		    (get-class-uri instance-name)))
	(type-uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
    (list inst-uri type-uri cls-uri)))

(defun add-property-triple (instance-name prop-name prop-value)
  (let ((prop-uri (if (is-uri prop-name) prop-name
		    (get-property-uri prop-name)))
	(instance-uri (if (is-uri instance-name) instance-name
			(get-class-uri instance-name))))
    (list instance-uri prop-uri prop-value)))
     
      
    








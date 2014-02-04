(in-package :user)

;;;;
;;;; TO CUSTOMIZE THIS FILE FOR YOUR OWN USE, FIRST CHANGE THE FUNCTION 
;;;; init-MCL TO SUIT YOUR PURPOSES AND *MCL-port* TO THE DESIRED PORT. 
;;;; THEN ADD CALLS SIMILAR TO THE FOLOWING TO YOUR INITIALIZE CODE.
;;;; 
;;;; (set-env-vars)
;;;; (init-MCL "meta_aqua" "objSensor" "$PERSON" t)
;;;; 
;;;; USE THE FUNCTION check-with-MCL TO CALL THE MCL MONITOR.
;;;; 
;;;; (print 
;;;;   (multiple-value-list
;;;;      (check-with-MCL "meta_aqua" "objSensor" anomaly-type t)))
;;;; 
;;;; BEFORE INITIALIZING THE HOST SIDE, START MCL WITH CODE SIMILAR TO THE FOLLOWING.
;;;; 
;;;; cd /fs/metacog/group/mcl/newmcl/mcl_current/bin
;;;; export LD_LIBRARY_PATH=/fs/metacog/group/mcl/newmcl/mcl_current:/fs/metacog/group/mcl/newmcl/mcl_current/lib
;;;; export MCL_CONFIG_PATH=/fs/metacog/group/mcl/newmcl/mcl_current/config/
;;;; export MCL=/fs/metacog/group/mcl/newmcl/mcl_current
;;;; export MCL_CORE_PATH=/fs/metacog/group/mcl/newmcl/mcl_current
;;;; ./server --mcl.port 5155


(defvar *time-out* 100
  "Time out period to make socket connection.")

(defvar *active-socket* nil
  "Bi-directional socket text stream.")

(defparameter *MCL-port* 5156
  "Port through which host and MCL communicate."
  )


;;; 
;;; Function establish-active-agent-connection has been modified from
;;; the code in file
;;; /fs/junkfood/mcox/prodigy/working/system/agent/latest-source/application-agent.lisp
;;; The function adds the optional host argument to communicate with
;;; remote hosts.
;;; 
;;; Changed from an assignment of the make-socket call to
;;; *active-socket* to the assigment of the socket to whatever was the
;;; value of socket-name. Thus we can use the function now to
;;; establish active socket connections for multiple sockets. 
;;; [mcox 4oct13]
;;; 
(defun establish-active-agent-connection (port-to-connect 
					  &optional
					  (host "localhost")
					  (socket-name '*active-socket*))
  ;;wait for a time interval *time-out* for the connection to
  ;;be established
  (socket:with-pending-connect
      (mp:with-timeout (*time-out* (error "Timeout: Connect failed."))
	(set socket-name 
	  (socket:make-socket :remote-host host :remote-port port-to-connect))))
  )



#|
Examples 

The MCL response string is of the form "ok([Response(parameter=value*)])"

(setf x "ok([response(type=suggestion,ref=0x00000001,code=crc_extended_code,action=true,abort=true,text=\"MCL response = (29,evaluateObject,crc_extended_code)\")])")

CL-USER(69): (read-from-string (substitute #\Space #\, x) t nil :start 2)
([RESPONSE (TYPE=SUGGESTION REF=0X00000001 CODE=CRC_EXTENDED_CODE ACTION=TRUE ABORT=TRUE TEXT= "MCL response = (29 evaluateObject crc_extended_code)") ])
152

[3] CL-USER(80): (seventh (second (read-from-string (substitute #\Space #\, x) t nil :start 2)))
"MCL response = (29 evaluateObject crc_extended_code)"

[3] CL-USER(81): (read-from-string (seventh (second (read-from-string (substitute #\Space #\, x) t nil :start 2))))
MCL
4

CL-USER(8): (second (second (read-from-string (substitute #\Space #\, x) t nil :start 2)))
REF=0X00000001

|#

;;; 
;;; Function parse-response takes as input an MCL response string such as in 
;;; the example above, and it replaces commas with spaces, reads the Lisp 
;;; object after the ok, then returns the response parameters (i.e., fields).
;;; 
(defun parse-response (response)
  ;; Get the response parameters
  (second
   ;; Read the list structure
   (read-from-string 
    (commas2spaces response)
    ;; skips past the "Ok" response part to get to the parenthetical fields
    t nil :start 2) 
   )
  )



(defun commas2spaces (str)
  "Change all commas in input string to spaces."
  (substitute #\Space #\, str)
  )


;;; "hello=world" --> "world"
(defun return-after-equal-sign (str)
  (subseq str 
	  (1+ (position #\= str))
	  (length str))
  )

;;; 
;;; The reference parameter is of the form ref=0x00000001
;;; 
(defun get-ref (parameters)
  (intern				;Change back to symbol
     (return-after-equal-sign 
      (string (second Parameters)))
    )
  )


;;; 
;;; The suggestion parameter is of the form 
;;; TEXT= "MCL response = (29 evaluateObject crc_extended_code)"
;;; although the seventh item in the parameters argument to the function is the
;;; text srting itself.
;;; 
(defun get-suggestion (parameters)
  (second 
   (read-from-string 
    (return-after-equal-sign 
     (seventh parameters))
    ))
  )


    
;;;; 
;;;; File MCL.lisp implements the functions to integrate host with the
;;;; MetaCognitive Loop. To use this file, load Prodigy/Agent. Read
;;;; the file README (need to update README).
;;;; 

;;; 
;;; Function prepend-dollar adds a dollar symbol to the front of the input 
;;; symbol. Thus a-symbol --> $a-symbol
;;; 
(defun prepend-dollar (symbol)
  "symbol --> $symbol"
  (intern 
   (coerce 
    (cons #\$ 
	  (coerce (string symbol) 'list)) 
    'string))
  )


;;; 
;;; Function check-with-MCL represents the monitor call from the host
;;; to MCL to check on the value of a sensor. The function returns
;;; either OK or FAIL (as does the MCL monitor call). If OK the
;;; function will also return the reference frame and suggestion
;;; provided by MCL. If print2screen is non-nil, then text is sent to
;;; standard-output to indicate the data exchanged between the host
;;; and MCL.
;;;
(defun check-with-MCL (host-name
		       sensor-name
		       sensor-value
		       &optional
		       print2screen)
  (let (
	(temp
	 (send2MCL 
	  (format nil 
		  "monitor(~a,{~a=~s})"
		  host-name
		  sensor-name
		  (prepend-dollar sensor-value))
	  print2screen))
	parameters
	reference-frame
	suggestion)
    (if (equal (intern 'OK) (read-from-string temp))
	(setf parameters 
	  (parse-response temp)))
    (cond (parameters
	   (setf reference-frame 
	     (get-ref parameters))
	   (if print2screen
	       (format t 
		       "~%Reference frame = ~S"
		       reference-frame))
	   (setf suggestion
	     (get-suggestion parameters))
	   (if print2screen
	       (format t 
		       "~%MCL Suggestion = ~S"
		       suggestion))
	   (values (read-from-string 
		    temp)
		   reference-frame
		   suggestion))
	  (t
	   (read-from-string
	    temp))))
  )


;;; Set 4 environment variables needed for MCL. This may not be necessary from 
;;; within the isp environment. Perform at command prompt instead. See 
;;; cheat-sheet.txt.
;;; 
(defun set-env-vars ()
  (setf (sys:getenv "LD_LIBRARY_PATH") 
    "/fs/metacog/group/mcl/newmcl/mcl_current:/fs/metacog/group/mcl/newmcl/mcl_current/lib")
  (setf (sys:getenv "MCL_CONFIG_PATH")
    "/fs/metacog/group/mcl/newmcl/mcl_current/config/")
  (setf (sys:getenv "MCL") 
    "/fs/metacog/group/mcl/newmcl/mcl_current")
  (setf (sys:getenv "MCL_CORE_PATH") 
    "/fs/metacog/group/mcl/newmcl/mcl_current")
  )


;;; 
;;; Function send2MCL takes a command string and prints it and then sends it to
;;; MCL over the given socket. It flushes the output and reads and prints the
;;; MCL response. Finally the MCL response is returned. Note that MCL will 
;;; return either fail or OK and then a parenthetical comment such as 
;;; "ok(initialized 'host'.)" 
;;; 
(defun send2MCL (command-str
		 &optional 
		 print2screen
		 (socket *active-socket*)
		 &aux 
		 (MCL-response "")
		 )
  (if print2screen
      (format t "~%Host: ~s" command-str))
  (format socket command-str)
  (finish-output socket)
  (setf MCL-response 
    (read-line socket))
  (if print2screen
      (format t "~%MCL: ~s" MCL-response))
  MCL-response
  )


;;; 
;;; Function init-MCL initializes MCL by first establishing a handshake over
;;; the given socket and then by sending MCL commands that initialize and 
;;; configure the system and establish the sensors and expectations. Two trial 
;;; monitor calls are made to test the interface. One should be OK the other 
;;; should fail. 
;;;
;;; CUSTOMIZE STRINGS FOR YOUR OWN USE. CURRENTLY THIS CODE HAS ONE SENSOR ONLY.
;;; TO HAVE MULTIPLE SENSORS, ONE WOULD HAVE A LIST OF SENSOR-NAMEs AND 
;;; CORRESPONDING LEGAL-VALUEs.
;;; 
(defun init-MCL (host
		 sensor-name
		 legal-value
		 &optional
		 print2screen
		 (port *MCL-port*)
		 (socket *active-socket*))
  (establish-active-agent-connection port)
  (do ((cmd-list 
	`(,(format nil "initialize(~a,0)" host)
	  ,(format nil "configure(~a,MA,MA)" host)
	  ,(format nil "declareObservableSelf(~a,~a,0)" host sensor-name)
	  ,(format nil "setObsPropSelf(~a,~a,prop_sclass,sc_object)" host sensor-name)
	  ,(format nil "addObsLegalValSelf(~a,~a,~a)" host sensor-name legal-value)
	  ,(format nil "declareEG(~a,0)" host)
	  ,(format nil "declareSelfExp(~a,0,ec_be_legal,~a)" host sensor-name)
	  ;;,(format nil "monitor(~a,{~a=~a})" host sensor-name legal-value)
	  ;;,(format nil "monitor(~a,{~a=101})" host sensor-name)
	  ;;,(format nil "suggestionImplemented(~a,1)" host)
	  )
	(rest cmd-list))
       (result "" (send2MCL (first cmd-list)
			    print2screen)))
      ((or (null cmd-list) (equal result 'FAIL))
       result)
    )
  )


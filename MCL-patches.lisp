(in-package :user)

;;;; This file adapts code that was defined elsewhere. The socket code is 
;;;; from Prodigy/Agent and the remainder from Meta-AQUA.


;; From /fs/junkfood/mcox/prodigy/working/system/agent/latest-source/application-agent.lisp
(defvar *time-out* 100)  ;;; Increased from 10 to 100 [mcox 7may05]
(defvar *active-socket* nil)

;;; 
;;; Function establish-active-agent-connection has been modified from the code
;;; in file /fs/junkfood/mcox/prodigy/working/system/agent/latest-source/application-agent.lisp
;;; The function adds the optional host argument.
;;; 
(defun establish-active-agent-connection (port-to-connect 
					  &optional
					  (host "localhost"))
  ;;wait for a time interval *time-out* for the connection to
  ;;be established
  (socket:with-pending-connect
      (mp:with-timeout (user::*time-out* (error "Timeout: Connect failed."))
  (setf user::*active-socket* 
    (socket:make-socket :remote-host host :remote-port port-to-connect))))
  )

;; (init-MCL 5150)



;;; The following function was defined in loader.lisp. 
;;; Added first two calls to initialize the Meta-AQUA/MCL interface.

;;; 
;;; The file init.lisp defines these initialization functions.
;;; 
(defun init-Meta-AQUA () 
  (set-env-vars)
  (init-MCL "meta_aqua" "objSensor" "$PERSON" t *MCL-port*)
  (in-package :meta-aqua) 
  ;; Added 4aug11 mcox so that opened files will be in Results directory
  (setf *default-pathname-defaults* 
    (pathname 
     (concatenate 
	 'string 
       *META-AQUA-SYSTEM-DIR* 
       "Results/")))
  (meta-aqua::init-4-speed) 
  (meta-aqua::init-aqua)
  (format 
   t "~%Meta-AQUA, Version 6, Copyright (C) 2005 Michael T. Cox~%")
  (format 
   t "~%Meta-AQUA comes with ABSOLUTELY NO WARRANTY.~%")
  (format 
   t "~%This is free software, and you are welcome to redistribute it")
   (format 
    t "~%under certain conditions as defined in the Gnu General Public License,")
   (format 
    t "~%Version 2.~%~%")
  )


(in-package :metaaqua)


;;;
;;; This code was initially in lowlevel.lisp.
;;; 


;;;
;;; Function declare-anomaly simply prints the discovery of an anomaly.
;;; 
(defun declare-anomaly (concept anomaly-type path
			)
  (when (and
	 (equal anomaly-type 'LUGGAGE)
	 (equal (first path) 'OBJECT))
    (print 
     (multiple-value-list
      (user::check-with-MCL "meta_aqua" "objSensor" anomaly-type t))))
  (format
   *aqua-window*
   (str-concat
    "~%~%Anomaly detected: "
    "Odd for a ~s~%  to be in "
    "path ~s of a ~s.")
   anomaly-type path concept)
  )


(in-package :user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Loader file for POIROT/Meta-AQUA component
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Demo Procedure:
;;; (0.)Create an Allegro init file .clinit.cl 
;;; 1.  Start Allegro CL binary
;;; 2. Start Tomcat at default. E.g., 
;;;    "C:\Program Files\Apache Software Foundation\Tomcat 5.5\bin\tomcat5.exe")
;;; 3. Open browser at default E.g., http://localhost:8008/poirot-servlet/
;;; 4. Select Debugger link, Click Run button
;;; 5. Perform actions with POIROT interface
;;; 6. Select Session Manager link, click 2bb button
;;; 7. POIROT meta-controller should make available Semantic Trace 
;;;; 
;;;; 
;;;; Below in comments is an example of code to place at the bottom of the
;;;; Allegro CL initialization file .clinit.cl to establish custom bindings for
;;;; a POIROT Lisp Component and to load the POIROT Lisp Connector files.
;;;; 

(defparameter *poirot-lisp-component-home*
    (make-pathname :directory 
		   (pathname-directory 
		    *load-truename*))
  "Op-sys independent path to dir where component exists."
  )

;;; The defsystem code
(require :asdf)

(pushnew *poirot-lisp-component-home* asdf:*central-registry*)

;;; Load the component subsystem
(asdf:oos 'asdf:load-op :Meta-AQUA-component)

;;; I think that the following is obsolete. [mcox 20nov06]
#|
;;; The (load-meta-aqua-windows) call is currently (5oct06) in the .clinit.cl. 
;;; Change later.
(in-package :user)
(when (or *autostart-interface*
	  (y-or-n-p "Start POIROT Lisp interface? "))
  (start-poirot-connection *poirot-sample-query*)
    (when (and (not *do-compile-Meta-AQUA*)
	       (or *autostart-interface*
		   (y-or-n-p "Load Meta-AQUA internal structures display?"))
	       )
      (load-meta-aqua-windows)
      
      )
    )
|#

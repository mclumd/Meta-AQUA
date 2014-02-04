(in-package :user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Loader file for POIROT Lisp connector.
;;;; 
;;;; Below in comments is an example of code to place in the Allegro CL
;;;; initialization file .clinit.cl to establish custom bindings for a POIROT
;;;; Lisp Component and to load the POIROT Lisp Connector files.
;;;; 
#|
;; Overrides the value of the variable in POIROT connector file loader.cl
(setf *poirot-lisp-connector-home* "c:/poirot/coretrunk/lisp/src/lisp/")

(unless (find-package "POIROT-CONNECTOR")
  (load 
   (concatenate 
       'string
     *poirot-lisp-connector-home*
     "poirot-connector-package"))
  )

(use-package :pc)

;; -----------------------------------------------------------------
;; Overrides the values of the variables in file poirot-lisp.cl
(setf *poirot-java-home* "c:/poirot/coretrunk")
(setf *poirot-port* 9095)
(setf *poirot-component-name* "Meta-AQUA")
(setf *poirot-component-description* 
  "Initial testing of Meta-AQUA with the lisp connector")

(setf *execute-callback* 
  #'(lambda (results)
      (test-Meta-AQUA
       results)))

(setf *loaded-callback* 
  #'(lambda ()
      (init-Meta-AQUA)))

(load 
 (concatenate 
     'string 
   *poirot-lisp-connector-home*
   "loader.cl"))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The following variable must be assigned a value in the file .clinit.cl
;;; or some initialization file that is loaded prior to this file.
(defvar *poirot-lisp-connector-home* "c:/poirot/coretrunk/lisp/src/lisp/")


(load 
 (concatenate 
     'string 
   *poirot-lisp-connector-home* 
   "poirot-connector-package.cl"))

(load (concatenate 'string *poirot-lisp-connector-home* "jl-config"))

(load (concatenate 'string *poirot-lisp-connector-home* "poirot-lisp.cl"))

(load (concatenate 'string *poirot-lisp-connector-home* "rdf.cl"))

;;;(load (concatenate 'string *poirot-lisp-connector-home* "example.cl"))

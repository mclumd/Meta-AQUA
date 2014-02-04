;;; This file is now (as of 21aug06) obsolete. The contents is within
;;; .clinit.cl

;; Overrides the values of the variables in POIROT connector file
;; loader.cl
;;

(setf *poirot-lisp-connector-home* "c:/poirot/coretrunk/lisp/src/lisp/")
(setf *poirot-lisp-component-home* "c:/Meta-AQUA/Poirot/")

(defvar *load-poirot-connector* t)

(unless (or (not *load-poirot-connector*)
	    (find-package "POIROT-CONNECTOR"))
  (load 
   (concatenate 
       'string
     *poirot-lisp-connector-home*
     "poirot-connector-package"))
  (use-package :pc)
  ;; -----------------------------------------------------------------
  ;; Overrides the values of the variables in file poirot-lisp.cl
  ;;
  (setf *poirot-java-home* "c:\\poirot\\coretrunk")
  (setf *poirot-port* 9095)
  (setf *poirot-component-name* "Meta-AQUA")
  (setf *poirot-component-description* 
    "Initial testing of Meta-AQUA with the lisp connector")
  ;; See file trans2frames.lisp
  (setf *execute-callback* 
    #'(lambda (results)
	(test-Meta-AQUA
	 results)))
  )


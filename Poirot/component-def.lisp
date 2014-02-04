(in-package :user)

;;;; 
;;;; This file defines the five key features of a POIROT component. The are the
;;;; component name, description, port number, execute callback, and load-time
;;;; callback. The execute callback defines the Meta-AQUA behavior when the
;;;; POIROT meta-controller presents it with the current content of the
;;;; blackboard. The loaded-callback defines the initialization procedure when
;;;; the component loads.
;;;; 

(defclass Meta-AQUA-lisp-app ( query-lisp-app )
     ()
  (:default-initargs :component-name "Meta-AQUA"
    :description "Initial testing of Meta-AQUA with the lisp connector"
    :port-number 10090)
  )


(defmethod execute-callback progn ((app Meta-AQUA-lisp-app) results)
  "For the default method, we use Michael's example code."
  (format excl:*initial-terminal-io*
   "~%The Semantic Trace as seen by Meta-AQUA is :~%~s~%" 
   results)
  ;; Function process-results re-defined 
  ;; in trans2frames.lisp
  (process-results 
   app results
   )
  (in-package :meta-aqua)
  (meta-AQUA::Meta-AQUA t)
  (in-package :poirot-connector)
  (gen-hypo app)
  )

(defmethod loaded-callback ((app Meta-AQUA-lisp-app))
  (format excl:*initial-terminal-io* "~&Component named ~A loaded by poirot core.~%"
	  (component-name app))
  (init-Meta-AQUA)
  (meta-aqua::set-story-22)
  )

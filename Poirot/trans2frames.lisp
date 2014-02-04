(in-package :user)

(use-package :poirot-connector)

;;; 
;;; This file represents the code to translate POIROT SWS invocations into
;;; Meta-AQUA frame representations (see rep_poirot.lisp).
;;; 
;;; We need to create something similar to the hand-coded example in
;;; homework.lisp. 
;;; 


;;; 
;;; Function process-results implements the procedure that parses the
;;; blackboard triple contents into uri structures and then writes out the
;;; content organized by semantic web service invocations.
;;; 
(defun process-results (app		;The lisp component 
			results		;The query results
			&aux 
			temp		;A temporary local var
			bb		;The parsed blackboard
			)
  "Returns a processed list of results"
  (format t "Processing...~%")
  (dolist (result results)
    (format t "Result: ~A~%" result)
    ;; Query for all triples in the context
    ;; We're assuming here that the results are a list of semantic trace URIs
    ;; The semantic trace uri also serves as the context on the blackboard
    (format 
     t 
     "Query Result: ~A~%" 
     (convert-subscription-result-list 
      (coerce 
       (setf temp
	 (parse-trace 
	  (coerce (execute-query 
		   app
		   nil nil nil 
		   (first result))
		  'list)))
       'vector)
      3))
    (setf bb (append bb (convert2triples temp)))
    )
  (sleep 2)
  (mapcar #'(lambda (x) 
	      (show-SWS-call 
	       x bb t))
	  (show-SWSs bb))
  (format t "Finished Input Processing~%")
  bb
  )


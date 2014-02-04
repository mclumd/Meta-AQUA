(use-package :poirot-connector)

(require :jlinker)

;; Main application loop
;;  We'll poll for results

(setf my-results nil)

(defun main-loop ()
  (format t "Lisp Hypothesizer~%")
  
  (let ((num-processed 0)
	(cur-results nil))
    (while (= num-processed 0)
	   (setf cur-results (pc::get-new-subscription-results))
	   (setf my-results cur-results)
	   (if (not (null cur-results))
	       (progn 
		 (process-results cur-results)
		 (setf pc::*execute-finished* t)
		 (incf num-processed))
	     (progn
	       (format t "Waiting for data~%")
	       (sleep 5)))))
  
  
  (format t "Finished~%"))

(defun process-results (results)
    
  (format t "Processing...~%")
  
  (dolist (result results)
    (format t "Result: ~A~%" result)
    
    ;; Query for all triples in the context
    ;; We're assuming here that the results are a list of semantic trace URIs
    ;; The semantic trace uri also serves as the context on the blackboard
    
    (let ((flat-list (pc::execute-query nil nil nil (first result)))
	  (result-list nil))
      (setf result-list (pc::convert-subscription-result-list flat-list 3))
      
      (format t "Query Result: ~A~%" result-list)))
  (sleep 5)      
  
  (format t "Finished Input Processing~%")
  )

(defun gen-hypo ()
  ;; Construct a hypothesis
  (let* ((hyp1-triple 
	  (pc::create-instance-triple "hypothesis" "hypothesis-lisp-1"))
	 (hyp1 (first hyp1-triple))
	 (thesis1-triple (pc::create-instance-triple "thesis" "thesis-lisp-1"))
	 (thesis1 (first thesis1-triple))
	 (thesis2-triple (pc::create-instance-triple "thesis" "thesis-lisp-2"))
	 (thesis2 (first thesis2-triple))
	 (hypothesis-triples 
	  (list hyp1-triple thesis1-triple thesis2-triple)))
    
    (setf hypothesis-triples 
      (append hypothesis-triples 
	      (list (pc::add-property-triple hyp1 "hasThesis" thesis1)
		    (pc::add-property-triple hyp1 "hasThesis" thesis2))))
    
    (format t "Publishing ~A~%" hypothesis-triples)
    (pc::publish-addlist hypothesis-triples nil)
    )
  (format t "Finished Hypothesis Processing~%")
  )
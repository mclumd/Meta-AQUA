(in-package :user)

(use-package :poirot-connector)

;;;; 
;;;; This file will house the code that formulates learning hypotheses for
;;;; Meta-AQUA.
;;;; 


;;; Dummy from Brett's code as modified by rpg.
(defun gen-hypo-old (app)
  ;; Construct a hypothesis
  (let* ((hyp1-triple 
	  (create-instance-triple app "hypothesis" "hypothesis-lisp-1"))
	 (hyp1 (triple-subject hyp1-triple))
	 (thesis1-triple (create-instance-triple app "thesis" "thesis-lisp-1"))
	 (thesis1 (triple-subject thesis1-triple))
	 (thesis2-triple (create-instance-triple app "thesis" "thesis-lisp-2"))
	 (thesis2 (triple-subject thesis2-triple))
	 (hypothesis-triples 
	  (list hyp1-triple thesis1-triple thesis2-triple)))
    
    (setf hypothesis-triples 
      (append hypothesis-triples 
	      (list (create-property-triple app hyp1 "hasThesis" thesis1)
		    (create-property-triple app hyp1 "hasThesis" thesis2))))
    
    (format t "Publishing ~A~%" hypothesis-triples)
    (publish-addlist app hypothesis-triples nil)
    )
  (format t "Finished Hypothesis Processing~%")
  )

(defun gen-hypo (app)
  ;; Post to blackboard a series of XP paraphrases.
  (dolist (each-XP (extract-XPs))
    (create-hypo each-XP app))
  (format t "Finished Hypothesis Processing~%")
  )


(defun create-hypo (XP app)
  ;; Create a single hypothesis
  (let* ((hyp1-triple 
	  (create-instance-triple app "XP" (string XP)))
	 (hyp1 (triple-subject hyp1-triple))
	 (thesis1-triple 
	  (create-instance-triple 
	   app 
	   "TextString"
	   (get-XP-paraphrase XP)))
	 (thesis1 (triple-subject thesis1-triple))
	 (hypothesis-triples 
	  (list hyp1-triple thesis1-triple)))
    (setf hypothesis-triples 
      (append hypothesis-triples 
	      (list (create-property-triple app hyp1 "Paraphrase" thesis1))))
    (format t "~%Publishing ~A~%" hypothesis-triples)
    (publish-addlist app hypothesis-triples nil)
    )
  )



(defun get-XP-paraphrase (xp)
  (meta-aqua::do-say 
      (meta-aqua::frame-type xp))
  )

(defun extract-XPs ()
  "Return a list of XPs from the reasoning model constructed by Meta-AQUA"
  (mapcar #'(lambda (each-trace)
	      (frames::f.get 
	       each-trace 
	       'meta-aqua::main-xp))
	  (meta-aqua::get-model meta-aqua::*reasoning-model*))
  )



(in-package :metaaqua) ;Was :user
(require :sock)
(use-package :socket)

(eval-when (eval compile load)
  (load (concatenate 'string 
	  user::*Meta-AQUA-system-dir* "MCL-interface"))
  (load (concatenate 'string 
	  user::*Meta-AQUA-system-dir* "MIDCA-debug"))
  ;; No longer needed since wrote find-xp function.
  ;;(load (concatenate 'string 
  ;;        user::*Meta-AQUA-system-dir* "Old/MIDCA-patches"))
  )

;;;; 
;;;; This code implements the K-track interpretation (i.e.,
;;;; explanation) process at the cognitive level for MIDCA. It uses
;;;; sockets to communicate with the python code of MIDCA. The input
;;;; and output sockets are passive.
;;;; 


(defvar *passive-socket* nil)
(defvar *passive-socket2* nil)

(defvar *MIDCA-input* nil)		;Read story from this passive socket
(defvar *MIDCA-output* nil)		;Write Xps to this passive socket

(defparameter *MIDCA-port* 5151
  "Port through which Meta-AQUA and MIDCA (simulator) communicate."
  )

(defparameter *Done-Token* 'Done
  "When detected, shutdown Meta-AQUA")


;;; 
;;; Establish two passive sockets for I/O. This will wait until MIDCA
;;; connects before proceeding.
;;; 
(defun setup-sockets ()
  (setf *MIDCA-input* 
    (accept-connection 
     (setf *passive-socket* 
       (make-socket :connect :passive 
		    :local-port *MIDCA-port* ;5150
		    ))))
  (setf *MIDCA-output* 
    (accept-connection 
     (setf *passive-socket2* 
       (make-socket :connect :passive 
		    :local-port user::*MCL-port* ;5155
		    ))))
  )

;;; 
;;; Function filter-states also removes the numbers after the "." in
;;; names of actions. The MIDCA-input parameter is assumed to be a
;;; list of pairs. Pairs are of the form (concept-repr string).
;;;
(defun filter-states (MIDCA-input 
		      &optional 
		      (first-pair 
		       (first MIDCA-input)))
  (cond ((null MIDCA-input) nil)
	((and (consp (first first-pair))
	      (equal (first (first first-pair))
		     'state))
	 (filter-states (rest MIDCA-input)))
	(t (cons 
	    (cons 
	     (cons (strip-after-dot (first ;e.g., unstack.1
				     (first first-pair) ;The action structure
				     ))
		   (rest (first first-pair)) ;A list with the string
		   )
	     (rest first-pair))
	    (filter-states (rest MIDCA-input)))))
  )

;;; Should probably use string search instead.
(defun ends-with-dot-something-p (symbol)
  (if (member  #\.
	       (coerce
		(string symbol)
		'list))
      t)
  )

;;; 
;;; Function up2item returns the part of alist up to but not including
;;; item.
;;; 
(defun up2item (alist item)
  (cond ((equal (first alist) item)
	 nil)
	(t (cons (first alist)
		 (up2item (rest alist) item)))))

;;;
;;; ||||| Note that this currently strips the last two characters from
;;; the symbol. Need to add search for the period and delete from
;;; there to the end.
;;; 
(defun strip-after-dot (symbol)
  (if (ends-with-dot-something-p symbol)
      (intern
       (coerce
	(up2item 
	 (coerce
	  (string symbol)
	  'list)
	 #\.)
	'string))
    symbol)
  )

;;; 
;;; Assumes that the input from MIDCA is in form suitable for
;;; Meta-AQUA or the Done token. If not Done, outputs the transformed
;;; input sent to init-story.
;;; 
(defun read-story-from-MIDCA (MIDCA-stream 
			      &optional 
			      (input-item 
				;;Should I also worry about eof?
			       (read MIDCA-stream)
			       ))
  (if *Debug-On*
	  (format t 
		  "~%Meta-AQUA reads from MIDCA: ~s~%"
		  input-item))
  (when (not (equal input-item *Done-Token*))
    (setf input-item
      (filter-states
       (second input-item)))
    (meta-aqua::init-story input-item))
  input-item
  )


(defun init-MIDCA-K-track-interpreter ()
  (if (y-or-n-p "Set verbose output to true? ")
      (setf *Debug-On* t))
  (user::init-Meta-AQUA)
  (setup-sockets)
  )

(defun shutdown-sockets ()
  (shutdown *MIDCA-output* :direction :output)
  (shutdown *MIDCA-output* :direction :input)
  (shutdown *MIDCA-input* :direction :output)
  (shutdown *MIDCA-input* :direction :input)
  (close *MIDCA-output*)
  (close *MIDCA-input*)
  )

(defun containsXP-p (TMXP)
  (equal (f.chase-path TMXP 
		       'generation 
		       'strategy-choice)
	 'explanation.0)
  )

(defun fetch-XP (TMXP)
  (first 
   (f.chase-path TMXP 
		 'generation 
		 'main-result 
		 'members))
  )

(defun find-XP (&optional 
		(candidates 
		 (f.get *Reasoning-Model* 'frame-list)))
  (cond ((null candidates)
	 nil)
	(t 
	 (if (containsXP-p (first candidates))
	     (fetch-XP (first candidates))
	   (find-XP (rest candidates))))
	)
  )

;;; 
;;; Read input, explain, and output until signal that MIDCA is
;;; done. Then shutdown sockets.
;;; 
(defun MIDCA-K-track-interpreter (&aux
				  XP) 
  ;;(init-MIDCA-K-track-interpreter)
  (loop 
    (when (equal (read-story-from-MIDCA *MIDCA-input*)
		 *Done-Token*)
      (shutdown-sockets)
      (return))
    (meta-aqua::meta-aqua t)
    (setf XP (find-XP))
    (if *Debug-On*
	(format t 
		"~%Meta-AQUA writes to MIDCA: ~s~%"
		(or (*FRAME* XP) 'None)))
    (cond (XP 
	   ;(break)
	   (format *MIDCA-output* 
		   "~S ~%"
		   (*FRAME* XP))
	   (f.traverse-frame
	    (let ((copied-frame (f.copy-instantiated-frame XP)))
	      (f.remove-filler! copied-frame 'mxp)
	      copied-frame)
	    #'(lambda (each-node parent role facet-name level target)
		(when (not (equal role 'mxp))
		  (if (isa-p 'state each-node)
		      (format *MIDCA-output* "~%~%~%For the state frame ~s:"
			      each-node)
		    (if (isa-p 'process each-node)
			(format *MIDCA-output* "~%~%~%For the process frame ~s:"
				each-node)
		      (if (isa-p 'physical-object each-node)
			  (format *MIDCA-output* "~%~%~%For the physical-object frame ~s:"
				  each-node)
			(if (isa-p 'abstract-object each-node)
			    (format *MIDCA-output* "~%~%~%For the abstract-object frame ~s:"
				    each-node)
			  (format *MIDCA-output* "~%~%~%For the frame ~s:"
				  each-node)
			  ))))
		  (if (null parent)
		      (format *MIDCA-output* "~%")
		    (format *MIDCA-output* "~% (Which is in the ~s slot of frame ~s)~%"
			    role
			    parent))
		  (if (literal-p each-node)
		      (format *MIDCA-output* "~%The value of the literal is ~s."
			      (*FRAME* each-node))
		    (dolist (each-role (f.role-list each-node))
		      (if (not (equal each-role 'mxp))
			  (dolist (each-facet-name (f.facet-list each-node each-role))
			    (format *MIDCA-output* "~%The ~s facet of the ~s role is ~s."
				    each-facet-name
				    each-role
				    (f.get each-node each-role each-facet-name))))))))
	    nil)
	   )
	  (t
	   (format *MIDCA-output* 
		   "None~%")))
    (finish-output *MIDCA-output*)
    )
  )


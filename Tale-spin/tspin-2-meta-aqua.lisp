;;; -*- Mode: LISP; Syntax: Common-lisp; Package: TALE-SPIN; Base: 10 -*-

(in-package :tspin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	    Elvis World and the Tale-Spin Story Generation Subsystem
;;;;				 for Meta-AQUA
;;;;
;;;;	     Copyright (C) 1996   Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;			 File: tspin-2-meta-aqua.lisp
;;;;
;;;;
;;;;	      *******************************************************
;;;
;;; This  program is  free  software; you can redistribute it and/or  modify it
;;; under the terms  of the GNU General Public License as published by the Free
;;; Software  Foundation;  either  version  1,  or  (at  your option) any later
;;; version.
;;; 
;;; This program is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without  even the  implied  warranty  of  MERCHANTABILITY  or
;;; FITNESS FOR  A PARTICULAR PURPOSE.  See the GNU General Public  License for
;;; more details.
;;; 
;;; You should  have received a copy of  the  GNU  General Public License along
;;; with this program; if not, write to the Free Software Foundation, Inc., 675
;;; Mass Ave, Cambridge, MA 02139, USA.  In emacs type C-h C-w to view license.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







;;;****************************************************************************
;;;
;;;			   TSPIN-2-META-AQUA
;;;
;;;****************************************************************************


;;; |||||| Note that my calls of f.unify have arguments that often MUST be kept
;;; in the same order so that the tokens of Tspin are maintained. [cox 24aug93]



;;;
;;; [cox 16aug11sep93]
;;; Function convert-2-frame is the main function to convert from the cd
;;; representation used by Tale-Spin to the frame representation used in
;;; Meta-AQUA. Function convert-2-frame takes as input a cd structure.  It
;;; checks to see if the type of the cd is equal to some legitimate conceptual
;;; type in Meta-AQUA. If so it instantiates a new frame of that type by
;;; dispatching the cd to an appropriate convert subfunction. It then converts
;;; the mode slot of the cd to an equivalent truth slot for the frame.
;;; Finally, it imports the new frame symbol to Meta-AQUA's package. The frame
;;; is also returned as a result of the function.
;;; 
;;; 
;;; [cox 25aug93] 
;;; The program still does not handle a ?unspec filler as in the following:
;;;
;;;   Officer1 asked Elvis whether Elvis would give officer1 the ganja1.  
;;;  #{MTRANS ACTOR=OFFICER1 
;;;           OBJECT=#{QUERY OBJECT=MODE 
;;;                          CON=#{ATRANS ACTOR=ELVIS 
;;;                                       OBJECT=GANJA1 
;;;                                       TO=OFFICER1 
;;;                                       FROM=?UNSPEC}} 
;;;                          TO=ELVIS 
;;;                          FROM=OFFICER1}
;;;
(defun convert-2-frame (cd)
  "Convert a concept represented in cd to a frame representation."
  (do-break convert-2-frame)
  (let*
    ((cd-type
       (if (old-cd-p cd)
	   (old-cd-head cd)))
     (new-frame
       (cond
	 ((null cd)
;;; 	   (format *tspin-stream* "~%Tried to convert nil cd.")
	  nil)
	 ((pp-p cd)				; Is picture producer (pp) literal.
	  (convert-pp cd))
	 ((member cd-type
		   (append			; These are the unary states.
		     *property-states*
		     *problem-states*)) 
	   (convert-unary-state cd cd-type))
	  ((member cd-type
		   '(filled attached))
	   (convert-bi-state cd cd-type))
	  ((member cd-type
		   '(like fear trust))
	   (convert-emotive-state cd cd-type))
	  ((f.type-p cd-type)			; This cd matches a predefined frame type.
	   (convert-defined-frame cd cd-type))
	  (t
	   (case cd-type
	     ((achieve wants)
	      (convert-goal cd))
	     (query				; It is a question.
	       (convert-query cd))
	     (loc
	       (convert-loc cd))
	     (cont
	       (convert-cont cd))
	     (t
	       (break)
	       (issue-complaint cd)))
	   )
	   ;;		  (unify-bindings
	   ;;		   new-frame
	   ;;		   (extract-bindings
	   ;;		    *roles* (make-filler-list cd)))
	  )))
    (when (and					; If a new frame was instantiated,
	    (not (null new-frame))
	    (frame-var-p new-frame))
      (add-truth-slot new-frame cd)		; convert mode to truth value.
      (import new-frame *target-package*)	; and import symbol into Meta-AQUA.
      new-frame))
  )


;;;
;;; Function add-truth-slot converts the mode slot of a given cd into a truth
;;; slot and inserts the slot into the given frame.  Because f.put is used
;;; instead of the destructive f.put!, if the frame already has a truth slot,
;;; no change will be made.
;;; 
(defun add-truth-slot (frame cd)
  (let ((truth-value (convert-mode cd)))
    (if truth-value
	(f.put frame
	       *truth-slot*
	       truth-value)))
  )


;;;
;;; Predicate pp-p returns true if cd is a pp or picture producer in conceptual
;;; dependency terminology. Pps are literal objects in the world like a person 
;;; or a rock. 
;;; 
;;; [cox 18aug93]
;;;
(defun pp-p (cd)
  "Is cd a picture-producer?"
  (not (old-cd-p cd))
  )


  
(defun convert-pp (cd)
  "Convert picture-producer cd to a frame."
  (cond ((boundp cd)
	 ;; The tokens used in the story are instantiated earlier
	 ;; and bound to the value of the symbols.
	 (symbol-value cd))
	;;|||||| Actually if this happens then there is 
	;;something wrong & should flag error. [cox 24aug93]
	(t 
	 (if (eq cd 'past)
	     'past.0
	   (format *tspin-stream*
		   "~%ERROR: unbound pp - ~s~%" cd))))
  )



(defun convert-unary-state (cd cd-type)
  "Convert unary-state cd of type cd-type to a frame."
  (let ((new-frame (f.instantiate-frame
		    (if (eq cd-type 'on)
			turned-on
		      (symbol-value cd-type))
		    *story-instance*)))
    (f.unify (f.get 
	      new-frame
	      *domain-slot*)
	     (convert-2-frame 
	      (role->filler 
	       'actor cd)))
    (f.unify (case (convert-mode cd)
	       (in.0 'true.0)
	       (out.0 'false.0))
	     (f.get new-frame 
		    *co-domain-slot*))
    new-frame)
  )


(defun convert-bi-state (cd cd-type)
  "Convert bi-state cd of type cd-type to a frame."
  (let ((new-frame (f.instantiate-frame
		    (symbol-value cd-type)
		    *story-instance*)))
    (f.unify (f.get 
	      new-frame
	      *domain-slot*)
	     (convert-2-frame 
	      (role->filler 
	       'actor cd)))
    (f.unify (f.get 
	      new-frame
	      *co-domain-slot*)
	     (convert-2-frame 
	      (role->filler 
	       'val cd)))
    new-frame)
  )

(defun convert-emotive-state (cd cd-type)
  "Convert emotional-state cd of type cd-type to a frame."
  (let ((new-frame (f.instantiate-frame
		    (case cd-type
		      (like likes)
		      (fear fears)
		      (trust trusts))
		    *story-instance*)))
    (f.unify (f.get 
	      new-frame
	      *domain-slot*)
	     (convert-2-frame 
	      (role->filler 
	       'actor cd)))
    (f.unify (f.get 
	      new-frame
	      *co-domain-slot*)
	     (convert-2-frame 
	      (role->filler 
	       'to cd)))
    new-frame)
  )



(defun convert-defined-frame (cd cd-type)
  "Convert cd corresponding to a predefined frame action to a frame."
  (do-break convert-defined-frame)
  (let ((new-frame (f.instantiate-frame (symbol-value cd-type)
					*story-instance*)))
    (dolist (each-role *roles*)
      ;; |||||| Verify that each slot is legitimate?
;;;       (if (not (eq 'mode each-role))
      (let* ((new-filler 
	       (if (eq each-role 'mode)
		   (convert-mode cd)
		   (convert-2-frame
		     (role->filler each-role cd))))
	     (frame-filler 
	       (if new-filler
		   (f.get new-frame each-role)))
	     )
	(when new-filler
	  (case each-role			; [cox 19aug93]
	    ((from to)
	     (setf new-filler 
		   (case cd-type 
		     (atrans
		       (map-2-controls new-filler new-frame))
		     (tilt
		       (map-2-inside new-filler new-frame))
		     (otherwise
		       (map-2-at-location new-filler new-frame))
		     ))))
	  (if frame-filler
	      (multiple-value-bind
		(merged-node failures error-path)
		  (f.unify
		    frame-filler
		    new-filler
		    nil)			; Do not notify and prompt for break if failed unification.
		(when failures
		  (format
		    *tspin-stream*
		    "~%Creating disparity between nodes ~s~%along path ~s in frame ~s.~%~%"
		    failures
		    error-path
		    new-frame)
		  (f.put-all! new-filler new-frame each-role)))
	      (f.put new-filler new-frame each-role))
	  )))
;;;       )
    new-frame)
  )


;;; 
;;; Function convert-query returns a frame represenation of the query cd input.
;;; First the con slot of the query is converted to a frame, then the resulting
;;; frame is marked as a question by giving it a status slot having a question
;;; marker as filler.
;;; 
(defun convert-query (cd)
  "Convert query cd to a frame."
  (let ((new-frame 
	 (convert-2-frame (old-cd-con cd))))
    (f.put *question*
	   new-frame
	   *status-slot*)
    new-frame)
  )


;;;
;;; Function convert-goal produces a frame representation of both achieve and
;;; wants cds. There seems to be no real difference between the two in
;;; Tale-Spin.
;;; 
(defun convert-goal (cd)
  "Convert goal cd to a frame."
  (let ((new-frame (f.instantiate-frame
		     goal
		     *story-instance*)))
    (f.unify (f.get new-frame 'goal-actor)
	     (old-cd-actor cd))
    (f.unify (f.get new-frame 'goal-object)
	     (old-cd-object cd))
    new-frame)
  )



(defun convert-loc (cd)
  "Convert a location cd to a at-location frame."
  (let ((new-frame (f.instantiate-frame at-location
					*story-instance*)))
    
    (f.unify (f.get new-frame *co-domain-slot*)
	     (f.instantiate-frame 
	       `(near
		  (,*domain-slot* 
		   (,*value-facet*
		    ,(f.unify (f.get new-frame *domain-slot*)
			      (convert-2-frame (old-cd-actor cd)))))
		  (,*co-domain-slot* 
		   (,*value-facet*
		    ,(convert-2-frame (old-cd-val cd)))))
	       *story-instance*))
    new-frame)
  )


;;; 
;;; CD cont representations are somewhat backward in slot conventions since the
;;; actor slot is the thing controlled, whereas, the val slot it the
;;; controlling agent.
;;; 
(defun convert-cont (cd)
  "Convert a control cd to a possess frame, except for control of a person."
  (let ((new-frame 
	  (f.instantiate-frame
	    (if (is-a (old-cd-actor cd) 'person)
		control-state
		possess)
	    *story-instance*)))
    (cond ((is-a (old-cd-actor cd) 'person)
	   (f.put (convert-2-frame (old-cd-val cd))
		  new-frame
		  *domain-slot*)
	   (f.put (convert-2-frame (old-cd-actor cd))
		  new-frame
		  *co-domain-slot*))
	  (t
	   (f.unify (f.get new-frame *domain-slot*)
		    (convert-2-frame (old-cd-val cd)))
	   (f.unify (f.get new-frame *co-domain-slot*)
		    (convert-2-frame (old-cd-actor cd)))))
    new-frame)
  )


;;;
;;; Note that this is a slight misnomer as compared to the other convert-*
;;; functions.
;;; 
(defun convert-mode (cd)
  "Return the frame representation of the mode filler of cd."
  (if (old-cd-p cd)
      (let ((mode (old-cd-mode cd)))
	(if (member 'pos mode)
	    *in*
	    (if (member 'neg mode)
		*out*))))
  )



(defun map-2-at-location (filler frame)
  "Map a from or to filler to frame represenation."
  (if (not (isa-p 'at-location (list frame)))
      (f.instantiate-frame 
	`(at-location
	   (,*domain-slot* 
	    (,*value-facet*
	     ,(f.get frame 'object)))
	   (,*co-domain-slot*
	    (,*value-facet*
	     ,(if (isa-p 'physical-location
			 (list filler))
		  filler
		  `(near
		    (,*domain-slot* 
		     (,*value-facet* ,(f.get frame 'object)))
		    (,*co-domain-slot* (,*value-facet* ,filler)))))))
	*story-instance*))
  )


;;;
;;; |||||| I guess we should be checking what is in the various slots to
;;; determine (if possible) whether the control-state would be a controls, owns
;;; or possess frame. [cox 23oct94]
;;;
;;; |||||| Inplicit in all the mapping routine that use calls of f.get on the
;;; frame parameter is that the slot has been defined in an appropriate order
;;; so that the filler will be present already. [23oct94]
;;; 
(defun map-2-controls (filler frame)
  (if (not (isa-p 'controls (list frame)))
      (f.instantiate-frame 
       `(control-state
	 (,*domain-slot* 
	  (,*value-facet* ,filler))		; Although not checked for, filler should be a person.
	 (,*co-domain-slot*
	  (,*value-facet* ,(f.get frame 'object))))
       *story-instance*))
  )


(defun map-2-inside (filler frame)
  (if (not (isa-p 'inside (list frame)))
      (f.instantiate-frame 
	`(at-location
	   (,*domain-slot* 
	    (,*value-facet*
	     ,(f.get frame 'object)))
	   (,*co-domain-slot*
	    (,*value-facet*
	     (inside
	       (,*domain-slot*
		(,*value-facet*
		 ,(f.get frame 'object)))
	       (,*co-domain-slot*
		(,*value-facet*
		 ,filler))))))
	*story-instance*))
  )



(defun issue-complaint (cd)
  "Error message when cd in unknown category."
  (format *tspin-stream*
	  "~%Cannot handle conversion of ~s yet.~%Spoken: " cd)
  (say cd nil)
  (terpri *tspin-stream*)
  (when (old-cd-con cd)
	(format *tspin-stream*
		"Con spoken as ")
	(say (old-cd-con cd) nil)
	(terpri *tspin-stream*))
  (terpri *tspin-stream*)
  )





;;;
;;; [cox 18aug93]
;;;
(defun map-literal (cd)
  (let ((cd-type (get cd 'is-a))
	(new-frame nil))
    (case cd-type
      (person 
	(setf 
	  new-frame 
	  (f.instantiate-frame 
	    person
	    *story-instance*))
	(f.put! (f.instantiate-literal 
		  (string cd)
		  *story-instance*)
		new-frame 
		'name))
      (loc
	(setf new-frame
	      (f.instantiate-frame 
		at-location))
	(f.put-all!
	  (f.instantiate-frame 
	    (symbol-value cd))
	  new-frame
	  *co-domain-slot*)
	(when (eq cd 'outside)
	  (f.unify (f.get new-frame *domain-slot*)
		   (f.chase-path new-frame 
				 *co-domain-slot* 
				 *domain-slot*))))
      (t (setf new-frame 
	       (f.instantiate-frame dummy))))
    new-frame)
  )

	   
;;; [cox 19aug93]
(defun make-filler-list (cd &optional (roles *roles*))
  (if (not (null roles))
      (cons (role->filler (first roles) cd)
	    (make-filler-list cd (rest roles))))
    )



;;; [cox 19aug93]
(defun extract-bindings (role-list filler-list)
  (if (not (null filler-list))
      (let ((match (and (first filler-list) ;Do not match with a nil filler.
			(member (first filler-list)
				(rest filler-list)))))
	(if match
	    (cons (list (first role-list)
			(nth (- (length filler-list)
				(length match))
			     role-list))
		  (extract-bindings (rest role-list)
				   (rest filler-list)))
	  (extract-bindings (rest role-list)
			   (rest filler-list)))))
  )
	

;;; [cox 19aug93]
(defun unify-bindings (frame binding-list)
  (cond ((null frame)
	 nil)
	((null binding-list)
	 frame)
	(t
	 (f.unify 
	  (f.get frame (first (first binding-list)))
	  (f.get frame (second (first binding-list))))
	 (unify-bindings frame (rest binding-list))))
  )




(defun convert-interval (interval)
  "Convert one time interval in a story."
  (cond ((null interval)
	 nil)
	(t
	 (let ((say-string
		 (remove
		   #\Newline
		   (with-output-to-string
;;; 		     (say-stream) Changed to the stream below 9nov93
		     (*tspin-stream*)
		     (say (first interval)
			  nil
			  t)))))
	   (if (blank-string-p say-string)
	       (break "~%Current cd ~s~%"
		      (first interval)))
	   (cons (list
		   (convert-2-frame
		     (first interval))
		   say-string)
		 (convert-interval
		   (rest interval))))))
  )



(defun blank-string-p (string)
  "Return t if string has only spaces or tabs."
  (eq 0
      (length
	(string-trim
	  '(#\Space #\Tab)
	  string)))
  )


;;;
;;; Function convert-story is the routine that translates an entire story from
;;; cd representation to frame representation. A story consists of a series of
;;; time intervals within which all events are assumed to occur simultaneously.
;;; A story, then, is a list of time interval sublists containing one or more
;;; cd representations. The story list is in reverse order. Convert-story1 calls
;;; convert-interval on each sublist and appends the results in correct order
;;; as its return value.
;;;
;;; Note that the interval boundaries are destroyed by removing the extra level
;;; of lists between intervals. If the append was a cons this would be
;;; retained. Could make an optional parameter flag for this.
;;; 
(defun convert-story (&optional (story *ALL*))
  "Convert a list of story interval sublists to frame representation."
  (format
    *tspin-stream*
    "~%Converting Tale-Spin cds into Meta-AQUA frames.~%")
  (convert-story1 story)
  )


;;;
;;; Function convert-story1 is the recursive helper function for function
;;; convert-story.
;;; 
(defun convert-story1 (story)
  (if (not (null story))
      (append
	   (convert-story1 (rest story))
	   (convert-interval (first story))
	   ))
  )



;;;
;;; Some utilities.
;;;



(defun find-predicate-match (alist)
  (mapcan #'satisfies-predicate alist)
  )

(defun satisfies-predicate (cd)
  (let ((found nil)
	(filler nil))
    (dolist (each-fn *role-functions*)
	    (setf filler (funcall each-fn cd))
	    (if (old-cd-p filler)
		(if (satisfies-predicate filler)
		    (setf found t))
	      (if (search-predicate-p filler)
		  (setf found t))))
    (if found (list cd))))

;then call (mapcan #'find-predicate-match *ALL*)


(defun search-predicate-p (filler)
;  (equal '?unspec filler)
  (equal 'ELVIS filler)
;  (and (old-cd-p filler)
;       (eq (old-cd-head filler)
;	   'K-9-SQUAD-ARRIVE))
  )

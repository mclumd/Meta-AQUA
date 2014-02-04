;;; -*- Mode: LISP; Syntax: Common-lisp; Package: Meta-aqua; Base: 10 -*-

(in-package :metaaqua)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	    The Meta-AQUA Introspective Multistrategy Learning System
;;;;				   Version 6
;;;;
;;;;	     Copyright (C) 1996   Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;			     File: questions.lisp
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







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;           QUESTION HANDLING
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; A question is represented as any frame with a status slot having the value
;;; equal to the value of the global constant *question*.  Questions will have
;;; an explanations slot that specifies the answer to the question and a truth
;;; slot that specifies its expected truth value. Thus we can specify
;;; questions such as: Why did the dog bark at the luggage? Did the dog bark
;;; at the luggage? Did the dog not bark at the luggage? Did the dog bark at
;;; the luggage because it was threatened? This explanations slot is a list
;;; that may have one or more alternative hypotheses. Thus this level is an OR
;;; level in the AND/OR HVQ tree such that hypo-1 or hypo-2 or ... or hypo-n
;;; may be true. Each of these hypotheses may have additional associated
;;; questions, all of which if answered positively will result in the
;;; acceptance of the hypothesis. Thus this level is AND level since all must
;;; be true. The questions are HVQs, and are to be founf on the HVQ slot of
;;; the hypothesis.
;;;



;;;
;;; Predicate question-p returns true if the argument frame isa a question as
;;; determined by the frame's status slot. Questions not only have the value
;;; *question* on the status slot, but they usually possess an 'indices slot
;;; also. See below.
;;; 
(defun question-p (frame)
  (equal (f.get frame *status-slot*)
	 *question*)
  )


;;;
;;; Function why-question-p returns t if the frame is asking a "why question,"
;;; nil otherwise.
;;; 
(defun why-question-p (frame)
  (question-p
    (first
      (f.get frame *explanations-slot*)))
  )


;;;
;;; Function why-did-actor-do-act-p returns t if frame is a why question and
;;; is an actor relation, nil otherwise.
;;; 
(defun why-did-actor-do-act-p (frame)
  (and (why-question-p frame)
       (isa-p 'actor (list frame)))
  )


;;;
;;; Function reasserts-act-being-questioned-p returns t if the action frame is
;;; basically reasserting the action that the question ask about. Ther
;;; question is why did the actor of the action frame perform the action. The
;;; action frame is just the representation of the action. This function is
;;; used to avoid resuming the indexed question when the story repeats the
;;; action. For example, the question why did the authority perform the arrest
;;; will remain in memory unanswered. If later stories have the same authority
;;; performing similar arrests, we do not want these arrest scenes to be
;;; considered an answer to the earlier question. [cox 21jan95]
;;; 
(defun reasserts-act-being-questioned-p (action-frame question)
  (and (why-did-actor-do-act-p question)
       (can-unify-p (f.get action-frame *actor-slot*)
		    (f.get question *co-domain-slot*))
       (can-unify-p action-frame
		    (f.get question *domain-slot*)))
  )


;;;
;;; Function mark-as-question takes as input some frame instance and the
;;; instance's expected truth value (e.g., in.0 implies that the system
;;; expects the question to be answered in the affirmative), putting the
;;; expected truth on its truth slot and question on its status slot.  The
;;; instance is returned as the value of the function. See main comments above
;;; in this section.
;;; 
(defun mark-as-question (instance expected-truth)
  (do-break mark-as-question)
  (f.put *question* instance *status-slot*)
  (f.put expected-truth instance *truth-slot*)
  instance)


;;;
;;; Function get-old-goal returns the original goal that was being
;;; processed that led to the question being formed, indexed in memory,
;;; and now retrieved. To find the goal, try the first question
;;; (oracle), look on its explanations slot, take the first explanation
;;; (oracle, though there is only one explanation in the case of the
;;; drug bust), look on the mxp slot to find the trace of processing,
;;; then finally get the main goal of the tmxp.
;;; 
(defun get-old-goal (old-questions)
  (let ((old-goal
	  (f.chase-path
	    (first
	      (f.get
		(first old-questions)
		'explanations))
	    'mxp
	    'main-goal)))
;    (format
;      *aqua-window*
;      "~%Old goal passed to funct. answer-old-questions: ~s"
;      old-goal)
    old-goal)
  )




;;;
;;; Function extract-question returns either the given form, if it passes the
;;; question-p predicate, or a question that is posed on its explanations slot,
;;; if that slot passes the question-p predicate, nil otherwise.
;;;
(defun extract-question (form)
  (if (question-p form)
      form
      (let ((first-explanation
	      (first
		(f.get
		  form
		  *explanations-slot*))))
	(if (question-p
	      first-explanation)
	    first-explanation)))
  )



;;;
;;; Function find-unifyable-question searches the given question list for a
;;; frame that can unify with the answer. It returns the first one that
;;; successfully unifies, nil otherwise.
;;; 
(defun find-unifyable-question (answer questions lazy?)
  (some
    #'(lambda (each-question)
	(let ((question
		(extract-question
		  each-question)))
	  (if (can-unify-p answer question lazy?)
	      each-question)))
    questions)
  )



;;;
;;; Function try-matching2 acts as a predicate for direclty matching answers.
;;; For example, if the question is "Is the color of the ball blue?" the a
;;; literal answer that specifies the color as such will return true given the
;;; following tests: one of the questions matches the answer (passes
;;; can-unify-p), the answer is believed true, and the forms actually do unify.
;;; 
(defun try-matching2 (answer questions &optional (lazy? nil))
  (let* ((matching-question
	   (find-unifyable-question
	     answer
	     questions
	     lazy?))
	 (matching-answer
	   (extract-question matching-question)))
    (if matching-answer
	(do-break try-matching2))
    (cond ((and
	     matching-answer
	     (in-set-of-beliefs-p answer)
	     ;; ||||| NOTE that answer may become obsolete by this unification.
	     (f.unify answer
		      matching-answer
		      t nil			; Default arguments.
		      lazy? ))
;	   (with-character-style (*Style*)
	     (format
	       *aqua-window*
	       (str-concat
		 "~%Question ~s successfully "
		 "answered~%  with matching input.~%")
	       matching-question)
;	     )
	   
	   (set-successful-answers
		   *Current-Result-Record*)
	   (remove-indices-to-answered-question
	     matching-question)
	   t
;	   (cond (lazy?
;;;; 		  (add-break resume-examination-phase)
;		  t)
;		 ((not
;		    (remaining-learning-goals-p
;		      *Goal-Queue*))
;		  (new-queue '*Goal-Queue*)t)	;|||||| This is a hack because we know to quit.
;		 )
	   )))
  )



(defun remaining-learning-goals-p (queue)
  (some
    #'(lambda (each-goal)
	(if (learning-goal-p each-goal)
	    t))
    (list-queue queue))
  )


(defun learning-goal-p (goal)
  "Returns t, if goal is a learning goal, nil otherwise."
  (eq (frame-type
	(goal-object goal))
      'review/learn)
  )



;;;
;;; Function question-type returns the kind of question the input form
;;; represents or has attached to it. It will return either 'what-question,
;;; 'when-question, 'where-question, 'why-question, 'how-question,
;;; 'who-question, 'is-question, 'did-question or nil if no question can be
;;; discovered in the form.
;;;
;;; ||||| How questions are currently not represented. They represent a querry
;;; for additional details at a finer level of granularity.
;;;
(defun question-type (form &optional
		           (domain (if (relation-p form)
				       (f.get form *domain-slot*))))
;  (cond ((question-p form)
;	 (cond ((null domain)
;		(if (state-p form)
;		    'is-question
;		    (if (action-p form)
;			'did-question)))
;	       (case (return-slot-name form domain)
;		 ;; ||||||How to differentiate between who did x and why did actor do x?
;		 (actor))))
;	(t
;	 ;; otherwise look at each slot for the question.
;	 ))
  (if (question-p (f.get form *explanations-slot*))
      'why-question)
      )



;;; 
;;; As a temporary hack, we produce a simple answer by just unifying the answer
;;; (the new input) with the first question on the list.  Do omnicient
;;; unification. Eventually we want to do different things depending on the
;;; type of question or knowledge goal, ie. apply XP (derived or given) to
;;; why-questions, slot filling for what-questions, etc.
;;;
;;; Parameter answer is the new input and questions is a list with the old
;;; question retrieved from the input cues.
;;; 
;;; ||||| How to find the original Trace-Meta-XP where the trace of prior
;;; reasoning is? Now can look to mxp slot of old-answer. So we need to post a
;;; goal to verify (test) the hypothesis by comparison! Comparison will post a
;;; goal to review and the review process will find and apply the
;;; Introspective-Meta-XP.
;;;
;;; This function is still relying on oracle features like the knowledge that
;;; there exists two separate xps.
;;; 
(defun answer-old-questions (answer questions old-goal current-goal &optional automatic?)
  (let* ((question (if (atom questions)
		       questions
		       (if (listp (first questions))
			   (first (first questions))
			   (first questions))))
	 (old-answer (first (f.get question *explanations-slot*)))	; The actual question.
	 (trace (processing-trace old-goal))	; Reasoning trace that formed the question.
	 (phase (f.get trace 'current-phase)))	; Where the reasoning left-off.
    (assert (eq (f.get old-answer 'mxp) trace)
	    (old-answer trace)
	    "Error in answer-old-questions")
    (do-break answer-old-questions)
    (when
      (case (f.relation-slot-name phase)
	(identification				; This should never occur.
	  (format
	    *aqua-window*
	    (str-concat
	      "Error in answer-old-questions: "
	      "Should not be in identification phase."))
	  (break)
	  )
	(generation
	  (resume-generation-phase
	    answer questions old-goal current-goal
	    question old-answer phase automatic?))
	(examination
	  (resume-examination-phase
	    answer questions old-goal
	    question old-answer phase))
	;; Review will never be present since it is never suspended?
	(t (format
	     *aqua-window*
	     "~%ERROR in case statement of function answer-old-questions.~%")
	   (break)))
      (remove-achieved-goal current-goal)
      ;; ||||| Need to set the parts of the trace like x.strategy-decision does,
      ;; ie. set the basis for choosing this comparison. Also need to set the
      ;; strategy-execution slot of the d-c-node to compare. But what of the
      ;; strategy-decision slot. Leave it at suspend and we can recover the chain
      ;; of events?
    
      ;; Remove the knowledge goal which had been suspended.
      ;; |||||| But it is not present, no? When we suspend, we remove the whole goal hierarchy.
      (delete-item old-goal
		   *Goal-Queue*)
      t)					; If successful return t
    ;; 	Otherwise return nil.
    ))



;;;
;;; Function remove-indices-to-answered-question is called after a question is
;;; sucessfully answered in order to remove the question from memory. This is
;;; fairly straight-forward because a list of indexes were placed on the
;;; question when it was originally created. This list is simply traversed and
;;; each-index has nil plced on the memory at that location.
;;; 
(defun remove-indices-to-answered-question (question)
  (do-break remove-indices-to-answered-question)
  (if question
      (dolist (each-index (f.get question 'indices))
	(do-index nil
		  (f.get each-index 'type)
		  (f.get each-index 'relation))))
  )



;;; 
;;; For each xp-asserted node in the concept's explanation , check to
;;; see if it is in the set of beliefs. If not, ask a hypothesis
;;; verification question.
;;;
;;; ||||| NOTE that I have not checked to see if it is known whether the
;;; negation of at least one xp-asserted node is in the set of beliefs
;;; (or can be infered). If this were true then the explanation would be
;;; falsified.
;;; 

(defun gen-sub-questions (xp)
  (let ((premises (f.get xp *asserted-node-slot*))
	(hvqs nil))
    (dolist (each-node (remove-duplicates
			 premises))
      (cond ((not (or (in-set-of-beliefs-p each-node)
		      (exists-in-story each-node)))
	     (setf hvqs (cons (add-hvq each-node xp) hvqs))
	     (index-question each-node
			     (not (null (f.get each-node *explanations-slot*)))))))
    (cond ((null hvqs)
	   (f.put *in* xp *truth-slot*)
;	   (format
;	     *aqua-window*
;	     "~%Hypothesis accepted. All premises true.~%")
	   )
	  (t
	   ;; |||||
	   ;; Actually there is an issue here whether the truth should be marked only 
	   ;; as hypothesized instead of hypothesized-in if there already exists other
	   ;; hypotheses on the explanations slot of the question concept. Also if we should
	   ;; update the first hypothesis to just hypothesized once a second arrives.
	   (f.put *hypothesized-in* xp *truth-slot*)
	   (f.put hvqs xp *hvqs-slot*)
	   (format
	     *aqua-window*
	     "~%Generating additional questions from explanation:~%  ~s.~%"
	     hvqs)))
    ;; It is important that we return nil if (null xp). See function explain.
    xp)
    )



;;;
;;; Function add-hvq places the backlink to the hypothesis from the hvq,
;;; marks the hvq as a question, and returns the hvq as its value
;;; since mark-as-question does.
;;; 
(defun add-hvq (hvq hypothesis) 
  (f.put! hypothesis hvq *hvq-back-slot*)
  (mark-as-question hvq *hypothesized-in*)
  )


;;; 
;;; ||||| NOTE that these index property lists are never initialized so
;;; subsequent runs of the program will inherit the past values. May
;;; give side-effect.
;;; 
;;; Note that we are no longer using the old style assoc list-property
;;; scheme, but the above comment may still be relevant. However, when
;;; subsequent questions are indexed, the latest one is at the front of
;;; the list (ie. the car).
;;; 
;;; ||||| Should also index the question by the expected outcome or
;;; predictions so that the expected input will trigger the reminding.
;;; Thus, if we explain that a threatening situation caused the dog to
;;; bark, then if later input mentions a threatening situation, we want
;;; to have the reminding.
;;;
;;; |||||| The anomaly itself is also indexed as if it were a question
;;; (see function index-anomaly). Is there redundancy here? [28nov93]
;;; 
;;; 
;;; 
;;;
;;; An important side-effect of this function is that it increments the current
;;; total number of questions posed in the run. This update is performed on the
;;; global variable *Current-Result-Record*. Originally [30oct94] on function
;;; print-question-posed, but moved here because of gen-sub-questions also
;;; calls this function to pose questions. [1jul95]
;;; 
(defun index-question (question
		       &optional
		       is-repeat-question?	; If non-nil, question has been posed before.
		       )
  (if (not is-repeat-question?)
      (set-posed-questions
	*Current-Result-Record*))
  (cond ((isa-p 'relation (list question))
	 (do-index question 'question-type.0 question)
	 (back-index question question))
	(t
	 (if (or (f.get question *actor-slot*)
		 (let ((inherited-slot
			 (f.inherit question
				    *actor-slot*)))
		   (if inherited-slot
		       (f.put (f.instantiate-frame
				inherited-slot)
			      question
			      *actor-slot*))))
	     (let ((indexable-slot
		     (f.get-relation question *actor-slot*)))
	       (do-index question
			 'question-type.0
			 indexable-slot)
	       (back-index question indexable-slot)))))
  )


;;;
;;; Function back-index is used to provide a list of all the indices for
;;; a given question. Then when the question is answered, one can remove
;;; the indices for the question, so it will not be repeated.
;;; 
(defun back-index (question relation)
  (f.put! (cons
	    (f.instantiate-frame
	      `(index
		 (type (value question-type.0))
		 (domain (value ,relation))
		 (co-domain (value ,question))
		 (relation (value ,relation))
		 (memory-item (value ,question))
		 ))
	    (f.get question 'indices))
	  question
	  'indices)
  )




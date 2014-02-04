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
;;;;			       File: solver.lisp
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



;;;;
;;;; RIPSAU (Robust Integrated Problem-Solving and Understanding)
;;;;
;;;; VERSION 3 - Handles the Dog barks at an inanimate object anomaly.
;;;;              - Also Cops & Robbers as well as the LISP Programming
;;;               domain.
;;;;
;;;; RIPSAU3 is not just a shell around McAQUA. It also redefines two of the
;;;; functions in file mcaqua4.lisp. These function are make-new-instance
;;;; instantiate-next.
;;;  This has since been revised. Function make-new-instance has long been
;;;  redesigned and put in frame.lisp as f.make-new-instance. Instantiate-next
;;;  is now only defined in this file. See old-fragments.lisp for the old versions
;;;  of the code. The old aqua4 is no longer intact. The system has merged and there
;;;  is no current code to run the story that had viewing one concept as another, etc.
;;;  [17Dec91]
;;;;





;;;;
;;;; PROGRAM VARIABLES
;;;;



;;; 
;;; ||||| These really should not be dot-zero instances?
;;; These are usually attribute values. But we want them to be 
;;; constant so that when the program is run multiple times
;;; memories of a reasoner reasoning about a story refer to the
;;; same self.
;;;

(defconstant *criminal-actor* 'smuggler.0
  "The token for the terrorist smuggler.")

(defconstant *police-actor* 'authority.0
  "The token for the counter-terrorist authority.")




(defvar *current-character* nil
  "May be either the constants cop or robber.")



;;; 
;;; The reasoning model for the cop.
;;; See comment for *Reasoning-Model* in file constants.lisp.
;;; 
(defvar *Cop-Reasoning* nil)

;;; 
;;; The reasoning model for the robber.
;;; See comment for *Reasoning-Model* in file constants.lisp.
;;; 
(defvar *Robber-Reasoning* nil)




;;;
;;; The following queue is for police goals. Is copied into *Goal-Queue* when
;;; it is the officer's turn.
;;; 
(defvar *Cop-Queue* nil
  "Queue of goals for the airport authorities.") 


;;;
;;; The following queue is for crook goals. Is copied into *Goal-Queue* when
;;; it is the robber's turn.
;;; 
(defvar *Robber-Queue* nil
  "Queue of goals for the terrorist smuggler.")



;;; Example of an item that could be in this list is rain.
(defvar *World-Events* nil
  "List of events independent of the major characters.")



;;;;
;;;; SWITCHING BETWEEN COPS & ROBBERS
;;;;



(defun toggle-current-agent-window ()
  (current-window
    (if (equal *aqua-window* *window1*)
	*window2*
	*window1*))
  )



;;; 
;;; Function swap-perspective performs a kind of context switching
;;; between the cop and robber perspectives. If one of the queues is
;;; empty, then the swap is not  executed.
;;;
(defun swap-perspective (&optional over-ride)
  (cond ((equal *current-character* *criminal-actor*)
	 (set-model
	   *Robber-Reasoning*
	   (get-model *Reasoning-Model*))
	 (set-queue
	   *Robber-Queue*
	   (list-queue *Goal-Queue*))
	 (cond ((or over-ride
		    (list-queue *Cop-Queue*))
		(set-model
		  *Reasoning-Model*
		  (get-model *Cop-Reasoning*))
		(set-queue
		  *Goal-Queue*
		  (list-queue *Cop-Queue*))
		(setf *current-character*
		      *police-actor*)))
	 )
	((equal *current-character*
		*police-actor*)
	 (set-model
	   *Cop-Reasoning*
	   (get-model *Reasoning-Model*))
	 (set-queue
	   *Cop-Queue*
	   (list-queue *Goal-Queue*))
	 (cond ((or over-ride
		    (list-queue *Robber-Queue*))
		(set-model
		  *Reasoning-Model*
		  (get-model *Robber-Reasoning*))
		(set-queue
		  *Goal-Queue*
		  (list-queue *Robber-Queue*))
		(setf *current-character*
		      *criminal-actor*)))
	 )
	(t
	 (format
	   *aqua-window*
	   "ERROR: swap-perspective.")))
  (toggle-current-agent-window)
;;; (send *aqua-window*
;;;   :set-reverse-video-p t)  
  )


;;;;
;;;; INITIALIZATION  FUNCTIONS
;;;;


;;; 
;;; Initialize the authority's goals.
;;; 
(defun init-police ()
  (f.put! *nil*
	  (spawn-new-goal
	    *police-actor*
	    (make-goal-state
	      'wants
	      *police-actor*
	      (f.instantiate-frame
		`(controls
		   (,*domain-slot* (,*value-facet* ,*police-actor*))
		   (,*co-domain-slot* (,*value-facet* ,*criminal-actor*)))
		*predefined-instance*))
	    'achievement-goal.0
	    'six.0)
	  'mxp)
  )


;;; 
;;; Initialize the smuggler's goals.
;;; 
(defun init-smuggler ()
  (f.put! *nil*
	  (spawn-new-goal
	    *criminal-actor*
	    (make-goal-state
	      'wants
	      *criminal-actor*
	      (f.instantiate-frame
		`(at-location
		   (,*domain-slot* (,*value-facet* (explosives)))
		   (,*co-domain-slot* (,*value-facet* (nation))))
		*predefined-instance*))
	    'achievement-goal.0
	    'six.0)
	  'mxp)
  )


;;; Initialize the previous smuggling plan.
;;; Called by init-aqua.
;;;
;;; |||||Note that by calling f.make-new-instance the frame
;;; is NOT marked as being a predefined instance as would
;;; f.instantiate-instance.
;;; 
(defun init-smuggling-plan (mode)
  (let ((old-plan (f.make-new-instance
		    'smuggling-plan
		    t
		    *predefined-instance*)))
    (f.unify (f.get old-plan *actor-slot*)
	     (if (action-mode-p mode)
		 *criminal-actor*
		 *reasoner*))
    (f.unify (f.get old-plan 'object)
	     (f.instantiate-instance explosives))
;    (f.put! 'true.0
;	    old-plan
;	    'success)
    old-plan))


(defun init-bust-plan (mode)
  (let ((old-plan (f.make-new-instance
		    'bust-plan
		    t
		    *predefined-instance*)))
    (f.unify (f.get old-plan *actor-slot*)
	     (if (action-mode-p mode)
		 *police-actor*
		 *reasoner*))
    (f.unify (f.get old-plan 'object)
	     (f.instantiate-instance smuggler))
;    (f.put! 'true.0
;	    old-plan
;	    'success)
    old-plan))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;           FOUR PHASES OF PLANNING
;;;           (will become three, right?)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;
;;;; Problem Identification Phase
;;;;



;;; 
;;; Function to notice opportunities to carry out a previously
;;; suspended plan or action because favorable conditions have
;;; arisen, or the goal has already been satisfied. This function
;;; also notices when implicit goals to understand an event in the
;;; world (perception-goals) and explicit goals to understand whether
;;; actions meet one's expectations (feedback-goals) are the same. If
;;; so, unify the two. It also needs to be able to recognize the
;;; conditions under which background or adjunct plans apply. Thus
;;; when anonymity is in danger of being lost (in the cops & robber
;;; domain), the noticer (smuggler) must trigger a response (by
;;; raising the priority of the goal?).
;;;
;;; ||||| Maybe this should be called by a potential-solution-p predicate.
;;; See function do-plan.
;;; 
(defun notice (concept current-goal world-model)
  ;; Dumb kludge of unification mentioned in above comments.
  (cond ((equal
	   (*FRAME*
	     (f.get current-goal
		    'goal-object))
	   (*FRAME*
	     (f.get (second (list-queue *Goal-Queue*))
		    'goal-object)))
	 (remove-item *Goal-Queue*)		; The first item IS current-goal
	 (remove-item *Goal-Queue*)		; Remove the equivalent goal.
	 (add-item current-goal *Goal-Queue*))	; Put current-goal back on as first.
	(t  nil))
  )



(defun go-thru-motion (concept id-node w-goal)
  )



(defun formulate-problem (concept w-goal)
  ;; Post goal to generate a solution.
  (spawn-sub-goal
    *current-character*
    (make-goal-state 'generate
		     *current-character*
		     concept)
    ;; ||||| NOTE that this is not really an achievement-goal
    ;; but it is in the service of one. If we make it a knowledge-
    ;; acquisition-goal the control passes to do-understand. Fix later.
    'achievement-goal
    'seven.0
    w-goal)
  )


(defun exists-primitive-p (concept)
  nil)


;;; If the goal-state can be accomplished by a trivial (primitive)
;;; action, then perform the action without much "thought".
;;; Otherwise pose a problem by forming the goal to solve it.
;;;
;;; NOTE that the logic is backwards compared to the strategy-
;;; decision for question-identification. If there is a primitive the
;;; system lightly processes the concept, whereas in the question-
;;; identification if there does not exist a reason the system skims.
;;; 
(defun pr.strategy-decision (decision-basis concept)
  (let ((primitive (exists-primitive-p concept))
	(k-state (f.instantiate-frame
		   knowledge-state)))
    (f.put-all!					; Note that believed-item slot is bound to co-domain.
      (or primitive		  
	  (list 'noprimitive nil))
      k-state
      'believed-item)
    (f.put!
      (list k-state)
      (f.get
	decision-basis
	'knowledge)
      'members)
    (if primitive
	'gtm.0
	'formulation.0)
    ))


(defun pr.runstrategy (id-node choice concept w-goal)
  (f.unify (f.get id-node 'main-result)
	 (f.instantiate-frame outcome))
  (case choice
    (formulation.0
      (f.put! (list (formulate-problem
		      concept
		      w-goal))
	      (f.get id-node 'main-result)
	      'members)
      'formulate-problem.0)
    (gtm.0
      (go-thru-motion concept id-node w-goal)
      'go-thru-motion.0)
    ( t (format *aqua-window* "ERROR: unknown pr-strategy - ~s." choice)))
  )


(defun identify-problem (w-goal id-node)
  (f.unify (pr.runstrategy
	   id-node
	   (f.unify (f.get id-node 'strategy-choice)
		  (pr.strategy-decision
		    (return-decision-basis id-node)
		    (goal-state w-goal)))
	   (goal-state w-goal)
	   w-goal)
	 (f.get id-node 'strategy-execution))
  )



;;;;
;;;; Plan Generation Phase
;;;;


;;; 
;;; Function get-plan-from-node retrieves the plan using the index
;;; stored in the decision-basis. It unifies it with the item wanted
;;; in the trace-meta-xp. (REWRITE).
;;; 
(defun get-plan-from-node (problem plan-node w-goal)
  (let* ((first-belief
	   (car
	     (return-decision-basis plan-node)))
	   (new-plan
	     (f.make-new-instance 
	       (*FRAME*
		 (first
		   (retrieve-memory
		     (f.chase-path
		       first-belief
		       'believed-item
		       'type)
		     (f.chase-path
		       first-belief
		       'believed-item
		       'relation)))))))
    (f.unify
      (f.get
	new-plan
	'main-result)
      problem)
    (f.unify (f.chase-path
	       (processing-trace
		 w-goal)
	       'main-goal
	       'backptr)
	     new-plan))
  )


;;;
;;; Function remember-plan is akin to memory-based planning.  Since the
;;; system has already decided on this strategy, the plan is assumed to
;;; be located on the d-c-node passed to the function. Function
;;; get-plan-from-node is called to retrieve it. Support goals are
;;; added to the goal queu if there exist any in the gen-goals slot of
;;; the plan. Then the individual plan steps are marked as HVQs as if
;;; the plan represents an answer to the question 'What is the solution
;;; to the problem?' Finally a goal is spawned to evaluate the plan.
;;;
;;; ||||| Problem will always be a goal-object?
;;; 
(defun remember-plan (problem plan-node w-goal)
  (let ((new-plan (get-plan-from-node problem plan-node w-goal)))
    ;; When we set these additional goals,
    ;; we need to establish the backpointers 
    ;; to the new-plan.
;    (with-character-style (*Style*)
      (format
	*aqua-window*
	"~%~%Retrieving plan for concept ~s~%"
	  problem)
      (format
	*aqua-window*
	"~%Produced plan: ~s.~%"
	new-plan)
;      )
    
;    ;; For each scene in the list, post a plan to want it accomplished.
;    (dolist (each-scene (f.get new-plan 'scenes))
;      (spawn-goal *current-character*
;		  (make-goal-state 'wants
;				   *current-character*
;				   each-scene)
;		  'achievement-goal
;		  'seven.0))
    (dolist (each-goal (f.get new-plan 'gen-goals))
      (add-item each-goal *Goal-Queue*))
    ;; Make each step of the plan a HVQ on the proposed anwser to the problem.
    (let ((plan-steps (f.get new-plan 'scenes)))
      (dolist (each-step plan-steps)
	(add-hvq each-step new-plan)
      (f.put plan-steps new-plan *hvqs-slot*)))
    (mark-as-question new-plan *hypothesized-in*)
    ;; Post goal to verify plan/solution.
    (spawn-sub-goal
      (goal-actor w-goal)
      (make-goal-state
	'test
	(goal-actor w-goal)
	new-plan)
      'achievement-goal
      'seven.0
      w-goal)
    new-plan)
  )



(defun plan-from-scratch (problem w-goal)
  (format
    *aqua-window*
    "~%Dummy execute of function plan-from-scratch.~%")
  )


(defun p.strategy-decision (decision-basis problems)
  (do-break p.strategy-decision)
  (let ((new-plan (first (retrieve-memory
			   'plan-type.0
			   problems)))
	)
    (cond (new-plan
	   (let ((k-state (f.instantiate-frame
			    knowledge-state)))
	     (f.unify (f.instantiate-frame
			`(index			;|||||| Need to add memory-item etc.?
			   (type (,*value-facet* plan-type.0))
			   (domain (,*value-facet* ,problems))
			   (co-domain (,*value-facet* ,new-plan))
			   (relation (,*value-facet* ,problems))
			   (memory-item (,*value-facet* ,new-plan))))
		      (f.get
			k-state
			'believed-item))
	     (f.put! (list k-state)
		     (f.get
		       decision-basis
		       'knowledge)
		     'members))
	   'MBR.0)
	  (t
	   'MEA.0)))
  )


(defun p.runstrategy (plan-node choice problem w-goal)
  (f.unify (f.get plan-node 'main-result)
	 (f.instantiate-frame outcome))
  (case choice
    (MEA.0
      (f.put! (list (plan-from-scratch problem w-goal))
	      (f.get plan-node 'main-result)
	      'members)
      'do-MEA.0)
    (MBR.0
      (f.put! (list (remember-plan
		      problem
		      plan-node
		      w-goal))
	      (f.get plan-node 'main-result)
	      'members)
      'do-MBR.0)
    (suspension.0
      (suspend-task problem w-goal)
      'suspend-task.0)
    ( t (print "ERROR: unknown p-strategy." )))
  )



(defun generate-plan (w-goal plan-node)
  ;; ||||| The following code is currently dependent on the order of parameters.
  (f.unify (p.runstrategy
	     plan-node
	     (f.unify (f.get plan-node 'strategy-choice)
		      (p.strategy-decision
			(return-decision-basis plan-node)
			(goal-state w-goal)))
	     (goal-state w-goal)
	     w-goal)
	   (f.get plan-node 'strategy-execution))
  ;; ||||| The following comes from generate-hypothesis. Will we need something similar?
  ;; Do we need a main-plan slot in a Trace-Meta-XP?
  (f.put-all!
    (car (return-result plan-node))
    (processing-trace w-goal)
    'main-xp)
  )



;;;;
;;;; Evaluation Phase
;;;;

;;;
;;; To evaluate a plan, Meta-AQUA currently (12 Jul 93) tries to run it.
;;; To run it the system calls do-action on the plan. This function
;;; calls function perform-next-scene on each plan step, spawning a new
;;; goal to understand the resultant action at each step's execution.
;;;



;;; 
;;; Function execute-in-world simulates the world independently of the
;;; agents expectations, knowledge (agents know only subset of the
;;; world, and some of that is perhaps incorrect), or desires (goals).
;;; Note that the results of the event may not all be apparent to the
;;; agent, even after the event.
;;;
;;; |||||Very hacked right now. [29 may 93]
;;;
;;; The world itself gives the agent the goal of comprehending changes
;;; and actions that occur. Careful though since this is a situated
;;; point of view.
;;;
;;; ||||| But how to notice when the world produces a different result
;;; than expected??? The goal the world spawns will be simply to
;;; understand the next input, and the input will be different.
;;; 
(defun execute-in-world (event)
  (f.put!
    *nil*
    (spawn-new-goal
      (goal-actor w-goal)
      (make-goal-state
	'understands
	(goal-actor w-goal)
	event)
      'knowledge-acquisition-goal
      'seven.0
;;; 			   w-goal
      )
    'mxp)
;  (if (f.get scene 'main-result)
;      (set-model
;	*World-Model*
;	(cons (f.get scene 'main-result)
;	      (get-model *World-Model*))))
  (cond ((equal (frame-type event)
		'write-body)
	 (let ((alternative-solution
		 (f.instantiate-frame recursive-add1nums-defun)))
;	   (with-character-style (*Style*)
	     (format
	       *aqua-window*
	       "~%LISP simulator provides alternative solution ~s~%"
	       alternative-solution)
;	     )
	   
	   (set-model
	     *World-Model*
	     (cons
	       alternative-solution
	       (get-model *World-Model*)))))
	)
;  (case mode
;    (act-out-story
;      )
;    (read-story
;      )
;    (LISP-programming
;      (case (frame-type event)
;	(run-program
;	  )))
;    )
  )

;;; 
;;; Kludge for the moment.  
;;; 
(defun do-scene (scene w-goal)
  (do-break do-scene)
  (let ((goal-agent (goal-actor w-goal)))
;    (with-character-style (*Style*)
      (format
	*aqua-window*
	"~%Actor ~s performs act ~s~%"
	goal-agent
	scene)
      (format
	*aqua-window*
	"~%and expects result ~s~%"
	(f.get scene 'main-result))
;      )
    
    (execute-in-world scene)
    )
  scene
  )


;;; 
;;; Any scene already performed will have its success slot
;;; marked true or false by the understanding task. Of
;;; course this depends on their being no value when the
;;; scene is initially instantiated.
;;; 
(defun has-performed-scene-p (scene)
  (let ((success-val
	  (f.get scene 'success)))
    (or (equal
	  success-val
	  'true.0)
	(equal
	  success-val
	  'false.0)))
  )



;;;
;;; To perform the next scene we find the first scene in
;;; the scene-list which has not already been executed and
;;; then call do-scene.
;;; 
(defun get-next-unexecuted-scene (scene-list)
  (do-break get-next-unexecuted-scene)
  (cond ((null scene-list)
	 nil)
	((has-performed-scene-p
	   (first scene-list))
	 (get-next-unexecuted-scene
	   (rest scene-list))))
  )



;;; 
;;; Perform each scene with their preconditions met until reaching
;;; one not met. For each scene performed, post a goal to understand
;;; the outcome of the action. Finally it does it. Check that it
;;; meets the actions expectations, i.e, it produces the main-result
;;; if the action is a mop.
;;;
;;; ||||| What should the function be returning?  Whatever it is will
;;; be placed in the main-result slot of the verify d-c-node.
;;; However, we will not know if the plan was successful until we
;;; know whether or not the main plan goal was successful. There will
;;; be lots of baggage to do this, but it will be somewhat like was
;;; done with the main-results slot of the generate d-c-node after
;;; the suspended goal was resumed during understanding. Do not
;;; forget that the main-result slot will have to be made *nil*
;;; before the f.unify is called
;;; 
(defun do-action (plan w-goal)
  (do-break do-action)
  (let* ((current-step
	   (get-next-unexecuted-scene
	     (f.get plan 'scenes))))
    (cond (current-step
	   ;; |||||The following is a temporary kludge. 24 May 93
	   ;; Should do this later when sure that step was successful.
	   (f.put! 'true.0
		   current-step
		   'success)
	   (do-scene
	     current-step
	     w-goal)
	   ;; Need to include the larger context.
	   (f.put!
	     *nil*
	     (spawn-new-goal
	       (goal-actor w-goal)
	       (make-goal-state
		 'understands
		 (goal-actor w-goal)
		 current-scene)
	       'knowledge-acquisition-goal
	       'seven.0
;;; 			   w-goal
	       )
	     'mxp)
	   (f.put!
	     plan
	     new-goal
	     'backptr))
	  (t
	   ;; Here is where the system should verify that the expected main-result of the
	   ;; plan was indeen the actual result in the world.
	   ;; |||||Mark action as successful or not? Clean-up rituals anyhow.
	   (f.get plan 'main-result))))
  )




;;;
;;; No replanning yet.
;;; 
(defun replan (plan w-goal)
  )



(defun preconditions-met (plan)
  t)



;;;
;;; At the current time (12 Jul 93) all plans will be enabled for
;;; execution; no preconditions will not be met since the predicate
;;; will always return true.
;;; 
(defun e.strategy-decision (decision-basis plan)
  (if (preconditions-met plan)
      'execution.0
      'replanning.0)
  )


(defun e.runstrategy (eval-node choice plan w-goal)
  (f.unify (f.get eval-node 'main-result)
	 (f.instantiate-frame outcome))
  (case choice
    (execution.0
      (f.put! (list (do-action plan w-goal))
	      (f.get eval-node 'main-result)
	      'members)
      'execute.0)
    (replanning.0
      (f.put! (list (replan plan w-goal))
	      (f.get eval-node 'main-result)
	      'members)
      'replan.0)
    (suspension.0
      (suspend-task plan w-goal)
      'suspend-task.0)
    (t (print "ERROR: unknown e-strategy." ))))



(defun evaluate (w-goal eval-node)
  ;; ||||| The following code is currently dependent on the order of parameters.
  (f.unify
    (e.runstrategy
      eval-node
      (f.unify
	(f.get eval-node 'strategy-choice)
	(e.strategy-decision
	  (return-decision-basis eval-node)
	  (goal-state w-goal)))
      (goal-state w-goal)
      w-goal)
    (f.get eval-node 'strategy-execution))
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;           MAIN CONTROL FUNCTIONS 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; 
;;; ||||| Check to see whether the spawning of goals are done right.
;;; 
;;; On input the calling function, dispatch-world-goal, assures that
;;; the w-goal is for the system to want some state.
;;; 
;;; Function boot-wanting is the routine to handle world goals in the
;;; dispatch call of function plan-about. It spawns a goal to
;;; identify any problems concerning the desired goal-state of the
;;; w-goal if there is no plan yet, otherwise it spawns a goal to
;;; test the plan.
;;; 
(defun boot-wanting (w-goal)
  (let ((current-actor (goal-actor w-goal)))
    (if (or
	  (equal (frame-type (f.get w-goal 'backptr))
		 *nil*)
	  (equal (frame-type (f.get w-goal 'backptr))
		 'plan))			; then this plan is not instantiated yet.	
	;; Post goal to identify the problem.
	(spawn-sub-goal
	  current-actor
	  (make-goal-state
	    'id
	    current-actor
	    (goal-state w-goal))
	  ;; ||||| NOTE that this is not really an achievement-goal but it is in the
	  ;; service of one. If we make it a knowledge-acquisition-goal the control
	  ;; passes to do-understand. Fix later.
	  'achievement-goal
	  'seven.0
	  w-goal)
	;; Otherwise post goal to test the instantiated plan.
	(spawn-sub-goal
	  current-actor
	  (make-goal-state 'test
			   current-actor
			   (first
			     (return-result
			       (return-d-c-node
				 (processing-trace w-goal)
				 'generation))))
		    ;; (goal-state w-goal))
	  ;; ||||| NOTE that this is not really an achievement-goal either.
	  'achievement-goal
	  'seven.0
	  w-goal))
    ))




;;;
;;; Function dispatch-world-goal calls particular reasoning functions
;;; depending on the type of goal-object in the knowledge goal.  It
;;; returns a phase identifier that corresponds to the goal-object
;;; type. The counterpart function in the understanding process is
;;; dispatch-knowledge-goal.
;;; 
(defun dispatch-world-goal (w-goal d-c-node)
  (case (frame-type (goal-object w-goal))
    (wants
      (boot-wanting w-goal)
      'wants)
    (id
      (identify-problem w-goal d-c-node)
      'identification)
    (generate
      (generate-plan w-goal d-c-node)
      'generation)
    (test
      (evaluate w-goal d-c-node)
      'examination)
    (review/learn
      ;; A f.unify is performed on goal-target internally to function review.
      ;; Thus we need to make sure that it remains bound to the proper frame.
      (learn w-goal d-c-node)
      'review)
    ( t
     (print
       "ERROR: unknown goal type in function dispatch-world-goal.")
     nil))
  )


;;; 
;;; Old Control:
;;; 
;;; (defun plan-about (concept)
;;;   (p.learn-about    
;;;     (evaluate 
;;;       (generate-plan 
;;;         (identify-problem concept)))))
;;; 
(defun plan-about (w-goal)
  (let* ((d-c-node (gen-d-c-node w-goal)) 
	 (phase (dispatch-world-goal
		  w-goal
		  d-c-node)))
    (if (and phase
	     (not (equal phase 'wants)))
	(let ((which-tmxp
		(if (equal phase 'review)
		    (goal-state w-goal)
		    (processing-trace
		      w-goal))))
	  (f.unify
	    d-c-node 
	    (f.get which-tmxp phase))
	  (f.put! (f.make-relation
		    which-tmxp phase)
		  which-tmxp
		  'current-phase)))
    ))



;;;
;;; Function solve-old-problems is the parallel of function
;;; answer-old-questions. The new input has signalled an object which may
;;; potentially aid in the achievement of an old suspended world-goal.
;;;
;;; |||||| This remains to be worked on.
;;; 
(defun solve-old-problems (solution problems)
  (format
    *aqua-window*
    "~%Entering solve-old-problems with nothing in it.~%")
  )


;;; 
;;; Look for opportunities to resume previously suspended planning
;;; tasks. If the opportunity does not exist then we call plan-about.
;;; This parallels the function do-understand.  Or, on the other
;;; hand, do we call the Noticer here?
;;;
(defun do-plan (w-goal)
  (let* ((goal-target (goal-state w-goal))
	 (old-problems
	   (and
	     (is-goal-object-p 'wants w-goal)
	     ;; ||||| Should have potential-solution-p predicate calling notice instead?
	     (notice goal-target
		     w-goal
		     (get-model *World-Model*)))))
    (cond (old-problems
;	   (with-character-style (*Style*)
	     (format
	       *aqua-window*
	       "~%~%Input triggers reminding of old problem:~%  ~s~%"
	       old-problems)
	     
;	     )
	     (solve-old-problems
	      goal-target
	      old-problems))
	  (t
	   (plan-about w-goal)
	   )))
  )





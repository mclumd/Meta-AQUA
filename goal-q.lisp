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
;;;;			       File: goal-q.lisp
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
;;;;GOAL and GOAL QUEUE MANAGEMENT
;;;;
;;;; This file also contains the model data structure from which 
;;;; the priority queue data structure is built.

;;;
;;; GOAL PREDICATES
;;; 


;;;
;;; Predicate is-new-goal-p can tell if a goal is new
;;; because it has no supergoal.
;;; 
(defun is-new-goal-p (goal)
  (equal (f.get goal 'supergoal)
	 *nil*)
  )

;;;
;;; ||||| Note that there is some overlap in the semantics of the
;;; goal-type slot and testing to see if the goal-object isa-p
;;; an understands goal, etc. See the next function. 1 May 93
;;; 
(defun is-goal-type-p (goal-type goal)
  (isa-p goal-type (goal-type goal))
  )


;;;
;;; See comments above.
;;;
(defun is-goal-object-p (object-type goal)
  (isa-p
    object-type
    (list (goal-object goal)))
  )


(defun has-supergoal-p (goal)
  (not (eq
	 *nil*
	 (supergoal goal)))
  )


(defun goal-achieved-p (goal)
  (eq 'true.0
      (f.get goal 'achieved))
  )


(defun all-subgoals-achieved-p (goal)
  (every #'goal-achieved-p
	 (subgoals goal))
  )




;;;
;;; GOAL PRINT FUNCTION
;;;

;;;
;;; ||||| Currently not used.
;;; 
(defun print-goals (stream &optional (goal-queue (list-queue *Goal-Queue*)))
  (dolist (each-goal goal-queue)
    (format
      stream
      "~%Goal:     ~s~%"
      (*FRAME* (f.get each-goal 'goal-object)))
    (format
      stream
      "~%Priority: ~s~%"
      (f.get each-goal 'priority)))
  )




;;;
;;; GOAL ACCESSORS
;;;
;;; Because goals are especially distinguished,
;;; we give them named access functions.
;;; 


(defun supergoal (goal)
  (f.get goal 'supergoal)
  )

(defun subgoals (goal)
  (f.get goal 'subgoals)
  )

(defun goal-actor (goal)
  (f.get goal 'goal-actor)
  )

(defun goal-type (goal)
  (f.get goal 'goal-type)
  )

(defun goal-object (goal)
  (f.get goal 'goal-object)
  )


(defun goal-state (goal)
  (f.get (goal-object goal) *co-domain-slot*)
  )


;;;
;;; Function associated-trace is a very important feature.
;;; The only way to find the current trace is through the
;;; current goal. This function returns the trace of the
;;; processing in pursuit of the passed goal (and since the
;;; goal may be a sub-goal, in pursuit of others as well.
;;; 
(defun processing-trace (goal)
  (f.get goal 'mxp)
  )



;;;
;;; GOAL CREATION 
;;;

(defun make-goal-state (relation-type actor state)
  (f.instantiate-frame `(,relation-type
			 (,*domain-slot*
			  (,*value-facet* ,actor))
			 (,*co-domain-slot*
			  (,*value-facet* ,state)))))


;;; 
;;; Function spawn-goal makes a new goal instance, placing it
;;; on the goal queue. If this is being called during init-goals
;;; then we make the mxp trace have this goal as its main-goal
;;; and we add relations to the goal-state.
;;;
;;; NOTE that it is important that the new goal is added to the
;;; queue before any calls of f.unify (as this function enforces
;;; if all goals are added through calls of spawn-goal). Because
;;; the queue is itself a frame, any f.unify will keep bindings
;;; listed in the goals backpointers consistent, including those
;;; bindings which tie them to the queue. Thus the goal queue 
;;; will not become incorrect.
;;; 
(defun spawn-goal (goal-actor goal-object goal-type priority)
;  (let ((return-val nil))
  (do-break spawn-goal)
;  (print-goal-status
;    (setf
;      return-val
      (add-item
	(f.make-new-instance 
	  `(
;	    ,(if (ends-with-dot-zero-p goal-type)
;		 (strip-dot-zero goal-type)
;		 goal-type)
	    goal
	     (goal-actor  (value ,goal-actor))
	     (goal-object (value ,goal-object))
	     (goal-type   (value ,goal-type))
	     (priority    (value ,priority))
	     (achieved    (value false.0))
	     ))
	*Goal-Queue*)
;      )
;    *goal-queue*)
;  return-val)
  )


;;;
;;; NOTE that if one passes a variable that holds the value of supergoal
;;; as the supergoal parameter, because of the f.unify, the variable may no
;;; longer be valid. In these cases one needs to call it thusly:
;;;      (setf supergoal (spawn-subgoal .... supergoal)
;;; because it returns the supergoal
;;; ||||| This hold true for any f.unify call on a variable directly.
;;; 
(defun spawn-sub-goal (goal-actor goal-object goal-type priority supergoal)
  (let ((new-goal
	  (spawn-goal
	    goal-actor goal-object
	    goal-type priority)))
    (setf
      supergoal
      (f.unify supergoal
	       (f.get new-goal 'supergoal)))
    (f.unify (f.get new-goal 'mxp)
	     (f.get supergoal 'mxp))
    (f.put! (append (f.get supergoal 'subgoals)
		    (list new-goal))
	    supergoal
	    'subgoals)
    supergoal)
  )



;;;
;;; Returns the new goal.
;;; 
(defun spawn-new-goal (goal-actor goal-object goal-type priority)
  (let ((new-goal
	  (spawn-goal
	    goal-actor goal-object
	    goal-type priority)))
    (f.add-relations				; Is this not done at earlier init time?
      (goal-state new-goal))
    (f.put!
      *nil*
      (f.unify
	new-goal
	(f.get
	  (f.unify (f.get new-goal 'mxp)
		   (f.instantiate-frame trace-meta-xp))
	  'main-goal))
      'supergoal)
    new-goal)
  )



;;;
;;; GOAL SUSPENSION and RESUMPTION 
;;;


;;;
;;; |||||NOTE that we probably should suspend all subgoals and
;;; siblings too.
;;; 
(defun suspend-goal-hierarchy (goal)
  (delete-item goal *Goal-Queue*)
  (if (has-supergoal-p goal)
      (suspend-goal-hierarchy (supergoal goal)))
  )


;;;
;;; Function get-supergoal-chain returns a list of a superordinate
;;; goals in order from the highest node in the tree (root node)
;;; to the current goal.
;;;
(defun get-supergoal-chain (goal)
  (cond ((or (null goal)
	     (eq goal *nil*))
	 nil)
	((has-supergoal-p goal)
	 (append
	   (get-supergoal-chain
	     (supergoal goal))
	   (list goal)))
	(t
	 (list goal)))
  )

(defun resume-goal-hierarchy (goal)
  (dolist (each-goal
	    (get-supergoal-chain goal))
    (add-item each-goal *Goal-Queue*))
  )


;;;
;;; GOAL DELETION 
;;;


;;;
;;; Function remove-achieved-goal marks the parameter goal
;;; as being achieved, removes it from the goal queue, then
;;; if all other sibling subgoals of its supergoal are already
;;; acheived, recursively calls itself on the supergoal.
;;;
;;; NOTE that since remove-achieved-goal should have also been
;;; called on any sibling goals, they need not be removed from
;;; the queue.
;;; 
(defun remove-achieved-goal (goal)
  (do-break remove-achieved-goal)
  (f.put! 'true.0 goal 'achieved)		; First mark goal as achieved.
  (delete-item goal *Goal-Queue*)		; Remove it from the goal queue.
  (if (and
	(has-supergoal-p goal)
	(all-subgoals-achieved-p
	  (supergoal goal)))
      (remove-achieved-goal
	(supergoal goal)))
  )



;;;
;;; PRIORITY QUEUE MANIPULATION
;;;

;;;
;;; Function add-item puts an additional item in the (already
;;; sorted) priority queue.  It searches for the first element
;;; whose priority is not greater than the new item's priority.
;;; The new item is then inserted before this element. Thus the
;;; new item will preceed all additional elements in the queue
;;; that have the same priority.
;;; 
(defun add-item (item queue)
  (cond ((empty-queue-p queue)
	 (set-queue queue (list item)))
	(t
	 (let ((found nil)
	       (item-priority (f.get item 'priority)))
	   (do* ((head
		   nil
		   (append
		     head
		     (list current-item)))
		 (current-item
		   (front-of queue)
		   (first remainder))
		 (remainder
		   (rest (list-queue queue))
		   (rest remainder)))
		((or (null current-item)
		     ;; If the item's priority is less or equal to or less than the current item's 
		     (if
		       (not
			 (greater-than-p
			   (f.get
			     current-item
			     'priority)
			   item-priority))
		       (setf found t)))
		 (if found
		     ;; Then current-item is less priority than item.
		     (set-queue
		       queue
		       (append head
			       (list item)
			       (list current-item)
			       remainder))
		     (set-queue
		       queue
		       (append (list-queue queue)
			       (list item)))))
	     ))))
  (if *Debug-On*
      (print-goal-status
	(front-of queue)
	queue
	'ADD-ITEM))
  item)



;;;
;;; Function delete-item searches for and deletes an item wherever
;;; it is found in the queue. It returns the resultant queue.
;;; 
(defun delete-item (item queue)
  (set-queue
    queue
    (delete item (list-queue queue)))
  (if *Debug-On*
      (print-goal-status (front-of queue) queue 'DELETE-ITEM))
  (list-queue queue)
  )


;;;
;;; Function remove-goal removes the goal at the front of the queue
;;; as side-effect and returns the goal that was at the front.
;;; 
(defun remove-item (queue)
  (let ((first-item (front-of queue)))
    (set-queue queue (rest (list-queue queue)))
    (if *Debug-On*
      (print-goal-status (front-of queue) queue 'REMOVE-ITEM))
    first-item)
  )


;;;
;;; Calls to new-queue must quote the variable passes as
;;; parameter.
;;; 
(defun new-queue (queue-name)
  (check-type queue-name symbol)
  (assert (not (or (null queue-name)
		   (frame-var-p queue-name)))
	       (queue-name)
	       "Queue-name parameter ~s cannot be nil or a frame."
	       queue-name)
  (if (null (symbol-value queue-name))
      (new-model queue-name)
      (set-queue (symbol-value queue-name)
		 nil))
  (symbol-value queue-name)
  )


;;;
;;; Function front-of returns the goal at the front of the queue.
;;;
(defun front-of (queue)
  (first (list-queue queue))
  )


;;;
;;; Predicate empty-queue-p returns t if the queue passed
;;; to it has no items.
;;; 
(defun empty-queue-p (queue)
  (null (list-queue queue))
  )

;;;
;;; Function list-queue is a primitive that returns the
;;; entire queue as a list.
;;; 
(defun list-queue (queue)
  (get-model queue)
  )

;;;
;;; Function set-queue is a primitive to assign an arbitrary
;;; value to a queue.
;;; 
(defun set-queue (queue new-value)
  (set-model queue new-value)
  )



;;;
;;; MISCELLANEOUS GOAL SUPPORT FUNCTIONS
;;;

;;;
;;; Function trans performs translation from a frame representation of a number
;;; to its integer representation. Trans is currently used to generate numbers
;;; for goal priority comparison.
;;;
(defun trans (integer-frame)
  (case integer-frame
    (one.0 1)
    (two.0 2)
    (three.0 3)
    (four.0 4)
    (five.0 5)
    (six.0 6)
    (seven.0 7)
    (eight.0 8)
    (nine.0 9)
    (ten.0 10)
    (eleven.0 11)
    (otherwise
      (format
	*aqua-window*
	"~%ERROR: incorrect parameter passed to function trans.~%")))
  )


;;;
;;; Function inverse-trans performs translation from an integer to its frame
;;; representation. Inverse-trans is currently used to generate priorities
;;; given a numerical value.
;;;
(defun inverse-trans (integer)
  (case integer
    (1 one.0)
    (2 two.0)
    (3 three.0)
    (4 four.0)
    (5 five.0)
    (6 six.0)
    (7 seven.0)
    (8 eight.0)
    (9 nine.0)
    (10 ten.0)
    (11 eleven.0)
    (otherwise
      (format
	*aqua-window*
	"~%ERROR: incorrect parameter passed to function inverse-trans.~%")))
  )


;;;
;;; Predicate greater-than-p returns t if the first priority is
;;; greater than the second, nil otherwise. Since both
;;; priorities are frame representations of integers the
;;; function trans is called to translate them to numeric
;;; arguments for the ">" function.
;;; 
(defun greater-than-p (priority1 priority2)
  (cond ((or (null priority1)
	     (null priority2))
	 (format
	   *aqua-window*
	   "~%ERROR: null priority in greater-than-p.~%")
	 (break))
	(t
	 (> (trans priority1) (trans priority2))))
  )



;;;
;;; MODEL DATA STRUCTURE
;;;
;;; The following functions are the access functions for the type
;;; MODEL. See comments on variable *World-Model* for some extra details.
;;;


;;; 
;;; Function get-model returns the value of the frame-list of the
;;; model. Function set-model places a new value on the frame-list
;;; of the model. Of course function print-model displays the value
;;; of the frame-list appropriately.
;;;

(defun get-model (model)
  (let ((model-value (f.get model 'frame-list)))
    (if (equal *nil* model-value)
	nil
	model-value))
  )


;;;
;;; Function set-model assigns a new value to the model argument.
;;; The new value must be a list.
;;; 
(defun set-model (model new-value)
  (do-break set-model)
  (check-type new-value list)
  (f.put!
    (if (null new-value)
	*nil*
	new-value)
    model
    'frame-list)
  )


;;;
;;; Function add-to-model adds new item to the current value of the model.  If
;;; optional parameter is non-nil then new item must not already exist within
;;; model.
;;; 
(defun add-to-model (new-item model &optional unique)
  "Add a new item to model."
  (let ((current-model
	  (get-model model)))
    (if (or (not unique)
	    (not (member new-item
			 current-model)))
	(set-model
	  model
	  (cons new-item
		current-model))))
  )




;;;
;;; Function new-model creates a new model-instantiation.
;;; Calls to new-model must quote the variable passed as
;;; parameter.
;;; 
;;; There is an issue whether or not to use the same model,
;;; but initialized to *nil*, if model-name is already bound
;;; to an old model (see comments on new-queue function).
;;; In the present version we ignore this efficiency concern
;;; in favor of guaranteeing a clean, new model with no
;;; backptrs to worry about.
;;; 
(defun new-model (model-name)
  (check-type model-name symbol)
  (assert (not (or (null model-name)
		   (frame-var-p model-name)))
	       (model-name)
	       "Model-name parameter ~s cannot be nil or a frame."
	       model-name)
  (setf (symbol-value model-name)
	(f.instantiate-frame model))
  )


;;;
;;; Function print-model prints a model in reverse order. It precedes the
;;; display with an optional heading (some string).
;;; 
(defun print-model (stream model &optional heading)
  (if heading
      (format stream
	      "~%~%~a" heading))
  (dolist (each-frame (reverse (get-model model)))
    (format
      stream
      "~%-> ~s" each-frame))
  (format stream "~%")
  )







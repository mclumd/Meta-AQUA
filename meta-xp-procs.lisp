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
;;;;			    File: meta-xp-procs.lisp
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
;;;; META-XP MANIPULATION  FUNCTIONS
;;;;


;;; Function gen-d-c-node instantiates a new decide-compute-node to
;;; hold the record of each of the four phases in a Trace-Meta-XP.
;;; The function also creates a place for the basis of the decision. At
;;; the current time, the program only bases decisions on knowledge,
;;; not inferences or heuristics. See representation for a basis
;;; frame.
;;;
;;; By placing the goal in the prime-state slot of the considerations
;;; of the initial-state of the d-c-node we link the side-effect of
;;; the previous phase with the initial-state of the current phase.
;;; 
(defun gen-d-c-node (goal)
  (let ((d-c-node (f.instantiate-frame d-c-node))
	(new-basis (f.instantiate-frame basis)) ; Instantiate a dummy basis for the upcoming decision.
	(new-considerations (f.instantiate-frame considerations))
	)
    ;; Instantiate and insert the knowledge part of the basis.
    ;; ||||| (The other parts to be handled in the future.)
    (f.unify (f.get new-basis 'knowledge)
	     (f.instantiate-frame collection))
    (f.unify goal
	     (f.get (f.unify
		      new-considerations
		      (f.get d-c-node 'initial-state))
		    'prime-state))
    ;; Place the basis in the d-c-node.
    (f.unify (return-decision-basis
	       d-c-node)
	     new-basis)
    d-c-node)
  )



;;;
;;; Function return-d-c-node returns a Decide-Compute-Node
;;; from a given Trace Meta-XP corresponding to the given phase.
;;; 
(defun return-d-c-node (tmxp phase)
  (f.get tmxp phase)
  )


;;;
;;; Given a d-c-node of a TMXP phase, return the goal which
;;; the phase is pursuing. The initial-state is a considerations
;;; node. The prime-state slot is the goal.
;;; 
(defun return-phase-goal (d-c-node)
  (f.chase-path d-c-node
		'initial-state
		'prime-state)
  )



;;;
;;; Access functions for XPs.
;;;


;;;
;;; Function return-decision-basis will return the most specific
;;; information in the D-C-Node for the basis upon which the strategy
;;; was chosen. Note that currently inference and heuristic slots are
;;; ignored; just knowledge is recognized (see definition of a basis
;;; frame).
;;; 
;;; ||||| Also see get-plan-from-node for perhaps even a deeper part
;;; of the basis that may be returned; that is, fetching the first
;;; belief in the members set.
;;; 
(defun return-decision-basis (d-c-node)
  (let* ((basis-frame
	   (f.chase-path
	     d-c-node
	     'strategy-decision
	     'basis-of-decision))
	 (basis-knowledge
	   (f.get basis-frame 'knowledge))
	 (basis-set (f.get basis-knowledge
			   'members)))
    (if (and (not (null basis-set))
	     (listp basis-set)
	     (not (equal
		    'entity
		    (first
		      (get-abstraction
			(first basis-set))))))	; Fix this.
	basis-set
	basis-frame))
  )



(defun display-d-c-node (d-c-node)
  "Displays the key nodes of a d-c-node."
  (format
    *aqua-window*
    "~%Previous Considerations: ~s -> ~s"
    (f.chase-path
      d-c-node
      'initial-state 'prime-state)
    (*FRAME*
      (f.chase-path
	d-c-node
	'initial-state 'prime-state 'goal-object))
    )
  (format
    *aqua-window*
    "~%Strategy Choice: ~s"
    (f.get
      d-c-node
      'strategy-choice))
  (format
    *aqua-window*
    "~%Strategy Execution: ~s"
    (f.get
      d-c-node
      'strategy-execution))
  (format
    *aqua-window*
    "~%Decision Basis: ~s"
    (f.get (first (return-decision-basis
		    d-c-node))
	   'believed-item))
  (format
    *aqua-window*
    "~%Side Effect: ~s -> ~s"
    (f.chase-path
      d-c-node
      'side-effect 'prime-state)
    (*FRAME*
      (f.chase-path
	d-c-node
	'side-effect 'prime-state 'goal-object)))
  (format
    *aqua-window*
    "~%Main Result: ~s~%"
    (return-result
      d-c-node))
  )



(defun display-TMXP (tmxp &optional display-learn-node?)
  "Displays the key nodes of a Trace Meta-XP."
  (format
    *aqua-window*
    "~%~%TRACE META-XP: ~s"
    tmxp)
  (format
    *aqua-window*
    "~%~%IDENTIFICATION NODE: ~s"
    (f.get
      tmxp
      'identification))
  (display-d-c-node
    (f.get
      tmxp
      'identification))
  (format
    *aqua-window*
    "~%GENERATION NODE: ~s"
    (f.get
      tmxp
      'generation))
  (display-d-c-node
    (f.get
      tmxp
      'generation))
  (format
    *aqua-window*
    "~%EXAMINATION NODE: ~s"
    (f.get
      tmxp
      'examination))
  (display-d-c-node
    (f.get
      tmxp
      'examination))
  (when display-learn-node?
    (format
      *aqua-window*
      "~%REVIEW/LEARN NODE: ~s"
      (f.get
	tmxp
	'review))
    (display-d-c-node
      (f.get
	tmxp
	'review))
    )
  (terpri
    *aqua-window*)
  )


;;;
;;; Function return-result knows about collections (sets).
;;; If the main-result is a collection, then it will return the
;;; members of the collection, otherwise it returns the result.
;;;
(defun return-result (frame)
  (let ((main-results (f.get frame 'main-result)))
    (if (isa-p
	  'collection
	  main-results)
	(f.get main-results 'members)
	main-results))
  )



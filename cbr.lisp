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
;;;;				File: cbr.lisp
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
;;           CASE-BASED REASONING 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Given some role as input, function retrieve-case performs a simple case
;;; retrieval. The retrieval mechanism is the same as that used for retrieval
;;; of opportunistic goals and questions, explanations and all memory items.
;;; The constant 'case-type.0 selects for the proper type of memory item.
;;; 
(defun retrieve-case (role)
  (let ((found-case (first (retrieve-memory 'case-type.0 role))))
;    (with-character-style (*Style*)
      (if found-case
	  (format
	    *aqua-window*
	    "~%Found past case ~s.~%" found-case)
	  (format
	    *aqua-window*
	    "~%No case found.~%"))
;      )
    
    found-case
    ))



;;;
;;; Function additional scenes returns a list of scenes from the new
;;; case that precede the given episode. If new-case has no scenes, or
;;; if episode is not in these scenes, then return nil.
;;; 
(defun additional-scenes (new-case episode)
  (let ((case-scenes (f.get new-case 'scenes)))
    (reverse
      (subseq
	case-scenes
	0
	(or
	  (position episode case-scenes)
	  ;; If episode is not in the scenes
	  ;; (including the case when case-scenes is nil),
	  ;; make sure that this function returns nil.
	  0))))
  )



;;;
;;; Function expand world enlarges the world model to include scenes
;;; from the new case that precede the given episode.  The new scenes
;;; are inserted immediately before the episode in the world model. NOTE
;;; that the list of events in the world model are in order of most
;;; recent to least recent.
;;; 
(defun expand-world (new-case episode)
  (let ((world (get-model *World-Model*)))
    (set-model
      *World-Model*
      (append (subseq
		world
		0
		(position episode world))	; Often this position will be 0.
	      (list episode)
	      ;; ||||| Should check that these new scenes
	        ;; are not already in the world model.
	      (additional-scenes new-case episode)
	      (rest (member episode world)))))
  )



;;;
;;; Access functions for Cases.
;;;


;;;
;;; Function get-scene returns a particular scene in a case.
;;;
;;; NOTE that one is subtracted from which-scene because if
;;; one wants the first scene, then which-scene = 1, but nth
;;; takes indexes zero through n-1.
;;;
(defun get-scene (which-scene case)
  (let ((case-scenes
	  (f.get case 'scenes)))
    (if (null case-scenes)
	(format
	  *aqua-window*
	  "~%Case has no scenes.~%")
	(nth (- which-scene 1)
	     case-scenes)))
  )




;;; 
;;; This function reasons from cases.  Made up the verb since could not think
;;; of existing one.
;;; 
;;; ||||| What should the function return actually?  There is much side-effect.
;;; The main-result slot of the d-c-node will hold the return value. However
;;; the generate-hypothesis function wants to unify the main-result of the
;;; node with the main-xp slot of the processing-trace.
;;; 
(defun episodize (question k-goal)
  (let* ((past-case (f.get (processing-trace k-goal) 'cases))
	 (new-case 
	   (f.make-new-instance
	     (frame-def past-case)))
	 (old-xps (f.get past-case *explanations-slot*))
	 (new-xps nil))
    (do-break
      "Break on (episodize ~s ~s)"
      question k-goal)
;    (with-character-style (*Style*)
      ;; The following unifies the 2 arrests in our scenario so far.
      ;; ||||| Remove this comment after no longer true due to particular scenario.
      (f.unify (f.get question *domain-slot*)
	     (f.get new-case 'goal-scene))
      (dolist (each-xp old-xps)
	(setf new-xps
	      (cons (apply-xp
		      (f.instantiate-frame
			(*FRAME* (frame-type each-xp)))
		      (f.make-relation
			(get-scene 1 new-case)
			*actor-slot*))
		    new-xps)))
;      )
    
    ;; ||||| Or should I make a relation in arrest to get back to the containing Bust?
    (expand-world new-case (f.get question *domain-slot*))
    ;; ||||| This is a big HACK too.
    ;; We cannot just port all explanations from the old case without
    ;; justification. It may not apply in the new situation. This is just 
    ;; a good place to look for XPs since the case will be SIMILAR.
    ;; If fact the XP should really be on the interdiction-act.
    (f.put! new-xps
	    new-case
	    *explanations-slot*)
    ;; Post goal to verify hypothesis.
    (if (and new-xps
	     (first new-xps))
	(spawn-sub-goal
	  *current-character*
	  (make-goal-state 'test
			   *current-character*
			   (first new-xps))
	  'knowledge-acquisition-goal
	  'seven.0
	  k-goal)
	(suspend-goal-hierarchy k-goal))
;;;     (first new-xps)
    new-case)
  ;; ||||| Keep in mind that we used to return the action, but now this function
  ;; returns the role, ie actor slot.
  ;; No, now it returns the first explanation from the bust case. Oh boy!
  ;; No, now it returns nil. Really the explanation is a side-effect to this routine.
  ;; The main-result should be the case (episode). This function needs much rethinking and more work!
  )





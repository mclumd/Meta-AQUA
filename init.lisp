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
;;;;				File: init.lisp
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


;;; 
;;; Added [cox 11Jul11]
;;; 
(defvar *seed-file-name* 
    (concatenate 'string 
	  CL-USER::*Meta-AQUA-system-dir* "Results/seed.file"))

;;; 
;;; Added [cox 18Jul11]
;;; 
(defvar *recover-file-name*
    (concatenate 'string 
	  CL-USER::*Meta-AQUA-system-dir* "Results/recover.file"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;           INITIALIZATION FUNCTIONS 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Other initialization functions can be found in solver.lisp (particular
;;; initialization of memory for the cops & robber scenario).
;;;

;;;
;;; Function init-aqua prepares the windows depending on the program running
;;; mode, loads the stories if in story mode (the default mode where mode equal
;;; read-story), indexes previously known explanations in memory, and indexes
;;; previously known plans. If mode is act-out-story then we are in action mode
;;; for multiple agent planning abnd problem solving.
;;; 
;;; Function init-aqua must be called once after each computer reboot.
;;;
;;; The parameter from-file? is used in order to initialize the random number
;;; seed to a specific value from a file that had been previously saved by
;;; another program run. By doing the initialization in this manner, the
;;; previous behavior should be replicated across an entire set of runs of
;;; Meta-AQUA. [cox 12mar95]
;;; 
(defun init-aqua (&optional
		  (recover-file-name		; optional file name for tspin crash recovery.
		    *recover-file-name*)
		  from-file?			; t -> init tspin random number seed from file.
		  (mode 'read-story))
  "Initialize Meta-AQUA after reboot."
  (setf *original-window* 
	nil  ; Commented out below [mcox 15aug96]
;	tv:selected-window			; Removed the burden of the user. [cox 2mar95]
;;; 	(accept '((tv:sheet )))
	)
  (init-struct-window-flag)			; Initialize flag to display goal priority queue.
  (new-model '*indices*)			; Initialize run-time index list as model.
  (new-model '*extant-indices*)			; Initialize  pre-run-time index list as model.
  (setf *previous-mode* nil)			; Specify that there was no previous program run.
  (setf *aqua-window* *window1*)		; Using *window1* is arbitrary (prep sets it properly),
						; but the next 2 functions need it set to something and
						; prep needs the side effect of utils-output-var.
  (f.frame-output-var '*aqua-window*)		; Associate *aqua-window* with all output of the frame system.
  (utils-output-var '*aqua-window*)		; Associate *aqua-window* with all output of utility functs.
  (TSPIN::tspin-output-var '*aqua-window*)	; Associate *aqua-window* with all output of Tale-Spin.
  (prep mode t)					; Prep the windows depending on the mode. t -> do-init = true.
  (toggle-structures-window)			; Currently set memory monitor mode as default.
  
  (if (and (not (action-mode-p mode))		; If not in action mode and 
	   (null *Story-Concepts*))		; the stories are not currently loaded, 
      (init-story *init*))			; then load the default  story.
  (index-xp-prep)				; Index the pre-existing explanations in memory.
  (index-case-prep)				; Index the pre-existing cases in memory.
;  (index-plan-prep)				; Index pre-existing plans in memory.
  (set-model					; Because do-index stores new indexes in *indices*,
    *extant-indices*				; set pre-existing indices to the value of
    (get-model *indices*))			; the ones placed on *indices* by the calls above.
  (set-model *indices* nil)			; Now reset *indices* so that it will only store new indexes.
  (TSPIN::init-tspin from-file?			; Initialize Tale-Spin.
		     recover-file-name)
  (if from-file?
      (with-open-file 
	(seed-file *seed-file-name*
		   :direction :input)
	(read seed-file)			; Skip over tspin seed
	(setf *shuffle-state*			; Create a new random state variable 
	      (make-random-state		; for function shuffle 
		(read seed-file))))		; by reading old value from disk.
      (with-open-file				; In the same file that the random state variable for tspin
	(seed-file *seed-file-name*		; was saved during init-tspin, we save the shuffle seed.
		   :direction :output
		   :if-exists :append)
	(print (setf *shuffle-state*		; Create a new random state variable for function
		     (make-random-state t))	; shuffle.
	       seed-file)))
;mcox 15aug96  (send *original-window* :expose)		; These three lines set the terminal back 
;Commented out 2 lines below. [cox 6mar97]
;  (setf *terminal-io* *original-window*)	; to screen that was current before the 
;  (setf *query-io* *original-window*)		; call of init-aqua.
  (in-package 'METAAQUA)			; Home package is the MetaAQUA package.
  )


;;;
;;; The function init-4-speed is meant to be used at the beginnijg of a long
;;; series of runs. It improves performance somewhat while testing and
;;; debugging.
;;; 
(defun init-4-speed ()
  (setf *Show-Bugs* nil)
  (toggle-structures-window nil)
  )



;;; 
;;; This function creates a hyothetical previous case that helps guide
;;; the reasoner to understand arrest events.
;;; 
;;; ||||| Why does the search method have 2 detection backpointers?  [I
;;; modified the function on 17Dec91 by wrapping the setf  around the
;;; f.unify below. Did this eliminate the above problem?]
;;; 
(defun init-old-case ()
  (let ((old-case
	  (mark-as-believed (f.instantiate-instance bust-act)))
	(i-act
	  (mark-as-believed (f.instantiate-instance interdiction-act)))
	(detect-scene
	  (mark-as-believed (f.instantiate-instance detection))))
    (f.unify (f.chase-path old-case 'goal-scene 'charge)
	     (mark-as-believed (f.instantiate-instance smuggling-crime)))
    (f.unify (f.get i-act 'object)
	     (mark-as-believed (f.instantiate-instance weapon)))
    (f.unify (f.get detect-scene 'method)
	     (mark-as-believed (f.instantiate-instance tip-off)))
    (setf detect-scene (f.unify (f.get i-act 'instrumental-scene)
				detect-scene))
    (f.unify (f.get old-case 'instrumental-scene)
	     i-act)
    ;; ||||| Need to apply the xp so the nodes are bound with those in the interdiction-act.
    (f.put (list (apply-xp (f.instantiate-frame
			     XP-DISABLE-ACT-PRECONDITION)
			   (mark-as-believed
			     (f.make-relation detect-scene *actor-slot*))))
	   old-case *explanations-slot*)
    ;; ||||| If we perform the second f.unify below first, then the first f.unify is superfluous?
    (f.unify (get-scene 1 old-case)
	     (f.get (first (f.get old-case *explanations-slot*)) 'action))
    (f.unify (f.get old-case 'instrumental-scene)
	     (f.get (first (f.get old-case *explanations-slot*)) 'main-action))
    (mark-as-believed old-case)
    old-case)
  )



;;;
;;; The following plan is used during LISP-programming mode.
;;; 
(defun init-programming-plan (mode)
  (let ((old-plan (f.make-new-instance
		    'recursive-programming-plan
		    t
		    *predefined-instance*)))
    (f.unify (f.get old-plan *actor-slot*)
	     *reasoner*)
;    (f.put! 'true.0
;	    old-plan
;	    'success)
    old-plan))



;;; 
;;; The side-effect of function create-windows (i.e., creation of the three
;;; independent windows) used to be performed during compile time at the
;;; defvar statements of *window1* - *window3*. It is now performed here in
;;; order to affect any desired change by the value of the new program
;;; parameter *force-mouse*.
;;; 
(defun create-windows ()
  "Initialize the three windows used by Meta-AQUA."
  (setf *window1* 
	*standard-output*)
;	(if (and (macivory-p)
;		 (not *force-mouse*))
;	    (tv:make-window
;	      'dw:dynamic-lisp-listener
;	      :x 0
;	      :y 0
;	      :width    742
;	      :height   808
;	      :save-bits T)
;	    (tv:make-window
;	      'dw:dynamic-lisp-listener
;	      :edges-from :mouse
;	      :save-bits T))

  (setf *window2*
	*standard-output*)
;	(if (and (macivory-p)
;		 (not *force-mouse*))
;	    (tv:make-window
;	      'dw:dynamic-lisp-listener
;	      :x 743
;	      :y 0
;	      :width    407
;	      :height   808
;	      :save-bits T
;	      :reverse-video-p
;	      nil)
;	    (tv:make-window
;	      'dw:dynamic-lisp-listener
;	      :edges-from :mouse
;	      :save-bits T
;	      :reverse-video-p
;	      nil))
)

  (setf *window3*
	*standard-output*)

;	(if (and (macivory-p)
;		 (not *force-mouse*))
;	    (tv:make-window
;	      'dw:dynamic-lisp-listener
;	      :x 743
;	      :y 0
;	      :width    407
;	      :height   808
;	      :save-bits T)
;	    (tv:make-window
;	      'dw:dynamic-lisp-listener
;	      :edges-from :mouse
;	      :save-bits T)))
;  )


;;;
;;; Function prep prepares the windows and sets their labels depending
;;; on the program mode. 
;;; 
(defun prep (&optional (mode 'read-story) do-init)
  (create-windows)
  (current-window
    *window3*
    (str-concat
      "Internal Structures Window  "
      (if (goal-monitor-mode-p)
	  "- Goal Priority-Queue"
	  "- Memory Monitor"))
    do-init)
  (case mode
    (read-story
      (current-window
	*window1*
	"Meta-AQUA Output Window"
	do-init)
      )
    (act-out-story
      (current-window
	*window1*
	"Robber Window"
	do-init)
      (current-window
	*window2*
	"Police Window"
	do-init))
    (LISP-programming
      (current-window
	*window1*
	"INTRO LISP Learning Window"
	do-init))
    )
  )


;;;
;;; Function index-xp-prep indexes the memory for some common
;;; explanations used in the story understanding domain. This
;;; is performed before any program run by the call of
;;; init-aqua.
;;; 
(defun index-xp-prep ()
  ;; Added 19nov13 for MIDCA example
  (make-predefined-explanation
    'ARSONIST-XP
    'burns
    'nature)
  ;; Added 25sep13 for MIDCA example
;;  (make-predefined-explanation
;;    'FORCED-BY-STATES
;;    'burns
;;    'nature)
  (make-predefined-explanation
    'XP-INJURY-HIT
    'hit
    'person)
  (make-predefined-explanation
    'XP-INJURY-HIT
    'hit
    'adult)
  (make-predefined-explanation
    'XP-INJURY-HIT
    'hit
    'child)
  (make-predefined-explanation
    'XP-TYPICAL-ACTION-ANIMATES->ACTOR
    'sniff
    'dog)
  (make-predefined-explanation
    'XP-DEFENSIVE-BARK
    'dog-barks
    'dog)
  (make-predefined-explanation
    'XP-GOAL-OF-OUTCOME->ACTOR
    'smoke-pipe
    'adult)
  (make-predefined-explanation
    'XP-HUNGRY-BARK
    'seal-barks
    'seal)
  (do-index
    (list
      (f.instantiate-frame IMXP-NOVEL-SITUATION-ALTERNATIVE-REFUTED)
      (f.instantiate-frame IMXP-NOVEL-SITUATION-ALTERNATIVE-REFUTED-NO-ANOMALY))
    'xp-type.0
    (f.instantiate-frame
      `(mentally-initiates
	 (domain
	   (,*value-facet*
	    (not-equal-relation)))
	 (co-domain
	   (,*value-facet*
	    (expectation-failure))))
      *predefined-instance*))
  (do-index
    (list (f.instantiate-frame IMXP-ANOMALY-EXPLAINED))
    'xp-type.0
    (f.instantiate-frame
      `(mentally-initiates
	 (domain
	   (,*value-facet*
	    (not-equal-relation)))
	 (co-domain
	   (,*value-facet*
	    (incorporation-failure))))
      *predefined-instance*))
  (do-index
    (list (f.instantiate-frame IMXP-ANOMALY-EXPLAINED))
    'xp-type.0
    (f.instantiate-frame
      `(mentally-initiates
	 (domain
	   (,*value-facet*
	    (equal-relation)))
	 (co-domain
	   (,*value-facet*
	    (successful-prediction))))
      *predefined-instance*))
  (do-index
    (list
       (f.instantiate-frame IMXP-ANOMALY-AND-BAFFLED)
      (f.instantiate-frame IMXP-BAFFLED-AND-RESOLVED)
;;;       (f.instantiate-frame IMXP-ANOMALY-AND-BAFFLED)
      )
    'xp-type.0
    (f.instantiate-frame
      `(mentally-initiates
	 (domain
	   (,*value-facet*
	    (truth)))
	 (co-domain
	   (,*value-facet*
	    (retrieval-failure))))
      *predefined-instance*))
  )




(defun make-predefined-explanation (xp action actor-type)
  "Create and index a predefined simple XP."
  (let ((explanation-frame
	  (f.instantiate-frame
	    (*FRAME* xp))))
    (do-index
      (list explanation-frame)
      'xp-type.0
      (f.unify
	(f.get
	  explanation-frame
	  *explains-node*)
	(f.make-relation
	  (f.instantiate-frame
	    `(,action
	      (,*actor-slot*
	       (,*value-facet* (,actor-type))))
;;; 	    *predefined-instance
	    )
	  *actor-slot*))))
  )


;;; Wrote these to get through initialization. Where did the code go?
;;; [mcox 15aug96]
(defun init-smuggling-plan (mode)
  nil)
(defun init-bust-plan (mode)
  nil)

;;;
;;; Function index-plan-prep initializes the plans in memory for the
;;; LISP-programming mode and for the action-mode (whatever its name).
;;; [cox 26feb95]
;;; 
(defun index-plan-prep ()
  (let ((old-plan (init-smuggling-plan mode)))	; Initialize the smuggling plan 
    (do-index (list old-plan)			; and index it in memory.
	      'plan-type.0
	      (f.get old-plan 'main-result))
    (setf old-plan (init-bust-plan mode))	; Initialize the bust plan 
    (do-index (list old-plan)			; and index it in memory.
	      'plan-type.0
	      (f.get old-plan 'main-result))
    (setf old-plan				; Initialize the programming plan 
	  (init-programming-plan mode))
    (do-index (list old-plan)			; and index it in memory.
	      'plan-type.0
	      (f.get old-plan 'main-result)))
  )



(defun index-case-prep ()
  "Initializ the pre-existing case in memory."
  (let ((old-case (init-old-case)))		; Initialize the old-case 
    (do-index (list old-case)			; and index it in memory.
	      'case-type.0
	      (f.instantiate-frame
		(f.make-relation
		  (f.get old-case 'goal-scene)
		  *actor-slot*)
		*predefined-instance*)))
  )




(defun init-global-vars (mode)
  (setf *existing-frames* nil)
;;;   (setf *indices* nil)

  (new-queue '*Goal-Queue*)

  (new-model '*World-Model*)
  (new-model '*Reasoning-Model*)
  (new-model '*Story-Tokens*)
  
  (cond ((action-mode-p mode)
	 (new-queue '*Cop-Queue*)
	 (new-queue '*Robber-Queue*)
	 (new-model '*Cop-Reasoning*)
	 (new-model '*Robber-Reasoning*)))

  ;; The following removes previous questions placed in memory.
;  (dolist (each-index *indices*)
;    (apply #'do-index (cons nil each-index)))
  )


;;;
;;; Function init-run is executed at the start of of a program run.
;;; The function initializes the various output windows, the global
;;; variables that represent the reasoning and world models,
;;; current agents, the current story, etc., initializes the agent
;;; goals, clears the screen, and returns control to meta-aqua.
;;; 
(defun init-run (mode previous-mode)
  ;; Temporarily commented out while debugging so we do not loose
  ;; past screen info during clear screen ops.
;;;   (screen-init mode previous-mode)
  (set-run-number *Current-Result-Record*)	; Increment run number.
;  (with-character-style (*Style*)
    (format
      *aqua-window*
      "~%~%Initialize Memory.~%~%")
;    )
  
  (init-global-vars mode)
  (init-goals mode)
  (init-script-applier previous-mode)
  ;;;   (cls)					; Clear screens.
;  (with-character-style (*Style*)
    (format
      *aqua-window*
      "~%~%Begin Meta-AQUA .~%~%")
;    )
  
  )



;;;
;;; Function fill-goal-queue is the routine that actually loads the goal
;;; priority queue. Funny that is uses only global variables ;-)
;;;
;;; This function also sets the story-length field of global
;;; *Current-Result-Record* [cox 27feb95]
;;; 
(defun fill-goal-queue (&optional silent? &aux (counter 0) (hits 0))
  ;; The story concept list must be reversed because goals of equal priority will
  ;; be in the front of the queue, and thus the queue would be initialized backwards.
  (dolist (each-input
	    (reverse *Story-Concepts*))
    (setf counter (+ 1 counter))
    (when (not (member (frame-type
			 (first each-input))
		       '(possess at-location)
		       :test
		       #'equal))
      (setf hits (+ 1 hits))
      (format-if				; Feedback.
	(or (not silent?) *Debug-On*)
	*aqua-window*
	" #~s"
	counter)
      (spawn-new-goal
	*reasoner*
	(make-goal-state			; Make a goal
	  'understands				; to understand
	  *reasoner*				; by the reasoner
	  (return-last-element			; the newly instantiated input
	    (set-model				; that is placed on the list of story tokens.
	      *Story-Tokens*
	      (append
		(get-model *Story-Tokens*)
		(list
		  (instantiate-next
		    (first each-input)
		    (second each-input)
		    ))))))
	'knowledge-acquisition-goal.0		; The goal is to acquire some understanding
	'five.0)				; with an arbitrarily average priority.
      ))
  (format-if					; End of feedback.
    (or (not silent?) *Debug-On*)
    *aqua-window*
    ".~%Number of sentences to understand: ~s.~%"
    hits)
  (set-story-length				; Number of story concepts.
    *Current-Result-Record*
    hits)
  )



;;;
;;; There are two important side-effects in function init-goals.  The call to
;;; function spawn-new-goal places the created goal on the *Goal-Queue*,
;;; whereas the setf (within function fill-goal-queue) places the newly created
;;; token for each sentence on the *Story-Token* list. One final side-effect is
;;; the assignment of the *current-character* global variable.
;;; 
(defun init-goals (mode &optional silent?)
  (case mode
    (read-story
      (setf *current-character*
	    *reasoner*)
      (format-if				; Output some feedback if in debug mode or not silent.
	(or (not silent?) *Debug-On*)
	*aqua-window*
	"~%Number concepts to examine for goal generation: ~s. ~%~%Spawn goal"
	(length *Story-Concepts*))
      (fill-goal-queue silent?)				; Load the goal priority queue.
      )
    (act-out-story
      (setf *current-character*
	    *criminal-actor*)
      (init-smuggler)
      (swap-perspective t)
      (init-police)
      (swap-perspective t)
      )
    (LISP-programming
      (setf *current-character* *reasoner*)
      ;; First set the main learning goal of operationalize programming knowledge from text.
      (spawn-new-goal
	*current-character*
	(make-goal-state
	  'wants
	  *current-character*
	  (f.instantiate-frame
	    `(each-one-greater
	       (,*domain-slot* (,*value-facet* (program-output-list)))
	       (,*co-domain-slot* (,*value-facet* (program-input-list))))
	    ))
	'achievement-goal.0
	'six.0)
      ;; Then create a short instructional text for it to read.
;;;   (fill-goal-queue silent?)
      ))
  )



;;; 
;;; Function instantiate-next is called by function init-goals only.
;;; 
; NOTE we now have to explicitly remove the goals from the queue through some 
; side effect of processing. The items are not automatically removed as with 
; Aqua3.
;;;
;;; Function f.make-new-instance is called with nil notify parameter to
;;; suppress complains when anomalous input is instantiated.
;;; 
;;; An optional parameter of the function is descr. This provides a text
;;; description of the concept. It is placed on the *say-way* property
;;; of the token. See lowlevel.lisp.
;;;
;;; |||||| All input from TSpin will be marked *story-instance* for status
;;; slots.  F.make-new-instance need not do it. But the goals to understand
;;; the input should be marked what?
;;; 
(defun instantiate-next (new-input &optional descr)
  (do-break instantiate-next)
  (let ((token
	  (add-story-status
	    (if (frame-var-p
		  new-input)
		new-input			; Input from Tale-Spin is already instantiated.
		(f.make-new-instance		; Handcoded examples are not.
		  new-input
		  nil)))))
    (if (stringp descr)
	(do-add-say token descr))
    token)
  )




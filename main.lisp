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
;;;;				File: main.lisp
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
;;;; HISTORY
;;;; 
;;;; Changed the format string message for check-type in macro do-run.
;;;; [mcox 2nov05]
;;;; 




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                      ALIAS  FUNCTIONS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;
;;; The macro do-run is used to run spinqua when collecting data. The parameter
;;; run-number is a string such as "first" or "thirtieth". Condition is one of
;;; either the constants 'LG, 'RL, or 'NL. These stand for learning goals,
;;; random learning, and no learning, respectively. NOTE that because this is a
;;; macro, the user does not need to quote the consants (macros do not evaluate
;;; their arguments. Story count is the number of stories spinqua should
;;; generate. Note that for both the RL and NL conditions, the macro will call
;;; function recover-from-disk.
;;; 
(defmacro do-run (run-number &optional (condition 'LG) (story-count -1))
  (check-type run-number string "the run-number argument needs to be a string")
  `(spinqua ,story-count
	    t t
	    ,(if (not (eq condition 'LG))
		 `(quote
		    ,(first
		       (recover-from-disk
			 (str-concat
			   run-number
			   ".goals.recover.file")))))
	    nil nil nil 0 nil 'read-story
	    ,(str-concat
	       run-number
	       (case condition
		 (LG ".goals")
		 (RL ".random")
		 (NL ".nolearn"))
	       ".results.file")
	    ,(str-concat
	       run-number
	       (case condition
		 (LG ".goals")
		 (RL ".random")
		 (NL ".nolearn"))
	       ".recover.file")
	    ))

;;; 
;;; Function ripsau is a synonym for function meta-aqua.
;;; Unlike the meta-aqua function, ripsau has action mode
;;; for planning (signaled by constant act-out-story) as
;;; a default mode.
;;; 
(defun ripsau (&optional
	       automatic?
	       (mode 'act-out-story))
  (meta-aqua automatic? mode)
  )



;;; 
;;; Function intro (Introductory LISP programming) is another synonym for
;;; function meta-aqua.  Unlike the meta-aqua function, ripsau has action mode
;;; for learning from reasoning from reflection on one's own programming errors
;;; (signaled by constant LISP-programming) as a default mode. The mode is
;;; meant to model Mimi Recker's data from Berkeley.
;;; 
(defun intro (&optional
	      automatic?
	      (mode 'LISP-programming))
  (meta-aqua automatic? mode)
  )



;;;
;;; The function print-record will print the results recorded from a Meta-AQUA
;;; run. If called by the user (without specifying a record parameter) it will
;;; print the *Current-Result-Record*; if called by spinqua it will be called
;;; once for each run in the series of Meta-AQUA calls performed by spinqua.
;;; 
(defun print-record  (&optional
		      (record *Current-Result-Record*)
		      (record-number 0)
		      )
  "Function to print a Met-AQUA run result-record."
  (format *aqua-window*
	  "~%~%Record Number ~s - ~%"
	  record-number)
  (format *aqua-window*
	  "Absolute Number ~s - ~%"
	  (result-record-run-number record))
  (format *aqua-window*
	  "~%TSpin Story ID: ~s."
	  (result-record-story-id record))
  (format *aqua-window*
	  "~%Input story length: ~s."
	  (result-record-story-length
	    record))
  (format *aqua-window*
	  "~%Output story length: ~s."
	  (result-record-world-model-length
	    record))
  (format *aqua-window*
	  "~%Matched scripts: ~s."
	  (if (result-record-script-matched-flag
		record)
	      (result-record-matched-scripts-list
		record)
	      'None))
  (format *aqua-window*
	  "~%Anomalies detected: ~s."
	  (or (result-record-anomaly-list
		record)
	      'None))
  (format *aqua-window*
	  "~%Number of questions posed: ~s."
	  (result-record-posed-questions
	    record))
  (format *aqua-window*
	  "~%Number of unanswered questions: ~s."
	  (result-record-unanswered-questions
	    record))
  (format *aqua-window*
	  "~%Number of self-generated answers: ~s."
	  (result-record-self-gen-answers
	    record))
  (format *aqua-window*
	  "~%Number of above answers that were correct: ~s."
	  (result-record-successful-answers
	    record))
  (calc-question-points record)
  (format *aqua-window*
	  "~%Number of learning episodes: ~s."
	  (result-record-learning-episodes
	    record))
  (format *aqua-window*
	  "~%Completion Time: ~s."
	  (result-record-time-to-completion
	    record))
  (if (not (eq 0 (result-record-story-length
		   record)))		; Do not divide by zero.
      (format *aqua-window*
	      "~%Av. Processing Time per Story Concept: ~s.~%~%"
	      (calc-av-proc-time record t)))
  )



;;; 
;;; Automatic Story Generation and Evaluation 
;;;
;;; Function spinqua (tale-SPIN meta-aQUA) is yet another synonym for function
;;; meta-aqua.  Like the meta-aqua function, spinqua has action mode for
;;; reading stories (signaled by constant act-out-story) as a default mode.
;;; These stories, however, are ones automatically spun by the Tale-Spin module.
;;;
;;; Added optional param to manually generate stories and default value for
;;; story-num [mdd27apr94]
;;;
;;; One can pass an optional repeat parameter to the program to run a specific
;;; number of trials. If the number is positive, then Meta-AQUA will be run on
;;; spun stories the given amount of times. If a negative number is passed
;;; instead, the program will be run until the user explicitly stops it with a
;;; keyboard interruption (as explained below).
;;;
;;; Between each run, the program will beep three times and pause for 5
;;; seconds. If before the program continues the user strikes a key (any key
;;; but a space or "y,"), the program will stop with the prompt "Continue?"
;;; Therefore the user can prematurely abort the specified number of runs by
;;; hitting the "n" key (once at the prompt, the user can also use the
;;; <suspend> key to return to the LISP Listener in order to look at or change
;;; varaibles etc.; the <resume> and "y" keys will then continue the program
;;; runs). [cox 29dec94]
;;;
;;; If auto-spin = 'nospin then tale-spin is not used and convert-story is not
;;; called; rather meta-aqua is called with *Story-Concepts* as is. Thus
;;; calling (spinqua 1 'nospin) is like calling (meta-aqua t), but the user
;;; receives some summary statistics and additional information at the end of
;;; the execution. [cox 27jan95]
;;;
;;; If the variable from-file-name is non-nil, then spinqua will attempt to
;;; recover stories from a previously generated run. The from-file-name should
;;; be the name of a recover-file. The list of recovered story identifiers is
;;; placed in auxilliary local variable story-id-list. The repeat number is set
;;; to the length of this list and spinqua is then run once for each recovered
;;; story. [cox 20jun95]
;;;
;;; If pick-yarn? is a number, then spin will be called with that index as if
;;; it had been chosen interactively. The main use of this feature is that if
;;; you want to test with one of the settings in function specify-yarns (file
;;; extensions.lisp), just pass spinqua a negative repeat value and a
;;; legitimate yarn number from specify-yarns as the value of parameter
;;; pick-yarn? Remember that the indexes go from 0 through n-1.
;;; 
(defun spinqua (&optional
		(repeat 1)			; Repeat counter.
		(auto-spin t)			; Flag to control automatic story generation.
		(automatic? t)			; Flag to control user interaction.
		story-num			; Identifier if rerunning a previous story.
		pick-yarn?			; Flag for specifying a particular type of yarn to spin.
		from-file-name			; non-nil -> read from specific recover file.
		person-num			; Can specify an actor to spin stories about
		(problem-num 0)			; with specified or default problems.
		suppress?			; Flag for suppressing meta-aqua process.
		(mode 'read-story)		; Meta-AQUA mode.
		(results-file-name		; Save results to this file.
		  "results.file")
		(recover-file-name		; Save crash recovery info to this file.
		  "recover.file")
		&aux
		story-id-list			; List of recovered stories from from-file-name
		)
  "Tale-Spin auto-generates Meta-AQUA input."
  (labels (					; Functions local to spinqua.
	   (print-results-list		       	; Local function to print all records.
	      (eval-results)
	     (format *aqua-window*
		     "~%~%Results of runs:~%")
	     (let ((rec-num 0))
	       (dolist (each-result eval-results)
		 (setf rec-num (+ 1 rec-num))
		 (print-record
		   each-result rec-num))))
	   (number-understood			; Local function to count the number of stories
	      (result-list)			; understood (was at least one script matched?)
	     (cond
	       ((null result-list) 0)
	       (t
		(+
		  (if
		    (result-record-script-matched-flag
		      (first result-list))
		    1 0)
		  (number-understood
		    (rest result-list))))))
	   (print-results			; Local function to summarize results.
	      (number-executed
		script-match-number)
	     (print-results-list
	       (reverse *Eval-Results*))
	     (format *aqua-window*
		     "~%~%Number of runs: ~s.~%"
		     number-executed)
	     (format *aqua-window*
		     "Total number of matched scripts: ~s.~%"
		     script-match-number)
	     (format-if
	       (not (eq 0 number-executed))
	       *aqua-window*
	       "Percentage of understood stories: ~s.~%"
	       (if (eq 0 number-executed)
		   'n/a
		   (*  100.00
		       (/ (number-understood
			    *Eval-Results*)
			  number-executed))))
	     (format *aqua-window*
		     "Scripts matched: ~s.~%"
		     (or *Matched-Scripts*
			 'None))
	     ))
    (pre-init results-file-name)		; Miscellaneous initialization on globals etc.
    (if from-file-name				; If non-nil file name
	(setf repeat				; then repeat number is number of stories
	      (length				; recovered from disk and placed 
		(setf story-id-list		; into local variable story-id-list
		      (tspin::recover-from-disk
			from-file-name)))))
    ;; Spinqua main program loop.
    (do ((number-executed			; Number of stories processed or runs.
	   0 (+ 1 number-executed))
	 (script-match-number			; Cummulative number of scripts matched; it is 
	   0 script-match-number))		; actually changed by function identify-question.
	;; Stopping conditions.
	((or (eq repeat 0)			; If either finished with the number of desired repetitions,
	     (user-stops-early-p))		; Let the user end program prematurely.
	 (print-results number-executed		; then stop execution and print the results.
			script-match-number)
 	 (init-script-applier *Previous-Mode*)	; Re-initialize the *Script-List* for next set of runs.
	 'Spinqua-Done)				; Return dummy symbol.
      ;; Do loop body
      (if					; If
;	(eq (mod
;	      (+ 1 (length *Eval-Results*))	; Run number
;	      10)				; a multiple of 10
;	    0)
	(eq 12 (length *Eval-Results*))
	(excl:gc)			        ; then garbage collect.
;	(scl:gc-immediately)			; Was Symbolics code [mcox 15aug96]
	)
      (init-result-record)			; Initialize global results variable for this story.
      (format *aqua-window*			; Print the run number.
	      "~%~%Run number ~s . . .~%"
	      (+ 1 (length *Eval-Results*)))
      (when from-file-name
	(setf story-num (first story-id-list))
	(setf story-id-list (rest story-id-list)))
      (multiple-value-setq
	;; Meta-aqua may change script-match-number
	;; whereas, spinqua-loop-body may change story-num.
	(script-match-number story-num)
	(spinqua-loop-body			; The major part of the spinqua function.
	  pick-yarn?
	  story-num
	  auto-spin
	  person-num
	  problem-num
	  automatic?
	  mode
	  script-match-number
	  suppress?
	  recover-file-name))
      (save-record results-file-name)		; Save results to disk.
      (setf repeat (- repeat 1))		; Decrement repeat counter.
      ))
  )



;;; 
;;; Preliminary initialization before spinqua's main do-loop.
;;; 
(defun pre-init (results-file-name)
  (setf *Matched-Scripts* nil)			; Initialize global
  (setf *Eval-Results* nil)			; Initialize global
  (tspin::init-file				; Initialize a file with which to save results.
    results-file-name)
  (terpri *aqua-window*)			; Begin with a carriage return.
  )



;;;
;;; Function spinqua-loop-body returns two values: It returns the value
;;; returned by the call of function meta-aqua, and it returns the possibly
;;; changed value of story-num. The second return value is changed if it was
;;; non-nil to begin with.  The value returned will (always) be nil.
;;; 
(defun spinqua-loop-body (pick-yarn?
			  story-num
			  auto-spin
			  person-num
			  problem-num
			  automatic?
			  mode
			  script-match-number
			  suppress?
			  &optional
			  (recover-file-name
			    "recover.file"))
  ;; Spin a story
  (cond (pick-yarn?
	 (tspin::spin nil nil nil nil		; the type of which the user specifies,
		      pick-yarn?
		      recover-file-name))
	(story-num
	 (tspin::re-run-story story-num)	; or one that has been previously spun,
	 ;; So subsequent stories will be different.
	 (setf story-num nil))
	((eq auto-spin t)
	 (tspin::aspin				; or one that is randomly spun,
	   nil
	   recover-file-name))
	((null person-num)
	 (if (not (eq auto-spin
		      'nospin))
	     (tspin::mspin			; or one for which the user specifies a character & problem,
	       nil nil nil
	       recover-file-name)))
	(t
	 (tspin::mspin				; or one using a particular character and problem.
	   nil
	   person-num
	   problem-num
	   recover-file-name))
	)
  (if (not (eq auto-spin
	       'nospin))
      (setf *Story-Concepts*			; Will pass to Meta-AQUA
	    (tspin::convert-story)))		; a converted version of the story.
  ;; Return both the value returned by meta-aqua (which may
  ;; change the script-match-number) and the story-num.
  (values
    (meta-aqua automatic?			; Run Meta-AQUA on the story.
	     mode				; The meta-aqua function will return (as the value
	     script-match-number		; of thus function) the new script-match-number
	     suppress?
	     t					; t -> called by program rather than user.
	     t)					; t -> remove unanswered questions at end of run.
    story-num)
  )



;Original function
;(defun spinqua (&optional
;		(automatic? 'semi)
;		(mode 'read-story)
;		story-num) 
;  (if story-num
;      (TSPIN::re-run-story story-num)
;      (TSPIN::aspin))
;  (setf *Story-Concepts*
;	(TSPIN::convert-story))
;  (meta-aqua automatic? mode)
;  )



(defvar *user-intervention-permision-flag* nil
  "If non-nil then user may stop set of runs early when using function spinqua.")


;;;
;;; User function that toggles the global permission flag. A user may suspend
;;; operation of the program and then call this function in order to stop the
;;; execution of spinqua early. Spinqua will still print the results at the end
;;; of the execution this way.
;;; 
(defun toggle-permission-flag ()
  (setf
    *user-intervention-permision-flag*
    (not *user-intervention-permision-flag*))
  )


(defun user-stops-early-p (&optional
			   (permision-flag
			     *user-intervention-permision-flag*)
			   (naptime 5))		; Five second default pause.
  "Predicate to beep & allow abort w/keystroke of n."
  (if permision-flag
      (or 
	(beep)(beep)(beep)			; announce done (returns 3 nils)
	(sleep naptime)				; wait for response (returns nil)
	(if (listen)				; or if the keyboard has been struck 
	    (not (y-or-n-p			; let the user end the program
		   "~%Continue ? ")))))
  )



;;; 
;;; User function to test the old set of stories. Useful for making sure that
;;; no side-effect has crept into the execution of the program.
;;; 
(defun standard-test (&optional
		      full-test?		; t -> run all stories.
		      (just-rebooted?		; t -> No need to perform memory reset.
			(null *previous-mode*)))
  "Test meta-aqua with hand-coded set of stories."
  (if (not just-rebooted?)
      (reset-memory t))				; Make sure clean memory and default story loaded.
  ;; Test the dog barking stories.
  (meta-aqua t)					; Dog barks at luggage.
  (when full-test?
    (if (user-stops-early-p)
	(return-from standard-test))
    (set-story-4)				; Dog barks at garbage pail.
    (meta-aqua t))
  (if (user-stops-early-p)
      (return-from standard-test))
  (set-story-5)					; Dog barks at laundry pile.
  (meta-aqua t)
  (if (user-stops-early-p)
      (return-from standard-test))
  (set-story-7)					; Dog barks at compost pile.
  (meta-aqua t)
  (when full-test?
    (if (user-stops-early-p)
	(return-from standard-test))
    (set-story-10)				; Dog barks at person who throws ball at it.
    (meta-aqua t)
    ;; Test the hitting stories.
    (if (user-stops-early-p)
	(return-from standard-test))
    (set-story-6)				; Player hits the ball. (parallel to default story)
    (meta-aqua t)
    (if (user-stops-early-p)
	(return-from standard-test))
    (meta-aqua t)				; Test  learning with same story.
    (if (user-stops-early-p)
	(return-from standard-test))
    (set-story-8)				; Person hits a another person. (parallel to story 10)
    (meta-aqua t)
    (if (user-stops-early-p)
	(return-from standard-test))
    (set-story-9)				; Person hits a lamp. (parallel to story 5)
    (meta-aqua t)				; but no forgotten explanation. baffled&resolved.
    )
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;      MAIN CONTROL  -  THE META-AQUA FUNCTION
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; 
;;; Main Program Function.
;;;
;;; Function meta-aqua is the outer-most function (not including aliases above)
;;; of the Meta-AQUA program.  Meta-AQUA has three modes in which it can be run
;;; (and is determined by the mode parameter). The standard mode is for story
;;; understanding and is signaled by the constant 'read-story, whereas the
;;; constant 'act-out-story signals problem solving mode.  A new mode is
;;; 'LISP-programming used for modeling Mimi Recker's data. The function first
;;; initializes the system, enters a main loop that process input until no
;;; remaining goals exist to be achieved, then exists after displaying both the
;;; world model of the input and the mental model of the processing of the
;;; input.
;;;
;;; The optional parameter automatic? can be either nil, t or 'semi. It
;;; controls the user-prompts for whether to continue or not at the end of each
;;; cycle thru the main do-loop. At non-automatic speed (automatic? = nil),
;;; Meta-AQUA will prompt the user whether to continue both after processing a
;;; previous sentence and after presenting the subsequent sentence on the
;;; screen. Semi(automatic) will not stop between finishing the last sentence
;;; and presenting the next for display. At fully automatic (Automatic? = t)
;;; speed, Meta-AQUA will not prompt the user whether or not to process any
;;; sentence. It stops only to prompt whether to process a question of which it
;;; is reminded or whether to use an explanation is has retrieved.
;;;
;;; Meta-AQUA actually returns a value. It returns the number of scripts
;;; matched while processing a story. Because spinqua can call Meta-AQUA
;;; numerous times during evaluation runs, the old count is passed in as the
;;; parameter script-match-number so that Meta-AQUA can maintain the running
;;; count and return it to spinqua.
;;; 
(defun meta-aqua (&optional
		  automatic?			; Automatic processing is not a default.
		  (mode 'read-story)		; Default mode of reading stories.
		  (script-match-number 0)	; Number of scripts matched from previous runs.
		  suppress?			; Flag for suppressing the Meta-AQUA process.
		  called-by-spinqua?		; Flag used to effect output of say-input.
		  remove-unanswered-questions	; If t, then remove remaining questions after
		  				; processing a story.
		  &aux
		  (start-time
		    (get-universal-time))	; Used to time the run.
		  )
  (if suppress?
      (return-from meta-aqua
	script-match-number))
  (init-run mode *Previous-Mode*)		; Initialize program in the given mode.
  ;; Main program control loop.
  (do ((next-goal (front-of *Goal-Queue*)
		  (front-of *Goal-Queue*)))
      ;; Stopping Conditions and cleanup.
      ((or (if (action-mode-p mode)		; We are done if either 
	       (and (empty-queue-p		; a program goal queue is empty
		      *Cop-Queue*)
		    (empty-queue-p
		      *Robber-Queue*))
	       (empty-queue-p *Goal-Queue*))
	   (user-quits-p automatic?		; or the user signals to quit.
			 next-goal))
       (format
	 *aqua-window*				; If so, 
	 (str-concat
	   "~%Done.~% ~%Time to "		; signal program completion 
	   "completion: ~s minutes.~%~%")	; show elapsed execution time,
	 (set-time-to-completion		; Compute the time it took 
	   *Current-Result-Record*		; to complete one run of Meta-AQUA.
	   start-time))
       (if (or (goal-monitor-mode-p)
	       (memory-monitor-mode-p))
	   (display-models mode automatic?))	; display the domain and mental models,
       (setf *Previous-Mode* mode)		; set previous mode for next invocation of init-run,
       (set-world-model-length
	 *Current-Result-Record*		; set the world-model-length field of the current result,
	 *World-Model*)
       (setf *Eval-Results*			; update the global results of all runs in this series
	     (cons *Current-Result-Record*	; by adding the latest result record,
		   *Eval-Results*))
       (if remove-unanswered-questions		; if this flag argument is t, 
	   (set-unanswered-questions		; then record the result of removing any old questions
	     *Current-Result-Record*	        ; in the current result record, therefore,
	     (remove-old-questions)))	        ; not allowing questions to interfere with further stories,
       script-match-number)			; and finally, return the running match number.
    ;; Main body.
    (cond ((is-new-goal-p next-goal)		; New goal is one that is not a subgoal.
	   (update-models next-goal)		; For new goals, update world & mental models
	   (say-input				; and print the new input,
	     (goal-state next-goal)
	     called-by-spinqua?))
	  (t
	   (say-sub-goal			; otherwise display subgoal info.
	     next-goal)))
    (cond ((is-goal-type-p			; If the current goal 
	     'world-goal next-goal)		; is a world-goal
	   (do-plan next-goal))			; then plan to achieve it.
	  ((is-goal-type-p			; otherwise, if the current goal 
	     'knowledge-goal next-goal)		; is a knowledge-goal
	   (setf script-match-number
		 (do-understand			; then perform understanding 
		   next-goal			; to achieve the knowledge.
		   script-match-number
		   automatic?))))
    (if (action-mode-p mode)			; When in action mode, agents perspectives 
	(swap-perspective))			; are swapped for the next round.
    )
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   MODEL SUPPORT FUNCTIONS FOR META-AQUA FUNCTION
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; The model data-structure is included in file goal-q.lisp, where it supports
;;; the priority queue data-structure. Models are used for global variables
;;; instead of lists because of the need to update references during frame
;;; unification. I believe that discussion of this issue can be found in the
;;; frame.lisp file.
;;; 

;;; 
;;; When new goals are encountered (those not generated by an extant
;;; reasoning process, such as the goal to understand a new story 
;;; concept or understand an event in the world) update-models is 
;;; called. It adds the new goal state to the world model and hooks the
;;; Trace-Meta-XP associated with the new goal into the reasoning model 
;;; that records the processing that will produce the changes in the
;;; world model.
;;;
(defun update-models (goal &aux (unique nil))
  (do-break update-models)
  (set-model
    *Reasoning-Model*
    (cons (f.get goal 'mxp)
	  (get-model *Reasoning-Model*)))
  ;; ||||| If it is a wanting-goal should we not also  add it to the world?
  (if (is-goal-object-p 'understands goal)
      (add-to-model
	(goal-state goal)
	*World-Model*
	unique))
  )



;;;
;;; The function display-models prints out the story representation and
;;; reasoning representation computed by Meta-AQUA at the completion of a
;;; processing run. 
;;; 
(defun display-models (&optional
		       (mode 'read-story)
		       (automatic? t))
  (let ((previous-window *aqua-window*))
    ;; Change to structure window.
    (current-window *window3*)
    (print-cycle-division "mental models")
    ;; Show constructed models.
    (print-model
      *aqua-window*
      *World-Model*
      "WORLD MODEL")
    (cond ((action-mode-p mode)
	   (print-model
	     *aqua-window*
	     *Robber-Reasoning*
	     "ROBBER REASONING MODEL")
	   (print-model
	     *aqua-window*
	     *Cop-Reasoning*
	     "COP REASONING MODEL"))
	  (t					; Otherwise it is read-story mode.
	   (print-model
	     *aqua-window*
	     *Reasoning-Model*
	     "REASONING MODEL")))
    ;; |||||| Should perform the following only when learning occurs. 
    (if (or automatic?
	    (y-or-n-p  "~%Print IMXP ? "))
	(f.pprint
	  (first			; |||||Hack to know where this is.
	    (get-model *Reasoning-Model*))))
  
    ;; Change back to output window.
    (current-window previous-window))
  )





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
;;;;				File: eval.lisp
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
;;                       GLOBAL VARIABLES
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *Matched-Scripts* nil
  "Global list of matched scripts during story understanding.")



(defstruct result-record
  (time-to-completion nil)			; execution time of the run
  (matched-scripts-list nil)			; list of scripts matched in this story
  (script-matched-flag nil)			; t -> script applier successful
  (story-id nil)				; unique identifier of story assigned by TSpin 
  (story-length 0)				; number of concepts in story stream
  (anomaly-list nil)				; list of detected anomalies
  (run-number 0)				; number since system init, independent of current run series
  (world-model-length 0)			; number of concepts in final representation of the story
  (unanswered-questions 0)			; number of questions remaining unanswered at end of run
  (posed-questions 0)				; total number of questions posed during the run
  (self-gen-answers 0)				; total number of answers (hypotheses) generated
  (successful-answers 0)			; self-generated answers that are correct
  (learning-episodes 0)				; Number of learning episodes in run.
  (script-inferences 0)				; Number of additional understanding goals due to script
  						; inferences.
  )


;;;
;;; *Eval-Results* is a list of records, one for each run during evaluation.
;;; Each record is a structure of type result-record having the following
;;; fields:  time-to-completion matched-scripts-list script-matched-flag
;;; story-id story-length anomaly-list run-number world-model-length
;;; unanswered-questions. See comments on the definition of result-record above
;;; for semantics. The results list is in reverse order. Therefore, while a
;;; run is being executed, the first record in *Eval-Results* is the results
;;; from the last run, whereas, *Current-Result-Record* contains the current
;;; one.  *Eval-Results* is updated at the end of function Meta-AQUA execution.
;;; The results are printed at the end of a series of runs by the function
;;; local to spinqua, print-results-list.
;;; 
(defvar *Eval-Results* nil
  "Global list of results obtained for each run in a series of runs.")


;;;
;;; The *Current-Result-Record* global variable contains information for the
;;; Meta-AQUA run being executed by the spinqua function. The record is a
;;; structure of type result-record.
;;; 
;;; The time-to-completion field is set by function meta-aqua in the locally
;;; defined function process-completion-time.  Both matched-scripts-list and
;;; script-matched-flag fields are set by function scriptify. The story-id
;;; field is set by function tspin::print-story-params. The story-length field
;;; is set by function fill-goal-queue. Anomaly-list set by function
;;; interesting-p. The run-number is set at the beginning of a call to function
;;; meta-aqua by function init-run, and both the world-model-length and
;;; unanswered-questions are set my meta-aqua itself just before it returns
;;; control. Finally, posed-questions is set by the function index-question,
;;; self-gen-answers is set by the function h.runstrategy, successful
;;; answers is set by function try-matching2, and learning-episodes is set by
;;; the function learn.
;;;
;;; |||||| Matched-scripts-list and anomaly-list should be model data
;;; structures so there their values will be updated if any component is
;;; unified with other frames.
;;; 
(defvar *Current-Result-Record* (make-result-record)
  "fields: time-to-completion matched-scripts-list script-matched-flag story-id 
    story-length anomaly-list run-number world-model-length unanswered-questions
    posed-questions self-gen-answers successful-answers learning-episodes")


;;;
;;; Function init-result-record initializes the global variable
;;; *Current-Result-Record* above. The run-number field, however, is left
;;; alone. The meta-aqua function will update the run-number at run-time. It is
;;; persistent across runs rather than being re-set each run to a newly
;;; initialized value.
;;; 
(defun init-result-record ()
  "Initializes the global variable *Current-Result-Record*."
  (setf
    *Current-Result-Record*
    (make-result-record
      :run-number
      (result-record-run-number
	*Current-Result-Record*)))
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             RECORD VALUE ASSIGNMENT FUNCTIONS 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun set-time-to-completion (record start-time)
  (setf
    (result-record-time-to-completion
      record)
    (/ (- (get-universal-time)
	  start-time) 60.0))			; Compute in minutes, rather than seconds.
  )

(defun set-matched-scripts-list (record found-script)
  (setf
    (result-record-matched-scripts-list
      record)
    (cons
      found-script
      (result-record-matched-scripts-list
	record)))
  )

(defun set-script-matched-flag (record)
  (setf
    (result-record-script-matched-flag
      record)
    t)
  )



(defun set-story-id (record story-id)
  (setf
    (result-record-story-id
      record)
    story-id)
  )




(defun set-story-length (record hits)
  (setf
    (result-record-story-length
      record)
    hits)
  )




(defun set-anomaly-list (record anomaly-frame)
  (setf
    (result-record-anomaly-list
      record)
    (cons anomaly-frame
	  (result-record-anomaly-list
	    record)))
  )




(defun set-run-number (record &optional forced-number)
  (setf
    (result-record-run-number
      record)
    (or forced-number
	(+ 1 (result-record-run-number
	       record))))
  )



(defun set-world-model-length (record world-model)
  (setf
    (result-record-world-model-length
      record)
    (length
      (remove-duplicates (get-model world-model))))
  )



(defun set-unanswered-questions (record question-list)
  (setf
    (result-record-unanswered-questions
      record)
    question-list)
  
  )


(defun set-posed-questions (record)
  (setf
    (result-record-posed-questions
      record)
    (+ 1 (result-record-posed-questions
	   record)))
  )



(defun set-self-gen-answers (record)
  (setf
    (result-record-self-gen-answers
      record)
    (+ 1 (result-record-self-gen-answers
	   record)))
  )



(defun set-successful-answers (record)
  (setf
    (result-record-successful-answers
      record)
    (+ 1 (result-record-successful-answers
	   record)))
  )


(defun set-learning-episodes (record)
  (setf
    (result-record-learning-episodes
      record)
    (+ 1 (result-record-learning-episodes
	   record)))
  )


(defun set-script-inferences (record)
  (setf
    (result-record-script-inferences
      record)
    (+ 1 (result-record-script-inferences
	   record)))
  )






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                    CALCULATION  FUNCTIONS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun plot-av-time-4-anomalous-stories
       (&optional
	(plot-file-name
	  "plot.file"))
  (plot-av-proc-time
    plot-file-name
    #'sixth)
  )


(defun plot-av-time-4-non-anomalous-stories
       (&optional
	(plot-file-name
	  "plot.file"))
  (plot-av-proc-time
    plot-file-name
    #'(lambda (record)
	(null (sixth record))))
  )



(defun plot-av-proc-time (&optional
			  (plot-file-name
			    "plot.file")
			  predicate		; if non-nil, then each-record must pass test before save.
			  (result-records *Eval-Results*)
			  &aux
			  (counter 0)
			  av-time)
  "Write average results over time to disk."
  (with-open-file 
    (plot-file plot-file-name
	       :direction :output
	       :if-exists :new-version
	       :if-does-not-exist :create))
  (with-open-file 
    (plot-file plot-file-name
	       :direction :output
	       :if-exists :append)
    (dolist (each-record
	      (reverse result-records))
      (setf counter (+ 1 counter))
      (if (or (null predicate)
	      (funcall
		predicate each-record))
	  (setf av-time
		(calc-av-proc-time
		  each-record
		  t)))
      (if av-time
	  (format
	    plot-file
	    "~%~s ~s"
	    counter
	    av-time))))
  plot-file-name
  )




(defun plot-question-behavior
       (&optional
	(plot-file-name
	  "plot.file")
	(result-records *Eval-Results*)
	cumulative?
	&aux
	(counter 0)
	(points 0))
  "Write questioning behavior to disk."
  (with-open-file 
    (plot-file plot-file-name
	       :direction :output
	       :if-exists :new-version
	       :if-does-not-exist :create))
  (with-open-file 
    (plot-file plot-file-name
	       :direction :output
	       :if-exists :append)
    (dolist (each-record
	      (reverse result-records))
      (setf counter (+ 1 counter))
      (if (setf points
		(+ (calc-question-points
		     each-record
		     t)
		   (if cumulative?
		       points
		       0)))
	  (format
	    plot-file
	    "~%~s ~s"
	    counter
	    points))))
  plot-file-name
  )




(defun plot-absolute-question-behavior
       (&optional
	(plot-file-name
	  "plot.file")
	(result-records *Eval-Results*)
	cumulative?
	&aux
	(counter 0)
	(points 0))
  "Write number of correct answers to disk."
  (with-open-file 
    (plot-file plot-file-name
	       :direction :output
	       :if-exists :new-version
	       :if-does-not-exist :create))
  (with-open-file 
    (plot-file plot-file-name
	       :direction :output
	       :if-exists :append)
    (dolist (each-record
	      (reverse result-records))
      (setf counter (+ 1 counter))
      (if (setf points
		(+ (calc-absolute-question-points
		     each-record
		     t)
		   (if cumulative?
		       points
		       0)))
	  (format
	    plot-file
	    "~%~s ~s"
	    counter
	    points))))
  plot-file-name
  )




(defun plot-anomaly-rate (&optional
			  (plot-file-name
			    "plot.file")
			  (result-records *Eval-Results*)			  
			  &aux
			  (counter 0)
			  av-time)
  "Write ratio of anomalies to input concepts (scaled by 5)."
  (with-open-file 
    (plot-file plot-file-name
	       :direction :output
	       :if-exists :new-version
	       :if-does-not-exist :create))
  (with-open-file 
    (plot-file plot-file-name
	       :direction :output
	       :if-exists :append)
    (dolist (each-record
	      (reverse result-records))
      (setf counter (+ 1 counter))
      (setf av-time
	    (calc-anomaly-rate
	      each-record
	      t))
      (if av-time
	  (format
	    plot-file
	    "~%~s ~s"
	    counter
	    av-time))))
  plot-file-name
  )



;;;
;;; One point for posing a question, one for providing an answer, and one for a
;;; correct answer. Eventually the function should discriminate between correct
;;; answers and correct answers the system knows is correct. [28july95]
;;; 
(defun calc-question-points
       (result-record
	&optional
	silent
	&aux
	(points
	  (+ (result-record-posed-questions
	       result-record)
	     (result-record-self-gen-answers
	       result-record)
	     (result-record-successful-answers
	       result-record))))
  "Calculate points for questioning behavior."
  (if (not silent)
      (format
	*aqua-window*
	"~%Number of questioning points: ~s.~%~%"
	points))
  points
  )




(defun calc-absolute-question-points
       (result-record
	&optional
	silent
	&aux
	(points
	  (result-record-successful-answers
	       result-record)))
  "Calculate absolute points for questioning behavior."
  (if (not silent)
      (format
	*aqua-window*
	"~%Number of absolute questioning points: ~s.~%~%"
	points))
  points
  )




;;;
;;; Function calc-anomaly-rate calculates the average rate of anomalies per
;;; input story concept.  Note that the number of input concepts is not the
;;; same as the number generated by Tale-Spin.  Some concepts (e.g.,
;;; at-location and possess frames) are filtered out during preprocessing by
;;; function fill-goal-queue.
;;; 
(defun calc-anomaly-rate (result-record
			  &optional
			  silent
			  &aux
			  dividend)
  "Calculate rate of anomalies per input concept."
  (when (not (eq 0				; Do not divide by zero.
		 (result-record-story-length
		   result-record)))
    (setf dividend
	  (/ (length
	       (result-record-anomaly-list
		 result-record))
	     (result-record-story-length
	       result-record)))
    (if (not silent)
	(format
	  *aqua-window*
	  "~%Rate of anomalies per Input Concept: ~s.~%~%"
	  dividend))
    (* 15.0 dividend))
  )


;;;
;;; Function calc-av-proc-time calculates the average time spent processing
;;; each story.
;;; 
(defun calc-av-proc-time (result-record
			  &optional
			  silent
			  &aux
			  dividend)
  "Calculate average processing time per input concept."
  (when (not (eq 0				; Do not divide by zero.
		 (result-record-story-length
		   result-record)))
    (setf dividend
	  (/ (result-record-time-to-completion
	       result-record)
	     (+
	       (result-record-story-length
		 result-record)
	       (result-record-script-inferences
		 result-record))))
    (if (not silent)
	(format
	  *aqua-window*
	  "~%Av. Processing Time per Input Concept: ~s.~%~%"
	  dividend))
    dividend)
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                       USER FUNCTIONS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;
;;; Because of the optional parameter record-list and the fact the the plot
;;; function returns the record list from the read-records call, one can
;;; perform the following:
;;;
;;; (plot "fourth" nil 'anomaly nil (plot "fourth" ))
;;; 
;;; or
;;; 
;;; (plot "twentysecond" t 'anomaly nil (plot "twentysecond" t 'time))
;;;
;;; or
;;; 
;;; (plot "twentyninth" 'nolearn 'anomaly nil
;;;    (plot "twentyninth" 'nolearn 'absolute t
;;;      (plot "twentyninth" 'nolearn 'absolute nil 
;;;        (plot "twentyninth" 'nolearn 'question t
;;;          (plot "twentyninth" 'nolearn)))))

;;; Parameter file-num must be a string. Optional parameter is-random? can be
;;; equal to the constant 'nolearn, nil or otherwise non-nil (should be t).
;;; Optional parameter plot-type can be equal to the constant 'time (the
;;; default) or the constant 'anomaly.
;;;
;;; 
;;; 
(defun plot (file-num
	     &optional
	     is-random?
	     (plot-type 'question)
	     cumulative?			; Valid only when plot-type = 'question
	     record-list
	     &aux
	     return-val)
  (let ((plot-file-name
	  (str-concat file-num
		      (if is-random?
			  (if (eq is-random? 'nolearn)
			      ".nolearn."
			      ".random.")
			  ".goals.")
		      "plot-"
		      (if (eq plot-type 'time)
			  "time"
			  (if (eq plot-type 'question)
			      (if cumulative?
				  "question-c"
				  "question")
			      (if (eq plot-type 'absolute)
				  (if cumulative?
				      "abs-c"
				      "abs")
				  "anomaly")))
		      ".file"))
	(results-file-name
	  (str-concat file-num
		      (if is-random?
			  (if (eq is-random? 'nolearn)
			      ".nolearn."
			      ".random.")
			  ".goals.")
		      "results.file")))
    (case plot-type
      (time
	(plot-av-proc-time
	  plot-file-name
	  nil
	  (setf
	    return-val
	    (or
	      record-list
	      (read-records results-file-name
			    t)))))
      (anomaly
	(plot-anomaly-rate
	  plot-file-name	 	
	  (setf
	    return-val
	    (or
	      record-list
	      (read-records results-file-name
			    t)))))
      (question
	(plot-question-behavior
	  plot-file-name	 	
	  (setf
	    return-val
	    (or
	      record-list
	      (read-records results-file-name
			    t)))
	  cumulative?))
      (absolute
	(plot-absolute-question-behavior
	  plot-file-name	 	
	  (setf
	    return-val
	    (or
	      record-list
	      (read-records results-file-name
			    t)))
	  cumulative?))
    ))
  return-val
  )


(defun read-records (&optional
		     (results-file-name
		       "results.file")
		     (stream *aqua-window*)
		     &aux
		     record-list)
  "Return a list of result-records from disk."
  (with-open-file 
    (input-file results-file-name
		:direction :input)
    (loop
      (let ((next-record
	      (read 
		input-file nil 
		TSPIN::*end-of-file* nil)))
	(cond ((TSPIN::eof-p next-record)
	       (format
		 stream
		 "~%The following record(s) was recovered from disk: ~s."
		 record-list)
	       (return record-list))
	      (t 
	       (setf record-list
		     (cons next-record
			   record-list)))))))
  )



(defun save-record (&optional
		    (results-file-name
		      "results.file")
		    (record *Current-Result-Record*))
  "Save the given result record to disk."
  (with-open-file 
    (results-file results-file-name
		  :direction :output
		  :if-exists :append
		  :if-does-not-exist :error)
    (print record
	   results-file)
    )
  )




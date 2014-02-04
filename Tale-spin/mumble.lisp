;;; -*- Mode: LISP; Syntax: Common-lisp; Package: TALE-SPIN; Base: 10 -*-

;;; ===========================================================================
;;; TaleSpin
;;; Copyright (C) 1987 James R. Meehan
;;; ===========================================================================

(in-package :tspin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	    Elvis World and the Tale-Spin Story Generation Subsystem
;;;;				 for Meta-AQUA
;;;;
;;;;	              Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;			       File: mumble.lisp
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





(defun init-gen ()
  (setq *me* nil *you* nil *him* nil *her* nil))


;;;
;;; The random odds determine how often random events are inserted into the
;;; story. [cox 7aug93]
;;;
;;; Make it 120 to guarantee that random events will occur at every possible
;;; chance. [cox 15sep93]
;;; 
(defparameter *random-odds* 10)	;Changed to parameter from variable.


;;; 
;;; To turn on random events being inserted into the story, make this flag t.
;;; [cox 11aug93]
;;;
(defparameter *allow-random-events* nil)


;;; 
;;; Program parameter to force function say to pause before deciding on random
;;; event insertion.  If user then strikes 't random event will be inserted
;;; into story. [cox 12aug93]
;;;
(defparameter *request-for-random-event* nil
  "If t, lets user insert random events into story at will.")


;;;
;;; Instead of inserting random events when the user wants, more control is
;;; allowed by requesting specific events to be inserted. Because the
;;; add-random-event function has been modified with an optional parameter to
;;; pass a particular event, one can set the *request-for-random-event* flag to
;;; t and then set *requested-event* to some quoted form (unevaluated) that can
;;; be evaluated to produce a cd. The add-random-event function will then
;;; insert this event when the next action (rather than state) rolls around.
;;; One should also at this time set *no-prompt* to t so that the user does not
;;; have to provide additional intervention. After insertion, the function say
;;; will reset these parameters to normal values. See function s-jonesing for
;;; an example. [cox 9feb95]
;;; 
(defvar *requested-event* nil
  "A unevaluated form that will produce a cd.")


(defvar *no-prompt* nil
  "Inhibit interactive i/o when inserting random events.")



;  say prints a CD as an English sentence.  If CD is an mloc of the
;  world, then only the fact itself is said, otherwise the whole mloc
;  is used.  The original CD is returned.  say1 is called with the 
;  infinitive flag off and the say-subject flag on. [icu]
;
;;; NOTE that the comment above looks quite out of date. Update later. 
;;; [cox 7aug93]
;;;
;;; Say has been modified to control the insertion of random events. By setting
;;; the parameter *allow-random-events* to nil, the user can turn off all
;;; insertion of random events. By setting *request-for-random-event* to t, the
;;; user can insert random events at any time following every action generated
;;; in a story. Finally, the user can also insert particular events into the
;;; story.  See above comments on the parameter *requested-event*.
;;; 
(defun say (cd &optional (add-to-story-cds? t)
	    say-loc)				;To override not saying locations default.
  "Function say is main CD->English translator." 
  (do-break say)
  (let ((x (next-subject cd)))
    (cond ((eq (get x 'gender) 'male)
           (unless (eq x *him*) (setq *him* nil)))
          ((eq (get x 'gender) 'female)
           (unless (eq x *her*) (setq *her* nil)))))
  (let ((final-punctuation? nil)
	(top-cd cd)
	(say-stream (if (and *random-answers?*
			     (eq (old-cd-head cd) 'query))
			*dump-stream*
;;; 		       say-stream		;Changed [cox 11Sep93]
			*tspin-stream*
			)))
    (declare (special final-punctuation?))
    (when (act? cd) 
      (push (time-marker) *story-cds*)
      ;; Was if instead of when. [cox 13aug93]
      (when (and *allow-random-events*		; Added flag so can turn off random events. [cox 11aug93]
		 (or				; Added call so can insert random  
		   (do-insert-p			; events at will. [cox 12aug93]
		     *request-for-random-event*
		     *no-prompt*) 
		   (< (random 100) *random-odds*)))
	(add-random-event
	  ;; Allow insertion of specific events and then
	  ;; reset control variables.
	  (when *requested-event*
	    (let ((new-event *requested-event*))
	      (control-reset t)
	      new-event)))
	))
    (if (and (not (eq (old-cd-head cd) 'query))
	     add-to-story-cds?)
	(push-cd  cd))
    (unless (and (eq (old-cd-head cd) 'loc)
		 (not say-loc))
      (when say-stream 
	(say0 cd (or (cdpath '(time) cd) *default-tense*) nil t))
      (format? say-stream "  ")))
  cd)



;;;
;;; Function do-insert-p prompts the user before inserting a random event into
;;; the story to make sure that it is OK. If, however, the optional parameter
;;; no-prompt? is non-nil, then the insertion is done without the users
;;; intervention. This option is used when the program is running in unattended
;;; mode. [cox aug93]
;;; 
(defun do-insert-p (prompt? &optional no-prompt?)
  (or no-prompt?
      (when prompt?
	(format *tspin-stream*
		"~%Insert event? (ans :y or :n) ")
	(let ((response (read)))
	  (terpri *tspin-stream*)
	  (if (eq response ':y)
	      t
	      nil)))))


(defun push-cd(cd)
  (when *story-cds*
    (if (not (unobservable cd))
      (updated-story cd))))


;;; If *noise* is non nil then all cds passed to function say above
;;; are added to the *story-cds* global list. Otherwise random cd state
;;; changes (act results) are left missing from the story representation, 
;;; though they are said in the English output. This is used by OCCAM as 
;;; a learning parameter for empirical studies. See, for example, discussion
;;; by Pazzani (1993). Mach Learn, 10(3). [cox 1aug93]
;;;
(defvar *noise* nil)

(defvar *print-time-intervals* nil)

;;; When this is the first cd of a new time interval, flush the last interval
;;; by calling new-time. If new is an act or no noise is called for, then push
;;; the new cd on the global list of story cds. [cox 1aug93]
(defun updated-story(new &aux cd)
  (when (and (eq (car *story-cds*) (time-marker))
             (cdr *story-cds*))
    (pop *story-cds*) ;;marker
    (new-time)
    ;; |||||| Note that there other places where the timemarker is pushed. [cox 6aug93]
    (if (or *print-time-intervals* ;Added 1aug93 [cox]
	    *TDebug-On*)            ;Added 6aug93 [cox]
	(format t "~%~%=============== Time Interval===================~%~%" ))
    )
  (when  (or (act? new)
	     (null *noise*)
	     (>= (random 100) *noise*))
     (push new *story-cds*))
;;; Changed above from commented out code below in order to strip tspin 
;;; of its Occam modifications. [cox 23 jul 93]
;  (when  (or (null *learn*)
;	     (act? new)
;	     (null *noise*)
;	     (>= (random 100) *noise*))
;	 (push (cons new (setq cd (talespin->occam new)))  *story-cds*))
;  (if (act? new)
;    (make-predictions cd)) ;;; delete if you don't make predictions
)


;;; [cox 1aug93] 
;;; Process an entire time interval.
;;; Pop all cds off global *story-cds* until time-marker detected. All of these 
;;; items are reversed and collected in events parameter, then passed to 
;;; process-time (which simply stores them in a list on the front of the global 
;;; variable *ALL*. Thus *ALL* will contain a list for each time interval containing 
;;; all acts and state changes during the interval. 
;;;
;;; Function new-time is called only by the story update function (updated-story above)
;;; and right before the main program function (spin) returns after completing the yarn.
(defun new-time( &optional (events nil))
  ;(if (null *story-cds*)
   ; (break))
  (Cond ((eq (car *story-cds*) (time-marker))
         (process-time events))
        
        ;((eq (car *story-cds*) 'ignore-time-marker)
         ;(setq *story-cds* (ignore-time-marker (cdr *story-cds*))))
        (t (new-time (cons (pop *story-cds*) events)))))


;;; Search and remove the time-marker from the list unless hitting ignore-time-marker 
;;; first in the search. This function was probably used in conjucntion with the 
;;; routine that collapses time intervals into one single interval. [cox 1aug93]
(defun ignore-time-marker(lis )
  (if (eq (car lis) (time-marker))
    (cdr lis)
    (if (eq (car lis) 'ignore-time-marker)
      lis
      (cons (car lis) (ignore-time-marker (cdr lis))))))
         
      
      
(defun say0 (cd tense inf subj)
  (declare (special final-punctuation?))
  (let ((*first-word-in-sentence?* t))
    (say1 cd tense inf subj))
  (unless final-punctuation?
    (if *question?*
	(format? *tspin-stream* "?")
	(format? *tspin-stream* "."))
    (setq final-punctuation? t)))

(defun say1 (*cd* *tense* *inf* *subj*)
  (LET ((SAY-PROG (get (old-cd-head *cd*) 'SAY-PROG)))
    (COND (SAY-PROG (funcall SAY-PROG))
          (T (warn "Can't say ~S in English." *cd*)))))

;;; Had to remove the capitalization handling code because of a bug introduced
;;; by Genera 8. Richard is sending a bug report. [1mar94]
;;; 
(defun say-word (x)
  (if (eq x '?unspec) (setq x 'something)) ;Just in case ...
  (cond (*first-word-in-sentence?*
          (format? *tspin-stream* "~<~% ~4,72:;~@(~A~)~>" x))
;;;           (format? say-stream "~<~% ~4,72:;~A~>" x))	
        ((or (get x 'proper-name)
             (member x *first-person-singular-words*))
          (format? *tspin-stream* "~<~%~4,72:; ~@(~A~)~>" x))
;;;           (format? say-stream "~<~%~4,72:; ~A~>" x))
        (t
  	 (format? *tspin-stream* "~<~%~4,72:; ~(~A~)~>" x)
;;;  	 (format? say-stream "~<~%~4,72:; ~A~>" x)
	 ))
  (setq *first-word-in-sentence?* nil))

(DEFun SUBCLAUSE (WORD cd TENSE)
  (if WORD (say-word (OR (RELATIVE-PRONOUN cd) WORD)))
  (let ((*question?* nil))
    (SAY1 CD 
          (COND ((STATE? CD) *DEFAULT-TENSE*)
                ((AND (EQ TENSE 'PAST) (EQ (CDPATH '(TIME) CD) 'FUTURE)) 'COND)
                (T TENSE))
          nil
          t)))

(DEFun RELATIVE-PRONOUN (cd)
  (COND ((AND (EQ (old-cd-head CD) 'LOC) (eq (CDPATH '(VAL) CD) '?unspec))
         'WHERE)
        ((eq (NEXT-SUBJECT cd) '?UNSPEC) 'WHO)
        (T NIL)))

(DEFun NEXT-SUBJECT (cd)
  (CASE (old-cd-head CD)
    (CONT (cdpath '(VAL) cd))
    (mloc (if (eq (cdpath '(val) cd) 'world)
              (next-subject (cdpath '(con) cd))
              (cdpath '(val) cd)))
    (query (next-subject (cdpath '(con) cd)))
    (T (cdpath '(ACTOR) cd))))

(DEFun INFCLAUSE (cd SUBJ-FLAG TENSE)
  (SAY1 cd TENSE T SUBJ-FLAG))

(defun SAY-FILLER (filler case) (say-pp filler case))

;;; This is say Picture Producer, not say prepositional phrase. [cox 31jul93]
(defun SAY-PP (x &optional (case 'objective))
  (cond ((eq x *me*)
         (say-word
             (if (get x 'plural)
                 (case case (nominative 'we) (objective 'us) (possessive 'ours))
                 (case case 
                   (nominative 'i) (objective 'me) (possessive 'mine)))))
        ((eq x *you*)
         (say-word
             (if (eq case 'possessive) 'yours 'you)))
        ((eq x *him*)
         (say-word
             (case case (nominative 'he) (objective 'him) (possessive 'his))))
        ((eq x *her*)
         (say-word
             (case case (nominative 'she) ((objective possessive) 'her))))
        (t (if (MEMber x *ALL-OBJECTS* :test #'eq) (say-word 'the))
           (say-word x))))

(defun SAY-PREP (PREP cd)
  (COND (CD (say-word PREP) (SAY-PP CD 'objective))))

(defun IN-MODE? (X) (MEMber X $(MODE)  :test #'eq))

(defun SAY-NEG () (if (IN-MODE? 'NEG) (say-word 'not)))

(defun SAY-SUBJ-VERB (subject INFINITIVE)
  (COND (*INF* (if *SUBJ* (SAY-PP subject 'objective))
               (SAY-NEG)
               (say-word 'to)
               (say-word infinitive))
        (t
            (let ((tense 'present)
                  (modal nil)
                  (word-for-subject
                      (cond ((eq subject *me*) 'i)
                            ((eq subject *you*) 'you)
                            ((eq subject *her*) 'she)
                            ((eq subject *him*) 'he)
                            (t subject))))
              (cond ((in-mode? 'maybe) (setq modal 'might))
                    ((eq *tense* 'cond) (setq modal 'would))
                    ($(TIME)  (setq tense $(TIME) ))
                    (*tense* (setq tense *tense*))
                    (t (setq tense *default-tense*)))
              (gen-verb infinitive :subject word-for-subject :tense tense
                        :modal modal
                        :able? (in-mode? 'possible)
                        :number (if (get subject 'plural) 'plural 'singular)
                        :person (case word-for-subject (i 1) (you 2) (t 3))
                        :question? *question?* :neg? (in-mode? 'neg)))))
  (case (get subject 'gender)
    (male (if (null *him*) (setq *him* subject)))
    (female (if (null *her*) (setq *her* subject)))))

(defun SAY-TENSE (INFINITIVE PLURAL)
  (let ((pair (get infinitive *tense*)))
    (cond (pair (say-word (if plural (cdr pair) (car pair))))
          ((member *tense* '(present past future) :test #'eq)
           (warn "Conjugating ~S" infinitive)
           (def-verb infinitive) ;regular verb
           (say-tense infinitive plural))
          (t (error "Need the ~S tense of ~S" *tense* infinitive)))))


;;; ||||| What does the acronym dsp represent? [cox 31jul93]
;;; Define Say Procedure (Process?) ? [cox 13aug93]
;;;
(eval-when (eval lisp:load compile)
  (DEFMACRO DSP (NAME &body L)
    (let ((fname (intern (format nil "SAY-~A" name))))
      `(progn (setf (get ',name 'say-prog) ',fname)
              (defun ,fname ()  
		(do-break ,fname)
		,@l)))))

(DSP ATRANS 
     (COND ((eq $(ACTOR)  $(TO) )
            (say-subj-verb $(ACTOR)  'TAKE)
            (SAY-FILLER $(OBJECT)  'objective)
            (SAY-PREP 'FROM $(FROM) ))
           (T (say-subj-verb $(ACTOR)  'GIVE)
              (say-filler $(TO)  'objective)
              (say-filler $(OBJECT)  'objective))))

(dsp kiss
  (say-subj-verb $(ACTOR)  'kiss)
  (say-filler $(OBJECT)  'objective))

(defun concrete? (cd)
  (dolist (role *role-functions*)
    (if (eq (funcall role cd) '?unspec) (return-from concrete? nil)))
  (not (member '?unspec (mode cd) :test #'eq)))

(DSP MTRANS 
     (cond ((eq (old-cd-head $(OBJECT) ) 'query)
            (say-subj-verb $(ACTOR)  'ASK)
            (say-filler $(TO)  'objective)
            (if (and (eq *cd* top-cd) (concrete? $(OBJECT CON) ))
                (let ((*me* $(ACTOR) )
                      (*you* $(TO) )
                      (*him* nil)
                      (*her* nil)
                      (*default-tense* 'present))
                  (format? say-stream ", ~<~% ~10:;\"~>")
                  (say0 $(OBJECT)  'cond nil t)
                  (format? say-stream "\""))
                (SUBCLAUSE 'WHETHER $(OBJECT CON)  'COND)))
	   ;; Added the following clause to speak about "seeing" where something is; that is
	    ;; its location. Should really check that actor and to are the same, however. [cox 8feb95]
           ((eq (old-cd-head $(OBJECT) ) 'loc)
            (say-subj-verb $(ACTOR)  'know)
            (if (and (eq *cd* top-cd) (concrete? $(OBJECT) ))
                  (let ((*me* $(ACTOR) )
                        (*you* $(TO) )
                        (*him* nil)
                        (*her* nil)
                        (*default-tense* 'present))
                    (format? say-stream ", ~<~% ~10:;\"~>")
                    (say0 $(OBJECT)  $(OBJECT TIME)  nil t)
                    (format? say-stream "\""))
                  (SUBCLAUSE 'THAT $(OBJECT)  $(TIME) )))
           (T (say-subj-verb $(ACTOR)  'TELL)
              (say-filler $(TO)  'objective)
              (if (and (eq *cd* top-cd) (concrete? $(OBJECT) ))
                  (let ((*me* $(ACTOR) )
                        (*you* $(TO) )
                        (*him* nil)
                        (*her* nil)
                        (*default-tense* 'present))
                    (format? say-stream ", ~<~% ~10:;\"~>")
                    (say0 $(OBJECT)  $(OBJECT TIME)  nil t)
                    (format? say-stream "\""))
                  (SUBCLAUSE 'THAT $(OBJECT)  $(TIME) )))))


(DSP SNIFF 
     (cond (t
            (say-subj-verb $(ACTOR)  'sniff)
            (say-filler $(OBJECT)  'objective)
	    )
	   ))

(DSP PLAY 
     (cond (t
            (say-subj-verb $(ACTOR)  'play)
            (say-prep 'with $(OBJECT) )
;;; 	    (say-filler $(OBJECT)  'objective)
	    )
	   ))

(DSP DETECTION 
     (cond (t
            (say-subj-verb $(ACTOR)  'detect)
            (say-filler $(OBJECT)  'objective)
;            (if (and (eq *cd* top-cd) (concrete? $(OBJECT CON) ))
;                (let ((*me* $(ACTOR) )
;                      (*you* $(TO) )
;                      (*him* nil)
;                      (*her* nil)
;                      (*default-tense* 'present))
;                  (format? say-stream ", ~<~% ~10:;\"~>")
;                  (say0 $(OBJECT)  'cond nil t)
;                  (format? say-stream "\""))
;                (SUBCLAUSE 'WHETHER $(OBJECT CON)  'COND))
	    )
	   ))

(dsp query
  (let ((*question?* t))
    (case $(OBJECT) 
          (mode (say0 $(CON)  *tense* *inf* *subj*))
          (actor (say0 (set-role 'actor 'who *cd*) *tense* *inf* *subj*))
          (t (warn "Can't say ~S" *cd*)))))

(DSP PTRANS 
  (COND ((eq $(ACTOR)  $(OBJECT) )
         (say-subj-verb $(ACTOR)  'GO))
	;; Added this clause to cover the case when you ptrans something to 
	;; yourself. It is not necessarily an atrans since you may already have
	;; control over it. [cox 17aug93]
	((eq $(ACTOR)  $(TO) )
            (say-subj-verb $(ACTOR)  'TAKE)
            (SAY-FILLER $(OBJECT)  'objective)
            (if $(FROM) 
	      (SAY-PREP 'FROM $(FROM) )))
        (T (say-subj-verb $(ACTOR)  'MOVE)
           (say-filler $(OBJECT)  'objective)))
  (if
      (not (eq $(ACTOR)  $(TO) ))	; Do not want to "take" an obj to himself. [cox 17aug93]
      (SAY-PREP 'TO $(TO) )))



(DSP tilt 
 (say-subj-verb $(ACTOR)  'POUR)
 (say-filler $(OBJECT)  'objective)
 (SAY-PREP 'InTO $(TO) ))

(DSP MBUILD 
     (say-subj-verb $(ACTOR)  'DECIDE)
     (COND ((eq $(ACTOR)  $(OBJECT ACTOR) )
            (infclause $(OBJECT)  NIL 'FUTURE))
           (T (SUBCLAUSE 'THAT $(OBJECT)  'FUTURE))))

(DSP PROPEL 
     (cond ((eq $(TO)  'floor)
            (say-subj-verb $(ACTOR)  'drop)
            (say-filler $(OBJECT)  'objective)
            (say-prep `to $(TO) ))
           (t
            (say-subj-verb $(ACTOR)  'push) 
            (say-filler $(OBJECT)  'objective)
            (cond ($(TO) (say-prep `to $(TO) )))
            (cond ($(FROM)  (say-prep `|away from| $(FROM) ))))))
           

(DSP TOY-PROPEL 
     (cond ((eq $(TO)  'floor)
            (say-subj-verb $(ACTOR)  'drop)
            (say-filler $(OBJECT)  'objective)
            (say-prep `to $(TO) ))
           (t
            (say-subj-verb $(ACTOR)  'push) 
            (say-filler $(OBJECT)  'objective)
            (cond ($(TO) (say-prep `to $(TO) )))
            (cond ($(FROM)  (say-prep `|away from| $(FROM) ))))))
           

(DSP GRASP 
     (COND ((IN-MODE? 'TF)
            (say-subj-verb $(ACTOR)  'LET)
            (say-word 'go) (say-word 'of))
           (T (say-subj-verb $(ACTOR)  'pick)
              (say-word 'up)))
     (say-filler $(OBJECT)  'objective))

(DSP unGRASP 
  (say-subj-verb $(ACTOR)  'LET)
  (say-word 'go) (say-word 'of)
  (say-filler $(OBJECT)  'objective))

(DSP expel 
  (say-subj-verb $(ACTOR)  'exhale) 
  (say-filler  $(OBJECT)  'objective)
  (say-word 'into)
  (say-filler $(TO)  'objective))

(DSP tie 
  (say-subj-verb $(ACTOR)  'tie)
  (say-filler $(OBJECT)  'objective))


;;; Modified to cover Elvis' habit.
(DSP INGEST 
     (SAY-SUBJ-VERB
      $(ACTOR) 
      (COND ((is-a $(OBJECT)  'drink)
             'DRINK) 
            ((is-a $(OBJECT)  'plant)
	     'SMOKE)
	    (t
	     'EAT)
	    ))
     (say-filler $(OBJECT)  'objective))
     

(DSP PLAN 
     (say-subj-verb $(ACTOR)  'PLAN)
     (infclause $(OBJECT)  NIL 'FUTURE))

(DSP WANT 
  (cond ((member 'neg $(OBJECT MODE)  :test #'eq)
         (say1 (negate (wants $(ACTOR)  (affirm $(OBJECT) )))
               *tense* *inf* *subj*))
        (t
            (say-subj-verb $(ACTOR)  'WANT)
            (infclause $(OBJECT) 
                       (NOT (eq $(ACTOR)  (NEXT-SUBJECT $(OBJECT) )))
                       'FUTURE))))

(DSP LOC 
  (say-subj-verb $(ACTOR)
		 (if (in-mode? 'toward)
		     'get 'BE))
  (unless (eq $(VAL)  '?UNSPEC)
    (if (is-a $(VAL) 'container)		; Changed so that only if the location is a container
	(SAY-PREP 'in $(VAL) )			; does the English say that the object is in the location,
	(SAY-PREP 'with $(VAL)))))		; else it says it is with the location. [cox 28dec94]

;;; If a person controls another person (e.g., as a result of an arrest),
;;; say control rather than has. [cox 1sep93]
(DSP CONT
  (if (is-a $(ACTOR)  'person)
      (say-subj-verb $(VAL)  'control)
    (say-subj-verb $(VAL)  (if (in-mode? 'toward) 
			      'get 
			    'HAVE)))
  (say-filler $(ACTOR)  'objective))

(DSP open
  (say-subj-verb $(ACTOR)  'BE)
  (say-word 'open))

(DSP dirty
  (say-subj-verb $(ACTOR)   'BE)
  (say-word 'dirty))

(DSP peeled
  (say-subj-verb $(ACTOR)   'BE)
  (say-word 'peeled))

(DSP flowing
  (say-subj-verb $(ACTOR)   'BE)
  (say-word 'flowing))

(DSP on
  (say-subj-verb $(ACTOR)   'BE)
  (say-word 'on))

;[cox 31jul93]
(DSP burning
  (say-subj-verb $(ACTOR)   'BE)
  (say-word 'burning))

;[cox 8aug93]
(DSP barking
  (say-subj-verb $(ACTOR)   'BE)
  (say-word 'barking))

(DSP ring
  (say-subj-verb $(ACTOR)   'BE)
  (say-word 'ringing))

;[cox]
(DSP lit
  (say-subj-verb $(ACTOR)   'BE)
  (say-word 'lit))


;;; [cox 26aug93]
(DSP arrest
  (say-subj-verb $(ACTOR)  'arrest)
  (say-filler $(OBJECT)  'objective)
  )


(DSP HIT
  (say-subj-verb $(ACTOR)  'hit) 
  (say-filler $(OBJECT)  'objective)
  )
           

(DSP broken
  (say-subj-verb $(ACTOR)   'BE)
  (say-word 'broken))

(DSP bursted
  (say-subj-verb $(ACTOR)   'BE)
  (say-word 'bursted))

(DSP inflated
  (say-subj-verb $(ACTOR)   'BE)
  (say-word 'inflated))

(DSP tied
  (say-subj-verb $(ACTOR)   'BE)
  (say-word 'sealed))

(DSP sharp
  (say-subj-verb $(ACTOR)   'BE)
  (say-word 'sharp))

(DSP shattered
  (say-subj-verb $(ACTOR)   'BE)
  (say-word 'shattered))

(DSP attached
  (say-subj-verb $(ACTOR)   'BE)
  (say-word 'attached)
  (say-prep 'to $(VAL) ))

(DSP flying
  (say-subj-verb $(ACTOR)   'BE)
  (say-word 'flying))

;;;;#changed
(DSP filled
  (say-subj-verb $(ACTOR)   'BE)
  (say-word 'filled)
  (say-prep 'with $(VAL) )) 
  

(DSP MLOC
  (cond ((or 
	  (and (member (old-cd-head $(CON) )
		       ;; |||||| The following list hould be global var. [cox 2aug93]
		       '(like fear trust hungry thirsty sad jonesing) ; |||||| Should smart be added?
		       :test #'eq)
	       (eq $(VAL)  $(CON ACTOR) )))
         (say1 $(CON)  *tense* *inf* *subj*))
        (t
	 (say-subj-verb $(VAL) 
			(if (OR (RELATIVE-PRONOUN $(CON) ) 
				(IS-TRUE? $(CON) ))
			    'KNOW 
			  'think))
	 (SUBCLAUSE 'THAT $(CON)  *DEFAULT-TENSE*))))

;;; |||||| Need to look at this closer. [cox 12aug93]
;Officer1 asked him whether he'd give officer1 the ganja1.  
(dsp achieve
  (flet ((alpha ()
                (case (old-cd-head $(OBJECT) )
                  (mloc
                      (say-subj-verb $(OBJECT VAL)  'find)
                      (say-word 'out)
                      (SUBCLAUSE 'THAT $(OBJECT CON)  *DEFAULT-TENSE*))
                  (loc
                      (say-subj-verb $(OBJECT ACTOR)  'get)
                      (unless (eq $(OBJECT VAL)  '?UNSPEC)
                        (SAY-PREP 'NEAR $(OBJECT VAL) )))
                  (cont
                      (say-subj-verb $(OBJECT VAL)  'get)
                      (say-filler $(OBJECT ACTOR)  'objective))
		  (filled
                      (say-subj-verb $(OBJECT VAL)  'fill)
                      (say-filler $(OBJECT ACTOR)  'objective))
		  ;;Added bored and jonesing. [cox 12aug93]
		  ;;Dissatisfied . [cox 13aug93]
		  ;;Changed to concerned on [cox 17aug93]
                  ((hungry thirsty bored jonesing concerned)
                   (cond ((and (in-mode? 'neg)
                               (member 'neg $(OBJECT MODE) ))
                          (let ((*cd* (set-role 'mode '(pos) *cd*)))
                            (say-subj-verb $(OBJECT ACTOR)  'be)
                            (say-word 'still)
                            (say-word (old-cd-head $(OBJECT) ))))
                         (t
                             (say-subj-verb $(OBJECT ACTOR)  'satisfy)
                             (say-word (case (get $(OBJECT ACTOR)  'gender)
                                         (male 'his) (female 'her)))
			     ;; [cox 27aug93] Changed the following:
;			     (say-word
;                                 (if (eq (old-cd-head $(OBJECT) ) 'hungry)
;                                     'hunger 'thirst))
			     ;; [cox 27aug93] to the following:
                             (say-word
                                 (let ((obj-type (old-cd-head $(OBJECT) )))
				   (case obj-type
					 (hungry 'hunger)
					 (thirsty 'thirst)
					 (bored 'boredom)
					 (jonesing 'craving)
					 (concerned 'concern)))))))
		  ;; and filled. [cox 13aug93]
		  ((filled  burning)
		   (cond ((in-mode? 'neg)
			  (say-subj-verb $(OBJECT ACTOR)  'be)
			  (say-word (old-cd-head $(OBJECT) )))))
                  (sad
                      (cond ((in-mode? 'neg)
                             (let ((*cd* (set-role 'mode '(pos) *cd*)))
                               (say-subj-verb $(OBJECT ACTOR)  'be)
                               (say-word 'still)
                               (say-word
                                   (if (member 'neg $(OBJECT MODE) )
                                       'sad 'happy))))
                            (t
                                (say-subj-verb $(OBJECT ACTOR)  'become)
                                (say-word 'happy))))
                  (t 
		   ;Added test and saying the negation to cover when goals fail. [cox 18aug93]
		   (if (negative? *cd*)	
		       (say1 (negate $(OBJECT) ) *tense* *inf* t) 
		     (say1 $(OBJECT)  *tense* *inf* t))))))  ;;;bad changed to t
    (cond ((or (eq $(ACTOR)  (next-subject $(OBJECT) ))
               (not (is-a (next-subject $(OBJECT) ) 'person)))
           (alpha))
          (t (say-subj-verb $(ACTOR)  'persuade)
             (let ((*inf* t) (*subj* t)) (alpha))))))
        
(DSP HEALTHy
  (cond ((in-mode? 'neg)
         (let ((*cd* (affirm *cd*)))
           (say-subj-verb $(ACTOR)  'BE)
           (say-word 'dead)))
        (t (say-subj-verb $(ACTOR)  'be)
           (say-word 'ALIVE))))

(DSP SMART
  (cond ((in-mode? 'neg)
         (let ((*cd* (affirm *cd*)))
           (say-subj-verb $(ACTOR)  'be)
           (say-word 'stupid)))
        (t
         (say-subj-verb $(ACTOR)  'BE)
         (say-word 'smart))))

;;;Needs that ganja [29 jul 93 cox]
(DSP jonesing (say-subj-verb $(ACTOR)  'BE) 
     (if (>= (random 10) 5)
	 (say-word 'WITHDRAWING)
       (say-word 'DRUG-WITHDRAWING))) 

(DSP HUNGRY (say-subj-verb $(ACTOR)  'BE) (say-word 'HUNGRY))

(DSP THIRSTY (say-subj-verb $(ACTOR)  'BE) (say-word 'THIRSTY))

(DSP bored (say-subj-verb $(ACTOR)  'BE) (say-word 'bored))

;;; Concerned from dissatisfied on [cox 17aug93]
(DSP concerned
  (say-subj-verb $(ACTOR)  'BE)
  (say-word 'concerned))			; [cox 13aug93]

(DSP curious (say-subj-verb $(ACTOR)  'BE) (say-word 'curious))


(dsp sad
  (cond ((in-mode? 'neg)
         (let ((*cd* (affirm *cd*)))
           (say-subj-verb $(ACTOR)  'be)
           (say-word 'happy)))
        (t
            (say-subj-verb $(ACTOR)  'be)
            (say-word 'sad))))

(DSP CAUSE 
      (COND (*question?* ;(IN-MODE? 'QUES)
             (say1 $(CONSEQ)  'future nil t)   
             ;(SUBCLAUSE NIL $(CONSEQ)  'FUTURE)
             (say-word 'if)
             (SUBCLAUSE NIL 
                        $(ANTE) 
                        (CASE *TENSE* 
                               ((FUTURE) 'PRESENT)
                               ((COND) *DEFAULT-TENSE*)
                               (T *TENSE*))))
            (T (say-word 'if)
               (SUBCLAUSE NIL $(ANTE)  'present)
               (say-word 'then)
               (SUBCLAUSE NIL $(CONSEQ)  'future))))


;;; [cox 27jun95]
(DSP SELF-CAUSE 
      (COND (*question?* ;(IN-MODE? 'QUES)
             (say1 $(CONSEQ)  'future nil t)   
             ;(SUBCLAUSE NIL $(CONSEQ)  'FUTURE)
             (say-word 'if)
             (SUBCLAUSE NIL 
                        $(ANTE) 
                        (CASE *TENSE* 
                               ((FUTURE) 'PRESENT)
                               ((COND) *DEFAULT-TENSE*)
                               (T *TENSE*))))
            (T (SUBCLAUSE NIL $(CONSEQ)  'past)
               (say-word 'because)
               (SUBCLAUSE NIL $(ANTE)  'past))
	    ))


(DSP xp-defensive-bark 
      (COND (*question?* ;(IN-MODE? 'QUES)
             (say1 $(CONSEQ)  'future nil t)   
             ;(SUBCLAUSE NIL $(CONSEQ)  'FUTURE)
             (say-word 'if)
             (SUBCLAUSE NIL 
                        $(ANTE) 
                        (CASE *TENSE* 
                               ((FUTURE) 'PRESENT)
                               ((COND) *DEFAULT-TENSE*)
                               (T *TENSE*))))
	    ;; Made more specific to threat [cox 27jun95]
            (T (SUBCLAUSE NIL $(CONSEQ)  'past)
               (say-word 'because)
               (SUBCLAUSE NIL $(ANTE)  'past)
	       (say-word 'threatened)
	       (say-filler $(ACTOR) 'objective))
;	    (T (say-word 'if)
;               (SUBCLAUSE NIL $(ANTE)  'present)
;               (say-word 'then)
;               (SUBCLAUSE NIL $(CONSEQ)  'future))
	    ))


;;; [cox 24feb95]
(DSP xp-goal-of-outcome->actor
  (SUBCLAUSE NIL 
	     $(CONSEQ)
	     (CASE *TENSE* 
	       ((FUTURE) 'PRESENT)
	       ((COND) *DEFAULT-TENSE*)
	       (T *TENSE*)))
  (say-word 'because )
  (SUBCLAUSE NIL 
	     (wants $(ACTOR) $(ANTE))
	     (CASE *TENSE* 
	       ((FUTURE) 'PRESENT)
	       ((COND) *DEFAULT-TENSE*)
	       (T *TENSE*)))
  )



;;; [cox 27jun95]
(DSP xp-instrumental-scene->actor
  (SUBCLAUSE NIL 
	     $(CONSEQ)
	     (CASE *TENSE* 
	       ((FUTURE) 'PRESENT)
	       ((COND) *DEFAULT-TENSE*)
	       (T *TENSE*)))
  (say-word 'because )
  (SUBCLAUSE NIL 
	     (wants $(ACTOR) $(ANTE))
	     (CASE *TENSE* 
	       ((FUTURE) 'PRESENT)
	       ((COND) *DEFAULT-TENSE*)
	       (T *TENSE*)))
  )



;;; [cox 12feb95]
(DSP RESULTS-IN
      (COND (*question?* ;(IN-MODE? 'QUES)
             (say1 $(CONSEQ)  'future nil t)   
             ;(SUBCLAUSE NIL $(CONSEQ)  'FUTURE)
             (say-word 'if)
             (SUBCLAUSE NIL 
                        $(ANTE) 
                        (CASE *TENSE* 
                               ((FUTURE) 'PRESENT)
                               ((COND) *DEFAULT-TENSE*)
                               (T *TENSE*))))
            (T (say-word 'if)
               (SUBCLAUSE NIL $(ANTE)  'present)
               (say-word 'then)
               (SUBCLAUSE NIL $(CONSEQ)  'future))))

(DSP LIKE
  (say-subj-verb $(ACTOR)  'LIKE)
  (say-filler $(TO)  'objective))

(DSP call-on-phone
  )


;;; [cox 9aug93]
;;; Added the actual say-subj-verb call because a nil dsp definition will 
;;; produce a single period in the story text output. Call-on-phone must do
;;; same. [cox 13aug93]
;;;
(DSP police-arrive
     (say-subj-verb 'police 'arrive))

(DSP k-9-squad-arrive
     (say-subj-verb 'police-and-dogs 'arrive))



;;; [cox 9aug93]
;;; Say actor name too [mdd 5apr94]
(DSP dog-barks
   (say-subj-verb $(actor) 'bark)
   (if $(OBJECT)				; [cox 16feb95]
       (say-prep `at $(OBJECT) ))
  )


;; [mdd 4apr94] Removed object of fear
(DSP fear
  (say-subj-verb $(actor) 'be)
  (say-word 'afraid)
)
;;  (say-word 'of)
;;  (say-filler $(to) 'objective))

(DSP trust
  (say-subj-verb $(ACTOR)  'trust)
  (say-filler $(TO)  'objective))
         
(setf (get 'BERRIES 'PLURAL) T)


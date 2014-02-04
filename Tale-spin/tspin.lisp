;;; -*- Mode: LISP; Syntax: Common-lisp; Package: TALE-SPIN; Base: 10 -*-


(in-package :tspin)


;;; ===========================================================================
;;; TaleSpin
;;; Copyright (C) 1987 James R. Meehan
;;; ===========================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	    Elvis World and the Tale-Spin Story Generation Subsystem
;;;;				 for Meta-AQUA
;;;;
;;;;	              Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;			       File: tspin.lisp
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
;;; Modifications of Meehan's program were initially the responsibility of
;;; Michael Pazzani, and were created for automatic data generation for the
;;; OCCAM learning program. Many of the OCCAM specific features were removed by
;;; and further updates and comments made by:
;;;
;;;
;;; COX,MICHAEL T                    |      "Discus,                   
;;; AI / Cognitive-Science Group     |       starred with premonitions,
;;; College of Computing             |       throw yourself            
;;; Georgia Institute of Technology  |       out of yourself."         
;;; Atlanta, Georgia 30332-0280      |                                 
;;; Internet: cox@cc.gatech.edu      |              	     -- Paul Celan
;;; Mosaic ftp://ftp.cc.gatech.edu/pub/ai/students/cox/cox.html
;;;
;;; 
;;; NOTE that all changes and comments associated with the marker "[cox]" is
;;; the responsibility of Michael Cox, whereas the "[mdd]" signifies changes
;;; made by Mark Devaney. The marker "[icu]" references comments from the
;;; original code found in _Inside_Computer_Understanding_ (ICU) by Schank and
;;; Riesbeck. The comments were copied from Warren Sack's (wsack@media.mit.edu)
;;; version of Tale-Spin (~/Research/Tale-Spin/tspin-waander.lisp) given to me
;;; by Bill Andersen (waander@cs.UMD.EDU). Andersen has been involved in
;;; porting many of the micro programs from ICU.
;;;
;;; NOTE also that a series of 6 pipes (||||||) in a comment specifies that
;;; there is a problem at or near that location which needs to be resolved (or
;;; was resolved, but left in because it is now informative).
;;;
;;; 
;;; In order to add new knowledge to Tale-Spin, here are some of the
;;; modifications that must be performed:
;;;
;;; 1. To add new state definitions, also add to the list in the state? and 
;;;    mental-state? predicates and possibly modify the global parameter 
;;;    *property-states* (in the file data.lisp). See also #2.
;;; 2. Relations include like, fear and trust. If adding another like entity, 
;;;    expand the list in the relation? predicate.
;;; 3. When adding a new action, also add to the list in the act? predicate.
;;; 4. I am not sure what is the semantics of the global parameter variable 
;;;    *unobservables*, but it is a list of cds.
;;; 5. A necessary  change is always performed on the function init-world in 
;;;    data.lisp. It is a initialization function with many literal lists that 
;;;    provide a starting  value to the following variables:
;;;       *goals*
;;;       *all-locations*
;;;       *closed-locations*
;;;       *all-objects*
;;; 6. Adding most additional states and relations will require change to
;;;    MLOC's DSP in file mumble.lisp.
;;; 7. See file alt-story.lisp for new items that can be established to create 
;;;    alternate scenarios. Calls of mspin or aspin will use these variables
;;;    instead of defaults. [cox 7aug93]
;;; 8. Adding new goal also requires additions to dsp achieve in mumble.lisp.
;;;    [cox 13aug93]
;;;
;;; 
;;; |||||| See definition of propel for specific code that should be made
;;; general.
;;;
;;; Why does s-answer not have the "a" macro call, whereas s-hunger does?
;;; Because the plan for hunger requires the additional variable food that is
;;; not in the parameter list of s-hunger. It allows the plan to use any food
;;; item in the world. The parameter of s-hunger though is a particular actor
;;; and will remain constant for all plans and all foods. The plan generated
;;; for s-answer is for a particular actor answering a particular phone
;;; receiever. There are now variables needed. [cox 7aug93]
;;;
;;; |||||| How to get the generator to say bi-state relations and unary states
;;; when the states are not first-class cds, but instead, are values on
;;; property lists? For example "The color of ball1 is red." for (color ball1
;;; red).[cox 2aug93]
;;;
;;; |||||| It seems that even if all of the actions of a plan do not work the
;;; goal is still recorded as being achieved. At least when the propel was
;;; mis-defined and the lighter did not light, thus the ganja did not light,
;;; the story still said that it was lit. Likewise when I set the random
;;; element high so that Elvis dropped both of his pipes, spilling all of the
;;; dope, the program still pronounced him as not jonesing anymore. A related
;;; problem in this scenario is that when he drops the pipe, he apparently
;;; does not know where the pipe is since when he dropped them both Elvis had
;;; to quit. Why didn't he just pick them up? Also need to assure that one
;;; cannot use a broken pipe. If Elvis drops the glass pipe he has to get the
;;; other, whereas if he drops the plastic one he can just pick it up and
;;; reuse it. If fact he should before he goes to the cupboard to get the
;;; second pipe. [cox 2aug93]
;;;
;;; |||||| Another problem with Tspin as Pazzani uses it is there is no actual
;;; character interaction. The knowledge in the persuade package, for instance,
;;; is never used, nor is any of the relations between characters, such as
;;; likes, trusts, and fears. [cox 3aug93]
;;;
;;; |||||| Need to change the macro "a" to use something besides *facts* or
;;; otherwise modify the code so that we can change the background knowledge
;;; for various scenarios.  [cox 6aug93]
;;;
;;; |||||| Need to finish the problem with inferences from ptrans as to the
;;; final location of the item ptransed. Have modified where-is and is-at.
;;; Look at what I did. [cox 10aug93]
;;;
;;; [cox] Commented out beep function while on Symbolics.
;;;



;;;
;;; Variable definitions
;;; 

(defvar *random-answers?* t)     ;; ==> random answers to all questions
(defvar *dump-stream* nil)       ;; For queries when *ramdom-answers?* non-nil. See function say.
(defvar *english?* t)            ;; ==> t: print story; nil: don't print story

(defvar *auto?* nil)             ;; ==> t: auto mode; nil: manual mode

(defvar *story-cds* '())         ;; list of cds used in the story
(defvar *all-story-cds* '())     ;; list of cds used in the story

(defvar *cd* nil)                ;; Global variable for the current cd being processed.
                                 ;; The $ read-macro assumes this when calling cdpath
                                 ;; to find slot fillers. [cox]
				 ;; As far as I can tell, *cd* is never explicitly assigned
				 ;; a value. Instead the variable is lexically redefined 
				 ;; implicitly each time doit or conseqs is called. They 
				 ;; both have a parameter named *cd*. The same scoping tricks
				 ;; are played with *actions*, *plans* and *conseqs*. 
				 ;; Actually I do not know why these are defined, since
				 ;; function assert-fact is a top-level call and has its own 
				 ;; local variables by the names *actions* etc. [cox 10aug93]			 


;;; When true 
;;;   glass objects propelled against hard items alsways break,
;;;   objects other than balloons propelled against glass always shatter the glass,
;;; & balloons that propel against a sharp object always burst,
;;; otherwise there is a random chance they will not. See defconseqs propel [cox 6aug93]
;;; 
(defparameter *always* t)


;;; When generating what everyone knows about locations of objects and world facts
;;; during init-world, this global is passed to now-knows. [cox 31jul93]
;;;
;(defvar                         ;; Changed from program variable to program parameter 
(defparameter                    ;; and moved here from file data.lisp so that tdebug 
  *say-facts* nil)               ;; closure would see its initial value. [cox 7aug93]  
                                 

(defparameter			 ;; Controls output of additional knowledge added 
  *say-new-knowledge*		 ;; to the world during function assert-fact. [cox 10aug93]
  nil)				 
				 

;;; Only used in spin-demo. 
(defvar *init-facts* nil)        ;; Is the user supplied assertions. [cox]


;;; The following variables are initialized by init-world.

;;; Because the program never changes this variable, moved it to data.lisp and changed it to 
;;; a program parameter. [cox 8aug93]
;(defvar *all-locations* nil)    ;; Is list of places agents and objects can be in the world [cox]

(defvar *closed-locations* nil)  ;; Is list of objects (from locations above) w/shut openings [cox]
(defvar *all-objects* nil)       ;; Is list of world objects (not including *personae* and Sheryl) 
                                 ;; along with all locations. When saying PictureProducers (pps), 
                                 ;; Mumble precedes these objects with the word "the". [cox 7aug93]
(defvar *goals* nil)             ;; Is list of possible initial goals that serve as story problem 

(defvar *actions* nil)		 ;; See comments on *cd*.
(defvar *plans* nil)		 ;; See comments on *cd*.
(defvar *conseqs* nil)		 ;; See comments on *cd*.
(defvar *main-char* nil)
(defvar *personae* nil)          ;; Current list of agents set by init-world. [cox]

(defparameter *DEFAULT-TENSE* 
  'PAST)
(defvar *TENSE* nil)
(defvar *INF* nil)
(defvar *SUBJ* nil)

(defvar *roles* nil)            ;; Is list of standard slot names in a generic cd [cox]
(defvar *role-functions* nil)   ;; Is list of corresponding functions that retrieve their fillers.
                                ;; Is necessary because cds are implemented as LISP structures. [cox]

(defvar say-stream nil)		;; Is output stream for output of say function and supporting
                                ;; functions. Set to *tspin-stream* usually [cox 11sep93]

(defvar 
  *first-word-in-sentence?* 
  nil)
;(defvar *person* 3)             ;; Is never used by Tale-Spin itself. Must be OCCAM specific. [cox]

(defparameter *me* (gensym))
(defparameter *you* (gensym))
(defparameter *him* (gensym))
(defparameter *her* (gensym))

(defvar *question?* nil)

(defparameter 
  *first-person-singular-words*
  '(i i\'m i\'d i\'ve i\'ll))    ;; Always capitalize these.

(defvar top-cd nil)              ;; This is a mysterious global. Mumble tests to see if the value 
                                 ;; of *cd* is eq to it, but nowhere in the program is the value 
                                 ;; of it changed. Thus it will always be equal to nil. A 
                                 ;; function by the name top-cd also exists in tspin.lisp. It 
                                 ;; simply returns nil. [cox 3aug93] 


;;; [cox 9feb95]
(defvar *old-parameters* nil
  "Backup variable to save program parameter settings before restores.")


;;; 
;;; Initialized during init-tspin. Is used to reset parameter setting to
;;; original values by function . [cox 14feb95]
;;; 
(defvar *original-parameters* nil
  "Original program parameter settings at init time.")


;;; [cox 9feb95]
(defparameter *parameter-names*
  '(
    ;; From above -
    *random-answers?* *always*
    ;; From mumble.lisp -
    *allow-random-events* *random-odds*
    ;; From below (Mark's variables) -
    *sniff-success* *dog-speak-freq* *officer-bark-orders-freq*
    *officer-bark-freq* *comply-freq* *bark-at-contraband-freq*
    *search-success-rate* *amount-threshold* *annoy-dog-freq*
    *high-4-entertainment* *no-cops* *random-at-same-time* *no-spills*
    ;; and from function pick-yarn (extensions.lisp) -
    *force-balloon-throwing* *force-ball-throwing* *force-original-demo*
    *auto?*
    ;; More from mumble.lisp
    *request-for-random-event* *requested-event* *no-prompt*
    )
  "The names of all program parameters that control TaleSpin random variation.")


;;; --------------------------
;;; Parameters to control frequency of random events that I've added
;;; [mdd 23mar94]
;;; 
(defvar *sniff-success* 95)           ;; percentage of time that dog will correctly locate contraband
(defvar *dog-speak-freq* 0)           ;; percentage of time that dog will speak 
(defvar *officer-bark-orders-freq* 0) ;; chance officer will 'bark' orders
(defvar *officer-bark-freq* 0)        ;; chance that officer will bark like a dog
(defvar *comply-freq*  75)            ;; chance of suspect handing over contraband if asked
(defvar *bark-at-contraband-freq* 99) ;; chance dog will bark at contraband	   
(defvar *search-success-rate* 80)     ;; chance cop will find contraband
(defvar *amount-threshold* 3)         ;; amount (out of 10) that Elvis won't get busted for
(defvar *annoy-dog-freq* 0)           ;; entertain by pestering doggie
(defvar *no-spills* t)                ;; t -> will not drop pipe when lighting up. [cox 16mar95]

(defvar *high-4-entertainment* 80
  "Chance Elvis would rather get high than otherwise entertain himself.")


;;; parameters to control randomness of stories
(defvar *no-cops* nil)		      ;; t -> no police events in random events added to stories.



;;;
;;; Function make-boring-story will by default, set the parameters to a very
;;; conservative configuration. If the optional argument is-boring? is nil,
;;; then a slightly more interesting set of stories will be generated. The
;;; difference is that, in the former case, no random events will be inserted
;;; into the major actions of a story, and food, drink or smoking products will
;;; not be accidentally spilled; in the latter case, these twists may occur.
;;; 
(defun make-boring-story (&optional (is-boring? t))
  (cond (is-boring?
	 (reset-params)
	 (setf *no-cops* t)
	 (setf *no-spills* t)
	 (setf *allow-random-events* nil))
	(t
	 (setf *no-cops* nil)
	 (setf *no-spills* nil)
	 (setf *allow-random-events* t)))
  )


(defun make-wild-story (&optional (is-wild? t))
  (cond (is-wild?
	 (make-boring-story nil)
	 (setf *dog-speak-freq* 50)
	 (setf *officer-bark-orders-freq* 50)
	 (setf *officer-bark-freq* 50)
	 (setf *annoy-dog-freq* 50)
	 )
	(t
	 (make-boring-story)
	 (setf *dog-speak-freq* 0)
	 (setf *officer-bark-orders-freq* 0)
	 (setf *officer-bark-freq* 0)
	 (setf *annoy-dog-freq* 0)
	 ))
  )

;;; --------------------------

;;; This code establishes the access functions for each slot in the generic cd 
;;; structure. So given a slot in a cd, the access function is on the 'path-fn
;;; property of the role name. It also sets the global role list *roles* and 
;;; corresponding global access function list *role-functions*. [cox]
;;;
(eval-when (eval load compile)
  (setq *roles* '(actor object to from ante conseq part val con mode time))
  (setq *role-functions* nil)
  (dolist (role *roles*)
    (push
        (setf (get role 'path-fn) (intern (format nil "OLD-CD-~A" role)))
        *role-functions*))
  (setq *role-functions* (nreverse *role-functions*)))


(eval-when (load eval compile)
  (defmacro format? (&rest forms)
    `(if *english?* (format ,@forms)))
  (defmacro terpri? (&optional output-stream)	;Added optional arg [cox 11sep93]
    `(if *english?* (terpri ,output-stream))))


(defun time-marker ()
  (do-break  time-marker )
    'TIME-MARKER)

(defun time-marker? (x)
  (do-break  time-marker? )
    (eq x 'TIME-MARKER))



;;; Automatic story generator. The main character and her/his 
;;; problem is chosen by the program.  [cox]
;;;
(defun aspin (&optional alt?
	      (recover-file-name "recover.file"))
  (do-break aspin)
  (if (not *spin-initialized*)
      (init-tspin
	nil
	recover-file-name))
  (setf *auto?* t)
  (setf *random-answers?* t)
  (setf *dump-stream* nil)
  (spin alt? nil nil nil nil recover-file-name))


;;; Manual story generator. The main character and her/his 
;;; problem is chosen by the user.  [cox]
;;;
(defun mspin (&optional alt? person-num problem-num
	      (recover-file-name "recover.file"))
  (do-break mspin)
  (if (not *spin-initialized*)
      (init-tspin
	nil
	recover-file-name))
  (setf *auto?* nil)
  (setf *random-answers?* nil)
  (spin alt? person-num problem-num nil nil recover-file-name))


;;; The main program function. 
;;; Commented this out because it is redefined in the file patch.lisp. [cox 8aug93]
;;;
;(defun spin ()
;  (do-break spin)
;  (setq *story-cds* nil)
;  (setq say-stream nil)
;  (init-world)
;  (setq say-stream nil)
;  (setq *story-cds* (list (time-marker)))
;  (if (not *auto?*)
;     (format t "~2%This is a story about ...~%"))
;  (let* ((main-character-index
;             (pick-one 
;                 (mapcar #'(lambda (p) 
;                             (list (string-capitalize (string p))
;                                   (string-capitalize (string (get p 'is-a)))))
;                         *personae*)))
;         (main-character (nth main-character-index *personae*))
;         (g (get main-character 'gender)))
;    (setq *main-char* main-character)
;    (when (member *main-char* '(karen lynn))
;      (push 'bored *goals*)
;      (push 'thirsty *goals*)
;      (push 'bored *goals*))
;    (when (eq *main-char* 'lynn)
;      (push 'bored *goals*))
;    (if (not *auto?*)
;       (format? t "~%What is ~@(~A~)'s problem?~%" main-character))
;    (let* ((*him* (if (eq g 'male) main-character nil))
;           (*her* (if (eq g 'female) main-character nil))
;           (problem-index
;               (pick-one 
;                   (mapcar #'(lambda (g)
;                               (list 
;                                   (remove #\Newline
;                                           (with-output-to-string
;                                               (say-stream)
;                                             (say (state main-character
;                                                         g 'pos) nil)))))
;                           *goals*)))
;           (problem (nth problem-index *goals*)))
;      (format? t "~%One day ...~%")
;      (init-gen)
;      (assert-fact (mloc 'world (state main-character problem 'pos)))
;      (format? t "~2%~30t--- The End ---~%")
;      (new-time)
;      (values))))


;;; From the original Meehan code. Not that useful. [cox 3aug93]
;;;
(defun spin-demo (story)
  (do-break  spin-demo )
  (let ((main-character (car story))
        (problem (cadr story))
        (*init-facts* (append *init-facts* (cddr story))))
    (setq say-stream *terminal-io*)
    (format? *tspin-stream* "~3%Once upon a time, ")
    (init-world)
    (format? *tspin-stream* "~%One day, ")
    (assert-fact (mloc 'world (state main-character problem 'pos)))
    (format? *tspin-stream* "~2%--- The End ---~%")))


(defvar *analyzed-cds* nil)  ;; OCCAM used this. [cox 3aug93]

;;; This function is never called, even in the OCCAM code. [cox 7aug93]
;;;
(defun final-analysis ()
  (do-break  final-analysis )
  (setf *analyzed-cds* (time-group *story-cds*)))


;;; This function is never called outside of function final-analysis above,
;;;  even in the OCCAM code. [cox 7aug93]
(defun time-group (cds)
  (do-break  time-group )
  (let ((res '() )
	(curr-group '()))
    (do ((cds cds (cdr cds)))
	((null cds) res)
      (let ((curr-cd (car cds)))
	(cond ((eq curr-cd 'ignore-time-marker)
               (setf (car res) (append (car res) curr-group))
               (setf curr-group '()))
              ((time-marker? curr-cd)
	       (when curr-group (push curr-group res))
	       (setf curr-group '()))
	      (t (push curr-cd curr-group)))))))



(defparameter *unobservables* '(mloc want achieve bored call-on-phone))

;(defparameter        ;Made this a variable instead of parameter [cox 6aug93]
(defvar *say-unobservables* nil)

(defun observables(x)
  (do-break  observables)
  (mapcar #'(lambda(x)
              (remove-if #'(lambda(x)(unobservable (car x)))
                         x))
          x))

(defun unobservable (x)
  (do-break  unobservable )
  (and (old-cd-p x)
       (member (old-cd-head x) *unobservables*)))


(defun pick-one (l)
  (do-break  pick-one )
  (let ((len (length l)))
    (if *auto?*
       (random len)
       (loop
	 (do ((l l (cdr l))
	      (i 1 (1+ i)))
	     ((null l))
	    (format *tspin-stream* "   ~D. ~{~A ~}~%" i (car l)))
	 (format *tspin-stream* "Type 1~@[,~]" (> len 2))
	 (do ((n 2 (1+ n)))
	     ((>= n len)
	      (format *tspin-stream* " or ~D, and then type the RETURN key. -> " len))
	    (format *tspin-stream* " ~D," n))
	 (clear-input)
	 (let ((x (read				; Pazzani had t as parameter. [cox 5jun95]
;;; 				 *tspin-stream*	; Will not work in Harlequin LISP
		    )))
	   (if (and (integerp x) (<= 1 x len))
	       (return (1- x))))
;;; 	 (beep)
	 (format *tspin-stream* "~%Oops!  Try again.~%")))))


;;; A generic cd is defined as a structure with traditional actor, object, 
;;; to, and from slots, along with an ante(cedent) slot and conseq(uence)
;;; slot. [cox 7aug93]
;;;
(defstruct (old-cd (:print-function print-cd))
  head (actor nil) (object nil) (to nil) (from nil) (ante nil) (conseq nil)
  (part nil) (val nil) (con nil) (mode nil) (time nil))

(defvar *cd-level* 0)

;;; Print function for cd structures. [cox 4aug93]
;;;
(defun print-cd (cd stream depth)
  (declare (ignore depth))
  (do-break  print-cd )
  (let ((*cd-level* (1+ *cd-level*)))
    (cond ((> *cd-level* 4)
           (format stream "..."))
          (t
            (format stream "#{~S" (old-cd-head cd))
            (dolist (role *roles*)
              (let ((x (funcall (get role 'path-fn) cd)))
                (if x (format stream "~* ~s=~s" ;"~<~%~VT  ~1:; ~S=~S~>"
                              (* 3 *cd-level*) role x))))
            (format stream "}")))))


; [icu]
;  cdpath finds the filler at the end of the role list in a CD.
;
;  For example, if
;  CD = (mtrans (actor joe)
;               (object (ptrans (actor joe) 
;                               (object worm)
;                               (from joe)
;                               (to irving))))
;  then
;  (cdpath '(actor) cd) returns joe;
;  (cdpath '(object) cd) returns (ptrans (actor joe) 
;                                        (object worm)
;                                        (from joe)
;                                        (to irving));
;  (cdpath '(object object) cd) returns worm.
;
;  If a role doesn't exist in a CD form, then cdpath returns nil.
;
(defmacro cdpath (roles cd)
    (if (and (consp roles) (eq (car roles) 'QUOTE))
	(let ((v (gensym "CD")))
	  (labels ((f (roles cd)
		     (cond ((null roles) cd)
			   ((null (cdr roles)) `(,(g (car roles)) ,cd))
			   (t `(let ((,v (,(g (car roles)) ,cd)))
				 (and ,v ,(f (cdr roles) v))))))
		   (g (role)
		     (intern (format nil "OLD-CD-~A" role))))
	    (if (atom cd)
		`(and ,cd ,(f (cadr roles) cd))
		`(let ((,v ,cd)) (and ,v ,(f (cadr roles) v))))))
	`(*cdpath ,roles ,cd)))


;;; [cox 8sep93]
(eval-when (compile load eval)
  (defun dollar (stream char)
    (declare (ignore char))
    `(cdpath ',(read stream t nil t) *cd*)
    ))

;;; 
;;; When passing function to set-macro-character, Symbolics compiler generates
;;; the weird "leximal map not found" error message if the function is not a
;;; compiled one (and is interpreted function instead). However, cannot try to
;;; recompile it during load operation. [cox 8sep93]
;;; 
(eval-when (compile eval)
  (compile 'dollar)
  )

(eval-when (compile load eval)
  (set-macro-character
    #\$
    #'dollar
;;;       (lambda (stream char)
;;; 	 (declare (ignore char))
;;; 	 `(cdpath ',(read stream t nil t) *cd*))
    t))


(defun *cdpath (roles cd)
  (dolist (role roles cd)
    (setq cd (funcall (get role 'path-fn) cd))
    (if (null cd) (return nil))))


;  set-role makes a new CD form with (role filler) added
;  or replacing the old (role ...) pair. [icu]
;
(defmacro set-role (role value cd)
  (unless (and (consp role) (eq (car role) 'quote))
    (error "set-role can't expand ~S" role))
  (let ((g (gensym "CD")))
    `(let ((,g (copy-old-cd ,cd)))
       (setf (,(get (cadr role) 'path-fn) ,g) ,value)
       ,g)))


;  goal evaluator: executes each plan until one works and the goal
;  can be removed, or until none do and the character fails to get the
;  goal.  If the goal is already true (and the actor knows that), then
;  return success immediately.  If the actor already has the goal,
;  then he's in a loop and has failed.  Otherwise, set up the goal and go.
;  [icu]
;
(defun goal-eval (actor goal plans)
  (do-break  goal-eval )
  (cond ((or (is-true? goal)		;||||||Should not this be an "and" rathe than "or" ? [cox 12aug93]
             (knows? actor goal))
         t)
        ((has-goal? actor goal)
         nil)
        (t (gets-new-goal actor goal)
           (dolist (plan plans)
             (funcall plan)
             (when (is-true? goal)
               (forgets-goal actor goal)
               (return-from goal-eval t)))
           (say (negate (possible (achieve actor goal))))
           nil)))


;;; ||||||Macro "a" should not be mapping across *facts*, rather it should be
;;; mapping accross a list of all of the objects in the world. [cox 6aug93]
;;;
;;; So we can run Tale-Spin with various sets of background knowledge in order
;;; to generate various scenarios, this macro uses a pointer (*facts-pointer*)
;;; to *facts* or its alternative instead of the old way directly. The pointer
;;; is set by the function init-world which is called by spin. The user
;;; control it by passing a flag when calling either mspin or aspin. [cox
;;; 6aug93]
;;;
(defmacro a (type var constraint &rest plan)
          `(let ((things (mapcan #'(lambda(f)
                                    (and (eq (first f) 'is-a)
                                         (eq (third f) ',type)
                                         (list (second f))))
				 ;; Gets indirectly to either *facts* or alt. set of facts [cox 6aug93]
				 (symbol-value *facts-pointer*) 
                            ;    *facts*
				 )))
            (mapcan #'(lambda(,var) . ,plan)
                     (append (random-order
                              (remove-if-not #'(lambda (,var)
                                             ,constraint) things))
                             (random-order
                              (remove-if #'(lambda (,var)
                                             ,constraint) things))))))



;;; [cox 5aug93]
;;; Commented out declare directive and added the format call.
;;; Made it contingent on the new global *TDebug-On*. 
;;;
;;; The ability to print these comments are crucial for debugging.
;;; Since the plans are expanded by the macro into lambda calls 
;;; it is not possible to trace them by ordinary means. Turning on
;;; this debug info allows the programmer to understand Tale-Spin's
;;; control flow as it spins a yarn.
;;;
(defmacro make-plan (comment &rest body)
;  (declare (ignore comment))
  `#'(lambda () 
       (if *TDebug-On*
	   (format *tspin-stream* "~%Plan: ~s~%" ,comment))
;	   (print ,comment)
       . ,body))


;  The simulator [icu]
;
;  doit adds a CD and its consequences to the data base, by calling
;  assert-fact.  mtranses with '?unspecified have to be filled out, as in
;  "Irving told Joe where the honey was" -- the "where" being represented
;  in the CD with an '?unspecified form.
;
(defun DOIT (*CD*)
  (do-break  DOIT )
  (when (check-preconditions *cd*)		;Added [cox 27aug93]
    (if (EQ (old-cd-HEAD *CD*) 'MTRANS)
	(let ((x (KNOWS? $(ACTOR)  $(OBJECT))))
	  (cond (x (SETf (old-cd-object *CD*) x))
		(nil				; |||||| So this will never occur? 
		 ;; Why did not they just comment it out? [29 jul 93 cox]
		 (member (old-cd-head $(OBJECT)) '(loc cont mloc))
		 (dknow $(ACTOR) $(OBJECT))
		 (let ((x (knows? $(ACTOR) $(OBJECT))))
		   (if x (setf (old-cd-object *cd*) x)))))))
    (ASSERT-FACT *CD*)
    *CD*)					;Need to return nill if the plan step fails its 
						;preconditions, not the *CD* as before. [cox 12&27aug93] 
  )


;  assert-fact is one of the central control functions.  It starts with
;  one fact, infers the consequences, infers the consequences of the
;  consequences, etc.  Besides the simple result put in *conseqs*
;  (e.g., ptrans changes locs), new states may lead to response actions
;  (put in *actions*) or new plans (put in *plans*).  The plans are
;  done after all the consequences are inferred. [icu]
;
;;; Added better variable names than l (->alist) and i (->each-item). [cox 4aug93]
;;; And x -> new-fact. [cox 27aug93]
;;;
(defun assert-fact (new-fact)
  (do-break  assert-fact )
  (let ((*actions* nil) (*plans* nil) (*conseqs* nil) (alist (list new-fact)))
    (loop
      (dolist (each-item alist)
        (progn				;Why the progn? [cox 27aug93]
	  (now-knows 'world each-item 
		     *say-new-knowledge*
;		     nil	        ;Pazzani had this nil instead of *say-new-knowledge*
		     )
	  ;;this will work for now
	  (dolist (each-person 
		   (cons 'officer1	;Added [cox 18aug93]
			 *personae*))
	    (now-knows each-person each-item nil)))
        (conseqs each-item))

      (if *conseqs*
          (setq alist (nreverse *conseqs*) 
		*conseqs* nil)
          ;; (shiftf alist *conseqs* nil)
          (return)))

    (dolist (cd (nreverse *actions*))
      (doit (set-role 'time *default-tense* cd)))
    (mapc 'funcall (nreverse *plans*))))


;;; Added the do-break into the consequences function. [cox 9aug93]
;;;
(eval-when (load eval compile)
  (defmacro def-conseqs (name &body body)
    (let ((fname (intern (format nil "~A-CONSEQS" name))))
      `(progn (setf (get ',name 'conseq-fn) ',fname)
              (defun ,fname () 
		(do-break ,fname)
		,@body)))))


;;; 
;;; Macro to create precondition functions for cds. [cox 27aug93]
;;; 
;;; The following macro is in file extensions.lisp in the preconditions checking
;;; section.  See the file for comments on function check-preconditions.
;;; 
;(eval-when (load eval compile)
;  (defmacro def-preconds (name &body body)
;    (let ((fname (intern (format nil "~A-PRECONDS" name))))
;      `(progn (setf (get ',name 'precond-fn) ',fname)
;              (defun ,fname () 
;		(do-break ,fname)
;		,@body)))))


;;; Instantiate the consequences of a given cd (action only?) by
;;; evaluating the function on the conseq-fn property. [cox 10aug93]
;;;
(defun conseqs (*cd*)
  (do-break  conseqs )
  (let ((proc (get (old-cd-head *cd*) 'conseq-fn)))
    (if proc (funcall proc)))
  ;; (setq *conseqs* (nreverse *conseqs*))
  )

(defun add-conseq (x)
  (do-break  add-conseq )
  (save-trace x 'conseq)
  (push x *conseqs*)
  x)

(defun add-action (x)
  (do-break  add-action )
  (save-trace x 'action)
  (push x *actions*) 
  x)


;;; 
;;; Created this function and added trace mechanism to this and above 
;;; two routines. [cox 10aug93]
;;;
;;; See file extension.lisp in the trace section for comments.
;;;
;(defun add-plan (x)
;  (do-break  add-plan )
;  (save-trace x 'plan)
;  (push x *plans*) 
;  ;; I did dot return x because I am replacing calls of push.
;  )					


(defmacro def-reaction (head &body body)
  (let ((name (intern (format nil "~A-REACT" head))))
    `(progn (setf (get ',head 'react-fn) ',name)
            (defun ,name () 
	      (do-break ,name)
	      ,@body))))



;  Stored under each character is a list of "demons."  A demon is
;  a CD pattern plus an action.  Whenever the character learns
;  something this list is checked to see if there is a response to make.
;  Demons are set up by things like the mbuild in a bargain-plan. [icu]
;;;
;;; |||||| I do not know if this comment is wholy accurate since the 
;;; code from Sack was quite different than the following. [cox 31jul93]
;;;
(defun DEMON-CHECK (WHO EVENT)
  (do-break  DEMON-CHECK )
  (let ((l nil))
    (dolist (demon (get who 'demons))
      (if (match (car demon) event)
          (push (cdr demon) *actions*)
          (push demon l)))
    (setf (get who 'demons) (nreverse l))))



(defun SGOAL-CHECK (ACTOR SCALE)
  (do-break  SGOAL-CHECK )
  (if (IN-STATE? ACTOR SCALE)
      (add-plan ;;; [cox 10aug93]
          (make-plan `(discovered-goal ,actor ,scale)
                     (cond ((eq scale 'thirsty)
			    (s-thirst actor))
			   ((eq scale 'hungry)
			    (s-hunger actor))
			   ;; Added the following. I assume it is ok. [cox 4aug93]
			   ;; ||||||It will not ever be executed as is.
			   ;; See loc reaction above. [cox 12aug93]
			   ((eq scale 'bored)     
			    (s-entertain actor))
			   ((eq scale 'jonesing) ;[cox]
			    (s-jonesing actor)))))))

(defun NOTICE (WHO CD)
  (do-break  NOTICE )
  (LET ((WHERE (LOC-NAME-OF WHO)))
    (dolist (i *personae*)
      (WHEN (eq (LOC I) WHERE) (ADD-CONSEQ (MLOC I CD))))))


(eval-when (load eval compile)
  (defmacro deletef (item loc &rest l 
                          &key from-end test test-not start end count key)
    (declare (ignore from-end test test-not start end count key))
    (multiple-value-bind (vars vals stores store-form access-form)
                         (CLTL1:get-setf-method loc) ;Added package spec 13jan99
      `(let* (,@(mapcar #'list vars vals)
                (,(car stores) (delete ,item ,access-form ,@l)))
         ,store-form))))
       

;  addfact adds a CD to knower's knowledge set.  Also if world
;  learns a character has died, then the character is removed from the
;  global list of characters.
;
;  The CD is added to the front of the fact list, so that memquery
;  will get the most recent CD that matches its query pattern.  Older
;  contradicted facts are still on the list but are not seen. [icu]
;
;;; I believe that in Pazzani's version contradicted facts are removed
;;; during the deletef operation. [cox]
;;;
;;; Addfact is called only by function now-knows. [cox 11aug93]
;;;
;;; There was a problem with this function as received from Pazzani:
;;; PTRANS adds the consequences that when one physically moves an object from 
;;; location x to location y, the item's location is now y, and the location
;;; is no longer x. Now there existed an interaction with function addfact. 
;;; When a new cd is added to the data base of an agent, the routine correctly
;;; removes the negation of the cd. However, if the new cd is a location, then 
;;; it additionally removes all other locations. This is fine for positive 
;;; assertions about locations; an object can only be at one location at a 
;;; time. But if the new fact is a negative assertion, as in the second
;;; ptrans inference, then it also removed all other positive assertions about
;;; the objects location, that is, that the item is at location y. The result
;;; was that (loc object) returned nil. Moreover, one can't even remove
;;; all other negative assertions, since it is true that the object is NOT
;;; in many other places, and negative knowledge may be useful. We only want
;;; to remove a possible negative assertion about the object being in y.
;;;
(defun ADDFACT (KNOWER CD)
  (do-break  ADDFACT )
  (let ((h (old-cd-head cd)))
    (deletef (negate cd) (getf (get knower 'facts) h) :test #'equalp)
    (cond ((and (eq h 'loc)     ; If cd is a positive location, remove
		(positive? cd)) ; all other positive locations for the cd actor [sic]. [cox 11aug93]
	   (do ((l (get-concepts knower h) ;Changed to call of get-concepts. [cox 11aug93]
		   (cdr l))
                (z nil
                   (if (match (is-at (cdpath '(actor) cd) '?unspec) (car l))
                       z 
                       (cons (car l) z))))
               ((null l) (setf (getf (get knower 'facts) h) z)))
	   )
          ((AND (EQ KNOWER 'WORLD)
                (EQ h 'HEALTHy)
                (negative? cd))
           (SETq *PERSONAE* (remove (CDPATH '(ACTOR) CD) *PERSONAE*))))
    (pushnew cD (getf (get knower 'facts) h) :test #'equalp))
                                                ;(GET KNOWER 'FACTS))
  (values))


;;; Shorthand for common "data base:" operation. [cox 11aug93]
;;;
(defun get-concepts (knower concept-type)
  (do-break get-concepts)
  (getf (get knower 'facts) concept-type))


;  state? returns non-nil if CD is one of the state forms. [icu]
;
(defun STATE? (CD)
  (do-break  STATE? )
  (MEMber (old-cd-head CD)
	  ;; Added jonesing [cox]
	  ;; |||||| Why is not bored in here? [cox 31jul93]
          '(LOC MLOC CONT LIKE trust fear HUNGRY THIRSTY HEALTHy
		SMART sad JONESING CONCERNED BORED) 
          :test #'eq))


(defun mental-state? (cd)
  (do-break  mental-state? )
  (member (old-cd-head cd)
          '(mloc like trust fear hungry thirsty
		 jonesing smart sad		;[Added jonesing cox]
		 CONCERNED BORED)		;Added [cox 2sep93]
          :test #'eq))



;  now-knows adds what to the data base for who.  It also prints in
;  English this new fact.  If who = world (a true fact) and what is
;  an mloc, then save the content of the mloc under the person who
;  learned it.  If say-flag is t, then mlocs are always generated in
;  English; otherwise only facts (who = world) are generated.  This
;  reduces the volume of the output. [icu]
;
(defun NOW-KNOWS (WHO WHAT SAY?)
  (if (eq 'world who)				;|||||| Change back to normal later. [cox 12aug93]
      (do-break  NOW-KNOWS ))
  (when (and (eq who 'world)
	     (eq (old-cd-head what) 'call-on-phone)	;||||||Do same for police-arrive etc [cox 10aug93]
	     (not *random-at-same-time*))
    (push (time-marker) *story-cds*))		; |||||| Why here? [cox 2aug93]
  (when (and (eq who 'world)
             (member (old-cd-head what) *property-states*))
    (setf (get (old-cd-actor what) (old-cd-head what))
	  (car (old-cd-mode what))))
  (COND ((AND (EQ (old-cd-head WHAT) 'MLOC)
              (or (eq who 'world) (eq who (cdpath '(val) what))))
         (now-knows (cdpath '(val) what) (cdpath '(con) what) say?))
        ((and (eq who 'world) (mental-state? what))
         (now-knows (cdpath '(actor) what) what say?))
        (t (ADDFACT WHO WHAT)
	   (COND ((and
		    (EQ WHO 'WORLD)		; Getting tired of redundant output, so added. [cox 24feb95]
		    (OR SAY? (and (or (EQ WHO 'WORLD) (mental-state? what)) 
				  (not (eq (old-cd-head what) 'mloc))))
		    (if *say-unobservables*
			t
			(not(unobservable what))))
		  (SAY (if (eq who 'world) what (MLOC WHO WHAT))))))))



;  knows?(knower,fact) returns fact if fact is in data base for knower:
;  -- if fact = knows(knower,subfact), assume everyone knows what they
;     know and look up subfact,
;  -- if fact has a ?unspec, then return the filler that replaces
;    the ?unspec in the data base. [icu]
;;; |||||| Seems to be differnces between this version and that of ICU. [cox 2aug93]
;;;
(defun KNOWS? (KNOWER FACT)
  (do-break  KNOWS? )
  (cond ((and (eq (old-cd-head fact) 'mloc)
              (or (eq knower 'world) 
		  (eq knower (cdpath '(val) fact))))
         (knows? (cdpath '(val) fact) (cdpath '(con) fact)))
        ((and (eq knower 'world) (mental-state? fact))
         (knows? (cdpath '(actor) fact) fact))
        (t (memquery knower fact))))


(defun KNOWS-LOC (KNOWER OBJECT)
  (do-break  KNOWS-LOC )
  (CDPATH '(VAL) (KNOWS? KNOWER (WHERE-IS OBJECT))))


;;; 
;;; Given a location (and pass the world as the knower), returns just the first
;;; found object that is at location loc. [cox 12aug93]
;;; 
(defun KNOWS-whats-at-LOC (KNOWER loc)
  (do-break  KNOWS-whats-at-LOC )
  (CDPATH '(actor) (KNOWS? KNOWER (WHats-at loc))))


(defun KNOWS-OWNER (KNOWER OBJECT)
  (do-break  KNOWS-OWNER )
  (CDPATH '(VAL) (KNOWS? KNOWER (WHO-HAS OBJECT))))


(defun KNOWS-IF (KNOWER CD)
  (do-break  KNOWS-IF )
  (CDPATH '(MODE) (KNOWS? KNOWER (SET-ROLE 'MODE '?UNSPEC CD))))


;;; 
;;; Function contents returns the contents of container specified cd.
;;; [cox 11sep93]
;;; 
(defun contents(cd)
  (do-break  contents)
  (cdpath '(val) (knows? 'world (bi-state cd '?unspec 'filled 'pos))))



;  memquery find the first item in knower's data base that
;  matches fact. [icu]
;;;
;;; The facts data base for each agent, including the world itself, is 
;;; representeds a list on the 'facts property of the symbol representing the 
;;; agent (or the symbol 'world). This list is composed of a series of pairs.
;;; The first item in the pair is the type of the knowledge (e.g., PTRANS),
;;; whereas the second item in the pair is a list of all cds of that type the
;;; agent knows. Thus a data base would be of the form:
;;;     (type1 (type1-cd1 type1-cd2 ... ) type2 (type2-cd1 type2-cd2 ... ) ...
;;;      typen (typen-cd1 typen-cd2 ...)) 
;;; This representation is natural for getf function. [cox 5&10aug93]
;;;
(defun MEMQUERY (KNOWER PAT)
  (do-break  MEMQUERY )
  (PAT-MEMBER PAT (getf (get knower 'facts) (old-cd-head pat))))
                  ;(GET KNOWER 'FACTS)))



;  pat-member finds the first item in cd-list that matches
;  pat and returns cd-list from that item on. [icu]
;
;;; This function is different from that of ICU and, for instance,
;;; returns the first matching cd rather than acting like member function 
;;; (returning the tail after and including the matching cd) at
;;; all. [cox 2aug93]
;;;
(defun PAT-MEMBER (PAT CD-LIST)
  (do-break  PAT-MEMBER )
  (dolist (cd cd-list nil)
    (if (and (old-cd-p cd)
             (match pat cd))
      (return cd))))



;  Returns non-nil if actor has goal. [icu]
;
(defun HAS-GOAL? (ACTOR PAT)
  (do-break  HAS-GOAL? )
  (PAT-MEMBER PAT (GET ACTOR 'GOALS)))


;  Adds goal to data base. [icu]
;
(defun GETS-NEW-GOAL (ACTOR GOAL)
  (do-break  GETS-NEW-GOAL )
  (push GOAL (GET ACTOR 'GOALS))
  (when *say-unobservables*
    (SAY (WANTS ACTOR (achieve actor GOAL)))))


;  Removes goal from data base. [icu]
;
(defun FORGETS-GOAL (ACTOR GOAL)
  (do-break  FORGETS-GOAL )
  (setf (get actor 'goals)
        (delete (has-goal? actor goal) (get actor 'goals))))


;;; Predicate in-state? is called by sgoal-check only. [cox 2aug93]
;;;
(defun IN-STATE? (X ST)
  (do-break  IN-STATE? )
  (FIND-OUT 'WORLD (STATE X ST 'POS)))


;;; [cox 2aug93]
;;; The parameter rel takes only values that pass the relation? predicate:
;;; like, fear, trust. [cox 2aug93]
;;; Function relate finds out whether X knows that Y has the relation rel with Z.
;;; Thus (relate x x y like) --> Does X know that x likes y.
;;;      (relate x y x like) --> Does X know that y likes him.
;;;      (relate x x x like) --> Does X know that x likes himself.
;;;
(defun RELATE (X Y Z REL)
  (do-break  RELATE )
  (FIND-OUT X (RELATION Y Z REL 'POS)))


;  [icu]
;  Looks up CD in the data base for who.  If there, return non-nil if
;  the CD is not a negative fact.  If not there, ask the user at the
;  terminal and save the result.  Note that the generator is used to
;  ask questions.
;
;  find-out is used to determine if a given character is in a
;  given state (e.g., is the character hungry or thirsty)
;;; Call in-state? above to do this. [cox 2aug93]
;  and is also used to determine how two characters relate to one
;  another (e.g., do they like one another?, does one have a tendency 
;  to deceive the other, etc.).
;;; Call relate above to do this. [cox 2aug93]
;;;
(defun FIND-OUT (WHO CD)
  (do-break  FIND-OUT )
  (LET ((MODE (KNOWS-IF WHO CD)))
    (COND (MODE (MEMber 'POS MODE))
	  (*random-answers?*                     ;; auto random decisions
	   (let ((answer (zerop (random 2))))
	     (init-gen)
	     (say (question (mloc who cd)))
	     (init-gen)
	     (now-knows who (set-role 'mode (if answer '(pos) '(neg)) cd) nil)
	     answer))
          (T (init-gen)
             (format *tspin-stream* "~2%   You decide: ")
             (SAY (question (MLOC WHO CD)))
             (init-gen)
;;;              (beep)
             (clear-input)
             (LET ((ANSWER (y-or-n-p "   Type Y for yes or N for no --> ")))
               (terpri?
		 *tspin-stream*)			;[cox 11sep93]
               (now-knows WHO (SET-ROLE 'MODE (if ANSWER '(POS) '(NEG)) CD) nil)
               ANSWER)))))


;  True if y thinks x is a friend of his. [icu]
;
(defun IS-FRIEND-OF? (X Y)
  (do-break  IS-FRIEND-OF? )
  (AND (NOT (eq X Y)) (RELATE Y X Y 'LIKE)))


;  Returns location of x.
;
(defun LOC (X)
  (do-break  LOC )
  (KNOWS-LOC 'WORLD X))


;  True if x and y are in the same place.
;
(defun IS-PROX? (X Y)
  (do-break  IS-PROX? )
  (eq (LOC-NAME-OF X) (LOC-NAME-OF Y)))


;;; Predicate to test if the cd has a negative mode slot. [cox 4aug93] 
;;;
(defun negative? (cd)
  (do-break  negative? )
  (member 'neg (cdpath '(mode) cd) :test #'eq))


;;; The negation of the negative? predicate. 
;;; |||||| But this works for binary slot values only. Perhaps the cd has mode
;;; 'maybe, however, so this is neither negative nor necessarily positive. 
;;; [cox4aug93] 
;;; 
(defun positive? (cd)
  (do-break  positive? )
  (not (negative? cd)))


;  A CD is true if it's an mloc and the content is in the person's
;  data base, or it's in the data base for world. [icu]
;;; Modified by Pazzani to cover negative assertions and case where 
;;; cd is a location. [cox 2aug93]
;;;
(defun IS-TRUE? (CD)
  (do-break  IS-TRUE? )
  (COND ((negative? cd)
         (not (is-true? (negate cd))))
        ((EQ (old-cd-head CD) 'MLOC)
         (KNOWS? (CDPATH '(VAL) CD) (CDPATH '(CON) CD)))
        ((eq (old-cd-head cd) 'loc)
         (is-prox? (cdpath '(actor) cd) (cdpath '(val) cd)))
        (T (KNOWS? 'WORLD CD))))


;  loc-name-of returns the real location of x.  This may involve going
;  up several levels -- e.g., when Joe takes a worm, its location is
;  stored as joe, but its real location is the location Joe is at. [icu]
;;; Totally different from ICU. [cox 2aug93]
;;;
(defun LOC-NAME-OF (X)
  (do-break  LOC-NAME-OF )
  (do ((new (loc x) (loc new))
       (old x new)
       (l (list x) (cons new l)))
      ((or (null new) (member new l :test #'eq)) old)))


;  get-isa is like get but checks is-a node for x if x has no
;  y property.	[icu]
;
(defun GET-ISA (X Y)
  (do-break  GET-ISA )
  (OR (GET Y X) (GET (GET Y 'IS-A) X)))





(defun relation? (cd)
  (do-break  relation? )
  (member (old-cd-head cd) 
	  '(like fear trust) 
	  :test #'eq))


;;; |||||| Why is not play-catch is this list? [cox]
;;;
(defun act? (cd)
  (do-break  act? )
  (member (old-cd-head cd)
	  '(turn ptrans atrans propel mtrans 
	    kiss grasp ingest mbuild ungrasp 
            call-on-phone tilt expel tie
	    police-arrive dog-barks		; [cox 12aug93]
	    k-9-squad-arrive			; [cox 2sep93]
	    detect play				; [cox 14feb95]
	    )
	  :test #'eq))


(defun MODE (CD)
  (do-break  MODE )
  (CDPATH '(MODE) CD))


(defun AFFIRM (CD)
  (do-break  AFFIRM )
  (COND ((MEMber 'POS (MODE CD)) 
	 CD)
        (T (SET-ROLE 'MODE 
		     (CONS 'POS 
			   (remove 'NEG (MODE CD)))
		     CD))))

(defun negate (CD)
  (do-break  negate )
  (COND ((negative? cd) 
	 (AFFIRM CD))
        (T (set-role 'MODE 
		     (CONS 'NEG 
			   (remove 'POS (MODE CD)))
		     CD))))

;  maybe makes a CD hypothetical -- doesn't matter if it's true or false. [icu]
;
(defun MAYBE (CD)
  (do-break  MAYBE )
  (COND ((MEMber 'MAYBE (MODE CD)) CD)
        (T (set-role 'MODE (CONS 'MAYBE (MODE CD)) CD))))

(defun possible (cd)
  (do-break  possible )
  (cond ((member 'possible (mode cd)) cd)
        (t (set-role 'mode (cons 'possible (mode cd)) cd))))

;;; Boy this is overly specific! [cox 2aug93]
(defun pick-a-pot ()
  (do-break  pick-a-pot )
  'pan1)


;  tf adds "transition final" to a CD -- doesn't matter if it's true
;  or false. [icu]
;
(defun TF (CD)
  (do-break  TF )
  (COND ((MEMber 'TF (MODE CD)) CD)
        (T (set-role 'MODE (CONS 'TF (MODE CD)) CD))))


;  future sets a CD to a future time. [icu]
;
(defun FUTURE (CD)
  (do-break  FUTURE )
  (set-role 'TIME 'FUTURE CD))


;;; At the top level, const is the current cd pat-member has passed. 
;;; [cox 10aug93]
;;;
(defun MATCH (PAT CONST)
  (declare (inline every))
  (and (eq (old-cd-head pat) (old-cd-head const))
       (every #'(lambda (slot)
                  (match-item? (funcall slot pat) (funcall slot const)))
              '(old-cd-actor old-cd-object old-cd-to old-cd-from old-cd-ante old-cd-conseq
                old-cd-val old-cd-con old-cd-time))
       (match-mode? (old-cd-mode pat) (old-cd-mode const))))


(defun match-item? (pat const)
  (do-break  match-item? )
  (cond ((null pat) t)
        ((null const) t)
        ((eq pat '?unspec) t)
        ((old-cd-p pat) (and (old-cd-p const) (match pat const)))
        ((old-cd-p const) nil)
        ((eq pat const) t)
        (t nil)))


(defun match-mode? (p c)
  (do-break  match-mode? )
  (or (null c) (eq p '?unspec) (subsetp p c)))


;;; Commented out so would not conflick with Symbolics function by same name.
;;; [cox 9sep93]
;(defun beep ()
;  ;; Do something to get the person's attention.
;  #+:ccl (ccl:ed-beep)
;  #+dec (write-char #.(int-char 7))
;  )

(defun is-a(x Y)
  (do-break  is-a)
  (when x (or (eq x y)
              (is-a (get x 'is-a) y))))


(defun component-of (x)
  (do-break  component-of )
  (get x 'component))



;;; |||||| Hardly random. [cox 2aug93]
;;;
(defun random-append (x y)
  (do-break  random-append )
  (append x y))

(defun random-choice (n x y)
  (do-break  random-choice )
  (if (> (random 1.0) n)
      x
    y))
  
(defun random-order(x)
  (do-break  random-order)
  (randomize x nil))

(defun random-element(x)
  (do-break  random-element)
  (nth (random (length x)) x))

(defun randomize(X y)
  (do-break  randomize)
  (if (null x) 
      y
    (let ((a(random-element x)))
      (randomize (remove a x) (cons a y)))))
  

;;; Controls whether or not randomly generated events are placed in a separate 
;;; time interval of their own. If nil they are, t they are not. The program
;;; parameter is used below in function add-random-event and also by function 
;;; now-knows. [cox 7aug93]
;;;
(defparameter *random-at-same-time* nil)


;;; Initialized to *saved-random-events* during init-world. Contains the 
;;; events that are randomly inserted into the story by function add-random-event
;;; below. Variable was not defined with a defvar by Pazzani. [cox 7aug93]
;;;
(defvar *random-events* nil)


;;; If there are random events remaining, remove one and instantiate it. The event is 
;;; placed in a separate time interval unless *randon-at-same-time* is true. 
;;;
;;; In function say (and only there), when the cd parameter is an action then time marker
;;; is pushed onto *story-cds*, and then add-ramdom-event is sometimes called depending
;;; on the *random-odds* variable in file mumble.lisp. [cox 7aug93]
;;;
;;; Modified so that the function can be passed a random event directly,
;;; instead of taking it from the random events list. Thus it is not always
;;; random. When passing a random event, it must be quoted. [cox 6feb95]
;;;
(defun add-random-event (&optional passed-event)
  (do-break  add-random-event)
  (when (or passed-event
	    *random-events*)
    (let* ((e (or passed-event
		  (random-element *random-events*)))
           (cd (apply (first e) (rest e))))
      (when (not passed-event)
	(setq *random-events* (remove e *random-events*))
	;; Added the following so that only one random police 
	;; event can occur per episode. [cox 27aug93]
	(if (member e *random-police-events* :test #'equal)
	    (setf *random-events* 
		  (set-difference *random-events*
				  *random-police-events*
				  :test #'equal))))
      (if (act? cd)
	  (progn (add-action cd) 
		 (when *random-at-same-time* 
		   (remove-time-marker)))
	  (progn (add-conseq cd)
		 (when (not *random-at-same-time*)
		   (push (time-marker) *story-cds*)))))))



(defun remove-time-marker ()
  (do-break  remove-time-marker )
  (if (eq (car *story-cds*) (time-marker))
    (pop *story-cds*)
    (progn (break "No time-marker")
           (push 'ignore-time-marker  *story-cds*))))



;;; Where was this function supposed to be used and in what role? [cox 25 july 93]
;;; It can be used to print the story and shows the time interval divisions.
;;; Call it by passing *ALL*, for example. [cox 1aug93]
;;;
;;; The call to print-cd passes x instead of the car of x because we are now
;;; using Tale-Spin representations instead of Occam reps.
;;;
(defun display-sequence (&optional (e *analyzed-cds*) (time 0))
  (do-break  display-sequence )
  (when e
    (format *tspin-stream* "~%~%=============== Time ~a===================~%~%" time)
    (mapc #'(lambda(x)
	      (terpri *tspin-stream*)
	      (print-cd 
	       ; (car x) 
	       x
	       *tspin-stream* 10))
	  (car e))
    (display-sequence (cdr e)(+ 1 time))))

 
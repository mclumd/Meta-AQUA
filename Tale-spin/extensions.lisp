;;; -*- Mode: LISP; Syntax: Common-lisp; Package: TALE-SPIN; Base: 10 -*-


(in-package :tspin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	    Elvis World and the Tale-Spin Story Generation Subsystem
;;;;				 for Meta-AQUA
;;;;
;;;;	     Copyright (C) 1996   Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;			     File: extensions.lisp
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
;;;; Changed to create from error for conditional action of opening
;;;; file when no file exists in function off-to-disk. [mcox 1nov05]
;;;; 




;;;****************************************************************************
;;;
;;;			   TALE-SPIN EXTENSIONS
;;;
;;;****************************************************************************

;;;
;;; Tale-Spin is a story generation program that spins yarns about the
;;; characters in a small house (actually the kitchen, the door, and the
;;; immediate yard and garage just outside of the door). The main characters
;;; include Mom and Dad and their children Lynn and Karen. The house also has a
;;; cat that sometimes knocks things over and a background character named
;;; Sheryl. Tale-Spin weaves stories by selecting (or by receiving from the
;;; user) a main character and a problem for that character. The character will
;;; then attempt to solve the goal of removing the problem by selecting plans
;;; to achieve the goal. As the character acts in this small world, various
;;; random events co-occur.
;;;
;;; I have altered the program to include two additional characters: Elvis, a
;;; drug-addicted boarder who occasionally smokes pot, and the police who
;;; occasionally come unexpectedly to try to locate contraband and arrest
;;; Elvis. The house also has been given a house dog which occasionally barks.
;;; In addition, the police sometimes arrive with a drug-detection dog to help
;;; find the pot.  Characters now will interact because random events can
;;; include the introduction of a characters with conflicting plans and goals
;;; of their own. For example, Elvis wants to smoke pot to relieve his tension;
;;; whereas, the police will arrive to take the pot from him and to deprive him
;;; of his freedom. Therefore, Elvis' plans may be interrupted and even
;;; thwarted.
;;;
;;; Another change to the behavior of Tale-Spin is that it now generates
;;; explanations for the main actions of major characters in the story. So, if
;;; Dad is eating a banana, Tale-Spin will tell the reader that the reason Dad
;;; does this action is because he has the goal of relieving his hunger. 
;;;


;;;
;;; The code in this file augments the basic operation of the Tale-Spin
;;; program.  All functions were written from scratch, rather than being
;;; modifications of other TSpin functions. The additions include support for
;;; CRASH RECOVERY, EMPIRICAL-EXPERIMENT REPLICATION, and additional TRACE
;;; mechanisms (the TDebug facility) that monitor global plans, actions and
;;; consequences during story creation. New functions that were created from
;;; scratch are also included in this file.
;;;
;;; The new functions include one to list all objects at a given location, and
;;; a new predicate to test whether an agent has a given object. This file also
;;; contains the new function add-plan (isomorphic to functions add-action and
;;; add-conseq). Another new function, check-preconditions, is called by
;;; function doit in order to check for action preconditions before executing
;;; the schema. This is necessary because now various characters can now
;;; nondeterministically enter a scenario and modify the world, including
;;; undoing the preconditions of other agents' actions. Finally, there are two
;;; new user functions: one, called print-params, displays the new program
;;; parameters that control Tale-Spin's concept generation (especially
;;; randomness); the other, called reset-params, reassigns original values to
;;; all program parameters.
;;;
;;; Note also that Tale-Spin has been modified elsewhere to facilitate loading
;;; of various worlds. Before the modification, one would have to change the
;;; file data.lisp each time (or rename the file to something else and replace
;;; it with a surrogate). Now, because init-world uses pointers to the facts
;;; and objects in a world, one need only create alternative lists called
;;; *new-facts*, *new-objects*, *new-everyone-loc-facts*, and
;;; *new-world-facts*, then pass the value t as the 1st parameter (alt?) of
;;; spin. One could also simply change the following code (found in function
;;; spin) to include new names of your choice, then pass t to spin.
;;;
;;;   (if alt?
;;;  	  (init-world *new-facts*
;;;  		      '*new-facts*
;;;  		      *new-objects* 
;;;  		      *new-everyone-loc-facts* 
;;;  		      *new-world-facts*)
;;;  	  (init-world))
;;;
;;; 
;;; The functions say and add-random-event have also been modified in order to
;;; increase the user's control over inserting of events into the story. The
;;; user can now shut off all insertion of random events, the user can insert
;;; random events when desired, and the user can insert particular events when
;;; desired. See comments at the beginning of file mumble.lisp.
;;;
;;; Finally, a number of parameters have been added to the function spin in
;;; order to allow more control and flexibility. In particular, when set to t
;;; the flag pick-yarn? invokes the function specify-yarns (at the bottom of
;;; this file). The function starts to add interactive ability for the user to
;;; choose specific scenarios or tracks through the Tale-Spin world, rather
;;; than to simply specify main character and problem (as with function mspin).
;;; 
;;;
;;; 
;;; Publicly callable functions:
;;;	recover-from-disk re-run-story tdebug say-all print-params reset-params
;;; 
;;; Publicly callable variables:
;;;     *stack-trace* *plan-trace* *conseq-trace*
;;;     *action-trace*
;;;
;;; Files used for I/O:
;;;     recover.file seed.file
;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONSTANTS AND PROGRAM VARIABLES
;;; 


;;; 
;;; Flag specifies whether Tale-Spin has been initialized yet.
;;; 
(defvar *spin-initialized* nil
  "Has Tale-Spin been initialized yet?")


(defvar *tspin-stream* t			; The program variable should be bound to the output stream
  "Output stream for tale-spin.")		; the application uses by calling f.frame-output-var.
						; By default (nil) the output goes to *standard-output*.

;;;
;;; Initialized at the beginning of a session of creating stories or the
;;; beginning of a crash recovery. Assigned a random state by function
;;; init-random-seed called by init-tspin.
;;; 
(defvar *tspin-random-seed* nil
  "Initial random state for a Tale-Spin session.")



(defvar *end-of-file* (gensym)
  "Unique end of file character.")


;;;
;;; A list of flags that control the verbosity level of output
;;; during story generation and initialization. [cox 7aug93]
;;;
(defparameter *flag-list*
	      '(*say-new-knowledge* *say-facts* *say-unobservables*)
  "List of flags that control output-verbosity.")

;;;
;;; t ==> print comments during make-plan calls. [cox 5aug93]
;;;
(defvar *TDebug-On* nil
  "Flag controlling debug info from make-plan.")



;;;
;;; Global variables used by TDebug. They are used to collect inferences from
;;; function assert-fact.
;;;

(defvar *stack-trace* nil
  "Collects all inferences during assert-fact.")

(defvar *plan-trace* nil
  "Collects inferred plans during assert-fact.")

(defvar *conseq-trace* nil
  "Collects inferred consequences during assert-fact.")

(defvar *action-trace* nil
  "Collects inferred actions during assert-fact.")





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CRASH RECOVERY SUPPORT
;;;          AND
;;; EXPERIMENT REPLICATION
;;;
;;;
;;; The following support functions allow the user to recreate stories that
;;; were built in the past. I added this after having the LISP interpreter
;;; crash while trying to debug an important error. [cox 29aug93]
;;; 
;;;
;;; At the beginning of a normal story-generation run, Tale-Spin will print a
;;; "Story ID" reference number that can be used in the future to regenerate
;;; the exact same story. The message it prints is "Recreate story <REF#> by
;;; calling (re-run-story '<REF#>)". This feature is useful in order to track
;;; down bugs (especially intermittent one), or to run a previously established
;;; sequence (for demonstration purposes perhaps).
;;;
;;; The two main user functions are recover-from-disk and re-run-story. The
;;; former is used to reestablish an entire sequence (i.e., a run) of story
;;; generations from a crash-recovery file (with the default name of
;;; recover.file) on disk. It is invoked by the call (recover-from-disk
;;; <File-Name>). The latter function is used to rerun a single story
;;; generation, usually when debugging. As specified by the program's message
;;; at the beginning of a story-generation (see above), the story is
;;; regenerated by the call (re-run-story '<REF#>).
;;;
;;; To recreate a previous run's environment, the user must first call
;;; recover-from-disk. After that point, the user can call re-run-story on any
;;; story id reference number in order to regenerate that particular story.
;;; Because recover-from-disk returns a list of all story ids recovered, the
;;; following calling sequence will actually regenerate an entire sequence of
;;; previously generated stories (with appropriate user prompts):
;;;
;;; (mapcar #'(lambda (each-story-id)
;;; 	        (if (y-or-n-p (format nil
;;; 				      "Run story with id ~s"
;;; 				      each-story-id))
;;; 		    (re-run-story each-story-id)))
;;; 	    (recover-from-disk))
;;;
;;; A crash-recovery file contains in a series of lists the information
;;; necessary to recreate a story-generation run.  A story generation
;;; specification (story-gen-spec) is defined as the quad-tuple (story-id
;;; random-state function-call parameter-value-list). For example:
;;; 
;;; (T0626
;;;  #S(FUTURE-COMMON-LISP:RANDOM-STATE
;;;      :ARRAY #<ARRAY FIXNUM 71 simple 401023643>
;;;      :SIZE 71
;;;      :SEED 804000428
;;;      :POINTER-1 4
;;;      :POINTER-2 39)
;;;  (MSPIN NIL 3 1)
;;;  ((*sniff-success* 100)...(*always* t)))
;;;
;;; In the program, the relevant information is kept on the story id symbol.
;;; The symbol has three properties: The 'seed property has the (implementation
;;; specific) random number seed, the 'spin-call property contains the calling
;;; sequence with which the story was generated, and the 'param-list property
;;; contains a list of tuples, <parameter-name parameter-value>, for each
;;; parameter in Tale-Spin that controls random variation.   For instance, the
;;; above story-gen-spec corresponds to the story id symbol 'T0626 with 'seed
;;; property #S(FUTURE-COMMON-LISP:RANDOM-STATE :ARRAY #<ARRAY FIXNUM 71 simple
;;; 401023643> :SIZE 71 :SEED 804000428 :POINTER-1 4 :POINTER-2 39), 'spin-call
;;; property (MSPIN NIL 3 1), and 'param-list property ((*sniff-success* 100)
;;; ...(*always* t)). Note that to re-spin a previous story, spin must be
;;; called with the previous parameters using either mspin or aspin (hence the
;;; 'spin-call property), the random-number generator must have the previous
;;; random state value (hence the 'seed property), and each parameter in
;;; Tale-Spin that controls random event generation in Tale-Spin must have the
;;; previous value (thus the 'param-list property).
;;; 
;;; The system uses two files:  "recover.file" and "seed.file". The former is
;;; the crash-recovery file containing a line for each story-gen-spec triple
;;; and the latter file has the initial seed for the current set of
;;; story-generatetion runs.
;;;




;;;
;;; Function init-random-seed is called by function init-tspin at the beginning
;;; of a story-generation run. It is used to initialize the seed for the random
;;; number generator used by LISP. It is either used to set it to the
;;; previously saved seed in a particular file (if from-file? is true), or from
;;; scratch. The default filename to save the seed at the beginning of a run in
;;; case of a later crash is "seed.file"
;;;
(defun init-random-seed (&optional from-file? 
				   (file-name metaaqua::*seed-file-name*))
  "Initialize seed from either saved info on disk or from scratch."
  (setf *random-state*  
	(make-random-state 
	  (setf *tspin-random-seed* 
		(make-random-state 
		  (if from-file?
		      (with-open-file 
			(seed-file file-name
				   :direction :input) 
			(read seed-file))
		      t)))))
  (if (not from-file?)
      (with-open-file 
	(seed-file file-name
		   :direction :output
		   :if-exists :new-version)
	(print (make-random-state
		 *tspin-random-seed*)
	       seed-file)))
  )
  


;;;
;;; Function print-story-params is called by function spin to print a message
;;; identifying the story id before the story is output. It also lists the call
;;; necessary to recreate the story at as later time. An important side-effect
;;; is that the function also saves the story information locally and to disk
;;; so that it can be rerun and if a crash occurs, the story sequence can be
;;; recovered. The function returns the random story-id assigned to the story.
;;; 
(defun print-story-params (alt? person-num problem-num
			   &optional (file-name "recover.file"))
  (let* ((function-call 
	   (if (and (null problem-num)
		    (null person-num))
	       '(aspin)
	       `(mspin ,alt? ,person-num ,problem-num)))
	 (story-id
	   (Meta-AQUA::set-story-id
	     Meta-AQUA::*Current-Result-Record*
	     (save-story-info
	       function-call
	       (create-global-param-list)
	       file-name))))
    (format
      *tspin-stream*
      "~%Recreate story ~s by calling (re-run-story '~s)~%" 
      story-id 
      story-id)
    story-id)
  )



;;;
;;; The function print-params is a user function that displays all parameters
;;; in Tale-Spin used to affect variation in story generation. These are the
;;; parameters saved during function save-story-info.
;;; 
(defun print-params (&optional
		     (parameter-names *parameter-names*))
  "Print Tale-Spin program randomness parameters."
  (cond ((null parameter-names)
	 nil)
	(t
	 (format
	   *tspin-stream*
	   "~%~s: ~s~%"
	   (first parameter-names)
	   (symbol-value (first parameter-names)))
	 (print-params (rest parameter-names))))
  )



;;;
;;; Function reset-params reassigns the original values to all program
;;; parameter settings that were is effect at initialize time. See comments on
;;; function recreate-old-parameters.
;;;
(defun reset-params ()
  (recreate-global-params *original-parameters* nil)
  )



;;;
;;; Function recreate-params recursively re-establishes the values for all
;;; Tale-Spin randomness parameters from the argument global-param-list. the
;;; argument is a list of tuples, <parameter-name parameter-value>.
;;; 
(defun recreate-params (global-param-list)
  (cond ((null global-param-list)
	 nil)
	(t
	 (set (first (first global-param-list))
	      (second (first global-param-list)))
	 (recreate-params (rest global-param-list))))
  )


;;;
;;; Function recreate-global-params saves the value of all parameters first (if
;;; flag save-old-values? is non-nil), then re-establishes a set of parameter
;;; values from global-param-list by calling the recursive function
;;; recreate-params.
;;; 
(defun recreate-global-params (global-param-list
			       &optional
			       (save-old-values? t))
  (if save-old-values?
      (setf *old-parameters*
	    (create-global-param-list)))
  (recreate-params global-param-list)
  )


;;;
;;; Function recreate-old-parameters re-establishes the set of Tale-Spin
;;; randomness parameters contained in the global variable *old-parameters*.
;;; Note that it is paramount that recreate-global-parameters be passed a value
;;; of nil for the save-old-values? flag, otherwise *old-parameters* will be
;;; overwritten with the current values before the old-values are
;;; re-established. In such a case, no parameter change will occur and the old
;;; values will be lost .
;;; 
(defun recreate-old-parameters ()
  (recreate-global-params *old-parameters* nil)
  )



;;;
;;; Function create-global-param-list recursively creates a list of Tale-Spin
;;; parameter values from the list of parameter names passes as an argument.
;;; This returned list isa list of tuples, <parameter-name parameter-value>.
;;; No side-effect is performed.
;;; 
(defun create-global-param-list (&optional
				 (parameter-names *parameter-names*))
  (if (null parameter-names)
      nil
      (cons (list (first parameter-names)
		  (symbol-value
		    (first parameter-names)))
	    (create-global-param-list
	      (rest parameter-names))))
  )


;;;
;;; Function save-story-info is the main state-saving function. It stores the
;;; current random state structure and the call that generated the current story
;;; on properties of a unique story identifier. It then saves this information to
;;; disk and returns the story identifier as the result of the function.
;;; 
(defun save-story-info (function-call global-parameter-list
			&optional (file-name "recover.file"))
  (let ((story-id (save-story-seed)))
    (off-to-disk
      (setf (get story-id
		 'spin-call)
	    function-call)
      (setf (get story-id
		 'param-list)
	    global-parameter-list)
      story-id
      file-name)
    story-id)
  )


;;; 
;;; Function save-story-seed generates a unique story identifier and places a
;;; copy of the current random state structure on the seed property of the
;;; story-id. The identifier is returned as the result of the function.
;;; 
(defun save-story-seed ()
  (let ((story-id (gentemp)))
    (setf (get story-id 'seed)
	  (make-random-state 
	   *random-state*))
    story-id)
  )


;;;
;;; Function off-to-disk saves one story-gen-spec to the file named by file-name.
;;; 
(defun off-to-disk (function-call global-parameter-list story-id 
		    &optional (file-name "recover.file"))
  (with-open-file 
      (recover-file file-name
		 :direction :output
		 :if-exists :append
		 :if-does-not-exist :create)
      (print (list 
	      story-id
	      (make-random-state
	       (get story-id 
		    'seed))
	      function-call
	      global-parameter-list)
	     recover-file)
      )
  )


;;;
;;; Function init-recover-file creates a new file to which story-gen-specs may
;;; be saved in the new story generation session. On a Symbolics this function
;;; will create a file adhering to a sequential version series and thus will
;;; produce a unique file name. For example, if the file recover.file.1 is the
;;; newest version, a file named recover.file.2 will be opened. On Unix,
;;; however, the user must assure that all other files named recover.file have
;;; been renamed to other handles, otherwise the old files will be destroyed.
;;; 
(defun init-file (&optional (file-name "recover.file"))
  (with-open-file 
      (recover-file file-name
		 :direction :output
		 :if-exists :new-version
		 :if-does-not-exist :create)
      )
  )



;;;
;;; Function recover-from-disk is the main user-function that reestablishes a
;;; program state from disk so that stories can be re-run. The recover file is
;;; read one story-gen-spec at a time and each record is dissected so that the
;;; random state and function call info can be placed on properties of the
;;; unique story id symbol. See comments on story id data structure above. Once
;;; the user calls this function, the function re-run-story will be able to
;;; recreate stories by their story id reference number.
;;;
;;; The recover-from-disk function takes as optional input a file name from
;;; which the recovery information is found. By default the name is
;;; "recover.file". Local variable story-numbers contains a list of all story
;;; ids recovered from the file. It is printed to the user at the end of the
;;; recovery process. The function returns a list of all story ids recovered.
;;;
(defun recover-from-disk (&optional 
			  (file-name "recover.file"))
;;;   (init-random-seed )				;Initialize random seed from disk.
  (with-open-file 
      (recover-file file-name
		 :direction :input)
      (let ((story-numbers nil))
	(loop (let ((next-list (read 
				recover-file nil 
				*end-of-file* nil)))
		(cond ((eof-p next-list)
		       (format
			*tspin-stream* 
			"~%The following stories were recovered from disk: ~s."
			(reverse story-numbers))
		       (format
			*tspin-stream* 
			(str-concat
			  "~%Call (re-run-story '~s) "
			  "to regenerate the first story, for instance.")
			(first (last story-numbers)))
		       (return (reverse story-numbers)))
		      (t 
		       (setf story-numbers 
			     (cons (first next-list) 
				   story-numbers))
		       (re-establish-story-info
			next-list)))))))
  )




;;; 
;;; Predicate eof-p tests whether the char x is the end of file character. It
;;; is used by the function recover-from-disk.
;;; 
(defun eof-p (x)
  (eq x *end-of-file*))


;;;
;;; Function re-establish-story-info takes one story-gen-spec and
;;; re-establishes the information on the properties of the story identifier.
;;; 
(defun re-establish-story-info (recover-list)
  (let ((story-id (first recover-list)))
    (setf (get story-id 'seed)
	  (make-random-state 
	   (second recover-list)))
    (setf (get story-id
	       'spin-call)
	  (third recover-list))
    (setf (get story-id
	       'param-list)
	  (fourth recover-list))
    )
  )


;;;
;;; Function re-run-story is the main user function for recreating a previous
;;; story during the current story-generation run. The input story-id is that
;;; number provided as output by the Tale-Spin program just prior to generating
;;; the original story the user wishes to re-run. The story is re-called only
;;; if the story random state structure is restored.
;;; 
(defun re-run-story (story-id)
  (and (restore-story-state story-id)
       (re-call story-id))
  )


;;;
;;; Function restore-story-state re-establishes the random number seed from the
;;; state information attached to the story-id symbol. If, however, the
;;; story-id is notlegitimate, the seed property will be nil. If so a error
;;; message will be printed to the user including the bogus story id.
;;; 
(defun restore-story-state (story-id)
  (cond ((get story-id 'seed)
	 (recreate-global-params
	   (get story-id 'param-list))
	 (setf *random-state* 
	       (make-random-state 
		 (get story-id 'seed))))
	(t
	 (format *tspin-stream*
		 "Story number ~s does not exist."
		 story-id)))
  )


;;;
;;; Function re-call regenerates the previous story having the story-id
;;; reference number. The function call form which performs this operation is
;;; attached to the story id data structure.
;;;
(defun re-call (story-id)
  (let ((function-call (get story-id 'spin-call)))
    (apply (first function-call)
	   (rest function-call)))
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; T(race)DEBUG FACILITY
;;;
;;; The Trace Debug facility provides more information to the programmer or
;;; user when spinning stories. Turning on TDebug results in two features.
;;; First, all plans are printed along with their parameters when invoked.
;;; Because plans are often invoked by consequence functions that are
;;; implemented as macros, and because these macros expand into functions of
;;; non-traceable (by ordinary trace) names, the use of this feature allows the
;;; user to see what code is actually being executed at run-time. Secondly,
;;; with TDebug on the program will separate the English sentences into time
;;; intervals as they occur. This provides additional information that may help
;;; the programmer debug additions to the knowledge base.
;;;
;;; To turn on TDebug features, simply call (tdebug). This invocation actually
;;; toggles the active status of TDebug, so calling it a second time will turn
;;; it off. In addition to the features described above, passing the function
;;; an optional argument of t will cause all flags in the *flag-list* program
;;; parameter to be set to true. These flags control the verbosity level of
;;; output during program execution. When TDebug is turned off without the
;;; optional argument, they are all reset to their original values. Note that
;;; one could therefore turn TDebug off, yet leave the flags to verbose by
;;; calling TDebug twice with a t argument.
;;;


;;; 
;;; Function gen-flag-settings takes as input a list of global flags, producing
;;; as output a list of flag settings. The flag settings are tuples:
;;; (flag-name flag-value).  [cox 7aug93]
;;;
(defun gen-flag-settings (flag-list)
  (mapcar 
   #'(lambda (each-flag)
       (list each-flag 
	     (symbol-value each-flag)))
   flag-list))


;;; 
;;; Constructs the function "tdebug" that toggles the debug flag controlling
;;; whether or not comments are printed when plans are generated. When the
;;; debug flag (*TDebug-On*) is toggled to true and verbose? is passed as true,
;;; all global output control flags listed in *flag-list* are set to true as
;;; well.  Since construct-tdebug returns a closure the resultant function can
;;; remember the original values of the output control flags. So when the debug
;;; flag is toggled back to nil the output control flags are returned to their
;;; initial settings if verbose? is nil. If the user wishes to toggle the debug
;;; flag off but leave all other flags true, then verbose? is passed as t (or
;;; non-nil).  [cox 5,6&7aug93]
;;; 
(defun construct-tdebug (initial-flag-settings)
  (function 
   (lambda (&optional verbose?)
     (setf *TDebug-On* (not *TDebug-On*))
     (cond ((and *TDebug-On*
		 verbose?)
	    (mapc 
	     #'(lambda (each-flag-setting)
		 (set (first each-flag-setting)
		      t))
	     initial-flag-settings))
	   ((not verbose?)
	    (mapc 
	     #'(lambda (each-flag-setting)
		 (set (first each-flag-setting)
		      (second each-flag-setting)))
	     initial-flag-settings)
	    )))))

;;; 
;;; Initialize the tdebug function by calling the constructor function above. 
;;; Passes the constructor a generated list of original flag-settings. 
;;; [cox 7aug93]
;;;
(defun init-tdebug (flag-list)
  (setf (symbol-function 'tdebug) 
	(construct-tdebug 
	 (gen-flag-settings flag-list)))
  ;; String assigned to the value of tdebug below after 
  ;; this function sets tdebug's function definition.
  "tdebug is a closure that controls output while remembering initial flag settings.")



;;;
;;; This is not a program variable, but rather instantiates the tdebug
;;; function.  Defvar will not reinterpret the initial value so the initial
;;; flag settings will not be changed if the file is reloaded. [cox 7aug93]
;;;
;;; |||||| Actually, the interpreter on a Symbolics WILL change the value, but
;;; it will issue a warning. The user should be aware of this when making any
;;; assumptions. [cox 16sep93]
;;;
(defvar tdebug (init-tdebug *flag-list*))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ADDITIONAL TRACE INFORMATION
;;;
;;; Whenever assert-fact is called to instantiate a new action or state cd, it
;;; performs inference by forward chaining all additional plan, consequences
;;; and actions licensed by the asserted fact. If Trace Debug is on at fact
;;; assertion time, then all inferred cds are saved to a set of public global
;;; variables.
;;;
;;; 	  *stack-trace*  *plan-trace*  *conseq-trace*  *action-trace*
;;; 
;;; The *stack-trace* collects all inferences. The other three contain specific
;;; inferences. The user takes advantage of this information through direct
;;; inspection of the variables.
;;; 


;;; 
;;; Function init-traces resets the trace variables to nil.  [cox 10aug93]
;;; 
(defun init-traces ()
  "Reset trace variables."
  (setf *stack-trace* nil)
  (setf *plan-trace* nil)
  (setf *conseq-trace* nil)
  (setf *action-trace* nil)
  )


;;;
;;; Function save-trace squirrels inference information into a set of public
;;; global variables. [cox 10aug93]
;;; 
(defun save-trace (stack-item stack-type)
  (when *TDebug-On*
	(push 
	 (list *cd* stack-item stack-type)
	 *stack-trace*)
	(case stack-type
	      (plan (push 
		     (list *cd* stack-item) 
		     *plan-trace*))
	      (conseq (push 
		       (list *cd* stack-item) 
		       *conseq-trace*))
	      (action (push 
		       (list *cd* stack-item) 
		       *action-trace*))))
  )



;;; 
;;; Created this function and added trace mechanism to this and the related two
;;; functions add-conseq and add-action (in tspin.lisp). [cox 10aug93]
;;;
(defun add-plan (x)
  (do-break  add-plan )
  (save-trace x 'plan)
  (push x *plans*) 
  ;; I did dot return x because I am replacing calls of push.
  )					




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PRECONDITIONS CHECKING
;;; 

;;;
;;; Function check-preconditions adds explicit checking of the preconditions of
;;; an act before that act is carried out by characters in stories. The
;;; addition of this check was necessitated by the non-deterministic nature of
;;; stories Tale-Spin now generates. Previously, if a plan had all
;;; preconditions met during planning, then no random event inserted by the
;;; system as the plan was executed would change the state of the world so that
;;; the preconditions no longer were true. Now, however, because of random
;;; events like the police arriving, steps in a plan, such as getting an
;;; illegal pipe from the cupboard, may no longer be appropriate.
;;;
;;; Therefore function doit checks a cd's preconditions before calling
;;; assert-fact. Check-preconditions returns non-nil if the *cd* has no
;;; preconditions function or if the preconditions function for that cd returns
;;; non-nil. The preconditions functions are established by a new macro,
;;; def-preconds, below.
;;; 
;;; Note the parallel of the check-preconditions function to the conseqs
;;; function.  [cox 27aug93]
;;;
(defun check-preconditions (*cd*)
  "Check the preconditions of an act before instantiating it."
  (do-break  check-preconditions )
  (let ((proc (get (old-cd-head *cd*)
		   'precond-fn)))
    (if proc (funcall proc)
      t))					; If there are no preconditions to check, then ok.
  )


;;;
;;; Macro to create precondition functions for cds. [cox 27aug93]
;;;
;;; This macro is much like the macro def-conseqs in file tspin.lisp. See
;;; comments on function check-preconditions above.
;;; 
(eval-when (load eval compile)
  (defmacro def-preconds (name &body body)
    (let ((fname (intern (format nil "~A-PRECONDS" name))))
      `(progn (setf (get ',name 'precond-fn) ',fname)
              (defun ,fname () 
		(do-break ,fname)
		,@body)))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MISCELLANEOUS FUNCTIONS AND PREDICATES
;;;

;;; 
;;; Function say-all takes the complete cd representation for the newly
;;; generated story and reprints it in English. The cds are in reverse order on 
;;; the global variable *ALL*. [cox 8aug93]
;;;
(defun say-all (&optional (all *all*))
  "Display the English interpretation of the current story."
  (terpri *tspin-stream*)
  (mapc #'(lambda (each-sublist)
	    (mapc #'(lambda (each-item)
		      (say each-item nil))
		  each-sublist))
	(reverse all))
  (terpri *tspin-stream*)
  )

;;; 
;;; Predicate does-have-p returns true if agent has object, nil otherwise.
;;; [cox 11aug93]
;;;
(defun does-have-p (object agent)
  (cond ((eq agent
	     (knows-owner 'world object))
;;; 	 (format *tspin-stream*
;;; 		 "~%Agent ~s does have object ~s. " agent object)
	 t)
	(t
;	 (format *tspin-stream*
;		 "Agent ~s does not have object ~s. "
;		 agent
;		 object)
	 (let ((real-loc-statement (knows? 'world (who-has object))))
	   (if real-loc-statement
	       (say real-loc-statement nil)
	     (format *tspin-stream*
		     " No one knows who has object. ")))
;	 (format *tspin-stream*
;		 "~%The location of the object is ~s."
;		 (loc object))
;	 (format *tspin-stream*
;		 "~%Call of knows-owner: ~s."
;		 (knows-owner 'world object))
	 nil)))



;;; 
;;; Predicate-like function that returns the owner of the object if the owner
;;; is a person, otherwise returns nil. [cox 26aug93]
;;;
(defun person-controls? (object)
  (let ((owner (knows-owner
		      'worldl
		      object)))
    (if (is-a  owner 'person)
	owner)
    )
  )


;;;
;;; Function is-open? acts like a predicate, returning non-nil if the object
;;; (either container or faucet) is open, nil otherwise. [cox 2mar95]
;;; 
(defun is-open? (object)
  (is-true?
    (state object
	   (if (is-a object
		     'faucet)
	       'flowing
	       'open) 'pos))
  )

;;; 
;;; Because a location can be an object, sometimes we need to collect the
;;; transitive closure of possible locations. Function all-locations-of
;;; performs this collection.
;;; 
(defun all-locations-of (object &optional (all-locations nil))
  (let ((location (loc object)))
    (if (or (null location)
	    (member location all-locations))
	all-locations
	(all-locations-of
		location
		(cons location all-locations))))
  )

;;;
;;; Predicate police-present-p returns true if police are located in the same
;;; location as that of the object passed as argument. If the argument itself
;;; is a police character then nil is returned. The predicate is intended to
;;; signal the presence of police when one is worried about the state. Police
;;; themselves have other concerns.
;;; 
(defun police-present-p (object)
  (if (is-a object 'police)
      nil
    (some
      #'(lambda (each-location)
	  (some
	    #'(lambda (each-object)
		(is-a
		  each-object
		  'police))
	    (return-all-items-at-loc
	      'world
	      each-location)))
      (all-locations-of object)))
  )


;;;
;;; Predicate under-anothers-control-p returns true if argument object is
;;; controlled by another agent other than person. This includes the case where
;;; the object is the person, and the caller is testing whether the person
;;; controls himself or he is under another's control or custody.
;;; 

(defun under-anothers-control-p (object person)
  (let ((controller (knows-owner
		      'world
		      object)))
	(not
	  (or (null controller)
	      (eq person controller))))
  )

;;;
;;; Function return-all-items-at-loc returns a list of all the items known by
;;; the knower argument to be at location argument. [cox 12aug93]
;;;
(defun return-all-items-at-loc (knower location)
  (return-filler-list				; Return a list of 
   'actor					; actor [sic] fillers from
   (all-patterns				; Return a list of cds
    (whats-at location)				; that match this pattern
    (get-concepts knower 'loc))))		; from the list of locations that knower knows.



;;; 
;;; Function return-filler-list runs through all cds in cd-list, returning a
;;; list of all fillers of slot specified by role for each cd of the cd-list.
;;; Note that if a particular cd does not have such a slot then this nil filler
;;; is filtered out of the returned list. [cox 12aug93]
;;; 
(defun return-filler-list (role cd-list)
  (mapcan					;Mapcan used as a filter for cd-list.
   #'(lambda (each-cd)
       (let ((filler				;Get role's filler.
	       (role->filler 
		 role 
		 each-cd)))
	 (if filler				;Collect non-nil fillers.
	     (list filler))))
   cd-list))


;;; 
;;; Function all-patterns, unlike function pat-member which returns the first
;;; matching cd, returns all matching cds in the cd-list matching pat.
;;; [cox 11aug93]
;;;
(defun all-patterns (pat cd-list)
  (do-break all-patterns)
  (let ((matches nil))
    (dolist (each-cd cd-list (reverse matches))
	    (if (match pat each-cd)
		(setf matches 
		      (cons each-cd matches))))))


;;;
;;; Function role->filler returns the filler of the role in cd.  [cox 16aug93]
;;; 
(defun role->filler (role cd)
  (funcall
   (role->access-fn role)
   cd))


;;;
;;; Function role->access-fn returns the access function for cd slots of type
;;; named by role.  [cox 12aug93]
;;; 
(defun role->access-fn (role)
  (nth 
   (- (length *roles*)
      (length
       (member role
	       *roles*)))
   *role-functions*))



(defvar *force-original-demo* nil
  "If t, throw balloon, when entertainment is plan.")

(defvar *force-balloon-throwing* nil
  "If t, throw balloon, when entertainment is plan.")

(defvar *force-ball-throwing* nil
  "If t, throw ball, when entertainment is plan.")



;;;
;;; The function user-control-prog-parameters allows the user to change all of
;;; the program parameters that affect interesting output.
;;; 
(defun user-control-prog-parameters ()
  (format
    *tspin-stream*
    "~%~%Choose Program Parameters at the Following Prompts ~% ~%")  
  (format
    *tspin-stream*
    "~%*random-odds* (default 20) ~% Chance that random events will occur in the story ~%")
  (setf *random-odds*
	(get-between-range 0 100))
  (format
    *tspin-stream*
    "~%*allow-random-events* (default nil) ~% T -> Random events can occur according to *random-odds* ~%")
  (setf *allow-random-events*  (read
;;; 				 *tspin-stream*	; Will not work in Harlequin LISP
				 ))
  (format
    *tspin-stream*
    "~%*sniff-success* (default 95) ~% Percentage of time that dog will correctly locate contraband ~%")
  (setf *sniff-success*
	(get-between-range 0 100))
  (format
    *tspin-stream*
    "~%*dog-speak-freq* (default 0) ~% Percentage of time that dog will speak  ~%")
  (setf *dog-speak-freq*
	(get-between-range 0 100))
  (format
    *tspin-stream*
    "~%*officer-bark-orders-freq* (default 0) ~% Chance officer will 'bark' orders ~%")
  (setf *officer-bark-orders-freq*
	(get-between-range 0 100))
  (format
    *tspin-stream*
    "~%*officer-bark-freq* (default 0) ~% Chance that officer will bark like a dog ~%")
  (setf *officer-bark-freq*
	(get-between-range 0 100))
  (format
    *tspin-stream*
    "~%*comply-freq*  (default 90) ~% Chance of suspect handing over contraband if asked ~%")
  (setf *comply-freq*
	(get-between-range 0 100))
  (format
    *tspin-stream*
    "~%*bark-at-contraband-freq* (default 99) ~% Chance dog will bark at contraband ~%")
  (setf *bark-at-contraband-freq*
	(get-between-range 0 100))
  (format
    *tspin-stream*
    "~%*search-success-rate* (default 80) ~% Chance cop will find contraband ~%")
  (setf *search-success-rate*
	(get-between-range 0 100))
  (format
    *tspin-stream*
    "~%*amount-threshold* (default 3) ~% Pot amount (out of 10) that Elvis won't get busted for ~%")
  (setf *amount-threshold*
	(get-between-range 0 10))
  (format
    *tspin-stream*
    "~%*annoy-dog-freq* (default 0) ~% If Elvis is bored, chance he will pester dog ~%")
  (setf *annoy-dog-freq*
	(get-between-range 0 100))
  (format
    *tspin-stream*
    "~%*no-spills* (default t) ~% T -> will not drop pipe when lighting up ~%")
  (setf *no-spills* (read
;;; 				 *tspin-stream*	; Will not work in Harlequin LISP
		      ))
  (format
    *tspin-stream*
    "~%*high-4-entertainment* (default 80) ~% Chance Elvis would rather get high than otherwise entertain himself ~%")
  (setf *high-4-entertainment*
	(get-between-range 0 100))
  )


;;;
;;; Function init-yarn-vars is a simple initialization function to set global
;;; variables to nil at the end of function spin.
;;; 
(defun init-yarn-vars ()
  (setf *force-original-demo* nil)
  (setf *force-ball-throwing* nil)
  (setf *force-balloon-throwing* nil)
  )

;;;
;;; The function specify yarns is a interactive mechanism for allowing the
;;; program user to specify which tale to spin. It gives limited control over
;;; the stories generated for Meta-AQUA and is called by function spin if
;;; passed a non-nil value to the pick-yarn? parameter. The user can control
;;; this event by the pick-yarn? parameter of user function spinqua. The spin
;;; function is controlled by determining the character and goal parameters
;;; passed to it (person-num and problem-num, respectively). It also controls
;;; the special variables *force-original-demo*, *force-ball-throwing*,
;;; *force-balloon-throwing*.
;;; 
(defun specify-yarns (person-num problem-num &optional control-num)
  (do-break specify-yarns)
  (reset-params)
  (if (not (numberp control-num))
      (format
	*tspin-stream*
	"~%~%Choose the following yarn to spin:~%"))
  (if (and (null person-num)
	   (null problem-num))
      (case
	(if (numberp control-num)
	    control-num
	    (pick-one '((Bust Story with K-9)
			(Bust Story without K-9)
			(Dad plays with ball)
			(Kids play with balloon)
			(Dad smokes tobacco)
			(Spin Drug Bust story as in original demo)
			(Random Wild Story)
			(Random Boring Story)
			(Allow user control over when to insert random event)
			(Elvis teases dog)
			(Mom drinks)
			(Simple choose from problem menu)
			(Full program parameter control)
			)))
	(0 (force-event '(k-9-squad-arrive
			   officer1 police-dog1)))
	(1 (force-event '(police-arrive
			   officer1)))
	(2 (setf person-num 3)(setf problem-num 2)
	   (setf *force-ball-throwing* t))
	(3 (setf person-num 1)(setf problem-num 2)
	   (setf *force-balloon-throwing* t))
	(4 (setf person-num 3)(setf problem-num 0))
	(5 (setf person-num 4)(setf problem-num 0)
	   (setf *force-original-demo* t))
	(6 (make-wild-story)
	   (setf *auto?* t))
	(7 (make-wild-story nil)
	   (setf *auto?* t))
	(8 (setf *allow-random-events* t)
	   (setf *request-for-random-event* t)
	   (setf *random-odds* 0)
	   )
	(9 (setf *annoy-dog-freq* 100)
	   (setf *high-4-entertainment* 0)
	   (setf person-num 4)(setf problem-num 2)
	   )
	(10 (setf person-num 2)(setf problem-num 0))
	(11 (setf *auto?* nil))
	(12 (user-control-prog-parameters)
	    (setf *auto?* nil))
	))
  (values person-num problem-num)
  )




;;;
;;; Function get-between-range reads user input for an integer
;;; between a given upper and lower bounds (inclusive).
;;; 
(defun get-between-range (lower-bound upper-bound)
  (loop
    (let ((response (read
;;; 				 *tspin-stream*	; Will not work in Harlequin LISP
		      )))
      (if (and (integerp response)
	       (<= lower-bound response upper-bound))
	  (return response)))
    (format *tspin-stream* "~%Out of range. Try again.~%"))
  )

;;;
;;; Function yspin (yarn-spin) invokes the spin function so that the function
;;; specify-yarns above will be executed. Thus the user interacts with the menu
;;; in order to choose particular types of stories, rather than simply setting
;;; certain parameters.
;;; 
(defun yspin (&optional
	      (recover-file-name		; optional file name for tspin crash recovery.
		"recover.file"))
  (if (not *spin-initialized*)
      (init-tspin
	nil
	recover-file-name))
  (spin nil nil nil nil t
	recover-file-name)
  )


;;;
;;; Function force-event sets a few global flags that force function say to
;;; insert a particular event when the function decides whether to insert some
;;; "random" event.
;;; 
(defun force-event (event)
  (or *allow-random-events*
      (setf *allow-random-events*
	    t))
  (setf *requested-event* event)
  (setf *request-for-random-event* t)
  (setf *no-prompt* t)
  )


;;;
;;; Function control-reset reset the global flags that force function say to
;;; insert a particular event. After this call the insertion will not reoccur
;;; during a subsequent call to function say.
;;; 
(defun control-reset (&optional allow-further-random-events)
  (setf *requested-event* nil)
  (setf *request-for-random-event* nil)
  (setf *no-prompt* nil)
  (if (not allow-further-random-events)
      (setf *allow-random-events* nil))
  )



;;;
;;; See comments on f.frame-output-var for explanation.
;;; 
(defun tspin-output-var (user-stream)
  "Cause all output to tspin stream to go to user stream."
  (setf *tspin-stream*
	;;Commented out and added t [cox 6mar97]
;	(make-synonym-stream
;	  user-stream)
	t)
  )

;;;
;;; Function init-tspin prepares Tale-Spin so that it is able to 
;;; write stories to Meta-AQUA. In order to do this the function must 
;;; initialize Tspin's world and then create the frame tokens that
;;; represent the objects, locations, and characters within Tspin. 
;;; [cox 24aug93]
;;;

(defun init-tspin (&optional
		   from-file?			; t -> init tspin random number seed from file.
		   recover-file-name)
  (format
    *tspin-stream*
    "~%~%Initializing Tale-Spin~%~%")
  (setf *spin-initialized* t)
  (setf *print-array* t)			; So that arrays in the crash-recovery file are readable.
  (in-package 'TSPIN)
  (if (null *original-parameters*)
      (setf *original-parameters*
	    (create-global-param-list)))
  (init-random-seed from-file?)
  (init-file recover-file-name)					; Initialize crash recovery file.
  (init-world)
  (instantiate-frame-tokens)
;;;   (compile 'spin)				; Recompile so i/o works right.
  )




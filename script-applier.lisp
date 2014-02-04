;;; -*- Mode: LISP; Syntax: Common-lisp; Package: META-AQUA; Base: 10 -*-

(in-package :metaaqua)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	    The Meta-AQUA Introspective Multistrategy Learning System
;;;;				   Version 6
;;;;
;;;;	     Copyright (C) 1996   Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;			   File: script-applier.lisp
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
;;                    SCRIPT APPLICATION CODE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; 
;;; Assumptions:
;;;   1 - all scripts have instrumental-scene, which must be fully matched for the
;;;       script to apply.
;;;  mark d.

;;;
;;; |||||| The code really needs to be optimized. For example, there is waste
;;; when every time the program checks for matches it creates new
;;; instantiations of the scripts and scenes. Since can-unify-p does not change
;;; the original instantiation, but makes its own copies, there is no harm in
;;; keeping instantiated script candidates around until the instrumental-scene
;;; is matched.  In the short-run, could prevent some processing by not
;;; searching for a match when the next input story concept is a state or
;;; relation, since the script-applier ignores them anyway. [cox 27oct94]
;;;
;;; 
;;; |||||| Issues remaining: 
;;;
;;;    1. Realistically, some scenes will not be present because the results of
;;;    the actions will be already present in the world. For example, the
;;;    precondition to getting something from the cupboard requires that the
;;;    agent be at the cupboard. In those cases where this is already true,
;;;    Tale-Spin will not generate the action; in other cases it will. Note for
;;;    the time being, I have commented out the gain-proximity-to scene in the
;;;    gain-control-of-contained-object schema. [cox 27oct94]
;;;
;;;    2. Some scenes have multiple variations. For instance, when getting the
;;;    ganja from the cupboard the scene should be a gain-control-of-contained-
;;;    object; whereas when getting it from a vase it will be a gain-control-
;;;    of-object, both of which are instances of frame gain-control-of.
;;;    [cox 27oct94]
;;;
;;;    3. It would be easy to have the script applier detect and unify
;;;    main-results states and relations. Would have to modify the routing that
;;;    skips states and relations though.  [cox 27oct94]
;;; 




(defconstant *match-tag-property* 'match-tag
  "The property to tag symbols as being matched when checking for script application.")


;;; 
;;; Scripts Meta-AQUA knows about.
;;;
;;; Changed from defvar.  Added get-a-smoke. [cox 26oct94]
;;; Changed name to smoking-script (along with names of other scripts) [cox 26feb95]
;;;
;;; Added get-a-bite-from-fridge and get-a-bite-from-table. [cox 28dec94]
;;; |||||| Remove eat-something in future. It is flawed. [cox 28dec94]
;;; 
(defparameter *known-script-names*  '(
  				      playing-catch-script
 				      get-a-drink
 				      eat-something
  				      burglarize-script ;Added for GOBAT/D [cox 12oct99]
				      smoking-script
 				      get-a-bite-from-fridge
 				      get-a-bite-from-table
 				      cops-easy-bust-script
 				      )
  "Names of scripts Meta-AQUA knows about.")


(defvar *script-list* nil
  "Candidate scripts to match against during script identification.")


;;; Current active script
;;; 
(defvar *current-script* nil
  "Current active (identified) script.")



;;; Initialize global vars
;;; 
(defun init-script-applier
       (previous-mode
	&aux
	(current-matched-scripts
	  (if (null previous-mode)
	      *known-script-names*
	      (if (not (null *Eval-Results*))
		  (result-record-matched-scripts-list
		    (first *Eval-Results*))))))
  "Initialize global vars on *script-list* for script applier."
  (setf *current-script* nil)
  (if
    current-matched-scripts
    (setf *script-list*
	  (mapcar
	    #'(lambda				; The lambda function performs all side-effect
		(each-script			; during the calculation of
		  &optional			; the optional parameter.
		  (script-instantiation
		    (cond
		      ((member			; NOTE: true every time if previous-mode = nil
			 each-script
			 current-matched-scripts)
		       (format
			 *aqua-window*
			 "~%~%Initializing script ~s.~%"
			 each-script)
		       (initialize-instrumental-scenes
			 (f.make-new-instance
			   (frame-def each-script))))
		      (t
		       each-script))))
		script-instantiation)
	    (if (null previous-mode)
		*known-script-names*
		*script-list*))))
  )


;;;
;;; Function initialize-instrumental-scenes takes a list of subscenes of some
;;; script (assumed to have 'scenes slot if script), and recursively expands
;;; (instantiates and unifies subscenes) instrumental scenes of instrumental
;;; scenes.  This is useful to keep from making many copies to match for script
;;; activation. [29oct94]
;;;
;;; The function returns the script passed to it (which is necessary for the
;;; original calling function, that is, function init-script-applier. All
;;; side-effect is performed by function expand-subscenes.
;;; 
(defun initialize-instrumental-scenes
       (script
	&optional (scene-list (f.get
				script
				'scenes)))
  (cond ((null scene-list)
	 nil)
	((has-subscenes-p
	   (expand-subscenes
	     (first scene-list)
	     nil))				; No lazy matching. [24feb95]
	 (initialize-instrumental-scenes
	   script
	   (f.get (first scene-list)
		  'scenes))
	 (initialize-instrumental-scenes
	   script
	   (rest scene-list)))
	(t
	 (initialize-instrumental-scenes
	   script
	   (rest scene-list))))
  script
  )



;;; Top-level script applier
;;; returns t if world my-model altered, nil if not
;;; 
;;; |||||| Will not return t I think. Mark probably means non-nil. Changed
;;; local variable the-script to found-script. Removed the redundant test for
;;; (if (not (null the-script)) ...) ; replaced with (if found-script ...)
;;; [cox 26oct94]
;;;
;;; Now returns t only if new script instantiated, otherwise nil. [cox 30dec94]
;;; The function (now predicate) also has the side-effect of setting the second
;;; and third fields of the global variable *Current-Result-Record*. The third
;;; field is a flag representing whether or not a script has been matched in
;;; the current story; whereas the second is a list of such scripts. It also
;;; updates the global list of matched scripts, *Matched-Scripts*. [cox 8jan95]
;;; 
(defun scriptify (&optional (w-m *World-model*))
  (do-break scriptify)
  (let ((my-model (get-model w-m)))
    (cond
      ;; Try to match an instrumental scene with the world to activate a script.
      ((null *current-script*)
       (let ((found-script (find-script
			     (reverse my-model)	; Earliest concepts at the front of the list now.
			     *script-list*)))
	 (format *aqua-window*
		 "~%Checking for match...")
	 (when found-script
	   (setf *Matched-Scripts*
		 (cons found-script
		       *Matched-Scripts*))
	   (set-script-matched-flag
	     *Current-Result-Record*)
	   (set-matched-scripts-list
	     *Current-Result-Record*
		 found-script)
	   (unify-instrumental-scene my-model found-script)
	   t)))
      ;; If already have an active script, then use to it match subsequent scenes.
      (t
       (if (can-match-current-script
	     (first my-model)
	     (list *current-script*))
	   (unify-scene (first my-model)
			(list *current-script*)
			t))			; t -> Lazy matching
       nil)))
  )


;;; MATCHING FUNCS

;;;
;;; Function find-script returns the script whose instrumental scene can be
;;; unified with events in world my-model, nil otherwise. [cox 26oct94]
;;; 
;;; NOTE: function find-script is used to locate a candidate for an initial
;;; active-script; whereas function can-match-current-script is used to find
;;; matches thereafter.  [cox 26oct94]
;;;
(defun find-script (my-model scripts)
  ;; No real need to use a local variable here. [cox 26oct94]
  (let ((instrumental-scene
	  (first (f.get
		   ;; make-new-instance commented out because it is now done at initialize time. [cox 29oct94]
;;; 		   (f.make-new-instance (*frame*
		   (first scripts)
		   'scenes))))
    (do-break find-script)
    (cond ((null scripts) nil)
	  ((can-match-instrumental-scene
	     my-model
;;; 	     (get-scenes instrumental-scene) ; |||||| These subscenes will not be instantiated. [cox 26oct94]
	     (list instrumental-scene))	
	   (first scripts))
	  (t
	   (remove-match-tags my-model)
	   (remove-match-tags
	     (build-subscene-list
	       (list
		 (first
		   (f.get (first scripts)
			  'scenes)))
	       t))
	   (find-script my-model
			(rest scripts))))))




;;;
;;; In order for find-script to activate an initial script during script
;;; application, function can-match-instrumental-scene must match all subscenes
;;; of the instrumental-scene (or the first scene) of a given script. This is a
;;; stronger criterium than that used for instantiating subsequent scenes once
;;; a script is active.
;;; 
;;; Confusing to have the local variable called my-model, which is the name of
;;; the first parameter to the function. So I renamed it to something more
;;; meaningful --current-model.  [cox 26oct94]
;;; 
;;; Modified the function to tag each scene matched so that the subsequent
;;; unify function will not have to repeat the matching process. [cox 10jan95]
;;; 
(defun can-match-instrumental-scene (my-model instrumental-scene-components)
  (let ((current-model (skip-2-next-story-concept
			 my-model)))
  (do-break can-match-instrumental-scene)  
    (cond ((null instrumental-scene-components)
	   t)
	  ((null current-model) 
	   nil)
	  ;; We test for a match before testing for subscenes since the match 
	  ;; may be at a large grain size, rather than the finest. [cox 10jan95]
	  ((can-unify-p (first current-model)
			(first instrumental-scene-components))
	   (setf (get (first current-model)
		      *match-tag-property*)
		 t)
	   (setf (get (first instrumental-scene-components)
		      *match-tag-property*)
		 t)
	   (can-match-instrumental-scene
	     (rest  current-model)
	     (rest instrumental-scene-components)))
	  ((has-subscenes-p
	     (first
	       instrumental-scene-components))
	   (can-match-instrumental-scene
	     current-model
	     (append
	       (f.get
		 (first
		   instrumental-scene-components)
		 'scenes)
	       (rest
		 instrumental-scene-components)))
;	   (and (can-match-instrumental-scene
;		  current-model
;		  (f.get (first
;			   instrumental-scene-components)
;			 'scenes))
;		(can-match-instrumental-scene
;		  ;; The following skip function assumes that there will be no intervening irrelevant
;		  ;; actions (ones that do not match any subscenes of the first instrumental scene component).
;		  (skip-n-story-concepts
;		    (length
;		      (f.get
;			(first
;			  instrumental-scene-components)
;			'scenes))
;		    current-model)
;		  (rest instrumental-scene-components))
;		)
	   )
	  (t
	   (can-match-instrumental-scene
	     (rest  current-model)
	     instrumental-scene-components))))
  )


;;;
;;; Predicate has-subscenes is used to test whether or not the given frame has
;;; a list of subscenes in its scenes slot.  [cox 27oct94]
;;; 
(defun has-subscenes-p (frame)
  "Return t if frame has scenes slot, nil otherwise."
  (f.get frame 'scenes)
  )


;;;
;;; Function expand-subscenes guarantees that it will return the frame parameter
;;; passed to it originally. If the frame has no (sub)scenes, no side-effect will
;;; occur.
;;; 
(defun expand-subscenes (frame lazy? &optional (scene-list (f.get frame 'scenes)))
  (cond ((null scene-list)
	 frame)
	(t
	 (f.unify (f.instantiate-frame
		    (frame-def (first scene-list)))
		  (first scene-list)
		  t nil				; Use defaults so we can pass lazy? [24feb95]
		  lazy?)
	 (expand-subscenes
	   frame
	   lazy?
	   (rest scene-list))))
  )


;;;
;;; Function can-match-current-script is used to check for matches only if a
;;; script has been previously activated and is current. [cox 27oct94]
;;;
;;; The scene-list parameter used to be called "script."  [cox 27oct94]
;;; 
(defun can-match-current-script (story-concept scene-list)
  (do-break can-match-current-script)
  (cond ((null scene-list)
	 nil)
	((can-unify-p story-concept
		      (first scene-list)
		      t)			; Use lazy matching so that siblings will match.
	 ;; Added the following feedback. [cox 27oct94]
	 (format
	   *aqua-window*
	   "~%Matched story concept ~s with scene ~s.~%"
	   story-concept (first scene-list))
	 t)
	(t
	 ;; The following will probably get into an infinite loop if it cannot unify with the first
	 ;; subscene that has no scene list of its own (that is, a primitive subscene) since it will
	 ;; be appending nil onto the scene-list, leaving the first scene at the front of the list.
	 ;; |||||| [cox 27oct94]
	 (can-match-current-script
	   story-concept
	   (append
;	     (get-sub-scenes			; Replaced this call with the expand-subscenes
;	       (first scene-list))		; call below. [cox 27oct94]
	     (f.get
	       (expand-subscenes
		 (first scene-list)
		 t)				; Use lazy matching here too. [cox 24feb]
	       'scenes)
	     (rest scene-list)))))
  )


;;;
;;; Function build-subscene-list will construct a list of all subscenes of a
;;; given script (or frame). Note that one must pass the original script as a
;;; list. Thus (build-subscene-list (list my-script)) will produce a linear
;;; order of subscenes. On the other hand, unify-instrumental-scene calls
;;; (build-subscene-list (list (f.get my-script 'instrumental-scene)), which
;;; will return a list of a subscenes of the instrumental scene so that they
;;; can be unified with the world model. Note that if a subscene is not fully
;;; instantiated, so that its scenes list is not present, even though its
;;; conceptual definition has a default, the subsubscenes will not be added.
;;; [cox 10jan95]
;;; 
(defun build-subscene-list (subscene-list &optional match-sensitive?)
  (if (null subscene-list)
      nil
      (let ((first-item
	      (first subscene-list)))
	(append
	  (if (and (if match-sensitive?
		       (not (get first-item
				 *match-tag-property*))
		       t)
		   (has-subscenes-p
		     first-item))
	      (build-subscene-list
		(f.get
		  first-item
		  'scenes))
	      (list first-item))
	  (build-subscene-list
	    (rest subscene-list)))))
  )



;;; UNIFICATION FUNCS

;;;
;;; During scriptify, if function find-script locates a script that matches,
;;; function unify-instrumental-scene unifies the first scene (usually the
;;; instrumental-scene) of the script with actions from the *World-Model*.
;;; [cox 26oct94]
;;;
;;; |||||| Argument my-model never used.  [cox 27oct94]
;;; 
(defun unify-instrumental-scene (my-model script-name)
  (format *aqua-window*
	  "~%Instantiating script ~A!!"
	  script-name)
  (add-to-world-model
    (setf *current-script*
	  ;; Redundant to make new instance since was already done in initialization of script-list.
;;; 	  (f.make-new-instance			; Was f.instantiate-frame
;;; 	    (*frame*
	  script-name
;;; 	      ))
	  ))
;;;   (let (					; Was let*
;	(header-scene (first
;			(f.get *current-script*
;			       'scenes)))
;	(header-scene-i (f.instantiate-frame
;			  (frame-def header-scene)))
;;; 	(scene-list ))
  (do-break unify-instrumental-scene)
  (do ((candidate-list
	 (skip-2-next-match
	   (reverse my-model))
	 (skip-2-next-match
	   (rest candidate-list)))
       (subscene-list
	 (build-subscene-list
	   ;; The first scene is usually the instrumental
	    ;; scene (but not necessarily)
	   (list (first
		   (f.get *current-script*
			  'scenes)))
	   t)
	 (skip-2-next-match
	   (rest subscene-list))))
      ((or (null (print subscene-list))
	   (null (print candidate-list))))
    (unify-scene (first
		   candidate-list)
		 (list (first subscene-list))
		 nil)				; nil -> Strict matching
;      (unify-scene (first
;		     (skip-2-next-story-concept
;		       (get-model *World-model*)))
;		   (list each-scene))
    )
;;;     (f.unify header-scene-i header-scene)
;;;     )
  )



;;;
;;; Function skip-2-next-match recursively traverses a list of frames,
;;; returning the first frame whose match-tag is set, or nil if none are set.
;;; 
(defun skip-2-next-match (frame-list)
  (cond ((null frame-list)
	 nil)
	((get (first frame-list)
	      *match-tag-property*)
	 frame-list)
	(t
	 (skip-2-next-match
	   (rest frame-list))))
  )



;;;
;;; Function remove-match-tags recursively traverses a list of frames, removing
;;; the match-tags from each frame.
;;; 
(defun remove-match-tags (frame-list)
  (when frame-list
    (setf (get (first frame-list)
	       *match-tag-property*)
	  nil)
    (remove-match-tags (rest frame-list)))
  )



;;; 
;;; Original "script" input parameter is a list, not a script. Changed argument
;;; identifier to scene-list. Also there was a potential bug because the
;;; f.unify of the first cond clause was standalone and preceded the removal of
;;; the event from the world model. Therefore event might not be current after
;;; the unify. See comments on f.unify. [cox 26oct94]
;;;
;;; lazy-match? = t -> allow siblings to match in can-unify-p. [cox 23feb95]
;;; 
(defun unify-scene (event scene-list lazy-match? &optional parent)
  (do-break unify-scene)
  (cond ((or					; If match tags are set, then avoid re-matching.
	   (when (and (get (first scene-list)
			   *match-tag-property*)
		      (get event
			   *match-tag-property*))
	     (setf				; remove tags.
	       (get (first scene-list)
		    *match-tag-property*)
	       nil)
	     (setf
	       (get event
		    *match-tag-property*)
	       nil)
	     t)
	   (can-unify-p event
			(first scene-list)
			lazy-match?))
	 ;; Added the following feedback. [cox 27oct94]
	 (format
	   *aqua-window*
	   "~%Unify story concept ~s with scene ~s.~%"
	   event
	   (first scene-list))
	 (f.unify (remove-from-world-model
		    event)
		  (first scene-list)
		  t t lazy-match?)		 ; Note that we suppress f.unify's internal can-unify-p call.
	 (when (not
		 (or (null parent)
		     (member
		       (frame-type parent)
		       *Defined-CDs*)))
	   (format
	     *aqua-window*
	     "~%~%Will try to understand script inference ~s.~%"
	     parent)
	   ;; [cox 16jul95]
	   (set-script-inferences *Current-Result-Record*)
	   ;; [cox 22feb95]
	   (spawn-new-goal
	     *reasoner*
	     (make-goal-state			; Make a goal
	       'understands			; to understand
	       *reasoner*			; by the reasoner
	       (add-story-status
		 parent))
	     'knowledge-acquisition-goal.0	; The goal is to acquire some understanding
	     'five.0)))
	(t (some
	     #'(lambda (each-subscene)
		 (unify-scene
		   event
		   (list (f.unify
			   each-subscene
			   (f.instantiate-frame
			     (frame-def each-subscene))
			   t nil		; These are the defaults so that it can pass lazy-match? arg
			   lazy-match?))
		   lazy-match?
		   (first scene-list)))
	     (f.get (first scene-list) 'scenes))))
  )


;(defun unify-scene (my-model script)
;  (cond ((null script) nil)
;	((can-unify-p  my-model (first script))
;	 (f.unify  my-model (first script))
;	 (remove-from-world-model my-model))
;	(t (unify-scene my-model (append (get-scenes (first script)) (rest script))))))





;;; UTILITY FUNCS


;;; 
;;; Somewhat more simple than the code commented out below. Even more simple is
;;; the fact that this function is no longer necessary. If fact, it was not
;;; doing what was necessary, so it was incorrect. [cox 26oct94]
;;; 
(defun get-scenes (scene)
  "Returns sub-scenes of given scene."
  (or (get-sub-scenes scene)
      (list scene))
  )



;;; returns sub-scenes of given scene
;(defun get-scenes (scene)
;  (let ((sub-scenes
;	  (f.get
;	    (f.instantiate-frame
;	      (frame-def scene))
;	    'scenes)))
;    (if (not (null sub-scenes))
;	sub-scenes
;	(list scene))
;    ))


;;;
;;; The use of this function in a standalone fashion (e.g., in
;;; can-match-current-script) is also flawed. It is replaced by function
;;; expand-subscenes. [cox 27oct94]
;;; 
(defun get-sub-scenes (scene)
  (f.get
    (f.instantiate-frame
      (frame-def scene))
    'scenes))
   

;;; skip non-action scenes in world my-model
;;;
;;; Renamed from get-next-story-concept to the following to make less
;;; misleading.  [cox 26oct94]
;;; 
(defun skip-2-next-story-concept (my-model)
  (cond ((null my-model) nil)
	((or (isa-p 'relation
		    (list (first my-model)))
	     (isa-p 'state
		    (list (first my-model))))
	 (skip-2-next-story-concept
	   (rest my-model)))
	(t
	 my-model)))

;;; Assumes that there are n concepts to skip.
(defun skip-n-story-concepts (n my-model)
  (cond ((= n 0)
	 my-model)
	(t (skip-n-story-concepts
	     (- n 1)
	     (rest
	       (skip-2-next-story-concept
		 my-model)))))
  )


;;; Alter world my-model
;;;
;;; Note that it is significant that function remove-from-world-model return
;;; the story-concept. See how it is used in function unify-scene.[cox 11jan95]
;;; 
(defun remove-from-world-model (story-concept)
  (set-model *World-model*
	     (remove story-concept
		     (get-model
		       *World-model*)))
  story-concept)


(defun add-to-world-model (new-item &aux (unique t))
  "Add new item to world model only if not present in the model."
  (add-to-model
    new-item
    *World-Model*
    unique)
  )


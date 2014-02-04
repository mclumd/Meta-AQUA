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
;;;;			     File: constants.lisp
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
;;     GLOBAL CONSTANTS, VARIABLES, AND PROGRAM PARAMETERS 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; 
;;; ||||| The following really should not be a dot-zero instance?  They are
;;; usually attribute values. But we want them to be constant so that when the
;;; program is run multiple times memories of a reasoner reasoning about a
;;; story refer to the same self.
;;;

(defconstant *reasoner* 'person.0
  "The token for the reasoner in the story understanding mode.")


;;;
;;; The World Model is the current representation for the events in the world
;;; (from the story if in story understanding mode, or from the problem domain
;;; if in problem-solving mode). The variable will actually be implemented as a
;;; frame of type MODEL, Having a frame-list slot. This representation has the
;;; advantage that when items in the world are unified, the World Model will
;;; stay consistent automatically.
;;; 
(defvar *World-Model* nil)


;;; The Reasoning Model is the current representation for the events in the
;;; head of the understander (if in story understanding mode), or of the
;;; current actor (if in problem-solving mode). The variable will actually be
;;; implemented as a frame of type MODEL, having the same advantages as the
;;; World Model described above.
;;; 
(defvar *Reasoning-Model* nil
  "Current model of the reasoning of a character." )



;;;
;;; These variables are models like the *Reasoning-Model*.  See file
;;; goal-q.lisp for the priority-queue data structure and the model data
;;; structure upon which it is built.
;;; 
(defvar *Goal-Queue* nil
  "Contains the current priority queue of goals being processed by the proram.")


;;;
;;; Contains the list of story concepts input to Meta-AQUA under 'read-story
;;; mode. See file story-input.lisp. May be placed on this variable either by
;;; init-story (hand coded examples) or by Tale-Spin (automatically generated
;;; examples). Note, however, that they form of the input differs between the
;;; two. Hand-coded examples are in slot-filler notation with explicit variable
;;; bindings, as is conceptual definitions in the representations files (e.g.,
;;; (action (actor (value (dog))))); whereas Tale-Spin inputs frame variables
;;; (e.g., action.121).
;;; 

(defvar *Story-Concepts* nil
  "Meta-AQUA's input concepts constituting the story to be understood.")



;;;
;;; *Story-Tokens* is used to test how many sentences a given explanation
;;; references in the coherence metric. NOTE that it is implemented as a model.
;;; 
(defvar *Story-Tokens* nil
  "Contains the tokens created that represent the concepts of the story.")



;;;
;;; This is also represented as a model so that the list of indices stays current.
;;; 
(defvar *indices* nil
  "The list of indices created during run-time.")



;;;
;;; This is also represented as a model so that the list of indices stays
;;; current.
;;; 
(defvar *extant-indices* nil
  "The list of indices created during memory creation of init-aqua.")



;;;
;;; If the current mode is different that the previous mode, then function
;;; prep is called again by screen-init in order to update the window labels.
;;; This global is also used by function init-script-applier to determine
;;; whether it is the beginning of a set of Meta-AQUA runs. This condition is
;;; true if *Previous-Mode* is still nil.
;;; 
(defvar *Previous-Mode* nil
  "Specifies the program mode during the last run.")





;;; Need to keep constants which make calls to macros or functions out of
;;; constants.lisp.  |||||| Why is this statement above here. I used to have
;;; the following in rep_smuggle4.lisp, but moved it here recently.


;;; The following are attibute values for status slots:

;;;
;;; The following has been predefined by frame.lisp.
;;; 
;(defparameter *predefined-instance*
;	(define-frame predefined-instance.0
;       	  (isa (value (status-value)))
;	  (status (value predefined-instance.0))))
;(putprop *predefined-instance*  *instance-prop* t)
;(putprop *predefined-instance*  *attribute-value-prop* t)
;
;(defparameter *var-type*
;	(define-attribute-value var.0
;	  (isa (value (status-value)))))


;(defparameter *learned*
;	(define-attribute-value learned.0
;       	  (isa (value (status-value)))))
;
;(putprop *learned*
;	 *instance-prop*
;	 t)
;
;;; Also predefined in frame.lisp.
;(defparameter *story-instance*
;	(define-attribute-value story-instance.0
;	  (isa (value (status-value)))))

;;; Also predefined in frame.lisp.
;;; (eval-when (compile load eval)
;  (defparameter
;    *question*
;    (define-attribute-value question.0
;			    (isa (value (status-value)))))
;;;   )

;(import (list
;	  '*question*
;	  *question*
;;;; 	  *out*
;;;;  	  '*out*
;	  )
;	'TSPIN)


;;; The following are truth values:
;;; All are defined in frame.lisp.

;(defparameter *hypothesized*
;	(define-attribute-value hypothesized.0
;	  (isa (value (truth-value)))))
;(defparameter *hypothesized-out*
;	(define-attribute-value hypothesized-out.0
;	  (isa (value (truth-value)))))
;(defparameter *hypothesized-in*
;	(define-attribute-value hypothesized-in.0
;	  (isa (value (truth-value)))))
;(defparameter *in*
;	(define-attribute-value in.0
;	  (isa (value (truth-value)))))
;
;(defparameter *out*
;	  (define-attribute-value out.0
;				  (isa (value (truth-value)))))




;;;
;;; The following has been predefined by frame.lisp.
;;; 
;(defparameter *nil*
;	(define-attribute-value NIL.0 
;          (isa (value (attribute-value))))
;  )
;
;(defparameter *entity*  (f.define-frame '(entity))
;  )
;
;(define-frame ATTRIBUTE-VALUE
;    (isa            (value (entity)))
;)
;
;(define-frame STATUS-VALUE
;	      (isa (value (attribute-value)))
;)

;(define-frame TRUTH-VALUE
;	      (isa (value (attribute-value)))
;)



;;; |||||| I think that the following two parameters are never used. 11sep93
;;; Finally used 22feb95 !! See script-applier.lisp
(defparameter *Defined-CDs* 
	      '(ptrans atrans mtrans propel attend ingest grasp ungrasp
		possess loc knowledge-state goal-state hunger-state d-health tilt))

(defparameter *Defined-Structs* 
	      '(ptrans atrans mtrans propel attend ingest grasp ungrasp
		possess loc knowledge-state goal-state hunger-state d-health tilt 
		person animate-object dog))


;;;
;;; The value of *aqua-window* alternated between windows 1 through 3. The
;;; variable is also a stream synonym with *frame-stream* as a result of
;;; passing *aqua-window* to f.frame-output-var during init-aqua. The same is
;;; also with *tspin-stream* and tspin-output-var and *utils-stream* and
;;; utils-output-var during init-aqua.  Therefore all frame output goes to the
;;; current program display window of Meta-AQUA transparently.
;;; 
;;; Changed to t [cox 6mar97]
;;;
(defvar *aqua-window* t
  "The current program display window and LISP Listener.")


;;; Changed to t [cox 6mar97]
;;;
(defvar *original-window* t
  "The window from which Meta-AQUA is loaded.")

;;;
;;; The following program parameter is useful on a macivory with a
;;; non-standard monitor (such as the PrecisionColorDisplay/20.
;;; 
(defparameter *force-mouse* nil
  "Can override macivory settings & use mouse.")


;;;
;;; Main window for Meta-AQUA output. If on MacIvory, use predefined coordinates,
;;; else specify with the mouse.
;;; 
;;; Changed to t [cox 6mar97]
;;;
(defvar *window1* 
	t
  "The program display window for the cop (or the reasoner in read-story mode).")


;;;
;;; Used only in the 'act-out-story mode of ripsau (problem-solving mode of
;;; Meta-AQUA).
;;; 
;;; Changed to t [cox 6mar97]
;;;
(defvar *window2*
	t
  "The program display window for the police.")


;;;
;;; The smaller window that displays internal structures at run-time.
;;; 
;;; Changed to t [cox 6mar97]
;;;
(defvar *window3*
	t
  "The program display window for the goal queue and internal concepts.")


;;;
;;; Constants that seem redundant, but if ever changed, need be changed in only
;;; one place.  Less chance for a typo when using them, than when using quoted
;;; constants in the code.  Both the compiler and the interpreter will catch
;;; typos now.
;;;

(defconstant *World*              'world)
(defconstant *Reasoning*          'reasoning)

;;;
;;; Slot names.
;;;
(defconstant *explanations-slot*  'explanations)
(defconstant *asserted-node-slot* 'xp-asserted-nodes)
(defconstant *hvq-back-slot*      'hvq-)
(defconstant *hvqs-slot*          'hvqs)
(defconstant *explains-node*      'explains)
(defconstant *actor-slot*         'actor)

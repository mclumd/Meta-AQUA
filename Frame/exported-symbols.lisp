;;;-*- Mode: LISP; Syntax: Common-lisp; Package: Framesystem; Base: 10 -*-

(in-package :frames)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	 A Frame System for conceptual construction
;;;;
;;;;	   Copyright (C) 1994   Michael T. Cox   (cox@cc.gatech.edu)
;;;;
;;;;				   29 Oct 1994
;;;;
;;;;			  File: exported-symbols.lisp
;;;;
;;;;
;;;;   This file contains code that creates and initializes identifiers
;;;;   exported from the FrameSystem. All functions that are in the external
;;;;   interface are contained in the files frame.lisp and isa.lisp. Local
;;;;   variables and parameters are contained in file local-constants.lisp.
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
;;                 GLOBAL EXPORTED IDENTIFIERS 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; This definition MUST come before the others since it is used in the
;;; literal-p predication used by the define-frame macro.
;;;
;;; |||||| Wanted to have '(literal (isa (value (entity))) but would not
;;; compile and load right. See f.define-frame. [29oct94]
;;; 
(defparameter *literal*  (f.define-frame '(literal))
  )


;;;
;;; Note that this cannot be called with define-attribute-value
;;; since that function uses this constant. The following two 
;;; calls of putprop simulates the use of define-attribute-value.
;;; 
(defparameter *predefined-instance*
	(define-frame predefined-instance.0
       	  (isa (value (status-value)))
	  (status (value attribute-value.0))))

(putprop *predefined-instance*
	 *instance-prop*
	 t)
(putprop *predefined-instance*
	 *attribute-value-prop*
	 t)

(defparameter *attribute-value*
	(define-frame attribute-value.0
       	  (isa (value (status-value)))
	  (status (value attribute-value.0))))

(putprop *attribute-value*
	 *instance-prop*
	 t)
(putprop *attribute-value*
	 *attribute-value-prop*
	 t)


(defparameter *story-instance*
	(define-attribute-value story-instance.0
	  (isa (value (status-value)))))




(defparameter
  *question*
  (define-attribute-value question.0
			  (isa (value (status-value)))))

(defparameter *learned*
	(define-attribute-value learned.0
       	  (isa (value (status-value)))))

(putprop *learned*
	 *instance-prop*
	 t)



;;;
;;; the frame representation for no filler.
;;; The f.unify function knows about this.
;;; Thus to remove a filler from a facet,
;;; just call f.remove-filler, which performs
;;; a f.put! of *nil*.
;;; 
(defparameter *nil*
	(define-attribute-value NIL.0 
          (isa (value (attribute-value))))
  )


;;; 
;;; The type entity is at the root of the isa hierarchy. All tokens and types
;;; are eventually isa entity, either directly or through transitivity.
;;;
;;; Note that there is a commented out definition of entity in meta-xp.lisp in
;;; representations directory that was taken from Aswin's AQUA.
;;;
(defparameter *entity*
	(f.define-frame
	  `(entity
	     (,*status-slot* (value (status-value)))
	     (,*truth-slot* (value (truth-value)))
	     (,*derivation-slot* (value ,*nil*))))
  )


;;; Should this not have a domain and co-domain slot? 20nov93
(define-frame ATTRIBUTE-VALUE
    (isa            (value (entity)))
)


(define-frame STATUS-VALUE
	      (isa (value (attribute-value)))
)

;;; |||||| Needs to be isa relational-attribute-value or something.
(define-frame TRUTH-VALUE
	      (isa (value (attribute-value)))
)




(defparameter *var-type*
	(define-attribute-value var.0
	  (isa (value (status-value)))))

(defparameter *in*
	(define-attribute-value in.0
	  (isa (value (truth-value)))))

(defparameter *out*
	(define-attribute-value out.0
	  (isa (value (truth-value)))))

(defparameter *hypothesized-in*
	(define-attribute-value hypothesized-in.0
	  (isa (value (truth-value)))))

(defparameter *hypothesized*
	(define-attribute-value hypothesized.0
	  (isa (value (truth-value)))))

(defparameter *hypothesized-out*
	(define-attribute-value hypothesized-out.0
	  (isa (value (truth-value)))))



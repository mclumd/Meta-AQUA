;;; -*- Mode: LISP; Syntax: Common-lisp; Package: Framesystem; Base: 10 -*-

(in-package :frames)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	 A Frame System for conceptual construction
;;;;
;;;;	   Copyright (C) 1994   Michael T. Cox   (cox@cc.gatech.edu)
;;;;
;;;;				   20 May 1994
;;;;
;;;;				   File: isa.lisp
;;;;
;;;;
;;;;   This file contains code that implements the isa hierarchy in the
;;;;   FrameSystem.
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
;;           ISA HIERARCHY
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(export '(*isa-property* get-abstraction put-isas putprop
	  addprop isa-p init-isas init-isa-hierarchy ))


;;; When frames are defined they have a isa slot.  This is removed from the
;;; form which gets bound to the frame symbol (eg., ptrans) and placed on the
;;; symbol's isa property. The isa relation is really a "system slot". NOTE
;;; that this parameter is really found in the file isa.lisp
;;; 
(defparameter *isa-property*       'isa)



; Assumes that frame is a frame-variable.
; Will return a list of abstractions, even if there
; is only one, because of the way put-isas functions.
;
(defun get-abstraction (frame)
  (get frame *isa-property*))
  

; Function put-isas has the option of putting the given value on the
; given property of a list of symbols. They are all direct descendents
; of class. NOTE:The parent class is enforced to be a list. If it is not
; then the routine makes it so without warning.
; ||||| This should make sure that no duplicates are put.
;
(defun put-isas (children parent)
  (if (and (not (null parent))(atom parent))
      (setf parent (list parent)))
  (cond ((null children)
	 nil)
	((atom children)
	 (putprop children 
		  *isa-property* 
		  (union parent (get children *isa-property*))))
	((listp children)
	 (putprop (car children) 
		  *isa-property* 
		  (union parent 
			 (get (car children) *isa-property*)))
	 (put-isas (cdr children) parent))))


;;;
;;; Proclaim these two primitive functions to be inline. [cox 16feb95]
;;; 
(proclaim '(inline putprop
		   addprop
		   )
	  )

;;; Function putprop is destructive. Whatever was the previous value on the
;;; property is overwritten.
;;; 
(defun putprop (symbol property avalue)
  (setf (get symbol property) avalue)
  )


;;; Function addprop is non-destructive.
;;;
;;; If the optional argument avoid-duplicates is non-nil, the value(s) will not
;;; be added if already extant.  [29oct94] 
;;; 
(defun addprop (symbol property avalue &optional avoid-duplicates)
  (if (and (not (null avalue))			; Skip if no value to place
	   (or (not avoid-duplicates)		; and if avoiding duplicates
	       (not (member			; do not place if already
		      avalue			; a value 
		      (get symbol property)))))	; is there.
      (setf (get symbol property)
	    (cons avalue (get symbol property)))))


;isa-p guarantees that An-Item will be a list, not an atom.
;
(defun isa1-p (ParticularClass An-Item)
  (if (null An-Item)
      nil
      (let ((SuperClass (get (car An-Item) *isa-property*)))
	(cond ((null SuperClass)
	       nil)
	      ((or (member ParticularClass SuperClass)
		   (isa1-p ParticularClass (cdr An-Item)))
	       t)
	      (t
	       (isa1-p ParticularClass SuperClass))))))


; Returns true if frame has abstraction, immediate  or
; indirect. |||| This may be backwards.
;;; Parameter frame should be a list I believe.
;
(defun isa-p (abstraction frame)
  (cond ((or (null abstraction)
	     (null frame))
	 nil)
	((and (listp frame)
	      (equal abstraction (car frame))))
	((equal abstraction frame))
	(t
	 (isa1-p abstraction (list (frame-type frame))))))
	 


(defun init-isas (entity-list)
  (cond ((null entity-list)
	 nil)
	((atom entity-list)
	 (putprop entity-list *isa-property* nil))
	(t
	 (putprop (car entity-list) *isa-property* nil)
	 (init-isas (cdr entity-list)))))

(defun init-isa-hierarchy ()
;;;   (init-isas 'entity)
  (init-isas 'action)
  (init-isas '(cd))
  (init-isas *Defined-CDs*)

;;;   (init-isas 'physical-object)
;;;   (init-isas 'animate-object)

  ; Just in case something ever sets this otherwise.
;;; Always was redundant  (put-isas 'entity nil)

;;; ||||| What was this line doing?  (put-isas 'action)
  (put-isas '(cd) 'action)
  (put-isas *Defined-CDs* 'cd)

;;;   (put-isas 'physical-object 'entity)
;;;   (put-isas '(animate-object inanimate-object) 'physical-object)
;;; Finally defined a volitional-agent,a person and phycical object
;;; along with animate and inanimate
						; objects,..
;  (put-isas '(volitional-agent) 'animate-object)
;  (put-isas '(person dog) 'volitional-agent)
;;;   (put-isas '(dog) 'volitional-agent)
;;;   (put-isas '(authority) 'person)
;;;   (put-isas '(book container) 'inanimate-object)
  )

     

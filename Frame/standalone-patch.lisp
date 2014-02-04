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
;;;;				File: internals.lisp
;;;;
;;;;
;;;; To use the Frame System in standalone mode with the file
;;;; application-frame-demo.lisp, this patch must be loaded. The use of the reps
;;;; package is commented out in the call to create-self-ref-var in function
;;;; instantiate-frame below. [mcox 27dec05]
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
;;; Function instantiate-frame is the actual work horse of function
;;; f.instantiate-frame. It is also called by functions f.instantiate-literal,
;;; create-dummy-frame and instantiate-filler. Note that the pattern property
;;; is actually used by function f.put-all! to determine the scope of the
;;; changes made. [cox 27jan95]
;;; 
;;; note: an application should never call instantiate-frame.  use
;;; f.instantiate-frame or define-frame instead.
;;;
;;; status-filler is the designated status type for the status slot.
;;; 
; is a difference between the pattern and the structure being built.
; put fillers of all slots on the binding-list?
;;; 
(defun instantiate-frame (pattern &optional status-filler subframe-p)
;;;   (declare (special *existing-frames* ))	; Removed for efficiency [cox 25feb95]
  (cond ((frame-var-p pattern)
	 pattern)
	((and (listp pattern)
	      (not (eq
		     (length pattern)
		     1))
	      (equal (first pattern)
		     *literal*))
	 (when (not (eq
		      (length pattern)
		      2))
	   (format *frame-stream*
		   "error in instantiate-frame: bad literal.")
;;; 	   (break)				; Removed for efficiency [cox 25feb95]
	   )
	 (f.instantiate-literal (first (rest pattern))))
	(t
	 (let ((frame-name (gen (car pattern))))
	   (put-isas frame-name (car pattern))
	   (if (not subframe-p)
	       (setf  *global-bindings*
		      (acons *self* frame-name *global-bindings*))
	       (cond ((var-binding-p (second pattern))
		      (setf  *global-bindings* 
			     (acons (second pattern) frame-name *global-bindings*))
		      (setf pattern
			    (cons (car pattern) (cddr pattern))))))	; remove variable marker.
	   (putprop frame-name 'pattern pattern)
	   (mapcar
	     #'(lambda (each-slot)
		 (let ((current-role (slot->role each-slot)))
		   (mapcar
		     #'(lambda (each-facet)
			 (let ((facet-name (facet->facet-name each-facet)))
			   (format-if 
			     *debug-on*
			     t 
			     "~%facet = ~s;slot = ~s" 
			     each-facet each-slot)
			   ;; test to make sure that the facet has not already been
			   ;; established as when variables pointing to it instantiate the slot.
			   (cond ((not (f.get frame-name current-role facet-name))  
				  ;; |||||| should (*frame* frame-name) be used below?
				  ;; note that the facet is filled at the same time
				  ;; that current-filler assignment happens below.
				  (let ((dummy-var (cdr (assoc 
							  (create-self-ref-var 
							   current-role 
;;;							   'reps
							   )
							  *global-bindings*)))
					(current-filler (f.put (instantiate-filler 
								 (facet->filler
								   each-facet) 
								 current-role
								 facet-name
								 status-filler)
							       frame-name 
							       current-role
							       facet-name)))
				    (if (and dummy-var 
					     (not (equal dummy-var current-filler)))
					(switch-back-ptrs-of 
					  dummy-var current-filler)))))))
		     (slot->facets each-slot))))
	     (frame->slots pattern))
	   (if status-filler
	       (f.put status-filler frame-name *status-slot*))
	   frame-name))))


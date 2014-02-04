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
;;;;			    File: local-constants.lisp
;;;;
;;;;
;;;;   This file contains code that defines local constants and variables in the
;;;;   FrameSystem.  All functions that are in the external interface are
;;;;   contained in the files frame.lisp and isa.lisp. Externally exported
;;;;   identifiers are in exported-symbols.lisp. Local-constants.lisp must be
;;;;   loaded before the frame.lisp file because *visited-prop* is used there
;;;;   in the definition of another program variable (*traverse-marker*).
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
;; FRAME SYSTEM LOCAL CONSTANTS, VARIABLES, PARAMETERS, AND FUNCTIONS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 
;;; History 
;;; 
;;; Changed the value of *visited-prop* from a symbol to a string, because it
;;; is passed as an argument to gentemp. In Allegro 8.0 the function no longer
;;; takes symbol parameters. [mcox 3aug06]
;;; 

;;;
;;; Global variable (but local to frames package) used by the frame instantiation
;;; functions f.instantiate-frame, instantiate-filler, and instantiate-frame. It
;;; is used to manage the variable bindings, even if there are circular references.
;;; 
(defvar *GLOBAL-Bindings* nil)


;;; Needs to be evaluated especially at load time so the constant *traverse-marker*
;;; will be created properly.
;;; 
;;; (eval-when (compile load eval) ;Instead I am placing it in an earlier module
;;;                                ;within the frame system definition.
;;;
;;; Changed from symbol to string. [mcox 3aug06]
;;; Changed back and change gentemp calls instead. [mcox 23oct06]
;;;
(defparameter *visited-prop*  'visited)
;;; )


(defparameter *Back-Ptrs*     'bkptrs)


;;; 
;;; A list of those frames created during execution.
;;; If two existing frames are merged, then the one not
;;; returned is removed from this list. The list is reset
;;; at the beginning of each program run.
;;;
;;; Removed all references and uses of this parameter [sic] for reasons of
;;; efficiency. I never did debug it use, and so never really used it. It was
;;; used in the functions merge-nodes and gen and was also declared special
;;; within function instantiate-frame. [cox 25feb95]
;;; 
;;; (defparameter *existing-frames* nil)


;;; 
;;; The list of pre-defined frames declared with f.instantiate-instance
;;; or define-attribute-value.
;;;
(defvar *predefined-frames* nil)


;;; The following two global variables
;;; are used strictly by functions pertaining
;;; to f.copy-instantiated-frame. They are used
;;; to avoid cycles and handle variable bindings.
;;; 
(defvar new-list nil)
(defvar old-list nil)


(defvar *PATH-NODES* nil "Used in f.find-filler and f.find-paths only")
(defvar *FINISHED-NODES* nil "Used in f.find-paths only")



;;;
;;; The following variables are used exclusively for the 
;;; bookkeeping functions. See section on error checking and bookkeeping.
;;;

(defvar *undefined-frames* nil)
(defvar *num-nulls* 0)
(defvar *num-non-lists* 0)








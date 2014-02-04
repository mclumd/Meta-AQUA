;;; -*- Mode: LISP; Syntax: Common-lisp; Package: Framesystem; Base: 10 -*-

(in-package :frames)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	           A Frame System for conceptual construction
;;;;
;;;;	   Copyright (C) 1994   Michael T. Cox   (cox@cc.gatech.edu)
;;;;
;;;;				   20 May 1994
;;;;
;;;;			    File: frame-interface.lisp
;;;;
;;;;
;;;;   This file contains the external interface to the FrameSystem. All
;;;;   functions, variables, and constants that are publically available
;;;;   outside the frame package are exported by this file.
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
;;           FRAME SYSTEM PUBLIC INTERFACE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 
;;; Export the functions that are the interface to the
;;; other packages that will use the frame system.
;;;
(export '(FRAME->FACET-NAMES
	  SLOT->ROLE              DEFINE-FRAME
	  SLOT->FACETS            DEFINE-RELATION
	  FRAME->SLOTS            DEFINE-ATTRIBUTE-VALUE
	  FACET->FACET-NAME       DEFINE-INSTANCE
	  FRAME->ROLES            F.INSTANTIATE-FRAME
	  FRAME->FRAME-NAME       F.INSTANTIATE-LITERAL 
	  FRAME->SLOT             F.MAKE-NEW-INSTANCE 
	  FRAME->FILLER           F.INSTANTIATE-INSTANCE
	  SLOT->FILLER            
	  FACET->FILLER           F.GET
	  VAR->ROLE               F.CHASE-PATH
                                  F.INHERIT
	  INIT-FRAME-BOOKKEEPING  F.PUT
	  PRINT-BAD-FRAMES        F.PUT!
	  PRINT-ONLY-DEFINERS     F.PUT-ALL!
	  PRINT-ONLY-UNDEFS       F.SET-LITERAL
	                          F.MODIFY
	                          F.REMOVE-FILLER!
	  *FRAME*   		  F.MAKE-RELATION
	  FRAME-DEF               F.GET-RELATION
	  FRAME-TYPE              F.ADD-RELATIONS     
	  FRAME-BODY              F.TRAVERSE-FRAME
	  F.RELATION-SLOT-NAME    F.FIND-PATHS
	  F.ROLE-LIST             F.FIND-FILLER
	  F.SLOT-LIST             F.UNIFY
	  F.FACET-LIST            F.LOWEST-COMMON-ANCESTOR
	  F.WHERE-BOUND           F.PUT-BACK-PTRS-FOR-INDEX
    	                          F.DISPLAY
	  VISITED-P               F.LET
	  F.COMMON-ANCESTOR-P     F.PPRINT
	  F.DEFINED-ROLE-P        F.FRAME-OUTPUT-VAR
	  F.TYPE-P                F.SPECIALIZATIONS-OF 
	  FRAME-VAR-P             F.RETURN-PATH-TO
	  CAN-UNIFY-P		  F.COPY-INSTANTIATED-FRAME
	  VAR-BINDING-P           F.COUNT-SUBNODES
	  INSTANCE-P              
	  LITERAL-P               F.DEBUGOFF
	  RELATION-P              F.DEBUGON
	  IN-SET-OF-BELIEFS-P
	  ATTRIBUTE-VALUE-P
	  FRAME-LIST-P                         ) )


;;; 
;;; Export the public constants.
;;; NOTE that these must be explicitly imported by the
;;; calling package for the Frame System to work properly.
;;; Also note that exported "constants" must be defined using
;;; defvar instead of defconstant because the compiler makes
;;; assumptions about the constants and their values which interfere
;;; with the importing and exporting of them.
;;;
;;; |||||| It may not be necessary to export these explicitly if
;;; the file import-mcaqua.lisp (or some such file) explicitly
;;; imports them. I had left off truth-value in the following list,
;;; but had no problem. What is the best way to provide the interface?
;;; 
(export '(
	  *status-slot*
	  *derivation-slot*
	  *truth-slot*     
	  *domain-slot*    
	  *co-domain-slot* 
	  *value-facet*
	  *relation-facet*
	  *dummy*
	  *self*
	  *instance-prop*
	  *attribute-value-prop*
	  *slot-prop*
	  *entity*
	  *literal*
	  attribute-value
	  status-value
	  truth-value
	  *predefined-instance*
	  *story-instance*
	  *learned*
	  *question*
	  *attribute-value*
	  *var-type*
	  *nil*
	  *in*
	  *out*
	  *hypothesized-in*
	  *hypothesized-out*
	  *hypothesized*
	  *extant-frames*
	  *traverse-marker*
	  ))


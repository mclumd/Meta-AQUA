;;; -*- Mode: LISP; Syntax: Common-lisp; Package: Meta-aqua; Base: 10 -*-
(defpackage Meta-AQUA
  (:nicknames MetaAQUA)
  (:use cl
   Misc Frames Representations 
;; Nonlin
   )
  )


(in-package :metaaqua)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	    The Meta-AQUA Introspective Multistrategy Learning System
;;;;				   Version 6
;;;;
;;;;	     Copyright (C) 1996   Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;			File: meta-aqua-interface.lisp
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



;;; Explicitly import constants from package Framesystem
;;; in order that they be internal symbols of this package,
;;; not just accessible.
;;; 
(import (list
	  frames::*entity*
	  frames::*literal*
	  ;; |||||| Are the following three necessary?
;;; 	  'frames::attribute-value
;;; 	  'frames::status-value
;;; 	  'frames::truth-value
	  frames::*predefined-instance*
	  frames::*status-slot*
	  frames::*truth-slot*     
	  frames::*co-domain-slot* 
	  frames::*domain-slot*    
	  frames::*relation-facet*
	  frames::*value-facet*
	  frames::*var-type* 
	  frames::*Back-Ptrs*
	  frames::*instance-prop*
	  frames::*attribute-value-prop*
	  frames::*slot-prop*
	  frames::*nil*
	  frames::*in*
	  frames::*out*	  
	  frames::*isa-property*
	  frames::*dummy*
	  frames::*self*
	  ))

;;;
;;; Export *question* so Tale-Spin will see it.
;;; 
(export '(*question*
          *story-instance*)
	)


;;;
;;; Function import-slot-names is used to make the symbols standing for
;;; the slot names (roles) defined in another package also accessable
;;; in the current package. The parameter f-list is the list of frames
;;; to be imported into the current package.
;;; 
(defun import-slot-names (f-list)
  (let ((slot-list nil))
    (dolist (each-frame f-list)
      (dolist (each-slot
		(frame->slots
		  (symbol-value
		    each-frame)))
	(pushnew (slot->role each-slot)
		 slot-list)))
    (import slot-list)))

;(trace import-slot-names)
;;;
;;; The following makes internalizes the definitions
;;; of Representations module. If one does not
;;; want the knowledge base included in the
;;; application, then comment out this line and the next
;;; non-comment line.
;;; 
(import REPS::*extant-frames*)

;;;
;;; Use the following if one wishes to build their conceptual memory on top
;;; of the knowledge in Representations Package.
;;; 
(import-slot-names REPS::*extant-frames*)



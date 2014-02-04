;;;-*- Mode: LISP; Syntax: Common-lisp; Package: Tale-spin; Base: 10 -*-
(defpackage Tale-Spin
  (:nicknames TSpin)
  (:use cl 
   Misc Frames Representations Meta-AQUA)
  )

(in-package :tspin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	    Elvis World and the Tale-Spin Story Generation Subsystem
;;;;				 for Meta-AQUA
;;;;
;;;;	     Copyright (C) 1996   Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;			   File: spin-interface.lisp
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
;;           TALE-SPIN PUBLIC INTERFACE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 
;;; Export the functions that are the interface to the other packages
;;; that will use Tale-Spin. These functions include story generation
;;; and conversion functions, as well as trace and debug routines.
;;;

(export '(tspin-output-var mspin aspin yspin spin convert-story 
	  recover-from-disk re-run-story tdebug say-all
	  print-params reset-params init-file
	  make-boring-story make-wild-story *action-trace*
	  *stack-trace* *plan-trace* *conseq-trace*
	  )
	)

(import '(tspin-output-var mspin aspin spin convert-story 
	  recover-from-disk re-run-story tdebug say-all
	  print-params reset-params init-file
	  make-boring-story make-wild-story *action-trace*
	  *stack-trace* *plan-trace* *conseq-trace*
	  )
	'META-AQUA)



;;; Explicitly import constants from package Framesystem
;;; in order that they be internal symbols of this package,
;;; not just accessible.
;;; 
(import (list
	  frames::*entity*
	  frames::*literal*
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
	  'meta-aqua::*aqua-window*
	  ))


;;; 
;;; In order to be able to covert query cds in tspin-2-meta-aqua TSpin must
;;; possess question related symbols. The second import is in file
;;; constants.lisp since at load time the variable is not yet bound.
;;; 
;;; ||||||But this interface file is loaded before Meta-AQUA package, so
;;; let meta-aqua call the import. See file constants.lisp.
;;; (import 'Meta-Aqua::*question*)




;;;
;;; Function import-slot-names is used to make the symbols standing for
;;; the slot names (roles) defined in another package also accessable
;;; in the current package. The parameter f-list is the list of frames
;;; to be imported into the current package.
;;; 
(defun import-slot-names (f-list)
  (let ((slot-list nil))
    (dolist (each-frame f-list)
      (dolist (each-slot (frame->slots (symbol-value each-frame)))
	(pushnew (slot->role each-slot) slot-list)))
    (import slot-list)))


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


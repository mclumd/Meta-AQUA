;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
(in-package :user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	 A Frame System for conceptual construction
;;;;
;;;;	   Copyright (C) 1994   Michael T. Cox   (cox@cc.gatech.edu)
;;;;
;;;;				   20 May 1994
;;;;
;;;;			      File: frame-sys-def.lisp
;;;;
;;;; 
;;;; IMPORTANT, YOU MUST CHANGE THE :DEFAULT-PATHNAME BELOW.
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
;;; Miscellaneous general support functions.
;;; 
(defpackage Miscellaneous
  (:nicknames Misc)
  (:use cl )
  )


;;;
;;; M. Cox's stand-alone frame knowledge-representation system.
;;; 
(defpackage FrameSystem
  (:nicknames Frames)
  (:use cl Misc)
  )



(defsystem FRAME-SYSTEM
    
    (:pretty-name "Frame Knowledge Representation System"
     :default-pathname "/fs/metacog/group/systems/Meta-AQUA/Frame/"
     )
  
  ;; The Miscellaneous package.
  (:module assorted ("utils"
		     "break-facility")
	   )
  
  ;; Contains constants that must be defined before main code is loaded.
  (:module constants ("local-constants")
	   (:in-order-to (:compile :load) (:load assorted))
	   )

  ;; The Main module.
  (:module main ("isa"
		 "frame"
		 )
	   (:in-order-to (:compile :load) (:load constants))
	   )
  
  ;; Contains local (non-exported) code that some of which depends on code in frame.lisp.
  (:module internals ("internals")
	   (:in-order-to (:compile :load) (:load main))
	   )

  ;; Contains exported symbols and identifiers.
  (:module interface ("exported-symbols"
		      "frame-interface")
	   (:in-order-to (:compile :load) (:load internals))
	   )
  )


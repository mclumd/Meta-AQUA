;;;-*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
(in-package :user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;		    Nonlin Hierarchical Planning Subsystem
;;;;				 for Meta-AQUA
;;;;
;;;;	     Copyright (C) 1994   Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;			   File: nonlin-sys-def.lisp
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



;;;; 
;;;; IMPORTANT, YOU MUST CHANGE THE :DEFAULT-PATHNAME BELOW.
;;;; 

;;;
;;; UM Nonlin. A Common LISP implementation of Austin Tate's Nonlin planner
;;; available on-line from the University of Maryland at cs.umd.edu in
;;; directory /pub/nonlin.
;;; 
(defpackage NonLinear
  (:nicknames NONLIN)
  (:use cl USER
;mcox   Meta-AQUA
   )
  )




(defsystem NONLIN-SYSTEM
    
    (:pretty-name "Nonlin Planning System"
;mcox     :short-name "NonlinSystem"
     :default-pathname "/fs/metacog/group/systems/Meta-AQUA/Nonlin/"
;mcox     :patchable T
     )
    
  ;; The next seven modules constitute the NONLIN package.

  ;; The interface (really exports) module.
  (:module n-imports "nonlin-interface"
	   )

  ;; The utilities module.
  (:module utilities ("util")
 	   (:in-order-to (:compile :load) (:load n-imports))
; 	   (:in-order-to :compile (:load n-imports))
	   )

  ;; The definition module.
  (:module definitions ("def")
 	   (:in-order-to (:compile :load) (:load utilities))
; 	   (:in-order-to :compile (:load utilities))
	   )

  ;; The module for unification routines.
  (:module unification ("unify")
 	   (:in-order-to (:compile :load) (:load definitions))
; 	   (:in-order-to :compile (:load definitions))
	   )

  ;; The module with macro definitions.
  (:module macro-defs ("macros")
 	   (:in-order-to (:compile :load) (:load unification))
; 	   (:in-order-to :compile (:load unification))
	   )


  ;; Nonlin's main code module.
  (:module main ("backtrack"
		 "dev"
		 "establish"
		 "expand"
		 "gost"
		 "init-planner"
		 "link"
		 "mark"
		 "plan"
		 "printplan"
		 "readschema"
		 "schema"
		 "tome"
		 )
	   (:in-order-to (:compile :load) (:load macro-defs))
;	   (:in-order-to :compile (:load  macro-defs))
	   )


  ;; The operator module with action schemas, operator schemas, and short-cuts for plan-for calls.
;  (:module operators (
;		      "learn-operators5.lisp"
;		      )
;	   (:in-order-to (:compile :load) (:load main))
;	   (:in-order-to :compile (:load main))
;	   )

  )


;;;-*- Mode: LISP; Syntax: Common-lisp; Package: NONLINEAR; Base: 10 -*-

(in-package :nonlin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;		    Nonlin Hierarchical Planning Subsystem
;;;;				 for Meta-AQUA
;;;;
;;;;	     Copyright (C) 1994   Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;			  File: nonlin-interface.lisp
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
;;           NONLIN PUBLIC INTERFACE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 
;;; Export the functions that are the interface to the other packages that will
;;; use NONLIN. |||||| UPDATE the FOLLOWING: These functions include the main
;;; functions plan-for, store-always-ctxt, store-init-ctxt, and readschema.
;;; Global data structures are also exported so Metaq-AQUA can access them.
;;;

(export '(
;;; 	  plan-for store-always-ctxt store-init-ctxt readschema debug
;;; 	  *autocond* *planschema* *allnodes*

     ;; GLOBAL VARS:
     *CURRENT-NONLIN-VERSION*
     *CURRENT-NONLIN-VERSION-DATE*
     *NONLIN-LOADED-P*
     *NONLIN-OPS-LOADED*
     *NONLIN-USE-MODE*
     *CYCLE-COUNT*
     *CYCLE-LIMIT*
     *PLANNER-IN*
     *PLANNER-OUT*
     *PLANNER-EXPANSIONS*
     *GOALS*
     *PLANSCHEMA*
     *NONLIN-TERMIN-STATUS*
     *INIT-CTXT*
     *ALWAYS-CTXT*
     *AUTOCOND*
     *DEVISOR-MODS*
     *TIME0*
     *INFINITY*

     ;; FUNCTION NAMES:
     PLAN-FOR
     DEBUG
     STORE-ALWAYS-CTXT
     STORE-INIT-CTXT
     GET-PROBLEM
     PRINT-ALLNODES
     PRINT-TOME
     PRINT-GOST
     PRINT-TASKQUEUE
     RESET-SCHEMATABLE
     DUMP-SCHEMATABLE
     
     ;; MACRO NAMES:
     ACTSCHEMA
     OPSCHEMA
     PLAN-SCHEMA

     ->
     <-
     )
	)

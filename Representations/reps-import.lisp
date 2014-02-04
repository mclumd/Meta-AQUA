;;; -*- Mode: LISP; Syntax: Common-lisp; Package: Representations; Base: 10 -*-

(defpackage Representations
  (:nicknames Reps)
  (:use cl Misc Frames)
  )

(in-package :reps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	      Meta-AQUA Background Knowledge Represented as Frames
;;;;
;;;;	    Copyright (C) 1996   Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;                     File: reps-import.lisp
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
;;           REPRESENTATIONS PUBLIC INTERFACE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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
	  frames::*question*
	  ))

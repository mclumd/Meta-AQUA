;;; -*- Mode: LISP; Syntax: Common-lisp; Package: Meta-aqua; Base: 10 -*-

(in-package :metaaqua)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	    The Meta-AQUA Introspective Multistrategy Learning System
;;;;				   Version 6
;;;;
;;;;	     Copyright (C) 2013   Michael T. Cox   (mcox@cs.umd.edu)
;;;;
;;;;
;;;;			    File: fire-story.lisp
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


;;; Blocksworld with fire test
;;;
(defun set-story-12 ()
  (setf  *Story-Concepts*
    '((
       (pick-up
	(actor (value person.0))
	(object (value (triangle)))
	(success (value true.0)))
       "The person picks up the triangle block.")
      (
       (burns
	(actor (value (nature)
		      (relation (actor
				(domain (value =self))
				(co-domain (value =actor))))))
	(object (value (square)))
	(main-result (value (burning
			     (domain (value =object))
			     (co-domain (value true.0))))))
       "A square block is burning.")
      (
       (stack
	(actor (value person.0))
	(object (value (triangle)))
	(recipient (value (square))))
       "He stacks the triangle on a square block.")
      ((forced-by-states
	(actor (value (nature)))
	(object (value (square)))
	(conseq (value (burns
			(actor (value =actor))
			(object (value =object))
			
			(main-result (value =result)))))
	(fuel (value (at-location (domain (value =object))
				  (co-domain (value (physical-location =l))))))
	(element (value (at-location (domain (value (oxygen)))
				     (co-domain (value =l)))))
	(heat (value (temperature (domain (value =object))
				  (co-domain (value very-hot.0)))))
	(result (value (burning (domain (value =object))
			  (co-domain (value true.0)))))
	(ante (value (model
		      (frame-list (value (=heat =element =fuel))))))
	
	)
       "The square burns because of heat, fuel and oxygen.")
      )))


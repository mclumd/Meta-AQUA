;;; -*- Mode: LISP; Syntax: Common-lisp; Package: Meta-aqua; Base: 10 -*-

(in-package :metaaqua)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	    The Meta-AQUA Introspective Multistrategy Learning System
;;;;				   Version 6
;;;;
;;;;	     Copyright (C) 1996   Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;			    File: burglar-story.lisp
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


;;; GOBAT/D
;;;
;;; Robbery crime story.
;;;
(defun set-story-11 ()
  (init-story
    '(
      ((ptrans
	     (actor (value
		      (criminal-volitional-agent)
			   ))
	     (object (value =actor))
	     (to  (value (at-location
			(domain (value =actor))
			(co-domain
			  (value (near
				  (domain (value =actor))
				  (co-domain (value (house =h))))))))))
       "The man with the gun approached a house.")

      ((ptrans 
	(actor (value (person)))
	(object (value =actor))
	(from (value (at-location
		      (domain (value =object))
		      (co-domain (value
				  (outside
				   (domain (value =object))
				   (co-domain (value (house =h)))))))))
	(to (value (at-location
		    (domain (value =object))
		    (co-domain (value
				(inside
				 (domain (value =object))
				 (co-domain (value =h))))))))
	)
       "The person entered the house.")
 
      ((rob
	(actor (value (person)))
	(object (value (valuables)))
	(opponent (value (person)))
;	(instrumental-object (value (weapon)))
	(main-result (value (controls (domain (value =actor))
				      (co-domain (value =object)))))
	(goal-scene
	 (value (atrans
		 (actor (value =actor))
		 (object (value =object))
		 (from  (value (controls
				(domain (value =opponent))
				(co-domain (value =object)))))
		 (to (value =main-result)))))
	)
       "The person robs the owner")

      ((ptrans
	    (actor (value (person)))
	    (from  (value (at-location
			(domain (value =actor))
			(co-domain
			  (value (inside
				  (domain (value =actor))
				  (co-domain (value (house =h)))))))))
	    (to  (value (at-location
			(domain (value =actor))
			(co-domain
			  (value (outside
				  (domain (value =actor))
				  (co-domain (value =h))))))))
	)
       "The person left the scene")
      )
    ))


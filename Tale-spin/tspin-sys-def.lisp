;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-

(in-package :user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;
;;;;	    Elvis World and the Tale-Spin Story Generation Subsystem
;;;;				 for Meta-AQUA
;;;;
;;;; 	      Copyright (C) 1996   Michael T. Cox   (mcox25@covad.net)
;;;; 
;;;; 
;;;;			    File: tspin-sys-def.lisp
;;;;
;;;;                  Loader file and system definition.
;;;
;;;; IMPORTANT, YOU MUST CHANGE THE THE :default-pathname VALUE IN THE
;;;; DEFSYSTEM BELOW TO THE ROOT DIRECTORY OF TALE-SPIN.
;;; 
;;;           *******************************************************
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
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Tale-Spin is a modification of M. Pazzani's version of J. Meehan's 
;;; Tale-Spin story generator.
;;; 



;;;
;;; Miscellaneous general support functions.
;;; 
(defpackage Miscellaneous
  (:nicknames Misc)
  )

(defpackage FrameSystem
  (:nicknames Frames)
  (:use Misc)
  )


(defpackage Representations
  (:nicknames Reps)
  (:use cl Misc Frames)
  )


(defpackage Meta-AQUA
  (:nicknames MetaAQUA)
  (:use cl
   Misc Frames Representations 
   )
  )


(defpackage Tale-Spin
  (:nicknames TSpin)
  (:use 
   Misc Frames)
  )



(defsystem TALE-SPIN-SYSTEM
    (:pretty-name "Tale-Spin Story Generation System"
     :default-pathname "/fs/metacog/group/systems/Meta-AQUA/Tale-spin/"
     )
  
  ;; The Conceptual Frame Definition Subsystem subsystem.
  (:module frame-system frame-system 
	   )

  ;; The next three modules constitute the Tale-Spin package.
  (:module t-imports ("spin-interface.lisp")
	   (:in-order-to (:compile :load) (:load frame-system))
	   )
  (:module tale-spin1 ("tspin.lisp")
	   (:in-order-to (:compile :load) (:load t-imports))
	   )
  (:module tale-spin2 ("extensions.lisp"
		       "spin-cd-reps.lisp"
		       "data.lisp"
		       "patch.lisp"
		       "mumble.lisp" 
		       "verbs.lisp"
		       "alt-story.lisp"
		       "tspin-tokens-4-meta-aqua"
		       "tspin-2-meta-aqua")
	   (:in-order-to (:compile :load) (:load tale-spin1)))
  )

(defvar *frame-dir* "Frame/"
  "Directory where the frame system is located.")

(load (concatenate 'string 
	*Meta-AQUA-system-dir* *frame-dir* "frame-sys-def"))

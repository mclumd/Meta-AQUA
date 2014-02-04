;;; -*- Mode: LISP; Syntax: Common-lisp; Package: Framesystem; Base: 10 -*-

(in-package :frames)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;		 Meta-AQUA Internal Structures Display Windows
;;;;
;;;;	     Copyright (C) 2005   Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;			   File: patches-frame.lisp
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





;;; prints goal frames on the goal-window (sgeorge )
;;; 
;;; Function defined in file frames.lisp in System Frames,
;;; subdirectory Frame.
;;; 
(defun f.pprint (frame 
		 &optional 
		 (level 1) 
		 (stream *frame-stream*) 
		 processed-list)
  (cond ((or
	  (<= level 0)
	  (null frame))
	 nil)
	((listp frame)
	 (mapcar
	  #'f.pprint
	  frame
	  (make-sequence
	   'list
	   (length frame)
	   :initial-element level)
	  (make-sequence
	   'list
	   (length frame)
	   :initial-element stream)
	  (make-sequence
	   'list
	   (length frame)
	   :initial-element processed-list)))
	((not (boundp frame))
	 
	 (javatools.jlinker:jcall 
	  (javatools.jlinker:jmeth 
	   "java.awt.TextArea" "append" "java.lang.String") 
	  user::*textarea1* 
	  (format
	   nil
	   (str-concat
	    "~%The frame symbol passed to "
	    "f.pprint has no value.~%")))
	 )
	((and (equal level 1)
	      (not (member frame processed-list)))
	 (javatools.jlinker:jcall 
	  (javatools.jlinker:jmeth 
	   "java.awt.TextArea" "append" "java.lang.String") 
	  user::*textarea1* 
	  (format nil "~%~s~%"(pprint frame stream)))
	 
	 (cons frame processed-list))
	((not (member frame processed-list))
	 (javatools.jlinker:jcall 
	  (javatools.jlinker:jmeth 
	   "java.awt.TextArea" "append" "java.lang.String") 
	  user::*textarea1* 
	  (format 
	   nil "~%~s~%" (pprint frame stream)))
	 
	 (javatools.jlinker:jcall 
	  (javatools.jlinker:jmeth 
	   "java.awt.TextArea" "append" "java.lang.String") 
	  user::*textarea1* 
	  (format nil "~%~s~%" stream))
	 
	 (setf processed-list
	   (cons frame processed-list))
	 (let ((frame-value (*FRAME* frame)))
	   (dolist (each-role (f.role-list frame))
	     (setf processed-list
	       (union
		processed-list
		(f.pprint
		 (frame->filler frame-value each-role)
		 (- level 1)
		 stream
		 processed-list)))))))
  )


;;; 
;;; Function defined in file internals.lisp in System Frames,
;;; subdirectory Frame.
;;; 
(defun pprint (frame stream)
  (print-header frame stream)
  (let ((frame-slots
	  (f.slot-list frame)))
    (mapcar
      #'print-slot
      frame-slots
      (make-sequence
	'list
	(length frame-slots)
	:initial-element stream)))
 
  (javatools.jlinker:jcall 
   (javatools.jlinker:jmeth 
    "java.awt.TextArea" "append" "java.lang.String") 
   user::*textarea1* 
   (format 
    nil 
    ")"))
  )


;;; 
;;; prints onto goal-window
;;; 
;;; Function defined in file internals.lisp in System Frames,
;;; subdirectory Frame.
;;; 
(defun print-header (frame stream)

  (javatools.jlinker:jcall 
   (javatools.jlinker:jmeth 
    "java.awt.TextArea" "append" "java.lang.String") 
   user::*textarea1* 
   (format 
    nil "~%Frame ~s has the following value:~%~%" frame))

  (javatools.jlinker:jcall 
   (javatools.jlinker:jmeth 
    "java.awt.TextArea" "append" "java.lang.String") 
   user::*textarea1*  
   (format
    nil "(~s" (first
	       (get-abstraction
		(first
		 (get-abstraction frame))))))
  )


;;; 
;;; Function defined in file internals.lisp in System Frames,
;;; subdirectory Frame.
;;; 
(defun print-slot (slot stream)
  (javatools.jlinker:jcall 
   (javatools.jlinker:jmeth 
    "java.awt.TextArea" "append" "java.lang.String") 
   user::*textarea1* 
   (format 
    nil "~%   (~s ~s)"
    (slot->role slot)
    (slot->filler slot)))
  )


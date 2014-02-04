(in-package :user)

(require :jlinker)
(use-package :javatools.jlinker)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;		 Meta-AQUA Internal Structures Display Windows
;;;;
;;;;	     Copyright (C) 2005   Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;			     File: makewindow.lisp
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



(defvar *textarea1*)
(defvar *textarea2*)

(defvar *jlinker-initialized* nil)

;;; 
;;; Initialize for the Java Linker mechanism for Allegro CL.
;;; 
(defun init-jlink ()
  (unless *jlinker-initialized*
    (chdir 
     (concatenate 'string 
       *Meta-AQUA-system-dir* "java-windows/")
	   )
    (load (concatenate 'string 
	    ;;"c:/poirot/coretrunk/lisp/src/lisp/"  "jl-config")
	    *Meta-AQUA-system-dir* "java-windows/jl-config.cl")
	  )
    (setf *jlinker-initialized* t)
    )
  )


;;; 
;;; Function load-meta-aqua-windows is called in the .clinit.cl after
;;; loading the rest of the Meta-AQUA system. It loads two additional patch
;;; files that change i/o routines so that output goes to the java windows
;;; instead of standard output. It then creates the two special output
;;; windows by invoking the function make-window.
;;; 
(defun load-meta-aqua-windows ()
  (load (concatenate 'string 
	  *Meta-AQUA-system-dir* "java-windows/patches"))
  (load 
   (concatenate 'string 
     *Meta-AQUA-system-dir* "java-windows/patches-frame"))
  ;; Invoked only if not running POIROT. Need better condition :-( 
  ;; [mcox 13sep07]
  (if (not (boundp '*run-connector-immediately*))
      (jlinker-init))
  (make-window)
  (format t "~%~%CALL (user::jlinker-end) TO END DISPLAY WINDOWS.~%~%")
  )



(defun make-window (&aux (displayfont nil) 
			 (bgcolor nil) 
			 (bgcolor1 nil) 
			 (java-window1 nil) 
			 (java-window2 nil))
  ;(jlinker-init)
  (setf java-window1 
    (jnew (jcons "java.awt.Frame" "java.lang.String") 
	  "Meta-AQUA : Goal Monitor"))  
  (setf java-window2 
    (jnew (jcons "java.awt.Frame" "java.lang.String") 
	  "Meta-AQUA : Memory Monitor"))  
  (jcall (jmeth "java.awt.Frame" "setSize" "int" "int") 
	 java-window1 400 400)
  (jcall (jmeth "java.awt.Frame" "setSize" "int" "int") 
	 java-window2  400 400)
  (setf displayfont  
    (jnew (jcons "java.awt.Font" "java.lang.String" "int" "int") "" 0 11))
  (setf bgcolor 
    (jnew (jcons "java.awt.Color" "int" "int" "int") 250 200 200))
  (setf bgcolor1 
    (jnew (jcons "java.awt.Color" "int" "int" "int") 250 200 200))
  (setf *textarea1* 
    (jnew (jcons "java.awt.TextArea")))
  (jcall (jmeth "java.awt.TextComponent" "setEditable" "boolean") 
	 *textarea1* (make-immediate-object nil :boolean))
  (jcall (jmeth "java.awt.Component" "setBackground" "java.awt.Color") 
	 *textarea1* bgcolor)
  (jcall (jmeth "java.awt.Component" "setFont" "java.awt.Font") 
	 *textarea1* displayfont)
  (jcall (jmeth "java.awt.Frame" "add" "java.awt.Component" ) 
	 java-window1 *textarea1*)
  (jcall (jmeth "java.awt.Frame" "show") 
	 java-window1)
  (setf *textarea2* 
    (jnew (jcons "java.awt.TextArea")))
  (jcall (jmeth "java.awt.Component" "setBackground" "java.awt.Color") 
	 *textarea2* bgcolor1)
  (jcall (jmeth "java.awt.Component" "setFont" "java.awt.Font") 
	 *textarea2* displayfont)
  (jcall (jmeth "java.awt.TextComponent" "setEditable" "boolean") 
	 *textarea2* (make-immediate-object nil :boolean))
  (jcall (jmeth "java.awt.Frame" "add" "java.awt.Component") 
	 java-window2 *textarea2*)
  (jcall (jmeth "java.awt.Frame" "show") 
	 java-window2)
)












(in-package :user)
(require :jlinker)
;(use-package :javatools.jlinker)

(in-package :metaaqua)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;		 Meta-AQUA Internal Structures Display Windows
;;;;
;;;;	     Copyright (C) 2005   Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;			      File: patches.lisp
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
;;; The following functions call goal-monitor-mode-p
;;; 
;;; META-AQUA, :OPERATOR --same also calls memory-monitor-mode-p
;;; TOGGLE-STRUCTURES-WINDOW, :OPERATOR --just swaps the windows
;;; PRINT-GOAL-STATUS, :OPERATOR --done
;;; PREP, :OPERATOR --ceates windows
;;; USER-QUITS-P, :OPERATOR



;;; 
;;; Changed to print goals on goal-window (sgeorge)
;;; Function defined in file lowlevel.lisp
;;; 
(defun print-goal-status (next-goal goal-queue &optional calling-function)
  (let ((previous-window *aqua-window*))
    (current-window *window3*)
    (format-if
     calling-function
     nil
     `"~%Called by function ~s."
     calling-function)
    
    (javatools.jlinker:jcall 
     (javatools.jlinker:jmeth 
      "java.awt.TextArea" "append" "java.lang.String") 
     user::*textarea1* 
     (format
      nil "~%~%Goal Queue  --> ~s~%"
      (list-queue goal-queue)))
    
    (javatools.jlinker:jcall 
     (javatools.jlinker:jmeth 
      "java.awt.TextArea" "append" "java.lang.String") 
     user::*textarea1*  
     (format
      nil "Next Goal   --> ~s~%" next-goal))
    
    (javatools.jlinker:jcall 
     (javatools.jlinker:jmeth 
      "java.awt.TextArea" "append" "java.lang.String") 
     user::*textarea1*
     (format 
      nil "~%~s~%" (f.pprint next-goal)))
    
    (javatools.jlinker:jcall 
     (javatools.jlinker:jmeth 
      "java.awt.TextArea" "append" "java.lang.String") 
     user::*textarea1*
     (format
      nil "~%Goal-State  --> ~s ~s ~s~%"
      (goal-actor next-goal)
      (first (get-abstraction
	      (goal-object next-goal)))
      (goal-state next-goal
		  )))
    
    (current-window previous-window))
  )


;;; Function defined in file lowlevel.lisp
;;; 
(defun print-cycle-division (&optional title-string)
  (let ((previous-window *aqua-window*))
    (current-window *window3*)
 
    (javatools.jlinker:jcall 
     (javatools.jlinker:jmeth 
      "java.awt.TextArea" "append" "java.lang.String") 
     user::*textarea1* 
     (format
     nil
      (str-concat
	"-----------------"
	(or title-string
	    "new-cycle")
	"-----------------~%")))
 
    (javatools.jlinker:jcall 
     (javatools.jlinker:jmeth 
      "java.awt.TextArea" "append" "java.lang.String") 
     user::*textarea2* 
     (format
     nil
      (str-concat
	"-----------------"
	(or title-string
	    "new-cycle")
	"-----------------~%")))
   
	(current-window previous-window))
   )
  

;;; Function defined in file memory.lisp.
;;;
(defun announce-storage (memory-item)
  "Print memory item as they are stored in memory."
  
      (let ((previous-window *aqua-window*))
	;; Change to structure window.
	(current-window *window3*)
	
	(javatools.jlinker:jcall 
	 (javatools.jlinker:jmeth 
	  "java.awt.TextArea" "append" "java.lang.String") 
	 user::*textarea2*  
	 (format
	 nil            ;;;memory.lisp
	  "~%~%Storing new memory  --> ~s~%"
	  memory-item))
	
	;; Change back to output window.
	(current-window previous-window))
      )


;;; 
;;; no if statement needed coz both goal and memory are taking place
;;; simultaenously (sgeorge)
;;; 
;;; Function defined in file memory.lisp.
;;;
(defun announce-index (memory-item relation index memory-type)
  "Print memory activity as indexes are added/changed in memory."
      (let ((previous-window *aqua-window*))
	;; Change to structure window.
	(current-window *window3*)
	(cond ((null memory-item)
	
	       (javatools.jlinker:jcall 
		(javatools.jlinker:jmeth 
		 "java.awt.TextArea" "append" "java.lang.String") 
		user::*textarea2*
	      (format
		nil
		 "~%~%Index is being removed on memory item ~s.~%"
		 memory-item))
	
	       
	       (javatools.jlinker:jcall 
		(javatools.jlinker:jmeth 
		 "java.awt.TextArea" "append" "java.lang.String") 
		user::*textarea2*	       
		(format
		 nil
		 "~%~%Item was indexed by relation ~s.~%~%"
		 relation))
					
	       )
	      
	      (t
	       (javatools.jlinker:jcall 
		(javatools.jlinker:jmeth 
		 "java.awt.TextArea" "append" "java.lang.String") 
		user::*textarea2*
	       (format
		nil
		 "~%~%Memory item ~s (of type ~s) stored under index ~s.~%"
		 memory-item
		 memory-type
		 index))
	 
	       (javatools.jlinker:jcall 
		(javatools.jlinker:jmeth 
		 "java.awt.TextArea" "append" "java.lang.String") 
		user::*textarea2*
	       (format
		 nil
		 "~%~%Relation ~s was used for full indexing.~%~%"
		 relation))
					      ))
	;; Change back to output window.
	(current-window previous-window))
  )


;;; 
;;; Function announce-retrieval prints to the internal-structures
;;; window if in memory printing mode. It shows ...
;;;
;;; NOTE that it is imperative that announce-retrieval return the
;;; retrieved-item passed to it.
;;; 
;;; Function defined in file memory.lisp.
;;;
(defun announce-retrieval (retrieved-item memory-type relation)
  "Print memory activity as new items are retrieved from memory."
      (let ((previous-window *aqua-window*))
	;; Change to structure window.
	(current-window *window3*)
	(cond ((null retrieved-item)
	       
	       (javatools.jlinker:jcall 
		(javatools.jlinker:jmeth 
		 "java.awt.TextArea" "append" "java.lang.String") 
		user::*textarea2*	       
		(format
		nil
		 "~%~%No item "
		 memory-type
		 relation))
	       )
	      
	      (t
	       (javatools.jlinker:jcall 
		(javatools.jlinker:jmeth 
		 "java.awt.TextArea" "append" "java.lang.String") 
		user::*textarea2*
	       (format
		 nil
		 "~%~%Memory item ~s "
		 retrieved-item))
	       ))
	(javatools.jlinker:jcall 
	 (javatools.jlinker:jmeth 
	  "java.awt.TextArea" "append" "java.lang.String") 
	 user::*textarea2*	
	 (format
	  nil
	  "was retrieved given memory type ~s and cue ~s.~%~%"
	  memory-type
	  relation))
	
	;; Change back to output window.
	(current-window previous-window))
  retrieved-item
  
)



;;; 
;;; Function defined in file lowlevel.lisp.
;;; 
(defun goal-monitor-mode-p ()
  t)


;;; 
;;; Function defined in file lowlevel.lisp.
;;; 
(defun memory-monitor-mode-p ()
  t)



(defun quit-meta-aqua-windows ()
  (javatools.jlinker:jlinker-end)
  "To run Meta-AQUA again with windows call reload-meta-aqua-windows ()")
  

(defun reload-meta-aqua-windows ()
  (user::make-window))



(in-package :user)


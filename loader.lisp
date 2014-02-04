(in-package :user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;	    The Meta-AQUA Introspective Multistrategy Learning System
;;;;				   Version 6
;;;; 
;;;			     META-AQUA SYSTEM LOADER
;;;
;;; 	      Copyright (C) 1996   Michael T. Cox   (mcox25@covad.net)
;;;
;;; IMPORTANT, YOU MUST CHANGE THE *Meta-AQUA-system-dir* VARIABLE BELOW. YOU
;;; CAN ACCOMPLISH THIS WITHOUT RECOMPILING THE CODE OR CHANGING THE SOURCE BY
;;; PLACING A SETF CALL IN YOUR .clinit.cl INITIALIZATION FILE.  HOWEVER YOU
;;; *MUST* CHANGE THE :default-pathname VALUE IN THE DEFSYSTEM BELOW
;;; EXPLICITLY.
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
;;; This file provides the system definition for Meta-AQUA Version VI. This
;;; version represents a public release of the code under the GNU License. The
;;; Meta-AQUA system is composed of a main system (defined below) in the
;;; directory defined by *Meta-AQUA-system-dir* and three supporting
;;; subsystems. They are the Frame Definition System found in subdirectory
;;; *frame-dir*, the Nonline Planning System vers. 1.3 found in subdirectory
;;; *nonlin-dir*, and the Tale-Spin Story Generation System found in
;;; subdirectory *tspin-dir*. The Tale-Spin Subsystem provides pseudo-randomly
;;; generated input stories for Meta-AQUA to understand during evaluation. The
;;; Representations subdirectory contains a conceptual hierarchy for Meta-AQUA
;;; written in the frame language defined by the Frame System. The java-windows
;;; subdirectory contains optional code to send output concerning memory to a
;;; special Meta-AQUA Memory Monitor Window and output concerning goals to a
;;; special Goal Monitor Window. The Results directory exists to store output
;;; files for evaluation and for bookkeeping and error data.
;;; 
;;; The file loader.lisp will load the system the first time the file is loaded
;;; into Lisp. The file also defines the init-Meta-AQUA function. After loading
;;; the system the user can call (init-Meta-AQUA). This invocation will prepare
;;; Meta-AQUA for the standard demo usage. Run a session by then typing
;;; (Meta-AQUA) or (Meta-AQUA t) to suppress interactive pauses. To finish a
;;; session that uses the optional Java output windows, do the following.
;;; (meta-aqua::quit-meta-aqua-windows)
;;; 
;;; See http://hcs.bbn.com/personnel/Cox/thesis/meta-aqua-chapt.html#REF19222
;;;     for further info on the Meta-AQUA implementation.
;;; See http://www.cs.umd.edu/projects/plus/Nonlin/ for further info on Nonlin.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; HISTORY
;;; 
;;; 3dec05 (#1-5 below) and 10-11dec05 (#6-8)
;;; 
;;; Made a number of cleanup changes to support the public release that include
;;; the following.
;;; 
;;; 1. Changed back to using the defsystem declaration below. This allowed not
;;; only better control during system wide operations such as compile and load,
;;; but the load now does not needed to use the global variable string lists
;;; that exhaustively enumerated the files and paths to the files. It was
;;; previously awkward to port the system to different directory structures or
;;; machines. The current change simplified the specification of a root
;;; directory using just three globals (*Meta-AQUA-system-dir*, *frame-dir*,
;;; and *nonlin-dir*) for the system and two subsystems together with the
;;; :default-pathname specification in the defsystem declarations.
;;; 
;;; 2. Changed the system module load order. Although I had previously moved up
;;; t-imports module def to right after m-interface in the defsystem found
;;; below, I had not changed the :in-order-to specification. By changing
;;; :in-order-to of t-imports, lowlevel, and microaqua the package definition
;;; load when they need to. See 3 next.
;;; 
;;; 3. Moved the defsystem declaration here from a previous file called
;;; system6.lisp. Moved the package definitions for the various modules from
;;; that file into files belonging to the respective modules. Moved
;;; Representations def into reps-import.lisp; moved Meta-AQUA def into file
;;; meta-aqua-interface.lisp; and moved Tale-Spin def into spin-interface.lisp.
;;; 
;;; 4. Removed old directions that function init-aqua (file init.lisp) printed
;;; to standard output concerning mouse clicks and special control key
;;; sequences for old versions of Meta-AQUA that ran on Symbolics machines.
;;; 
;;; 5. Removed now obsolete backup-system function from file lowlevel.lisp.
;;; This function used the now discontinued global lists of file pathname
;;; strings.
;;; 
;;; 6. Made Tale-Sping into a separate subsystem. Split the source files into
;;; three modules, t-import, tale-spin1, and tale-spin2, such that tale-spin1
;;; has the single file tspin.lisp. This was necessary so that the file was
;;; loaded before other files and the loc function existed. Otherwise errors
;;; occurs with binary loading. Added the variable *tspin-dir* below. Added
;;; file tspin-sys-def.lisp to the Tale-Spin source code.
;;; 
;;; 7. Added *do-compile-Meta-AQUA* variable to allow user to easily compile
;;; system during installation. Wrote INSTALL file. Modified the Lisp
;;; initialization code (in README.clinit.cl) to facilitate compiling and
;;; loading.
;;; 
;;; 8. Added jvwindows module to the Meta-AQUA system definition. Modified the
;;; Lisp initialization code (in README.clinit.cl) to facilitate.
;;; 
;;; 
;;; 14jan99
;;; Problem with ACL and package locks (e.g., function debug in nonlin sys).
;;; Added the following (from Norvig's code) to mask the problem.
(eval-when (eval compile load)
  ;; Make it ok to place a function definition on a built-in LISP symbol.
  #+EXCL
  (dolist (pkg '(excl common-lisp))
    (setf (excl:package-definition-lock (find-package pkg)) nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Directories
;;;
;;;;;;;;;;

(defvar *Meta-AQUA-system-dir* 
    "/fs/metacog/group/systems/Meta-AQUA/"
  "Directory where the Meta-AQUA system is rooted.")

(defvar *frame-dir* "Frame/"
  "Directory where the frame system is located.")

(defvar *nonlin-dir* "Nonlin/"
  "Directory where the Nonlin system is located.")

(defvar *tspin-dir* "Tale-spin/"
  "Directory where the Tale-Spin system is located.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Flags
;;;
;;;;;;;;;;

(defvar *do-compile-Meta-AQUA* nil
  "Set  to t to compile instead of load system.")

(defvar *maqua-loaded* nil 
  "If true, then Meta-AQUA already loaded.")



;;; 
;;; Some (partially dated) documentation for the Meta-AQUA System
;;; Implementation is in the file Meta-AQUA-doc.txt.
;;;
(defsystem AQUA6
    (:pretty-name "Meta-AQUA Version VI"
     :default-pathname "/fs/metacog/group/systems/Meta-AQUA/"
     )
  
  ;; The Conceptual Frame Definition Subsystem subsystem.
  (:module frame-system frame-system 
	   )

  ;; The next two modules constitute the Representations module.
  (:module r-imports "Representations/reps-import"
	   (:in-order-to (:compile :load) (:load frame-system))
	   )

  (:module representations
	   ("Representations/rep_smuggle4"
	    "Representations/rep_meta-xps"
	    "Representations/rep_planner"
	    "Representations/rep_lisp-programmer2"
	    "Representations/rep_tspin"
	    "Representations/rep_hit"
	    "Representations/rep_scripts"
	    "Representations/rep_cop_scripts"
	    "Representations/rep_burglarscript"
	    "Representations/rep_poirot"
	    "Representations/rep_roles"
	    "Representations/rep_fire"
	    )
	   (:in-order-to (:compile :load) (:load r-imports))
	   )
  
  ;; The Nonlin Planning Subsystem
  (:module nonlin-system NONLIN-SYSTEM )

  ;; The next three (not including t-imports) modules constitute the Meta-AQUA package.
  (:module m-interface ("meta-aqua-interface")
	   ;; |||||| REPS or TSPIN?
	   (:in-order-to (:compile :load) (:load representations nonlin-system))	
	   )

  ;; The Tale-Spin Story Generation system.
  (:module TALE-SPIN-SYSTEM TALE-SPIN-SYSTEM
	   (:in-order-to (:compile :load) (:load m-interface))
	   )

  (:module low-level ("constants"
		      "lowlevel")
	   (:in-order-to (:compile :load) (:load TALE-SPIN-SYSTEM
						 ))
	   )

  (:module main ("story-input"		; Hand-crafted input examples
		 "fire-story"		; Newly added MIDCA story (12)
		 "goal-q"		; Goal priority-queue management
		 "memory"		; Memory management
		 "meta-xp-procs"	; Procedures for handling meta-xps
		 "learner"		; Learning library and associated functions
		 "script-applier"	; Micro mini-SAM (Script Applier Mechanism).
		 "questions"		; Question handling
		 "explainer"		; Explanation facility
		 "cbr"			; Ultra-simple case-based reasoner
		 "understander"		; Main story-understanding code
		 "solver"		; Main code for problem solving mode
		 "eval"			; Evaluation functions that calculate learning performance
		 "init"			; Initialization functions
		 "main")		; The main control loop and sysnonym functions.
	   (:in-order-to (:compile :load) (:load low-level))
	   )
  
  (:module jvwindows (;"java-windows/jl-config.lisp"
		      "java-windows/makewindow.lisp")			
	   (:in-order-to (:compile :load) (:load main))
	   )
  )



(when (not *maqua-loaded*)
  (load (concatenate 'string 
	  *Meta-AQUA-system-dir* *frame-dir* "frame-sys-def"))
  (load (concatenate 'string 
	  *Meta-AQUA-system-dir* *nonlin-dir* "nonlin-sys-def"))
  (load (concatenate 'string 
	  *Meta-AQUA-system-dir* *tspin-dir* "tspin-sys-def"))
  (if *do-compile-Meta-AQUA* 
      (compile-system 'AQUA6)
    (load-system 'AQUA6))
  (setf *maqua-loaded* t)
      )


;;; 
;;; The file init.lisp defines these initialization functions.
;;; 
(defun init-Meta-AQUA () 
  (in-package :meta-aqua) 
  ;; Added 4aug11 mcox so that opened files will be in Results directory
  (setf *default-pathname-defaults* 
    (pathname 
     (concatenate 
	 'string 
       *META-AQUA-SYSTEM-DIR* 
       "Results/")))
  (meta-aqua::init-4-speed) 
  (meta-aqua::init-aqua)
  (format 
   t "~%Meta-AQUA, Version 6, Copyright (C) 2005 Michael T. Cox~%")
  (format 
   t "~%Meta-AQUA comes with ABSOLUTELY NO WARRANTY.~%")
  (format 
   t "~%This is free software, and you are welcome to redistribute it")
   (format 
    t "~%under certain conditions as defined in the Gnu General Public License,")
   (format 
    t "~%Version 2.~%~%")
  )

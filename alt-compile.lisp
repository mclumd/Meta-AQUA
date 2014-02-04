(in-package :user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	    The Meta-AQUA Introspective Multistrategy Learning System
;;;;				   Version 6
;;;;
;;;;	     Copyright (C) 1996   Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;			     File: alt-compile.lisp
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
;;; If you do not use Allegro Lisp, the defsystem macros and related code to
;;; load and compile the Meta-AQUA system will not work. Comment out the
;;; defsystem calls in the source code and try to use this. Start by changing
;;; the strings in the variables below to reflect file names and paths to your
;;; source code. Then follow the instructions in the subsequent comment. 
;;; [mcox 26dec05]


;;; 
;;; [mcox 10jan04]
;;; 
;;; Delete all fasl (binary) files first.
;;; Next Load Meta-AQUA 
;;; Then load this file to do the compiles.
;;;

;;; Removed .lisp extensions and added pathname prefix. [cox 5mar97]
(defvar *tspin-mod*
	'("~/PotalaFiles/Meta-AQUA/Tale-spin/spin-interface"
	  "~/PotalaFiles/Meta-AQUA/Tale-spin/tspin"
	  "~/PotalaFiles/Meta-AQUA/Tale-spin/extensions"
	  "~/PotalaFiles/Meta-AQUA/Tale-spin/spin-cd-reps"
	  "~/PotalaFiles/Meta-AQUA/Tale-spin/data"
	  "~/PotalaFiles/Meta-AQUA/Tale-spin/patch"
	  "~/PotalaFiles/Meta-AQUA/Tale-spin/mumble" 
	  "~/PotalaFiles/Meta-AQUA/Tale-spin/verbs"
	  "~/PotalaFiles/Meta-AQUA/Tale-spin/alt-story"
	  "~/PotalaFiles/Meta-AQUA/Tale-spin/tspin-tokens-4-meta-aqua"
	  "~/PotalaFiles/Meta-AQUA/Tale-spin/tspin-2-meta-aqua")
  "List of filenames for Tale-Spin")

;;; Removed .lisp extensions, added pathname prefix, and commented out 
;;; doc file, the meta-aqua-interface file, and system5 [cox 5mar97]
(defvar *meta-aqua-mod*
	'(
;	  "~/PotalaFiles/Meta-AQUA/Meta-AQUA.doc"
;	  "~/PotalaFiles/Meta-AQUA/system5"
;	  "~/PotalaFiles/Meta-AQUA/meta-aqua-interface"
	  "~/PotalaFiles/Meta-AQUA/constants"
	  "~/PotalaFiles/Meta-AQUA/lowlevel"
	  "~/PotalaFiles/Meta-AQUA/story-input"
	  "~/PotalaFiles/Meta-AQUA/goal-q"
	  "~/PotalaFiles/Meta-AQUA/memory"
	  "~/PotalaFiles/Meta-AQUA/meta-xp-procs"
	  "~/PotalaFiles/Meta-AQUA/learner"	
	  "~/PotalaFiles/Meta-AQUA/script-applier"
	  "~/PotalaFiles/Meta-AQUA/questions"	
	  "~/PotalaFiles/Meta-AQUA/explainer"	
	  "~/PotalaFiles/Meta-AQUA/cbr"	
	  "~/PotalaFiles/Meta-AQUA/understander"
	  "~/PotalaFiles/Meta-AQUA/solver"	
	  "~/PotalaFiles/Meta-AQUA/eval"
	  "~/PotalaFiles/Meta-AQUA/init"	
	  "~/PotalaFiles/Meta-AQUA/main")
  "List of filenames for Main")

;;; Removed .lisp extensions and added pathname prefix. [cox 5mar97]
(defvar *reps-mod*
	'(
	  "~/PotalaFiles/Meta-AQUA/Representations/reps-import"
	  "~/PotalaFiles/Meta-AQUA/Representations/rep_smuggle4"
	  "~/PotalaFiles/Meta-AQUA/Representations/rep_meta-xps"
	  "~/PotalaFiles/Meta-AQUA/Representations/rep_planner"
	  "~/PotalaFiles/Meta-AQUA/Representations/rep_lisp-programmer2"
	  "~/PotalaFiles/Meta-AQUA/Representations/rep_tspin"
	  "~/PotalaFiles/Meta-AQUA/Representations/rep_hit"
	  "~/PotalaFiles/Meta-AQUA/Representations/rep_scripts"
	  "~/PotalaFiles/Meta-AQUA/Representations/rep_cop_scripts"
	  "~/PotalaFiles/Meta-AQUA/Representations/rep_burglarscript" ;Added for GOBAT/D [cox 12oct99]
	  "~/PotalaFiles/Meta-AQUA/Representations/rep_roles")
  "List of filenames for Representations"
  )

(defvar *nonlin-sys*
    '(
					;Commented out system definition file below. 
					;Also removed .lisp extensions 14jan99
      ;"~PotalaFiles/Meta-AQUA/Nonlin/nonlin-sys-def"
    "~/PotalaFiles/Meta-AQUA/Nonlin/nonlin-interface"
    "~/PotalaFiles/Meta-AQUA/Nonlin/util"
    "~/PotalaFiles/Meta-AQUA/Nonlin/def"
    "~/PotalaFiles/Meta-AQUA/Nonlin/unify"
    "~/PotalaFiles/Meta-AQUA/Nonlin/macros"
    "~/PotalaFiles/Meta-AQUA/Nonlin/backtrack"
    "~/PotalaFiles/Meta-AQUA/Nonlin/dev"
    "~/PotalaFiles/Meta-AQUA/Nonlin/establish"
    "~/PotalaFiles/Meta-AQUA/Nonlin/expand"
    "~/PotalaFiles/Meta-AQUA/Nonlin/gost"
    "~/PotalaFiles/Meta-AQUA/Nonlin/init-planner"
    "~/PotalaFiles/Meta-AQUA/Nonlin/link"
    "~/PotalaFiles/Meta-AQUA/Nonlin/mark"
    "~/PotalaFiles/Meta-AQUA/Nonlin/plan"
    "~/PotalaFiles/Meta-AQUA/Nonlin/printplan"
    "~/PotalaFiles/Meta-AQUA/Nonlin/readschema"
    "~/PotalaFiles/Meta-AQUA/Nonlin/schema"
    "~/PotalaFiles/Meta-AQUA/Nonlin/tome"
    )
  "List of filenames for Nonlin System.")

(defvar *frame-sys*
  '(
					;Commented out system definition file below. 
					;Also removed .lisp extensions 14jan99
      ;"~/PotalaFiles/Meta-AQUA/Frame/frame-sys-def.lisp"
    "~/PotalaFiles/Meta-AQUA/Frame/utils"
    "~/PotalaFiles/Meta-AQUA/Frame/break-facility"
    "~/PotalaFiles/Meta-AQUA/Frame/local-constants"
    "~/PotalaFiles/Meta-AQUA/Frame/isa"
    "~/PotalaFiles/Meta-AQUA/Frame/frame"
    "~/PotalaFiles/Meta-AQUA/Frame/internals"
    "~/PotalaFiles/Meta-AQUA/Frame/exported-symbols"
    "~/PotalaFiles/Meta-AQUA/Frame/frame-interface"
    )
  "List of filenames for Frame System.")




(compile-file "c:/Documents and Settings/mcox/My Documents/PotalaFiles/Meta-AQUA/Frame/frame-sys-def.lisp")
(mapcar #'compile-file CL-USER::*FRAME-SYS*)

(compile-file "c:/Documents and Settings/mcox/My Documents/PotalaFiles/Meta-AQUA/Nonlin/nonlin-sys-def")
(mapcar #'compile-file cl-user::*nonlin-sys*)

(compile-file "c:/Documents and Settings/mcox/My Documents/PotalaFiles/Meta-AQUA/meta-aqua-interface")
(compile-file "c:/Documents and Settings/mcox/My Documents/PotalaFiles/Meta-AQUA/Tale-spin/spin-interface")
(mapcar #'compile-file cl-user::*tspin-mod*)
(mapcar #'compile-file cl-user::*meta-aqua-mod*)

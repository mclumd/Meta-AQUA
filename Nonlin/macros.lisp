;;;-*- Mode: Lisp; Package: NONLIN -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;    Application:  NONLIN
;;;;  Appl. Version:  1.3
;;;;     Appl. Date:  Oct. 1992
;;;;
;;;;           File:  MACROS.LISP
;;;;       Contents:  Misc. Macros
;;;;
;;;;                  Dept. of Computer Science, University of Maryland at College Park
;;;;                  email: nonlin-users-request@cs.umd.edu
;;;;
;;;;       Language:  Macintosh Common Lisp 2.0
;;;;
;;;;   Requirements:  package NONLIN 
;;;;
;;;;  History:
;;;;  Date      Who  Modification
;;;;  ---------------------------------------------------------------------------------   
;;;;  11/22/92  bpk - cleanup, doc.

(in-package :nonlin)

;;;   ********* LOAD THIS FILE AFTER UTIL.LISP, DEF.LISP, UNIFY.LISP **********

; from DEF.LISP
(defmacro predicate (pattern)
   `(car ,pattern)
)

; from GOST.LISP
(defmacro relevant-gost-entrys (pattern)
   "Returns list of gost-entry structures with (ground) pattern."

    `(gethash (predicate ,pattern) *gost*)
)

    
; from READSCHEMA.LISP
(defmacro get-relevant-schemas (pattern)
   "Returns schemas whose :todo pattern (which may have vars.) matches pattern."

    `(gethash (predicate ,pattern) *schematable*)
)


(defmacro enter-schema-table (schema)
   "Enter schema into Schema Table."

    `(push ,schema (gethash (predicate (schema-todo ,schema)) *schematable*))
)


; from SCHEMA.LISP
(defmacro destructive-replace-variables (place binding)
   "Destructively replaces vars. at place with a binding."
    `(setf ,place (replace-variables ,place ,binding))
    ;; replace-variables comes from unify routines
)

; from TOME.LISP
(defmacro  enter-tome-table (entry)
   "Add tome-entry structure to TOME."

    `(push ,entry (gethash (predicate (tome-entry-effect ,entry)) 
	   *tome*))
)

(defmacro relevant-tome-entrys (pattern)
   "Returns list of tome-entry structures with pattern."

    `(gethash (predicate ,pattern) *tome*)
)

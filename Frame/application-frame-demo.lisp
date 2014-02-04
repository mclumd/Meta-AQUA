;;; -*- Mode: LISP; Syntax: Common-lisp; Package: MY-PACKAGE; Base: 10 -*-



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	        A Frame System for Conceptual Construction (FS4CC)
;;;;
;;;;	    Copyright (C) 1994   Michael T. Cox   (cox@cc.gatech.edu)
;;;;
;;;;
;;;;                     File: application-frame-demo.lisp
;;;;
;;;;     This file provides the minimum necessary calls to create a user
;;;;     application on top of the Frame System. In order to actually run
;;;;     such an application one must load the system definition file (see
;;;;     file frame-sys-def.lisp).
;;;;
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
;;           FRAME USER APPLICATION SHELL AND DEMO
;;
;;
;;           DEMONSTRATION OF BASIC FUNCTION CALLS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; The following is a couple of examples of calls
;;; to the frame system.

(defvar test-token nil)
(defvar person1 nil)


;;; Entity is the root of the isa hierarchy.
;;; Every concept isa entity, either directly or by
;;; transitivity.
;;;
;;; Note the variable binding =actor.
;;; This means that whatever frame is in the actor slot
;;; is also in the owned-by slot of the dog frame. 

(define-frame MY-TYPE
  (isa (value (entity)))
  (actor (value (person)))
  (possession
    (value
      (dog
	(owned-by (value =actor)))))
  )


(define-frame PERSON
  (isa (value (volitional-agent)))
  (name (value (literal)))
  (gender (value (gender-value)))
  )

(define-frame DOG
  (isa (value (animate-object)))
  (name (value (literal)))
  (gender (value (gender-value)))
  )

(define-attribute-value MALE.0
  (isa (value (gender-value)))
  )

(define-attribute-value FEMALE.0
  (isa (value (gender-value)))
  )

(define-frame GENDER-VALUE
  (isa (value (attribute-value)))
  )

;; Each of the following definitions should actually have additional attributes.

(define-frame OBJECT
  (isa (value (entity)))
  )

(define-frame INANIMATE-OBJECT
  (isa (value (object)))
  )


(define-frame ANIMATE-OBJECT
  (isa (value (entity)))
  )


(define-frame VOLITIONAL-AGENT
  (isa (value (animate-object)))
  )


;; Instantiate some tokens.

(setf test-token (f.instantiate-frame my-type))




(setf person1 (f.instantiate-frame
		'(person
		   (gender (value male.0)))
		))


;;; NOTE that put will work here ONLY because the call
;;; of f.instantiate-frame did not include a name slot.
;;; If there was a name slot, then one would have had
;;; to use f.put! or f.unify.

(f.put (f.instantiate-literal "Michael")
       person1
       'name)



;;; Now place person1 as the value facet of the actor slot
;;; of the test token. (NOTE that the last parameter passed to f.get is
;;; an optional parameter. The value facet is the default,
;;; so in the following case could have been ommitted.
;;;
;;; Also NOTE that the variable is still consistent because
;;; of the setf. See comments in definition of f.unify.
;;;
;;; If f.put! had been used instead of f.unify then the variable
;;; binding for who owns the dog would have been broken.

(setf person1
      (f.unify person1
	     (f.get test-token 'actor *value-facet*)))




;;;
;;; Function micro-demo displays the structure of the simple
;;; frames created above and it illustrates the role and
;;; calling convention of various functions in the Frame System.
;;; It also shows a useful method of navigating the frame network
;;; (created during frame instantiation) with the f.traverse-frame
;;; function. Another useful excercise is to type (inspect 'test-token).
;;; Then explore the frame structure manually. This will provide an
;;; interactive ability to "zoom" through the frame variable and list
;;; components. To exit the inspector simply select "exit" from the menmu.
;;; 
(defun micro-demo ()
  (format t "~%The following is a test frame: ~s.~%"
	  (*FRAME* test-token))
  (format t "~%The actor filler of ~s is ~s.~%"
	  test-token
	  (f.get test-token 'actor))
  (format t "~%The value of the actor ~s is ~s.~%"
	  (f.get test-token 'actor)
	  (*FRAME* (f.get test-token 'actor)))
  ; I did not have to use var person1 here.
  (format t "~%And the name of the person is ~s."
	  (*FRAME* (f.chase-path test-token 'actor 'name)))
  (format t "~%And its type is ~s.~%"
	  (get-abstraction person1))

  (if (y-or-n-p "~%Continue ? ")
      (block nil
	(format t "~%Now for a little recursive loop through the test token ~s."
		test-token)

	(f.traverse-frame
	  test-token
	  #'(lambda (each-node parent role facet-name level target)
	      (format t "~%~%~%For the frame ~s:"
		      each-node)
	      (if (null parent)
		  (format t "~%")
		  (format t "~% (Which is in the ~s slot of frame ~s)~%"
			  role
			  parent))
	      (if (literal-p each-node)
		  (format t "~%The value of the literal is ~s."
			  (*FRAME* each-node))
		  (dolist (each-role (f.role-list each-node))
		    (dolist (each-facet-name (f.facet-list each-node each-role))
		      (format t "~%The ~s facet of the ~s role is ~s."
			      each-facet-name
			      each-role
			      (f.get each-node each-role each-facet-name))))))
	  nil)
  
	(format t "~%~%Finished micro-demo.~%~%")))
  )



;;; The following offers to execute the demo after loading the file.

(format t "~%To execute the Frame System demo simply")
(format t "~%respond with a Y or type (micro-demo) at your leisure.")
(terpri)
(if (y-or-n-p "~%Continue ? ")
    (micro-demo))




;;; This is the output of a micro-demo call above.

;Command: (micro-demo)
;The following is a test frame: (MY-TYPE (ACTOR (VALUE PERSON.4)) (POSSESSION (VALUE DOG.3))).
;
;The actor filler of MY-TYPE.1 is PERSON.4.
;
;The value of the actor PERSON.4 is (PERSON (GENDER (VALUE MALE.0)) (NAME (VALUE LITERAL.5))).
;
;And the name of the person is "Michael".
;And its type is (PERSON).
;
;Continue ? (Y or N) Yes.
;Now for a little recursive loop through the test token MY-TYPE.1.
;
;
;For the frame MY-TYPE.1:
;
;The VALUE facet of the ACTOR role is PERSON.4.
;The VALUE facet of the POSSESSION role is DOG.3.
;
;
;For the frame PERSON.4:
; (Which is in the ACTOR slot of frame MY-TYPE.1)
;
;The VALUE facet of the GENDER role is MALE.0.
;The VALUE facet of the NAME role is LITERAL.5.
;
;
;For the frame MALE.0:
; (Which is in the GENDER slot of frame PERSON.4)
;
;The VALUE facet of the STATUS role is PREDEFINED-INSTANCE.0.
;
;
;For the frame PREDEFINED-INSTANCE.0:
; (Which is in the STATUS slot of frame MALE.0)
;
;The VALUE facet of the STATUS role is PREDEFINED-INSTANCE.0.
;
;
;For the frame PREDEFINED-INSTANCE.0:
; (Which is in the STATUS slot of frame PREDEFINED-INSTANCE.0)
;
;The VALUE facet of the STATUS role is PREDEFINED-INSTANCE.0.
;
;
;For the frame LITERAL.5:
; (Which is in the NAME slot of frame PERSON.4)
;
;The value of the literal is "Michael".
;
;
;For the frame DOG.3:
; (Which is in the POSSESSION slot of frame MY-TYPE.1)
;
;The VALUE facet of the OWNED-BY role is PERSON.4.
;
;
;For the frame PERSON.4:
; (Which is in the OWNED-BY slot of frame DOG.3)
;
;The VALUE facet of the GENDER role is MALE.0.
;The VALUE facet of the NAME role is LITERAL.5.
;
;Finished micro-demo.
;
;NIL

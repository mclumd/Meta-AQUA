;;; -*- Mode: LISP; Syntax: Common-lisp; Package: Representations; Base: 10 -*-

(in-package :reps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	      Meta-AQUA Background Knowledge Represented as Frames
;;;;
;;;;	    Copyright (C) 1996   Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;                     File: rep_lisp-programmer2.lisp
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




(define-frame PROGRAMMER
  (isa (value (person)))
  )

;;;
;;; NOTE that the programming step is the planning step to write part of the
;;; code, not a computer run-time instruction. But note that in the
;;; Cops & Robbers version the plan steps were equivalent to the
;;; actions performed in the world. There is a difference that should
;;; be resolved with the C&R version, no?
;;; 
(define-frame PROGRAMMING-STEP
  (isa (value (abstract-object)))
  )

(define-frame PROGRAM
  (isa (value (abstract-object)))
  (program-structure (value ((computer-command))))	; A list of programming steps.
  )

(define-frame LISP-PROGRAM
  (isa (value (program)))
  )

;;;
;;; |||||Now is the program some state that enables the computer to
;;; performs the action that produces the output, or is the program the
;;; object?  Or if it is a MOP like representation, then it is an
;;; action.  However, must we differentiate in this representation
;;; between a function and a function call?
;;; 
(define-frame PROGRAM-EXECUTION
  (isa (value (mop)))
  (actor (value (computer)))
  (co-actor (value (program)))
  (object (value (state)))
  (main-result (value (state)))
  (input (value =object))
  (output (value =main-result))
  )

(define-frame PROGRAMMING-PLAN
  (isa (value (plan)))
  )

(define-frame MAPPING
  (isa (value (relation)))
  )


(define-frame PROGRAM-INPUT-LIST
  (isa (value (entity)))
  (contents (value ((entity))))
  )


(define-frame PROGRAM-OUTPUT-LIST
  (isa (value (entity)))
  (contents (value ((entity))))
  )

;;;
;;; The overall goal of the programmer will be in terms of creating a successful
;;; mapping from input to output. NOTE, however, that the additional task that
;;; subject AK88 had -- checking for, and skipping, letters in the input list --
;;; has been removed for simplicity's sake.
;;; 
(define-frame EACH-ONE-GREATER
  (isa (value (mapping)))
  (domain (value (program-output-list)))
  (co-domain (value (program-input-list)))
  ;; Needs semantics slot.
  )


;;;
;;; There is a substantial hack in this representation. The recursive
;;; programming plan is overly specific since it has the exact goal necessary
;;; for the particular task performed by AK88 already in the programming
;;; behavior slot. Therefore, programming any other LISP task is not possible
;;; with this implementation.
;;; 
(define-frame RECURSIVE-PROGRAMMING-PLAN
  (isa (value (programming-plan)))
  (actor (value (programmer)))
  (input (value (program-input-list)))
  (output (value (program-output-list)))
  (object (value (LISP-function)))
  (program (value =object))
  ;; Would like to be able to say for all x of i |x element of output list
  ;; and all y of i|y element of input list
  ;; (x of i ) - (y of i) = 1.
  (program-behavior (value (each-one-greater	; should be general mapping to remove HACK.
			     (domain (value =output))
			     (co-domain (value =input)))))
  ;; The preconditions are that the actor knows what the program should do.
  (preconditions (value ((knowledge-state
			  (domain (value =actor))
			  (believed-item
			    (value =program-behavior))))))
  ;; Expects that the program works or expects that the computer will produce the desired behavior if given the program.
  (expected-outcome (value ))			; =output or =program-behavior with a in.0 or what?
  (main-result (value =program-behavior))
;;; 		 =program))
  (side-effect (value (newly-learned-add1nums-defun
			      (actor (value =actor)))))
  ;; Are there instrumental and goal scenes in a plan to program?
  (instrumental-scene (value ))
  (goal-scene (value ))
  (scene1 (value (write-defun)))
  (scene2 (value (write-name)))
  (scene3 (value (add-parameters)))
  (scene4 (value (write-body
		   (main-result
		     (value =side-effect)))))
  (scenes (value (=scene1 =scene2 =scene3 =scene4)))
  (success (value (boolean-value)))             ; For recording an actual plan: did
						; it work? 
  )


(define-frame WRITE-DEFUN
  (isa (value (programming-step)))
  )


(define-frame WRITE-NAME
  (isa (value (programming-step)))
  )


(define-frame ADD-PARAMETERS
  (isa (value (programming-step)))
  )


(define-frame WRITE-BODY
  (isa (value (programming-step)))
  (main-result (value (LISP-function)))
  )


;;;
;;; |||||Where was this to be used?
;;; 
(define-frame ABSTRACT-ACTION
  (isa (value (entity)))
  )


(define-frame COMPUTER-STATE
  (isa (value (state)))
  )



(define-frame COMPUTER-COMMAND
  (isa (value (mop)))
;;;   (outcome (value (computer-state)))
  )


;;;
;;; Program could be thought of as agents for the programmer that
;;; command another agent (the computer) to perform actions
;;; (computations). Thus the actor of this run-program action is
;;; the program, who commands the computer who does the actual
;;; action.  When we add the CMU-LISP Tutor simulator, then we
;;; have a fourth agent.  So there is the programmer, the
;;; program, the simulation, and the computer. Woops, don't
;;; forget LISP itself; that makes five. Now these abstract
;;; objects are not so abstract, and the computer is a
;;; volitional-agent instead of a physical-object.
;;;
;;; Another issue is that the CMU Tutor probably interprets the
;;; subject's code and intervenes as he plans out the program.
;;; There is no plan/write program followed by execute (nor
;;; compile) program.
;;; 
(define-frame RUN-PROGRAM
  (isa (value (action)))
  (actor (value (programmer)))
  ;; Where are the scenes (the program steps)? The result?
  ;; This could be a loop frame that continues until halt command or abort.
  (object (value (program)))
  (device (value (computer)))
  )

(define-frame COMPUTER
  (isa (value (physical-object)))
  )


(define-frame IDENTIFIER
  (isa (value (abstract-object integer-value)))
  )

;;;
;;; NOTE that the beginning of the definition is just like the definition of a MOP.
;;; There is one important difference between MOPs and LISP-functions:  The object(s)
;;; that is(are) manipulated is the precondition state(s), instead of being something
;;; separate. One could think of the preconditions as being some kind of test on the
;;; objects that are passed to the function, but this analogy still does not hold well.
;;; How will conditionals be represented in frames? How were "tracks" represented in
;;; SAM, such that a particular script would have various paths that might be taken
;;; through the structure. Thus depending on the values of the various roles in the
;;; structure one could have a fast-food instantiation or a fancy-dining experience.
;;; This is much like a cond statement structure. However, the difficulty is that many
;;; fillers are not determined until run-time (including, most importantly, the
;;; main-result slot).
;;; 
(define-frame LISP-FUNCTION
  (isa (value (computer-command LISP-program)))
  (actor (value (programmer)))			; This seems to be an odd slot.
  (preconditions (value ((state))))
  ;; Note that, depending on branching, this may be
  ;; determined only at run-time, not statically like MOPs.
  (goal-scene					; But usually not known until run-time.
    (value (LISP-function
	     (main-result
	       (value =main-result)))))
  (scenes (value (=goal-scene)))		; And other scenes are of course possible.
  (main-result (value (state)))
  (side-effect         (value (state)))

  (semantics (value (literal)))			; How the function will behave.
  (parameter-list (value =preconditions))
  (program-steps (value =scenes))
  (return-value (value =main-result))
  )


;;;
;;; Representation of 1+ function.
;;; 
(define-frame INCREMENT-OP
  (isa (value (LISP-function)))
  (parameter-list (value ((integer-value))))
  (semantics (value (literal
		      '(+ 1 (trans =parameter)))))
  (main-result (value ))
  )


;;;
;;; Representation of null predicate.
;;; 
(define-frame NULL-OP
  (isa (value (LISP-function)))
  (parameter-list (value ((entity))))
  (semantics (value (literal
		      '(if
			 (eq *nil* =parameter-list)
			 true.0
			 *nil*))))
  (main-result (value ))
  )


;;;
;;; Representation of car function.
;;; 
(define-frame FIRST-OP
  (isa (value (LISP-function)))
  (parameter-list (value ((entity))))
  (semantics (value (literal
		      '(if (eq *nil* =parameter-list)
			   *nil*
			   (f.get =parameter-list
				  (car members))))))	; Members no longer relevant.
  (main-result (value ))
  )


;;;
;;; Representation of cadr function.
;;; 
(define-frame SECOND-OP
  (isa (value (LISP-function)))
  (parameter-list (value ((entity))))
  (semantics (value (first-op
		      (parameter-list
			(value
			  (rest-op
			    (parameter-list
			      (value =parameter-list))))))))
  (main-result (value ))
  )


;;;
;;; Representation of cdr function.
;;;
(define-frame REST-OP
  (isa (value (LISP-function)))
  (parameter-list (value ((entity))))
  (semantics (value (literal
		      '(if (eq *nil* =parameter-list)
			   *nil*
			   (f.get =parameter-list
				  (cdr members))))))	; Members no longer relevant.
  (main-result (value ))
  )


;;;
;;; Representation of cons function.
;;;
(define-frame CONSTRUCTOR-OP
  (isa (value (LISP-function)))
  (new-item (value (entity)))
  (old-list (value ((entity))))
  (parameter-list (value (=new-item =old-list)))
  (semantics (value (literal
		      '(f.put!
			 (cons =new-item
			       (f.get =old-list members))	; Members no longer relevant.
			 =old-list
			 members))))		; Members no longer relevant.
  (main-result (value ))
  )



;;;
;;; Representation of list function.
;;;
(define-frame LIST-OP
  (isa (value (LISP-function)))
  (first-item (value (entity)))
  (second-item (value (entity)))
  (parameter-list (value (=first-item =second-item)))
  (main-result (value =parameter-list))
  )



(define-frame CONDITIONAL-CLAUSE
  (isa (value (LISP-function)))
  (test-clause (value (LISP-function)))
  (action-clauses (value ((LISP-function))))
  (semantics (value (literal
		      '(if =test-clause
;;; 			  (or @=action-clauses)	; |||||This needs some type of
						; splice operation eventually.
			   (eval		; For now avoid the issue with
			     (cons		; eval.
			       'or
			       =action-clauses))
			   ))))
  )


(define-frame COND-FUNCTION
  (isa (value (LISP-function)))
  (scenes (value ((conditional-clause))))		; List of conditional clauses.
  (semantics (value (literal
		      '(eval
			 (cons
			   or
			   =clauses)))))
  (clauses (values =scenes))
  )


(define-frame DEFUN-CALL
  (isa (value (LISP-function)))
  (name (value (literal)))
  (lambda-list (value ((entity))))
  (body (value ((LISP-function))))
  (parameter-list (value =name =lambda-list =body))
  )


  
(define-frame RECURSIVE-ADD1NUMS-DEFUN
  (isa (value (defun-call)))
  (actor (value (programmer)))
  (function-name (value (literal
			  "Add1Nums")))
  (lambda-list (value ((entity))))		; List of parameters.
  (body (value
	  ((cond-function
	     (scenes
	       (value
		 ((conditional-clause
		    (test-clause		; Test for terminating condition.
		      (value
			(null-op
			  (parameter-list
			    (value =lambda-list)))))
		    (action-clauses
		      (value (nil.0))))
		  (conditional-clause
		    (test-clause
		      (value true.0))
		    (action-clauses
		      (value ((constructor-op
				(new-item
				  (value
				    (increment-op
				      (parameter-list
					(value (first-op
						 (parameter-list
						   (value =lambda-list)))))))
				  )
				(old-list
				  (value	; The recursive call.
				    (recursive-add1nums-defun
					 (lambda-list
					   (value
					     (rest-op
					       (parameter-list
						 (value =lambda-list))))))
				    )))
			     )))))))))))
  )


;;;
;;; The following defintion represents the students attempt to do recursion.
;;; However he is using a sort of iterative approach: Make an output list
;;; with the incremented value of the first item of the input as the first
;;; element, the incremented value of the second item in the input as the
;;; second element, and so on.
;;; 
(define-frame NEWLY-LEARNED-ADD1NUMS-DEFUN
  (isa (value (recursive-add1nums-defun)))	; recursive-add1nums-defun wanna-be.
  (actor (value (programmer)))
  (function-name (value (literal
			  "Add1Nums")))
  (lambda-list (value ((entity))))
  (body (value
	  ((cond-function
	     (scenes
	       (value
		 ((conditional-clause
		    (test-clause
		      (value (null-op
			       (parameter-list
				 (value =lambda-list)))))
		    (action-clauses
		      (value (nil.0))))
		  (conditional-clause
		    (test-clause
		      (value true.0))
		    (action-clauses
		      (value ((constructor-op
				(first-item
				  (value
				    (increment-op
				      (parameter-list
					(value (first-op
						 (parameter-list
						   (value =lambda-list)))))))
				  )
				(old-list
				  (value
				    (list-op
				      (parameter-list
					(value
					  (increment-op
					    (parameter-list
					      (value (second-op
						       (parameter-list
							 (value =lambda-list)))))))))
				    )))
			      )))))))))))
  )
;;; -*- Mode: LISP; Syntax: Common-lisp; Package: Framesystem; Base: 10 -*-

(in-package :frames)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	            A Frame System for conceptual construction
;;;;
;;;;	   Copyright (C) 1994   Michael T. Cox   (cox@cc.gatech.edu)
;;;;
;;;;				   20 May 1994
;;;;
;;;;				 File: frame.lisp
;;;;
;;;;
;;;;      Modelled after the frame system developed by Erik Jones and
;;;;      Ashwin Ram at Yale University.
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

;;;; 
;;;; History
;;;; 
;;;; Changed gentemp call to pass string. See var *traverse-marker*. 
;;;; [mcox 23oct06]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;
;;;; FRAME.LISP is the main definition file for the frame system used in
;;;; Meta-AQUA. Above is the interface for the package. The conceptual
;;;; hierarchy of isa relations is managed by the functions found in the file
;;;; isa.lisp, also in the frames package.
;;;;
;;;; NOTE that piped comments refer to glitches which must eventually be
;;;; addressed, or comments to the frame system developer (not to the
;;;; applications programmer).
;;;;
;;;;
;;;
;;; Frames are referenced in Meta-AQUA's frame system in two ways. A frame
;;; variable is a unique LISP symbol, such as ptrans.111, whose symbol value is
;;; either a literal or a frame form. Frame forms are always lists with a
;;; particular structure. A frame is defined to be either a literal frame whose
;;; symbol-value is arbitrary, or it is a list consisting of a frame-name
;;; followed by an arbitrary number of slots. Literals are different from other
;;; frame variables since routines which traverse frame structures should not
;;; inspect or try to traverse a literal. Literals have no subframe value.
;;; Slots are relations (usually thought of as an attribute-value pair) having
;;; some name (role of the slot) and an arbitrary number of facets. A
;;; distinguished facet is the value facet. It represents the filler of the
;;; slot, and may itself be a frame. However slots themselves are treated in
;;; the frame system as a first-class citizen, and thus are frames themselves.
;;; So the relation facet has as its filler a frame representing the attribute-
;;; value relation. Consider the following.
;;;
;;; ptrans.111 ->
;;;     (ptrans
;;;       (actor (value    person.21)
;;; 	         (relation actor.12)))
;;; 
;;; actor.12 ->
;;;     (actor
;;;       (domain    (value ptrans.111))
;;;       (co-domain (value person.21)))
;;;
;;; A number of advantages accrue from this representation. The actor slot of the
;;; above ptrans frame can be refered to directly by specifying the object actor.12.
;;; For example one can assert that actor.12 has a particular truth value (say false),
;;; or one can question who was the actor of ptrans.111 by tagging actor.12 with a
;;; question marker.
;;;
;;; One should conceive of the domain slot of such a relation frame in mathematical terms.
;;; The relation is thus a function from domain to co-domain (like range). Or one could
;;; simply think of the value of the domain as the frame for which the co-domain is the
;;; filler of the actor role. It is not always true however that if one just strips off
;;; the ".XXX" of a relation frame that one then has the slot name. Instead one should use
;;; the function f.relation-slot-name to obtain the name. Such suggests the following
;;; equivalences:
;;;
;;;     (f.get frame role *value-facet*)
;;;    =
;;;     (f.get
;;;        (f.get frame role *relation-facet*)
;;;        *co-domain-slot*)
;;;    =
;;;     (f.get
;;;        (f.get
;;;           (f.get frame role *relation-facet*)
;;;           *domain-slot*)
;;; 	   (f.relation-slot-name
;;; 	     (f.get frame role *relation-facet*))
;;; 	   *value-facet*)
;;;
;;; One may manually create such an arangement by the use of the macros define-frame
;;; and define-relation. In the define-relation call one would explicitly specify a
;;; slot with the role slot. For example the following definitions would suffice.
;;;
;;; (define-frame PTRANS
;;;   (isa   (value    (mop)))
;;;   (actor (value    (person))
;;; 	     (relation (actor))))
;;; 
;;; (define-relation ACTOR
;;;   (isa       (value (relation)))
;;;   (domain    (value (mop)))
;;;   (co-domain (value (person)))
;;;   (slot      (value (actor))))
;;; 
;;; Alternatively one may forego the relation definition. Then after instantiating a
;;; particular ptrans, one should call f.make-relation to add the relation frames and
;;; the slot property.
;;;
;;; Ie.,
;;; (define-frame PTRANS
;;;   (isa   (value (mop)))
;;;   (actor (value (person))))
;;;
;;; (f.make-relation (setf new-ptrans (f.instantiate-frame ptrans)))
;;;
;;; 
;;; All frames are linked into the conceptual (ISA) hierarchy automatically
;;; when they are defined. To accomplish this one need only include an isa slot
;;; in the definition somewhere having some value facet. The filler must be a
;;; list of at least one member. NOTE that since there may be more than one
;;; superframe in the list, this frame system performs multiple inheritance.
;;; At the root of the hierarchy is the ENTITY type (use the *entity* constant
;;; when refering to the type in code). All frames are entities, either directly
;;; or through transitivity.
;;; 
;;; Frames include variable bindings. Variable bindings are those fillers in a frame
;;; which are defined to be the same frame. For example the concept of walking may
;;; be defined as a ptrans where the actor and the object of the ptrans are one in the
;;; same. Thus to walk an agent ptrans himself somewhere. To capture this one must use
;;; a variable binding as follows.
;;;
;;; (define-frame WALK
;;;   (isa (value (ptrans)))
;;;   (actor (value (person)))
;;;   (object (value =actor))))
;;;
;;; Thus when a particular walk frame is instantiated with a call to f.instantiate-frame
;;; the actor and object of the walk will be the same entity. NOTE that when one uses a
;;; binding marker in this fashion it refers to the top-level slots in the defined frame.
;;; Thus the location refered to in the following nonsense definition refers to the left-
;;; most occurence of actor (ie., the person frame, not the dog frame).
;;; 
;;; (define-frame EXAMPLE1
;;;   (isa (value (entity)))
;;;   (actor (value (person)))
;;;   (object (value (mop
;;;                    (actor (value (dog))))))
;;;   (another-slot (value =actor)))
;;;
;;; If one wished to refer to the dog frame instead, then one marks the dog with a binding
;;; marker not corresponding to a left-most slot name, and then uses it as follows.
;;;
;;; (define-frame EXAMPLE2
;;;   (isa (value (entity)))
;;;   (actor (value (person)))
;;;   (object (value (mop
;;;                    (actor (value (dog =x))))))
;;;   (another-slot (value =x))) 
;;; 
;;; Note also that one may use the predefined variable binding =self to refer to the
;;; frame one is defining. Thus the filler of another-slot will be the frame instantiated
;;; for example3 below.
;;; 
;;; (define-frame EXAMPLE3
;;;   (isa (value (entity)))
;;;   (actor (value (person)))
;;;   (object (value (mop
;;;                    (actor (value (dog))))))
;;;   (another-slot (value =self)))
;;;
;;; (setf new-example (f.instantiate-frame example3)
;;; 
;;; new-example -> example3.303
;;;
;;; (*FRAME* new-example) 
;;;   -> (example3
;;;        (actor (value person.304))
;;;        (object (value dog.305))
;;;        (another-slot (value example.303))
;;;

;;; 
;;; There are at least two different flavors of functions in this file.
;;; Some are primitive low-level functions which process or access
;;; frames directly. Other functions traverse or otherwise process the 
;;; net of frames. An example of the latter type is f.unify while f.put!
;;; is an example of the former. Another distinction in these routines
;;; are those more primitive functions that input frame variables, such
;;; as mtrans.21, and those that input frame formes, such as 
;;; (mtrans (actor john.12)). |||||| Shall we split these into layers such 
;;; as Jones did with his? For now I will wait till it is decided how much
;;; effort will be expent on porting code to commercial systems.
;;; 

;;;
;;; When a call of define-frame is performed, all slot specifications (except
;;; isa and slot slots) along with the frame name are placed in a list and
;;; bound to the symbol value of the LISP symbol <frame name>. Thus the
;;; definition of PTRANS may be found by evaluating the symbol 'ptrans (one
;;; should actually use the access function *FRAME* to be implementation
;;; independent). The purpose is to provide an efficient access for the values of
;;; the definitions without having to use hash tables as Tom H. did. However,
;;; there is a serious problem with placing the frame definitions on the
;;; symbols being defined. By placing the definitions on particular symbols, we
;;; are assuming that frame names are special, and we must stress such to
;;; the user of the frame system. This is a severe restriction on the use of
;;; local variables, since the programmer must be aware of the global variable
;;; state. For example the frame ACTOR is defined in the representational files
;;; of Meta-AQUA. Now any time that a function declares a local variable of the
;;; name actor, the definition of the frame ACTOR becomes non-accessable. In
;;; order to compensate for this difficulty and to provide a warning to the
;;; programmer, the system should implement a transparent trap such as the
;;; function f.let below.
;;; 

;;;
;;; Need to rewrite the representations and the code which depends on most all 
;;; fillers being value facets. There is currently no definitional inferences
;;; because of this, as well as other features that should be reflected by the 
;;; distinction.
;;;

;;;
;;; Eventually need to create a shell around the let and let* special forms
;;; as well as other means of declaring local variables. An example is the
;;; following function definition. I am not sure that this is sufficient
;;; since there are questions concerning scope and lexical versus dynamic
;;; bindings. But the function f.let is in the right spirit.
;;; 
(defun f.let (varlist &rest body)
  (dolist (each-var varlist)
    (format-if (boundp
		 (if (listp each-var)
		     (first each-var)
		     each-var))
	       *frame-stream*
	       "Warning: Variable ~s used in let has binding ~s."
	       (if (listp each-var)
		   (first each-var)
		   each-var)
	       (symbol-value (if (listp each-var)
				 (first each-var)
				 each-var))))
  (eval `(let ,varlist ,@body))
  )






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    FRAME SYSTEM PUBLIC CONSTANTS, VARIABLES, AND PARAMETERS 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *frame-stream* t			; The program variable should be bound to the output stream
  "Output stream for frame system.")		; the application uses by calling f.frame-output-var.
						; By default (t) the output goes to *standard-output*.

(defparameter *status-slot*        'status)	; Entity's status slot specifies its epistemological source.
(defparameter *derivation-slot*    'derivation)	; Derivation slot of an entity specifies how it was obtained.
(defparameter *truth-slot*         'truth)	; Truth slot of an entity specifies its propositional belief.

(defparameter *domain-slot*        'domain)	; The frame a relation maps from. (See main comments above.)
(defparameter *co-domain-slot*     'co-domain)	; The frame a relation maps to.

(defparameter *value-facet*        'value)	; The filler value of a slot.
(defparameter *relation-facet*     'relation)	; The frame representing the slot filler relation itself.

(defparameter *dummy*              'dummy)	; Publically declared but internally used.
(defparameter *self*               '=self)	; Self referential variable binding in a frame declaration.

(defvar *extant-frames* nil)			; List of those frames created by calls to f.define-frame. 

;;; When frames are defined they have a isa slot.  This is removed from the
;;; form which gets bound to the frame symbol (eg., ptrans) and placed on the
;;; symbol's isa property. The isa relation is really a "system slot". NOTE
;;; that this parameter is really found in the file isa.lisp
;;; 
;;; (defparameter *isa-property*       'isa)

;;;
;;; Defined symbol properties.
;;; 
(defparameter *instance-prop* 'instance)
(defparameter *attribute-value-prop* 'attribute-value)
(defparameter *slot-prop*     'slot)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;           FRAME DEFINITION ERROR CHECKING AND BOOKKEEPING
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; In a non-trivial conceptual memory it is easy to declare frames that
;;; reference other frames which are then never defined themselves. Many
;;; errors and bugs can be traced back with great difficulty to some side-
;;; effect of undefined frames. The following routines are designed to aid
;;; the system developer with maintaining consistency in the conceptual
;;; network.
;;;
;;; To use the following functions call init-frame-bookkeeping before
;;; evaluating the define-frame forms setting up the conceptual memory of the
;;; system. After evaluating these forms one can call print-bad-frames to get
;;; summary statistics. Function print-only-undefs lists those frames
;;; referenced somewhere but not currently defined. Print-only-definers
;;; performs the inverse function. It lists those definitions that refer to
;;; some undefined frame.
;;;
;;; Additional error checking may be obtained during system creation by calling
;;; f.debugon (has no arguments). This will provide verbose (and somewhat
;;; cryptic) info during calls of f.make-relation, f.put, f.instantiate-frame,
;;; and define-frame.  To turn off reporting simply call f.debugoff.
;;; 

(defun init-frame-bookkeeping ()
  "Initialize frame definition error checking globals."
  (setf *undefined-frames* nil)
  (setf *num-nulls* 0)
  (setf *num-non-lists* 0)
  )


(defun print-bad-frames ()
  "Report summary bookkeeping statistics on frame definitions."
  (format *frame-stream*
	  "~%~%Undefined frames referenced:~%         ~s~%"
	  (remove-duplicates *undefined-frames*))
  (format *frame-stream*
	  "~%Number of null slot-filler definitions:  ~s" *num-nulls*)
  (format *frame-stream*
	  "~%Number of non-list values for symbols: ~s~%" *num-non-lists*)
  )



(defun print-only-undefs ()
  "List frames referenced in some definition but not defined themselves."
  (let ((undefs (remove-duplicates 
		  (mapcar #'(lambda (each-pair)
			      (first each-pair))
			  *undefined-frames*))))
    (format *frame-stream*
	    "~%~%These ~s frames are undefined but referenced:~%"
	    (length undefs))
    undefs))


(defun print-only-definers ()
  "List frames that reference an undefined frame definition."
  (let ((definers (remove-duplicates 
		    (mapcar #'(lambda (each-pair)
				(second each-pair))
			    *undefined-frames*))))
    (format *frame-stream*
	    "~%~%These ~s frame definitions reference undefined frames:~%"
	    (length definers))
    definers
    ))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;           FRAME INITIALIZATION FUNCTIONS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Function f.frame-output-var creates an association between
;;; the user's symbol passed in parameter user-stream and
;;; the FRAME package's symbol *frame-stream*. All output
;;; from functions in the frame system is directed through
;;; *frame-stream*. The result of this function, however, is
;;; that a user application can subsequently change the
;;; direction of all frame output by simply changing the
;;; stream associated with the symbol user-stream.
;;;
;;; NOTE that user-stream must be a symbol, NOT a stream itself.
;;; Therefore the parameter is usually quoted when the function is called.
;;;
;;; A Symbolics example:
;;;
;;; (defvar *window1*
;;;   (tv:make-window 'dw:dynamic-lisp-listener :edges-from :mouse ))
;;; (defvar *window2*
;;;   (tv:make-window 'dw:dynamic-lisp-listener :edges-from :mouse ))
;;; (defvar *my-output* *window1*)
;;; (f.frame-output-var '*my-output*) ;All frame output goes to 1st window
;;; (setf *my-output* *window2*)      ;All frame output goes to 2nd window
;;; (setf *my-output* *window1*)      ;All frame output goes to 1st window again
;;; 
(defun f.frame-output-var (user-stream)
  "Cause all output to frame stream to go to user stream."
  (setf *frame-stream*
	;;Commented out below and added t [cox 6mar97]
;	(make-synonym-stream
;	  user-stream)
	t)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;           PRIMITIVE FRAME HANDLING
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; The following functions which are written X->Y takes an X form, returning a
;;; Y form. They work on frame forms, not variables such as PTRANS.23. Use
;;; f.get etc to do this (or call the function *FRAME* to pass the correct
;;; form.
;;; 
;;;  Frames are of the form:
;;;  
;;;  (frame 
;;;   (slot1 (value ..)
;;;    (constraint ..)
;;;    (relation ..)
;;;    (slot ..)
;;;    )
;;;   (slot2 (value ..)
;;;    (constraint ..)
;;;    (relation ..)
;;;    (slot ..)
;;;    )
;;;   .
;;;   .
;;;   .)
;;;

;;; 
;;; NOTE that on 8feb95 cox proclaimed the entire class of these functions to
;;; be open-coded (inline) at every location in which they appear.
;;;
(proclaim '(inline slot->role
		   slot->facets
		   frame->slots
		   facet->facet-name
		   frame->frame-name
		   frame->roles
		   frame->facet-names
		   frame->slot
		   frame->filler
		   slot->filler
		   facet->filler
		   var->role 
		   )
	  )

(defun slot->role (slot)
  "Given a slot, output the slot name (role) of the slot."
  (first slot))

(defun slot->facets (slot)
  "Given a slot, output the list of facets for the slot."
  (rest slot))

(defun frame->slots (frame)
  "Given a frame form, output a list of slots for the frame."
  (rest frame))


(defun facet->facet-name (facet)
  "Given a facet, output the name of the facet."
  (if (not (null facet))
      (first facet)))


(defun frame->frame-name (frame)
  "Given a frame form, output the name of the frame."
  (first frame))


(defun frame->roles (frame)
  "Given a frame form, output a list of all slot names (roles)."
  (mapcar #'(lambda (each-slot)
	      (slot->role each-slot))
	  (frame->slots frame)))


(defun frame->facet-names (frame role)
  "Given a frame form and a slot name,
   output a list of facet names for the slot."
  (mapcar #'(lambda (each-facet)
	      (facet->facet-name each-facet))
	  (slot->facets
	    (frame->slot frame role)))
  )


(defun frame->slot (frame role)
  "Given a frame form and a slot name, output the slot."
  (if (listp frame)
      (some #'(lambda (slot) 
		(if (equal (slot->role slot) role) 
		    slot))
	    (frame->slots frame))))


(defun frame->filler (frame role &optional (facet *value-facet*))
  "Given a frame form, a slot name and an optional facet name,
   output the corresponding filler."
  (slot->filler (frame->slot frame role) facet))


;;; If facet not provided, assume value facet.
;;; 
(defun slot->filler (slot &optional (facet-name *value-facet*))
  "Given a slot and an optional facet name,
   output the corresponding filler."
  (if (listp slot)
      (some #'(lambda (facet) 
		(if (equal (facet->facet-name facet) facet-name)
		    (facet->filler facet)))
	    (slot->facets slot))))


(defun facet->filler (facet)
  "Given a facet, output the filler of the facet."
  (second facet))


;;; Takes a frame variable of the form =X and returns x.
;;; Ie. strips the leading "=" from parameter variable.
;;; 
(defun var->role (variable)
  "Given a binding variable, return the correspondng slot name."
  (intern
    (coerce
      (cdr (coerce (string variable) 'list))
      'string)))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;           FRAME BASICS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 
;;; The following functions take as input either a frame or a frame
;;; variable, and perform some basic function. The function provides 
;;; some information on a frame as a whole without making any 
;;; change or side-effect. Sections  following this one compare
;;; frames, operate on them, or access a parts of a frames.
;;; 


;;; 
;;; The function *FRAME* returns the frame
;;; definition of a frame variable, otherwise
;;; it simply returns the input form. Thus
;;; (*FRAME* 'PTRANS) -> (ptrans (actor (constraint ...)
;;; See comments on f.gen and f.gen0.
;;; 
(defun *FRAME* (frame)
  (if (or (frame-var-p frame)
	  (and (atom frame)
	       (boundp frame)))
      (symbol-value frame)
      frame)
  )


;;; 
;;; Function frame-type works on either frame variables (tokens)
;;; or frame forms. The function returns the type class of the frame.
;;; This is performed by returning the car of the frame form. If the 
;;; parameter passed to the function is already a frame form then return
;;; the car, else call *FRAME* to evaluate the instantiated frame variable.
;;; Eg. (frame-type '(PTRANS (actor...))-> PTRANS.
;;;
;;; Literals are defined to have no type as in the sense here, although
;;; get-abstraction would return 'literal for them.
;;; 
(defun frame-type (frame)
  (cond ((or (null frame)
	     (literal-p frame))
	 nil)
	((frame-var-p frame)
	 (frame-type (*FRAME* frame)))
	; Should this be an error?
	((atom frame)
	 frame)
	((and (listp frame)
	      (atom (car frame)))
	 (frame->frame-name frame))
	(t
	 (format
	   *frame-stream*
	   "~%ERROR in function frame-type - frame = ~s."
	   frame)
	 (break)
	 (return-from frame-type (read))))
  )


;;; 
;;; Function frame-body returns a list of slots for parameter frame.
;;; The function works on either frame variables or frame forms.
;;; 
;;; Literals are defined to have no body as in the sense here, although
;;; *FRAME* would return the literal value for them.
;;; 
(defun frame-body (frame)
  (cond ((or (null frame)
	     (literal-p frame))
	 nil)
	((frame-var-p frame)
	 (frame-body (*FRAME* frame)))
	((and (listp frame)
	      (atom (car frame)))
	 (frame->slots frame))
	(t
	 (format
	   *frame-stream*
	   "~%ERROR in function frame-body.")
	 (format
	   *frame-stream*
	   "~%Parameter is ~s.~%"
	   frame)
	 (break)))
  )


;;;
;;; Function frame-def returns the conceptual definition for a given frame. The
;;; function *FRAME* returns the definition of the header returned by frame-type.
;;; [cox 24apr94]
;;; 
(defun frame-def (frame)
  (*FRAME* (frame-type frame))
  )



;;;
;;; Function f.relation-slot-name returns the slot name (role) of the
;;; relation frame passed to it. The slot name is obtained from the
;;; *slot-prop* property set by f.make-relation (and define-relation
;;; or define-frame macros when they include a slot slot. See main frame
;;; system comments for the rationale.
;;; 
(defun f.relation-slot-name (relation)
  (cond ((not (isa-p
		'relation
		(list relation)))
	 (format
	   *frame-stream*
	   "~%Error in f.relation-slot-name.~% relation is ~s"
	   relation)
	 (break))
	(t
	 (get relation *slot-prop*)))
  )


;;; 
;;; Function f.role-list takes as input either a frame variable (eg., PTRANS.1),
;;; a frame name (eg., PTRANS), or a frame form (eg., (PTRANS (ACTOR (VALUE...))))
;;; and returns a list of the role names for each slot of the frame.
;;; 
(defun f.role-list (frame)
  (if (not (literal-p frame))
      (frame->roles (*FRAME* frame)))
  )


;;; 
;;; Function f.slot-list takes as input either a frame variable (eg., PTRANS.1),
;;; a frame name (eg., PTRANS), or a frame form (eg., (PTRANS (ACTOR (VALUE...))))
;;; and returns a list of the slots (role-filler pairs) for the frame.
;;; 
(defun f.slot-list (frame)
  (if (not (literal-p frame))
      (frame->slots (*FRAME* frame)))
  )


;;;
;;; Function f.facet-list returns a list of facets in the slot defined
;;; by the tuple (frame,role).
;;; 
(defun f.facet-list (frame role)
  (if (not (literal-p frame))
      (frame->facet-names (*FRAME* frame) role))
  )


;;;
;;; Function f.where-bound returns a list of frames where the
;;; parameter frame is bound. Eg., if person.12 is the actor value
;;; of ptrans.111, then ptrans.111 will be a member of the list
;;; (f.where-bound 'person.12).
;;; 
(defun f.where-bound (frame)
  (if (frame-var-p frame)
      (get frame *Back-Ptrs*)
      (format
	*frame-stream*
	"Error: Parameter ~s in f.where-bound not a frame variable."
	frame))
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;           FRAME DEFINITION AND INSTANTIATION
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; The next 3 macros were created to read the rep_*.lisp files from
; AQUA without having to change the calling conventions. It was
; only necessary to change all brackets to parens and remove all
; occurances of "list." in list slots.
;
(defmacro define-frame (&rest args)
  `(f.define-frame (quote ,args)))


(defmacro define-relation (&rest args)
  `(f.define-frame (quote ,args)))


(defmacro define-attribute-value (&rest args)
  `(f.instantiate-attribute (quote ,args))
  )


;;; 
;;; |||||| What is the semantics of define-instance in AQUA?
;;; Should it not be used for instantiating memories that
;;; exist before the program is run? If so it could be used
;;; to create frames like past cases.
;;; 
; Quoted out the occurances in rep_smuggle3.lisp temporarily.
(defmacro define-instance (&rest args)
  `(f.instantiate-instance (quote ,args *predefined-instance*))
  )


(defun f.instantiate-instance (pattern &optional status-filler)
  (let ((new-instance (f.instantiate-frame
			pattern
			status-filler)))
    (f.put *predefined-instance*
	   new-instance
	   *status-slot*)
    (putprop new-instance
	     *instance-prop*
	     t)
    (setf *predefined-frames*
	  (cons new-instance
		*predefined-frames* ))
    new-instance))



;;;
;;; Example call:
;;; 
;;; (f.instantiate-frame 
;;;   '(ptrans
;;;      (actor (value (person))
;;;             (constraint (animate-object)))
;;; 	 (to (value =object))
;;; 	 (object (value (dog))) 
;;;      (from (value =object)))))
;;;
;;; Function f.instantiate-frame is used to generate tokens in an
;;; application. The function expects as input a form of some type.
;;; Thus one can pass a quoted list as above, or pass it an unquoted
;;; frame type. Since frame definitions are on the symbol of the frame
;;; type, LISP will expand the symbol to a pattern which will be
;;; instantiated into a token. For example in the call
;;;                   (f.instantiate ptrans)
;;; the definition pattern of ptrans will be passed to the function.
;;;
;;;
;;; Status-filler is the designated status type for the status slot.
;;; 
(defun f.instantiate-frame (pattern &optional status-filler)
  (if (and (listp pattern)
	   (equal 1 (length pattern))
	   (or (instance-p  (first pattern))
	       (attribute-value-p (first pattern))))
      ;; If we try to instantiate something that is already an instance,
        ;; or is an attribute value then there is something wrong.
      (break))
  (setf *GLOBAL-Bindings* nil)
  (let ((new-frame (instantiate-frame pattern status-filler)))
    (setf *GLOBAL-Bindings* nil)
    new-frame)
  )



;;;
;;; Function f.instantiate-literal creates a new literal frame variable and
;;; binds its value to the literal value passed as an argument. It returns the
;;; frame variable eg., literal.101.
;;; 
;;;
;;; Status-filler is the designated status type for the status slot.
;;;
(defun f.instantiate-literal (literal-value &optional status-filler)
  (let ((new-literal
	  ;; Do not want to affect the *GLOBAL-Bindings* list,
	  ;; so we do not use f.instantiate-frame.
	  (instantiate-frame (*FRAME* *literal*)
			     status-filler
			     t)))
    (f.set-literal new-literal literal-value)
    new-literal)
  )



;;;
;;; Function f.make-new-instance is similar to f.instantiate-frame. It
;;; instantiates a new token from the parameter in frame. However if frame is a
;;; frame form, then it will not only instantiate the form, but it will unify
;;; each slot filler of the token with an instantiated definition of each filler.
;;; Consider the following call.
;;;
;;; (setf bust-var
;;;     (f.make-new-instance
;;;       `(BUST-ACT
;;; 	     (actor      (,*value-facet* ,sarge))
;;; 	     (object     (,*value-facet* ,bad-guy))
;;; 	     (goal-scene (,*value-facet*
;;; 		           (arrest
;;; 			     (actor  (,*value-facet* =actor))
;;; 			     (object (,*value-facet* =object))
;;; 			     (charge (,*value-facet* smuggling-crime.0))))))
;;;       "Sarge busted the bad-guy for smuggling."))
;;; 
;;; The above call instantiates a bust-act token including all slots defined by the
;;; bust-act definition (eg., instrumental-scene). Additionally it unifies the arrest
;;; event in the goal-scene slot with a complete instantiation of arrest, including
;;; the reading of the suspect's rights and taking him to jail. If the above call was
;;; made with f.instantiate-frame instead, then only those slots in the pattern passed
;;; to the function (actor, object, and goal-scene for the bust-act, and only actor,
;;; object, and charge for the arrest) would have been created.
;;; 
;;; 
;;; |||||| Variable bindings are necessary for any non-trivial story.
;;; In fact the program needs to eventually have the capability to do cross-concept
;;; or cross-sentence bindings. One supposes that the program should indeed do
;;; this, but this referential inference is not central to the theory. So hack
;;; it and comment the hack! We added limited variable binding capability. See
;;; comment on old version found in file old-fragments.lisp. ||||||
;;;
;;; |||||| Does this work OK with a literal filler in frame?
;;; |||||| Should the calls of symbol-value be replaced with *FRAME* ?
;;;
;;; NOTE: Parameter notify is set to nil by instantiate-next to suppress
;;; f.unify complaining during creation of anomalous input concepts.
;;; 
(defun f.make-new-instance (frame &optional (notify t) status-filler)
  (let ((new-instance nil))
    (cond ((atom frame)
	   (setf new-instance
		 (f.instantiate-frame (symbol-value frame) status-filler)))
	  ((and (listp frame)
		(not (frame-list-p frame)))
	   (setf new-instance
		 (f.instantiate-frame frame status-filler))
	   (mapcar
 	     #'(lambda (each-slot)
		 (let ((each-role (slot->role each-slot)))
		   (mapcar
		     #'(lambda (each-facet)
			 (let ((new-filler (facet->filler each-facet)))
			   (cond ((and (atom new-filler)
				       (not (attribute-value-p new-filler)))
				  (f.unify new-filler
					   (f.instantiate-frame
					     (frame-def new-filler)
					     status-filler)
					   notify)))))
		     (slot->facets each-slot))))
	     (frame->slots (*FRAME* new-instance)))
 	   (setf new-instance
		 (or (f.unify new-instance
			      (f.instantiate-frame
				(symbol-value (frame-type frame))
				status-filler)
			      notify)
		     new-instance)))
	  (t
	   (format
	     *frame-stream*
	     "ERROR: in f.make-new-instance")
	   (break)))
    new-instance
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;           MAIN FRAME MANIPULATION FUNCTIONS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;
;;; Function f.copy-instantiated-frame returns a copy of the parameter
;;; frame with unique fillers. This function is useful when performing
;;; multiple calls of f.unify. Some early unifications may succeed, but
;;; a further one fails. Thus one does not have to try to undo all of the
;;; changes which resulted from the successful calls. Instead the copy
;;; can simply be jetisoned. On the other hand, if all unification attempts
;;; succeed, then the application simply unifies the copy with the original
;;; to complete the changes.
;;; 
(defun f.copy-instantiated-frame (frame &optional filter)
  (let ((new-frame
	  (copy-instantiated-frame frame filter)))
    (setf old-list nil)
    (setf new-list nil)
    new-frame)
  )


;;; 
;;; Function f.get is the main frame access
;;; function. It is intended to take a frame variable
;;; as input (though it will work with a form also),
;;; a slot identifier, and an optional facet identifier.
;;; The default facet is the value facet.
;;; 
;;; Eg., (f.get ptrans.101 actor)
;;;      -> (person (name (value john.0))...)
;;; 
(defun f.get (frame slot &optional facet)
  (frame->filler (*FRAME* frame) 
		 slot 
		 (or facet *value-facet*)))



;;; 
;;; Function f.chase-path returns the value of a slot
;;; at the end of path through subframes. To access
;;; a facet in the path other than value, which is
;;; the default, then call with a list pair (role facet).
;;; 
;;; Eg. (f.chase-path sniff-event
;;;        'object
;;;        '(co-domain constraint)
;;;        'believed-item
;;;         *co-domain*)
;;; 
;;; |||||| NOTE there is a problem with forms having variable
;;; bindings (eg., type definitions). The binding variable
;;; (eg., =domain) will be returned instead of the slot filler
;;; that it points to. Moreover, if the binding is an intermediate
;;; frame along the path that the function is chasing, then the
;;; function will not even work. Probably will return nil.
;;; 
(defun f.chase-path (frame &rest slot-names)
  (do-break f.chase-path)
  (cond ((null slot-names) nil)
	((eq (length slot-names) 1)
	 (if (listp (first slot-names))
	     (f.get frame (first (first slot-names)) (second (first slot-names)))
	     (f.get frame (first slot-names))))
	(t
	 (apply #'f.chase-path `(,(if (listp (first slot-names))
				      (f.get frame 
					     (first (first slot-names))
					     (second (first slot-names)))
				      (f.get frame (first slot-names)))
				 ,@(rest slot-names))))))


;;;
;;; The seemingly most simple functions turn out to be quite complicated.
;;; Function f.inherit will return a filler like f.get, but if not found
;;; in the frame parameter, then it will search up the isa hierarchy for
;;; a default value. Note there are a number of problems that could occur
;;; in a multiple-inheritance hierarchy:
;;;  1. Node X isa both Y and Z, AND they both have defaults.
;;;     One prefers a default that is definitional, or the most specific.
;;;     Function select-most-specialized is responsible for such.
;;;  2. Node X can inherit from both Y and Z, BUT Y isa Z even though Z is
;;;     just two links away in the hierarchy, while Y is three. Predicate
;;;     lowest-not-isa-remainder-p enforces this constraint.
;;; 
;;;            Z
;;;          /   \
;;;        Y       \
;;;        |         \
;;;        A          B
;;;         \        /
;;;           C    /
;;;            \ /
;;;             X
;;; 
;;;  3. Node X isa both Y and Z, AND Z isa Y. We do not currently check
;;;     for this possibility. ||||||
;;;  4. Finally, the isa hierarchy could have cycles in it. This may be
;;;     disasterous for the algorithms. Macro define-frame must eventually
;;;     be modified to check for this conceptual error. ||||||
;;; 
(defun f.inherit (frame slot &optional facet)
  (or (f.get frame slot facet)
      (let ((abstractions (get-abstraction frame)))
	(if (null abstractions)
	    nil
	    (let ((inherit-list
		    (remove-duplicates
		      (inherit frame slot (or facet *value-facet*) 0)
		      :test #'equal)))
	      (if (null inherit-list)
		  nil
		  (if (equal (length inherit-list) 1)
		      (second (first inherit-list))
		      (do* ((lowest (get-lowest inherit-list)
				    (get-lowest i-list))
			    (i-list (remove (first lowest) inherit-list :key #'first)
				    (remove (first lowest) i-list :key #'first)))
			   ((lowest-not-isa-remainder-p lowest i-list)
			    (second lowest)))))))))
  )



;;; 
;;; The function f.put is a non-destructive facet filler operation.
;;; If there already exists a filler for the slot's facet, then no
;;; action is taken unless the filler is the *nil* filler.
;;; 
;;; The (if *Debug-On* ..) forms are guaranteed to return nil
;;; because stream is t.
;;; 
(defun f.put (filler frame slot &optional (facet *value-facet*))
  (if (and (or (not (null filler))
	       (format-if 
		 *Debug-On*
		 *frame-stream*
		 (str-concat
		   "~%Failed f.put: cannot put "
		   "nil filler in ~s.")
		 frame))
	   (or (not (null slot))
	       (format-if 
		 *Debug-On*
		 *frame-stream*
		 (str-concat
		   "~%Failed f.put: cannot put filler "
		   "in a null slot of ~s." frame)))
	   (or (frame-var-p frame)
	       (format-if 
		 *Debug-On*
		 *frame-stream*
		 (str-concat
		   "~%Failed f.put: ~s "
		   "is not a frame var.")
		 frame))
	   (or (let ((current-filler (f.get frame slot facet)))
		 (or (null current-filler)
		     (equal current-filler *nil*)))
	       (format-if 
		 *Debug-On*
		 *frame-stream*
		 (str-concat
		   "~%Failed f.put: ~s already "
		   "has a ~s slot.")
		 frame slot))
	   )
      (f.put! filler frame slot facet))
  )



;;; 
;;; The function f.put! is a destructive put. if there already exists a filler
;;; in the slot's facet then it is lost.
;;; 
;;; This function could be more efficient if it did not use rest and append in
;;; the setf calls.
;;; 
;;; There are other functions which depend on f.put! returning the filler
;;; originally passed. Make sure to keep it that way.
;;; 
;;; |||||| Eventually need to change the function so that it preserves the slot
;;; ordering. This would need to be done when we are changing a filler, or when
;;; there exists an inherited frame that has a slot with a role equal to the
;;; parameter slot the function should preserve the ordering of such an
;;; inherited frame. FINALLY WRITTEN NEW CODE TO ACCOMPLISH THIS ORDER
;;; PRESERVING CONSTRAINT (SEE FUNCTION SPLICE-INTO). I WONDER HOW LONG IT TOOK
;;; TO GET AROUND TO IT. FOUR YEARS? [cox 20jan95]
;;; 
;;; WARNING: By using f.put! and f.put-all!, the value of the pattern property
;;; can become obsolete.
;;;
(defun f.put! (filler frame slot &optional (facet *value-facet*))
  (cond  ((null filler)
	  (format *frame-stream*
		  "Error in f.put!: Tried to put nil filler.")
	  (break))
	 ((not (frame-var-p frame))
	  (format *frame-stream*
		  "Error in f.put!: frame parameter ~s not a variable." frame)
	  (break))
	 (t
	  (let ((previous-val
;;;  		  (f.get frame slot facet)	; Redundant and inefficient.
		  nil				; Set previous-val below instead. [cox 19jan95]
		  )
		(frame-structure (*FRAME* frame)))
	    (do* ((slots-left (frame->slots frame-structure) (rest slots-left))
		  (each-slot (first slots-left)(first slots-left)))
		 ((or (null each-slot)
		      (equal (slot->role each-slot)
			     slot))
		  (if (null each-slot)
		      ;; Splices the slot into the frame maintaining correct slot order. [cox 20jan95]
		      (splice-into frame `(,slot (,facet ,filler)))
;		      (setf (symbol-value frame)
;			    (append frame-structure 
;				    `((,slot (,facet ,filler)))))
		      (do* ((facets-left (slot->facets each-slot)(rest facets-left))
			    (each-facet (first facets-left)(first facets-left)))
			   ((or (null each-facet)
				(equal (facet->facet-name each-facet)
				       facet))
			    (cond ((null each-facet)
				   (setf (rest each-slot) 
					 (append (rest each-slot) 
						 `((,facet ,filler)))))
				  (t
				   (setf previous-val (second each-facet))	; [cox 19jan95]
				   (setf (second each-facet) filler))))))))
	    ;; The following case is the replacement of one filler with another.
	    ;; The backptrs of the previous filler must be removed since no longer valid.
	    (if previous-val
		(if (atom previous-val)
		    (f.rem-back-ptrs previous-val frame slot facet)
		    (if (listp previous-val)
			(mapc #'(lambda (each-filler)
				  (f.rem-back-ptrs each-filler frame slot facet))
			      previous-val))))
	    (if (and (atom filler)
		     (not (attribute-value-p filler)))	; For efficiency sake [24feb95]
		(f.put-back-ptrs filler frame slot facet)
		(if (listp filler)
		    (mapc #'(lambda (each-filler)
			      (f.put-back-ptrs each-filler frame slot facet))
			  filler))))))
  filler)



;;;
;;; Function get-tail returns the tail of the slot list whose role matches the
;;; role of the new slot.
;;; 
(defun get-tail (new-slot slot-list)
  (if (not (null new-slot))
      (member (slot->role new-slot)
	      slot-list
	      :test
	      #'(lambda (new-role each-slot)
		  (equal new-role
			 (slot->role
			   each-slot)))))
  )


(defvar *undefined-roles* nil)

;;;
;;; Function splice-into is used to add a new slot into a frame that does not
;;; already have such a slot. The new slot is added to the current slots so as
;;; to maintain the original order of slots in the frame definition.
;;; 
(defun splice-into (frame new-slot
		    &optional (role-file-name 
			       (concatenate 'string 
				 CL-USER::*Meta-AQUA-system-dir* 
				 "Representations/rep_undefined-roles.lisp")))
  (do-break splice-into)
  (let* ((prototype 
	   (*FRAME*
	     (frame-type frame)))
	 (prototype-tail			; Prototype-tail will be a list of slots following 
	   (cond ((or (not (atom prototype))
		      (boundp prototype))
		  (rest (get-tail		; the new slot in the conceptual definition of frame.
			  new-slot
			  (frame->slots
			    prototype))))
		 (t
;;; 		  (break)
		  (when (not (member (frame-type frame) *undefined-roles*))
		    (with-open-file 
		      (role-def-file role-file-name
				     :direction :output
				     :if-exists :append
				     :if-does-not-exist :create)
		      (print `(define-relation ,(frame-type frame)
					       (isa (value (relation)))
				(domain (value (entity)))
				(co-domain (value (entity))))
			     role-def-file))
		    (setf *undefined-roles*
			  (cons (frame-type frame) *undefined-roles*)))
		  nil))))
    (if (null prototype-tail)			; Here the match will be the last prototype slot
	(setf (rest (*FRAME* frame))
	      (append				; so append ther new slot as the last slot.
		(frame-body frame)
		(list new-slot)))
	(if (get-tail (second (*FRAME*		; Here the match is with the first prototype slot
				frame))
		      prototype-tail)
	    (setf (rest (*FRAME* frame))	; so make new slot the first slot of the frame.
		  (cons new-slot
			(frame-body frame)))
	    (do ((slots-left			; otherwise search for the match and splice it in.
		   (frame-body frame)
		   (rest slots-left)))
		((or (null slots-left)
		     (get-tail
		       (second slots-left)
		       prototype-tail))
		 (if (null slots-left)
		     (setf (symbol-value frame)
			   (append (symbol-value frame) (list new-slot)))
		     (setf
		       (rest slots-left)
		       (cons new-slot
			     (rest slots-left)))))))))
  )


;;;
;;; Function f.set-literal is like an f.put! for literals.
;;; It creates a new binding for the frame variable representing the
;;; literal. It however does not manage any backpointer information
;;; as do the put functions for ordinary frames.
;;; 
(defun f.set-literal (literal literal-value)
  (set literal literal-value)
  )



;;; 
;;; Function f.modify is used to change the fillers of forms.
;;; Thus one can modify frame definitions during learning.
;;; The calling convention uses a path through the subframes
;;; to find nested fillers. The convention is the same as used
;;; in the function f.chase-path for slot-names. The parameter
;;; new-filler is the value the caller wishes to substitute.
;;; NOTE that the application still must bind the value herself
;;; since f.modify just returns a copy of the modified frame.
;;;
;;; |||||| What happens to variable bindings?
;;; What happens if there are instances or attribute values in here?
;;; OK this now handles simple variable bindings such as modification
;;; of a filler which is a frame variable. It will then change the
;;; slot poiunted to by the binding. The exceptions are *self* bindings
;;; and nested interior bindings. These are as of yet not handled.
;;; 
(defun f.modify (frame new-filler &rest slot-names)
  (let ((old-filler (apply #'f.chase-path frame slot-names)))
  (if (var-binding-p old-filler)
      (f.modify frame new-filler (var->role old-filler))
      (apply #'modify frame new-filler slot-names)))
  )

		 

;;;
;;; ||||||NOTE that this function will NOT handle
;;; variable bindings. Use with care.
;;;
;;; Function f.remove-filler! is a destructive
;;; removal of a facet's filler. This is performed by
;;; placing the nil frame on the facet. Function f.unify
;;; is aware of the frame and will act as if there is
;;; no facet on the slot.
;;; 
(defun f.remove-filler! (frame slot &optional facet)
  (f.put! *nil* 
	  frame
	  slot
	  (or facet
	      *value-facet*))
  )



;;;
;;; Function f.make-relation takes as input a frame variable and a
;;; slot-name (role), and creates a relation frame for the slot,
;;; placing it on the relation facet of the slot.
;;; 
(defun f.make-relation (frame slot-name)
  ;; If there already exists a relation facet then return it,
  ;; otherwise create a new one.
  (or (f.get frame slot-name *relation-facet*)
      (let ((relation-frame
	      (f.instantiate-frame
		(list (frame-type slot-name))
		(f.get frame *status-slot*)))
	    (slot (frame->slot (*FRAME* frame) slot-name)))
	(format-if 
	  *Debug-On*
	  *frame-stream*
	  "~%     Frame: ~s~%     Slot: ~s~%     Slot Name: ~s~%"
	  frame slot slot-name)
	(f.put frame
	       relation-frame
	       *domain-slot*)
	(f.put (slot->filler slot *value-facet*)
	       relation-frame
	       *co-domain-slot*)
	;; Add a slot property. This is like a slot system slot.
	(putprop relation-frame *slot-prop* slot-name)
	(f.put relation-frame
	       frame
	       (slot->role slot)
	       *relation-facet*)
	;; The following marks the relation-frame in the set of beliefs
	;; if not already in, and both domain and co-domain are in the set of beliefs.
	(in-set-of-beliefs-p relation-frame)
	relation-frame))
  )



;;;
;;; Function f.get-relation returns the relation frame of a slot.  If relation
;;; facet does not already exist on the slot, one is created and returned.
;;; 
(defun f.get-relation (frame slot-name)
  (let ((relation-frame (f.get frame slot-name *relation-facet*)))
    (or relation-frame
	(f.make-relation frame slot-name)))
  )
  

;;;
;;; Function f.add-relations takes a frame token (eg., PTRANS.101)
;;; as input, and creates an explicit relation frame for each slot of
;;; token having a value facet, but not already having a relation facet.
;;; 
;;; |||||| What happens when frame is a symbol? This
;;; may happen when the slot is a system slot
;;; such as status.
;;; 
(defun f.add-relations (frame)
  (add-relations frame)
  (un-mark frame)
  )

(defun add-relations (frame)
  (cond ((or (null frame)
	     (visited-p frame))
	 nil)
	((and (frame-var-p frame)
	      (not (attribute-value-p frame)))
	 (dolist (each-slot (f.slot-list frame))
		  (let ((val-facet (slot->filler
				     each-slot
				     *value-facet*))
			(role (slot->role each-slot)))
		    (cond ((and val-facet
				(not (equal *status-slot* role))
				(not (equal *truth-slot* role)))
			   (if (and
				 (not (frame-list-p val-facet))
				 (not (slot->filler
					each-slot
					*relation-facet*)))
			       (f.make-relation frame role))
			   (cond ((frame-list-p val-facet)
				  (dolist (each-sub-frame val-facet)
				    (mark-as-visited each-sub-frame)
				    (add-relations each-sub-frame)))
				 (t
				  (mark-as-visited val-facet)
				  (add-relations val-facet)))))))))
  frame)




;;;
;;; Function f.unify will merge two frames and return the result.
;;; All slots in one node but not the other will be contained in
;;; the merged result. Any facets in common will have a recursive
;;; call of f.unify applied to them. Backpointers are maintained
;;; so that the result is consistent.
;;;
;;; F.unify returns multiple values as the result of its call:
;;;           result and was-successful.
;;; The first return value is the resultant, unified, frame variable 
;;; if the unification was successful, otherwise nil is returned. The
;;; second return value is nil if the execution was successful,
;;; otherwise it is a list of the offending subframes. Frames will 
;;; unify if they are equal, if one is the null frame nil.0 (equal to
;;; the *nil* constant), or one isa descendent of the other.
;;;
;;; For example
;;; if x = ptrans.101 = (ptrans (actor (value animate-object.12))
;;;                             (object (value rock.10)))
;;;    y = ptrans.202 = (ptrans (actor (value criminal.21))
;;;                             (instrument (value propel.33)))
;;;    criminal isa person
;;;    person isa animate-object
;;; then (f.unify x y) will return ptrans.202 and nil
;;;      ptrans.202 now equals
;;;      (ptrans (actor (value criminal.21))
;;;              (instrument (value propel.33))
;;;              (object (value rock.10)))
;;; 
;;; Also, any defined frame that had a filler of ptrans.101 in the entire
;;; system will now have a filler of ptrans.202.
;;;
;;; BEWARE of a subtle  side-effect that may cause much grief.
;;; If node.111 and node.222 are passed to f.unify, then one of the
;;; two will be returned as the unified result. It cannot be decided
;;; by the programmer which will be the case. Thus if either parameter
;;; is bound to a local or global variable, then there is a good likelyhod 
;;; that the variable is no loger valid. Consider the following example.
;;;
;;; (let ((temp (f.instantiate-frame person)))
;;;   (unify temp another-person)
;;;   ;; The result of unify may have been another-person
;;;   ;; so temp may be obsolete when called now.
;;;   (function-that-uses-temp temp))
;;;
;;; A better approach is illustrated next.
;;; 
;;;  (let ((temp (f.instantiate-frame person)))
;;;    (setf temp (unify temp another-person))
;;;    ;; Now temp is guaranteed to be consistent.
;;;    (function-that-uses-temp temp))
;;;
;;; 
;;; To use the second and thrid returned values of f.unify just perform a call
;;; similar to the following.
;;;
;;;  (let ((return-val nil)
;;;  	   (failures nil)
;;;        (error-path nil))
;;;  	(multiple-value-setq 
;;;  	  (return-val failures error-path) 
;;;  	  (f.unify frame1 frame2))
;;;  	(if failures
;;;  	    (do-recovery failures error-path)
;;;  	    (further-processing return-val)))
;;;
;;; The notify parameter is used to suppress unification failure notifications.
;;; If notify is set to the constant :notify-but-no-break, then the error
;;; messages are printed, but the user is not prompted for the break call.
;;;
;;; |||||| Since can-unify-p now unmarks the nodes, is it necessary to do it
;;; here also?
;;;
;;; Many times, the program will have already checked that the match is OK with
;;; can-unify-p before calling f.unify. In such cases it is not necessary to
;;; recheck it in the first cond clause. Therefore optional parameter flag
;;; match-prechecked? can be set to nil in order to skip this operation.  [cox
;;; 10jan95]
;;;
;;; |||||| NOTE that there is an anomaly with f.unify if one frame has
;;; variables bound whereas anoter does not. For example, you could have a
;;; representation for "A person takes himself somewhere." as the frame
;;; 
;;;        (ptrans (actor (value (person)))(object (value =actor)))
;;; 
;;; whereas the representation for "A person takes someone else somewhere." is
;;; 
;;;        (ptrans (actor (value (person)))(object (value (person))))
;;;
;;; Unfortunately can-unify-p and f.unify will not discriminate them. The
;;; result of such a unification (as far as I can tell) is the frame with the
;;; variable bindings; that is, "A person takes himself somewhere." 
;;; [cox 25feb95]
;;; 
(defun f.unify (node1 node2 &optional (notify t) match-prechecked? lazy?)
  (flet ((unmark-list (node)			; Locally defined function
	   (if (listp node)			; to unmark frame lists.
	       (mapcar #'un-mark node)
	       (un-mark node))))
    (let ((return-val nil)			; Allocate variables to
	  (failures nil)			; store multiple values
	  (error-path nil))			; returned by can-unify-p.
      (if (or (isa-p 'goal node1)
	      (isa-p 'goal node2))
	  (break))
      (cond ((and (null node1)
		  (null node2))
	     (return-from			; If this occurs, then probably
	       f.unify				; should send some error message.
	       (values nil nil nil)))
	    ((or match-prechecked?
		 (multiple-value-setq
		   (return-val failures error-path)
		   (can-unify-p
		     node1 node2 lazy?)))
	     (unmark-list node1)
	     (unmark-list node2)
	     (when (null
		     (setf
		       return-val
		       (do-unify
			 node1 node2 lazy?)))
	       (format 
		 *frame-stream*
		 (str-concat
		   "~%ERROR: Can-Unify returns t "
		   "but do-unify fails."
		   "~%Nodes ~s and ~s")
		 node1 node2)
	       (break)))
	    )
      (unmark-list node1)
      (unmark-list node2)
      ;; |||||| Eventually remove or make dependent on *Debug-On*.
      (cond ((and notify failures)
	     (format *frame-stream*
		     "~%~%Unification fails.")
	     (format *frame-stream*
		     "~%Attempt to merge ~s and ~s"
		     (first failures)
		     (second failures))
	     (if error-path
		 (format *frame-stream*
			 "~%  along path ~s" error-path))
	     (format *frame-stream*
		     "~%  during unify call on ~s and ~s." node1 node2)
	     (if (and (not (eq notify :notify-but-no-break))
		      (y-or-n-p "Do you wish to BREAK ? "))
		 (break))))
      (values return-val failures error-path)))
  )


;;; 
;;; This function should always return something since at the
;;; top of the isa hiearchy everything is an entity.
;;; 
(defun f.lowest-common-ancestor (node1 node2)
  (let ((returned-value (my-filter (lca node1 node2))))
    (if (or (equal returned-value (list nil))
	    (null returned-value))
	;; |||||| If the hiearchy is right this should not happen.
	;; Probably should flag as error.
	(list *entity*)
	returned-value)))



;;; 
;;; Called by do-index.
;;; 
(defun f.put-back-ptrs-for-index (symbol property filler)
  (if (frame-var-p filler)
      (addprop filler *Back-Ptrs* (list 'index symbol property)))
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;           FRAME PREDICATES
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;
;;; The function can-unify-p is evaluated before starting an actual
;;; unification in order to avoid retracting backpointers in mid-process.
;;; The function returns three values: the first is true or nil for its
;;; success, the second is the reason if it fails, while the third is the
;;; path from the start to the failure if there is one. If success the
;;; second and third values returned are nil.
;;;
(defun can-unify-p (node1 node2 &optional lazy?)
  (let ((return-val nil)
	(failures nil)
	(error-path nil))
    (multiple-value-setq
      (return-val failures error-path)
      (can-unify-p1 node1 node2 lazy?))
    (un-mark node1)
    (un-mark node2 )
    (values return-val failures error-path))
  )

;;;
;;; |||||| This predicate is obsolete now?
;;; 
(defun instance-p (symbol)
  (get symbol *instance-prop*)
  )



;;;
;;; Predicate literal-p returns t if the frame is a literal, nil otherwise. It
;;; assumes that frame is not a frame form such as (literal "Mark"), rather it
;;; is a frame variable such as literal.12.  Note that both instantiate-frame
;;; and instantiate-filler test for literal frame forms without using
;;; literal-p.
;;; 
(defun literal-p (frame)
  (and (not (listp frame))
       (isa-p *literal* (list frame)))
  )


;;;
;;; Predicate relation-p returns t if the frame is a relation, nil otherwise.
;;; It assumes that frame is not a frame form such as (relation (domain
;;; ptrans.12)), rather it is a frame variable such as literal.12.
;;; 
(defun relation-p (frame)
  (and (not (listp frame))
       (isa-p 'relation (list frame)))
  )


;;;
;;; Predicate attribute-value-p must not only check the property list, but in
;;; those cases where an attribute value is included in a frame definition
;;; before the attribute value is defined the property list will not yet be
;;; available. This predicate will be used in f.instantiate-frame to test for
;;; these.
;;; 
(defun attribute-value-p (symbol)
  (or (get symbol *attribute-value-prop*)
      (ends-with-dot-zero-p symbol))
  )




;;; Predicate var-binding-p returns true is the symbol starts with the
;;; character "=". Nil is returned otherwise.  In frame definitions this symbol
;;; indicates a variable binding. For example the filler =actor stands for the
;;; filler of the value facet of the actor slot. Thus it acts as a pointer in
;;; frame definitions.
;;; 
(defun var-binding-p (symbol)
  (if (and
	(atom symbol)
	(find #\= (string symbol)))
      t)
  )



;;; ||||||| Quickly became apparent how complex this would be to make totally general.
;;; So is not working.
;;; 
(defun f.make-binding-list (frame-form)
  (let ((binding-list nil))
  (dolist (each-slot (frame->slots frame-form))
    (dolist (each-facet (slot->facets each-slot))
      (let ((filler (facet->filler each-facet)))
      (if (var-binding-p filler)
	(let ((var-role (var->role filler)))
	  (add-binding )))))))
  )



;;; 
;;; Function add-path-extension takes a path and extends it by adding a new
;;; segment at the end. If path is ((role1 facet-name1)(role2 facet-name2)),
;;; then function add-path-extension returns the following value of  
;;; ((role1 facet-name1)(role2 facet-name2)(role3 facet-name3))
;;;
;;; Function Created: [cox 27jan95]
;;; 
(defun add-path-extension (slot facet path)
  (append
    path
    (list
      (list (slot->role slot)
	    (facet->facet-name facet))))
  )



;;;
;;; Function get-paths-2-bound-vars returns a list of paths to all variables in
;;; frame-form bound to the filler of role in frame-form. That is, it takes as
;;; input a frame form [e.g., (ptrans (actor (value (actor)))), not a frame
;;; variable like ptrans.123] and a role name, and produces a list of binding
;;; paths. Therefore, the following call
;;; 
;;; (get-paths-2-bound-vars
;;;    '(ptrans
;;;        (actor  (value (person)))
;;;        (object (value =actor))
;;;        (to     (value (at-location
;;;                          (domain (value =actor))
;;;                          (co-domain (value (physical-location)))))))
;;;    'actor)
;;;
;;; returns the following list
;;; 
;;; ( ((TO VALUE) (DOMAIN VALUE))
;;;   ((OBJECT VALUE))            )
;;;
;;; Unfortunately, if the actor binding to the domain of the at-location was
;;; instead bound to the object slot, the function will not return the
;;; transitive binding.
;;;
;;; Another problem exists because the function does not support the
;;; commutative property of variable bindings; that is, if x is bound to y,
;;; then y is bound to x.  Take for example the simple case of following frame
;;; that has a synonym binding. The values of both the domain and my-synonym
;;; roles are the same entity.
;;;
;;; (my-relation
;;;   (domain (value (entity)))
;;;   (my-synonym (value =domain)))
;;; 
;;; Now, assuming that the variable q is assigned the above form, a call of
;;; (get-paths-2-bound-vars q 'domain) returns (((MY-SYNONYM VALUE))); whereas
;;; a call of (get-paths-2-bound-vars q 'my-synonym) returns nil.
;;; 
;;; 
;;; Function Created: [cox 26jan95]
;;; 
(defun get-paths-2-bound-vars (frame-form role &optional path)
  (cond
    ((null frame-form) nil)
    (t
     (let ((return-val nil))
       (if (var-binding-p (second frame-form))	; Then this is a named filler. [cox 26jun95]
	   (setf frame-form (cons (first frame-form)	; Strip out var marker if so.
				  (rest (rest frame-form)))))
       (dolist (each-slot (frame->slots frame-form))
	 (dolist (each-facet (slot->facets each-slot))
	   (let ((filler (facet->filler each-facet)))
	     (cond
	       ;; If filler is a frame variable (e.g., ptrans.12), no hit.
	       ((frame-var-p filler)
		nil)
	       ;; If it is a variable binding, check for hit.
	       ((var-binding-p filler)
		(if (eq (var->role filler) role)
		    (setf return-val
			  (cons (add-path-extension
				  each-slot each-facet path)
				return-val))))
	       ;; If it is a list of fillers, then traverse the list to check.
	       ((frame-list-p filler)
		(dolist (each-item filler)
		  (cond
		    ((frame-var-p each-item)
		     nil)
		    ((listp each-item)
		     ;; then this is an embedded frame form.
		     (setf return-val
			   (append
			     (get-paths-2-bound-vars
			       each-item
			       role
			       (add-path-extension
				 each-slot each-facet path))
			     return-val)))
		    ;; then this is a variabhle binding to check.
		    ((var-binding-p each-item)
		     (if (eq (var->role each-item) role)
			 (setf return-val
			       (cons (add-path-extension
				       each-slot each-facet path)
				     return-val)))))))
	       ;; Else it muist be another frame form to recurse on.
	       (t (setf return-val
			(append
			  (get-paths-2-bound-vars
			    filler
			    role
			    (add-path-extension
			      each-slot each-facet path))
			  return-val)))
	       ))))
       return-val)))
  )

;;; The following was test code.
;
;(defun trans55 (alist )
;  (cond ((null alist )nil)
;	(t(cons `(quote ,(first alist))
;	      (trans55 (rest alist)))))
;  )
;
;
;(defun test (f role )
;  (dolist (each-path (get-paths-2-bound-vars f role ))
;    (let ((translist (trans55 each-path)))
;    (print (eval `(f.chase-path (quote ,f) ,@translist))))))
;
;(setf *x*
;      '(ptrans
;	 (actor  (value (person)))
;	 (object (value =actor))
;	 (to     (value (at-location
;			  (domain    (value =actor))
;			  (co-domain (value (physical-location))))))))

;;; 
;;; The function f.put-all! is a destructive frame assignment like f.put! since
;;; it replaces any value already on the given slot and facet of frame with the
;;; passed filler parameter, but unlike f.put!, it also replaces any other
;;; fillers that are bound to the same filler in that frame. The scope of the
;;; frame is determined by the original instantiated pattern during the
;;; creation of the frame parameter. That is, the bindings are found on the
;;; pattern property assigned to the frame variable at instantiation time by
;;; function instantiate-frame (the helper function of f.instantiate-frame).
;;; See function get-paths-2-bound-vars.
;;;
;;; WARNING: By using f.put-all! and f.put!, the value of the pattern property
;;; can become obsolete.
;;;
;;; See comments on function get-paths-2-bound-vars for caveats for using this
;;; function. Beware of using synonym slot names and transitive bindings.
;;;
;;; Function Created: [cox 26jan95]
;;; 
(defun f.put-all! (filler frame slot &optional (facet *value-facet*))
  (f.put! filler frame slot facet)
  (if (eq facet *value-facet*)
      (let ((bound-frame-paths
	      (get-paths-2-bound-vars
		(get frame 'pattern) slot)))
	(dolist (each-path bound-frame-paths)
	  (f.put! filler
		  (or (apply
			#'f.chase-path
			(cons
			  frame
			  (butlast each-path)))
		      frame)			; e.g., (ptrans (actor (val (person)))(object (val =actor))))
		  (first
		    (first (last each-path)))
		  (second
		    (first (last each-path)))))))
  )


		
;;; 
;;; Predicate frame-var-p returns true if symbol is a frame
;;; variable such as PTRANS.1. The test is confirmed if the
;;; symbol has a period imbedded in it.
;;; Nil is returned otherwise.
;;;
;;; |||||| NOTE that as an unfortunate choice of conventions
;;; the following call will return true.
;;;
;;; (frame-var-p 'f.get)
;;; 
(defun frame-var-p (symbol)
  (if (and (atom symbol)
	   (not (numberp symbol))
	   (find #\. (string symbol)))
      t)
  )

;;; 
;;; Predicate f.type-p returns t if symbol has been defined with define-frame.
;;; The predicate currently depends upon a trick. In the frame system all types
;;; defined by define-frame have their definition placed on the value of the 
;;; type specifier symbol. The definition is a list whose head is always equal
;;; to the symbol itself. Therefore, the value of the symbol "ptrans" is
;;;       (ptrans (role1 (value filler1)...)...)
;;; The test is to check if the symbol is equal to the car of the symbol.
;;; 
;;; NOTE to quote the argument passed to it, e.g., (f.type-p 'ptrans)
;;; 
;;; [16aug93]
;;;
;;; But actually, aren't there frames that break this convention.
;;; Perhaps it is instantiated fillers after unification or something
;;; similar, but I have seen frames like PTRANS = (MOP (...)).  [2sep93]
;;; 
(defun f.type-p (symbol)
  (if (and (atom symbol)
	   (boundp symbol))
      (let ((sym-val (symbol-value 
		      symbol)))
	(and (consp sym-val)
	     (eq symbol
		 (first sym-val)))))
  )



;;; 
;;; Function frame-list-p returns true if filler is a frame list.
;;; For example the links slot of XPs have value facets which
;;; contain a list of causal relations in the explanation. Lists
;;; are detected when its first item is another list, a frame
;;; variable, or a variable binding.
;;;
;;; NOTE that this function is NOT equivalent to listp. For example
;;; it will not return t if filler is a subframe of the form
;;;      (ptrans (actor (person (volitional-agent))))
;;; 
(defun frame-list-p (filler)
  (and
    (not (null filler))
    (listp filler)
    (let ((first-item (first filler)))
      (or
	(var-binding-p first-item)
	(frame-var-p first-item )
	(listp first-item)
	(equal first-item '.list))))
  )



;;; 
;;; Predicate f.defined-role-p determines whether the given role is defined for
;;; the given frame. True is returned if something can be inherited on that
;;; role.
;;;
(defun f.defined-role-p (role frame)
  "Return t if role is defined for frame (or ancestor)."
  (if (f.inherit frame role)
      t)
  )


;;; 
;;; If this predicate is true then node1 is a specialization of one of node2
;;; abstractions.  Eg. if Node2 = Animate-object.12 which isa animate-object
;;; and Node1 is person.12 this function returns non-nil since person.12 isa
;;; person isa animate-object.
;;; 
(defun f.common-ancestor-p (node1 node2)
  (let ((class (get-abstraction node1)))
    (some #'(lambda (each-abstr)
	      (isa-p each-abstr class))
	  (get-abstraction node2))))


;;;
;;; Predicate in-set-of-beliefs-p returns t or nil depending on the truth
;;; condition of the assertion.  IT ALSO HAS THE SIDE EFFECT OF MARKING THE
;;; ASSERTION TRUE IF IT IS NOT ALREADY, BUT IT PASSES THE THIRD CONDITION
;;; BELOW.
;;; 
;;; An assertion is in the set of beliefs if either
;;;  1. it has no truth-slot [commented out 21may94]
;;;  2. it has a truth slot equal to in.0 or,
;;;  3. if
;;;     a. it is a relation,
;;;     b. both the domain and co-domain are in the set of beliefs
;;;     c. and the domain has a relation facet on the slot
;;;        represented by the assertion equal to it.
;;;        E.g., assertion = name.12 = (name
;;;                                     (domain (value person.21)))
;;;             person.21 = (person
;;;                           (name (relation name.12)))
;;; 
(defun in-set-of-beliefs-p (assertion)
  (cond ((listp assertion)
	 (dolist (each-assertion assertion)
	   (in-set-of-beliefs-p
		each-assertion)))
	(t
	 (or
;;;        (null (f.get assertion *truth-slot*)); Condition 1 above.
	   (equal
	     (f.get assertion *truth-slot*)	; Condition 2 above.
	     *in*)
	   (if (isa-p 'relation			; Condition 3 above.
		      (list assertion))
	       (and
		 (in-set-of-beliefs-p		; Condition a above.
		   (f.get assertion
			  *domain-slot*))
		 (in-set-of-beliefs-p		; Condition b above.
		   (f.get assertion
			  *co-domain-slot*))
		 (equal assertion		; Condition c above.
			(f.get
			  (f.get assertion
				 *domain-slot*)
			  (frame-type assertion); ||||||Should use f.relation-slot-name instead of frame-type
			  *relation-facet*))
		 (f.put *in*			; Now for the side-effect.
			assertion
			*truth-slot*))))))
  )



;;; 
;;; Predicate visited-p returns true if the parameter has already been visited
;;; during a net traversal during execution of either function f.traverse-frame
;;; or f.unify. See comment on the former function for an example of the
;;; predicate's use.
;;; 
(defun visited-p (node &optional (marker *visited-prop*))
  (get node marker))



;;; 
;;; |||||| Will we ever use this again? If not remove. It now returns nil
;;; always.
;;; 
(defun needs-special-handling-p (concept)
;  (or (null concept)
;      (not (member (car concept) *Defined-Structs*)))
  nil
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    FRAME SYSTEM USER FUNCTIONS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;
;;; A unique marker is needed because the function funct applied to all nodes
;;; in the network, may directly or indirectly call one of the visited
;;; functions. For example, perform-ebg-on passes the function generalize-node
;;; to f.traverse-frame. This function in turn performs f.unify on individual
;;; nodes. Since f.unify uses the *visited-prop* value, there exists a
;;; conflict.
;;; 
;;; (defconstant ;as constant the compiler does some tricky in-code
;;; optomization that throws off loading the system.
(defvar  
  *traverse-marker* (gentemp (string *visited-prop*)))

;;;
;;; Function f.traversal-frame may be the most useful of all functions for
;;; custom system building. This generic utility is designed to automatically
;;; traverse all subframes reachable from some starting root node (the
;;; parameter frame), applying a user defined function (funct) at each node
;;; along the way. The user provided function is required to have the following
;;; initial parameters: current-frame, parent, role, facet-name, and level.
;;; Additional parameters may be provided after these.  Any arguments bound to
;;; the &rest parameter of f.traverse-frame will be called after the mandatory
;;; parameters of funct each time it is evaluated.  These mandatory parameters
;;; are bound to the following each iteration thru the subframes. Current-frame
;;; is the current subframe, parent is the super- frame of current-frame. Role
;;; and facet-name specify the current location within parent of current-frame,
;;; and level is the current recursion depth from the original root node.
;;;
;;; Function f.traversal-frame manages cycles in the subframes, so will not
;;; enter infinite loops. It will terminate when all subframes from the
;;; original root node bound to frame at calling time are visited. The
;;; predicate visited-p is a useful function to be used in funct. For example
;;; the following function and call to f.traverse-frame will result in printing
;;; all persons in subframes reachable from the frame
;;; *current-story-representation*.
;;;
;;; (defun find-characters (current-frame parent role facet-name level)
;;;   (if (and
;;; 	    (not (visited-p current-frame *traverse-marker*))
;;; 	    (equal (get-abstraction current-frame)
;;; 	           'person))
;;;       (format *frame-stream* "~%Story character: ~s" current-frame)))
;;; 
;;; (f.traverse-frame *current-story-representation* #'find-characters)
;;;
;;; Function f.count-subnodes shows another simple use of f.traverse-frame.
;;; Also see the structure of functions f.find-filler and f.find-paths below
;;; for a more elaborate use of f.traverse-frame. More examples are found in
;;; file application-frame-demo.lisp.
;;; 
(defun f.traverse-frame (frame funct &rest args)
  (cond ((frame-var-p frame)
	 (apply #'traverse-frame1 `(,frame ,funct nil nil nil 0 ,@args))
	 (un-mark frame *traverse-marker*))
	(t
	 (format
	   *frame-stream*
	   (str-concat
	     "~%Error: Frame parameter ~s passed to "
	     "f.traverse-frame was not a frame variable.~%")
           frame)))
  )



;;;
;;; Really should use a closure rather than a global variable, but ...
;;; 
(defvar *counter*  0
  "Counter for function count-subnodes.")


;;; [cox 26feb95]
(defun f.count-subnodes (current-frame)
  "Count the number of subnodes below current-frame."
  (setf *counter* 0)
  (f.traverse-frame current-frame #'sub-count)
  *counter*
  )


;;; [cox 26feb95]
(defun sub-count (current-frame parent role facet-name level)
  "Funct passed to f.traverse-frame to count subnodes."
   (if (not (visited-p current-frame *traverse-marker*))
       (setf *counter* (+ 1 *counter*)))
)
 

;;;
;;; Function f.find-paths is used to locate and print all locations of a target
;;; subframe reachable from a given starting-frame. This function is useful,
;;; for instance, when f.unify issues an error message when trying to unify two
;;; frames. F.unify will print two subframes of the original frames to be
;;; unified that were incompatible.  The task then is to find where the error
;;; occured. In non- trivial frames this task is problematic. However the
;;; f.find-paths function will locate the offending subframe by calling
;;; (f.find-paths original-frame offending-frame).
;;; 
(defun f.find-paths (starting-frame target)
  (setf *PATH-NODES* (list target))
  (setf *FINISHED-NODES* nil)
  (let((temp nil))
    (loop
      (if (null *PATH-NODES*)
	  (return))
      (setf temp *PATH-NODES*)
      (setf *PATH-NODES* nil)
      (print temp *frame-stream*)
      (dolist (each-subframe temp)
	(cond ((not (or (equal starting-frame each-subframe)
			(member each-subframe *FINISHED-NODES*)))
	       (setf *FINISHED-NODES* (cons each-subframe *FINISHED-NODES*))
	       (f.find-filler starting-frame each-subframe))))))
  )


;;;
;;; Function f.find-filler will print the facet name, role
;;; (slot name) and parent of target if it is a subframe of
;;; starting-frame.
;;; 
(defun f.find-filler (starting-frame target)
  (f.traverse-frame
    starting-frame
    #'(lambda (filler parent role facet-name level target)
	(cond ((equal filler target)
	       (format
		 *frame-stream*
		 "~%Found filler ~s in the ~s facet of the ~s slot of ~s."
		 filler facet-name role parent)
	       (setf *PATH-NODES* (cons parent *PATH-NODES*)))))
    target)
  )



;;; 
;;; Function f.pprint is used to pretty-print a given frame. Only attribute
;;; values (the filler of value facets) are printed; no relation facets or
;;; other facets are printed. Additionally, rather than printing a slot as
;;; (color (value blue.0)), (color blue.0) is printed instead.
;;;
;;; The optional parameter level specifies the number of subframes to print. By
;;; default the function only prints the first level of subframes, that is, it
;;; prints the slot fillers of the original frame passed to the function. If
;;; level is 2, then it would print all slots, and call pretty print all
;;; fillers as well.
;;;
;;; The additional optional parameter allows the user to over-ride the default
;;; stream of *frame-stream*. The *frame-stream* may be bound to a different
;;; window than the window in which the user is currently working.
;;;
;;; |||||| This function may only partially restrict reprinting duplicate
;;; frames in a hierarhical structure. I have not tested it well.
;;; [cox 24apr94]
;;; 
(defun f.pprint (frame &optional (level 1) (stream *frame-stream*) processed-list)
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
	 (format
	   stream
	   (str-concat
	     "~%The frame symbol passed to "
	     "f.pprint has no value.~%")))
	((and (equal level 1)
	      (not (member frame processed-list)))
	 (pprint1 frame stream)
	 (cons frame processed-list))
	((not (member frame processed-list))
	 (pprint1 frame stream)
	 (terpri stream)
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
;;; Function return-path-to returns a path from the root node to the target
;;; node. Since it performs a depth-first search to find it, the path may be
;;; deeper into the frame than the user expects.
;;; 
(defun f.return-path-to (target-frame root-frame)
  (let ((return-val (return-path-to1 target-frame root-frame)))
    (un-mark root-frame)
    (reverse return-val))
  )


;;;
;;; Function f.specializations-of is a kind of inverse isa function. Given a
;;; frame that is a type (as opposed to an instantiated frame variable), the
;;; function will return all other types that isa frame-type. The list is on
;;; the 'instances property of the symbol frame-type. It is placed there by
;;; function f.define-frame.  [29oct94]
;;; 
(defun f.specializations-of (frame-type)
  (if (not (f.type-p frame-type))
      (break
	"Argument ~s is not a frame type in f.specializations-of."
	frame-type)
      (get frame-type 'instances))
  )




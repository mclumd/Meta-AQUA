;;; -*- Mode: LISP; Syntax: Common-lisp; Package: TALE-SPIN; Base: 10 -*-

(in-package :tspin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	    Elvis World and the Tale-Spin Story Generation Subsystem
;;;;				 for Meta-AQUA
;;;;
;;;;	     Copyright (C) 1996   Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;		      File: tspin-tokens-4-meta-aqua.lisp
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





;;;****************************************************************************
;;;
;;;			   TSPIN-TOKENS-4-META-AQUA
;;;
;;;****************************************************************************

;;;
;;; This file contains the code that instantiates of all frame representations 
;;; of the tokens available in Tale-Spin stories. The problem is therefore to 
;;; take the story tokens (e.g., character and object instances), which are
;;; represented as symbols with property values and to transform them into the
;;; equivalent frame with slot-filler representation that Meta-AQUA understands.
;;; All tokens used by Tale-Spin are defined in the file data.lisp. A partial 
;;; conceptual hierarchy for the tokens is specified through an is-a property of
;;; each symbol. These property values define a type for each token. The 
;;; hierarchy is a strict single inheritance hierarchy. Other symbol properties 
;;; specify the various attributes of the types and tokens, such as the 'color 
;;; property of the symbol 'apple1 has the value 'red. An equivalent frame 
;;; would of course be (food (color (value red.0))). Unfortunately, Tale-Spin's
;;; conceptual hierarchy is unusual in some ways. For example, apple1 is-a food,
;;; not is-a apple which is-a food. Hence the odd frame header.
;;; 
;;; Since Tspin never assigns a value to type or token symbols, nors uses the 
;;; value space in any manner, the program can safely bind the TSpin symbols to
;;; their Meta-AQUA equivalent frame representations. The knowledge 
;;; representation system in the frame package already uses the symbol value for
;;; frame definitions of types. Type definitions are in the rep_*.lisp files.
;;; Thus, it is a straight forward process to input a TSpin token, retrieve the 
;;; is-a property value to obtain a type, instantiate a frame token by calling 
;;; function f.instantiate-frame on the value of type symbol, pass through all 
;;; properties of the TSpin token symbol to find those that match a legitimate
;;; frame role, insert these atributes into the new frame, and finally place the 
;;; instantiated frame on the value of the Tspin token symbol.
;;; 
;;; All of the tokens available to Tale-Spin are listed below.
;;;
;;; *cast* = (lynn karen mom dad elvis officer1 sheryl)
;;; *objects* = (cup1 cup2 cup3 cup4 glass1 glass2 glass3 glass4 apple1 apple2 
;;;              orange1 orange2 banana1 banana2 table1 table2 basket1 ganja1
;;;              pipe1 pipe2 lighter1 hot-faucet cold-faucet milk1 cold-water1
;;;              hot-water cold-water2 jack cat1 vase1 vase2 vase3 calla-lilly1
;;;              calla-lilly2 rose-bush1 rose-bush2 ball1 ball3 ball2 balloon1
;;;              balloon2 balloon3 nail1 board1 hammer window1 grass door-bell1
;;;              floor1 phone1 light1 dog1 smoke1 air1 air2 police-dog1	pan1))
;;; 
;;; *components* = (light-switch1 orange-peel1 orange-peel2 banana-peel1 
;;;                 banana-peel2 door-bell-switch1 fridge-door cupboard-door 
;;;                 cold-faucet-handle hot-faucet-handle phone-receiver1)
;;; 
;;; *all-locations* = (kitchen cupboard sink stove fridge garage  outside1)


(defparameter *target-package* 'Meta-Aqua
  "Symbol package that imports new symbols from TSPIN.")

;;;
;;; See function insert-fillers. Note that components are added by function
;;; link-component-with-whole. 
;;;
(defparameter *ignore-properties* '(facts demons goals is-a component component-of)
 "Properties not to map from tale-spin tokens to frame slots.")


;;; 
;;; The *cast* is equal to *personae* along with officer1, police-dog1,
;;; and sheryl.
;;; 
(defparameter *cast* '(lynn karen mom dad elvis officer1 police-dog1 sheryl)
  "The cast of Tale-Spin characters.")



(defun property-names (symbol)
  "Return a list of properties associated with parameter symbol."
  (let ((name-toggle t)
	(properties nil))
  (dolist (each-item (symbol-plist symbol))
	  (if name-toggle
	      (setf properties
		    (cons each-item
			  properties)))
	  (setf name-toggle (not name-toggle)))
  properties))




;;; |||||| Note that if one tries to create the new symbol with the concat
;;; once then nil will be returned. However if a second attempt is made the
;;; bad symbol will be returned because the first time added it to the symbol 
;;; table. [cox 20aug93]
;;;
(defun map-property-val (property-val)
  (if (integerp property-val)
      (f.instantiate-literal
	property-val
	*story-instance*)
    (let ((new-symbol nil)
	  (package-name nil))
      (multiple-value-setq 
       (new-symbol package-name)
       (concat property-val (intern ".0")))
      (if (eq package-name :internal)
	  new-symbol)))
  )


;;;
;;; Function insert-fillers passes through all properties of tspin-token,
;;; inserting the property values into  corresponding slots of frame-token 
;;; (if slot exists named by property) except for the properties listed in 
;;; program parameter *ignore-properties*.
;;;
;;; The function returns the (possibly) modified frame-token.
;;;
(defun insert-fillers (tspin-token frame-token)
  (dolist (each-property (property-names 
			   tspin-token))
    (if (and (not (member each-property 
			  *ignore-properties*))
	     (f.defined-role-p each-property 
			       frame-token))
	;;||||||Should check to see if frame-token already 
	;; has a filler and do unification instead if so. 
	(f.put-all!
	  (map-property-val 
	    (get tspin-token 
		 each-property))
	  frame-token
	  each-property)))
  frame-token
  )



;;;
;;; Function make-token takes as input a Tale-Spin token
;;; (an unbound symbol in the TSPIN package having various
;;; property values) and outputs a frame representation of 
;;; it. As a side-effect the frame is bound to the value of 
;;; the token symbol. The frame is created by passing the 
;;; symbol's type, or isa property, to the frame instantiation 
;;; function f.instantiate-frame. The type identifier is assumed
;;; to have a value which constitutes its frame definition. This 
;;; definition is set by the knowledge representation files 
;;; (usually rep_*.lisp)  The properties values are mapped to 
;;; slot-fillers by calling insert-fillers.
;;;
(defun make-token (tspin-token)
  (let* ((type (get tspin-token 'is-a))
	 (frame-token (f.instantiate-frame 
		       (symbol-value type)
		       *story-instance*)))
    (if (eq type 'person)
	(f.put! (f.instantiate-literal 
		 (if (numberp tspin-token)
		     tspin-token
		   (string tspin-token))
		 *story-instance*)
		frame-token
		'name))
    ;; Insert-fillers returns the new frame token,
    ;; so make-token does as well.
    (insert-fillers tspin-token frame-token)
    ))
    

;;;
;;; Function instantiate-frame-tokens is the main function of this file. It
;;; passes through every token in Tale-Spin, binding the symbol value of each
;;; (which Tale-Spin never uses) to a frame representation of the token (by
;;; calling function make-token). The result is imported into the package
;;; specified by program parameter *target-package*.
;;;
;;; The optional parameter ignore-previous-bindings tells the function to set
;;; the value of the Tspin tokens even if they are set already.
;;;
(defun instantiate-frame-tokens (&optional ignore-previous-bindings)
  (format
    *tspin-stream*
    "~%Instantiating frame tokens for each object in Tale-Spin's world.~%~%")
  (dolist (each-token 
	    (append *cast* 
		    *objects* 
		    (remove 'outside1 
			    *all-locations*)
		    *components* 
		    ))
    (if (or
	  (not (boundp each-token))
	  ignore-previous-bindings)
	(import
	  ;; NOTE that objects come before components in the dolist variable list.
	  ;; Must stay that way (including *all-locations* because of cupboard-door1 etc)
	  ;; We are also assuming that for every
	  ;; component-of property
	  ;; there exists a complementary component property.
	  (link-component-with-whole
	    each-token
	    (setf (symbol-value each-token)
		  (make-token each-token)))	  
	  *target-package*)))
  ;; Now handle outside1 specially.
  (setf outside1
	(f.instantiate-frame
;;; 		   outside
;;; 	  location				; This does not work either. 
	  door					; So let's equate outside with the house door. [cox 2feb95]
	  *story-instance*))
  (f.put-all!
    (f.instantiate-frame
      kitchen
      *story-instance*)
    outside1
    *co-domain-slot*)
  (setf ?unspec *nil*)
  (setf future 'future.0)
  (import outside1 *target-package*)
  )


;;;
;;; The Tale-Spin program does not represent many object feature very well. For
;;; example, component properties are used to represent many different
;;; features, that is, the component and its inverse, component-of, are heavily
;;; overloaded. The link-component-with-whole function is use to map these
;;; various attributes to more proper attributes in the frame representation as
;;; used by Meta-AQUA.
;;;
;;; If there is no component property, the function returns the input value of
;;; new-token. If it has a component property it performs the transform as
;;; side-effect and returns the original value of the new-token. 
;;; 
(defun link-component-with-whole (each-token new-token)
  (if (get each-token 'component-of)
      (cond ((isa-p 'hand (list new-token))
	     (f.unify				; Add hand as body-part of person.
	       (f.get (f.unify			; Add the person as body-part-of of hand.
			(symbol-value
			  (get each-token
			       'component-of))
			(f.get new-token
			       'body-part-of))
		      'body-part)
	       new-token))
	    ((isa-p 'peel (list new-token))
	     (f.unify				; Add peel as component of food (orange or banana).
	       (f.get (f.unify			; Add the food as component-of of peel.
			(symbol-value
			  (get each-token
			       'component-of))
			(f.get new-token
			       'component-of))
		      'component)
	       new-token))
	    ((or (isa-p 'handle (list new-token))
		 (isa-p 'switch (list new-token))
		 (isa-p 'receiver (list new-token)))	; Phones are included here also.
	     (f.unify				; Add handle/switch as actuator of faucet/electrical-device.
	       (f.get (f.unify			; Add faucet/light/dorr-bell as actuator-of of handle/switch.
			(symbol-value
			  (get each-token
			       'component-of))
			(f.get new-token
			       'actuator-of))
		      'actuator)
	       new-token))
	    ((isa-p 'door (list new-token))
	     (f.unify				; Add door as door-for of closed-container.
	       (f.get (f.unify			; Add the fridge/cupboard as door-for-of of door.
			(symbol-value
			  (get each-token
			       'component-of))
			(f.get new-token
			       'door-for-of))
		      'door-for)
	       new-token))
	    (t
	     (format *tspin-stream* "~%ERROR: in function link-component-with-whole.")
	     (break)
	     new-token))
      new-token)
      )


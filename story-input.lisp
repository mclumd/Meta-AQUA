;;; -*- Mode: LISP; Syntax: Common-lisp; Package: Meta-aqua; Base: 10 -*-

(in-package :metaaqua)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	    The Meta-AQUA Introspective Multistrategy Learning System
;;;;				   Version 6
;;;;
;;;;	     Copyright (C) 1996   Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;			    File: story-input.lisp
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
;;     STORY INPUT VARIABLES AND FUNCTIONS 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;
;;; This file contains the hand-crafted input story examples that Meta-AQUA
;;; uses for processing. Since natural language processing is not the research
;;; focus of this dissertation, I assume text parsing and directly input
;;; simple conceptual representations (in frame form). See also the Tale-Spin
;;; module for story input that, while still in conceptual rather than text
;;; form, is less contrived.
;;; 

;;;
;;; Function init-story is used to establish the story that Meta-AQUA is to
;;; read. It is called before the Meta-AQUA function is called. For example, it
;;; can be used after running story-4 to set the story back to the initial
;;; example without calling reset-memory. Thus, (init-story *init*) will put
;;; the original example back without changing the modified memory structures
;;; learning during previous runs of the program.
;;;
;;; Made function parameter optional. So call of simply (init-story) now
;;; accomplished above. [cox 7feb95]
;;; 
(defun init-story (&optional (story-definitions *init*))
  (setf *Story-Concepts* story-definitions)
  )



;;; 
;;; The default input story for the read-story mode of Meta-AQUA is the
;;; original drug-bust scenario reported in MSL-91 and further publications.
;;; The dog barks at the luggage seems anomalous. Meta-AQUA explains that the
;;; luggage somehow threatened the dog. Later it is told that the dog barked
;;; because it detected drugs. Because it expected one explanation to be true,
;;; whereas another was the cause, it needs to explain this reasoning failure.
;;; The errors in its memory are 1. overly constrained definition of dog-barks,
;;; 2. mis-indexed threatened XP, 3. missing detection XP, and, of course, 4.
;;; missing index on the missing XP.
;;; 
(defvar *init*
	'(((sniff 
	     (actor (value (dog)))
	     (instrumental-scene (value (attend 
					  (to (value (luggage))))))
	     )
	   "Dog sniffs at passenger's baggage in airport.")

	  ((dog-barks 
	     (actor (value (dog))
		    (relation (actor
				(domain (value =self))
				(co-domain (value =actor)))))
	     (object (value (luggage)))
	     (to (value (at-location
			  (domain (value =object)))))
	     (instrumental-scene (value (speak
					  (actor (value =actor)))))
	     )
	   "Dog barks at passenger's baggage.")

	  ((arrest
	     (actor (value (authority)))
	     (object (value (person)))
	     (charge (value (smuggling-crime)))
	     )
	   "Authorities arrest passenger for smuggling drugs.")

	  ((self-cause
	     (actor1 (value (dog)))
	     (conseq (value (dog-barks 
				  (actor (value =actor1))
				  (object (value (luggage =l)))
				  (to (value (at-location
					       (domain (value =l)))))
				  (instrumental-scene (value (speak
							       (actor (value =actor1))))))))
	     (ante (value (detection
				  (actor (value =actor1))
				  (object (value (drug =d)))
				  (item-location (value (inside-container
							  (domain (value =d))
							  (co-domain (value =l))))))))
     
	     )
	   "The dog barked because he detected the drugs.")
	  ))


;;;
;;; Non-working scenario.
;;; 
(defun set-story-2 ()
(setf  *Story-Concepts*
'((
   (sniff 
     (actor (value dog))
     (instrumental-scene (value (attend 
				  (to (value luggage))))))
   "Dog sniffs at passenger's baggage in airport.")

  ((arrest
     (actor (value authority))
     (object (value person))
     (charge (value smuggling-crime)))
   "Authorities arrest passenger for smuggling drugs.")
  )))



;;;
;;; Non-working scenario.
;;; 
(defun set-story-3 ()
(setf *Story-Concepts*
'((
   (arrest
     (actor (value authority))
     (object (value person))
     (charge (value smuggling-crime)))
   "Authorities arrest passenger for smuggling drugs.")
  ((mtrans
     (actor (value (authority)))
     (mobject (value (odor
		       (domain (value (drug)))
		       (co-domain (value burning-rope-odor.0)))))
     (from (value (at-location
		    (domain (value =mobject))
		    (co-domain (value =actor)))))
     (to   (value (at-location
		    (domain (value =mobject))
		    (co-domain (value (authority =a))))))
     (instrument (value (speak
			  (actor (value =actor)))))
     (main-result (value (knowledge-state
			   (domain (value =a))
			   (co-domain
			     (value (belief
				      (believed-item (value =mobject)))))
		   )))
     )
   "One authority tells another that the drug smells like burning rope.")
  )))



;;;
;;; After default scenario above is run, this scenario demonstrates that a
;;; similar story will not result in the same errors. Instead of the dog
;;; barking at a passenger's luggage at the airport, the dog barks a a garbage
;;; pail in a suspect's house.
;;; 
(defun set-story-4 ()
 (init-story
'(
  ((ptrans 
     (actor (value (authority)))
     (object (value (authority)))
     (from (value (at-location
		    (domain (value =object))
		    (co-domain (value
				 (outside
				   (domain (value =object))
				   (co-domain (value (house =h)))))))))
     (to (value (at-location
		    (domain (value =object))
		    (co-domain (value
				 (inside
				   (domain (value =object))
				   (co-domain (value (house =h)))))))))
     )
   "Policeman enters the house of the suspect.")

  ((ptrans 
     (actor (value (dog)))
     (object (value (dog)))
     (from (value (at-location
		    (domain (value =object))
		    (co-domain (value
				 (outside
				   (domain (value =object))
				   (co-domain (value (house =h)))))))))
     (to (value (at-location
		    (domain (value =object))
		    (co-domain (value
				 (inside
				   (domain (value =object))
				   (co-domain (value (house =h)))))))))
     )
   "Dog enters the house.")

  ((dog-barks  
     (actor (value (dog))
	    (relation (actor
			(domain (value =self))
			(co-domain (value =actor)))))
     (object (value (pail)))
     (to (value (at-location
		  (domain (value =object)))))
     (instrumental-scene (value (speak
				  (actor (value =actor)))))
     )
   "Dog barks at suspect's garbage pail.")

  ((arrest
     (actor (value (authority)))
     (object (value (person)))
     (charge (value (smuggling-crime)))
     )
   "Authorities arrest suspect for drugs.")

  ((self-cause
     (actor1 (value (dog)))
     (conseq (value (dog-barks 
			  (actor (value =actor1))
			  (object (value (pail =l)))
 			  (to (value (at-location
				       (domain (value =l)))))
			  (instrumental-scene (value (speak
						       (actor (value =actor1))))))))
     (ante (value (detection
			  (actor (value =actor1))
			  (object (value (drug =d)))
			  (item-location (value (inside-container
						  (domain (value =d))
						  (co-domain (value =l))))))))
     
     )
   "The dog barked because he detected the drugs.")
  ))
 t)


;;;
;;; Story of the forgotten detection explanation. Unlike story-4, Meta-AQUA
;;; will not find the detection explanation because it is indexed by
;;; dogs-barking-at-containers. The threaten explanation is indexed under
;;; dogs-barking-at-animate-objects, so no explanations are retrieved;
;;; Meta-AQUA is baffled and forgets the proper explanation. When it tries to
;;; store the explanation inferred from the last concept, it finds the
;;; explanation created during the default scenario. It generalizes the two and
;;; indexes the result under dogs-barking-at-hiding-objects.
;;; 
(defun set-story-5 ()
 (init-story
'(
  ((ptrans 
     (actor (value (authority)))
     (object (value (authority)))
     (from (value (at-location
		    (domain (value =object))
		    (co-domain (value
				 (outside
				   (domain (value =object))
				   (co-domain (value (house =h)))))))))
     (to (value (at-location
		    (domain (value =object))
		    (co-domain (value
				 (inside
				   (domain (value =object))
				   (co-domain (value (house =h)))))))))
     )
   "Policeman enters the house of the suspect.")

  ((ptrans 
     (actor (value (dog)))
     (object (value (dog)))
      (from (value (at-location
		    (domain (value =object))
		    (co-domain (value
				 (outside
				   (domain (value =object))
				   (co-domain (value (house =h)))))))))
     (to (value (at-location
		    (domain (value =object))
		    (co-domain (value
				 (inside
				   (domain (value =object))
				   (co-domain (value (house =h)))))))))
     )
   "Dog enters the house.")

  ((dog-barks 
     (actor (value (dog))
	    (relation (actor
			(domain (value =self))
			(co-domain (value =actor)))))
     (object (value (laundry-pile)))
     (to (value (at-location
		  (domain (value =object)))))
     (instrumental-scene (value (speak
				  (actor (value =actor)))))
     )
   "Dog barks at suspect's laundry pile.")

  ((under 
     (domain (value (drug)))
     (co-domain (value (laundry-pile)))
     )
   "Drugs were under the laundry pile.")

  ((arrest
     (actor (value (authority)))
     (object (value (person)))
     (charge (value (smuggling-crime)))
     )
   "Authorities arrest suspect for drugs.")

  ((self-cause
     (actor1 (value (dog)))
     (conseq (value (dog-barks 
			  (actor (value =actor1))
;;; 			  (object (value (laundry-pile =l)))
			  ;; Note that the object MUST be a supernode of container to unify with the
			  ;; previously stored self-cause from story-4.
			  (object (value (hiding-object =l)))
 			  (to (value (at-location
				       (domain (value =l)))))
			  (instrumental-scene (value (speak
						       (actor (value =actor1))))))))
     (ante (value (detection
			  (actor (value =actor1))
			  (object (value (drug =d)))
;			  (item-location (value (under
;						  (domain (value =d))
;						  (co-domain (value =l)))))
			  )))
     
     )
   "The dog barked because he detected the drugs.")
  ))
 t)


;;; 
;;; Janis' story that parallels the dog barking story in the domnain of
;;; hitting. Conceptual definition of hit has object slot constrained to be
;;; animate. Has a mis-indexed explanation that people hit to inflict pain.
;;; Novel situation is that people hit balls (toys) to have fun.
;;; 
(defun set-story-6 ()
  (init-story
	'(
	  ((ptrans
	     (actor (value (person)))
	     (object (value =actor))
	     (from (value (at-location
			    (domain (value =object))
			    (co-domain (value 
					 (outside
					   (domain (value =actor))
					   (co-domain (value (handball-court =h)))))))))
	     (to (value (at-location
			  (domain (value =object))
			  (co-domain (value 
				       (inside
					 (domain (value =object))
					 (co-domain (value (handball-court =h)))))))))

	     )
	   "Person enters the handball-court.")

	  ((hit
	     (actor (value (person))
		    (relation (actor
				(domain (value =self))
				(co-domain (value =actor)))))
	     (object (value (ball)))
	     (to (value (at-location
			  (domain (value =object)))))
	     (instrumental-scene 
	       (value (propel
			(object (value (hand =h
					     (part-of (value =actor)))))
			(from (value (at-location (domain (value =h)))))
			(to (value (at-location (domain (value =h))
						(co-domain (value (physical-location
								    (location-of (value =object)))))))))))
	     )
	   "Person hits the ball.")

	  ((self-cause
	     (actor1 (value (person)))
	     (conseq
	       (value
		 (hit
		   (actor (value =actor1))
		   (object (value (ball =b)))
		   (instrumental-scene
		     (value (toy-propel
			      (actor (value =actor1))
			      (object (value (hand
;;; 					       =h
					       )))
			      (from (value
				      (at-location
;;; 					(domain (value =h))
					)))
			      (to (value
				    (at-location
;;; 				      (domain (value =h))
				      (co-domain
					(value (physical-location
						 (location-of (value =b))))))))))))))
	     (ante
	       (value
		 (have-fun
		   (actor (value =actor1)))))
	     )
	   "The person hit the ball because he wanted to have fun.")
	  ))
  t)



;;;
;;; A story to test the learning performed in story 5.
;;; 
(defun set-story-7 ()
 (init-story
'(
  ((at-location 
     (domain (value (person)))
     (co-domain (value (outside
			 (domain (value =domain))
			 (co-domain (value (house))))))
     )
   "A person was outside a house.")

  ((ptrans 
     (actor (value (authority)))
     (object (value (authority)))
     (from (value (at-location
		    (domain (value =object))
		    (co-domain (value
				 (on
				   (domain (value =object))
				   (co-domain (value (street)))))))))
     (to (value (at-location
		    (domain (value =object))
		    (co-domain (value
				 (near
				   (domain (value =object))
				   (co-domain (value (person)))))))))
     )
   "Policeman approaches the suspect.")

  ((ptrans 
     (actor (value (dog)))
     (object (value (dog)))
     (from (value (at-location
		    (domain (value =object))
		    (co-domain (value
				 (on
				   (domain (value =object))
				   (co-domain (value (street)))))))))
     (to (value (at-location
		    (domain (value =object))
		    (co-domain (value
				 (near
				   (domain (value =object))
				   (co-domain (value (authority)))))))))
     )
   "The dog follows.")

  ((mtrans
     (actor (value (authority)))
     (object (value (near
		      (domain (value (person)))
		      (co-domain (value (compost-pile =cpile))))))
     (instrumental-scene (value (attend
				  (actor (value =actor))
				  (object (value (eyes
						   (part-of (value =actor)))))
				  (from (value =cpile)))))
     (main-result (value (knowledge-state
			   (domain (value =actor))
			   (co-domain (value =object))))))
   "The policeman sees that the person is near a compostpile.")

  ((dog-barks 
     (actor (value (dog))
	    (relation (actor
			(domain (value =self))
			(co-domain (value =actor)))))
     (object (value (compost-pile)))
     (to (value (at-location
		  (domain (value =object)))))
     (instrumental-scene (value (speak
				  (actor (value =actor)))))
     )
   "The dog barks at the compost pile.")

  ((under 
     (domain (value (drug)))
     (co-domain (value (compost-pile)))
     )
   "Drugs were under the compost pile.")

  ((arrest
     (actor (value (authority)))
     (object (value (person)))
     (charge (value (smuggling-crime)))
     )
   "Authorities arrest suspect for drugs.")

  ((self-cause
     (actor1 (value (dog)))
     (conseq (value (dog-barks 
			  (actor (value =actor1))
;;; 			  (object (value (compost-pile =l)))
			  ;; Note that the object MUST be a supernode of container to unify with the
			  ;; previously stored self-cause from story-4.
			  (object (value (hiding-object =l)))
 			  (to (value (at-location
				       (domain (value =l)))))
			  (instrumental-scene (value (speak
						       (actor (value =actor1))))))))
     (ante (value (detection
			  (actor (value =actor1))
			  (object (value (drug =d)))
;			  (item-location (value (under
;						  (domain (value =d))
;						  (co-domain (value =l)))))
			  )))
     
     )
   "The dog barked because he detected the drugs.")
  ))
 t)

;;;A story to test that the program did not forget the original hit explanation,
;;; XP-INJURY-HIT
;;; Added by Janis

(defun set-story-8 ()
(init-story
'(
  ((ptrans
     (actor (value (person)))
     (object (value =actor))
     (from (value (at-location 
		    (domain (value =object))
		    (co-domain (value (physical-location
					(location-of (value =actor))))))))
     (to (value (at-location
		  (domain (value =object))
		  (co-domain (value (physical-location
				      (location-of (value (person)))))))))
     )
   "Person walks over to actor1.")

  ((hit
     (actor (value (person)))
     (object (value (person
		      (at-location (value =l)))))
     (to (value (at-location
		  (domain (value =object)))))
     (instrumental-scene
       (value (propel
		(object1 (value (hand =h
				      (body-part-of (value =actor)))))
		(from (value (at-location
			       (domain (value =h))
			       (co-domain (value (physical-location
						   (location-of (value =actor))))))))
		(to (value (at-location
			     (domain (value =h))
			     (co-domain (value (physical-location =l
								  (location-of (value =object)))
					       ))))))))
     (scenes (value (=instrumental-scene)))
     )
   "Person hits actor1.")

  ((xp-injury-hit
	 (actor (value (person)))
	 (injured-object (value (person)))
	 (hitting (value (hit)))
	 (cause-pain (value (hurt)))
	 )
       "The person hit the other person in order to cause pain.")
  )
))


;;; 
;;; story to test an index created by the program
;;; added by janis
;;; 
(defun set-story-9 ()
  (init-story
    '(
      ((ptrans
	 (actor (value (person)))
	 (object (value =actor))
	 (from (value (at-location 
			(domain (value =object))
			(co-domain (value (physical-location
					    (location-of (value =actor))))))))
	 (to (value (at-location
		      (domain (value =object))
		      (co-domain (value (physical-location
					  (location-of (value (lamp)))))))))
	 )
       "person walks over to the lamp.")
 
      ((hit
	 (actor (value (person))
		(relation (actor
			    (domain (value =self))
			    (co-domain (value =actor)))))
	 (object (value (lamp)))
	 (to (value (at-location
		      (domain (value =object)))))
	 (instrumental-scene 
	   (value (propel
		    (object (value (hand =h
					 (part-of (value =actor)))))
		    (from (value (at-location (domain (value =h)))))
		    (to (value (at-location (domain (value =h))
					    (co-domain (value (physical-location
								(location-of (value =object)))))))))))
	 )
;       (hit
;	 (actor (value (person)))
;	 (object (value (lamp
;			  (at-location
;			    (value (physical-location =l
;						      (location-of (value =object))))))))
;	 (to (value (at-location
;		      (domain (value =object)))))
;	 (instrumental-scene
;	   (value (propel
;		    (object (value (hand =h
;					 (body-part-of (value =actor)))))
;		    (from (value (at-location
;				   (domain (value =h))
;				   (co-domain (value (physical-location
;						       (location-of (value =actor))))))))
;		    (to (value (at-location
;				 (domain (value =h))
;				 (co-domain (value =l))))))))
;	 (scenes (value (=instrumental-scene)))
;	 )
       "person hits the lamp.")	

      (
;       (cause
;	 (actor1 (value (person)))
;	 (conseq
;	   (value (hit
;		    (actor (value =actor1))
;		    (object (value (lamp =lamp1)))
;		    (to (value (at-location
;				 (domain (value =lamp1)))))
;		    (instrumental-scene
;		      (value (propel
;			       (actor (value =actor1))
;			       (object (value (hand)))
;			       (from (value
;				       (at-location)))
;			       (to (value
;				     (at-location
;				       (co-domain
;					 (value (physical-location
;						  (location-of (value =lamp1))))))))))))
;;		  (hit
;;		    (actor (value =actor1))
;;		    (object (value (lamp =lamp1
;;					 (at-location (value =l)))))
;;		    (to (value (at-location
;;				 (domain (value =object)))))
;;		    (instrumental-scene
;;		      (value
;;			(propel
;;			  (object1 (value (hand =h
;;						(body-part-of (value =actor)))))
;;			  (from (value
;;				  (at-location
;;				    (domain (value =h))
;;				    (co-domain (value
;;						 (physical-location
;;						   (location-of (value =actor))
;;						   ))))))
;;			  (to (value (at-location
;;				       (domain (value =h))
;;				       (co-domain (value
;;						    (physical-location =l
;;								       (location-of
;;									 (value =object)))
;;						    )))))))))
;		  ))
;	     
;	 (ante
;	   (value (break-object
;		    (actor (value =actor1))
;		    (object (value =lamp1))
;		    (instrumental-scene
;		      (value (propel
;			       (actor (value =actor1))
;			       (object
;				 (value (hand =h
; 					  (body-part-of (value =actor1))
;					      )))
;			       (from
;				 (value (at-location
;					  (domain (value =h))
;					  (co-domain
;					    (value (near
;						     (domain
;						       (value =h))
;						     (co-domain
;						       (value =actor1))
;						     ))))))
;			       (to (value
;				     (at-location
;				       (domain (value =h))
;				       (co-domain (value
;						    (physical-location 
;						      (location-of
;							(value =lamp1))
;						      ))))))))))))
;	 )
;       (xp-goal-of-outcome->actor
;	 (actor (value (person)))
;	 (object (value (lamp)))
;	 (action
;	   (value (hit
;		    (actor (value =actor)
;			   (relation =role))
;		    (object (value
;			      =object
;;;; 			      (lamp =lamp1)
;			      ))
;		    (to (value (at-location
;				 (domain (value =object)))))
;		    (instrumental-scene
;		      (value (propel
;			       (actor (value =actor))
;			       (object (value (hand)))
;			       (from (value
;				       (at-location)))
;			       (to (value
;				     (at-location
;				       (co-domain
;					 (value (physical-location
;						  (location-of (value =object)))))))))))
;		    (main-result (value =good-state)
;				 (relation =main-result)))))   ;; should be any result, not just the main-result.
;	 (role
;	   (value (actor
;		    (domain (value =action))
;		    (co-domain (value =actor)))))
;	 (good-state
;	   (value (physical-state
;		    (domain (value =object))
;;;; 		    (co-domain (value broken.0))
;;;; 		    (main-result- (value =action))
;		    )))
;	 (main-result
;	   (value (main-result
;		    (domain (value =action))
;		    (co-domain (value =good-state)))))
;	 (goal
;	   (value (achievement-goal
;		    (goal-actor (value =actor))
;		    (goal-object (value =good-state)))))
;	 (plan-choice
;	   (value (plan-selection
;		    (actor (value =actor))
;		    (plan (value =action))
;		    (goal (value =goal))
;		    (role (value =role)))))
;	 (pre-xp-nodes       (value (=actor =action =role =good-state =main-result)))
;	 (explains           (value =role))
;	 (xp-asserted-nodes  (value (=main-result =goal)))
;	 (internal-xp-nodes  (value (=plan-choice)))
;	 (links (value (=link1)))
;	 (link1
;	   (value (mentally-results
;		    (domain    (value =plan-choice))
;		    (co-domain (value =role)))))) 
       (xp-goal-of-outcome->actor
	 (actor (value (person)))
	 (object (value (lamp)))
	 (conseq
	   (value (hit
		    (actor (value =actor))
		    (object (value =object)))))
	 (role
	   (value (actor
		    (domain (value =conseq))
		    (co-domain (value =actor)))))
	 (ante
	   (value (physical-state
		    (domain (value =object))
		    (co-domain (value broken.0)))))
	 )

       "person hit the lamp because he wanted it broken.")
      )
    ))




;;;
;;; Story 10 should always be explainable, both before learning from the
;;; initial story (even though the index is overly-general) and after the
;;; learning in the initial story or further stories. [cox 16feb95]
;;; 
(defun set-story-10 ()
  (init-story
    '(
      ((sniff 
	 (actor (value (dog)))
	 (instrumental-scene (value (attend 
				      (to (value (person))))))
	 )
       "The dog sniffs at a person.")

      ((propel
	 (actor (value (person)))
	 (object (value (ball)))
	 (from (value (at-location 
			(domain (value =object))
			)))
	 (to (value (at-location
		      (domain (value =object))
		      (co-domain (value (near
					  (domain (value =object))
					  (co-domain (value (dog)))))))))
	 )
       "The person throws a ball at the dog.")
 
      ((dog-barks 
	 (actor (value (dog))
		(relation (actor
			    (domain (value =self))
			    (co-domain (value =actor)))))
	 (object (value (person)))
	 (to (value (at-location
		      (domain (value =object)))))
	 (instrumental-scene (value (speak
				      (actor (value =actor)))))
	 )
       "The dog barks at the person.")

      ((xp-defensive-bark
	 (actor (value (dog)))
	 (object (value (person)))
	 (barking (value 
		    (dog-barks)))
	 (threatening-action (value (propel
				      (actor (value =object)))))
	 )
       "The dog barked becasue he was threatened by the person throwing the ball.")
      )
    ))


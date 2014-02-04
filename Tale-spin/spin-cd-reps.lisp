;;; -*- Mode: LISP; Syntax: Common-lisp; Package: TALE-SPIN; Base: 10 -*-


(in-package :tspin)

;;; ===========================================================================
;;; TaleSpin
;;; Copyright (C) 1987 James R. Meehan
;;; ===========================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	    Elvis World and the Tale-Spin Story Generation Subsystem
;;;;				 for Meta-AQUA
;;;;
;;;;	              Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;			    File: spin-cd-reps.lisp
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
;;; TALE-SPIN CD REPRESENTATIONS  [cox 31jul93]
;;;
;;; All primitive acts (such as ingest) have a defun which provides their slot
;;; filler representation (in spin-cd-reps.lisp), a def-conseq(uence)s (in
;;; spin-cd-reps.lisp) which specifies the inferences that follow from the act,
;;; and a dsp definition (in mumble.lisp) that defines how to generate an
;;; English paraphrase.  The verbs associated with the act have their congugates
;;; specified in a v-congugate declaration (in verbs.lisp).  Other acts (such as
;;; inflate) have procedural code specified by a defun that call other
;;; primitives.
;;; 
;;; It appears that some states (but why some and not others I am not sure) have
;;; a def-reaction defining the reactions agents have when discovering that the
;;; state exists. Many states do not have an apparent definition, whereas other
;;; states (such as happy) are defined in terms of further states (such as bored
;;; mode negative). Note that some states (e.g., mloc have consequences too),
;;; though others have had their consequences commented out (e.g., loc). Many of 
;;; the states such as ring and burning have dsp specifications also.
;;; 
;;; Goals are special and have a procedural definition provided by a defun (in
;;; spin-cd-reps.lisp).


;;; Note that I have rearranged the order of these representation definitions.
;;; Now cd defuns, def-preconds, def-conseqs and def-reactions all go together.
;;; Probably should place dsp functions along with them but have left them in
;;; mumble.lisp. Previously, all cd defuns were together, all def-conseqs, etc.
;;;



(defun s-jonesing (actor
		   &aux
		   (main-action nil)
		   (goal (state actor 'jonesing 'neg)))
  (do-break  s-jonesing )
  (goal-eval
    actor 
    goal
    (if (equal actor 'elvis)
	(a 
	  marijuana
	  drug (knows-loc actor drug)
	  (a pipe pipe (knows-loc actor pipe)
	     (a lighter lighter (knows-loc actor lighter)
		(list
		  (make-plan 
		    `(s-jonesing ,actor)
		    (cond
		      (*force-original-demo*
		       (force-event '(k-9-squad-arrive
				       officer1 police-dog1))
		       (setf *sniff-success* 100)
		       (dcont actor 'lighter1))
		      (t
		       (and
			 (dfill actor pipe drug)
			 (if (or
			       *no-spills*
			       (random-choice 
				 (/ 9.1 (get actor 'age))
				 t nil))
			     (and
			       (dlight actor drug lighter)
			       (doit (setf main-action
					   (ingest actor drug pipe)))
			       (doit (expel actor 'smoke1 'air1)))
			     (block nil
			       (doit (propel actor pipe 'floor1))
			       nil)))
		       (and (is-true? (state pipe 'dirty 'pos))
			    (daccess actor 'hot-faucet t)
			    (doit (ptrans actor pipe 'hot-faucet nil))
			    (daccess actor 'hot-faucet nil))
		       )))
		  ))))
	(a 
	  tobacco
	  drug (knows-loc actor drug)
	  (a pipe pipe (knows-loc actor pipe)
	     (a lighter lighter (knows-loc actor lighter)
		(list
		  (make-plan 
		    `(s-jonesing ,actor)
		    (cond
		      (*force-original-demo*
		       (force-event '(k-9-squad-arrive
				       officer1 police-dog1))
		       (setf *sniff-success* 100)
		       (dcont actor 'lighter1))
		      (t
		       (and
			 (dfill actor pipe drug)
			 (if (or
			       *no-spills*
			       (random-choice 
			       (/ 9.1 (get actor 'age))
			       t nil))
			     (and
			       (dlight actor drug lighter)
			       (doit (setf main-action
					   (ingest actor drug pipe)))
			       (doit (expel actor 'smoke1 'air1)))
			     (block nil
			       (doit (propel actor pipe 'floor1))
			       nil)))
		       (and (is-true? (state pipe 'dirty 'pos))
			    (daccess actor 'hot-faucet t)
			    (doit (ptrans actor pipe 'hot-faucet nil))
			    (daccess actor 'hot-faucet nil))
		       )))
		  ))))))
  (if (knows? 'world goal)
      (assert-fact				; Added the explanation [cox 16feb95]
	(mloc 'world
	      (xp-goal-of-outcome->actor
		actor main-action goal)))))


;;; 
;;; Occasionally, Elvis will pester the dog if bored 
;;; This allows dog to feel "threatened" by an animate object
;;; [mdd 3apr94]
;;; 
(defun s-irritate-dog (actor dog)
  (goal-eval
    actor 
    (state actor 'bored 'neg)
    (a ball ball (knows-loc actor ball)
       (list
	 (make-plan 
	   `(s-irritate-dog ,actor)
	   (let ((antecedent (propel actor ball dog))
		 (consequent (dog-barks dog actor)))
	     (and (doit antecedent)
		  (doit consequent)
		  (assert-fact			; Added the explanation [cox 16feb95]
		    (mloc 'world
			  (xp-defensive-bark	; Changed to 
			    dog			; xp-defensive-bark from cause [cox 28feb95]
			    actor
			    consequent
			    antecedent)
			  ))))))
       )))


;;;
;;; Added goal, main-action, and the explanation. [cox 28feb95]
;;; 
(defun s-thirst (actor
		 &aux
		 (main-action nil)
		 (goal (state actor 'thirsty 'neg)))
  (do-break  s-thirst )
  (goal-eval actor 
	     goal
             (a drink drink (knows-loc actor drink)
                (a cup cup (knows-loc actor cup)
                   (list
		     (make-plan
		       `(s-thirst ,actor)
		       (and
			 (dfill actor cup drink); removed dropping [mdd 4apr94]
			 (doit			; (random-choice (/ 2.1 (get actor 'age))
			   (setf main-action
				 (ingest actor drink cup))
						;    (propel actor cup 'floor1)))
			   )
			 (and (is-true? (state cup 'dirty 'pos))
			      (daccess actor 'hot-faucet t)
			      (doit (ptrans actor cup 'hot-faucet nil))
			      (daccess actor 'hot-faucet nil))
			 (if (knows? 'world goal)
			     (assert-fact	; Added the explanation [cox 16feb95]
			       (mloc 'world
				     (xp-goal-of-outcome->actor
				       actor main-action goal))))
			 )))))))

;;; What does Elvis do if bored? There is a twenty percent chance that 
;;; he would do the same actions as if jonesing. Added initial if to cover 
;;; this possibility. [cox] 
;;;
;;; Random chance for actor to pester the dog if they are bored [mdd 3apr94]
(defun s-entertain (actor)
  (cond ((and (eq actor 'elvis) (< (random 100) *high-4-entertainment*))
	 (assert-fact
	   (mloc 'world
		 (state actor 'jonesing 'pos) ))
	 ;; (s-jonesing actor) would not work above. [cox 9jan95]
	 ) 
	((< (random 100) *annoy-dog-freq*)	; Changed <= to < [cox 9jan95]
	 (if *TDebug-On*
	     (format *tspin-stream* "###"))
	 (s-irritate-dog actor 'dog1))
    (t (goal-eval actor 
		  (state actor 'bored 'neg)
		  (if
		    (and (not *force-balloon-throwing*)	; Added for function specify-yarns [cox 16jan95}
			 (or *force-ball-throwing*	; Added for function specify-yarns [cox 16jan95}
			     (>= (random 100)
				 50)))
		      (a ball ball (knows-loc actor ball)
			 (let((friend (if (eq actor 'karen)
					  'lynn
					  'karen)))
                    (list (make-plan `(s-entertain ,actor)
                                     (and 
                                      (doit (mtrans actor
						    (question
						      (propel friend ball actor friend))
						    friend actor))
                                      (doit (ptrans friend friend 'garage 'kitchen))
                                      (doit (grasp friend ball))
                                      (doit (ptrans friend friend 'outside 'garage))
                                      (doit (ptrans actor actor 'outside 'kitchen))
                                      (doit (play actor ball))
                                      (play-catch friend actor ball))))))
	       (a balloon balloon (knows-loc actor balloon)
		  (let((friend (if (eq actor 'karen)
				   'lynn
				 'karen))
		       (adult (random-element '(mom dad))))
		    (list (make-plan `(s-entertain ,actor)
				     (and
				      (doit (mtrans actor 
						    (question
						      (propel friend balloon actor friend)) 
						    friend actor))
				      (doit (mtrans actor
						    (question
						      (atrans adult balloon actor adult)) 
						    adult actor))
                                      (dcont adult balloon)
				      (inflate adult balloon)
				      (doit (atrans adult balloon actor adult))
				      (doit (ptrans actor actor 'outside 'kitchen))
				      (doit (ptrans friend friend 'outside 'kitchen))
                                      (doit (play actor balloon))
				      (play-catch friend actor balloon)))))))))))


(defun happy(p1)
  (do-break  happy)
  (state p1 'bored 'neg))

(defun inflate (a b)
  (do-break  inflate )
  (doit (expel a 'air2 b))
  (cond ((> 4 (random 11))
         (doit (tie a b)))
        (t (doit (ungrasp a b))
           (doit (grasp a b))
           (inflate a b))))

;(defun temp (number)
;  (setf *ALL* nil)
;  (do ((count number (- count 1))
;       (pitcher (cdpath '(val)
;			(knows? 'world (who-has 'ball1))(knows? 'world (who-has 'ball1))))) 
;      ((equal count 0) (print "Done"))
;    (play-catch pitcher
;		(first (remove 'pitcher '(lynn karen)))
;		'ball1)
;    (terpri)
;    (print count)
;    (terpri)
;     )
;  )


;;; Playing catch is a recursive affair. Commented [cox 1jan95]
;;;
;;; The local variable original-player was added in order to represent the
;;; person who first threw the obj. The reason is that if there is a question
;;; posed by Meta-AQUA, then it will be why did the original player throw the
;;; ball. We do not want to subsequently provide the reason why the other
;;; person is throwing. Meta-AQUA will not understand, although it will find
;;; the explanation relevant. [cox 26jun95]
;;; 
(defun play-catch (p1 p2 obj &optional (n 1))
  (do-break  play-catch)
  (cond
    ;; End on a happy note. Note that actors must have thrown at least once.
    ((and (> n 2)
	  (< (random 10) n))
     (doit (happy p1))
     (doit (happy p2))
     (let ((original-player
	     (if (oddp n) p2 p1)))
       (assert-fact
	 (mloc
	   'world
	   (xp-goal-of-outcome->actor
	     original-player
	     (play original-player obj)
	     (happy original-player))
;;;  	     (results-in (play p1 obj)		; Changed to xp-goal-of-outcome->actor [cox 28feb95]
;;; 	                 (happy p1))
	   ))))
    ;; Actor p1 throws it poorly.
    ((>= (random 100) 58)
     (doit (propel p1 obj
		   (random-element
		     '(calla-lilly1
			calla-lilly2 window1
			rose-bush1 rose-bush2
			window1 window1))))
     (cond
       ;; If the object broke, then go back inside without becoming happy.
       ((is-true? (state obj 'bursted 'pos))
	(doit (ptrans p2 p2 'kitchen 'outside))
	(doit (ptrans p1 p1 'kitchen 'outside))
	nil)
       ;; Otherwise  actor p2 picks up the object, and plays some more catch.
       (t (doit (grasp p2 obj))
	  (play-catch p2 p1 obj (+ n 1)))))
    ;; Actor p1 throws it right.
    (t (cond ((is-a obj 'ball)
	      (doit (hit p1 obj))		; Recreating the hit scenario
	      (doit (xp-instrumental-scene->actor  ; with a causal explanation. [cox 26-27jun95]
			 p1
			 (hit p1 obj)
			 (ptrans p1 obj p2 p1))
		    ))
	     (t (doit (propel p1 obj p2 p1))))
       (cond
	 ;; but p2 may drop it in the grass.
	 ((and (> n 2)
	       (>= (random 100) 50))
	  (doit (propel p2 obj 'grass p2))
	  ;; If it breaks in the grass, however, we are happy.
	  (cond
	    ((is-true? (state obj 'bursted 'pos))
	     (doit (happy p1))
	     (doit (happy p2))
	     (let ((original-player
		     (if (oddp n) p2 p1)))
	       (assert-fact
		 (mloc 'world
		       (xp-goal-of-outcome->actor
			 original-player
			 (play original-player obj)
			 (happy original-player))
; 			  (results-in		; Changed to xp-goal-of-outcome->actor [cox 11jun95]
;			    (play p1 obj)
;			    (happy p1))
			  ;;; 			  
		       ))))
	    ;; otherwise, pick up the object and play more catch.
	    (t (doit (grasp p2 obj))
	       (play-catch p2 p1 obj (+ n 1)))))
	 ;; otherwise, play more catch (throw it back).
	 (t (play-catch p2 p1 obj (+ n 1)))))))
      
    

;;; ||||||
;;; What kind of satisfaction does one derive from answering a ring?
;;; I believe that the author of this goal does not realize the s-X
;;; goals are as such, nor that there exist other goal types in the SPGU 
;;; goal taxonomy. [29 july 93 cox] 
;;;
(defun s-answer (actor rec)
  (do-break  s-answer)
  (goal-eval actor (state (get rec 'component-of) 'ring 'neg)
             (list (make-plan `(answer ,actor ,rec)
                        (doit (grasp actor rec))
                        (doit (ungrasp actor rec))
             ))))


;;;
;;; Added goal, main-action, and the explanation. [cox 4jun95]
;;; 
(defun s-hunger (actor
		 &aux
		 (main-action nil)
		 (goal (state actor 'hungry 'neg)))
  (do-break  s-hunger )
  (goal-eval
    actor
    goal
    (a food food (knows-loc actor food)
       (list
	 (make-plan
	   `(s-hunger ,actor)
	   (and
	     (dcont actor food)
	     (iprep actor food)
	     (doit
	       (random-choice
		 (/ 2.1 (get actor 'age))
		 (setf main-action
		       (ingest actor food nil))
		 (propel actor food 'floor1)))
	     (if (get food 'component) ;;peel
		 (doit (propel  actor (get food 'component) 'basket1 actor))
		 t)
	     (if (knows? 'world goal)
			     (assert-fact	; Added the explanation [cox 4jun95]
			       (mloc 'world
				     (xp-goal-of-outcome->actor
				       actor main-action goal))))
	     ))))))
                    

(defun s-happiness (actor)
 (do-break  s-happiness )
  (let ((goal (state actor 'sad 'neg)))
    (goal-eval actor goal
      ;; |||||| Why isn't the macro "a" used here?
      (mapcar
          #'(lambda (person)
              (make-plan `(s-happiness ,actor via ,person)
                (and (not (eq person actor))
                     (not (eq (get person 'gender) (get actor 'gender)))
                     (relate actor person actor 'like)
                     (persuade actor person (kiss person actor) goal))))
          *personae*))))


(defun iprep (actor food)
  (do-break  iprep )
  (if (get food 'component) ;;peel
    (doit (propel actor (get food 'component) nil food))
    t))


(defun dcont (actor object)
  (do-break  dcont )
  (let ((goal (has actor object)))
    (if (and (is-a object 'food)
             (< (or (get actor 'age)
		    11)				; If no age, assume older than 10 [cox 12oct94]
		10))
      (let ((o (random-element '(mom dad))))
        (doit (mtrans  actor (question (atrans o object actor o)) o actor))
        (doit (grasp o object))
        (doit (atrans o object actor o)))
      (goal-eval actor goal
		 (list
                  (make-plan `(dcont ,actor ,object no-owner)
                             
                             (AND (DKNOW ACTOR (WHERE-IS OBJECT))
                                  (DPROX ACTOR ACTOR OBJECT)
                                  (let ((loc (knows-loc actor object)))
                                    (and 
                                     (DACCESS actor loc t)
				     ;; Check for preconditions. [cox 17aug93]
;;; No longer necessary [cox 13sep93](if (let ((owner (knows-owner 'world object)))
;;; 					   (or (null owner)
;;; 					       (eq actor owner)))
				     (DOIT (ATRANS ACTOR OBJECT ACTOR loc))
;;; 					 )
                                     (daccess actor loc nil))))))))))


;;; Since actors can now fill pipes with ganja, the variables in this plan have 
;;; more general identifiers. So cup -> container & drink -> ingredients. [cox 6aug93]
;;;
(defun dfill (actor container ingredients)
  (when (is-true? (state container 'broken 'neg))
    (let ((goal (bi-state container ingredients 'filled 'pos)))
     (goal-eval actor goal
                 (list (make-plan `(dfill ,actor ,container ,ingredients)
                                  (AND (Dcont actor container)
                                       (let ((loc (knows-loc actor ingredients)))
                                         (cond  ((and loc (is-a loc 'faucet))
                                                 (and (daccess actor loc t)
;;; no longer necessary [cox 13sep93]		      (does-have-p container
						      ;;; actor) 
                                                      (doit (ptrans actor container loc nil))
                                                      (daccess actor loc nil))
						 ;t ||||||Comment these out? [cox 5aug93]
						 )
                                                (t 
						 (and (dcont actor ingredients)
						      ;||||||Doit should check for preconditions. [cox 12aug93]
;;; 						      (does-have-p container actor) 
;;; 						      (does-have-p ingredients actor) 
						      (doit (tilt ACTOR ingredients container NIL)))
						 ;t ||||||Comment these out? [cox 5aug93]
						 ))))
                                  ))))))


;;; Goal and associated plan to light a flamable item with an ignighter. [cox 31jul93]
;;;
(defun dlight (actor flamable-item ignighter)
  (do-break  dlight )
  (when (is-true? (state ignighter 'broken 'neg))
    (let ((goal (state flamable-item 'burning 'pos)))
     (goal-eval actor goal
                 (list (make-plan `(dlight ,actor ,flamable-item ,ignighter)
                                  (AND (Dcont actor flamable-item)
				       (dcont actor ignighter) ;Get the lighter
				       (doit (propel actor ignighter nil)) ;Turn it on.
;;; no longer needed [cox 13sep93]     (does-have-p flamable-item actor)
				       ;;;   ;If
				       ;;; the actor
				       ;;; 				       still has item
				       (doit (ptrans actor ignighter flamable-item nil)) ;Light the item.
				       (doit (propel actor ignighter nil)))))))) ;Turn the lighter off.
  )


;;; Gain access to some container? [cox 31jul93]
;;;
(defun daccess (actor object-loc open?)
  (do-break  daccess )
  (let ((goal (state object-loc (if (is-a object-loc 'faucet)
                                  'flowing
                                  'open)
                     (if open? 'pos 'neg))))
    (cond((or (not (is-a object-loc 'container))
              (is-true? goal))
           t)
         (t (goal-eval actor goal
               (list (make-plan `(daccess ,object-loc)
                                (let ((comp (component-of object-loc)))
                                  (if open? 
                                    (doit (propel actor comp nil object-loc))
                                    (doit (propel actor comp object-loc nil)))))))))))


;;; The actor's plan to persuade the owner of some object to leave his present
;;; location so that the actor can obtain the object. See the comment on 
;;; new-location for the reality of this plan. 
;;;
(defun lure-away (actor owner object)
  (do-break  lure-away )
  (let* ((new-loc (new-location owner))
         (goal (is-at owner new-loc)))
    (goal-eval actor goal
      (list (make-plan `(lure-away ,actor ,owner ,object)
              (and
                  (persuade actor owner (ptrans owner owner new-loc nil) goal)
                  (doit (atrans actor object actor nil))))))))




;;; Returns the first location from *all-locations* at which the actor is not 
;;; located. Called by lure away only. It is absurd in this microworld with
;;; only one room and all other locations being places like the cupboard. 
;;; Would it be a good plan to lure dad away to the cupbaord if he is in the 
;;; kitchen? Would he be convinced to crawl in there? Needs to be modified so
;;; that containers and tables are not chosen. [cox 8aug93]
;;;
(defun new-location (actor)
  (do-break  new-location )
  (dolist (loc *all-locations*)
    (if (not (eq loc (loc actor))) 
	(return loc))))


(defun dknow (actor info)
  #+dec (declare (notinline mapcar) (optimize safety (speed 0)))
  (let ((goal (mloc actor info)))
    (goal-eval actor goal
      (mapcar #'(lambda (agent)
                  (make-plan `(dknow ,actor ,agent to tell ,info)
                    (AND (KNOWS-LOC ACTOR AGENT)
                         (OR (IS-FRIEND-OF? AGENT ACTOR)
                             (NOT (RELATE ACTOR actor AGENT 'fear)))
                         (not (knows? actor (negate (mloc agent info))))
                         (PERSUADE ACTOR 
                                   AGENT 
                                   (MTRANS AGENT INFO ACTOR AGENT)
                                   GOAL))))
              (remove actor *personae*))))) ;;||||||Officer1 not in here? [cox 17aug93]
                

(defun dprox (actor object to-loc)
  (do-break dprox)
  (let ((goal (is-at object to-loc)))
    (GOAL-EVAL ACTOR 
               goal
               (LIST
                   (make-plan `(dprox ,actor ,object to ,to-loc 2)
                              (AND (NOT (eq ACTOR OBJECT))
				   ;;|||||| Officer1 need to be in here? [cox 17aug93]
                                   (MEMber OBJECT 
					   (cons 'officer1 ;Sure [cox 17aug93]
						 *PERSONAE*) )
				   ;; Persuade someone to do it for you. [cox 17aug93]
                                   (PERSUADE ACTOR 
                                             OBJECT 
                                             (PTRANS OBJECT 
                                                     OBJECT 
                                                     to-loc 
                                                     (LOC-NAME-OF OBJECT))
                                             GOAL)))
		   (make-plan `(dprox ,actor ,object to ,to-loc 1)
                     (AND (OR (eq ACTOR OBJECT) (DPROX ACTOR ACTOR OBJECT))
                          (DKNOW ACTOR (WHERE-IS to-loc))
                          (OR (eq ACTOR OBJECT) (DOIT (GRASP ACTOR OBJECT)))
                          (OR (and (not (eq to-loc '?unspec))
                                   (IS-PROX? ACTOR (LOC-NAME-OF to-loc)))
                              (DOIT (PTRANS ACTOR 
                                            OBJECT 
                                            (KNOWS-LOC ACTOR to-loc)
                                            (KNOWS-LOC ACTOR ACTOR))))
                          (OR (eq ACTOR OBJECT)
                              (DOIT (UNGRASP ACTOR OBJECT)))))
                   ))))


(defun persuade (actor agent action result)
  (do-break  persuade )
  (GOAL-EVAL ACTOR ACTION 
             (APPEND (LIST (move-plan actor agent action result)
                           (ASK-PLAN ACTOR AGENT ACTION RESULT))
                     (mapcar #'(lambda (food) 
                                 (bargain-plan actor agent action food))
                             (GET-ISA 'FOOD AGENT))
                     (LIST (THREAT-PLAN ACTOR AGENT ACTION)))))


(defun move-plan (actor agent action result)
  (do-break  move-plan )
  (make-plan `(move-plan ,actor ,agent action = ,action result = ,result)
             (do-move-plan actor agent action result)))

(defun do-move-plan (actor agent action result)
  (do-break  do-move-plan )
  (and (match action (ptrans nil agent nil nil))
       (not (relate actor actor agent 'like))
       (let ((food (new-food actor agent)))
         (and food
              (tell actor agent (is-at food (cdpath '(to) action)))
              (is-true? result)))))

(defun new-food (actor agent)
  (do-break  new-food )
  (dolist (food (get-isa 'food agent))
    (if (not (knows? actor (has agent food)))
        (return food))))


;  The success of asking something depends upon whether the other person
;  is honest and likes you. [icu]
;
(defun ASK-PLAN (ACTOR AGENT ACTION RESULT)
  (do-break  ASK-PLAN )
  (make-plan `(ask-plan ,actor ,agent ,action ,result)
      (AND (RELATE ACTOR actor AGENT 'trust)
           (RELATE ACTOR ACTOR AGENT 'LIKE)
           (TELL ACTOR AGENT (QUESTION ACTION))
           (IS-TRUE? RESULT))))


;  The success of bargaining with someone by giving them food depends
;  on whether the other person is honest, you don't already have the
;  goal of getting the food you're going to bargain with, and you can
;  get the food to the other person. [icu]
;
(defun BARGAIN-PLAN (ACTOR AGENT ACTION FOOD)
  (do-break  BARGAIN-PLAN )
  (LET ((ATRANS-FOOD (ATRANS ACTOR FOOD AGENT ACTOR)))
    (make-plan `(bargain ,actor ,atrans-food)
               (and (not (knows? actor (negate (possible action))))
                    (RELATE ACTOR actor AGENT 'trust)
                    (NOT (KNOWS? ACTOR (HAS AGENT FOOD)))
                    (NOT (HAS-GOAL? ACTOR (HAS ACTOR FOOD)))
                    (DOIT (MBUILD ACTOR (CAUSE ATRANS-FOOD (MAYBE ACTION))))
                    (TELL ACTOR AGENT (QUESTION (CAUSE ATRANS-FOOD (FUTURE ACTION))))
                    (DCONT ACTOR FOOD)
                    (DPROX ACTOR ACTOR AGENT)
                    (DOIT ATRANS-FOOD)
                    (cond ((IS-TRUE? ACTION))
                          (t (say (negate action))
                             (doit (mloc actor 
                                         (relation actor agent 'trust 'neg)))
                             nil))))))


;  The success of threatening depends upon whether you dominate
;  the other person. [icu]
;
(defun THREAT-PLAN (ACTOR AGENT ACTION)
  (do-break  THREAT-PLAN )
  (make-plan `(threat ,actor ,agent ,action)
      (and (not (knows? actor (negate (possible action))))
           (NOT (RELATE ACTOR actor AGENT 'fear))
           (TELL ACTOR AGENT 
                 (CAUSE (negate ACTION) (FUTURE (PROPEL ACTOR 'HAND AGENT))))
           (OR (IS-TRUE? ACTION)
               (AND (DOIT (PROPEL ACTOR 'HAND AGENT)) (IS-TRUE? ACTION))))))


(defun TELL (ACTOR AGENT INFO)
  (do-break  TELL )
  (let ((action (mtrans actor info agent actor)))
    (GOAL-EVAL ACTOR 
               action ;(MLOC AGENT INFO)
               (LIST
                 (make-plan `(tell ,actor ,agent ,info)
                   (AND (DPROX ACTOR ACTOR AGENT)
			;;Added to make sure that Dprox works. [cox 18aug93]
			;; |||||| But after the changes allowing officer1 knowledge,
			;; is this necessary? 
			(is-prox? actor (loc-name-of agent))
                        (DOIT action)))))))



;;; One of the random events that occur occasionally. [cox 8aug93]
;;;
;;; Commented out the initial parens. They were typo no doubt with no side-effect -> added 
;;; null statement to first action of the function. [cox 8aug93]
;;;
(def-conseqs call-on-phone
  ;() 
  (ADD-CONSEQ (state $(OBJECT) 'ring 'pos))
  (let((actor (random-element (remove *main-char* '(mom dad))))
       (rec (get $(OBJECT) 'component)))
    (add-plan  ;; [cox 10aug93]
     (make-plan `(answer-phone ,actor ,rec) ;Changed ,red to ,rec. [cox 6aug93]
		(s-answer actor rec)))))


;;; |||||| Need if dog is barking at stranger then everyone that there 
;;; stranger is present. [cox 9aug93]
;;;
(def-conseqs dog-barks
  (ADD-CONSEQ (state $(ACTOR) 'barking 'pos))
  )


;;; [cox 9aug93]
(def-conseqs police-arrive
  (ADD-CONSEQ (state $(ACTOR) 'concerned 'pos))
  (let ((police $(ACTOR) ))
    (add-plan
     (make-plan `(police-arrive  ,police)
		(and (doit (ptrans police police 'outside1 nil))
		     (doit (dog-barks 'dog1))
		     (doit (propel police 'door-bell-switch1 nil nil '(pos)))
		     (s-bust police 'elvis))))))


;;; [cox 25aug93]
(def-conseqs k-9-squad-arrive
  (ADD-CONSEQ (state $(ACTOR) 'concerned 'pos))
  (let ((police $(ACTOR) ))
    (add-plan
     (make-plan `(k-9-arrival-events  ,police)
		(and (doit (ptrans police police 'outside nil))
		     (doit (ptrans 'police-dog1 'police-dog1 'outside nil))
		     (doit (propel police 'door-bell-switch1 nil nil '(pos)))
		     (s-k-9-bust police 'police-dog1 'elvis))))))


;;; [cox 25aug93]
;;; Modified so that if dog does not successfully locate contraband, no
;;; arrest takes place and the police and dog leave.  
;;; [mdd 23mar94]
(defun s-k-9-bust (actor k-9 suspect)
  (do-break  s-k-9-bust )
  (goal-eval
    actor 
    (state actor 'concerned 'neg)
    (a marijuana contraband (knows-loc actor contraband)
       (list
	 (make-plan
	   `(s-k-9-bust ,actor ,k-9 ,suspect)
	   (let ((outcome nil))
	     (and
	       (or
		 (and
		   (doit (ptrans actor actor 'kitchen nil))
		   (doit (ptrans k-9 k-9 'kitchen nil))
;;		 (if (is-a (loc contraband) 'container)         ; generalized [mdd 22mar94]
		   (if t
;;; 		     (is-a (loc contraband) 'hiding-place)      ; to hiding place
		       (or (setf
			     outcome		; Outcome is not set to the dog-bark event by sniffs-out [cox 7feb95]
			     (sniffs-out k-9 contraband actor)) t) 
		       (if (person-controls? contraband)
			   (plan-to-take-from-person 
			     actor
			     contraband
			     (person-controls? contraband))
			   (doit (atrans actor contraband actor (loc contraband)))))
		   ;;||||||Note person may be diff under plan-to-take...
		   (let ((main-action
			   (if (not (null outcome))
			       (doit (arrest actor suspect))
			       ))
			 (goal (has actor suspect)))
		     (when (and main-action
				(knows? 'world goal ))
		       (assert-fact		; Added the explanation [cox 4jun95]
			 (mloc 'world
			       (xp-goal-of-outcome->actor
				 actor main-action goal))))
		     (or main-action
			 t))
		   )
		 t)
	       (doit (ptrans actor actor 'outside nil))
	       (if (not (null outcome))
		   (doit (ptrans suspect suspect 'outside nil))
		   t)
	       (doit (ptrans k-9 k-9 'outside nil))
	       (if (null outcome)
		   (assert-fact
		     (mloc 'world
			   (state actor 'concerned 'pos)))
		   ;; (s-jonesing actor) will not work below. [cox 6feb95]
;;; 		   (s-jonesing actor)
		   (assert-fact
		     (mloc 'world
			   (self-cause
;;; 			     (mtrans k-9 (where-is contraband) k-9 nil)
			     (detection k-9 contraband)
			     outcome) )))
	       ))))
       ))
  )


(defun plan-to-take-from-person (officer object person)
  (tell officer person 
	(question 
	 (atrans person object officer '?unspec)))
  (doit (atrans person object officer person))
  )

;;;  Modified so that police dog will sniff at a random location
;;;  There is a chance that the dog will sniff at a location that
;;;  does not hide the contraband, in which case the officer won't be
;;;  able to confiscate it and arrest Elvis.  Return t if contraband found
;;;
;;;  Each sniff has a random chance of the officer barking like a dog
;;;  Successful sniff has random chance of dog speaking
;;;  [mdd 23mar94]
(defun sniffs-out (dog contraband officer)
  (if (<= (random 100) *sniff-success*)
	  (sniffs-out-correct-loc dog contraband officer)
	(sniffs-out-wrong-loc dog contraband officer)))

;;; Dog sniffs at incorrect location 
;;; Sometimes the dog will think it found contraband and bark
;;; [mdd 24mar94]
(defun sniffs-out-wrong-loc (dog contraband officer)
  (let* ((contraband-loc (loc contraband))
	 (hiding-places 
	   (remove contraband-loc (enum-hiding-places (eval *facts-pointer*))))
	 (random-loc (nth (random (length hiding-places)) hiding-places)))
    (and
      (doit (ptrans dog dog random-loc nil))
      (sniff dog random-loc)
      (if (<= (random 100) *bark-at-contraband-freq*)	   
	  (doit (dog-barks dog random-loc))) 
      (when (<= (random 100) *officer-bark-freq*)
	(if *TDebug-On*
	    (format *tspin-stream* "####"))
	(doit (dog-barks officer random-loc)))
      (when (<= (random 100) *dog-speak-freq*)   
	(if *TDebug-On*
	    (format *tspin-stream* "#####"))
	(tell dog 'Elvis 
	      (question 
		(atrans 'Elvis contraband dog '?unspec))))
      (daccess officer random-loc t)
      (daccess officer contraband-loc nil)
      ))
  nil						; Return nil.
  )

;;; Dog successfully locates contraband
;;; There is a random chance that the dog will ask the suspect for
;;; the contraband or the officer will bark (noise)
;;; Occasionally the dog won't notice the contraband and so won't
;;; bark.  t is returned if contraband is found
;;; [mdd 24mar94]
;;;
;;; Modified this function so that it returns the dog-barking event if the
;;; detection was successful. [cox 7feb95]
;;; 
(defun sniffs-out-correct-loc (dog contraband officer)
  (let ((contraband-loc (loc contraband))
	(return-val nil))
    (doit (ptrans dog dog contraband-loc nil))
    (doit (sniff dog contraband-loc))
    (when (<= (random 100) *dog-speak-freq*)
      (if *TDebug-On*
	  (format *tspin-stream* "#"))
      (tell dog 'Elvis 
	    (question 
	      (atrans 'Elvis contraband dog '?unspec))))
    (when (<= (random 100) *officer-bark-freq*)
      (if *TDebug-On*
	  (format *tspin-stream* "##"))
      (doit (dog-barks officer contraband-loc)))
    ;; the officer will only find the contraband if the dog barks
    (if (<= (random 100) *bark-at-contraband-freq*)	   
	(and 
	  (doit (setf return-val
		      (dog-barks dog contraband-loc)))
	  (daccess officer contraband-loc t)
	  (doit (ptrans officer officer contraband-loc nil))
	  (doit (atrans officer contraband officer contraband-loc))
	  (daccess officer contraband-loc nil)
	  t)
	nil)
    return-val))
	  
;;; This is the original function
#|
(defun sniffs-out (dog contraband officer)
  (let ((contraband-loc (loc contraband)))
    (and
     (doit (ptrans dog dog contraband-loc nil))
     (sniff dog contraband-loc)
     (doit (dog-barks dog))
     (daccess officer contraband-loc t)
     (doit (atrans officer contraband officer contraband-loc))
     (daccess officer contraband-loc nil)
     ))
  )
|#


(def-conseqs arrest
  (add-conseq (has $(ACTOR)  $(OBJECT) ))
  )

(defun arrest (officer suspect)
  ;; Suspect quits pursuing other goals.
  (setf (get suspect 'goals) nil)
  (make-old-cd :head 'arrest :ACTOR officer :OBJECT suspect)
  )

;(defun sniff (dog location)
;  (format? *tspin-stream* "The ~s sniffed at the ~s." dog location)
;  t
;  )


;;; [cox 9aug93]
;;;
;;; Modified so that suspect has a random chance of complying with officer's
;;; request to hand over the contraband.  If request is not complied with,
;;; the cop will search for the contraband.  If found, the suspect will be
;;; arrested.  [mdd 25mar94]
;;;
;;; Added goal, main-action, and the explanation. [cox 4jun95]
;;; 
(defun s-bust (actor suspect
	       &aux
	       (main-action nil)
	       (goal
		 (has actor suspect)
;;; 		 (state actor 'concerned 'neg)
		 ))
  (do-break  s-bust )
  (goal-eval
    actor 
    goal
    (a marijuana contraband (knows-loc actor contraband)
       (list
	 (make-plan
	   `(s-bust ,actor ,suspect)
	   (and
	     (doit (ptrans actor actor 'kitchen nil))
;			(persuade actor 
;				  suspect 
;				  (atrans suspect contraband actor nil) 
;				  (has actor contraband))
	     ;; Added the following sequence [cox 17aug93]
	     (tell actor suspect 
		   (question 
		     (atrans suspect contraband actor '?unspec)))
				                        ;;; suspect may not comply
	     (if (<= (random 100) *comply-freq*)
		 (and (dcont suspect contraband)
		      (doit (atrans suspect contraband actor suspect))
		      (if (< (get 'ganja1 'amount) *amount-threshold*)
			  (doit (ptrans actor actor 'outside nil))
			  (and			; 
 			    (setf main-action
				  (doit (arrest actor suspect)))
			    (doit (ptrans actor actor 'outside nil))
			    (doit (ptrans suspect suspect 'outside nil)))))
		 (if (search-for-contraband actor contraband)
		     (and			; (dcont actor contraband)
		       (doit
 			 (setf main-action
			       (arrest actor suspect))
			 )
		       (doit (ptrans actor actor 'outside nil))
		       (doit (ptrans suspect suspect 'outside nil)))
		     (doit (ptrans actor actor 'outside nil)))))
	   (when (and main-action
		      (knows? 'world goal ))
	     (assert-fact			; Added the explanation [cox 4jun95]
	       (mloc 'world
		     (xp-goal-of-outcome->actor
		       actor main-action goal))))
	   ))
       ))
  )

;;;  Actor will search random location for contraband
;;;  t is returned if location is correct, else nil
;;;
;;;  [mdd 24mar94]
;;;
;;; Added the mtrans assertions and the second cond statement. [cox 4feb95]
;;; 
(defun search-for-contraband (actor contraband)
  (let ((contraband-loc
	  (loc contraband))
	(hiding-places
	  (enum-hiding-places (eval *facts-pointer*)))
	(random-loc nil))
    (dotimes (count (random (length hiding-places)))
      (cond ((<= (random 100) *search-success-rate*)
	     (doit (ptrans actor actor contraband-loc nil))
	     (doit
	       (mtrans actor
		       (bi-state contraband contraband-loc  'loc 'pos)
		       actor
		       nil))
	     (return-from search-for-contraband t))
	    (t
	     (setf random-loc
		   (nth (random (length hiding-places)) 
			hiding-places))
	     (doit (ptrans actor actor random-loc nil))
	     ;; Check to see if the contents are here. [cox 4mar95]
	     (cond ((eq  contraband-loc random-loc)
		    (doit
		      (mtrans actor
			      (bi-state contraband random-loc 'loc 'pos)
			      actor
			      nil))
		    (return-from search-for-contraband t))
		   (t
		    (doit
		      (mtrans actor
			      (bi-state contraband random-loc 'loc 'neg)
			      actor
			      nil))))
	     ))))
  ;; Unfortunately the following say that the actor knows where the contraband is't. [cox 4mar95]
;  (doit
;    (mtrans actor
;	    (bi-state contraband '?unspec  'loc 'neg)
;	    actor
;	    nil))
  nil)


;;;;;;;;;;;;;;;;
;;;  ATRANS
;;;;;;;;;;;;;;;;

(defun atrans (actor object to from)
  (make-old-cd :head 'atrans :actor actor :object object :to to :from from))


;;; |||||| Comment the hack below better. Explain why there is such a problem
;;; requiring the hack for now.
;;; 
(def-preconds atrans
  ;; No other agent can have control over the actor. [cox 13aug93]
  (let ((precond-test-value
	  (cond 
	    ((under-anothers-control-p
	       $(actor)
	       $(actor))
	     (eq $(to) (person-controls? $(actor))))
	    ((police-present-p $(actor))
	     (or (is-a $(to) 'police)
		 (not (is-a $(object) 'illegal-item))
		 (and	  ;||||||Hack: This is as a result of police telling Elvis to give it to him.
		   (eq $(actor) 'elvis)
		   (eq $(object) 'ganja1)
		   (eq $(to) 'elvis)
		   (eq $(from) 'fridge1))
		   ))
	    ((under-anothers-control-p
	       $(actor)
	       $(actor))
	     (eq $(to) (person-controls? $(actor))))
	    (t
	     t))))

   (cond
     ((null precond-test-value)
      (if *TDebug-On*
	  (format? *tspin-stream* "--> "))
;;;       (break)
      (if (under-anothers-control-p
	    $(actor)
	    $(actor))
	  (say (has (knows-owner 'world $(ACTOR) ) $(ACTOR) ))
	 (format? *tspin-stream*
		 "Police are present."))
      nil))
    precond-test-value)
  )



;;; |||||| Probably want to check to make sure that the to and from fillers are
;;; not the same before asserting the negative inferences (as in someone gave 
;;; himself a present). See comments on ptrans and addfact. [cox 11aug93]
;;;
(def-conseqs atrans
  (ADD-CONSEQ (HAS $(TO)  $(OBJECT) ))
  (ADD-CONSEQ (IS-AT $(OBJECT)  $(TO) ))
  (when (and $(FROM)  $(TO) )
	(ADD-CONSEQ (negate (HAS $(FROM)  $(OBJECT) )))
	(ADD-CONSEQ (negate (is-at $(OBJECT)  $(FROM) ))))
  (if (and (eq 'ganja1 $(OBJECT) )	;Too specific, but ... [cox 13aug93]
	   (eq 'officer1 $(TO) ))
      (add-conseq (state $(TO)  'concerned 'neg)))
  )


;;;;;;;;;;;;;;;;
;;;  KISS
;;;;;;;;;;;;;;;;

(defun kiss (actor kissee)
  (make-old-cd :head 'kiss :actor actor :object kissee))

(def-conseqs kiss
  (add-conseq (state $(OBJECT)  'sad 'neg)))



;;;;;;;;;;;;;;;;
;;;  GRASP
;;;;;;;;;;;;;;;;

(defun GRASP (ACTOR OBJECT)
 (make-old-cd :head 'GRASP :ACTOR ACTOR :OBJECT OBJECT))

(def-conseqs grasp
  (if (and (is-a $(OBJECT)  'receiver)
           (not (IN-MODE? 'TF)))
    (add-conseq (state (get $(OBJECT)  'component-of) 'ring 'neg)))
  (ADD-CONSEQ (COND ((IN-MODE? 'TF)
                     (negate (HAS $(ACTOR)  $(OBJECT) )))
                    (T
                     (HAS $(ACTOR)  $(OBJECT) ))))
  (ADD-CONSEQ (is-at  $(OBJECT)  $(ACTOR) )))



;;;;;;;;;;;;;;;;
;;;  UNGRASP
;;;;;;;;;;;;;;;;

(defun UNGRASP (ACTOR OBJECT) (make-old-cd :head 'unGRASP :ACTOR ACTOR :OBJECT OBJECT))

(def-conseqs ungrasp
         (ADD-CONSEQ (negate (HAS $(ACTOR)  $(OBJECT) )))
         (ADD-CONSEQ (negate (is-at  $(OBJECT)  $(ACTOR) )))
         (when (is-a $(OBJECT)  'balloon)
	   ;; |||||| And should check to make sure that the baloon is not tied.
           (ADD-CONSEQ (state $(OBJECT)  'flying 'pos))
           (ADD-CONSEQ (state $(OBJECT)  'inflated 'neg))))



;;;;;;;;;;;;;;;;
;;;  TIE
;;;;;;;;;;;;;;;;

(defun tie (ACTOR OBJECT)
 (make-old-cd :head 'tie :ACTOR ACTOR :OBJECT OBJECT))

(def-conseqs tie
  (ADD-CONSEQ (state $(OBJECT)  'tied 'pos)))



;;;;;;;;;;;;;;;;
;;;  EXPEL
;;;;;;;;;;;;;;;;

(defun expel (ACTOR OBJECT to &optional from)
  (make-old-cd :head 'expel :ACTOR ACTOR :OBJECT OBJECT :from from :to to))

(def-conseqs expel
  (if (and $(TO) 
	   (is-a $(TO)  'balloon))
    (ADD-CONSEQ (state $(TO)  'inflated 'pos))))



;;;;;;;;;;;;;;;;
;;;  INGEST
;;;;;;;;;;;;;;;;

(defun INGEST (ACTOR OBJECT &optional from)
  (make-old-cd :head 'INGEST :ACTOR ACTOR :OBJECT OBJECT :from from))

;;; Modified to handle drug addiction [29 jul 93 cox] & [23feb95]
;;;
(def-conseqs ingest
  ; If the actor is using an object with the ingest
  ; then it becomes empty and dirty.
  (when $(FROM) 
      (add-conseq (bi-state $(FROM)
			    $(OBJECT)
			    'filled 'neg))
      (add-conseq (state $(FROM)
			 'dirty 'pos)))
  (ADD-CONSEQ (STATE $(ACTOR) 
                     (if (is-a $(OBJECT)  'drink)
			 'THIRSTY 
		       (if (is-a $(OBJECT)  'plant)
			   'jonesing
			 'hungry
			 ))
                     'NEG)))


;;;;;;;;;;;;;;;;
;;;  LOC
;;;;;;;;;;;;;;;;

;;; |||||| Why is this commented out? [cox 7aug93]
(def-conseqs loc 
  ;(NOTICE $(ACTOR)  *CD*)
  nil)

;;; |||||| Do not exactly understand the test on the actor slot of the con
;;; slot's filler of a location cd since a loc does not use the con slot. An
;;; mloc does though, so perhaps this is a leftover from Meehan's original
;;; version. Going by the way this version handles locations anyway, I do not
;;; think that Pazzani's version used locs correctly. [cox 11sep93]
;;;
;;;  |||||| But looking at Sack's version of Tale-Spin there is the same code
;;;  as here ands as in where-is, etc. I do not understand.
;;; 
(def-reaction loc 
  (AND (OR (MEMber $(CON ACTOR)  (GET-ISA 'FOOD $(VAL) ))
           (EQ $(CON ACTOR)  'WATER))
       (SGOAL-CHECK $(VAL) 
                    (COND ((EQ $(CON ACTOR)  'WATER) 'THIRSTY)
                          (T 'HUNGRY)))))



;;;;;;;;;;;;;;;;
;;;  MBUILD
;;;;;;;;;;;;;;;;

(defun MBUILD (ACTOR OBJECT)
  (do-break mbuild)
  (make-old-cd :head 'MBUILD :ACTOR ACTOR :OBJECT OBJECT))

(def-conseqs mbuild
  (if (eq $(ACTOR)  $(OBJECT CONSEQ ACTOR) )
      (push (CONS $(OBJECT ANTE)  $(OBJECT CONSEQ) )
            (GET $(ACTOR)  'DEMONS))))


;;;;;;;;;;;;;;;;
;;;  MLOC
;;;;;;;;;;;;;;;;

;;; Asserts that a particular actor knows a particular concept, that
;;; is, the mental location of the concept is with the actor. [cox 7aug93]
;;;
;;; The whole use of the term actor is inconsistent in TSpin. Here the actor
;;; parameter is the location of the concept, whereas with regular loc cds the
;;; actor slot is the item for which a location is specified. In a control cd
;;; (see 'has Ddefun) the actor is the object controlled, whereas the val is the
;;; person who controls. [cox 11sep93]
;;; 
(defun MLOC (ACTOR CON)
  (do-break mloc)
  (make-old-cd :head 'MLOC :CON CON :VAL actor))

(def-conseqs mloc
  (DEMON-CHECK $(VAL)  $(CON) )
  ;(unless (MEMber 'NEG $(CON MODE) )
    (let ((p (get (old-cd-head $(CON) ) 'react-fn)))
      (if p (funcall p))))

;;; |||||| ??? [cox 17aug93]
(def-reaction mloc
  (if (and (not (eq $(VAL)  $(CON VAL) ))
           (positive? $(CON) )
           (or (eq $(CON CON ACTOR)  $(CON VAL) )
               (relate $(VAL)  $(VAL)  $(CON VAL)  'trust)))
      (add-conseq (mloc $(VAL)  $(CON CON) )))
  (if (negative? $(CON) )
      (add-conseq (mloc $(VAL) 
                        (negate (possible
                                    (mtrans $(CON VAL) 
                                            $(CON CON) 
                                            nil;'?unspec
                                            $(CON VAL) )))))))
                      


;;;;;;;;;;;;;;;;
;;;  MTRANS
;;;;;;;;;;;;;;;;

(defun MTRANS (ACTOR OBJECT TO FROM)
  (make-old-cd :head 'MTRANS :ACTOR ACTOR :OBJECT OBJECT :TO to :FROM FROM))

;  Consequences of an mtrans: if there is a ques in the CD mtransed,
;  and if it is a causal, then it is a bargaining promise; otherwise,
;  it is a request (assuming the actors in the sub-CD are in the right
;  places).  If there is no ques in the CD mtransed, then the hearer
;  knows about the mtrans, and if he believes the speaker, then he
;  believes what the speaker believes. [icu]
;;;
;;; |||||| Why is this commented out? It exists in Warren Sack's version 
;;; of Tale-Spin (~/Research/Tale-Spin/tspin-waander.lisp) given to me by 
;;; Bill Andersen. Was it that difficult to port? Ask Pazzani. [cox 26 july 93]
;;;
(def-conseqs mtrans ())
 ; (LET ((ACTOR $(ACTOR) )
 ;       (OBJECT $(OBJECT) )
  ;      (HEARER $(TO) ))
  ;  (COND ((eq (old-cd-head object) 'query) ;(MEMber 'QUES $(OBJECT MODE) )
  ;         (COND ((AND (EQ (old-cd-head (cdpath '(con) OBJECT)) 'CAUSE)
  ;                     (eq ACTOR $(OBJECT CON ANTE ACTOR) )
  ;                     (eq HEARER $(OBJECT CON CONSEQ ACTOR) ))
  ;                (PROMISE-CONSEQS
   ;                   HEARER 
   ;                   $(OBJECT CON CONSEQ) 
   ;                   ACTOR 
    ;                  $(OBJECT CON ANTE) ))
    ;             ((eq $(OBJECT CON ACTOR)  HEARER)
  ;                (REQUEST-CONSEQS ACTOR HEARER
   ;                                (cdpath '(con) object)))))
   ;       ((NOT (eq ACTOR HEARER))
     ;      (ADD-CONSEQ (MLOC HEARER *CD*))
    ;       (cond ((match object (state hearer 'smart 'neg))
    ;              (add-conseq
    ;                  (mloc hearer (relation hearer actor 'like 'neg))))
   ;              ((RELATE HEARER hearer ACTOR 'trust)
   ;               (ADD-CONSEQ (MLOC HEARER (MLOC ACTOR OBJECT)))))))))


(defun detection (actor object &optional to from)
  (make-old-cd :head 'detection :ACTOR ACTOR :OBJECT OBJECT :TO to :FROM FROM)
  )

(defun sniff (actor object &optional to from)
  (make-old-cd :head 'sniff :ACTOR ACTOR :OBJECT OBJECT :TO to :FROM FROM)
  )

(defun play (actor object &optional to from)
  (make-old-cd :head 'play :ACTOR ACTOR :OBJECT OBJECT :TO to :FROM FROM)
  )

;;;;;;;;;;;;;;;;
;;; PLAN
;;;;;;;;;;;;;;;;

(defun PLAN (ACTOR OBJECT)
  (make-old-cd :head 'PLAN :ACTOR ACTOR :OBJECT OBJECT))

(def-conseqs plane
  (if (eq $(ACTOR)  $(OBJECT ACTOR) )
      (add-action			; Was push on *actions*. [cox 10aug93]
       $(OBJECT) )))



;;;;;;;;;;;;;;;;
;;; PROPEL
;;;;;;;;;;;;;;;;

(defun PROPEL (ACTOR OBJECT TO &optional from mode )
  (make-old-cd :head 'PROPEL :ACTOR ACTOR :OBJECT OBJECT :TO TO :from from :mode mode))

(defun TOY-PROPEL (ACTOR OBJECT TO &optional from mode )
  (make-old-cd :head 'TOY-PROPEL :ACTOR ACTOR :OBJECT OBJECT :TO TO :from from :mode mode))

;;; [cox 26jun95]
(defun HIT (ACTOR OBJECT &optional mode )
  (make-old-cd :head 'HIT :ACTOR ACTOR :OBJECT OBJECT :mode mode))

;;; |||||| This is currently in limbo. As is there exists no preconditions for
;;; a propel. [cox 7jan95]
;;; 
(def-preconds propel
  ;; No other agent can have control over the actor. [cox 13aug93]
  (cond
    ((or (not $(to)) t
;;; 	 (equal (loc $(ACTOR)) (loc $(TO)))
;	   (IS-PROX? $(ACTOR)			;Actor is near place object is being moved to.
;		     (LOC-NAME-OF
;		       $(to)))
	   ))
;   ((not (under-anothers-control
;	   $(actor)
;	   $(actor)))
;    t)
   (t
    ;otherwise report the precondition failure. [cox 17aug93]
    (if *TDebug-On*
	(format? *tspin-stream* ">>> "))
;;;     (break)
    (say (has (knows-owner 'world $(ACTOR) ) $(ACTOR) ))
    nil)
   )
  )


;;; args: actor object to from mode [cox]
;;;
(def-conseqs propel 
  (let ((c (contents $(OBJECT) )))
    (when c (add-conseq (bi-state $(OBJECT)  c 'filled 'neg))))
  (when (and (is-a $(OBJECT)  'peel)
	     (is-a $(FROM)  'fruit))
	(add-conseq (state $(FROM)  'peeled 'pos))
	(add-conseq (bi-state $(FROM)
			      $(OBJECT)
			      'attached 'neg)))
  (let ((h (get (get $(TO)  'composition) 'hardness)))
    (when (and  h 
                (eq (get $(OBJECT)  'composition) 'glass)
                (or *always* (> (random (* h 3)) (+ h 2))))
      (add-conseq (state $(OBJECT)  'broken 'pos))))
  (cond ((eq $(OBJECT)  'door-bell-switch1)
         (add-conseq (state 'door-bell1 'ring (or (car $(MODE) ) 'pos)))
         (if (member 'pos $(MODE) )
           (add-conseq (propel $(ACTOR)  $(OBJECT)  $(TO)  $(FROM)  '(neg))))))
  (cond ((eq $(OBJECT)  'light-switch1)                ;Boy is this specific code! [cox] 
         (let ((mode (knows-if 'world (state 'light1 'on nil))))
           (add-conseq (state 'light1 'on (if (member 'pos mode)
                                            'neg
                                            'pos))))))
  ;;; At least the following is a bit more general. Created for turning on a lighter. [cox 31jul93]
  (cond ((is-a $(OBJECT)  'lighter) 
         (add-conseq (state $(OBJECT)  
			    'on 
			    (if (member 'pos 
					(knows-if 'world (state $(OBJECT)  'on nil)))
				'neg
			      'pos)))))
  (when (and $(TO) 
             (eq (get $(TO)  'composition) 'glass)
             (not (is-a $(OBJECT)  'balloon))
             (or *always* (> (random 11) 3)))
    (add-conseq (state $(TO)  'sharp 'pos))
    (add-conseq (state $(TO)  'shattered 'pos)))
  (when (and (is-a $(OBJECT)  'balloon)
             (eq (get $(TO)  'shape) 'pointed)
             (or *always* (> (random 11) 3)))
    (add-conseq (state $(OBJECT)  'bursted 'pos)))
  (COND ;((MEMber $(TO)  *PERSONAE*)
         ;(ADD-CONSEQ (negate (HAS $(ACTOR)  $(OBJECT) )))
         ;(ADD-CONSEQ (HAS $(TO)  $(OBJECT) )))
        ((and (is-a  $(TO)   'container)
              (eq $(OBJECT)  (component-of $(TO) )))
         (add-conseq (state $(TO)  (if (is-a $(TO)  'faucet)
                                    'flowing
                                    'open) 'neg)))
        ((and (is-a  $(FROM)   'container)
              (eq $(OBJECT)  (component-of $(FROM) )))
         (add-conseq (state $(FROM)  (if (is-a $(FROM)  'faucet)
                                      'flowing
                                      'open) 'pos))))
  ;;; The following says that when you propel an objects to a person
  ;;; that person then has the object. It was obviously intended for 
  ;;; throwing balls. However, if the threat-plan is ever run, then
  ;;; when the threatener carries out a threat and propels her fist
  ;;; to the other person, the other person then has the hand! [cox 8aug93]
  (when (and $(TO)  (is-a $(TO)  'person))
    (ADD-CONSEQ (HAS $(TO)  $(OBJECT) ))
    (add-conseq (is-at $(OBJECT)  $(TO) )))
  (when (and $(FROM)   (is-a $(FROM)  'person))
        (ADD-CONSEQ (negate (HAS $(FROM)  $(OBJECT) )))  ;one more problem and you are gone
        (ADD-CONSEQ (negate (is-at $(OBJECT)  $(FROM) ))))
	                        ;; [mdd 4apr94]  Dog feels threatend if someone throws 
	                        ;; something at it
  (when (is-a $(to) 'dog) (ADD-CONSEQ (state $(to) 'fear 'pos)))
)


;;;;;;;;;;;;;;;;
;;;  PTRANS
;;;;;;;;;;;;;;;;

(defun PTRANS (ACTOR OBJECT TO FROM)
  (make-old-cd :head 'PTRANS :ACTOR ACTOR :OBJECT OBJECT :TO TO :FROM FROM))

;;; 
;;; |||||| One problem with this precondition is that when Elvis is under the
;;; control of the police he will not be able to move with them when they take
;;; him away.  Fixed this by allowing the controlled person to ptrans himself
;;; only to the location of the person that controls him. That way he can follow
;;; his controller.[cox 27aug93]
;;;
;;; Another problem fixed [cox 15&16sep93] was Elvis' moving and retrieving his
;;; pipe from the cupboard after the cops take his pot if they do not arrest
;;; him. Solved this by making PTRANS sensitive to illegal objects in the
;;; presence of the authorities.
;;; 
(def-preconds ptrans
  ;;Actor must have object to move it precondition. [cox 12aug93]
  ;;No, actor must be moving himself and not under control by another person (e.g., police)
  ;;or must be at the same location as the object, and no other agent can have control 
  ;;over it. [cox 13&17aug93]
  (cond
    ((and
       (if (under-anothers-control-p
	       $(actor)
	       $(actor))
	   (let ((master (person-controls? 
			   $(ACTOR))))
	     (if (eq $(ACTOR) $(OBJECT))	;He is moving himself and
		 (or (eq $(TO)  (loc master))	;and he is going to the location 
		     (eq $(TO)  master))	;of his master
		 (eq $(to) master)))		;or he is giving something to the master.
	   t)					;else ok
       (if (police-present-p $(actor))
	   (or (is-a $(to) 'police)		;The object is going to the police
	       (not (is-a $(object)
			  'illegal-item)))	;or not illegal.
	   t)					;else ok
       (or (not $(FROM))
	   (IS-PROX? $(ACTOR)			;Actor is near place object is being moved from
		     (LOC-NAME-OF
		       $(FROM))))
       (or (not
	     (under-anothers-control-p		;and controls the object himself.
	       $(object)
	       $(actor)))
	   (eq $(actor) $(object)))))		;or he is moving himself. That case is handled above.
    (t	;Otherwise report that a precondition is not satisfied & provide feedback to user.
     (if *TDebug-On*
	 (format? *tspin-stream* "==> "))
;;;       (break)
     (if (not (IS-PROX? $(ACTOR)  (LOC-NAME-OF $(FROM) )))
	 (say (negate (is-at $(ACTOR)  (LOC-NAME-OF $(FROM) ))))
	 (say (has (knows-owner 'world $(OBJECT) ) $(OBJECT) )))
     nil))
  )
   

;;;
;;; |||||| What is the difference between knows? and knows-if ??? [cox]
;;;
(def-conseqs ptrans
  (cond (;; The following clause handles ptransing drinks into cups. [cox 12aug93]
	 (and (is-a $(OBJECT)  'drink)
	      (is-a $(TO)  'cup))
	 (add-conseq (bi-state $(TO)
			       $(OBJECT)
			       'filled 'pos)))
	;; The following clause handles ptransing water from a faucet.
	;; X is first item the world knows is at location in to slot. [cox 12aug93]
	((let ((X (knows-whats-at-loc 'world $(TO) )))
	   (when (and (is-a $(TO)  'faucet)
		      (knows? 'world (state $(TO)  'flowing 'pos))
		      (is-a x 'liquid))
		 (if (eq (get $(TO)  'temperture) 'cold)
		     (add-conseq (bi-state $(OBJECT)
					   x
					   'filled 'pos))
		   (add-conseq (state  $(OBJECT)  'dirty 'neg))
		   )
		 t)))
	;; The following clause handles ptransing lit lighters to objects.
	;; |||||| Should make sure that the filler of to is flamable. [cox 12aug93]
	((and (is-a $(OBJECT)  'lighter) ;Added clause [cox]
	      (knows? 'world (state $(OBJECT)  'on 'pos)))
	 (add-conseq (state $(TO)  'burning 'pos))))
  ;; The following clause should be done regardless, whether or not any clauses
  ;; above are executed. Thus it was removed from cond function. [cox 2aug93]
  ;; Of course this inference is not useful when the to and from slots are
  ;; the same (and because of an interaction with addfact, it is catastrophic)
  ;; hence the if clause. [cox 11aug93]
  (when (not (eq $(TO)  $(FROM) ))
	(ADD-CONSEQ (IS-AT $(OBJECT)  $(TO) ))
	(when (and $(TO)  $(FROM) )
	      (ADD-CONSEQ (negate (is-at $(OBJECT)  $(FROM) ))))
	(COND ((NOT (eq $(ACTOR)  $(OBJECT) ))
	       (when (and $(TO)  $(FROM) )
		     (has $(ACTOR)  $(OBJECT) ) ; Added [cox 11aug93]
		     (ADD-CONSEQ (negate (is-at $(ACTOR)  $(FROM) ))))
	       (ADD-CONSEQ (IS-AT $(ACTOR)  $(TO) )))
	      (T NIL)))
  
  )




;;;;;;;;;;;;;;;;
;;;  TILT
;;;;;;;;;;;;;;;;

(defun tilt (ACTOR OBJECT TO FROM)
  (make-old-cd :head 'tilt :ACTOR ACTOR :OBJECT OBJECT :TO TO :FROM FROM))


;;; Actor must have a container to pour ingredients into
;;; and ingredients to pour.
;;; 
(def-preconds tilt
  (cond ((and $(to) $(object))
	 (and (does-have-p $(to) $(actor))
	      (does-have-p $(object) $(actor))))
	($(object)
	  (does-have-p $(object) $(actor)))
	($(to)
	  (does-have-p $(to) $(actor)))
	(t t))
  )

  
;;; Tilt is representation for pouring. [cox]
;;;
;;; Added does-have-p requirement since random events can change the world
;;; in mid-plan. [cox 11aug93]
;;;
;;; Removed the does-have-p since doit now checks new tilt preconditions
;;; above. [cox 13sep93]
;;;
(def-conseqs tilt
  (cond ((or
	   (and (is-a $(OBJECT)  'drink)
		(is-a $(TO)  'cup))
	   (and (is-a $(OBJECT)  'plant)		;Added [cox]
		(is-a $(TO)  'pipe)))
         (add-conseq (bi-state $(TO)
			       $(OBJECT)
			       'filled 'pos)))))


;;;;;;;;;;;;;;;;
;;;  TURN
;;;;;;;;;;;;;;;;

(defun TURN (actor object state)
  (make-old-cd :head 'TURN :actor actor :object object :to state))

(def-conseqs TURN
  (notice $(ACTOR)  (add-conseq (state $(OBJECT)  'on (if (eq $(TO)  'on)
						       'pos 'neg)))))


;;; To cover Elvis' habit. [cox 29jul93]
;;;
(def-reaction jonesing
  (let ((actor $(CON ACTOR) ))
    (add-plan (make-plan 'reaction (s-jonesing actor)))))


(def-reaction hungry
  (let ((actor $(CON ACTOR) ))
    (add-plan  ;; [cox 10aug93]
     (make-plan 'reaction (s-hunger actor)))))

(def-reaction thirsty
  (let ((actor $(CON ACTOR) ))
    (add-plan  ;; [cox 10aug93]
     (make-plan 'reaction (s-thirst actor)))))

;;;
;;; [cox 4jun95]
;;; 
(def-reaction sad
  (let ((actor $(CON ACTOR) ))
    (add-plan  ;; [cox 10aug93]
     (make-plan 'reaction (s-happiness actor)))))


(def-reaction concerned
  (let ((actor $(CON ACTOR) ))
    ;; |||||| What emotion causes an officer to bust someone? [cox 9aug93]
    ;; Dissatisfaction is better than boredom. [cox 13aug93]
    ;; Changed dissatisfaction to concerned. [cox 17aug93]
    (if (eq actor 'officer1) 
	(add-plan			
	 (if (< (random 100) 50)		; 50-50 chance
	     (make-plan 'reaction (s-k-9-bust actor 'police-dog1 'elvis))
	     (make-plan 'reaction (s-bust actor 'elvis))))))
  )


(def-reaction wants
  (let ((actor $(CON ACTOR) ))
    (if (and (eq actor 'officer1)
	     (eq $(CON VAL) 'cont))
	(add-plan			
	 (if (< (random 100) 50)		; 50-50 chance
	     (make-plan 'reaction (s-k-9-bust actor 'police-dog1 'elvis))
	     (make-plan 'reaction (s-bust actor 'elvis))))))
  )


;;; |||||| Update.
;;;
(def-reaction bored
  (let ((actor $(CON ACTOR) ))
    (if (eq actor 'officer1) ;;; |||||| What emotion causes an officer to bust someone? [cox 9aug93]
	(add-plan			;; [cox 10aug93]
	 (make-plan 'reaction (s-bust actor 'elvis)))
      (add-plan				;; [cox 10aug93]
       (make-plan 'reaction (s-entertain actor))))))


(defun PROMISE-CONSEQS (X XDO Y YDO)
  (do-break  PROMISE-CONSEQS )
  (LET ((A (CAUSE YDO (AFFIRM XDO))))
    (COND ((not (RELATE X y X 'trust))
           (ADD-action
               (MBUILD X 
                       (CAUSE YDO 
                              (FUTURE (MTRANS X (STATE Y 'SMART 'NEG) Y X)
                                      ))))
           (ADD-action (MTRANS X A Y X)))
          ((RELATE X X Y 'LIKE)
           (ADD-action (MBUILD X A))
           (ADD-action (MTRANS X A Y X)))
          (T (ADD-action (MTRANS X (negate A) Y X))))))


(defun REQUEST-CONSEQS (X Y Z)
  (do-break  REQUEST-CONSEQS )
  (COND ((OR (NOT (RELATE Y Y X 'LIKE)) (RELATE Y x Y 'fear))
         (add-action (MTRANS Y (negate (future Z)) X Y)))
        ;;(PLAN Y (FUTURE (MTRANS Y (negate Z) X Y))))
        ;;(T (PLAN Y Z)))))
        ((not (eq y (cdpath '(actor) z))) nil)
        (t (case (old-cd-head z)
             (mtrans
                 (let ((a (knows? y (cdpath '(object) z))))
                   (cond (a (add-action (mtrans y a x y)))
                         (t
                          (add-action
                              (mtrans y (negate (mloc y (cdpath '(object) z)))
                                      x y))))))
             (t (add-action z))))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -------------- Misc CD constructors -------------- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; 
;;; Explanations.
;;;
;;; See file rep_meta-xps.lisp for these and more explanation types in frame
;;; representation format.
;;;

;;; Generalized action X causes action Y.
(defun CAUSE (X Y) (make-old-cd :head 'CAUSE :ANTE X :CONSEQ Y))


;;;
;;; Actor does consequent action, Y, because of the antecedent action, X, the
;;; actor performed earlier. This is the XP to represent "The dog barked
;;; because the dog detected the marijuana."
;;;
(defun SELF-CAUSE (X Y) (make-old-cd :head 'SELF-CAUSE :ANTE X :CONSEQ Y))


;;; 
;;;-----------------------------------------------------------------------------
;;; People participate in actions that result in states that they want.
;;;-----------------------------------------------------------------------------
;;; Given a person doing something that results in a state, assert that he wants
;;; that state, via a rational plan choice.
;;;
;;; This is the representation of the PAM chain.  People have a goal to achieve
;;; a state, so they consider their goal, decide to do an action that they know
;;; will result in that state, and do it.  To expand this further, we must
;;; explain how they knew that the action would result in that state, and how
;;; they selected this action over the others.
;;;
(defun XP-GOAL-OF-OUTCOME->ACTOR (actor main-action goal)
  (make-old-cd :head 'xp-goal-of-outcome->actor
	       :ACTOR actor
	       :ANTE goal
	       :CONSEQ main-action))


;;; Changed [cox 30jun95]
;(defun XP-INSTRUMENTAL-SCENE->ACTOR (actor main-action goal)
;  (make-old-cd :head 'xp-instrumental-scene->actor
;	       :ACTOR actor
;	       :ANTE goal
;	       :CONSEQ main-action))


;;;
;;; Actor does action because it is instrumental in causing another action.
;;; 
(defun XP-INSTRUMENTAL-SCENE->ACTOR (actor instrumental-action goal)
  (make-old-cd :head 'xp-instrumental-scene->actor
	       :ACTOR actor
 	       :ANTE goal
	       :CONSEQ instrumental-action))

					 
;;; [cox 28feb95]
(defun XP-DEFENSIVE-BARK (actor object main-action previous-action)
  (make-old-cd :head 'xp-defensive-bark
	       :ACTOR actor
	       :OBJECT object			; threatening-object
	       :CONSEQ main-action		; barking
	       :ANTE previous-action		; threatening-action
	       ))

					 

;;;
;;; ACTION X OF ACTOR CAUSES STATE Y.
;;;
;;; Answers "How did state (conseq) come about?" or "Why does this state
;;; exist?" Answer: "State conseq came about because it resulted from action
;;; ante." [cox 12feb95]
;;; 
(defun RESULTS-IN (X Y) (make-old-cd :head 'RESULTS-IN :ANTE X :CONSEQ Y))



(defun call-on-phone ()
  (make-old-cd :head 'call-on-phone :OBJECT 'phone1))

;;; Added [cox 8aug93]
;;;
(defun police-arrive (officer &optional to from)
  (do-break police-arrive)
  (make-old-cd :head 'police-arrive :ACTOR officer :TO TO :FROM FROM))

;;; Added [cox 25aug93]
;;;
(defun k-9-squad-arrive (officer &optional to from)
  (do-break k-9-squad-arrive)
  (make-old-cd :head 'k-9-squad-arrive :ACTOR officer :TO TO :FROM FROM))

;;; Added [cox 8aug93]
;;;
(defun dog-barks (dog &optional object)
  (make-old-cd :head 'dog-barks :ACTOR dog :OBJECT object))

(defun DOOR (object)
  (make-old-cd :head 'DOOR :object object))

(defun WANTS (ACTOR GOAL)
  (make-old-cd :head 'WANT :ACTOR ACTOR :OBJECT GOAL))

(defun HAS (ACTOR OBJECT)
  (affirm (make-old-cd :head 'CONT :ACTOR OBJECT :VAL ACTOR)))


;;; Added the affirmation because ... |||||| explain the location inference probl. 
;;; [cox 10aug93]
;;;
(defun IS-AT (ACTOR LOC)
  (affirm (make-old-cd :head 'LOC :ACTOR ACTOR :VAL LOC)))


(defun STATE (ACTOR ST MODE)
  (do-break state)
  (make-old-cd :head ST :ACTOR ACTOR  :MODE (LIST MODE)))

;;; Seems to be only like, fear and trust.
(defun RELATION (ACTOR OBJECT REL MODE)
  (make-old-cd :head REL :ACTOR ACTOR :TO OBJECT :MODE (LIST MODE)))

(defun bi-state (ACTOR OBJECT REL MODE)
  (do-break bi-state)
  (make-old-cd :head REL :actor ACTOR :val OBJECT :MODE (LIST MODE)))


;;; Had to make the mode pos or memquery would retrieve 
;;; locations where the actor is NOT when given the returned
;;; cd by knows-loc. [cox 10aug93]
;;;
(defun WHERE-IS (X)
  (make-old-cd :head 'LOC :ACTOR X :VAL '?UNSPEC :mode '(pos)))

;;; Ditto [cox 12aug93]
(defun WHATs-at (X)
  (affirm (make-old-cd :head 'LOC :ACTOR '?UNSPEC :VAL x)))

;;; Ditto as per above comment on Where-is. [cox 12aug93]
;;;
;;; |||||| Probably need to affirm most of these functions that produce a 
;;; pattern to represent some query to the data base.
;;;
(defun WHO-HAS (X)
  (affirm (make-old-cd :head 'CONT :ACTOR X :VAL '?UNSPEC)))

(defun open-status (X)
  (make-old-cd :head 'open :object X :mode '?UNSPEC))

(defun achieve (actor goal)
  (make-old-cd :head 'achieve :actor actor :object goal))

;  (cond ((member 'toward (mode cd)) cd)
;        (t (set-role 'mode (cons 'toward (mode cd)) cd))))

;;; Question were signified by simply making the mode of a cd with 'ques in ICU. [cox 2aug93]
(defun QUESTION (CD)
  (make-old-cd :head 'QUERY :con cd :object 'mode))


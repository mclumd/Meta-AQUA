;;; -*- Mode: LISP; Syntax: Common-lisp; Package: TALE-SPIN; Base: 10 -*-
;;; $Rev$

;;; This file was corrupted when I tried to play with Meta-AQUA in 1999. I
;;; restored the code by downloading it from the FreeWare Tools. I then
;;; remembered the code in the /usr/loca/mcox/Archives/cleon.tar. I made the
;;; copy consistent with that version. 14jan99

;;; ===========================================================================
;;; TaleSpin
;;; Copyright (C) 1987 James R. Meehan
;;; ===========================================================================
;;;
(in-package :tspin) ;Added Jan99

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	    Elvis World and the Tale-Spin Story Generation Subsystem
;;;;				 for Meta-AQUA
;;;;
;;;;	               Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;				File: data.lisp
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



;;; Is list of places agents and objects can be in the world [cox]
;;; Used to be a defvar statement in file tspin.lisp with an assignment of
;;; these variables in init-world. This value is appended to *objects* (or
;;; *new-objects* from alt-story) and 'outside removed then assigned to 
;;; *all-objects*. The only other place *all-locations* is used is by function
;;; new-loc where it chooses the first location not equal to the location of the
;;; actor passed to it. [cox 8aug93]
;;;
;;; |||||| Need to make this parameter changeable by alt-story. [cox 8aug93]
;;; 
;;; Garage, kitchen and outside are the only items defined to be is-a loc.
;;; Added garage to this list on [cox 24aug93]
;;; Made fridge, cupboard and outside tokens.
(defparameter *all-locations* 
  '(kitchen cupboard1 sink		; ||||||Why not floor1 here?[cox 5aug93]
    stove fridge1			; or garage ? [cox 19aug93]
    garage  outside1))			; Outside seemed missing. [cox 8aug93]
                                        ;; or rug1 or laundry-pile1 [cox 23oct94]

(defvar *components* '(light-switch1 orange-peel1 orange-peel2
		       banana-peel1 banana-peel2 door-bell-switch1 
		       fridge-door cupboard-door cold-faucet-handle 
		       hot-faucet-handle phone-receiver1 hand hand1 hand2))

;;; I suppose that the reason cold-faucet-handle is not in the list of objects
;;; is that it is a component of cold-faucet. Added components above to cover. 
;;; [cox 24aug93]
;;;
;;; There are some more serious problems with Pazzani's ontology. For example,
;;; often the same identifier will be used for both tokens and types, or at least 
;;; this is implied. Milk is a token in the list of objects below. There is no type
;;; of drink is-a milk, just the token milk. Instead there should be a milk1 is-a 
;;; milk and milk1 should be put in objects, not milk. The same can be said for 
;;; cold and hot faucets. [cox 24aug93] 
;;; Changed milk -> milk1 [cox 31aug93]
;;;
;;; Also missing in this list of objects are nail1 and the rest of the garage 
;;; supplies.
;;;
(defparameter *objects*			;Changed to parameter from var on [cox 24aug93]
  '(cup1 cup2 cup3 cup4 
    glass1 glass2 glass3 glass4
    apple1 apple2 orange1 orange2 banana1 banana2
    mashed-potatoes1				; [cox 28dec94]
    table1 table2 basket1 
    ganja1 pipe1 pipe2 lighter1
    pipe-tobacco1					; [cox 23feb 95]
    hot-faucet cold-faucet milk1
    cold-water1 hot-water cold-water2 
    jack cat1
    vase1 vase2 vase3			;Added the following [cox 24aug93]
    calla-lilly1 calla-lilly2
    rose-bush1 rose-bush2
    ball1 ball3 ball2			
    balloon1 balloon2 balloon3	
    nail1 board1 hammer 
    window1 grass			
    door-bell1 floor1 phone1 light1	;Objects from random events. Added [cox 25aug93]
    dog1 smoke1 air1			;Object from new drug bust random event. Added [cox 25aug93]
    air2				;From inflate. Made air token. Change later.||||||[cox 25aug93]
    police-dog1				;New police events. [cox 25aug93]
    pan1				;[cox 2sep93]
	laundry-pile1       ; [mdd 22mar94]
	rug1
    ))


;;; Since the macro "a" accesses *facts* directly instead of their instantiations 
;;; as property list created during the call to init-world, we have modified the macro
;;; to use this pointer. That way one can substitute various set of facts for different 
;;; scenarios. See init-world also. [cox 6aug93]
;;;
(defvar *facts-pointer* nil)

;;; The facts are of the following form: (property symbol value). 
;;; The function init-world sets the property list appropriately. [cox 1aug93]
;;; Included is these facts are the components of the isa hierarchy. [cox 7aug93]
;;; 
;;; Added elvis and drug facts facts [cox 29jul93]
;;;
(defparameter *facts*
    '((age karen 4) 
      (age lynn 11)			; Changed age to 11 (she smokes) [cox 12jun95]
      (age Mom 25) (age Sheryl 25)(age dad 29) (age elvis 42) 
    (mother karen mom)(mother lynn mom)(father lynn dad)(father karen dad)
    (husband mom dad)(wife dad mom)
    (gender dad male)(gender lynn female)(gender karen female)(gender mom female)
    (gender sheryl female) (gender elvis male)
    (hair mom brown)
    (hair sheryl blond)(hair dad brown)(hair karen blond)
    (hair lynn blond)(hair elvis brown)
    ;; Added children and adults [cox 10feb95]
    (is-a adult person)
    (is-a child person)
    ;; Made all persons adults except for lynn and karen [cox 10feb95]
    (is-a mom adult)
     ;Changed name jill to cat1 because when mumble wants to say "the jill" [cox]
     (is-a cat1 cat) 
     (hair cat1 auburn)
     (is-a jack cat)(hair jack white-and-black)
     (is-a sheryl adult)(is-a dad adult)(is-a karen child)(is-a lynn child)
     (is-a elvis adult)
     (is-a pipe-tobacco1 tobacco)			; [cox 23feb 95]
     (is-a tobacco plant)			; [cox 23feb 95]
     (is-a ganja1 marijuana)
     (is-a marijuana plant)
;;;      (is-a ganja1 drug)			;||||||Want to make this marijuana but there is some place
     (is-a drug illegal-item)      ;in the tspin code where it compares for drug i think. [cox 25aug93]
     (is-a pipe1 pipe)(is-a pipe2 pipe)
     (is-a pipe illegal-item)			;In our sense of the word pipe.
     (is-a dog1 dog)
     (is-a police-dog1 dog)
     (is-a officer1 police) (is-a police adult)
     (gender officer1 male)
     (is-a lighter1 lighter)
     (is-a smoke1 smoke)
     (is-a air1 air)
     (is-a air2 air)
     (composition lighter1 plastic)
     (composition pipe1 glass)(composition pipe2 plastic)
     (is-a mashed-potatoes1 food)		; Added to put in fridge for cold edibles. [28dec94]
     (is-a apple1 food) (is-a apple2 food) (is-a orange1 food) 
     (is-a orange2 food) (is-a banana1 food) (is-a banana2 food)
     (is-a table1 table) (is-a table2 table)
     (color ganja1 green) (color pipe1 clear)
     (color pipe-tobacco1 brown)			; [cox 23feb 95]
     (color pipe2 red)(color lighter1 blue)
     (color  apple1 red) (color  apple2 red) (color orange1 orange)
     (color orange2 orange) (color banana1 yellow) (color  banana2 yellow)
     (color table1 natural) (color table2 white)
     (shape  apple1 round)(shape  apple2 round)
     (shape  orange1 round)(shape  orange1 round)
     (shape  orange2 round) (shape  banana1 long)(shape  banana2 long)
     (complexion karen dark)(complexion mom freckled)
     (complexion sheryl heavy-makeup)(complexion lynn freckled)
     (complexion dad dark)(complexion elvis dark)
     (composition vase1 glass)(composition vase2 glass)
     (composition glass1 glass)(composition glass2 glass)
     (composition glass3 glass)(composition glass4 glass)
     (composition cup1 plastic)(composition cup2 plastic)
     (composition window1 glass)
     (is-a window1 window)
     (color window1 clear)
     (composition cup3 plastic)(composition cup4 plastic)
     (composition vase3 plastic)
     (composition floor1 tile)
     (hardness tile 7)
     (color cup1 clear)(color cup2 clear)(color cup3 red)(color cup4 red)
     (color glass1 clear)(color glass2 clear)
     (color glass3 red)(color glass4 red)
     (color vase1 clear)(color vase2 blue)(color vase3 red)
     (size cup1 large)(size cup2 small)(size cup3 large)(size cup4 small)
     (size glass1 large)(size glass2 small)
     (size glass3 large)(size glass4 small)
     (size vase1 large)(size vase2 small)(size vase3 large)
     (is-a cup1 cup)(is-a cup2 cup)(is-a cup3 cup)(is-a cup4 cup)
     (is-a vase1 vase)(is-a vase2 vase)(is-a vase3 vase)
     (is-a glass1 cup)(is-a glass2 cup)(is-a glass3 cup)(is-a glass4 cup)
     (is-a water liquid)
     (is-a drink liquid)
     (is-a hot-water water)		;; |||||| But it can be a drink too.[cox 20aug93]
     (is-a cold-water1 drink)
     (is-a cold-water2 drink)
     (is-a milk drink)			;; |||||| But it is a food too. [cox 20aug93]
     (is-a milk1 milk)
 
     (is-a light-switch1 switch)
     (is-a light1 light)
     (component light1 light-switch1)
     (component-of light-switch1 light1)

     (is-a orange-peel1 peel)
     (is-a orange-peel2 peel)
     (is-a banana-peel1 peel)
     (is-a banana-peel2 peel)

     ;; Persons have hands.  [cox 27aug93]
     ;; |||||| But must I do this for each individual in the scenario.
     ;; There is also the problem of multiple components. Pazzani never 
     ;; has more than one component per token.
     (component person hand)   	; I am not sure that the hand type should be a component.  [cox 24oct94]
     (component-of hand person)
     (component elvis hand1)
     (is-a hand1 hand)
     (component-of hand1 elvis)
     (component officer1 hand2)
     (component-of hand2 officer1)
     (is-a hand2 hand)

     (component orange1 orange-peel1)
     (component-of orange-peel1 orange1)
     (component orange2 orange-peel2)
     (component-of orange-peel2 orange2)
     (component banana1 banana-peel1)
     (component-of banana-peel1 banana1)
     (component banana2 banana-peel2)
     (component-of banana-peel2 banana2)
     (is-a basket1 container)
     (is-a door-bell-switch1 switch)
     (is-a door-bell1 door-bell)
     (component door-bell1 door-bell-switch1)
     (component-of door-bell-switch1 door-bell1)
     (temperture hot-faucet hot)
     (temperture cold-faucet cold)

     (is-a floor1 floor)
     (is-a cupboard container)
     (is-a cupboard1 cupboard)		;[cox 25aug93]
     (is-a cold-faucet faucet)
     (is-a hot-faucet faucet)
     (is-a faucet container)
     (is-a fridge container)
     (is-a fridge1 fridge)		;[cox 25aug93]
     (is-a phone1 phone)
     (component fridge1 fridge-door)
     (is-a fridge-door door)     
     (is-a cupboard-door door)
     (component-of fridge-door fridge1)
     (component cupboard1 cupboard-door)
     (component-of cupboard-door cupboard1)
     (component cold-faucet cold-faucet-handle)
     (component-of cold-faucet-handle cold-faucet)
     (component hot-faucet hot-faucet-handle)
     (component-of hot-faucet-handle hot-faucet)
     (is-a  hot-faucet-handle handle)
     (is-a  cold-faucet-handle handle)
     (component phone1 phone-receiver1)
     (is-a  phone-receiver1 receiver)
     (is-a ball1 ball)
     (is-a ball2 ball)
     (is-a ball3 ball)
     (is-a balloon1 balloon)
     (is-a balloon2 balloon)
     (is-a balloon3 balloon)
     (is-a grass plant)
     (is-a calla-lilly1 plant)
     (is-a calla-lilly2 plant)
     (is-a rose-bush1 plant)
     (is-a rose-bush2 plant)
     (is-a hammer tool)
     (is-a nail1 nail)
     (is-a garage loc)     
     (is-a kitchen loc)     
     (is-a outside1 loc) 
     (is-a board1 board)
     (is-a pan1 pan)
     (composition board1 wood)
     (color ball1 red)(color ball2 blue)(color ball3 white)
     (shape ball1 round)(shape ball2 round)(shape ball3 round)
     (color grass green)(shape grass pointed)
     (color rose-bush1 white)(shape rose-bush1 pointed)
     (color rose-bush2  red)(shape rose-bush1 pointed)
     (color calla-lilly1 white)(shape calla-lilly1 rounded)
     (color calla-lilly2 white)(shape calla-lilly2 rounded)
     (component-of phone-receiver1 phone1)
;; Hiding-place is a more general form of container, useful so things
;; can be hidden in other ways - i.e. by being underneath other things 
;; [mdd 22mar94]
     ;; Had to comment out the next line because this allowed types to be hiding places so during 
     ;; enum-hiding-places the ganga would be placed in locations like cupboard instead of cupboard1.[cox 23oct94]
;;; 	 (is-a container hiding-place)
     ;; Cannot use these at all. See comments on program parameter that takes the place of these isa links
     ;;  --*hiding-places* [cox 25oct94]
;;;      (is-a laundry-pile1 hiding-place)
      (is-a laundry-pile1 laundry-pile)
;;;      (is-a rug1  hiding-place)
      (is-a rug1 rug)
     ;; So added the following 5 hiding places. [cox 23oct94]
;;;      (is-a fridge1 hiding-place)
     ;; Seems I cannot make this cupboard a hiding place since it is
     ;; already a container and I cannot have multiple-inheritance or
     ;; is-a predicate wiull not work properly. [cox 23oct94] 
;;;      (is-a cupboard1 hiding-place) 
;;;      (is-a vase1 hiding-place)
;;;      (is-a vase2 hiding-place)
;;;      (is-a vase3 hiding-place)
))


;;; List with elements of the form (what loc do-print) each passed as 
;;; parameters to now-knows. The instantiation passed to now-knows is 
;;; created by a call of (is-at (first element) (second element)), and 
;;; thus is a loc structure; whereas the third item of each element is 
;;; itself passed to now-knows directly as the print flag.
;;; [cox 5,7&24aug93]
;;;
(defparameter *everyone-loc-facts*
  
 '( (elvis kitchen t)
    (dog1 kitchen t)
    (basket1 kitchen t)				;[cox 16sep93]
    (window1 outside t)				;[cox 16sep93]
	;;    (ganja1 fridge1 t)         ; Ganja location is randomized now [mdd 22mar94]
    (pipe1 cupboard1 t)
    (pipe2 cupboard1 t)
    (pan1 cupboard1 t)
    (lighter1 table2 t)
    (pipe-tobacco1 table1 t)			; [cox 23feb 95]
    (lynn kitchen t)
    (karen kitchen t)
    (mom kitchen t) 
    (dad kitchen t)
    (floor1 kitchen t)
    (cupboard1 kitchen t)
    (fridge1 kitchen t)
    (sink kitchen t)
    (stove kitchen t)
;    (table kitchen t)
    (cup1 cupboard1 t)
    (cup2 cupboard1 t)
    (cup3 cupboard1 t)
    (cup4 cupboard1 t)
    (glass1 cupboard1 t)
    (glass2 cupboard1 t)
    (glass3 cupboard1 t)
    (glass4 cupboard1 t)
    (cat1 table1  t)
    (phone1 kitchen t)
    (doorbell kitchen t)
    (table1 kitchen t)
    (table2 kitchen t)
    (doorbell-switch1 outside1 t)
    (vase1 table2 t)
    (vase2 table1 t)			;Why is vase on table but fruit is on table1? [cox 12aug93]
					;|||||| Why is not table1 in *all-locations* or *objects*? 
					;Finally resolved this by making table a type, 
					;tables 1&2 tokens [cox 24aug93]
    (vase3 table1 t)
    (jack table2 t)  ;; Jack the cat is on the table [cox 4aug93] 
    (milk1 fridge1 t)
    (mashed-potatoes1 fridge1 t)		; [cox 28dec94]
    (cold-water1 fridge1 t)
    (hot-faucet sink t)
    (cold-faucet sink t)
    (cold-water2 cold-faucet t)
    (apple1 table1 t)
    (apple2 table1 t)
    (orange1 table2 t)
    (orange2 table2 t)
    (banana1 table1 t)
    (banana2 table1 t)
    (ball1 garage t)
    (ball2 garage t)
    (ball3 garage t)
    (hammer garage t)
    (nail1 garage t)
    (board garage t)
    (balloon1 cupboard1 t)
    (balloon2 cupboard1 t)
    (balloon3 cupboard1 t)
    (grass outside1 t)
    (calla-lilly1 outside1 t)
    (calla-lilly2 outside1 t)
    (rose-bush1 outside1 t)
    (rose-bush2 outside1 t)
     
    (hot-water hot-faucet t)
	(laundry-pile1 kitchen t)        ;; [mdd 22mar94]
	(rug1 kitchen t)
	)
 )

;;; 
;;; Program parameter *property-states* lists states that a unary-relations
;;; among objects in the world. An object is in the state of burning or not;
;;; It is not a relation between two objects (e.g., filled-with), or between
;;; an object and some value (e.g., color is a relation between a particular
;;; object (say ball1) and some color value (say blue).  The program,
;;; however, does not seem to use these values. No state, act, or inference
;;; appears to follow from one of these relations.
;;;
;;; The property state relations are special in that they are not just cd
;;; structures, but are also placed on property lists of the objects. For
;;; example, the assertion that ganja1 is burning is represented by the cd
;;; #{BURNING ACTOR=GANJA1 MODE=(POS)}. The value (POS) is also placed on
;;; the BURNING property of the symbol GANJA1.  This property list is
;;; established by the function now-knows when the who parameter is the
;;; world. As an asside, the frame representation is of the burning relation
;;; is (BURNING (DOMAIN (VALUE DRUG.46)) (CO-DOMAIN (VALUE TRUE.0))). One
;;; minor function in file cd-utils.lisp in the Occam code I removed from
;;; Tale-Spin also references the *property-states* list. This function is
;;; remove-ids-and. 
;;;
;;; In addition to the original property states (open broken ring on flowing
;;; tied sharp inflated) I added burning and barking to the list for the
;;; drug bust domain. Also, there were a number of missing relations that I
;;; added for completeness: (dirty peeled bursted flying shattered). They
;;; were needed anyhow to assure correct processing during conversion to
;;; frames by the function convert-2-frame.  It tests for membership in this
;;; global list, and when an affirmative result, calls convert-unary-state.
;;;
;;; |||||| Probably want to remove the use of property lists in ther
;;; representation altogether. The only reservation I have is that the use
;;; of memquery in knows? (knows-if ?) may not fetch the latest state cd, so
;;; property lists may be more accurate as well as efficient. I wonder why
;;; property states were used to begin with.
;;; 
;;; Why is not "filled" in this list? [cox 2aug93]
;;; Because "filled" is a bi-state. Objects are filled with ingredients. [cox 7aug93]
;;;
;;; Note that many of these may have an intensity associated with them 
;;; and are not strictly binary (with the possible exception of on and 
;;; tied and maybe broken). For instance ring could have loudness sharp
;;; could have very-sharp somewhat-sharp, and open could have degree. 
;;; The intensities might be real values or qualitative values.
;;; [cox 24aug93]
;;;
;;; Why is not dirty or peeled in this list? [cox 25aug93]
;;; Or bursted or flying? [cox 2sep93]
;;;
(defparameter *property-states* 
  '(open broken ring on flowing tied sharp inflated
	 burning barking
	 dirty peeled bursted flying shattered))


;;; The states that agents can be in that represent the problems driving
;;; Tale-Spin can also be considered unary-valued relations. Thus we list
;;; them so the convert-2-frame function will also dispatch these to
;;; convert-unary-state.
;;; 
(defparameter *problem-states* 
  '(concerned thirsty hungry jonesing bored sad wants))


;;; Each fact is given to the world and to all agents in *personae* by function 
;;; init-world. Init-world instantiates the fact and passes it to calls of function 
;;; now-knows. The form of the representation for each fact in *world-facts* is 
;;;            (function-name arg1 arg2 ... argn).
;;; Thus the systems can instantiate a fact by the call 
;;;            (apply (first fact) (rest fact)) 
;;; This is the same representation as that of the elements of *saved-random-events*. 
;;; [cox 1&7aug93]
;;;
(defparameter *world-facts*
  '((relation officer1 elvis like pos)
    (relation elvis officer1 like pos)
    (relation lynn elvis like pos)		; [cox 4jun95]
    (relation elvis lynn like pos)		; [cox 4jun95]
    (relation lynn elvis trust pos)		; [cox 4jun95]
    (relation elvis lynn trust pos)		; [cox 4jun95]
    (state cupboard1 open neg)
    (state fridge1 open neg)
    (state stove open neg)
    (state cold-faucet flowing neg)
    (state hot-faucet flowing neg)
    (state cup1 broken  neg) 
    (state cup2 broken  neg) 
    (state cup3 broken  neg) 
    (state cup4 broken  neg) 
    (state glass1 broken  neg) 
    (state glass2 broken  neg) 
    (state glass3 broken  neg) 
    (state vase1 broken  neg) 
    (state vase2 broken  neg) 
    (state vase3 broken  neg) 
    (state glass4 broken  neg)
    (state phone1 ring neg)
    (state light1 on neg)

    ;; Added the following. [cox]
    (state door-bell-switch1 on  neg) ;Was missing. 
    (state pipe1 broken  neg) ;[cox]
    (state pipe2 broken  neg) ;[cox]
    (state lighter1 on  neg) ;[cox]
    (state ganja1 burning neg) ;[cox]
    (state pipe-tobacco1 burning neg) ;[cox 23feb 95]
    (state dog1 barking neg)
    ))


;;; The following events are assigned during init-world to global variable 
;;; *random-events*. They are then randomly chosen from (by function add-random-event) 
;;; at random times (during the say function) for inclusion in the story. [cox 1&7aug93]
;;; The form of the representation for each event is (function-name arg1 arg2 ... argn).
;;; Thus the system can call (apply (first event) (rest event)) to instantiate the 
;;; action (or result state if one is present). This is the same representation as that 
;;; of the elements of *world-facts*. [cox 1&7aug93]
;;;
;;; If new police events are added to the following list, then add them also to 
;;; *random-police-events*. [cox 27aug93]
;;;
(DEFPARAMETER *SAVED-RANDOM-EVENTS* 
  '(
;   (propel elvis vase1 floor1)
;   (s-entertain 'elvis)
;   (state elvis jonesing pos)
   (propel cat1 vase2 floor1)
;   (propel elvis vase3 floor1)
;   (propel dad light-switch1 nil) 
   (propel mom light-switch1 nil) 
;   (propel sheryl door-bell-switch1 nil nil (pos))
   (call-on-phone)
;   (dog-barks dog1)
   (police-arrive officer1)		;[cox 8aug93]
   (k-9-squad-arrive officer1 police-dog1) ;[cox 25aug93]
   ))


;;; Added to augment the add-random-event function.
;;; When one of these events are called by the function, all others 
;;; are removed so that only one per story will occur. [cox 27aug93]
;;;
(defparameter *random-police-events*
	      '((police-arrive officer1)
		(k-9-squad-arrive officer1 police-dog1)))


;;; -----------------
;;  Randomly place contraband in any one hiding place instead of always putting
;;  it in the fridge.  Location is added to global *everyone-loc-facts* during
;;  init-world.
;; 
;;           [mdd 22mar94]
(defun randomize-contraband-loc (item location-list everyone-loc-facts)
  (cons (list item
	      (nth (random (length location-list))
		   location-list)
	      t)
	everyone-loc-facts))


;;  Return list of locations that can be hiding-places
;;  used in above routine - hack eliminates faucet as potential hiding place
;;           [mdd 22mar94]
;;;
;;; There are too many problems with the current representation of
;;; hiding-place, not the least of which is that the isa hierarchy cannot be
;;; multiple inheritance.  So we are changing this code to simply return the
;;; value of a global variable *hiding-places*. [cox 25oct94]
;;; 
(defun enum-hiding-places (fact-list)
;  (let ((object (second (first fact-list))))
;    (cond ((null fact-list)
;	   nil)
;	  ((and (is-a object 'hiding-place)
;		(not (is-a object 'faucet)))
;	   (cons object
;		 (enum-hiding-places (rest fact-list))))
;	  (t
;	   (enum-hiding-places (rest fact-list)))))
  *hiding-places*
  )

;;; [cox 25oct94]
(defparameter *hiding-places*
	      '(fridge1 vase1 vase2 vase3 cupboard1 rug1 laundry-pile1))

;;; -------------------		  


;;; Added optional parameters in order to modify the background knowledge more
;;; easily. [cox 5aug93]
;;;
;;; So that the police officer does not re-enter the story if he is already the
;;; main character, the main-character argument exists in function init-world.
;;; [cox 27jun95]
;;; 
(defun init-world (&optional 
		   main-character
		   (facts *facts*) 
		   (facts-pointer '*facts*)
		   (objects *objects*) 
		   (everyone-loc-facts *everyone-loc-facts*)
		   (world-facts *world-facts*))
  (init-traces)					; [cox 10aug93]
  (setq *random-events* 
    (if (or				; Remove police arrival if necessary [mdd 1apr94]
	 (eq main-character 'officer1)
	 *no-cops*)
	(set-difference
	 *saved-random-events*
	 *random-police-events*
	 :test
	 #'equal)
      *saved-random-events*))
  (setf *facts-pointer* facts-pointer)		; See [cox 6aug93]
  (dolist (x facts)
    (setf (get (second x) (first x)) (third x)))
						;|||||| Why isn't Sheryl in *personae*? [cox 1aug93]
						; She is like the two cats: more like a prop. [cox 18aug93]
  (setf *personae* '(Lynn Karen Mom Dad Elvis)) 
  (dolist (p *personae*)
    (setf (get p 'proper-name) t))
  (setf *goals* 
    '(
;;;      hungry 
	   thirsty
	   bored				;only children are bored? - pushed later
	   ;; Should be curious here.
	   ))
  ;;; Removed the assignment statement to *all-locations* that was here.

  ;;; ||||||This should really be done by running through *all-locations* 
  ;;; and checking against the states in world-facts. [cox 31jul93]
  (setf *closed-locations* '(cupboard1 fridge1 stove))
 
  ;; The objects to precede with "the" when speaking. [cox 7aug93]
  ;; Because I added outside to *all-locations* we need to remove it from 
  ;; this list otherwise mumble will be saying the outside. [cox 8aug93]
  ;; Police was added, but are not actual objects, rather the indeterminate 
  ;; sense of the word is intended. [cox 13aug93]
  (setf *all-objects*
	(remove 'outside1 (append '(police) *all-locations* objects)))
  ;; Remove any old knowledge from previous stories. [cox 5aug93]
  (dolist (x (cons 'world
		   ;; Added officer1 because he had goals that must not persist
		   ;; after a story for goal-eval to work right. [cox 11aug93]
		   (cons 'officer1 
			 *personae*)))
    (setf (get x 'facts) nil
	  (get x 'goals) nil
	  (get x 'demons) nil))
  (init-gen)
  ;; create a random amount of contraband [mdd 1apr94]
  (setf (get 'ganja1 'amount) (random 10))

  (format
    *tspin-stream*
    (str-concat
      "~%Initializing Tale-Spin's knowledge of its world"
      "~%(and the knowledge possessed by the characters in that world).~%"))
  (dolist (fact (randomize-contraband-loc	; place ganja in a random spot [mdd 22mar94]
		  'ganja1
		  (enum-hiding-places
		    (symbol-value facts-pointer))
		  everyone-loc-facts))
    (dolist (who (append '(world police-dog1) *personae*))	; added police dog [mdd 24mar94]
;;    (dolist (who (cons 'world *personae*))
      (now-knows who
		 (is-at (first fact) (second fact))
		 (if (eq who 'world)
		     (and *say-facts* (third fact))
		     nil))))
  
  (dolist (fact world-facts)
    (dolist (who (cons 'world *personae*))
      (now-knows who
                 (apply (first fact) (rest fact))
                 (if (eq who 'world)
		     (and *say-facts* (third fact))
		     *say-facts*		; Allow world-facts to be spoken if desired. [cox 7aug93]
						;nil        ; Pazzani's copy did not.
		     )))))


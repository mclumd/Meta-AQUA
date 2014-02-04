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
;;;;			     File: alt-story.lisp
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
;;; This scenario is used to test what happend when the problem is 
;;; one of thirst, but one does not have any drinks in the world.
;;; [cox 6aug93]
;;;



(defvar *new-facts*
  '((age karen 4) (age lynn 6) (age Mom 25) (age Sheryl 25)(age dad 29) (age elvis 42) 
    (mother karen mom)(mother lynn mom)(father lynn dad)(father karen dad)
    (husband mom dad)(wife dad mom)
    (gender dad male)(gender lynn female)(gender karen female)(gender mom female)
    (gender sheryl female) (gender elvis male)
    (hair mom brown)
    (hair sheryl blond)(hair dad brown)(hair karen blond)
    (hair lynn blond)(hair elvis brown)
     (is-a mom person)
     ;Changed name jill to cat1 because when mumble wants to say "the jill" [cox]
     (is-a cat1 cat) 
     (hair cat1 auburn)
     (is-a jack cat)(hair jack white-and-black)
     (is-a sheryl person)(is-a dad person)(is-a karen person)(is-a lynn person)
     (is-a elvis person)
     (is-a ganja1 drug) 
     (is-a pipe1 pipe)(is-a pipe2 pipe)
     (is-a lighter1 lighter)
     (composition lighter1 plastic)
     (composition pipe1 glass)(composition pipe2 plastic)
     (is-a apple1 food) (is-a apple2 food) (is-a orange1 food) 
     (is-a orange2 food) (is-a banana1 food) (is-a banana2 food)
     (is-a table1 table) (is-a table2 table)
     (color ganja1 green) (color pipe1 clear)
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
     (is-a hot-water water)
;     (is-a cold-water1 drink)
;     (is-a cold-water2 drink)
;     (is-a milk drink)
 
     (is-a light-switch1 switch)
     (is-a light1 light)
     (component light1 light-switch1)
     (component-of light-switch1 light1)

     (is-a orange-peel1 peel)
     (is-a orange-peel2 peel)
     (is-a banana-peel1 peel)
     (is-a banana-peel2 peel)
     
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
     (is-a cold-faucet faucet)
     (is-a hot-faucet faucet)
     (is-a faucet container)
     (is-a fridge container)
     (is-a phone1 phone)
     (component fridge fridge-door)
     (is-a fridge-door door)     (is-a cupboard-door door)
     (component-of fridge-door fridge)
     (component cupboard cupboard-door)
     (component-of cupboard-door cupboard)
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
     (is-a outside loc)
     (is-a board1 board)
     (composition board1 wood)
     (color ball1 red)(color ball2 blue)(color ball3 white)
     (shape ball1 round)(shape ball2 round)(shape ball3 round)
     (color grass green)(shape grass pointed)
     (color rose-bush1 white)(shape rose-bush1 pointed)
     (color rose-bush2  red)(shape rose-bush1 pointed)
     (color calla-lilly1 white)(shape calla-lilly1 rounded)
     (color calla-lilly2 white)(shape calla-lilly2 rounded)
     (component-of phone-receiver1 phone1))
)

(defvar *new-objects*
  '(cup1 cup2 cup3 cup4 glass1 glass2 glass3 glass4
    apple1 apple2 orange1 orange2 banana1 banana2
    table1 table2 basket1 
    ganja1 pipe1 pipe2 lighter1
;   hot-faucet cold-faucet milk 
;   cold-water1 hot-water cold-water2 
    jack cat1))

(defvar *new-everyone-loc-facts*
  
 '( (elvis kitchen t)
    (ganja1 fridge t)
    (pipe1 cupboard t)
    (pipe2 cupboard t)
    (lighter1 table t)
    (lynn kitchen t)
    (karen kitchen t)
    (mom kitchen t) 
    (dad kitchen t)
    (floor1 kitchen t)
    (cupboard kitchen t)
    (fridge kitchen t)
    (sink kitchen t)
    (stove kitchen t)
    (table kitchen t)
    (cup1 cupboard t)
    (cup2 cupboard t)
    (cup3 cupboard t)
    (cup4 cupboard t)
    (glass1 cupboard t)
    (glass2 cupboard t)
    (glass3 cupboard t)
    (glass4 cupboard t)
    (cat1 table  t)
    (phone1 kitchen t)
    (doorbell kitchen t)
    (table1 kitchen t)
    (table2 kitchen t)
    (doorbell-switch1 outside t)
    (vase1 table t)
    (vase2 table t)
    (vase3 table t)
    (jack table t)  ;; Jack the cat is on the table [cox 4aug93] 
;    (milk fridge t)
;    (cold-water1 fridge t)
;    (hot-faucet sink t)
;    (cold-faucet sink t)
;    (cold-water2 cold-faucet t)
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
     (balloon1 cupboard t)
     (balloon2 cupboard t)
     (balloon3 cupboard t)
     (grass outside t)
     (calla-lilly1 outside t)
     (calla-lilly2 outside t)
     (rose-bush1 outside t)
     (rose-bush2 outside t)
     
    (hot-water hot-faucet t))
 )


(defparameter *new-world-facts*
  '((state cupboard open neg)
    (state fridge open neg)
    (state stove open neg)
;    (state cold-faucet flowing neg)
;    (state hot-faucet flowing neg)
    (state pipe1 broken  neg) ;[cox]
    (state pipe2 broken  neg) ;[cox]
    (state lighter1 on  neg) ;[cox]
    (state ganja1 burning neg) ;[cox]
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
    ))



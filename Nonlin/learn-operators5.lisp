;;;-*- Mode: LISP; Syntax: Common-lisp; Package: NONLINEAR; Base: 10 -*-

(in-package :nonlin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;		    Nonlin Hierarchical Planning Subsystem
;;;;				 for Meta-AQUA
;;;;
;;;;	     Copyright (C) 1994   Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;			  File: learn-operators5.lisp
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Common Lisp NONLIN - georgia Institute of Technology
;;;;
;;;; Version 1.2, 7/94
;;;; (email cox@cc.gatech.edu for more info)
;;;; 
;;;; LEARN-OPERATORS.LISP: Mental World Operators 
;;;;
;;;; History: 7 july 94; initial mini attempt


;; (note: in the current system, goals have to ground)

(setf *autocond* t)

(reset-schematable)  		;  clear out schema table



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LEARNING-GOAL OPERATOR AND ACTION SCHEMAS
;;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(opschema organize-bk1
	   :todo (knowledge-differentiation-goal !x !y !z)
	   :expansion (
		       (step1 :goal (index-wrt-item !y !z))
		       )
	   :conditions (
			(:use-when (isa person !x) :at step1)
			)
	   :variables (!x !y !z)
	   )


;;;
;;; |||||| I do not know whether the name of this schema (and the one above) is
;;; (are) proper.
;;;
;;; |||||| Is this the place to test whether we abstract, generalize
;;; (transmutation), or change the ontology?
;;;
(opschema organize-bk2
	   :todo (knowledge-reconciliation-goal !x !y !z)
	   :expansion (
		       (step1 :goal (abstracted !y !z))
		       )
	   :conditions (
			(:use-when (isa person !x) :at step1)
			)
	   :effects  (
		      )
	   :variables (!x !y !z)
	   )

;;;
;;; IGNORE this one Gordon. It is a transition definition to the more
;;; realistic set of definitions.
;;; 
(opschema organize-bk3
	   :todo (goal-object !x !y)
	   :expansion (
		       (step1 :goal (abstracted !q !r))
		       )
	   :conditions (
			(:use-only-for-query (domain !q !y))
			(:use-only-for-query (co-domain !r !y))
			(:use-when (instance-of
				     knowledge-reconciliation-goal  !x) :at step1)
			(:use-when (instance-of merged !y) :at step1)
			)
	   :effects  (
		      )
	   :variables (!x !y !z)
	   )


;;;
;;; If a person has the goal to acquire a case, then store it in memory.
;;; 
(opschema acquire-case
	   :todo (knowledge-acquisition-goal !x !y)
	   :expansion (
		       (step1 :goal (stored !y))
		       )
	   :conditions (
			(:use-when (isa person !x) :at step1)
;			(:use-when (isa case !y) :at step1)
			)
	   :variables (!x !y)
	   )

;;;
;;; To store an item in memory, make sure that it is indexed first, then
;;; perform the storage.
;;; 
(actschema do-store
	   :todo (stored !x)
	   :expansion ( (step1 :goal (indexed !x))
			(step2 :primitive (perform-storage !x))
			)
	   :orderings ( (step1 -> step2) ) ;|||||| Is it really necessary?
	   :conditions (
		       )
	  :variables (!x)
;;;           :duration 1 
	   )


;;;
;;; To index some item in memory, first make sure that it is a type (not a
;;; token), then the action is to index that item.
;;; 
(opschema index-op
    :todo (indexed !x)
    :expansion (
		(step1 :goal (isa type !x))
		(step2 :action (index-item !x))
	       )
	  :conditions (
		       )
    :orderings ( (step1 -> step2) )
    :variables (!x)
)


;;;
;;; To generate a type from a token, do explanation-based generalization.
;;; 
(opschema gen-op
	  :todo       (isa type !x)
	  :expansion  (
		       (step1 :action (do-ebg !x))
		      )
	  :conditions (
		       (:use-when (isa token !x) :at step1)
		      )
	  :variables  (!x)
	  )



;;;;;;;;;;;;
;;;
;;; There exist three different indexing actions because for each case the
;;; indexing is performed on a different value. For explanations, the indexing
;;; is performed on the domain of the explains-node of the xp; whereas for
;;; relations that are not xps (note allxps are relations), the indexing is
;;; performed on the domain of the relation; and on all other concepts,
;;; indexing is performed on the item itself.
;;;


;;;
;;; The action of indexing an explanation (XP) is to perform indexing on the
;;; domain of the explains-node of the xp (that is, the explained action) after
;;; any other changes to it take effect (that is, it becomes stable).
;;; 
(actschema do-index-xp
	   :todo       (index-item !x)
	   :expansion  ( 
		        (step1 :primitive (perform-indexing !explained-action))
		       )
	   :conditions (
;			(:unsuperv (changed true !explained-action ;!u   
;					    ) :at step1)
			(:use-when (isa xp !x) :at step1)
			(:use-only-for-query (explains !explains-node !x)
			 :at step1)
			(:use-only-for-query (domain !explains-node !explained-action)
			 :at step1)
			
		       )
	   :effects    (
		        (step1 :assert (indexed !explained-action))
			(step1 :delete (clear true !explained-action))
		       )
	  :variables   (!x !explains-node !explained-action) ; !u)
;;;           :duration 1 
	   )


;;;
;;; The action of indexing an item that is a relation is to perform indexing on
;;; the domain of that relation after any other changes to it take effect (that
;;; is, it becomes stable).
;;; 
(actschema do-index-relation
	   :todo       (index-item !x)
	   :expansion  ( 
		        (step1 :primitive (perform-indexing !relation-schema))
		       )
	   :conditions (
			(:use-only-for-query (isa !y !x))
			(:use-when (isa relation !x) :at step1)
			(:use-when (not (equal xp !y)) :at step1)
			(:use-only-for-query (domain !x !relation-schema) :at step1)
;			(:unsuperv (changed true  !relation-schema ;!u
;					    ) :at step1) 
		       )
	   :effects    (
		        (step1 :assert (indexed !relation-schema))
			(step1 :delete (clear true !relation-schema))
		       )
	  :variables   (!x !y !relation-schema) ; !u)
;;;           :duration 1 
	   )


;;;
;;; The action of indexing an item that is neither an explanation nor a
;;; relation is to perform indexing on that item after any other changes to it
;;; take effect (that is, it becomes stable).
;;; 
(actschema do-index
	   :todo       (index-item !x)
	   :expansion  ( 
		        (step1 :primitive (perform-indexing !x))
		       )
	   :conditions (
			(:use-only-for-query (isa !y !x))
			(:use-when (not (equal relation !y)) :at step1)
			(:use-when (not (equal xp !y)) :at step1)
;			(:unsuperv (changed true !x	; Was !x |||||| Is this now correct? 15oct94
;					    ) :at step1)
		       )
	   :effects    (
		        (step1 :assert (indexed !x))
			(step1 :delete (clear true !x))
		       )
	  :variables   (!x !y )			; !u)
;;;           :duration 1 
	   )



;;;
;;; To index two items with respect to each other, make sure that they are both
;;; indexed independently, then index them jointly.
;;; 
;;; |||||| Do I need orderings on this one?
;;;
;;;
(opschema mutual-index-op
	   :todo        (index-wrt-item !x !y)
	   :expansion ( 
			(step1 :goal (indexed !x))
			(step2 :goal (indexed !y))
			(step3 :action (index-dual-items !x !y))
			)
	   :orderings ( (step1 -> step3)
  			(step2 -> step3))
	   :conditions (
			(:use-when (not (equal !x !y)) :at step1)
			;; May not be correct when we want to reindex,
			;; even if they have been mutually indexed in the past.
;			(:use-when (not (indexed-wrt !x !y)) :at step1)	
		       )
	   :effects   (
		       )
	  :variables   (!x !y)
;;;           :duration 1 
	   )

;;;;;;;;;;;;
;;;
;;; Like indexing actions above, there exist two different mutual-indexing
;;; actions. For each case the mutual-indexing is performed on a different
;;; value: either explanations or non-explanations.
;;;


;;;
;;; The action of indexing two explanations jointly also requires that any
;;; changes to the definition of the parent type of the domains of their
;;; explains-node (that is, the explained-action) be performed before the
;;; indexing is performed.
;;; 
(actschema do-mutual-xp-indexing
	   :todo (index-dual-items !x !y)
	   :expansion ( (step1 :primitive (perform-mutual-indexing !x !y)))
	   :conditions (
		   	(:use-when (indexed !x) :at step1)
			(:use-when (indexed !y) :at step1)
			(:use-when (isa xp !x) :at step1)
			(:use-only-for-query (explains !explains-node !x)
			 :at step1)
			(:use-only-for-query (domain !explains-node !explained-action)
			 :at step1)
;			(:unsuperv (changed true !explained-action ;!u
;					    ) :at step1)
		       )
	   :effects  (
		        (step1 :assert (indexed-wrt !x !y))
		        (step1 :assert (indexed-wrt !y !x))
			(step1 :delete (clear true !explained-action))
		      )
	  :variables (!x !y !explains-node !explained-action) ; !u)
;;;           :duration 1 
	   )


;;;
;;; The action of indexing two (non-explanation) items jointly also requires
;;; that any changes to the definition of their parent type be performed before
;;; the indexing is performed.
;;; 
(actschema do-mutual-indexing
	   :todo (index-dual-items !x !y)
	   :expansion ( (step1 :primitive (perform-mutual-indexing !x !y)))
	   :conditions (
			(:use-only-for-query (isa !q !x))
		   	(:use-when (indexed !x) :at step1)
			(:use-when (indexed !y) :at step1)
			(:use-when (not (equal xp !q)) :at step1)
;			(:unsuperv (changed true !u	;  !z |||||| Is this correct? 15oct94
;					    ) :at step1)
		       )
	   :effects  (
		        (step1 :assert (indexed-wrt !x !y))
		        (step1 :assert (indexed-wrt !y !x))
			(step1 :delete (clear true !u))
		      )
	  :variables (!x !y !z !q !u)
;;;           :duration 1 
	   )


(actschema do-ebg
	   :todo (do-ebg !x)
	   :expansion ( (step1 :primitive (ebg !x)))
	   :conditions (
			(:use-only-for-query (isa !y !x))
		   	(:use-when (isa token !x) :at step1)
			(:use-when (not (equal xp !y)) :at step1)
;			(:unsuperv (changed true !u	; !x |||||| Is this correct? 15oct94
;					    ) :at step1)
		       )
	   :effects  (
		       (step1 :assert (isa type !x))
		       (step1 :delete (isa token !x))
		       (step1 :delete (clear true !u))
		      )
	  :variables (!x !y !u)
;;;           :duration 1 
	   )



(actschema do-xp-ebg
	   :todo (do-ebg !x)
	   :expansion ( (step1 :primitive (ebg !x)))
	   :conditions (
		   	(:use-when (isa token !x) :at step1)
			(:use-when (isa xp !x) :at step1)
			(:use-only-for-query (explains !explains-node !x)
			 :at step1)
			(:use-only-for-query (domain !explains-node !explained-action)
			 :at step1)
;			(:unsuperv (changed true  !explained-action ;!u	
;					    ) :at step1)
		       )
	   :effects  (
		       (step1 :assert (isa type !x))
		       (step1 :delete (isa token !x))
		       (step1 :delete (clear true !explained-action))		      )
	  :variables (!x !explains-node !explained-action !u)
;;;           :duration 1 
	   )

;;;
;;; Perform an abstraction transmutation on relation r1 given relation r2. 
;;; The function raises the co-domain of r1 to the shared parent type of 
;;; r1 and r2. 
;;;
;;; In the example tests below (see comments on var *init-ctxt1*)
;;;          r1 = bark-def 
;;; -> (object (domain dog-bark)(co-domain animate-obj))
;;;          r2 = bark-example 
;;; -> (object (domain dog-bark)(co-domain inanimate-obj)) 
;;;
(actschema do-abstraction-change
	   :todo (abstracted !r1 !r2)
	   :expansion ( (step1 :primitive (abstract-item !r1 !r2))
			)
	   :conditions (
			(:use-when (isa relation !r1) :at step1)
			(:use-when (isa relation !r2) :at step1)
			(:use-when (relation !r1 !r1-type) :at step1)
			(:use-when (relation !r2 !r2-type) :at step1)
			(:use-when (domain !r1 !r1-domain) :at step1)
			(:use-only-for-query (co-domain !r1 !c) :at step1)
			(:use-only-for-query (co-domain !r2 !a1) :at step1)
 			(:use-only-for-query (parent-of !c !c-parent) :at step1)
 			(:use-only-for-query (parent-of !a1 !a1-parent) :at step1)
			(:use-when (equal !r1-type !r2-type) :at step1)
 			(:use-when (equal !c-parent !a1-parent) :at step1)
			(:unsuperv (clear true !r1-domain)  :at step1)
		       )
	   :effects  (
 		      (step1 :assert (co-domain !r1 !c-parent))
;		      (step1 :assert (changed true !r1-domain))
 		      (step1 :delete (co-domain !r1 !c))
;		      (step1 :delete (changed false !r1-domain))
		      )
	  :variables (!r1 !r2 !r1-type !r2-type !r1-domain !c !a1 !c-parent !a1-parent)
;;;           :duration 1 
	   )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TEST PROBLEMS FOR LEARNING-GOAL OPERATORS
;;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;
;;; The initial context state values for testing learning operators in this
;;; file.
;;; 
(defvar *init-ctxt1*
      '(
;;; 	(changed true dummy)
 	(clear true dog-bark)

	(isa type case2)
	(isa case case2)
	(indexed case2)
 
	(isa token case1)
	(isa case case1)

	(isa relation detect-xp)
	(isa token detect-xp)
	(isa xp detect-xp)
	(relation detect-xp because)
	(explains actor1 detect-xp)
	(domain actor1 dog-bark)	; This is not in correct form.
 
	(isa relation threaten-xp)
	(isa type threaten-xp)
	(indexed threaten-xp)
	(isa xp threaten-xp)
	(relation threaten-xp causal-relation)
	(explains actor2 detect-xp)
	(domain actor2 dog-bark)
 
	;; Relates to !r1 in abstraction schema
	;; !r1 = bark-def -> (object (domain dog-bark)(co-domain animate-obj))
	(isa relation bark-def)
	(relation bark-def object)
	(domain bark-def dog-bark)
	(co-domain bark-def animate-obj)        ;Abstraction will delete this.

	;; Relates to !r2 in abstraction schema 
	;; !r2 = bark-example -> 
	;;       (object (domain dog-bark)(co-domain inanimate-obj))
	(isa relation bark-example)
	(relation bark-example object)
	(domain bark-example dog-bark)
	(co-domain bark-example inanimate-obj))
  "Initial context for a Nonlin test problem."
      )


;;;
;;; Problem l1-prob represents the classic test of goal interaction between two
;;; learning goals as described in Cox & Ram, 1994 (EWCBR-94). Reconcile the
;;; bark definition with the bark example while differentiating the new
;;; detection explanation with the old threaten explanation for why dogs bark.
;;; 
(defun l1-prob ()
  "The learning-goal interaction problem."
  (store-always-ctxt '((isa person mike)
		       (parent-of animate-obj physical-obj)
		       (parent-of inanimate-obj physical-obj)))
  (store-init-ctxt *init-ctxt1*)
  (plan-schema l1-prob
	(g1 :goal 
	    (knowledge-reconciliation-goal 
	     mike bark-def bark-example))
	(g2 :goal 
	    (knowledge-differentiation-goal 
	     mike detect-xp threaten-xp))
	))


(defun l2 ()
  "Same as l1-prob but with goal ordering switched."
  (store-always-ctxt '((isa person mike)
		       (parent-of animate-obj physical-obj)
		       (parent-of inanimate-obj physical-obj)))
  (store-init-ctxt *init-ctxt1*)
  (plan-schema l1-prob
	(g1 :goal 
	    (knowledge-differentiation-goal 
	     mike detect-xp threaten-xp))
	(g2 :goal 
	    (knowledge-reconciliation-goal 
	     mike bark-def bark-example))
	))


(defun l3 ()
  "Same as l1-prob but with a single goal."
  (store-always-ctxt '((isa person mike)
		       (parent-of animate-obj physical-obj)
		       (parent-of inanimate-obj physical-obj)))
  (store-init-ctxt *init-ctxt1*)
  (plan-schema l1-prob
	(g1 :goal 
	    (knowledge-differentiation-goal 
	     mike detect-xp threaten-xp))
;	(g2 :goal 
;	    (knowledge-reconciliation-goal 
;	     mike bark-def bark-example))
	))


;;;
;;; Function learn1 is the routine to run pre-selected problems. The default
;;; problem is l1-prob when no arguments are passed, but alternative problems
;;; can be run by passing them as the optional parameter "prob." For example,
;;; (learn 'l2) will run the l2 problem (l1-prob with goal ordering switched).
;;; 
(defun learn1 (&optional (prob 'l1-prob))
  (plan-for :problem prob)
  )


;;;
;;; Some of the following practice goals may be currently obsolete.
;;;

;((knowledge-acquisition-goal mike case1))
;((knowledge-differentiation-goal mike case1 case2))


;;; Goals to be achieved
;((knowledge-reconciliation-goal mike bark-def bark-example)
; (knowledge-differentiation-goal mike detect-xp threaten-xp))
;
;((knowledge-reconciliation-goal mike bark-def bark-example)
; (knowledge-differentiation-goal mike case1 case2))
;
;((abstracted (relation dog-bark animate-obj) (relation dog-bark inanimate-obj)))

;;;-*- Mode: Lisp; Package: NONLIN -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;    Application:  NONLIN
;;;;  Appl. Version:  1.3
;;;;     Appl. Date:  Oct. 1992
;;;;
;;;;           File:  MARK.LISP
;;;;       Contents:  Functions for marking the network.
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
;;;;  -----------------------------------------------------------------------------------   
;;;;  9/22/92   bpk  fixed bug in get-succnodes, get-prenodes; doc/cleanup

(in-package :nonlin)
    
(defun mark (mnode)
    "Mark all nodes in the network regarding temporal ordering with respect to a node."
    ;;; Nodes are marked :before, :after, or :parallel depending on their relation to mnode.
    ;;; Mnode is marked :node.

    (setf *netmarked* mnode)            ; holds node (num) for which net is currently marked
    (setf (node-mark *planhead*) :before)

    ;; first mark all nodes as being :parallel
    (propagate-parallel *planhead*)
    
    ;; next mark all nodes after mnode as :after
    (propagate-after mnode)	

    ;; next mark all nodes before mnode as :before
    (propagate-before mnode)

    ;; mark mnode itself as :node
    (setf (node-mark mnode) :node)
)

    
(defun propagate-parallel (node)
    "Recursively mark ALL nodes in the net with :parallel."
    ;; only this has to be changed if the plan is not in the tasknet form
    (setf (node-mark node) :parallel)       ; mark node
    (for (chnode :in (get-succnodes node))  ; mark all of its immediate successors
	 :do
	 (propagate-parallel chnode))
)
    

(defun propagate-after (node)
    "Recursively mark node and all its successors with :after."
    (setf (node-mark node) :after)         ; mark node
    (for (snode :in (get-succnodes node))  ; mark all of its immediate successors
	 :do
	 (propagate-after snode))
)    


(defun propagate-before (node)
    "Recursively mark node and all its predecessors with :before."            
    (setf (node-mark node) :before)       ; mark node
    (for (pnode :in (get-prenodes node))  ; mark all of its immediate predecessors
	 :do
	 (propagate-before pnode))
)


(defun get-succnodes (node)             ; rewritten 9/22/92 - bpk
   "Return immediate successors of node (as a list of node numbers)."
   ;;; if multiple sets of successors, only returns those in the latest context (the first set)

   (let ((sucs (node-succnodes node)))
      (cond ((null sucs) nil)        ; no elmts
            ((null (car sucs)) nil)   ; first elmt is nil
            ((listp (car sucs))  ; first elmt is nonnil sublist is (context node1 node2 ...)
               (cdar sucs))      ;    so return list (node1 node2 ....)
            (t sucs)             ; list is (node1 node2....)
      )
))


(defun get-prenodes (node)              ; rewritten 9/22/92 - bpk
   "Return immediate predecessors of node (as a list of node numbers)."
   ;;; if multiple sets of predecessors, only returns those in the latest context (the first set)

   (let ((pres (node-prenodes node)))
      (cond ((null pres) nil)        ; no elmts
            ((null (car pres)) nil)   ; first elmt is nil
            ((listp (car pres))  ; first elmt is nonnil sublist is (context node1 node2 ...)
               (cdar pres))      ;    so return list (node1 node2 ....)
            (t pres)             ; list is (node1 node2....)
      )
))


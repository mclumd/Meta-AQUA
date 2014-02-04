;;;-*- Mode: Lisp; Package: NONLIN -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;    Application:  NONLIN
;;;;  Appl. Version:  1.3
;;;;     Appl. Date:  Oct. 1992
;;;;
;;;;           File:  DEV.LISP
;;;;       Contents:  Partial implementation of DEVISER.
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
;;;;  ---------------------------------------------------------------------------------   
;;;;   7/27/93  bpk  fixed package bug
;;;;  11/09/92  bpk  cleanup, doc.
;;;;  11/26/91 bpk - changed name of structure WINDOW to TWINDOW to avoid conflict with
;;;;                 with CLOS classname.  Make/Access/Copy fns. retain OLD names 
;;;;                 (i.e. "window-est" not "twindow-est")

(in-package :nonlin)
 

;;;; Algorithms taken from Vere's DEVISER paper in _Readings in Planning_

(defun modify-start-times (node1 node2)
   "Adjust start time windows."

   ;;; called when two nodes become sequentially ordered
   ;;; to adjust start times if possible
   ;;; Returns t if all goes well, else returns nil (caller must backtrack).
   (prog (d1 est1 lst1 est2 lst2 eft1 lft1 ok
          consecutive duration-estimate)

      ; consecutive-p returns NIL for now since consecutive nodes have
      ; not been implemented in the TF
      (setf consecutive (consecutive-p node1 node2))
;      (setf d1 (convert-duration node1)) ; not implemented
      (setf d1 (snode-duration node1))    ; duration must be a constant
      (setf duration-estimate nil)        ; duration must be actual
      ; convert-duration sets duration-estimate
      (setf est1 (earliest-start-time node1))  
      (setf est2 (earliest-start-time node2))
      (setf ok t)

      ;; adjust est for node2
      (if (and (numberp est1) (numberp est2)) (then
         (setf eft1 (+ est1 d1))
         (if (> eft1 est2) (then
            (set-earliest-start-time node2 eft1)
            (if (not (goal-node-p node2))
               (order-est-against-scheduled-events node2))
            (if (not (propagate-window-compressions node2 node1))
               (setf ok nil)) ; must backtrack if cannot compress a window
         )) ; endif

         ; consecutive nodes not currently implemented
         (if (and ok consecutive (not duration-estimate)
                (not (goal-node-p node1)) 
                (not (revise-consecutive-est node1 d1)))
            (setf ok nil))
      )) ; endif

      ;; adjust lst for node1
      (setf lst1 (latest-start-time node1))
      (setf lst2 (latest-start-time node2))
      (if (and ok (numberp lst2) (numberp lst1)) (then
         (setf lft1 (+ lst1 d1))
         (if (and (> lft1 lst2) (not (equal lst2 *infinity*))
                (not (equal d1 *infinity*))) (then
            (setf lst1 (- lst2 d1))
            (set-latest-start-time node1 lst1)
            (if (not (goal-node-p node1))
               (order-lst-against-scheduled-events node1))
            (if (not (propagate-window-compressions node1 node2))
               (setf ok nil)) ; must backtrack if cannot compress a window
         )) ; endif

         ; consecutive nodes not currently implemented
         (if (and ok consecutive (not duration-estimate)
                (not (goal-node-p node1))
                (not (revise-consecutive-lst node2)))
            (setf ok nil))
      )) ; endif
      (return ok)
  ) ; endprog
   )

#|
; NOT CURRENTLY IMPLEMENTED
(defun revise-consecutive-est (node1 d1)
;;; consider revising the est of node1, which is consecutive to
;;; other nodes.
;;; Returns t if all goes well, else returns nil.

   (prog (est3 est1 est1p ok) 
   ; est1p is est1'

   (setf est1 (earliest-start-time node1))
   (setf ok t)

   ;; for each node3 that immediately follows node1,
   ;;  revise est1 such that est1' + d1 = min(est3) and est1' >= est
   ;;  (ignore est3 if it is a variable)
   (for (node3 :in (consecutive-from node1))
       :do
       (when ok
          (setf est3 (earliest-start-time node3))
          (if (not (variable-p est3)) (then
             (setf est1p (- (earliest-start-time node3) d1))
             (if (not (equal est1 est1p)) 
                (if (< est1p est1) (then
                   (setf ok nil)) ; problem - will have to backtrack
                (else
                   (set-earliest-start-time node1 est1p)
                   (setf est1 est1p)
                   (if (propagate-window-compressions node1 node3) (then
                      (order-est-against-scheduled-events node1))
                   (else
                      (setf ok nil))
                   ) ; endif
                )) ; endif
             ) ; endif
          )) ; endif
       ) ; endwhen
   ) ; endfor
   (return ok)
   ) ; endprog
)


; NOT CURRENTLY IMPLEMENTED
(defun revise-consecutive-lst (node2)
;;; see if any nodes consecutive to node2 should reduce its lst.
;;; if any est's or durations are variables, don't change the
;;; window.
   (progn (maximum-lft this-lft lst3 maximum-possible governing-node
           d3 ok)

   (setf ok t)
   (setf maximum-possible t)
   (setf maximum-lft -1)

   ;; for each node3 that node2 is consecutive to,
   ;;   find the maximum lft <= lst2 (where lft = lst3 + d3)
   ;;   (governing-node is node3 with max lft)
   (for (node3 :in (consecutive-to node2))
      :do
      (when ok
         (setf lst3 (latest-start-time node3))
         (setf d3 (get-duration node3))
         (if (or (variable-p lst3) (variable-p d3)) (then
            (setf maximum-possible nil))
         (else
            (setf this-lft (+ lst3 d3))
            (if (> this-lft maximum-lft) (then
               (setf maximum-lft this-lft)
               (setf governing-node node3)))
         )) ; endif
      ) ; endwhen
   ) ; endfor
   
   ;; now make lst = max. lft (if possible)
   (if (and maximum-possible 
            (< maximum-lft (latest-start-time node2))) (then
      (set-latest-start-time node2 maximum-lft)
;      (if (propagate-window-compressions governing-node) (then))
      (if (propagate-window-compressions node2 governing-node) (then
         (order-lst-against-scheduled-events node2))
      (else
         (setf ok nil))
      ) ; endif
   )) ; endif
   (return ok)
   ) ; endprog
)
|#   

(defun propagate-window-compressions (node caller)
   "Propagates window compressions to node's neighbors."

;;; caller was responsible for compressing the window of node.
;;; check all neighbors of node except caller to see if the change
;;; must propagate.
   (prog (est lst ok upper-neighbors lower-neighbors caller-num)
   
   (setf est (earliest-start-time node))
   (setf lst (latest-start-time node))
   (if caller (setf caller-num (snode-nodenum caller)))

   (if (and (numberp est) (numberp lst) (> est lst)) (then
      (return nil)) ; window for node is closed
   (else
      (setf ok t)

      ;; update nodes ordered sequentially BEFORE node (except caller)
      (setf upper-neighbors 
         (remove caller-num (get-prenodes node)))
      (for (upper-neighbor :in upper-neighbors)
         :do
         (when ok
            (if (not (modify-start-times (allnodes upper-neighbor) node))
               (setf ok nil))
         ) ; endwhen
      ) ; endfor

      (if ok (then
         ;; update nodes ordered sequentially AFTER node (except caller)
         (setf lower-neighbors 
             (remove caller-num (get-succnodes node)))

         (for (lower-neighbor :in lower-neighbors)
            :do
            (when ok
               (if (not (modify-start-times node (allnodes lower-neighbor)))
                  (setf ok nil))
            ) ; endwhen
         ) ; endfor
      )) ; endif
   )) ; endif
   (return ok)
   ) ; endprog
)


(defun order-est-against-scheduled-events (node)
   "Order node with respect to scheduled events using its EST."

;;; see if node should be ordered with respect to any of the scheduled
;;; events, based on its est
   (let ((est (earliest-start-time node))
         (nodenum (snode-nodenum node))
         enode)
      ;; check est against each scheduled event
      (for (e :in *sched-events*)
         :do
         (setf enode (allnodes e))
         (if (not (equal (snode-nodenum enode) nodenum))
         (then
         ;; node follows event if FTe (STe + De) < EST
         (when (< (+ (latest-start-time enode) (snode-duration enode))
                est) 
            (make-prenode enode node)
            (make-succnode node enode)
         ))) ; endwhen
      ) ; endfor
   )
)


(defun order-lst-against-scheduled-events (node)
   "Order node with respect to scheduled events using its LST."

;;; see if node should be ordered with respect to any of the scheduled
;;; events, based on its lst
   (let ((lst (latest-start-time node))
         (dur (snode-duration node))
         (nodenum (snode-nodenum node))
         enode)
      ;; check lst against each scheduled event
      (for (e :in *sched-events*)
         :do
         (setf enode (allnodes e))
         (if (not (equal (snode-nodenum enode) nodenum))
         (then
         ;; node precedes event if STe >= LFTn (where LFTn = LSTn + Dn)
         (when (> (earliest-start-time enode) (+ lst dur))
            (make-succnode enode node)
            (make-prenode node enode)
         ))) ; endwhen
      ) ; endfor
   )
)


(defun determine-ideal-start-times ()
   "Select ideal start time within each node's window."

;;; selects an ideal start time for each node that occurs within
;;; the node's time window
   (prog (est ideal lst node)

   ;; for each node in network,
   (do ((index 0 (1+ index)))
      ((> index (1- *striplen*)))
      (when (not (null (aref *allnodes* index)))
         (setf node (cdar (aref *allnodes* index)))    
         (setf ideal (ideal-start-time node))
         (if (null ideal) (setf ideal (earliest-start-time node)))
         (if ideal (then
            (setf est (earliest-start-time node))
            (if (< ideal est) 
               (then
               (setf ideal est))
            (else
               (setf lst (latest-start-time node))
               (if (> ideal lst)
                  (setf ideal lst))
            )) ; endif
            (set-ideal-start-time node ideal)
            (set-earliest-start-time node ideal)
            (set-latest-start-time node ideal)
            (propagate-window-compressions node nil)
           ) ; endthen
	 ) ; endif
      ) ; endwhen
   ) ; enddo
   (return)
   ) ; endprog
) ; enddefun


(defun consecutive-p (node1 node2)
   "Returns T if nodes are consecutive."
   ;; returns NIL since consecutive activities not implemented now
   nil
)


(defun variable-p (var)
   "Returns T if var is a variable time or duration."
   ;; returns NIL since variables for times and durations not implemented now
   nil
)


(defun goal-node-p (node)
   "Returns T if node has type :goal."

   (equal (snode-type node) :goal)
)


(defun create-window (w)
   "Create and return a new window (twindow structure)."
   (let ((w1 (car w))
         (w2 (cadr w))
         (w3 (caddr w))
         window)
      (cond ;; process keyword in window spec. (if any)
         ((null w)           (setf window (list *time0* nil *infinity*)))
         ((equal w1 'at)     (setf window (list w2 w2 w2)))
         ((equal w1 'before) (setf window (list *time0* nil w2)))
         ((equal w1 'after)  (setf window (list w2 nil *infinity*)))
         ((equal w1 'between) (setf window (list w2 nil w3)))
         (t (setf window w)))
      (make-window :est (car window) :ist (cadr window) :lst (caddr window))
   )
)


(defun print-window (w &optional (stream t) (depth 0))
   "Prints twindow structure."

   (format stream "(EST:~s  IST:~s  LST:~s)" 
      (window-est w) (window-ist w) (window-lst w))   
)


(defun earliest-start-time (node)  
   "Return est for a node's window."
   (window-est (snode-window node))
)


(defun latest-start-time (node)
   "Return lst for a node's window."
   (window-lst (snode-window node))
)


(defun ideal-start-time (node)
   "Return ist for a node's window."
   (window-ist (snode-window node))
)


(defun set-earliest-start-time (node est)
   "Set est for a node's window."
   (setf (window-est (snode-window node)) est)
)


(defun set-latest-start-time (node lst)
   "Set lst for a node's window."
   (setf (window-lst (snode-window node)) lst)
)


(defun set-ideal-start-time (node ist)
   "Set ist for a node's window."
   (setf (window-ist (snode-window node)) ist)
)


(defun get-duration (node)
   "Return duration for a node."
   (snode-duration node)
)


(defun init-duration (d)
;;; initialize duration field value
   (if (null d) 0 d)
)
            

(defun store-events ()
   "Store scheduled event."

;;; for each event,
;;;  store event as a node of type :event in allnodes (the plan net)
;;;  (events are ordered wrt to (actual) start time).
;;; Events were input as schemas with pattern = (event) so
;;;  retreive them from schema table where they were stored by the
;;;  schema read fns.
;;; Also sets global variable *store-events* (list of numbers of 
;;;  event nodes)
   (let ((some-events (get-relevant-schemas '(event)))
         event-schemas 
         schema-instance
         temp-node
         start-time
         dur
         node
	)

      (when some-events
         (setf event-schemas (get-applicable-schemas '(event))))
 
      (setf *sched-events* nil)

      ;; process each event schema stored in *schematable*
      (for (event :in event-schemas)
         :do
         (setf event (retrieval-result-data event))
         (setf schema-instance event)
         (setf window (schema-window schema-instance))
         (setf start-time (window-est window))
         (setf dur (schema-duration schema-instance))
         ;; this is already a ground instance
      
         ;; make a temporary dummy node in allnodes
         (setf temp-node (make-node :type :event :window window
            :duration dur))
         (set-allnodes temp-node)

         ;; order event after *planhead* node
         (make-prenode *planhead* temp-node)
         (make-succnode temp-node *planhead*)

         ;; add event node (num) to list of sched. events
         (push (snode-nodenum temp-node) *sched-events*)
         
         ;; order this temp node with respect to to other event nodes in net
         (order-est-against-scheduled-events temp-node)
         (order-lst-against-scheduled-events temp-node)          
         
         ;; expand this dummy node with event schema instance
         ;; (a copy of the schema isn't needed since it will not
         ;;  be used after this)
         (expand-node-using-schema-instance schema-instance temp-node)
         (setf (snode-eventname temp-node) (schema-name schema-instance))
      ) ; endfor
   ) ; endlet
)

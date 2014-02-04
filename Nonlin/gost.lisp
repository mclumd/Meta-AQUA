;;;-*- Mode: Lisp; Package: NONLIN -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;    Application:  NONLIN
;;;;  Appl. Version:  1.3
;;;;     Appl. Date:  Oct. 1992
;;;;
;;;;           File:  GOST.LISP
;;;;       Contents:  Functions for interfacing with GOST.
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
;;;;  11/07/92  bpk  cleanup, doc.
;;;;  10/11/92  bpk  added call to update-node-expansion-info in try-to-remove-contributor

(in-package :nonlin)

(defun print-gost-entry (gost-entry stream depth)
  "Print gost-entry structure."

  (declare (ignore depth))
  (format stream "Cond:~s  +[~s ]  -[~s ]~%" (gost-entry-condition gost-entry)
	  (gost-entry-pluses gost-entry) (gost-entry-minuses gost-entry))
)


(defun print-gentry (gentry stream depth)
  "Print gentry structure."

  (declare (ignore depth))
  (format stream "<< ~s :at ~s :from ~s>>" (gentry-type gentry) (get-gentry-node gentry)
	  (get-gentry-contributors gentry))
)


(defun print-gost (&optional (stream t) (depth t))
  "Print all GOST entries."

  (declare (ignore depth))
  (format stream "~%Gost Table Entrees:~%")
	  (for (gost-entry :in (all-gost-entrys))
               :do
	       (format stream "~s" gost-entry))
  nil
)

    
(defun all-gost-entrys ()
   "Returns list of all gost-entry structures in GOST (a hash table)."

    (let (glist)
	 (maphash #'(lambda (key gost-entry-list)
			    (declare (ignore key))
			    (setf glist (append glist gost-entry-list))
		    ) *gost*)
	 glist)
)


(defun enter-gost-table (entry)
   "Add gost-entry structure to GOST."

    (push entry (gethash (predicate (gost-entry-condition entry)) *gost*))
)


(defun reset-gost ()
   "Initialize GOST."

    (setf *gost* (make-hash-table))
)
    

(defun lookup-gost (condition)
    "Returns gost-entry structure for condition (a ground pattern) in GOST."

    (let (result (datalist (relevant-gost-entrys condition)))    
	
       (setf result (find condition datalist :key 'gost-entry-condition :test #'equal))
))


(defun index-gost (condition)
    "Returns entry for condition in GOST (creates entry if necessary)."

    (let ((entry (lookup-gost condition)))

       (unless entry ; entry doesn't exist so make one:
          (setf entry (make-gost-entry :condition condition))
	  (enter-gost-table entry))
     entry
))


(defun gost-entrys (cond)
  "Return all gentrys that match cond (a ground, possibly negated, pattern)."

  (multiple-value-bind (condition sign)  ; get pattern, polarity of cond.
      (convert-pat cond)

      (let ((entry (lookup-gost condition)))
      
         (unless entry ; no such entry exists
            (return-from gost-entrys nil))
      
         (case sign
	   (:plus  (gost-entry-pluses entry))
	   (:minus (gost-entry-minuses entry))
      ))
))

    
(defun purpose-nodes (node cond)
   "Returns nodes (numbers) for which Node (a number) contributes cond (a ground, 
    possibly negated, pattern)."

    (let ((gentrys (gost-entrys cond)))
	(for (gentry :in gentrys)
	     :when (member node (get-gentry-contributors gentry))
	     ;; when node is a member of the contributors for the condition
	     :save 
	     (get-gentry-node gentry)
	     ;;then the node of that gentry will be a purpose node
	))
)


(defun enter-gost (cond entry-type node contributors)
   "Make and return gost-entry for cond (gound, possibly negated pattern)."

   ;;; entry-type is condition type.
   ;;; called by: expand-goal-node, expand-phantom-node, expand-node-using-schema-instance
  
   (multiple-value-bind (condition sign) ; decompose cond into condition (pattern), 
                                         ; sign (:plus or :minus)
      (convert-pat cond)

         (let* ((entry   (index-gost condition))                     ; get existing gost-entry
	        (gentrys (case sign                                  ; pertinant gentry's                      
		            (:plus  (gost-entry-pluses  entry))
		            (:minus (gost-entry-minuses entry))))
	        (gentry  (find node gentrys :key #'get-gentry-node)) ; gentry for node (if any)
	       )

            (unless gentry  ; no gentry for node found so make one:
	       (setf gentry (make-gentry :node (list (add-context node))))
	       (case sign
	          (:plus  (pushnew gentry  (gost-entry-pluses entry)))
	          (:minus (pushnew gentry  (gost-entry-minuses entry)))
	       )
            )

            (setf (gentry-type gentry) entry-type) ; set gentry type
            (add-contributors gentry contributors) ; add contributors to gentry contributors
             
            (return-from enter-gost entry)
)))


(defun try-to-remove-contributor (node contributor cond)
  "Try to remove contributor (a node) from contributors of cond for (target) node."

  ;;; Called by linearize as one way of resolving an interaction.

  (let ((gost-entry (lookup-gost (convert-pat cond))) ; get gost-entry for cond
	gentry)

    ;; get gentry for target node in gost-entry
    (setf gentry (find node (gost-entrys cond) :key #'get-gentry-node))

    (remove-contributor gentry contributor) ; remove contributor (updates gentry)

    (when (< (length (get-gentry-contributors gentry)) 1)
        ;; Just removed the only contributor for cond. for target node.
        ;; So the target node reverts from a phantom node a goal node and goes back into taskqueue.
        (let ((newnode (my-copy-node (allnodes (node-nodenum node))))
             (old-id (snode-id (allnodes (node-nodenum node))))
             )
             (setf (node-type newnode)     :goal)
             (setf (node-expanded newnode) nil)
	     (set-allnodes newnode (node-nodenum node))
             (enter-taskqueue newnode)
             (update-node-expansion-info old-id newnode) ; bpk 10/11/92
    ))
))


(defun remove-use-only-for-query-cond (node contributor cond)
  "Remove a contributor of a use-only-for-query condition (cond) for (target) node." ; ???

  (let ((gost-entry (lookup-gost (convert-pat cond))) ; get gost-entry for cond
        gentry)

    ;; get gentry for target node in gost entry
    (setf gentry (find node (gost-entrys cond) :key #'get-gentry-node))

    (remove-contributor gentry contributor) ; remove contributor (updates gentry)
))


(defun remove-contributor (gentry contributor)
  "Deletes a contributor (a node num) from a gentry."

  (let ((contris (get-gentry-contributors gentry))) ; list of contributors (node nums)

     (setf (gentry-contributors gentry) 
              (push (add-context (remove-context (remove-list contributor contris)))  
                    (gentry-contributors gentry)))
))


(defun add-contributors (gentry contributors)
  "Adds a contributor (a node num) to a gentry."

  (let* ((contris (get-gentry-contributors gentry))          ; list of contributors (node nums)
	 (contrib (cond ((listp contributors) contributors)  
		        (t                    (list contributors)))))

     (setf contrib (for (contributor :in contrib)
		        :when (not (member contributor contris))
		        :save contributor))

     (setf (gentry-contributors gentry) 
              (push (add-context (append contris contrib))
                    (gentry-contributors gentry)))
))


(defun get-gentry-contributors (gentry)
   "Returns contributor(s) for gentry for most recent context."

   ;; -contributors is list of: (context nodenums)
   (cdar (gentry-contributors gentry))
)


(defun get-gentry-node (gentry)
   "Returns node for gentry."

   ;; -node is list of: (context . node)
   (cdar (gentry-node gentry))
)


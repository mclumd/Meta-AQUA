;;;-*- Mode: Lisp; Package: NONLIN -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;    Application:  NONLIN
;;;;  Appl. Version:  1.3
;;;;     Appl. Date:  Oct. 1992
;;;;
;;;;           File:  LINK.LISP
;;;;       Contents:  Functions for handling interactions.
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
;;;;  11/09/92  bpk  cleanup, doc.
;;;;  10/13/92  bpk  fixed bug in try-to-modify-binding

(in-package :nonlin)

(defun resolve-interaction (&key conflict-nodes offender offender-effect 
				 (extra-links '(nil)))
    "Resolve an interaction caused by offender node's effect (offender-effect).  This
     conflicts with affects asserted by other nodes (conflict-nodes)."

   ;;; called by add-effects.

   ;;; extra links = proposed new links to the network (list of suggest-entry structures).
   ;;; (nodes are actually node numbers)

   ;; compute conflict ranges: list of (inode node-purposes), where inode is node asserting
   ;; ~offender-effect, used by nodes in node-purposes.
   (let*  ((conflict-ranges (for (inode :in conflict-nodes)
				 :save (list inode (purpose-nodes inode 
				 (negate-pat offender-effect)))))
	   (node offender)
	   (node-purposes (purpose-nodes offender offender-effect)) ; offender-effect's purpose

           ;; get linearizations to resolve interaction
	   (linearizations 
              (nonlin-link (list (append (list (list node node-purposes)) conflict-ranges)) 
                           offender-effect extra-links))
          )
      (return-from resolve-interaction linearizations)
))


(defun resolve-establish-conflict (&key offender-range conflict-nodes offender-cond 
                                        (extra-links '(nil)))
   "Resolve a conflict between offender-range for which offender-cond holds and
    nodes in conflict-nodes."

   ;;; called by try-to-establish.

   ;;; offender-range = (contributor atnode), where atnode is offender-cond's purpose
   ;;; conflict-nodes = list of VNOTL nodes (numbers)
   ;;; offender-cond  = offender-effect
   ;;; extra links    = proposed new links to the network (list of suggest-entry structures).

   ;; compute conflict ranges: list of (inode node-purposes), where inode is node asserting
   ;; ~offender-cond, used by nodes in node-purposes.
   (let*  ((conflict-ranges (for (inode :in conflict-nodes)
				 :save (list inode (purpose-nodes inode 
				 (negate-pat offender-cond)))))
	   (node          (first offender-range))          ; offender node
	   (node-purposes (list (second offender-range)))  ; offender-cond's purposes

           ;; get linearizations to resolve interaction
	   (linearizations 
              (nonlin-link (list (append (list (list node node-purposes)) conflict-ranges)) 
                           offender-cond extra-links)))
      
     (return-from resolve-establish-conflict linearizations)
))
    
   
(defun nonlin-link (interact-list offender-effect extra-links)
   "Remove interactions by suggesting new links.  Returns suggested links."

   ;;; interact-list = list of interact-record:
   ;;;                 (node (node-purposes) (inode1 (inod1-purposes)) (inode2 (inode2-purp.))...)
   ;;;                 where node asserts offender-effect and inode asserts ~offender-effect
   ;;;                 (nodes are actually node numbers)
   ;;; extra links   = proposed new links to the network (list of suggest-entry structures).
   
   (let ((newsuggest extra-links) ; holds latest suggested links (suggest-entry structures)
	 suggest)                 ; holds previous suggested links

    ;; handle each interaction:
    (for (interact-record :in interact-list)
	 :do
         ;; first list in interact-record contains info about offender
	 (let* ((node-particulars (pop    interact-record))    ; (node (node-purposes))
		(node             (first  node-particulars))   ; offender node
		(node-purposes    (second node-particulars)))  ; offender effect's purposes

               ;; process rest of entries in interact-record  (i.e., the interactions)
	       (for (interaction :in interact-record)
		   :do
		   (let* ((inode          (first  interaction))
			  (inode-purposes (second interaction))
			  interaction-ranges)

                      ;; interaction-ranges is list of: ((node node-purpose) (inode inode-purpose))
	              (setf interaction-ranges
			       (construct-interaction-ranges node node-purposes 
                                                             inode inode-purposes))
		      ;; process each pair of ranges
		      (for (ranges :in interaction-ranges)
			      :do
			      (let ((vrange   (first ranges))   
				    (vbarange (second ranges))
                                   )
		                (setf suggest newsuggest) ; save previous links
			        (setf newsuggest nil)     ; initialize new links

                                ;; try to get linearization in each net under consideration
                                ;; (a net is current net plus suggested link)
				(for (net :in suggest)
				     :do
				      (let ((linearizations 
                                               (resolve-link vrange vbarange offender-effect net))
                                           )
				         (if (null linearizations)
					    (then ; no harmful interaction found in net
                                                  ; so save this net as a valid alternative
					       (setf newsuggest (append1 newsuggest net)))
					    (else ; a harmful interaction exists, find linearization
					       (if (and (no-valid-linearizations linearizations) 
                                                        (second vbarange)
							(not (eql (first vbarange) (second vbarange))))
						  (then
                                                     ;; no linearization was found!!
                                                     ;; try rebinding cond if a :use-only-for-query
					             (if (try-to-modify-binding 
                                                            (second vbarange) (first vbarange) 
                                                            (negate-pat offender-effect))
                                                        ; binding modified successfully, save net
							(setf newsuggest (append1 newsuggest net)))
                                                     )
						  ; else - save nonnull linearization(s)  found
					          (for (linearization :in linearizations)
						       :do 
						       (if (not (null linearization))
                                                          (setf newsuggest 
                                                             (append1 newsuggest  
                                                                      (cons linearization net ))))
					          )
                                                ) ; end if and no-valid...
                                         )) ; end if null lin.
				)) ;end of for-net
		    )) 	; ranges

                    ;; interaction could not be removed
		    (if (null newsuggest) (return-from nonlin-link (fail))) ; *** RETURN ***
		))  ; for interaction
	 ))	; for interaction record

    ;; all interactions can be removed by adding any link in newsuggest
    (return-from nonlin-link newsuggest)
))


(defun try-to-modify-binding (conflict-atnode conflict-node conflict-cond)
   "Try to rebind a use-only-for-query condition to resolve an interaction between
    conflict-atnode (asserting conflict-cond) and conflict-node (asserting ~conflict-cond)."

   ;; called only when interaction cannot be resolved by linearization

    (let ((old-conflict-cond ; conflicting condition (scondition structure)
             (find conflict-cond (node-expanconds conflict-atnode) :key #'scondition-pattern
				                                   :test #'equal))
          old-binding ; old binding for conflicting condition
         )

         ;;; 10/13/92 - bpk:
         ;;; the following statement fixes a bug that caused an error when old-conflict-cond
         ;;; was nil.  This occurs because a previous call to this fn (for an alternate net)
         ;;; succeeded in rebinding the old condition to a new one.  Hence the find fn. above
         ;;; can no longer find the old condition in the conflict-atnode's expanded conds.
         ;;; So, for now we will just assume that we can use the alternate net with its new
         ;;; binding for the cond. (though that net might not work for other interactions
         ;;; so we may need to do something more clever here).

         (unless old-conflict-cond
            (return-from try-to-modify-binding nil))  ; use new binding in a previous suggested net

	 (setq old-binding (get-scondition-binding old-conflict-cond))

	 (when (eql (scondition-type old-conflict-cond) :use-only-for-query)
	     (mark conflict-atnode) 
	     (let ((new-conflict-cond (my-copy-scondition old-conflict-cond)))
	        (setf (scondition-pattern new-conflict-cond)
		         (remove-binding (scondition-pattern new-conflict-cond) 
                           (mapcar #'reverse old-binding)))

	        ;; query the current network to get a new binding
	        (multiple-value-bind (already-true result new-binding)
		   (q&a (scondition-pattern new-conflict-cond) conflict-atnode)

	           (when already-true ; a new binding was found
                      ;; replace old cond. and binding with new cond. and binding
		      (destructive-replace-variables 
 	              (scondition-pattern new-conflict-cond) new-binding)
                      (setf (scondition-contributors new-conflict-cond) result)
                      (push (add-context new-binding) (scondition-binding new-conflict-cond))
	              (dremove old-conflict-cond (node-expanconds conflict-atnode))
	              (pushnew new-conflict-cond (node-expanconds conflict-atnode))
	              (remove-use-only-for-query-cond conflict-atnode
	                 conflict-node (scondition-pattern old-conflict-cond))
		      ;; update GOST
		      (enter-gost (scondition-pattern new-conflict-cond) :use-only-for-query
		           conflict-atnode result)
		      ;; modify effects of the node
		      (for (seffect :in (node-ctxt conflict-atnode))
		           :do
		           (let ((seffect-old-pat (seffect-pattern seffect))
			         (seffect-new-pat))
			      (setf seffect-new-pat 
                                 (remove-binding seffect-old-pat (mapcar #'reverse old-binding)))
			   (destructive-replace-variables seffect-new-pat new-binding)
			   (delete-tome seffect-old-pat (seffect-type seffect) conflict-atnode)
		           (enter-tome seffect-new-pat (seffect-type seffect) conflict-atnode)
			   (setf (seffect-pattern seffect) seffect-new-pat))
                      )
		      (return-from try-to-modify-binding t) ; *** RETURN *** (success)
               ))
))))


(defun construct-interaction-ranges (node node-purposes inode inode-purposes)
   "Returns interaction ranges.  Node asserts condition for node-purposes.  Inode
    asserts ~condition for inode-purposes."

    ;;; returns list of: ((node node-purpose) (inode inode-purpose))

    (let (interaction-ranges vranges vbaranges)

       (setf vranges 
          (if node-purposes
	     ;; if there are some purpose nodes, then make ranges of form (node purpose1)
	     (for (purpose :in node-purposes)
		  :save (list node purpose))
          ; else
             ;; if there are no purpose nodes, then make an artificial range of form (node node)
	     (list (list node node))
          ))
       (setf vbaranges 
          (if inode-purposes
             (for (ipurpose :in inode-purposes)
		  :save (list inode ipurpose))
          ; else
	     (list (list inode inode))
          ))

       (for (vrange :in vranges)
	    :do
	    (for (vbarange :in vbaranges)
		 :do (push (list vrange vbarange) interaction-ranges))
       )
  (return-from construct-interaction-ranges interaction-ranges)
))
			 

(defun resolve-link (vrange vbarange offender-effect extra-links)
    "Resolve an interaction between intersecting vrange and vbarange.  Offender-effect holds for
     vrange."

    ;;; extra links   = proposed new links to the network (list of suggest-entry structures).

    (let ((node          (first  vrange))
          (node-purpose  (second vrange)) ; note this may be = to node
	  (inode         (first  vbarange)) 
          (inode-purpose (second vbarange))
         )

       (when (or (null node-purpose) 
                 (range-deleted-in-suggested-net vrange offender-effect extra-links))
	  (setf node-purpose node))   ; vrange modified by net + extra links
       (when (or (null inode-purpose) 
                 (range-deleted-in-suggested-net vbarange (negate-pat offender-effect) extra-links))
	  (setf inode-purpose inode)) ; vbarange modified by net + extra links

       ;; see if interaction still present in net + extra links (i.e., do ranges still intersect?)
       (when (not (vrange-intersects-vbarange-p vrange vbarange extra-links))
	  (return-from resolve-link nil)) ; interaction does not exist anymore so return

       ;; if vbarange is of length 0 and vrange is of length > 0, interchange vrange with vbarange
       (when (and (eql inode inode-purpose) (not (eql node node-purpose)))
	  (psetq node inode node-purpose inode-purpose inode node inode-purpose node-purpose))
	
       ;; return all linearizations to fix interaction between vrange and vbarange    
       (list 
           ;; consider all possible ways to fix interaction 
           ;; (methods are numbered as in Tate's 1975 TR)

	   ;; 1a. try to put vbarange before vrange
	   (if (put-range1-before-range2  `(,inode ,inode-purpose)
                                            `(,node  ,node-purpose) extra-links)
	      (make-suggest-entry :link (list inode-purpose node)))

	   ;; 1b. try to put vrange before vbarange
	   (if (put-range1-before-range2  `(,node  ,node-purpose)
		                            `(,inode ,inode-purpose) extra-links)
	      (make-suggest-entry :link (list node-purpose inode)))
 
	   ;; 3a. try to remove node as a contributor to node-purpose and put vrange before vbarange
	   (if (and (check-contributor-removal node-purpose node offender-effect)
		    (put-range1-before-range2 `(,inode ,inode-purpose)
			                        `(,node  ,node) extra-links))
	     (if (eql inode inode-purpose)
		 (make-suggest-entry :cond offender-effect
	                             :contributor (list (list node node-purpose)))
                 ; else
	         (make-suggest-entry :link (list inode-purpose node)
				     :cond offender-effect
	                             :contributor (list (list node node-purpose))))
	   )

	   ;; 3b. try to remove node as a contributor to node-purpose and put vbarange before vrange
	   (if (and (check-contributor-removal node-purpose node offender-effect)
		    (put-range1-before-range2 `(,node  ,node)
			                        `(,inode ,inode-purpose) extra-links))
	     (if (eql inode inode-purpose)
		 (make-suggest-entry :cond offender-effect
	                             :contributor (list (list node node-purpose))) 
                 ; else
	         (make-suggest-entry :link (list node inode)
				     :cond offender-effect
	                             :contributor (list (list node node-purpose))))
	   )

	   ;; 2a. try to remove inode as contributor to inode-purpose and put vrange before vbarange
	   (if (and (check-contributor-removal inode-purpose inode (negate-pat offender-effect))
		    (put-range1-before-range2 `(,inode ,inode)
			                        `(,node  ,node-purpose) extra-links))
	     (if (eql node node-purpose)
		 (make-suggest-entry :cond (negate-pat offender-effect)  
				     :contributor (list (list inode inode-purpose)))
                 ; else
	         (make-suggest-entry :link (list inode node)
				     :cond (negate-pat offender-effect)
	                             :contributor (list (list inode inode-purpose))))
	   )

	   ;; 2b. try to remove inode as contributor to inode-purpose and put vrange after vbarange
	   (if (and (check-contributor-removal inode-purpose inode (negate-pat offender-effect))
		    (put-range1-before-range2 `(,node  ,node-purpose)
			                        `(,inode ,inode) extra-links))
	     (if (eql node node-purpose)
		 (make-suggest-entry :cond (negate-pat offender-effect)
	                             :contributor (list (list inode inode-purpose)))
	         ; else
	         (make-suggest-entry :link (list node-purpose inode)
	                             :contributor (list (list inode inode-purpose))
				     :cond (negate-pat offender-effect)))
	   )
	       
	   ;; 4. try to remove both inode and node as contributors to inode-purpose
	   ;;    and node-purpose - then no extra-links needed
	   (if (and (check-contributor-removal inode-purpose inode (negate-pat offender-effect))
		    (check-contributor-removal node-purpose node offender-effect))
	     (make-suggest-entry :cond offender-effect
                                 :contributor (list (list node node-purpose)
						    (list inode inode-purpose)))
	   )
         ) ; end list
))


(defun range-deleted-in-suggested-net (range cond extra-links)
   "Returns T if range has been removed in any of the suggested nets (current net + extra links)."

  (if (eql (first range) (second range)) ; range is artificial - i.e. (n n) - thus can't be removed
     (return-from range-deleted-in-suggested-net nil))

  ;; else check each net
  (do  ((suggest-entry (car extra-links) (car extra-links)))
       ((null suggest-entry) nil)
       (let ((contributor-cond (suggest-entry-cond suggest-entry))
             (contributor1     (first  (suggest-entry-contributor suggest-entry)))
             (contributor2     (second (suggest-entry-contributor suggest-entry))))
	  (when (or (and (eql range contributor1) (eql cond contributor-cond))
	          (and (eql range contributor2) (eql cond (negate-pat contributor-cond))))
	     (return t)
          )
          (setq extra-links (cdr extra-links))
  ))
)
    

(defun vrange-intersects-vbarange-p (vrange vbarange extra-links)
   "Returns T if vrange intersects with vbarange in net with extra links.  Ranges interact
    iff they have some part in parallel."

  (let ((node          (first  vrange)) 
        (node-purpose  (second vrange))
        (inode         (first  vbarange))
        (inode-purpose (second vbarange))
       )
     (not (or (and (eql node node-purpose) (eql inode inode-purpose))
	      (predecessor-p node-purpose  inode extra-links)
	      (predecessor-p inode-purpose node  extra-links)))
))


(defun put-range1-before-range2 (range1 range2 extra-links)
   "Returns T if range1 can be put before range2 (i.e., no links exist in the reverse direction."

    (let ((range1-end (second range1))
	  (range2-beg (first  range2))
	 )
       (not (predecessor-p range2-beg range1-end extra-links))
))


(defun check-contributor-removal (node contributor cond)
   "Returns T if a contributor of cond for node can be removed."
 
   (if (eql node contributor) 
      ;; range is artificial range - i.e., (n n) - so no removal is possible.
      (return-from check-contributor-removal nil)) ; 

   (let* ((gost-entry (lookup-gost (convert-pat cond))) ; needed for gentry removal if required
	 gentry)

      (if (null gost-entry) (return-from check-contributor-removal nil)) ; ??

      (setf gentry (find node (gost-entrys cond) :key #'get-gentry-node))
      (if (null gentry) (return-from check-contributor-removal nil)) ; ??
     
      (or (> (length (get-gentry-contributors gentry)) 1) ; there are multiple contributors
	  (eq (node-type node) :phantom)) ; or node is a phantom (and can revert to a goal)
))


(defun linearize (linearization)
   "Adds links (and removes contributors, if required) to network.  Linearization is list of
    suggest-entry structures."

   ;; process each link/removal suggestion
   (for (suggest-entry :in linearization)
       :do
       (let ((link         (suggest-entry-link suggest-entry)) ; link is node1 -> node2
	     (cond         (suggest-entry-cond suggest-entry))
	     (contributor1 (first  (suggest-entry-contributor suggest-entry))) ; vrange
	     (contributor2 (second (suggest-entry-contributor suggest-entry))) ; vbarange
            )
          (make-prenode  (first  link) (second link))
          (make-succnode (second link) (first  link))
          (when contributor1 
             (try-to-remove-contributor (second contributor1) (first contributor1) cond))
          (when contributor2 
             (try-to-remove-contributor (second contributor2) (first contributor2) (negate-pat cond)))

          ;; DEVISOR mods:
          ;; when ordering 2 nodes, call modify-start-times to adjust the nodes' windows if necessary
          (when *devisor-mods* 
             (unless (modify-start-times (allnodes (car link)) (allnodes (cadr link)))
                (backtrack)) ; backtrack if unsuccessful
	  )
      
  ))
)


(defun no-valid-linearizations (linearizations)
   "Returns T if there are no valid linearizations (linearization is a suggest-entry structure)."

   (do ((linearization (car linearizations) (car linearizations)))
       ((null linearizations) t)
          (if linearization
	      (return nil)
	  ;else
	      (setq linearizations (cdr linearizations)))
   ) 
)

     
(defun predecessor-p (isprenode node extra-links)
   "Returns T if isprenode is a predecessor of node in net with extra links."
 
   ;;; extra links   = proposed new links to the network (list of suggest-entry structures).

    (if (eql isprenode node) (return-from predecessor-p t))

    ;; for now, we can try brute-force scan backwards
    (let ((prenodes (append (get-prenodes node) (get-prenodes-from-extra-links node extra-links))))
	 
       (if (member isprenode prenodes) ; if isprenode is a direct predecessor of node
	  t ; return T
       ; else recursively check each predecessor's predecessors
          (when (there-exists (newnode :in prenodes) (predecessor-p isprenode newnode extra-links))
              t ; return T
       ))
))


(defun get-prenodes-from-extra-links (node extra-links)
   "Returns predecessors of node in net with extra links."

   ;;; extra links   = proposed new links to the network (list of suggest-entry structures).

   (for (suggest-entry :in extra-links)
        :when (eql (cadr (suggest-entry-link suggest-entry)) node)
        :save
        (car (suggest-entry-link suggest-entry))
   )
)


(defun get-scondition-binding (cond)
   "Returns binding for an cond (scondition structure)."
    (cdar (scondition-binding cond))
)

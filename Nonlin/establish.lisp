;;;-*- Mode: Lisp; Package: NONLIN -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;    Application:  NONLIN
;;;;  Appl. Version:  1.3
;;;;     Appl. Date:  Oct. 1992
;;;;
;;;;           File:  ESTABLISH.LISP
;;;;       Contents:  Functions for querying/establishing conditions.
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
;;;;  09/11/92 bpk - added fn ok-contributors to fix bug, cleanup/documentation

(in-package :nonlin)

(defun try-to-establish (condition atnode)
    "Tries to establish condition at node."
    ;;; Called when expanding a goal node to establish conditions.
    ;;; First checks to see condition is already true at atnode.
    ;;; If not, tries to make condition true by linking.
    ;;;
    ;;; RETURNS MULTIPLE (2-3) VALUES:
    ;;; If cond. already true or is establishd by link, returns:
    ;;;    T               (cond. established)
    ;;;    contributors    (list of node nums)
    ;;;    binding         (a binding structure)
    ;;; If cond. can never be established at node (no contributors), returns:
    ;;;    NIL             (cond. not established)
    ;;;    NIL             (no contributors) 
    ;;; If cond. cannot be established by linking, returns:
    ;;;    NIL
    ;;;    NIL
    ;;;    NIL

    ; First check to see if condition is already true.
    (multiple-value-bind (already-true result binding) 
        (q&a condition atnode)  ; (returns arbitrary contributors/binding if any)
                                ; q&a returns T if cond. can CERTAINLY be established;
                                ; contributors or list of critical node lists;
                                ; binding (if cond. is not ground)
	
	(if already-true ; cond. can CERTAINLY be established at node
	    (then 
		  ;; this procedure will not enter anything in gost
		  ;; any procedure that calls this should worry about 
		  ;; entering stuff into gost
		  
		  (return-from try-to-establish (values t result binding)))  ; *** RETURN *** 
	    
	    (else ; cond. CANNOT CERTAINLY be established at node
	        (if (null result) ; cond. can NEVER be established at node (no contributors),
                                  ; further reduction may be required
                   (then
	              (return-from try-to-establish (values nil nil))) ; *** RETURN ***

		   (else ; cond. can POSSIBLY be established at node
                     ; result is (VL PARVL VNOTL PARVNOTL)
		     (let* ((vl-nodes    (first result))
		            (parvl-nodes (second result))
                            (vnotl-nodes (append (third result) (fourth result)))
			    (alternative-nets nil))

		     ;; for each possible contributor node in vl and parvl find
		     ;; out corresponding network (interaction free)
		     (for (contributor-node :in (append vl-nodes parvl-nodes))
		        :do
		        (let* ((suggested-net 
                                  (if (member contributor-node parvl-nodes)
                                      ; if we choose a parallel contributor then the link 
                                      ; contributor-node->atnode must be included in the 
                                      ; suggested network
		                      (list 
                                         (list (make-suggest-entry 
                                                  :link (list contributor-node atnode))))
                                      ; else
				      '(nil)))
		               (result  ; list of suggested links.
		                  ; Link out all critical PNOTV nodes from the range
		                  ; (contributor-node atnode).
				  ; We consider that range chpv-node atnode is actually violating 
                                  ; the protection interval of pvbar-node and its purpose.
                                  (resolve-establish-conflict 
                                     :offender-range (list contributor-node atnode)
				     :conflict-nodes vnotl-nodes
				     :offender-cond condition
				     :extra-links suggested-net))
                              )

			     (if (not (fail-p result)) ; if result is success,
 				(for (linearization :in result) ; process suggested links
				     :do
				     (setf alternative-nets 
                                        (append1 alternative-nets 
					(list atnode contributor-node linearization condition))))
                             )
   			)) ; end for

		    (if (null alternative-nets) ; if no suggested links,
		       (then  ; no linearizations can establish cond.
			   (return-from try-to-establish (values nil nil nil))) ; *** RETURN ***
		       (else ; else try the first alt. net
			   (let* ((first-net     (first alternative-nets))
			          (contributor   (second first-net))
			          (linearization (third first-net))
                                  ; save alternative choices
			          (save-entry 
                                     (make-backtrack-entry :type :establish 
                                        :alternatives alternative-nets))
                                 )

		              (push (add-context save-entry) *context-list*) ; save backtrack point
                              ;; create new context in which linearization has been applied
			      (create-new-context)
			      (if (not-empty linearization) (linearize linearization))

			      (return-from try-to-establish ; *** RETURN ***
				 (values t (list contributor) binding))  ; cond. has been established
		       ))

		)    )
	  )))))
)


(defun q&a-init-ctxt (pattern)
   "Returns all initial context facts (with bindings) that match a pattern 
    (possibly containing variables)."

    (cond ((null (node-ctxt *init-ctxt*)) nil) ; no facts in initial context to match against
          (t 
             ;; match pattern against patterns in (node-ctxt *init-ctxt*)
             (retrieve pattern :from-data-list (node-ctxt *init-ctxt*) :with-pat t))
))
    

(defun q&a-always-ctxt (pattern)
   "Returns all always context facts (with bindings) that match a pattern 
    (possibly containing variables)."

    (cond ((null (node-ctxt *always-ctxt*)) nil) ; no facts in always context to match against
          (t 
             ;; match pattern against patterns in (node-ctxt *always-ctxt*)
             (retrieve pattern :from-data-list (node-ctxt *always-ctxt*) :with-pat t))
))
	

(defun q&a (cond atnode &key (all-bindings nil) &aux nd-bd-pairs)
    "Determines if a (possibly nonground) condition is true (in some instantiation) at a node."
    ;;; cond is condition;
    ;;; atnode is node where condition is tested at;
    ;;; if all-bindings = T, all bindings will be found (want this when finding schemas)
    ;;;    else an arbitrary binding will be returned (if any)
    ;;; 
    ;;; RETURNS MULTIPLE (2 or 3) VALUES:
    ;;; if cond is (equal...) or (not (equal...)), returns:
    ;;;       T|NIL          (result of predicate)
    ;;;       NIL            (contributors N/A)
    ;;; if all-bindings = nil, returns:
    ;;;       T|NIL          (condition is true in some instantiation at node)
    ;;;       contributors   (list of nums. of nodes that can contribute the instantiated cond.)
    ;;;       binding        (the binding that makes the condition true.  Binding is a structure.)
    ;;; if all-bindings = nil, returns:
    ;;;       T|NIL          (condition is true in some instantiation at node)
    ;;;       nd-bd-pairs    (list of (contributors binding) pairs) 

    ;; CONDITION IS :USE-WHEN (EQUAL var1 var2)
    (if (equal (car cond) 'equal)
      (then
	(let ((var1 (second cond))
	      (var2 (third  cond)))
	  (if (equal var1 var2)
	     (return-from q&a (values t nil))     ; Result = T,   contributors = nil (N/A)
	     ;else
	     (return-from q&a (values nil nil)))  ; Result = nil, contributors = nil (N/A)
    )))

    ;; CONDITION IS :USE-WHEN (NOT (EQUAL var1 var2))
    (if (equal (car cond) 'not)
      (then
	(let ((pred (caadr cond))
	      (var1 (cadr (cadr cond)))
	      (var2 (cadr (cdadr cond))))
             (case pred
		(equal
		  (if (equal var1 var2)
		     (return-from q&a (values nil nil)) ; Result = nil, contributors = nil (N/A)
		     ;else
		     (return-from q&a (values t nil)))) ; Result = T,   contributors = nil (N/A)
                (otherwise (error "~%Negated conditions are unsupported."))
	     )
    ))) 

    ;; CONDITION IS REGULAR TYPE, i.e. (pattern):
    
    ;; Check if cond holds in the always or initial contexts
    ;; If a pattern is true in init-ctxt or always-ctxt, we don't look for other contributors.
    (let (init-result always-result)

         ;; check the always context
	 (setf always-result  (q&a-always-ctxt cond))
	 (if always-result           ; if cond holds in always context,
	     (if (not all-bindings) 
	        ; return the first binding
	        (return-from q&a                ; *** RETURN ***
                   (values t                                                 ; result = T
                           (list (node-nodenum *always-ctxt*))               ; contributor: (n)
		           (retrieval-result-binding (car always-result))))  ; first binding
	        ; else return all bindings as list nd-bd-pairs: ((contributors binding)*)
	        (for (rbd :in always-result) ; for each binding,
		  :do (push (list 
                               (list (node-nodenum *always-ctxt*))  ; contributor is (n)
			       (retrieval-result-binding rbd))      ; binding
			    nd-bd-pairs))
	     ))
	     
         ;; check the initial context
	 (setf init-result (q&a-init-ctxt cond))
	 (if init-result 
	     (if (not all-bindings)      
                ; return the first binding
		(return-from q&a               ; *** RETURN ***
                   (values t                                                  ; result = T
                   (list (node-nodenum *init-ctxt*))                          ; contributor: (n)
		         (retrieval-result-binding (car init-result))))       ; first binding
	        ; else return all bindings as list nd-bd-pairs: ((contributors binding)*)
		(for (rbd :in init-result) ; for each binding, 
                   :do (push (list 
                                (list (node-nodenum *init-ctxt*))
			        (retrieval-result-binding rbd))
		             nd-bd-pairs))
	     ))

	 (if nd-bd-pairs ; if bindings found in always or initial contexts, return them all
             ; ONLY WHEN WE DON'T GET ANY THING FROM INPUT STATE WILL WE GO TO TOME--
	     ; BASICALLY A WAY OF AVOIDING WRONG "AND" BINDINGS WHERE IN ONE COND IS CONTRIBUTED
	     ; BY ONE NODE ANOTHER BY ANOTHER NODE AND THE PREVIOUS COND CAN'T BE CONTRIBUTED BY
	     ; THE SECOND NODE
	     (return-from q&a           ; *** RETURN ***
	        (values t               ; result = T
                        nd-bd-pairs)))  ; all bindings: ((contributors binding)*)
    ) ; end let

    ;; Condition not found in always or initial contexts.  Search partial contexts using TOME.
    (multiple-value-bind (condition sign)
	(convert-pat cond)

        ;; Condition is ground. 
	(if (ground-pat-p condition)
	   (then 
              (let ((entry (lookup-tome condition)))  ; find entry for cond. in TOME

	         (if entry ; if an entry exists
                    ; compute critical node lists for condition
		    (multiple-value-bind (result contributors) ; result   = T or nil
                                                               ; contrib. = C.PV-nodes or nil
	               (q&a-process-tome-entry entry condition sign atnode)
		          (if all-bindings 
		             (then
		                (if result
				   (then
			              (push (list contributors nil) nd-bd-pairs)  
                                         ; binding nil since ground
				      (return-from q&a 
                                         (values result         ; Result = T
                                                 nd-bd-pairs))) ; all bindings
				   ; (return-from q&a   ; *** RETURN ***
                                   ;    (values result (list (list contributors (list nil)))))
				   ;if not all-bindings
				   ))	
		             (else ; return only contributors from net
			        (return-from q&a        ; *** RETURN ***
                                   (values result contributors)) ; result = T, contrib. is list
		  ))))

		  (if (and all-bindings nd-bd-pairs) ; if no tome entry or result = nil,
                     ; return result, nb-bd-pairs from initial/always contexts
		     (return-from q&a (values t nd-bd-pairs))  ; *** RETURN ***
                  ; else     
		     (return-from q&a	; *** RETURN ***
		        (values nil nil)))))  ; result = NIL

            ;; Condition is partially instantiated (i.e., contains unbound vars.).
	    (else 
	       (let ((matching-rentrys                           
	                (lookup-tome-partial-pat condition)) ; find TOME entries matching cond
		     tome-query-results
		    )

	          (if (null matching-rentrys) ; if no matching entries found,
		     (if (and all-bindings nd-bd-pairs)
                        ; return result, nb-bd-pairs from initial/always contexts
			(return-from q&a (values t nd-bd-pairs)) ; *** RETURN ***
			; else
			(return-from q&a ; *** RETURN ***
                           (values nil nil nil))) ; result = NIL
		     )

                  ;; process matching entries found,
		  (for (rentry :in matching-rentrys)
                            ; process only tome entries
			    ; to avoid the problem of cleartop ?table coming in ???
                      :when (tome-entry-p (retrieval-result-data rentry)) 
		      :do 
                         (let ((entry (retrieval-result-data rentry))      ; get actual entry
			       (binding (retrieval-result-binding rentry)) ; get binding for match
			       tome-query-result
			      )

	                    (setf tome-query-result
		               (append1 (multiple-value-list
                                        ; process tome entry to get critical node lists for
                                        ; GROUND patterns.
					   (q&a-process-tome-entry entry condition sign atnode))
                                           ; returns (1) T or NIL, (2) contributors or NIL or list of
                                           ; critical lists
			                binding))

		            (if (first tome-query-result) 
                               ; if rentry can CERTAINLY establish cond. at node,
                               ; (i.e., tome-query-result is (T contributors binding)),

			       (if all-bindings ; if we want all bindings, 
                                  ; add (contributors binding) to nd-bd-pairs then continue
		                  (push (list (second tome-query-result) binding) nd-bd-pairs)
		                  ; else, return this binding (don't continue to find others)
		                  (return-from q&a  ; *** RETURN ***
			             (values (first tome-query-result)   ; result = T
					     (second tome-query-result)  ; contributors
					     (third tome-query-result))) ; binding 
			       )

		            ; else (renntry CANNOT CERTAINLY establish cond. at node)
                            ; tome-query-result is (NIL critical-lists/NIL binding)
		            (push tome-query-result tome-query-results)) ; save result 
		   )) ; end for

		(if nd-bd-pairs ; if any rentry can CERTAINLY establish cond. at node,
	           (return-from q&a          ; *** RETURN ***
                      (values t              ; result = T
                              nd-bd-pairs))  ; contributors-binding pairs
	        )

                ;; No rentry can CERTAINLY establish cond. at node.
                ;; It's possible that an rentry can POSSIBLY establish cond. at node 
                ;;  (need to do critical node analysis using critical lists returned by
                ;;   q&a-process-tome-entry).
                ;; return an arbitrary tome query result: (VL PARVL VNOTL PARVNOTL binding) ???
		(let ((tome-query-result (choose tome-query-results))) ; choose first (most recent)
                    ;;; !!! or choose best (i.e., one with nonnil second elmt.) ???
		    (return-from q&a ; *** RETURN ***
		       (values (first tome-query-result)  ; NIL
			       (second tome-query-result) ; NIL or (VL NOTVL PARVL PARNOTVL)
			       (third tome-query-result)  ; binding
	        )))
)))))


(defun q&a-process-tome-entry (entry condition sign atnode 
                               &aux ppvs     ; pv-nodes
                                    ppvbars  ; pvbar-nodes
				    pvs      ; VL
                                    cpvs     ; PARVL
                                    pvbars   ; VNOTL
                                    cpvbars) ; PARVNOTL
    "Computes critical node lists for a ground condition at a node."
    ;;; RETURNS MULTIPLE (2) VALUES:
    ;;; if no possible contributors, returns:
    ;;;    NIL                          (condition CANNOT EVER be established)
    ;;;    NIL                          (no contributors)
    ;;; if no possible clobberers, returns:
    ;;;    T                            (condition CAN CERTAINLY be established)
    ;;;    contributors                 (list of nums of critical pv-nodes)
    ;;; otherwise, returns:
    ;;;    NIL                          (condition CANNOT CERTAINLY be established;
    ;;;                                  condition CAN POSSIBLY be established)
    ;;;    (VL PARVL VNOTL PARVNOTL)    (list of critical node lists)

    ;; find p-nodes in tome (ppvs are pv-nodes, ppvbars are pvbar-nodes)
    (case sign
	  (:plus
		(setf ppvs     (get-tome-entry-asserts entry))   ; all nodes asserting condition
		(setf ppvbars  (get-tome-entry-deletes entry)))  ; all nodes denying condition
	  (:minus
	        (setf ppvbars  (get-tome-entry-asserts entry))   ; all nodes denying condition
		(setf ppvs     (get-tome-entry-deletes entry)))  ; all nodes asserting condition
    )
    
    ;; compute cpvs (critical pv-nodes)
    (for (ppv :in ppvs) ; check all nodes that assert the condition
	 :do
	 (case (node-mark ppv) ; nodes marked wrt atnode in a previous function
	       (:parallel (push ppv cpvs)) ; parallel pv-nodes are automatically critical 
	       (:before   (push ppv pvs))) ; before   pv-nodes are possibly critical 
               ;:after pv-nodes cannot be critical
    )

    ;; compute cpvbars (critical pvbar-nodes)
    (for (ppvbar :in ppvbars) ; check all nodes that deny the condition
	 :do
	 (case (node-mark ppvbar)
	       (:parallel (push ppvbar cpvbars)) ; parallel pvbar-nodes are automatically critical
	       (:before   (push ppvbar pvbars))) ; before   pvbar-nodes are possibly critical
               ;:after pvbar-nodes cannot be critical
    )

    (when (and pvs                            ; if possible contributors exists (VL not nil),
               (null pvbars) (null cpvbars))  ; and no possible clobberers exist,
       ; condition CAN CERTAINLY be established
       (return-from q&a-process-tome-entry (values t pvs)))   ; RETURN T, contributors
    
    ; !!! added this case 9/10/92
    (when (and pvs (null cpvbars)) ; if possible contributor (in VL), and no parallel clobberers,
       (let ((c (ok-contributors pvs pvbars))) ; and at least one contributor follows all clobberers
          (when c
             (return-from q&a-process-tome-entry (values t c))) ; RETURN T, contributors (c)
    ))

    (when (and (null pvs) (null cpvs))        ; if no possible contributors exist (i.e. no cpv-nodes),
       ; condition CANNOT EVER be established at all
       (return-from q&a-process-tome-entry (values nil nil))) ; RETURN nil, nil
   
    (when (and pvs (null cpvbars)) ; if possible contributor (in VL), and no parallel clobberers,
       (let ((c (ok-contributors pvs pvbars))) ; and at least one contributor follows all clobberers
          (when c
             (return-from q&a-process-tome-entry (values t c))) ; RETURN T, contributors (c)
    ))

    ; otherwise,
    ; condition CAN POSSIBLY be established (but cannot CERTAINLY be established)
       ; RETURN nil, list of critical lists
       (return-from q&a-process-tome-entry (values nil `(,pvs ,cpvs ,pvbars ,cpvbars)))
)


(defun ok-contributors (contribs clobbs)  ; added 9/10/92 bpk - fixes bug in q&a-process-tome-entry
   "Return (critical) contributors which follow all clobbers."
   ; contribs is list of nums of contributor nodes (from VL only)
   ; clobbs   is list of nums of clobberer nodes   (from VNOTL only)
   (let ((old-netmarked *netmarked*) ; remember node for which net is currently marked wrt
         ok-contribs ok
        )

      (dolist (c contribs) ; check each contributor
         (mark c) ; mark net wrt contributor c
         (setq ok t) 
         (dolist (d clobbs) ; check that no clobber is marked :before c
            (if (not (equal (node-mark d) :before))
               (setq ok nil)))
         (if ok (push c ok-contribs))
      )
      (mark old-netmarked) ; restore old net marks
      ok-contribs ; return ok contributors
))


(defun choose (list)
  "Choose and return an arbitrary (the first) element from list."
  (car list)
)
				     
     
(defun not-empty (net)
  "Returns T if net not empty."
  (not (equal net '(nil)))
)
		 


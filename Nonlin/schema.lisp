;;;-*- Mode: Lisp; Package: NONLIN -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;    Application:  NONLIN
;;;;  Appl. Version:  1.3
;;;;     Appl. Date:  Oct. 1992
;;;;
;;;;           File:  SCHEMA.LISP
;;;;       Contents:  Functions for selecting and instantiating schemas.
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
;;;;   9/15/92  bpk  added print to get-plausible-schemas, fixed bug in my-copy-schema
;;;;   2/29/92  BPK  fixed bug that assigned wrong values to *schema-parent-names*
;;;;                 (in get-plausible-schema-instances)
;;;;   2/15/92  BPK  added code to save names of parents of instantiated schemas
;;;;                 (*schema-parent-names*).  *schema-parent-name* holds parent of
;;;;                 latest schema instantiated and ultimately, the name of the 
;;;;                 parent of the chosen schema
;;;;                 (changes to select-schema-to-expand, get-plausible-schema-instances,
;;;;                 my-copy-schema)            

(in-package :nonlin)

(defun select-schema-to-expand (atnode)
    "Selects and returns an instantiated schema to use when expanding a goal or action node."
    ;;; atnode is node (structure) being expanded
    ;;; Returns one such schema instance found and saves others found as backtracking points.

    (let ((schema-instances 
	     (get-plausible-schema-instances atnode)))  ; schemas matching pattern of atnode
                                                        ; whose filter conds. are satisfied
       (if (null schema-instances) ; if no schemas found that can be instantiated,
	  (then 
             (when (debug-ds-p 'backtrack) 
                (format t "~&*** BACKTRACKING since unable to instantiate schema for node ~s ~s ~s."
                   (node-nodenum atnode) (node-type atnode) (node-todo atnode)))
             (backtrack)) ; *** backtrack ***
  	  (else ; schema(s) found that can be instantiated
	     ;; save the alternative schemas in context history
	     (let (save-entry)
	       (for (schema :in schema-instances)
		    :do
		    (setf save-entry (append1 save-entry (list atnode schema)))
	       )
	     (push (add-context (make-backtrack-entry
	  			:type :schema
				:alternatives 
				(cdr (reverse save-entry))))
			        *context-list*)
	     (create-new-context)

             ;; save the name of the schema whose instance was chosen 
             ;; (this will be output with info about node expansion)
             (setf *schema-parent-name* (car (reverse *schema-parent-names*)))

             ;; return most recent schema instance found:
	     (car (reverse schema-instances))  
      )))
))


(defun get-plausible-schema-instances (atnode &aux results)
    "Returns list of instantiated schemas that can be used to expand a node."
    ;;; atnode is node (structure) being expanded
    
    ;;; results = list of plausible schema instances

    (setf *schema-parent-names* nil) ; this var holds the names of the schemas that may be 
                                     ; instantiated
    
    ;; for each schema that matches the target pattern,
    ;; see if that schema's conds. are met and if so, return its full instantiation
    (for (schi :in (get-applicable-schemas (node-todo atnode)))  ; find schemas matching pattern
	 :do
	 (let* ((schema (retrieval-result-data schi))                 ; candidate schema
		(retrieval-binding (retrieval-result-binding schi))   ; candidate binding
		(si (make-schema-instance schema retrieval-binding))  ; schema instance
                (parent-name (schema-name schema))                    ; schema name
                ; (parent-name *schema-parent-name*) ; set in my-copy-schema                 
                res  ; a plausible schema instance
	       )

               ;; Check that schema conds. met and if so, return fully-instantiated schema instance
	       ;; (Variables are substituted for only the chosen schemas)
               (setf res (old-construct-schema-instances si atnode))

               (when res ; if a fully instantiated instance returned,
                  (setq *schema-parent-names*                         ; save schema name
                     (append *schema-parent-names* (list parent-name))))

               (when (and (not res) (or (debug-ds-p 'schema) (debug-ds-p 'schema-brief)))
                  (format t "~&* Applicable schema instance ~s (vars ~s) not plausible."
                     (schema-name si) (schema-vars si)))

	       (setf results (append results res)) ; add res to accumulated results
	 ))

      (when (or (debug-ds-p 'schema) (debug-ds-p 'schema-brief)) 
         (format t "~&Plausible schemas are: ~s"
            (mapcar #'(lambda (r) (schema-name r)) results))
      )
    results ;  return a list of fully-instantiated schema instances
) ; end get-plausible-schema-instances


(defun get-applicable-schemas (pattern)
   "Returns all schemas in schema table whose :todo field matches target pattern."
    
   (let* ((relevant-schemas (get-relevant-schemas pattern)) ; access schema table (a hash table)
          ; process what gethash returned...
          (result (retrieve pattern :from-data-list relevant-schemas
	                            :key-function 'schema-todo :with-pat t)))
   result
))


(defun old-construct-schema-instances (si atnode)
    "Checks and binds the filter conditions of a schema instance for expanding a node."
    ;;; si = schema instance (with some bound vars)
    ;;; atnode = node (structure) being expanded

    (let ((rec-result (recursive-propagate-usewhen-bindings si atnode 0
		         (1- (length (schema-conditions si ))) ; number of conds. - 1
			 ; basically we have to recurse with an upper bound
			 ; of the # of conds (we will use nth, so 1- & 0 will contains nil)
		      )))

	  (for (res :in rec-result) :when (not (null res))
	       :save res) ; save results
	  ;; return the resultant schemas without the null values.
))


(defun recursive-propagate-usewhen-bindings  (si atnode ind length)
   "Bind variables in :use-when and :use-only-for-query conditions.  Returns schema instance
    with bound vars."

    (if (> ind length) ; no conds. left to bind (???)
	;; index should not be greater than number of conditions
	(return-from recursive-propagate-usewhen-bindings (list si))) ; *** RETURN ***
    
    ;; else process next condition in schema instance:
    (let ((cond (nth ind (schema-conditions si))))

       (if (or (eql (scondition-type cond) :use-when)
	       (eql (scondition-type cond) :use-only-for-query))
	     (then
		  (multiple-value-bind (result nd-bd-pairs)
                      ;; get all bindings that make cond true at node atnode
		      (q&a (scondition-pattern cond) (node-nodenum atnode) :all-bindings t)
		      
		      (if (not result) 
			  (then ; no bindings found for the condition
			     nil) ; *** RETURN (FAILURE) ****
			  (else-let (results)
			    (if nd-bd-pairs ; process each binding returned
			      (then	
			      (for (nd-bd :in nd-bd-pairs)
				   :do
                                   ;; generate a new schema instance from si that uses binding
				   (let* ((contributors  (first nd-bd))
					  (binding       (second nd-bd))  ; list of: (var value)
					  (new-si        (make-schema-instance si binding))
					  (new-cond      (nth ind (schema-conditions new-si)))
					 )

                                      ;; add context to :use-only-for-query binding for possible
                                      ;; rebinding later
				      (when (eql (scondition-type new-cond) :use-only-for-query)
					  (push (add-context binding) 
                                                (scondition-binding new-cond))) 
                                      ;; store node contributors in condition
			              (for (ncont :in 
						  (mapcar #'(lambda (x) (- x *striplen*))
							     contributors))
					          :do (push ncont (scondition-contributors 
								  new-cond)))
                                      ;; attempt to bind rest of conds. in the new schema instance
				      (setf results (append results
							    (recursive-propagate-usewhen-bindings
								   new-si atnode (1+ ind) length)))
			      ))
			      results  ; *** RETURN ***
			      ) ; end then
                          ;; ???
 			  (else (recursive-propagate-usewhen-bindings si atnode (1+ ind) length)))
		       ) ; end if nd-bd-pairs...
		   )))
	     (else  
		  ;; cond. is not a use-when condition, just skip
		  ;; over it by increasing the index and recursing
		  (recursive-propagate-usewhen-bindings si atnode (1+ ind) length))
	 )
))


(defun make-schema-instance (schema binding)
   "Makes a schema instance from a schema and a binding."

    (let ((si (my-copy-schema schema))) ; copy the schema
       (propagate-binding si binding)   ; propagte the binding through copy
))


(defun propagate-binding (si binding)
   "Propagate a binding throughout schema instance.  Returns schema instance."

    ;;; binding is list of: (var value)

    (destructive-replace-variables (schema-todo si) binding) ; prop. thru :todo
 
    (for (cond :in (schema-conditions si))  ; prop. thru conditions
	 :do
	 (destructive-replace-variables (scondition-pattern cond) binding))
    (for (effect :in (schema-effects si))   ; prop. thru effects
	 :do
	 (destructive-replace-variables (seffect-pattern effect) binding))
    (do ((index 0 (+ index 1)))             ; prop. thru strip
	((> index (- (schema-size si) 1)))
	(let ((expnode  (aref (schema-strip si) index)))
	     (destructive-replace-variables (node-todo expnode) binding)))

    (setf (schema-vars si) binding) ; store binding in schema
    si 
)


(defun my-copy-schema (sch)
   "Returns a copy of schema."

    (let ((copy-id   (gensym "SCH"))      ; added this to give schema unique name
          (copy-name (schema-name sch))   ; fixed this to not corrupt schema names in table
	  (copy-todo (schema-todo sch))
            ;;(uniquify-variables (schema-todo sch))
	    ;;can't use uniquify stuff.  all the same variables should be mapped
	    ;;onto same variables... (not even that is necessary)
	    ;;pcvar by themselves are useless without a separate binding list...
	  (copy-size       (schema-size sch))
	  (copy-conditions (for (scond :in (schema-conditions sch))
				:save
				(my-copy-scondition scond)))
	  (copy-effects    (for (seff :in (schema-effects sch))
			        :save 
			        (my-copy-seffect seff)))
	  (copy-vars       (schema-vars sch))    
                           ;(uniquify-variables (schema-vars sch))
	  (copy-strip      (my-copy-strip (schema-strip sch) (schema-size sch)))
  	  ;; DEVISOR mods:
          (copy-duration   (schema-duration sch))
          (copy-swindow    (copy-window (schema-window sch)))
	 )

       (setq *schema-parent-name* (schema-name sch))

       (make-schema :name  copy-name  :id copy-id      :todo copy-todo 
	            :strip copy-strip :size copy-size  :effects copy-effects
	            :conditions copy-conditions        :vars copy-vars
                    :window copy-swindow               :duration copy-duration) ; DEVISOR mods

))
    

(defun my-copy-scondition (scond)
    "Returns a copy of an scondition structure."

    (let ((scond-cpy (copy-scondition scond)))
        ;;	 (setf (scondition-pattern scond-cpy)
	;;       (uniquify-variables (scondition-pattern scond-cpy)))
	 scond-cpy
))
    

(defun my-copy-seffect (seff)
    "Returns a copy of an seffect structure."

    (let ((seff-cpy (copy-seffect seff)))
	  ;;     (setf (seffect-pattern seff-cpy)
	  ;;     (uniquify-variables (seffect-pattern seff-cpy)))
	 seff-cpy
))
	 

(defun my-copy-strip (strip size)
   "Returns a copy of a schema strip."

    (let ((strip-cpy (make-array (list size))))

         ;; copy each node in strip array:
	 (do ((index 0 (+ index 1)))
	     ((> index (- size 1)))
	     (setf (aref strip-cpy index)
		   (my-copy-node (aref strip index)))
	 )
	 strip-cpy
))
    

(defun my-copy-node (node)
    "Returns a copy of a strip node."

    (let ((node-cpy (copy-node node)))
	   ;;     (setf (node-todo node-cpy)
	   ;;    (uniquify-variables (node-todo node-cpy)))
       (setf (snode-id node-cpy) (gensym "ND"))
       ; DEVISOR mod
       (setf (snode-window node-cpy) (copy-window (snode-window node)))
       node-cpy)
)

;;;;
;;;;  SOME OLD STUFF HERE (NO LONGER USED)
;;;;
#|
;;THREW IT AWAY BECAUSE IF THE 'AND REQUEST FORCES THE Q&A TO MAKE
;;ALL THE PATTERNS BECOME TRUE FROM EITHER -2 OR -1 OR FROM TRIES TO
;;SEE FOR THE 'AND REQUEST ITSELF IN TOME 7/13/89
(DEFUN construct-schema-instances (si atnode &aux results)
  (LET ((pat (for (COND :in (schema-conditions si))
		  :when
		  (EQL (scondition-type cond) :use-when)
		  :save
		  (scondition-pattern cond)))
	;;save all conditions
	)
    (PUSH 'AND pat)
    ;;make pat a conjunctive retrieval request
    ;;WE WILL MAKE ALL THE CONDITIONS HAVE THE SAME CONTRIBUTOR
    (MULTIPLE-VALUE-BIND (result nd-bd-pairs)
	(q&a pat ATNODE :all-bindings t)
      ;;WHAT EVER THE NODE NUMBER IS WE WANT TO MAKE SURE THAT
      ;;IT WONT GET CONFLICTING CONTRIBUTORS FOR THE NODE
      (for (nd-bd :in nd-bd-pairs)
	   :do
	   (LET* ((contributors (LIST (NODE-NODENUM *INIT-CTXT*)))
		  ;;(FIRST nd-bd)
		  (binding (SECOND ND-BD))
		  ;;make sure this is right...
		  ;;and keeps the binding inthe third place
		  (new-si (make-schema-instance si binding))
		  (nconts (MAPCAR #'(lambda (x) (- x *striplen*)) contributors))
		  ;;the goodal to get numbers down
		  )
	     (for (COND :in (schema-conditions new-si))
		  :when (EQL (scondition-type cond) :use-when)
		  :do
		  (SETF (scondition-contributors cond) nconts)
		  ;;we don't differentiate between always-ctxt and init-ctxt
		  )
	     (PUSH new-si results))
	   ))
    results ;;return the results
    )
  
  )
|#


#|
(defun propagate-mapping (si mapping)
    ;;propagate a variable binding to all parts of the schema instance
    (dmap-objects (schema-todo si) mapping)
    (for (cond :in (schema-conditions si))
	 :do
	 (dmap-objects (scondition-pattern cond) mapping))
    (for (effect :in (schema-effects si))
	 :do
	 (dmap-objects (seffect-pattern effect) mapping))
    (do ((index 0 (+ index 1)))
	((> index (- (schema-size si) 1)))
	(let ((expnode  (aref (schema-strip si) index)))
	     (dmap-objects (node-todo expnode) mapping)))
    
    ;; SUBSTITUTE VARIABLES???**this has to be taken care of****
    (dmap-objects (schema-vars si) mapping) ;; ???
    si ;return si
)
|#
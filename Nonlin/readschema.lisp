;;;-*- Mode: Lisp; Package: NONLIN -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;    Application:  NONLIN
;;;;  Appl. Version:  1.3
;;;;     Appl. Date:  Oct. 1992
;;;;
;;;;           File:  READSCHEMA.LISP
;;;;       Contents:  Functions for reading in schema definitions.
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
;;;;   9/22/91  bpk  added support to store goal specification in *goals*

(in-package :nonlin)

(defun autocond-p () 
   "Returns T if autocond enabled."

    *autocond*
)    


(defun reset-schematable ()
   "Reset Schema Table."

   (setf *schematable* (make-hash-table))
)

    
(defun dump-schematable ()
   "Print Schema Table."

    (format t "~%The contents of the Schema Table are...~%~%")
    (maphash #'(lambda (key schemas) 
		       (declare (ignore key)) 
		       (for (schema :in schemas)
			    :do (format t "~%-------------------------------------------~%")
			    (print-schema schema)
		       ))
	*schematable*)
    (format t "~%=================================================")
)
;; Note: opschemas and actschemas are processed identically:

(defmacro opschema (name &rest body)
   "Define an opschema.  Schema is entered into Schema Table."

    `(apply 'readschema ',name ',body)
)

    
(defmacro actschema (name &rest body)
   "Define an actschema  Schema is entered into Schema Table."

    `(apply 'readschema  ',name ',body)
)

				     				      
(defmacro plan-schema (name &rest goal-specs)
   "Define the plan schema which holds the planning problem and is stored in *planschema*."

    ;;; plan schema template is: (plan-schema <plan-name> {<goalname> :goal <goal pat>}*)

    `(progn (setf *planschema* 
	       (readschema ',name :todo 'plan-schema :dont-enter t :expansion ',goal-specs))
            (setf *goals* ',goal-specs)
     )
)

    
(defun readschema (name &rest body &key variables &allow-other-keys)
   "Process a schema definition with variables."

    ;; Called by opschema, actschema, and plan-schema macros.

    (let ((new-body body))

         ;; substitute gensym'd names for variables appearing in schema body.

	 (for (pcvar :in variables)
	      :do (setf new-body 
			(pcvar-subst (pcvar-id pcvar) 
			    (make-pcvar :id (gensym (symbol-name (pcvar-id pcvar))))
			    new-body)))

         ;; if you want to change the id only, then send just gensym and in
         ;; pcvar-subst just change the id, of any pcvar with substitutable id
	 
	 ;; (Basically to escape the argument evaluation do:)   
	 (apply #'readschemafn name new-body)
))

        
(defun readschemafn (name &rest body 
                          ;; op/actschema fields:
                          &key todo expansion orderings variables conditions effects
                          duration window
			  dont-enter) ; enter schema into table unless this is nil

   "Process a schema definition with variable names substituted.  Returns schema structure."

    ;;; Called by readschema.

    ;;; orderings  = list of: (n1 -> n2>
    ;;; expansion  = list of: (step-name step-type pattern)
    ;;; conditions = list of: (cond-type pattern :AT step-name :FROM step-name)
    ;;; effects    = list of: (step-name effect-type pattern)

    (multiple-value-bind (strip size expan-trans-list mod-conditions mod-effects)
        ;; make expansion strip for schema
	(make-strip orderings expansion conditions effects 
                    window duration) ; DEVISER mod

	(let ((sconditions (get-sconditions mod-conditions expan-trans-list))
	      (seffects    (get-seffects    mod-effects    expan-trans-list))
              (window      (create-window window)) ; DEVISER mod
              (duration    (init-duration duration))        
	      schema
             )
              
           ;; make new schema structure
	   (setf schema (make-schema :name name
			             :todo todo :strip strip :size size
			             :conditions sconditions :effects seffects :vars variables
                                     :duration duration :window window)) ;DEVISER mod

	   (unless dont-enter (enter-schema-table schema)) ; enter schema into schema table
	    
           (return-from readschemafn schema)
)))

	      
(defun make-strip (orderings expansion conditions effects swindow sduration)
   "Make strip (array of nodes) for schema expansion.  Returns strip stripsize 
    expan-trans-list mod-conditions mod-effects."

    ;;; Called by readschemafn.

    ;;; orderings  = list of: (n1 -> n2>
    ;;; expansion  = list of: (step-name step-type pattern)

    (let* ((window (create-window swindow))      ; DEVISER mods
           (duration  (init-duration sduration))
           (pretable  (make-hash-table)) ; holds predecessor relations
	   (succtable (make-hash-table)) ; holds successor relations
	   (expsteps  ; expsteps contains step names/numbers of the expansion
                      (for (exp-spec :in expansion) 
			             :save (car exp-spec)))
	   begstep-s endstep-s ; steps with no predecessors/successors
	   (strip (make-array (list (+ (length expsteps) 2)))) ; at most two dummy nodes
	   (stripsize 0)
	   (mod-conditions conditions)
	   (mod-effects    effects)
	   expan-trans-list
	  )
           
          ;; note: orderings describes a (partially-ordered) network of nodes (steps)

	  ;; update the pretable and succtable for each entry in orderings:
          ;; "(n1 -> n2)" or "(n2 -> n1)".
	  (for (order :in orderings)
	       :do (let ((n1  (first  order))  ; first node (step)
			 (ord (second order))  ; ordering relation
			 (n2  (third  order))) ; second node (step)
			(cond ((eq ord '->)
				(push n1 (gethash n2 pretable))
				(push n2 (gethash n1 succtable)))
			      ((eq ord '<-)
			       (push n2 (gethash n1 pretable))
			       (push n1 (gethash n2 succtable)))
			      (t 
				 (error "Unknown ordering relation in schema ~s."
				        ord)))))

          ;; find beginning/ending step(s)
	  (for (n :in expsteps)
	       :do 
	       (if (null (gethash n pretable))  ; there are no predecessors for this step
		   (push n begstep-s))
	       (if (null (gethash n succtable)) ; there are no successors for this step
		   (push n endstep-s)))

	  (if (> (length begstep-s) 1) ; if there are more than one beginning step,
	      (then  ; make new dummy node that precedes all beginning steps:
		   (push `(beg-exp :dummy) expansion)
		   (setf  (gethash 'beg-exp succtable)  begstep-s)
		   (for (node :in begstep-s)
			:do (push 'beg-exp (gethash node pretable)))))

	  (if (> (length endstep-s) 1) ; if there are more than one ending step,
	      (then  ; make new dummy node that follows all end steps:
		   (setf expansion (append expansion '((end-exp :dummy))))
		   (setf  (gethash 'end-exp pretable)  endstep-s)
		   (for (node :in endstep-s)
			:do (push 'end-exp (gethash node succtable)))))
	  
	  ;; Construct expansion translation list - list of: (step-name index),
          ;; where index is offset from 0. 
	  (setf expan-trans-list
		(let ((count 0))
		     (for (expentry :in expansion)
			  :save (prog1 (list (first expentry) count)
				       (setf count (+ count 1))))))

          ;; Put expansion steps into strip, an array of nodes.
	  (setf stripsize (length expansion))
	  (setf strip (make-array (list stripsize)))
	  (for (expentry :in expansion)
                   ;; replace step names with array index via translate
	       :do (let ((nodenum   (translate (first expentry) expan-trans-list))
			 (prenodes  (translate (gethash (first expentry) pretable)
				               expan-trans-list))
			 (succnodes (translate (gethash (first expentry) succtable)
					       expan-trans-list))
			 (type      (second expentry))
			 (pattern   (third  expentry))
			 newnode)
		      (setf newnode (make-node :type type :todo pattern
					  :nodenum nodenum
					  :prenodes prenodes 
                                          :succnodes succnodes
                                          :window (copy-window window)
                                          :duration duration)) ; DEVISER mod
		      (setf (aref strip nodenum) newnode)
	  ))

          ;; Add automatic conditions (if autocond enabled).
          ;; Returns updated conds, effects as mod-conditions, and mod-effects.
	  (if (autocond-p)
	      (for (exp-spec :in expansion)
		   :when (eql (second exp-spec) :goal)
		   :do
		   (let* ((stepname (first exp-spec))
			 (goalpat (third exp-spec))
			 eff-template cond-template
			 (cond-atstep (car (gethash stepname succtable)))
			)

			(setf eff-template 
			      `(,stepname :assert ,goalpat))
			(unless (find eff-template mod-effects :test #'equal)
				(push eff-template mod-effects))
;;;			(setf cond-template 
;;;			      (find goal-pat 
;;;				    (for (cond :in mod-conditions)
;;;					 :when (member stepname cond)
;;;					 :save cond)
;;;				    :key #'second
;;;				    :test #'equal))
			;;this is to check if there is already such a 
			;;condition template
			(if (null cond-template)
			    ;;if no such template exists already
			    (push `(:precond ,goalpat :at ,cond-atstep
				       :from ,stepname) mod-conditions)
			)))
	  ) ; end if autocond

    (return-from make-strip 
                 (values strip stripsize expan-trans-list mod-conditions mod-effects))
))
			          
    
(defun translate (item-or-list assoclist)
   "Return translation(s) for item or list of items using assoclist - a list of: 
    (old-val new-val)."

    (if (listp item-or-list)
	   (for (item :in item-or-list)
	        :save (translate item assoclist))
	   (second (assoc item-or-list assoclist)))
)


(defun get-sconditions (conditions expan-trans-list)
    "Process conditions in schema defn. and return list of scondition structures."

    ;;; conditions       = list of: (cond-type pattern :AT step-name :FROM step-name)
    ;;; expan-trans-list = list of: (step-name index)

    (for (cond-spec :in conditions)
	 :save
	 (let ((type          (first  cond-spec))
	       (pattern       (second cond-spec))
	       (atstep        (getf (cddr cond-spec) :at))
	       (contributor-s (getf (cddr cond-spec) :from))
               atnode cont-nodes
              )

	   (unless (listp contributor-s) (setf contributor-s (list contributor-s)))

	   (when (and (not (autocond-p)) (null atstep))
	      ;; This occurs when the expansion read-in has more than one ending step.
	      ;; If autocond is on this would have been handled by make-strip.
	      (setf atstep 'end-exp)
           )

           (setf atnode     (translate atstep expan-trans-list)) ; strip node requiring cond.
	   (setf cont-nodes (for (cstep :in contributor-s)       ; strip nodes contributing cond.
				 :save (translate cstep expan-trans-list)))
	   (make-scondition :atnode atnode :type type :contributors cont-nodes :pattern pattern)
    ))
)


(defun get-seffects (effects expan-trans-list)
    "Process effects in schema defn. and return list of seffect structures."

    ;;; effects    = list of: (step-name effect-type pattern)
    ;;; expan-trans-list = list of: (step-name index)

    (for (effect-spec :in effects)
       :save
       (let ((atstep  (first  effect-spec))
	     (type    (second effect-spec))
	     (pattern (third  effect-spec))
	     atnode
            )
	 (setf atnode (translate atstep expan-trans-list)) ; strip node giving effect
	 (make-seffect :type type :atnode atnode :pattern pattern)
    ))
)	      
	             

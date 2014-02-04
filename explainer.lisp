;;; -*- Mode: LISP; Syntax: Common-lisp; Package: Meta-aqua; Base: 10 -*-

(in-package :metaaqua)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	    The Meta-AQUA Introspective Multistrategy Learning System
;;;;				   Version 6
;;;;
;;;;	     Copyright (C) 1996   Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;			     File: explainer.lisp
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







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;           XP HANDLING
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; 
;;; Function apply-xp applies an explanation to a particular concept instance.
;;; The instance parameter is some relation such as the actor slot of a MOP.
;;; Note that the function will also act as a predicate since if the xp
;;; application is successful the function returns the xp, otherwise it returns
;;; nil.
;;;
;;; The following comment is taken from explain.lisp (partially ported version
;;; of explain function from original AQUA in T) in directory ~cox/Aqua/Main :
;;; 
;;;   When we apply the XP to the instance three things may happen:
;;;   1. The XP is applicable (all pre-XP-nodes have truth values of in.0),
;;;   2. the XP is inapplicable (some of the pre-XP-nodes do not have truth
;;;      values of in.0), or
;;;   3. the XP contradicts the instance (some pre-XP-node has a value different
;;;      from that of the instance). If exists contradiction, then recursively
;;;      explain. ||||| This added explanation still needs to be performed.
;;; 
(defun apply-xp (xp instance)
  (assert (not (or (null xp) (null instance)))	; Entry conditions
          (xp instance)
          "~%Parameters to apply-xp must be non-null.~%")
;;;   (with-character-style (*Style*)
    (format					; Print preliminary feedback.
      *aqua-window*
      (str-concat
	"~%Trying explanation ~s~%  "
	"on instance role ~s.~%")
      xp instance)
    (let ((merged-node			        ; explains-node merged w/copy of
	    (xp-applicable-p			; instance, if applicable.
	      xp
	      (f.get xp
		     *explains-node*)
	      instance)))
  (do-break apply-xp)
      (cond
	(merged-node				; If xp was applicable, merged node non-nil
	 (format
	   *aqua-window*			; Announce success.
	   (str-concat
	     "~%Explained node ~s successfully "
	     "~%  unified with instance.~%")
	   (f.unify instance merged-node))	; Explains-node merged w/actual instance.
;;; 	 (break)
	 xp)					; Return xp.
	(t					; Otherwsie nil node means no applicable xp.
	 (format
	   *aqua-window*
	   "~%Explanation ~s does not apply to ~s."
	   xp instance)
	 nil)))					; Return nil.
;;;     )
  )



;;;
;;; Function complain-wrong-xp prints a message to the user that the chosen xp
;;; does not apply to the current instance. This is detected by predicate
;;; xp-applicable-p when the pre-xp-nodes in unknown-list are not all in the
;;; current set of beliefs. If show-bugs flag is nil then we do not print the
;;; message. The function returns nil always.
;;;
(defun complain-wrong-xp (xp instance unknown-list
			  &optional (show-bugs? *Show-Bugs*))
  (when show-bugs?
    (format
      *aqua-window*
      "~%XP ~s does not apply to ~%"
      xp)
    (format
      *aqua-window*
      "  instance ~s because missing knowledge~%"
      instance)
    (format
      *aqua-window*
      "  ~s~%"
      unknown-list))
  nil)


(defun check-pre-xp-nodes (xp)
  (do-break check-pre-xp-nodes)
  (let ((prototype (frame-def xp)))
    (mapcan
      #'(lambda (each-pre-xp-node
		 each-prototype-node)
	  (if (eq
		(f.get
		  each-prototype-node
		  'truth)
		*out*)
	      (if (in-set-of-beliefs-p
		    each-pre-xp-node)
		  (list each-pre-xp-node))
	      (if (not (in-set-of-beliefs-p
			 each-pre-xp-node))
		  (list each-pre-xp-node))))
      (f.get xp 'pre-xp-nodes)
      (mapcar					; Produce a list of pre-xp-nodes
	#'(lambda (each-pre-xp-ptr)
	    (f.get
	      prototype
	      (var->role each-pre-xp-ptr)))	; Assumes every pre-xp-ptr is a variable binding.
	(f.get prototype 'pre-xp-nodes)))))



(defun tmxp-filter (frame)
  "Returns t if the given frame is a tmxp."
  (eq (frame-type frame) 'trace-meta-xp)
  )

;;;
;;; The explains-node is the EXPLAINS node of the xp.  It is then unified with
;;; the a copy of the instance in order to check whether or not the xp
;;; applies. A copy is used so we do not have to backup if unsuccessful.  The  
;;; xp is applicable if all nodes on the xp's pre-xp-nodes slot are in the set
;;; of beliefs.
;;;
;;; If successful unification and pre-xp-node checking, the unified
;;; explains-node is returned (so this function is not exactly a true predicate
;;; that returns t on success), otherwise if the error was simply unification
;;; error, then if the two conflicting nodes are similar the function will try
;;; to tweak them. In all other cases nil is returned.
;;; 
(defun xp-applicable-p (xp explains-node instance)
  (do-break xp-applicable-p)
  (let ((unknown-list nil)
	(failures nil)
	(error-path nil))
    (cond ((and
	     (if (f.get instance *explanations-slot*)
		 (multiple-value-setq
		   (xp failures error-path)
		   (f.unify xp
			    (first (f.get instance *explanations-slot*))
			    nil))
		 (f.put (list xp) instance *explanations-slot*))
	     (multiple-value-setq
	       (explains-node failures error-path)
	       (f.unify explains-node
			(setf instance
			      (f.copy-instantiated-frame
				instance))
			nil))
	     )
	   (if (null (setf unknown-list
			   (check-pre-xp-nodes xp)))
	       explains-node
	       (complain-wrong-xp
		 xp
		 instance
		 unknown-list
		 t)))				; For now always show bugs, regardless of *Show-Bugs*
	  (t					; Otherwise report unify failure & try to tweak if similar.
	   (format
	     *aqua-window* 
	     "~%XP-Application fails.~%~s and ~s would not unify"
	     explains-node
	     instance)
	   (format
	     *aqua-window*
	     "~%because incompatibility in subnodes ~s"
	     failures)
	   (format
	     *aqua-window*
	     "~%along path ~s."
	     error-path)
	   ;; ||||| Does the following algorithm still work?
	   (cond ((similar-p failures)
		  (do-break xp-applicable-p)
		  (tweak failures)
		  ;; ||||| NOTE that the following parameters are from function apply-xp I think.
		  ;; Need to fix if we are to use quasi-analogy.
		  (f.put! (cons xp
				(f.get instance *explanations-slot*))
			  instance 
			  *explanations-slot*)))
	   nil)))
  )



;;; 
;;; Not any longer just simple look-up on (action actor) pairs.
;;; Items (including xps) are indexed by the role, domain, and
;;; co-domain of relations.
;;; 
(defun retrieve-xps (role)
  (let ((found-xps
	  (retrieve-memory
		   'xp-type.0
		   role)))
    (do-break retrieve-xps)
;    (with-character-style (*Style*)
      (if found-xps
	  (format
	    *aqua-window*
	    (str-concat
	      "~%Found explanation(s)~%  "
	      "~s.~%" )
	    found-xps)
	  (format
	    *aqua-window*
	    "~%No explanation found.~%"))
;      )
    
    found-xps))



(defun kill-status (frame)
  (f.put! *nil* frame *status-slot*)
  frame)


;;;
;;; Function announce-xp simply prints the name of the xp along with its frame
;;; variable designation.  If the xp has no english designation, then print no
;;; name. The function is passed and then returns the xp.
;;; 
(defun announce-xp (xp)
  (if xp
      (format
	*aqua-window*
	"~%Explanation is ~s.~%"
	(or (say-xp xp)
	    (print "No name")))
      ;; It is an error because, although the retrieved xp might  not apply, the
      ;; input concept should at least have a dummy xp on its *explanations* slot.
      (format
	*aqua-window*
	"ERROR in function explain."))
  ;; ||||| Should this format statement be present?
  (format *aqua-window* "~%XP: ~s.~%" xp)
  xp
  )



;;; 
;;; Predicate dummy-explanation-p returns t if the given xp has no
;;; internal-nodes or xp-asserted-nodes, nil otherwise. Since there may exist
;;; an explains node, the pre-xp-node set may be non-empty, yet the xp may
;;; still be a dummy. Dummy xps occur in representations for why questions,
;;; since to ask the question "Why did x occur?" we put a dummy xp on the
;;; explanations slot of x and make the xp with a question status.
;;;
(defun dummy-explanation-p (xp)
  (or (null xp)
      (not (or
	     (f.get xp 'internal-nodes)
	     (f.get xp 'xp-asserted-nodes))))
  )


;;; 
;;; Explain no longer has to retrieve the explanation, only apply it and then
;;; check for questions. The function is guaranteed to be executed only if an
;;; XP has already been retrieved and placed in the reasoning trace.
;;;
;;; Note that the optional parameter is used to pass the function an xp
;;; directly, instead of retrieving it from memory. This is used, for example,
;;; when explaining with an xp provided by the story.
;;; 
(defun explain (concept d-c-node k-goal &optional given-xp (automatic? t))
  (do-break explain)
  (when (and
	concept
	(or automatic?
	    (y-or-n-p "Explain this ? ")))
;      (with-character-style (*Style*)
	(format
	  *aqua-window*
	  "~%~%Explaining concept ~s.~%"
	  concept)
	(let*
	  ((candidate-list 
	     (or (if (not (listp given-xp))	; Note that nil is a null list,
		     (list given-xp)
		     given-xp)			; so if given-xp is nil, it will be passed here.
		 (f.get
		   (f.get
		     (first
		       (return-decision-basis
			 d-c-node))
		     'believed-item)		; Get xp-index.
		   'memory-item)))		; Get previously retrieved XP list from index.
	   (xp
  	     (f.unify
	       (gen-sub-questions		; Returns the xp returned by apply-xp.
		 (some
		   #'(lambda (each-xp)
		       (apply-xp
			 (if (frame-var-p each-xp)
			     ;;||||||Temporary measure until f.unify is fixed. [cox 2may94]
			     (kill-status
			       (f.copy-instantiated-frame ;;Use a copy so that the retrieved explanation
				 each-xp))	;can be reused  in the future.
			     (f.instantiate-frame
			       (*FRAME* each-xp)))
			 concept))
		   candidate-list))
	       (first 
		 (or (f.get concept
			    *explanations-slot*)
		     ;; |||||| Will xp, below, be interpreted as the local var or global?
		     (f.put (list (f.instantiate-frame xp))
			    concept
			    *explanations-slot*)))
	       )
	     ))
	  (announce-xp xp)
	  ;; Put backpointer to the reasoning chain which resulted in this explanation.
	  ;; I believe that the results of the following step is no longer used. [cox 18feb95]
;;; 	  (putprop xp 'train (processing-trace k-goal))
	  ;; Post goal to verify hypothesis.
	  (if (not (isa-p 'introspective-meta-xp (list xp)))
	      (spawn-sub-goal
		*current-character*
		(make-goal-state 'test
				 *current-character*
				 xp)
		'knowledge-acquisition-goal
		'seven.0
		k-goal))
	  xp)
;	)
    )
  )



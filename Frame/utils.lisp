;;; -*- Mode: LISP; Syntax: Common-lisp; Package: Miscellaneous; Base: 10 -*-

(in-package :frames)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	 A Frame System for conceptual construction
;;;;
;;;;	   Copyright (C) 1994   Michael T. Cox   (cox@cc.gatech.edu)
;;;;
;;;;				   20 May 1994
;;;;
;;;;				 File: utils.lisp
;;;;
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
;;           UTILITIES
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(rv return-last-element remove-nil-lists
	  cls multi-val-every concat add-dot 
	  strip-dot-zero ends-with-dot-zero-p 
	  create-self-ref-var create-new-var init-prop 
	  add-assoc/s utils-output-var	macivory-p      ;print function no longer used.
	  add-break do-break un-break *Break-List*	;See break-facility.lisp for these.
	  add-monitor eh un-monitor *Monitor-List* 
	  *Debug-On* *Style* *Show-Bugs*
	  clear-symbols list-symbols str-concat
	  recursive-member filter format-if module-backup
	  eleventh remove-nth)
	)


(defvar *Style* '(:dutch :roman :very-large))


(defparameter *Debug-On* nil
  "Control the printing of trace information.")

;;;
;;; The *Debug-On* flag controls whether or not highly verbose output is given
;;; to the user.
;;; 

(defun f.debugon ()
  (setf *Debug-On* t))

(defun f.debugoff ()
  (setf *Debug-On* nil))



;;;
;;; Hopefully the following program parameter will be obsolete.
;;; 
(defparameter *Show-Bugs* t
  "If nil then suppress output that shows buggy performance.")


(defvar *utils-stream* t			; The program variable should be bound to the output stream
  "Output stream for frame system.")		; the application uses by calling utils-output-var.
						; By default (t) the output goes to *standard-output*.

;;; 
;;; Function utils-output-var creates an association between
;;; the user's symbol passed in parameter user-stream and
;;; the MISC package's symbol *utils-stream*. All output
;;; from functions in the Miscelaneous package is directed through
;;; *utils-stream*. The result of this function, however, is
;;; that a user application can subsequently change the
;;; direction of all Misc package's output by simply changing the
;;; stream associated with the symbol user-stream.
;;;
;;; NOTE that user-stream must be a symbol, NOT a stream itself.
;;; Therefore the parameter is usually quoted when the function is called.
;;;
;;; A Symbolics example:
;;;
;;; (defvar *window1*
;;;   (tv:make-window 'dw:dynamic-lisp-listener :edges-from :mouse ))
;;; (defvar *window2*
;;;   (tv:make-window 'dw:dynamic-lisp-listener :edges-from :mouse ))
;;; (defvar *my-output* *window1*)
;;; (utils-output-var '*my-output*) ;All utils output goes to 1st window
;;; (setf *my-output* *window2*)      ;All utils output goes to 2nd window
;;; (setf *my-output* *window1*)      ;All utils output goes to 1st window again
;;; 
(defun utils-output-var (user-stream)
  "Cause all utility output to go to user stream."
  (setf *utils-stream*
	(make-synonym-stream
	  user-stream))
  )



;;;
;;; Predicate macivory-p tests for macivory environment. Returns t if system
;;; running on a macivory, nil otherwise.
;;; 
(defun macivory-p ()
  (string= "Symbolics MacIvory model 3" (machine-type))
  )


;;;
;;; The following functions were to be a memory monitor to break when
;;; particular locations in memory (specified by identifiers) were touched or
;;; changed. |||||| Never finished.
;;;

(defvar *Monitor-List* nil
  "The list of identifiers to monitor for reference.")

(defmacro add-monitor (&rest identifier-names)
  `(cond ((null (quote ,identifier-names))
	  *Monitor-List*)
	 (t
	  (setf *Monitor-List*
		(union *Monitor-List* (quote ,identifier-names)))
	  (quote ,identifier-names)))
  )

(defmacro un-monitor (&rest identifier-names)
  `(cond ((null (quote ,identifier-names))
	  (let ((temp *Monitor-List*))
	    (setf *Monitor-List* nil)
	    temp))
	 (t
	  (setf *Monitor-List*
		(set-difference *Monitor-List* (quote ,identifier-names)))
	  (quote ,identifier-names)))
  )


;;; (defvar *level* 0)

(defun my-trace (form)
  (let ((*evalhook* 'my-eval-hook-fun))
    (eval form)))

;;; (export '(my-trace my-eval-hook-fun))

;;; (compile 'my-trace)

;(defun my-eval-hook-fun (form &optional env)
;  (let (val (*level* (+ *level* 3)))
;    (format *trace-output* "~%~V@T==>~S" *level* form)
;    (setq val
;	  (evalhook form #'my-eval-hook-fun nil env))
;    (format *trace-output* "~%~V@T~S" *level* val)
;    val))

(defun my-eval-hook-fun (form &optional env)
  (cond ((member form *Monitor-List*)
	 (format *trace-output* "~%Monitored changes to variable ~s" form)
	 (format *trace-output* "~%+  : ~s" +)
	 (format *trace-output* "~%++ : ~s" ++)
	 (format *trace-output* "~%+++: ~s~%~%" +++)))
  (evalhook form #'my-eval-hook-fun nil env)
  )

;(defun a (arg1 arg2)
;  (b arg1 arg2))
;
;(defun b (x y)
;  (c x y))
;
;(defun c (x y)
;  (list x y))
;
;(my-trace '(car (a 1 3)))

;;;
;;; Eval hook.
;;; 
(defun eh (form &optional env)
  (if (member form *Monitor-List*)
      (format *utils-stream* "Monitored changes to variable ~s~%" form))
  (eval form)
  )


;;; Sets the current window's reverse video attribute.
;;;  The default is to set it to white-on a black background.
;;; If passed a nil parameter then it resets the window to
;;; black on a white background.
;;; 
(defun rv (&optional (do-reverse t))
  (send *utils-stream* :set-reverse-video-p do-reverse)
  )


;;;
;;; Function return-last-element returns nil if alist is the
;;; empty-list, or recursively calls itself looking for the
;;; last element. It returns this value when the list becomes
;;; a single element list, and the first clause in the "or"
;;; statement fails, thus causing it to return the right one.
;;; Could have just written (first (last)), but this was more
;;; fun.
;;; 
(defun return-last-element (alist)
  (cond ((null alist)
	 nil)
	(t
	 (or (return-last-element (rest alist))
	     (first alist))))
  )


;;; 
;;; Function remove-nil-lists returns a copy of alist with all nils removed,
;;; including those in sublists of alist.
;;;
;;;
;;; Could be better done with the following instead:
;;; 
;;; (defun remove-nil-lists (alist)
;;;   (mapcan #'(lambda (each-item)
;;; 	      (and each-item (list each-item)))
;;; 	  alist))
;;;
;;; Not really, since the above code does not remove nils from sublists.
;;; 
;(defun remove-nil-lists (alist)
;  (cond ((null alist)
;	 nil)
;	((null (car alist))
;	 (remove-nil-lists (cdr alist)))
;	((atom (car alist))
;	 (cons (car alist)
;	       (remove-nil-lists (cdr alist))))
;	(t
;	 (append (remove-nil-lists (car alist))
;		 (remove-nil-lists (cdr alist)))))
;  )


;;; 
;;; Unfortunately this will not work because, as stated above, it does not
;;; remove sub-nils.  [31oct94]
;;;
;;; Works now [4nov94]
;;; 
(defun remove-nil-lists (alist)
  (filter alist
	  #'(lambda (item)
	      (if item t)))			; Could also be (not (null item)) which may be simpler.
  )


;;;
;;; Function filter takes a list and a predicate, returning a subset of the
;;; list consisting of those items passing the predicate's test.  [31oct94]
;;;
;;; Recursive version written [4nov94]
;;;
(defun filter (alist predicate &optional recursive?)
  "Filter all items from alist not passing predicate."
  (mapcan #'(lambda (each-item)
	      (let ((argument 
		      (if (and recursive?
			       (listp each-item))
			  (filter each-item predicate recursive?)
			  each-item)))
		(if
		  (funcall predicate argument)
		  (list argument))))
	  alist)
  )


;;;
;;; Function cls clears the output screen. This is Symbolics specific code.
;;; 
(defun cls ()
  (send *utils-stream* :clear-history)
  )



;;; The function multi-val-every is a version of every which will
;;; return the multiple value returned by the test-function, not
;;; just t or nil. For example:
;
;(defun x (a b c)
;  (if (and (equal a b)
;	   (equal b c))
;      (values t nil)
;      (values nil (list a b c))))
;
;(multi-val-every #'x '(q a b c g) '(q a b f g) '(q a b c g))
; -> nil, (C F C)
;
(defun multi-val-every (test-function parameter &rest more-parameters)
  (let ((return-vals (multiple-value-list
		       (apply test-function
			      (cons (car parameter)
				    (mapcar #'car more-parameters))))))
    (cond ((or (null (car return-vals))
	       (null (cdr parameter))
	       (notevery #'cdr more-parameters))
	   (values-list return-vals))
	  (t
	   (values-list 
	     (multiple-value-list
	       (apply #'multi-val-every
		      (cons test-function
			    (cons (cdr parameter)
				  (mapcar #'cdr more-parameters))))))))))



;Takes 2 or more symbols and makes one symbol that was the concatenation
;of the two.
;
(defun concat (symbol1 symbol2 &rest more-symbols)
  (if more-symbols
      (apply #'concat `(,(concat symbol1 symbol2) 
			,(car more-symbols) 
			,@(cdr more-symbols)))
      (intern
	(coerce
	  (append
	    (coerce
	      (string symbol1)
	      'list)
	    (coerce
	      (string symbol2)
	      'list)
	    )
	  'string))))


(proclaim '(inline add-dot))
;||||| Package might be other than USER.
(defun add-dot (symbol)
  (intern
   (coerce
    (append
     (coerce
      (string symbol)
      'list)
     (list #\.))
    'string)))
;    'string) 'USER))


(defun ends-with-dot-zero-p (symbol)
  (let ((string-list
	  (coerce
	    (string symbol)
	    'list)))
    (and (eq #\0
	     (return-last-element
	       string-list))
	 (eq #\.
	     (return-last-element
	       (butlast
		 string-list)))))
  )


(defun strip-dot-zero (symbol)
  (if (ends-with-dot-zero-p symbol)
      (intern
	(coerce
	  (butlast
	    (butlast
	      (coerce
		(string symbol)
		'list)))
	  'string)))
  )



(defun create-self-ref-var (symbol &optional package)
  (intern
   (coerce
    (append
      (list #\=)
      (coerce
	(string symbol)
	'list)
      )
    'string)
   (or package
       *package*))
  )


(proclaim '(inline create-new-var))
;;;
;;; Function create-new-var is a general routine to create a unique identifier
;;; (e.g., a frame variable).  Given an input symbol, it appends a period and a
;;; unique number. For instance, (create-new-var 'person) --> person.101
;;; 
(defun create-new-var (symbol)
  (gentemp 
   (string
    (add-dot symbol))))


;;; 
;;; This initialization routine has the option of passing to it two lists: a
;;; list of keys and a list of values.
;;; 
(defun init-prop (symbol property &rest associations)
  (if (null associations)
      (setf (get symbol property) nil)
      (setf (get symbol property) 
	    (pairlis (first associations)
		     (second associations)))))


;Function add-assoc/s has the options of passing either a key and value
; or a list of keys and a list of coresponding values to be added to
; the already existing association list on the property list of symbol.
;
(defun add-assoc/s (symbol property key/s value/s)
  (setf (get symbol property)
	(if (atom key/s)
	    (acons key/s value/s (get symbol property))
	    (pairlis key/s value/s (get symbol property)))))



;;; 
;;; Normal definition of print is SYS:IO;print.
;;; 
;;; ||||| I had commented out the use of *aqua-window*. Why? 
;;; I just put it back & commented out what was there before today.
;;; Does it have to do with *aqua-window* not being in effect at compile
;;; time when the frame definition files are being processed? I guess
;;; it does. Had to put it back to the way it was, and the way it
;;; stands now. 21Dec91
;;;
;;; Note that *aqua-window* is obsolete now. Would be *utils-stream*. 8Sep93
;;; 
;(defun print (arg)
;;;;   (format *aqua-window* "~s" arg)
;  (format t "~s" arg)
;  arg
;  )


;;;
;;; Macro nth-value is taken from p. 184 Steele's Common LISP:The language
;;; (2ed).  X3J13 voted to include it in Jan 89, but it is apparently not in the
;;; current version of LISP on the Symbolics.
;;;
;;; Commented out. [cox 6mar97]
;;; 
;(defmacro nth-value (n form)
;  `(nth ,n (multiple-value-list ,form))
;  )

;;;
;;; Function clear-symbols will loop over all symbols in a given package,
;;; removing the symbol values, property lists, and function declarations from
;;; them. The function is useful in starting a clean LISP environment without
;;; having to cold reboot the machine.
;;;
;;; |||||| Careful using this function. Is not debugged.
;;;
(defun clear-symbols (package-name)
  (let ((original-package *package*))
    (in-package package-name)
    (mapc
      #'(lambda (each-symbol)
	  (makunbound each-symbol)
	  (fmakunbound each-symbol)
	  )
      (list-symbols package-name))
    (in-package original-package))
  )
  
;;; 
;;; Function list-symbols collects and returns as a list all internal and
;;; external symbols of package package-name.
;;;
(defun list-symbols (package-name)
  (let ((symbol-list nil))
    (do-symbols (each-symbol package-name)
      (if (member (nth-value
		    1
		    (intern
		      (string each-symbol)))
		  '(:internal :external))
	  (setf symbol-list
		(cons each-symbol
		      symbol-list))))
    symbol-list)
  )


;;; 
;;; Function str-concat concatenates one or more strings.
;;; 
(defun str-concat (astring &rest more-strings)
;  (cond ((or (null more-strings)
;	     (null (first more-strings)))
;	 astring)
;	((stringp astring)
;	 (str-concat (format
;		       nil
;		       "~a~a"
;		       astring
;		       (first more-strings))
;		     (rest more-strings)))
;	(t
;	 (format
;	   *utils-stream*
;	   "ERROR in str-concat: Arg not a string.")))
  ;; The (presumably) more efficient method.
  (apply #'concatenate `(string ,astring ,@more-strings))
  )



;;;
;;; Function recursive-member is a simple-minded version of a search for a
;;; symbol in a tree structure. It uses eq and cannot be passed a :test arg.
;;; Thus, it cannot search to find substrings that are 'equal, tec. I am sure
;;; that there already exists a function built into LISP to perform this, but I
;;; gave up looking in Steele.
;;;
;;; NOTE that this is a BFS.
;;; 
(defun recursive-member (item list)
  (if (atom list)				; This includes nil and strings.
      nil
      (or (member item list)
	  (mapcan #'(lambda (each-sublist)
		      (recursive-member item each-sublist))
		  list)))
  )


;;;
;;; Function format-if prints to a stream only if the verbose? argument is t
;;; (or non-nil).
;;; 
(defun format-if (verbose?
		  &optional
		  (stream *utils-stream*)
		  (string "~%~s")
		  &rest
		  args)
  "Conditional format function dependent on verbose? argument."
  (if verbose?
      (apply #'format
	     `(,stream ,string ,@args)))
  )



;;;
;;; Function module-backup is designed to be used for copying sets of files to
;;; backup directories. Defaults are specific to the Meta-AQUA system path
;;; names. [cox 19feb95]
;;; 
(defun module-backup (file-list
		      &optional
		      (subdirectory "")
		      (backup-dir "backup/") 
		      (path-name "AZURE:/users/c/cox/Lispm/meta-aqua/")
		      )
  (dolist (each-file file-list)
    (copy-file
      (str-concat
	path-name
	subdirectory
	each-file )
      (str-concat
	path-name
	subdirectory
	backup-dir
	each-file)))
  )



;;;
;;; Function to access the eleventh element of a list.
;;; 
(defun eleventh (alist)
  (tenth (rest alist))
  )

;;;
;;; Function to set the eleventh element of a list.
;;; 
(defun set-eleventh (alist  avalue)
  (setf (nth 10 alist) avalue)
  )

;;;
;;; Now we can call (setf (eleventh my-list))
;;; 
(defsetf eleventh set-eleventh)


;;; 
;;; Function remove-nth returns a list that is equivalent to the input list
;;; except that is missing the nth element of the list.
;;; 
(defun remove-nth (n list &optional (count 1))
  "Remove the nth element from given list."
  (cond ((null list )
	 nil)
	((eq n count)
	 (remove-nth n (rest list ) (+ count 1)))
	(t
	 (cons (first list)
	       (remove-nth n (rest list) (+ count 1))))))
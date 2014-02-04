;;; 
;;; Add the following to your .clinit.cl initialization file. Read
;;; the INSTALL text file.
;;; 

;;; Change the following appropriately.
(setf *Meta-AQUA-system-dir* 
  "/fs/metacog/group/systems/Meta-AQUA/")

(setf *do-compile-Meta-AQUA* nil)

;; Added following two lines to fix the issue with modifying the
;; *readtable* in Nonlin [mcox 26sep13]
(defvar *my-rt* (copy-readtable nil))
(setq *readtable* *my-rt*)


(when (y-or-n-p (concatenate 
		    'string 
		  (if *do-compile-Meta-AQUA* 
		      "Compile " 
		    "Load ")
		  "Meta-AQUA? "))
  (load (concatenate 'string 
	  *Meta-AQUA-system-dir* "loader.lisp"))
  (setf *default-pathname-defaults* 
    (pathname (concatenate 'string 
		*Meta-AQUA-system-dir* "Results/")))
  (when (and (not *do-compile-Meta-AQUA*)
	     (y-or-n-p "Load Meta-AQUA windows?"))
    (init-jlink)
    (make-window)
    (load 
     (concatenate 'string 
       *Meta-AQUA-system-dir* "java-windows/patches"))
    (load 
     (concatenate 'string 
       *Meta-AQUA-system-dir* "java-windows/patches-frame"))
    (format t "~%~%CALL (user::jlinker-end) TO END WINDOWS.~%~%")
    )
  )


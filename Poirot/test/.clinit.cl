(in-package :user)


(unless (find-package "TEST")
  (load "c:/Meta-AQUA/Poirot/test/test-def.cl"
	)
  )  
(print "xxxxx")
(use-package :tst)
(print "fffff")

;; -----------------------------------------------------------------
;; Overrides the values of the variables in file poirot-lisp.cl
;;
(setf TEST::*poirot-java-home* "c:/poirot/coretrunk")

(load "c:/Meta-AQUA/Poirot/test/test.cl")
(describe '*poirot-java-home*)


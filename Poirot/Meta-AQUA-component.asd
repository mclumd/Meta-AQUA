;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-


(defpackage :Meta-AQUA-component-asdf
    (:use :common-lisp :asdf))

(in-package :Meta-AQUA-component-asdf)

;;;; 
;;;; This file defines the POIROT/Meta-AQUA subsystem.
;;;; 
;;;; The .clinit.cl file will load the loader.lisp file which loads
;;;; this file and the component files below. Then at the bottom of
;;;; file .clinit.cl exists the following sequence.
#|
(setf *app* (make-instance 'Meta-AQUA-lisp-app :port-number 9095))
(start *app*)
(load-meta-aqua-windows)
|#


(defsystem :Meta-AQUA-component
    :depends-on (:poirot-lisp-connector)
    :serial t
    :components 
    (
     ;; Representations now in Meta-AQUA proper
     ;;(:file "rep_poirot")		;Background representations for domain
     (:file "patches-poirot")		;Meta-AQUA code patches
     (:file "verifier")			;New XP verification code
     (:file "homework")			;Homework example input stub
     (:file "midterm")			;Midterm example input stub
     (:file "trans2frames")		;Translates blackboard to frame reprs
     (:file "hypo-form")		;Code to form hypotheses
     (:file "component-def")		;Component object definition 
     )
    )


    

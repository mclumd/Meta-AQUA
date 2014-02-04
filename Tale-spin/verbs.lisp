;;; -*- Mode: LISP; Syntax: Common-lisp; Package: TALE-SPIN; Base: 10 -*-

;;; ===========================================================================
;;; TaleSpin
;;; Copyright (C) 1987 James R. Meehan
;;; ===========================================================================

(in-package :tspin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	    Elvis World and the Tale-Spin Story Generation Subsystem
;;;;				 for Meta-AQUA
;;;;
;;;;	             Michael T. Cox   (mcox25@covad.net)
;;;;
;;;;
;;;;			       File: verbs.lisp
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






;;; Independent aspects of a verb:
;;;   tense: present, past, future, present perfect, past perfect, future perfect
;;;   voice: active, passive
;;;   person: first, second, third
;;;   number: singular, plural
;;;   mood: indicative, imperative, subjunctive
;;;   "form": simple, progressive
;;; 
;;; Not all 432 combinations are possible, let alone distinct, in English.
;;; 
;;; ----------------------  active voice ------------------------------------
;;; 	    Simple                 Progressive
;;; 
;;; present     I see you.             I am seeing you.
;;; past        I saw you.		   I was seeing you.
;;; future      I will see you.        I will be seeing you.
;;; pres.pref.  I have seen you.       I have been seeing you.
;;; past.perf.  I had seen you.        I had been seeing you.
;;; fut perf.   I will have seen you.  I will have been seeing you.
;;; 
;;; ---------------------- passive voice -------------------------------------
;;;             Simple                           Progressive
;;; 
;;; present     I am seen by you.                I am being seen by you.
;;; past        I was seen by you.		     I was being seen by you.
;;; future      I will be seen by you.           [I will be being seen by you.]
;;; pres.perf.  I have been seen by you.         [I have been being seen by you.]  
;;; past perf.  I had been seen by you.          [I had been being seen by you.]
;;; fut.perf.   I will have been seen by you.    [I will have been being seen by you.]
;;; 



(defun gen-verb (infinitive &rest l &key (tense 'present) (perfect? nil)
                            (voice 'active) (person 3) (number 'singular)
                            (form 'simple) (neg? nil) (question? nil)
                            subject (contraction? t) (aux? nil)
                            (modal nil) (able? nil))
  (cond ((eq voice 'passive)
         (apply #'gen-verb 'be :voice 'active :aux? t l)
         (say-word (past-participle infinitive)))
        ((eq form 'progressive)
         (apply #'gen-verb 'be :form 'simple :aux? t l)
         (say-word (progressive-form infinitive)))
        (able?
         (cond ((or (eq tense 'future) perfect? modal)
                (apply #'gen-verb 'be :able? nil :aux? t l)
                (say-word 'able) (say-word 'to) (say-word infinitive))
               (t
                (apply #'gen-verb infinitive 
                       :modal (if (eq tense 'present) 'can 'could)
                       :able? nil :aux? t l))))             
        (perfect?
         (apply #'gen-verb 'have :perfect? nil :aux? t l)
         (say-word (past-participle infinitive)))
        ((eq tense 'future)
         (apply #'gen-verb infinitive :modal 'will :tense 'present :aux? t l))
        (modal
         (let* ((c (and contraction? (not question?) (not neg?)
                        (pronoun? subject) (get modal 'pronoun-contractions)))
                (n (and (null c) contraction? neg?
                        (get modal 'neg-contraction))))
           (cond (c (say-pp (getf c subject) 'nominative))
                 (n (if (not question?) (say-pp subject 'nominative))
                    (say-word n)
                    (if question? (say-pp subject 'nominative)))
                 (t (if (not question?) (say-pp subject 'nominative))
                    (say-word modal)
                    (if question? (say-pp subject 'nominative))
                    (if neg? (say-word 'not)))))
         (say-word infinitive))
        ((and neg? (not aux?) (not (eq infinitive 'be)))
         (apply #'gen-verb
                (if (and neg? contraction?) 'not-do 'do)
                :neg? nil :aux? t l)
         (if (and neg? (not contraction?)) (say-word 'not))
         (say-word infinitive))
        ((and question? (not aux?) (not (eq infinitive 'be))
              (not (eq infinitive 'not-be)))
         (apply #'gen-verb 'do :aux? t l)
         (say-word infinitive))
        ((and neg?
              contraction?
              (member infinitive '(be have do) :test #'eq)
              (not (and (eql person 1) (eq tense 'present) (not question?)
                        (eq number 'singular) (eq infinitive 'be)))) ;I aren't
         (apply #'gen-verb
                (case infinitive (be 'not-be) (have 'not-have) (do 'not-do))
                :neg? nil l))
        ((eq tense 'present)
         (cond ((and contraction?
                     (not question?)
                     (member infinitive '(be have) :test #'eq)
                     (pronoun? subject))
                (say-word (getf (getf *present-contractions* infinitive) subject)))
               (t
                (if (not question?) (say-pp subject 'nominative))
                (say-word (case (if (eq number 'singular) person 2)
                         ((1) (or (get infinitive 'present-1st) infinitive))
                         ((2) (or (get infinitive 'present-2nd) infinitive))
                         ((3) (or (get infinitive 'present-3rd)
                                  (setf (get infinitive 'present-3rd)
                                        (intern (format nil "~AS" infinitive)))))))
                (if question? (say-pp subject 'nominative))))
         (if neg? (say-word 'not)))
        ((eq tense 'past)
         (cond ((and contraction?
                     aux?
                     (not question?)
                     (not neg?)
                     (eq infinitive 'have)
                     (pronoun? subject))
                (say-word (getf *had-contractions* subject)))
               (t
                (if (not question?) (say-pp subject 'nominative))
                (say-word
                    (or (get infinitive
                             (case (if (eq number 'singular) person 2)
                               ((1) 'past-1st)
                               ((2) 'past-2nd)
                               ((3) 'past-3rd)))
                        (past-participle infinitive)))
                (if question? (say-pp subject 'nominative))
                (if neg? (say-word 'not)))))
        (t (error "Unknown tense: ~S" tense))))

(defvar *present-contractions*
  '(be (i i\'m you you\'re he he\'s she she\'s it it\'s we we\'re
        they they\'re)
    have (i i\'ve you you\'ve he he\'s she she\'s it it\'s we we\'ve
          they they\'ve)))

(defvar *had-contractions*
  '(i i\'d you you\'d he he\'d she she\'d it it\'d we we\'d they they\'d))

(setf (get 'will 'pronoun-contractions)
      '(i i\'ll you you\'ll he he\'ll she she\'ll it it\'ll we we\'ll
          they they\'ll)
      (get 'would 'pronoun-contractions)
      *had-contractions*)

(dolist (i '((will won\'t) (would wouldn\'t) (should shouldn\'t)
             (must mustn\'t) (can can\'t) (could couldn\'t)))
  (setf (get (car i) 'neg-contraction) (cadr i)))

(defun pronoun? (x)
  (member x '(i you he she it we you they) :test #'eq))

(defun progressive-form (infinitive)
  (or (get infinitive 'progressive-form)
      (setf (get infinitive 'progressive-form)
            (intern (format nil "~AING"
                            (let* ((a (symbol-name infinitive))
                                   (n (1- (length a))))
                              (if (char= (char a n) #\E)
                                  (subseq a 0 n)
                                  a)))))))

(defun past-participle (infinitive)
  (or (get infinitive 'past-participle)
      (setf (get infinitive 'past-participle)
            (intern (format nil "~A~:[E~;~]D"
                            infinitive
                            (let ((a (symbol-name infinitive)))
                              (char= (char a (1- (length a))) #\E)))))))

(defmacro v-conjugate (&rest z)
  `(*v-conjugate ,@(mapcar #'(lambda (x) `',x) z)))

(defun *v-conjugate (infinitive &optional pres-prog past-part pres past)
  (if pres-prog (setf (get infinitive 'progressive-form) pres-prog))
  (if past-part (setf (get infinitive 'past-participle) past-part))
  (if (consp pres)
      (setf (get infinitive 'present-1st) (first pres)
            (get infinitive 'present-2nd) (second pres)
            (get infinitive 'present-3rd) (third pres)))
  (flet ((mark-past (p1 p2 p3)
                    (setf (get infinitive 'past-1st) p1
                          (get infinitive 'past-2nd) p2
                          (get infinitive 'past-3rd) p3)))
    (cond ((null past) nil)
          ((symbolp past) (mark-past past past past))
          (t (mark-past (first past) (second past) (third past)))))
  nil)

(v-conjugate be being been (am are is) (was were was))
(v-conjugate become becoming become nil became)
(v-conjugate do nil done (do do does) did)
(v-conjugate drink nil drunk nil drank)
(v-conjugate drop dropping dropped nil dropped)
(v-conjugate eat nil eaten nil ate)
(v-conjugate find nil found)
(v-conjugate get getting gotten nil got)
(v-conjugate give nil given nil gave)
(v-conjugate go nil gone (go go goes) went)
(v-conjugate grab grabbing grabbed)
(v-conjugate have nil had (have have has))
(v-conjugate hit hitting hit)
(v-conjugate light lighting lit) ;As in to light that ganja [29 jul 93 cox]
(v-conjugate smoke smoking smoked) ;As in to smoke that ganja [29 jul 93 cox]
(v-conjugate bark barking barked) ;Dog barking [cox 8aug93]
(v-conjugate arrive arriving arrived) ;police arriving [cox 8aug93]
(v-conjugate arrest arresting arrested) ;police action of course [cox 26aug93]
(v-conjugate fill filling filled)	;see dsp achieve [cox 27aug93]
(v-conjugate control controlling controlled)	;see dsp cont [cox 1sep93]
(v-conjugate detect detecting detected)	;detect contraband [cox 9feb95]
(v-conjugate sniff sniffing sniffed)	;dog sniffing [cox 9feb95]
(v-conjugate play playing played)	;playing catch [cox 9feb95]
(v-conjugate hit hitting hit)	;hitting a ball [cox 26jun]
(v-conjugate ask asking asked) ;don't know why not here unless it's so regular that there's a default [cox 9feb95]
(v-conjugate know knowing known nil knew)
(v-conjugate let letting let)
(v-conjugate not-be nil been (aren\'t aren\'t isn\'t)
             (wasn\'t weren\'t wasn\'t))
(v-conjugate not-do nil nil (don\'t don\'t doesn\'t) didn\'t)
(v-conjugate not-have nil hadn\'t (haven\'t haven\'t hasn\'t) hadn\'t)
(v-conjugate plan planning planned)
(v-conjugate satisfy nil satisfied)
(v-conjugate see seeing seen (see see sees) saw)
(v-conjugate strike nil struck)
(v-conjugate take nil taken nil took)
(v-conjugate tell nil told)
(v-conjugate think nil thought)

#|
(defun test-verb (infinitive)
  (dolist (voice '(active passive))
    (dolist (form '(simple progressive))
      (dolist (perfect? '(nil t))
        (dolist (tense '(present past future))
          (dolist (neg? '(nil t))
            (dolist (person '(1 2 3)) ; 2 3
              (dolist (number '(singular  plural))
                (dolist (q? '(nil t))
                  (dolist (able? '(nil t))
                    (gen-verb infinitive :tense tense :perfect? perfect?
                              :voice voice :person person :number number
                              :form form :neg? neg? :question? q?
                              :modal 'could
                              :able? able?
                              :subject 
                              (aref '#2A((i you he) (we you they))
                                    (if (eq number 'singular) 0 1) (1- person)))
                    (let ((s (nreverse *sent*)))
                      (format? t "~{ ~A~}~A" s (if q? #\? #\.))))))
              #+nil (terpri?))
            (terpri?)))))))
|#






      

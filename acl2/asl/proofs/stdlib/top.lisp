;;****************************************************************************;;
;;                                ASLRef                                      ;;
;;****************************************************************************;;
;;
;; SPDX-FileCopyrightText: Copyright 2025 Arm Limited and/or its affiliates <open-source-office@arm.com>
;; SPDX-License-Identifier: BSD-3-Clause
;; 
;;****************************************************************************;;
;; Disclaimer:                                                                ;;
;; This material covers both ASLv0 (viz, the existing ASL pseudocode language ;;
;; which appears in the Arm Architecture Reference Manual) and ASLv1, a new,  ;;
;; experimental, and as yet unreleased version of ASL.                        ;;
;; This material is work in progress, more precisely at pre-Alpha quality as  ;;
;; per Arm’s quality standards.                                               ;;
;; In particular, this means that it would be premature to base any           ;;
;; production tool development on this material.                              ;;
;; However, any feedback, question, query and feature request would be most   ;;
;; welcome; those can be sent to Arm’s Architecture Formal Team Lead          ;;
;; Jade Alglave <jade.alglave@arm.com>, or by raising issues or PRs to the    ;;
;; herdtools7 github repository.                                              ;;
;;****************************************************************************;;

(in-package "ASL")

(include-book "align")
(include-book "bits")
(include-book "ilog2")
(include-book "log2")
(include-book "misc")
(include-book "pow2")
(include-book "round")
(include-book "sint")
(include-book "sqrtrounded")
(include-book "uint")
(include-book "shift")
(include-book "../../trace-free")



;; Make sure there's a theorem for every stdlib function.

(assert-event
 (or
  (equal (mergesort
          (acl2::alist-keys (table-alist 'asl-subprogram-table (w state))))
         (mergesort
          (acl2::alist-keys
           (static_env_global->subprograms (stdlib-static-env)))))
  (cw "~%########################################################~%")
  (cw "## COMPLETION CHECK FAILED                            ##~%")
  (cw "## A routine has been added or removed to/from stdlib.##~%")
  (cw "## Proofs need adjustments                            ##~%")
  (cw "########################################################"))
  )


(local
 (defun my-get-thm (name state)
   (declare (xargs :mode :program :stobjs state))
   (er-let* ((ev-wrld
              (acl2::er-decode-logical-name name (w state) 'my-get-thm state)))
     (value (acl2::access-event-tuple-form (cddar ev-wrld))))))

(local
 (defun remove-hints (form)
   (cond ((atom form) nil)
         ((eq (car form) :hints)
          (cddr form))
         (t (cons (car form) (remove-hints (cdr form)))))))

(local
 (defun add-first-hyp (hyp form)
   (if (atom form)
       form
     (case-match form
       (('implies ('and . hyps) concl)
        `(implies (and ,hyp . ,hyps) ,concl))
       (& (cons (add-first-hyp hyp (car form))
                (add-first-hyp hyp (cdr form))))))))
        
(local
 (defun subst-subtree (new old form)
   (cond ((equal form old) new)
         ((atom form) form)
         (t (cons (subst-subtree new old (car form))
                  (subst-subtree new old (cdr form)))))))

(local
 (defun rename-defthm (name form)
   (if (atom form)
       form
     (case-match form
       (('defthm & . rest) `(defthm ,name . ,rest))
       (& (cons (rename-defthm name (car form))
                (rename-defthm name (cdr form))))))))

(local
 (defun conjoin-concl (concl form)
   (if (atom form)
       form
     (case-match form
       (('implies hyps ('b* bindings concl1))
        `(implies ,hyps (b* ,bindings (and ,concl ,concl1))))
       (& (cons (conjoin-concl concl (car form))
                (conjoin-concl concl (cdr form))))))))

(local
 (defun def-stdlib-trace-thm-fn (thmname fn state)
   (declare (xargs :mode :program :stobjs state))
   (b* (((er thm) (my-get-thm thmname state))
        (thm (remove-hints thm))
        (thm (add-first-hyp `(and (trace-free-fnname-p ,fn)
                                  (not (find-call-tracespec ,fn pos tracespec))
                                  (trace-free-ty-timeframe-imap-p
                                   (static_env_global->declared_types static-env)))
                            thm))
        (thm (subst 'eval_subprogram-*t 'eval_subprogram thm))
        (thm (subst-subtree 'static-env '(GLOBAL-ENV->STATIC (ENV->GLOBAL ENV)) thm))
        (thm (subst-subtree '(MV RES NEW-ORAC TRACE) '(MV RES NEW-ORAC) thm))
        (thm (conjoin-concl '(equal trace nil) thm))
        (thm (subst-subtree '(global-replace-static static-env (env->global env))
                            '(env->global env)
                            thm))
        (thm (rename-defthm
              (intern-in-package-of-symbol
               (concatenate 'string (symbol-name thmname) "-*T")
               thmname)
              thm)))
     (value thm))))

(defmacro def-stdlib-trace-thm (thmname fn)
  `(make-event (def-stdlib-trace-thm-fn ',thmname ',fn state)))



(fty::deffixequiv subprograms-match
  :omit (names)
  :hints(("Goal" :in-theory (enable subprograms-match))))


(local
 (defun def-stdlib-trace-thms-aux (table)
   (if (atom table)
       nil
     (b* (((list fn & & & thmname) (car table)))
       (cons `(def-stdlib-trace-thm ,thmname ,fn)
             (def-stdlib-trace-thms-aux (cdr table)))))))

(defmacro def-stdlib-trace-thms (tablename)
  `(make-event
    (cons 'progn
          (def-stdlib-trace-thms-aux
            (table-alist ',tablename (w state))))))


(local (in-theory (disable logext floor logapp mod loghead logbitp expt ceiling
                           abs ash integer-length logtail logcount truncate min
                           max logmask logbit evenp oddp)))

(def-stdlib-trace-thms asl-subprogram-table)
(def-stdlib-trace-thms asl-prim-subprogram-table)


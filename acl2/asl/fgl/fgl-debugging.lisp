;;****************************************************************************;;
;;                              ACL2 ASL Library                              ;;
;;****************************************************************************;;
;;
;; SPDX-FileCopyrightText: Copyright 2026 Arm Limited and/or its affiliates <open-source-office@arm.com>
;; SPDX-License-Identifier: BSD-3-Clause
;; 
;; ****************************************************************************;;



(in-package "ASL")
(include-book "centaur/fgl/ctrex-utils" :Dir :system)
(include-book "centaur/fgl/helper-utils" :Dir :system)


;; ----------------------------------------------------------------------------------
;; Some FGL functions for querying counterexample stuff from the interp-st

#!fgl
(define interp-st-ctrex-bit-length (interp-st)
  (stobj-let ((env$ (interp-st->ctrex-env interp-st)))
             (len)
             (stobj-let ((bitarr (env$->bitarr env$)))
                        (len)
                        (bits-length bitarr)
                        len)
             len))

#!fgl
(define interp-st-get-bvar->term ((n natp)interp-st)
  :guard (stobj-let ((bvar-db (interp-st->bvar-db interp-st)))
                    (ok)
                    (and (<= (base-bvar bvar-db) n)
                         (< n (next-bvar bvar-db)))
                    ok)
  (stobj-let ((bvar-db (interp-st->bvar-db interp-st)))
             (term)
             (get-bvar->term n bvar-db)
             term))

#!fgl
(define interp-st-ctrex-bit ((n natp) interp-st)
  :guard (< n (interp-st-ctrex-bit-length interp-st))
  :guard-hints (("goal" :in-theory (enable interp-st-ctrex-bit-length)))
  (stobj-let ((env$ (interp-st->ctrex-env interp-st)))
             (bit)
             (stobj-let ((bitarr (env$->bitarr env$)))
                        (bit)
                        (get-bit n bitarr)
                        bit)
             bit))


#!fgl
(define interp-st-bvar-ctrex-value ((n natp) interp-st)
  (stobj-let ((env$ (interp-st->ctrex-env interp-st))
              (logicman (interp-st->logicman interp-st)))
             (bit)
             (stobj-let ((aignet (logicman->aignet logicman)))
                        (id)
                        (if (< n (aignet::num-ins aignet))
                            (aignet::innum->id n aignet)
                          (msg "bad input: greater than ~x0" (aignet::num-ins aignet)))
                        (stobj-let ((bitarr (env$->bitarr env$)))
                                   (bit)
                                   (if (natp id)
                                       (if (< id (bits-length bitarr))
                                           (get-bit id bitarr)
                                         (msg "bad bit: greater than ~x0" (bits-length bitarr)))
                                     id)
                                   bit))
             bit))

#!fgl
(define interp-st-bvar-db-ctrex-values-aux ((n natp) (max natp) interp-st)
  :guard (<= n max)
  :measure (nfix (- (nfix max) (nfix n)))
  (if (mbe :logic (zp (- (nfix max) (nfix n)))
           :exec (eql n max))
      nil
    (cons (interp-st-bvar-ctrex-value n interp-st)
          (interp-st-bvar-db-ctrex-values-aux (1+ (lnfix n)) max interp-st))))

#!fgl
(define interp-st-bvar-db-ctrex-values (interp-st)
  (stobj-let ((logicman (interp-st->logicman interp-st)))
             (ins)
             (stobj-let ((aignet (logicman->aignet logicman)))
                        (ins)
                        (aignet::num-ins aignet)
                        ins)
             (interp-st-bvar-db-ctrex-values-aux 0 ins interp-st)))



#!fgl
(define interp-st-bvar-db-debug-with-ctrex-values (interp-st)
  :prepwork ((local (defthm alistp-bvar-db-debug-aux
                      (alistp (bvar-db-debug-aux n bvar-db))
                      :hints(("Goal" :in-theory (enable bvar-db-debug-aux)))))
             (local (defthm alistp-interp-st-bvar-db-debug
                      (alistp (interp-st-bvar-db-debug interp-st))
                      :hints(("Goal" :in-theory (enable interp-st-bvar-db-debug
                                                        bvar-db-debug))))))
  (b* ((ctrex-vals (interp-st-bvar-db-ctrex-values interp-st))
       (bvdb (interp-st-bvar-db-debug interp-st)))
    (pairlis$ (strip-cars bvdb)
              (pairlis$ ctrex-vals
                        (pairlis$ (strip-cdrs bvdb) nil)))))


#!fgl
(define interp-st-bvar-db-check-ctrex-consistency ((n natp) interp-st state)
  :verify-guards nil
  (stobj-let ((env$ (interp-st->ctrex-env interp-st))
              (logicman (interp-st->logicman interp-st))
              (bvar-db (interp-st->bvar-db interp-st)))
             (consist)
             (bvar-db-check-ctrex-consistency n bvar-db nil logicman env$ state nil)
             consist)
  ///
  ;; (skip-proofs (verify-guards interp-st-bvar-db-check-ctrex-consistency))
  )

#!fgl
(define interp-st-bvar-db-obj-ctrex-value ((obj fgl-object-p) interp-st)
  (let ((look (interp-st-get-term->bvar obj interp-st)))
    (and look
         (interp-st-bvar-ctrex-value look interp-st))))


#!fgl
(define interp-st-magic-fgl-object-eval ((x fgl-object-p)
                                          (env fgl-env-p)
                                          interp-st
                                          state)
  :guard (stobj-let ((logicman (interp-st->logicman interp-st)))
                    (ok)
                    (and ;; (bfr-env$-p env$ (logicman->bfrstate))
                     (stobj-let ((aignet (logicman->aignet logicman)))
                                (ok)
                                (eql (aignet::num-regs aignet) 0)
                                ok)
                     (lbfr-listp (fgl-object-bfrlist x))
                     (eql (logicman->mode logicman) 0)
                     ;; (<= (bfr-nvars logicman) (len (fgl-env->bfr-vals env)))
                     )
                    ok)
  :guard-hints (("goal" :in-theory (enable env->env$
                                           fgl::bfr-env$-p)))
  (stobj-let ((logicman (interp-st->logicman interp-st)))
             (err val)
             (b* (((acl2::local-stobjs env$)
                   (mv err val env$))
                  (env$ (env->env$-exec env env$ logicman))
                  ((mv err val)
                   (magic-fgl-object-eval x env$)))
               (mv err val env$))
             (mv err val)))

#!fgl
(define interp-st-ctrex-fgl-object-eval ((x fgl-object-p)
                                         interp-st
                                         state)
  :guard (stobj-let ((logicman (interp-st->logicman interp-st))
                     (env$ (interp-st->ctrex-env interp-st)))
                    (ok)
                    (and ;; (bfr-env$-p env$ (logicman->bfrstate))
                     (bfr-env$-p env$ (logicman->bfrstate))
                     (stobj-let ((aignet (logicman->aignet logicman)))
                                (ok)
                                (eql (aignet::num-regs aignet) 0)
                                ok)
                     (lbfr-listp (fgl-object-bfrlist x))
                     (eql (logicman->mode logicman) 0)
                     ;; (<= (bfr-nvars logicman) (len (fgl-env->bfr-vals env)))
                     )
                    ok)
  :guard-hints (("goal" :in-theory (enable env->env$
                                           fgl::bfr-env$-p)))
  (stobj-let ((logicman (interp-st->logicman interp-st))
              (env$ (interp-st->ctrex-env interp-st)))
             (err val)
             (magic-fgl-object-eval x env$)
             (mv err val)))



#!fgl
(define interp-st-pathcond-to-cube (interp-st)
  :returns (cube satlink::lit-listp)
  (stobj-let ((pathcond (interp-st->pathcond interp-st))
              (constraint-pathcond (interp-st->constraint interp-st)))
             (cube)
             (pathcond-to-cube pathcond (pathcond-to-cube constraint-pathcond nil))
             cube))


(defmacro define-interp-st-run-ctrex-non-guarded ()
  '(skip-proofs
    #!fgl
    (define interp-st-run-ctrex-ng (sat-config
                                    interp-st
                                    state)
      (interp-st-run-ctrex sat-config interp-st state))))



#!fgl
(define interp-st-reference-ctrex-bvar-db-debug (interp-st)
  (stobj-let ((reference-ctrex (interp-st->reference-ctrex interp-st)))
             (alist)
             (stobj-let ((bvar-db (reference-ctrex->bvar-db reference-ctrex)))
                        (alist)
                        (bvar-db-debug bvar-db)
                        alist)
             alist))





;; --------------------------------------------------------------------
;; Cheatsheet section
;; --------------------------------------------------------------------

;; Print counterexample inconsistencies semi-concisely (use with plev though)
#|
(fgl::summarize-bvar-db-consistency-errorlist
 (CDR (HONS-GET :BVAR-DB-CTREX-CONSISTENCY-ERRORS (@ :FGL-USER-SCRATCH))))
|#

;; Access FGL stack after interrupt or control stack overflow (use with plev)
#|
;; store object in (@ :stack)
(fgl::save-fgl-stack)

;; look at top of stack
(@ :stack)

;; look at bottom of stack
(nthcdr (- (len (@ :stack)) 20) (@ :stack))
|#

;; Turn profiling/accumulated-persistence on/off
#|
(local (table fgl::fgl-config-table :prof-enabledp t))
(local (table fgl::fgl-config-table :prof-enabledp nil))
|#

;; Trace a rule to see what it produced/why it didn't apply etc.
#|
(assign :fgl-trace-rewrites t)
(assign :fgl-trace-evisc-tuple '(nil 4 7 nil))
(fgl::fgl-trace (:formula my-rewrite-thm))
|#

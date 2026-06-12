;;****************************************************************************;;
;;                              ACL2 ASL Library                              ;;
;;****************************************************************************;;
;;
;; SPDX-FileCopyrightText: Copyright 2026 Arm Limited and/or its affiliates <open-source-office@arm.com>
;; SPDX-License-Identifier: BSD-3-Clause
;; 
;; ****************************************************************************;;


(in-package "ASL")
(include-book "centaur/fgl/def-fgl-rewrite" :dir :system)
(include-book "centaur/fgl/config" :dir :system)
(include-book "centaur/fgl/checks" :dir :system)
(include-book "centaur/fgl/bitops" :dir :system)
(include-book "centaur/fgl/annotation" :dir :system)
(include-book "centaur/fgl/backtrace" :dir :system)
(include-book "centaur/fgl/ctrex-utils" :dir :system)
(include-book "centaur/fgl/helper-utils" :dir :system)
(include-book "centaur/bitops/rational-exponent" :dir :system)
(include-book "trace-interp" :dir :acl2asl)
(include-book "defs")
(include-book "type-rules")
(include-book "proofs/stdlib/top" :dir :acl2asl)
(include-book "centaur/bitops/limited-shifts" :dir :system)
(local (include-book "centaur/bitops/ihsext-basics" :dir :system))




;; ####################### AlignDownP2 Hack ############################
;; We run into a call of AlignDownP2 inside LDR where the P2 argument is a
;; symbolic 20-something-bit value, even though the width of the bitvector is
;; fixed at 56.  Phrasing AlignDownP2 in terms of limshift-loghead-of-logsquash
;; lets us get around this; otherwise we blow the stack by trying to maybe
;; shift something left by 2^20something bits.

(include-book "proofs/stdlib/align" :dir :acl2asl)
(include-book "proofs/stdlib/ilog2" :dir :acl2asl)
(include-book "proofs/stdlib/sqrtrounded" :dir :acl2asl)

(local (defthm logsquash-in-terms-of-floor
         (equal (bitops::logsquash w x)
                (ash (logtail w x) (nfix w)))
         :hints(("Goal" :in-theory (acl2::enable* bitops::ihsext-inductions
                                                  bitops::ihsext-recursive-redefs)))))

(local (defthm integerp-expt
         (implies (natp n)
                  (integerp (Expt 2 n)))
         :hints(("Goal" :in-theory (enable expt)))
         :rule-classes :type-prescription))

(def-asl-subprogram aligndownp2-correct-fgl-lemma
  :function "AlignDownP2"
  :static-env (stdlib-prim-static-env)
  :params (n)
  :args (x p2)
  :hyps (< 0 n.val)
  :return-values ((v_bitvector n.val
                               (bitops::limshift-loghead-of-logsquash
                                n.val p2.val x.val)))
  :hints(("Goal" :in-theory (enable bitops::ash-is-expt-*-x
                                    logtail)))
  :no-expand-hint t)

(define eval-aligndownp2 (env params args &key (clk 'clk) (orac 'orac))
  :verify-guards nil
  (b* (((mv res orac) (eval_subprogram env "AlignDownP2" params args)))
    (mv res orac))
  ///
  (fgl::remove-fgl-rewrite eval-aligndownp2))

(defthm aligndownp2-correct-fgl
  (B* (((V_INT N))
       ((V_BITVECTOR X))
       ((V_INT P2)))
    (IMPLIES (AND (SUBPROGRAMS-MATCH '("AlignDownP2" "Zeros-1")
                                     (GLOBAL-ENV->STATIC (ENV->GLOBAL ENV))
                                     (STDLIB-prim-STATIC-ENV))
                  (< 0 N.VAL)
                  (EQUAL (VAL-KIND N) :V_INT)
                  (EQUAL (VAL-KIND X) :V_BITVECTOR)
                  (EQUAL X.LEN N.VAL)
                  (EQUAL (VAL-KIND P2) :V_INT)
                  (<= 0 P2.VAL)
                  (<= P2.VAL X.LEN)
                  (<= 1 (IFIX CLK)))
             (equal (eval-aligndownp2 ENV (LIST N) (LIST X P2))
                    (mv (EV_NORMAL
                         (FUNC_RESULT
                          (LIST (V_BITVECTOR
                                 N.VAL
                                 (BITOPS::LIMSHIFT-LOGHEAD-OF-LOGSQUASH N.VAL P2.VAL X.VAL)))
                          (ENV->GLOBAL ENV)))
                        orac))))
  :hints(("Goal" :in-theory (enable eval-aligndownp2))))


(fgl::add-fgl-rewrite aligndownp2-correct-fgl)


(fgl::def-fgl-rewrite eval_subprogram-of-aligndownp2
  (equal (eval_subprogram env "AlignDownP2" params args)
         (eval-aligndownp2 env params args))
  :hints(("Goal" :expand ((eval_subprogram env "AlignDownP2" params args))
          :in-theory (enable eval-aligndownp2))))


(define eval-aligndownp2-*t (env params args &key (clk 'clk) (orac 'orac) ((static-env static_env_global-p) 'static-env) ((tracespec tracespec-p) 'tracespec) ((pos posn-p) 'pos))
  :verify-guards nil
  (b* (((mv res orac traces) (eval_subprogram-*t env "AlignDownP2" params args)))
    (mv res orac traces))
  ///
  (fgl::remove-fgl-rewrite eval-aligndownp2-*t))

(encapsulate nil
  (local (defthm eval_subprogram-*t-decomp
           (b* ((call (eval_Subprogram-*t env fn params args)))
             (equal (list (mv-nth 0 call)
                          (mv-nth 1 call)
                          (mv-nth 2 call))
                    call))
           :hints (("goal" :expand ((eval_subprogram-*t env fn params args))
                    :in-theory (disable eval_subprogram-*t1-equals-original
                                        eval_subprogram-*t1-equals-original-kind
                                        ;; find-call-tracespec-when-tracespec-excludes-stdlib-fns
                                        ;; find-call-tracespec-when-tracespec-excludes-stdlib-fns-rw
                                        ;; open-eval_subprogram-*t1
                                        )))))

  (fgl::def-fgl-rewrite eval_subprogram-*t-of-aligndownp2
    (equal (eval_subprogram-*t env "AlignDownP2" params args)
           (eval-aligndownp2-*t env params args))
    :hints(("Goal" :in-theory (e/d (eval-aligndownp2-*t)
                                   (;; eval_subprogram-*t-decomp
                                    ;; aligndownp2-no-trace-rw
                                    ;; aligndownp2-no-trace-abort-rw
                                    ))
            ;; :use ((:instance eval_subprogram-*t-decomp
            ;;        (fn "AlignDownP2")))
            ))))

(fgl::def-fgl-rewrite aligndownp2-*t-correct-fgl
  (B* (((V_INT N))
       ((V_BITVECTOR X))
       ((V_INT P2)))
    (IMPLIES (AND (SUBPROGRAMS-MATCH '("AlignDownP2" "Zeros-1")
                                     static-env
                                     (STDLIB-prim-STATIC-ENV))
                  (not (find-call-tracespec "AlignDownP2" pos tracespec))
                  (trace-free-fnname-p "AlignDownP2")
                  (trace-free-ty-timeframe-imap-p (static_env_global->declared_types static-env))
                  (< 0 N.VAL)
                  (EQUAL (VAL-KIND N) :V_INT)
                  (EQUAL (VAL-KIND X) :V_BITVECTOR)
                  (EQUAL X.LEN N.VAL)
                  (EQUAL (VAL-KIND P2) :V_INT)
                  (fgl::fgl-validity-check (fgl::make-fgl-ipasir-config)
                                           (and (<= 0 P2.VAL)
                                                (<= P2.VAL X.LEN)))
                  ;; (<= 1 (IFIX CLK))
                  )
             (equal (eval-aligndownp2-*t ENV (LIST N) (LIST X P2))
                    (b* ((res
                          (fgl::fgl-hide (mv-nth 0 (eval-aligndownp2-*t ENV (LIST N) (LIST X P2))))))
                      (mv (eval_result-split
                           (or (<= 1 (nfix clk))
                               (eval_result-case res :ev_normal))
                           (FUNC_RESULT
                            (LIST (V_BITVECTOR
                                   N.VAL
                                   (BITOPS::LIMSHIFT-LOGHEAD-OF-LOGSQUASH N.VAL P2.VAL X.VAL)))
                            (global-replace-static static-env (ENV->GLOBAL ENV)))
                           nil nil nil nil ;; throwing
                           (termination-errmsg-fix (ev_error->desc res))
                           (ev_error->data res)
                           (ev_error->backtrace res))
                          orac
                          nil)))))
  :hints (("Goal"
           :in-theory (e/d (eval-aligndownp2-*t
                            eval_result-split
                            termination-error-p)
                           (aligndownp2-correct-prim
                            aligndownp2-correct))
           :use ((:instance aligndownp2-correct-prim
                            (env (env-replace-static static-env env)))))))


(fgl::def-fgl-rewrite check-aligndownp2-rewrites
  (implies (and (fgl::bind-fn-annotation annot 'eval-aligndownp2-*t-fn)
                (not annot))
           (equal (eval-aligndownp2-*t env params args)
                  (b* ((res (fgl::annotate '(:check-aligndownp2-rewrites) (eval-aligndownp2-*t env params args))))
                    (fgl::fgl-prog2
                     (and (fgl::syntax-bind res-is-call
                                            (fgl::fgl-object-case res
                                                                  :g-apply (eq res.fn 'eval-aligndownp2-*t-fn)
                                                                  :otherwise nil))
                          (fgl::fgl-error :msg (msg "eval-aligndownp2-*t call failed to rewrite -- see ~x0" 'aligndownp2-*t-correct-fgl)
                                          :debug-obj res))
                     res)))))




(define eval-ilog2-*t (env params args
                           &key
                           (clk 'clk)
                           (orac 'orac)
                           ((static-env static_env_global-p) 'static-env)
                           ((tracespec tracespec-p) 'tracespec)
                           ((pos posn-p) 'pos))
  :verify-guards nil
  (b* (((mv res orac traces)
        (eval_subprogram-*t env "ILog2" params args)))
    (mv res orac traces))
  ///
  (fgl::remove-fgl-rewrite eval-ilog2-*t))

(define eval-sqrtrounded-*t (env params args
                                 &key
                                 (clk 'clk)
                                 (orac 'orac)
                                 ((static-env static_env_global-p) 'static-env)
                                 ((tracespec tracespec-p) 'tracespec)
                                 ((pos posn-p) 'pos))
  :verify-guards nil
  (b* (((mv res orac traces)
        (eval_subprogram-*t env "SqrtRounded" params args)))
    (mv res orac traces))
  ///
  (fgl::remove-fgl-rewrite eval-sqrtrounded-*t))

(encapsulate
  nil

  (local (defthm eval_subprogram-*t-decomp
           (b* ((call (eval_Subprogram-*t env fn params args)))
             (equal (list (mv-nth 0 call)
                          (mv-nth 1 call)
                          (mv-nth 2 call))
                    call))
           :hints (("Goal" :expand ((eval_subprogram-*t env fn params args))
                           :in-theory (disable eval_subprogram-*t1-equals-original
                                               eval_subprogram-*t1-equals-original-kind
                                               ;; find-call-tracespec-when-tracespec-excludes-stdlib-fns
                                               ;; find-call-tracespec-when-tracespec-excludes-stdlib-fns-rw
                                               ;; open-eval_subprogram-*t1
                                               )))))
  (local (in-theory (enable eval_subprogram-*t-decomp)))

  (fgl::def-fgl-rewrite eval_subprogram-*t-of-ilog2
    (equal (eval_subprogram-*t env "ILog2" params args)
           (eval-ilog2-*t env params args))
    :hints(("Goal" :in-theory (e/d (eval-ilog2-*t)
                                   (;; eval_subprogram-*t-decomp
                                    ;; ilog2-no-trace-rw
                                    ;; ilog2-no-trace-abort-rw
                                    ))
                   ;; :use ((:instance eval_subprogram-*t-decomp
                   ;;        (fn "ILog2")))
                   )))

  (fgl::def-fgl-rewrite eval_subprogram-*t-of-sqrtrounded
    (equal (eval_subprogram-*t env "SqrtRounded" params args)
           (eval-sqrtrounded-*t env params args))
    :hints(("Goal" :in-theory (enable eval-sqrtrounded-*t)))))

(fgl::def-fgl-rewrite ilog2-*t-correct-fgl
  (B* (((V_real val)))
    (IMPLIES (AND (SUBPROGRAMS-MATCH '("ILog2" "Abs")
                                     static-env
                                     (STDLIB-prim-STATIC-ENV))
                  (not (find-call-tracespec "ILog2" pos tracespec))
                  (trace-free-fnname-p "ILog2")
                  (trace-free-ty-timeframe-imap-p (static_env_global->declared_types static-env))
                  (equal (val-kind val) :v_real)
                  (equal zerop (and (equal 0 val.val) t))
                  (syntaxp (progn$ (cw "zerop: ~x0~%" zerop)
                                   t))
                  (not zerop))
             (equal (eval-ilog2-*t ENV nil (list val))
                    (b* ((res
                          (fgl::fgl-hide (mv-nth 0 (eval-ilog2-*t ENV nil (list val))))))
                      (mv (eval_result-case res
                            :ev_error (termination-error-fix res)
                            :otherwise (EV_NORMAL
                                        (FUNC_RESULT
                                         (LIST (V_INT (ACL2::RATIONAL-EXPONENT VAL.VAL)))
                                         (GLOBAL-REPLACE-STATIC STATIC-ENV (ENV->GLOBAL ENV)))))
                          orac
                          nil)))))
  :hints (("Goal"
           :in-theory (e/d (eval-ilog2-*t)
                           (ilog2-correct-prim
                            ilog2-correct))
           :use ((:instance ilog2-correct-prim
                            (env (env-replace-static static-env env)))))))

(fgl::remove-fgl-rewrite acl2::rational-exponent)

(fgl::def-fgl-rewrite check-ilog2-rewrites
  (implies (and (fgl::bind-fn-annotation annot 'eval-ilog2-*t-fn)
                (not annot))
           (equal (eval-ilog2-*t env params args)
                  (b* ((res (fgl::annotate '(:check-ilog2-rewrites) (eval-ilog2-*t env params args))))
                    (fgl::fgl-prog2
                     (and (fgl::syntax-bind res-is-call
                                            (fgl::fgl-object-case res
                                              :g-apply (eq res.fn 'eval-ilog2-*t-fn)
                                              :otherwise nil))
                          (fgl::fgl-error :msg (msg "eval-ilog2-*t call failed to rewrite")
                                          :debug-obj res))
                     res)))))

(fgl::def-fgl-rewrite sqrtrounded-*t-correct-fgl
  (B* (((v_real val))
       ((v_int fracbits)))
    (IMPLIES (AND (SUBPROGRAMS-MATCH '("SqrtRounded" "Abs" "ILog2")
                                     static-env
                                     (STDLIB-prim-STATIC-ENV))
                  (not (find-call-tracespec "SqrtRounded" pos tracespec))
                  (trace-free-fnname-p "SqrtRounded")
                  (trace-free-ty-timeframe-imap-p
                   (static_env_global->declared_types static-env))
                  (EQUAL (val-kind val) :v_real)
                  (equal (val-kind fracbits) :v_int)
                  (equal negp (and (<= val.val 0) t))
                  (< 0 fracbits.val)
                  (syntaxp (progn$ (cw "negp: ~x0~%" negp)
                                   t))
                  (not negp))
             (equal (eval-sqrtrounded-*t ENV nil (list val fracbits))
                    (b* (((mv res ?new-orac ?trace)
                          (fgl::fgl-hide
                           (eval-sqrtrounded-*t ENV nil (list val fracbits)))))
                      (mv (eval_result-case res
                            :ev_error (termination-error-fix res)
                            :otherwise (EV_NORMAL
                                        (FUNC_RESULT
                                         (LIST (V_REAL (ACL2::SQRTROUNDED VAL.VAL FRACBITS.VAL)))
                                         (GLOBAL-REPLACE-STATIC STATIC-ENV (ENV->GLOBAL ENV)))))
                          orac
                          nil)))))
  :hints (("Goal"
           :in-theory (e/d (eval-sqrtrounded-*t)
                           (sqrtrounded-correct-prim
                            sqrtrounded-correct))
           :use ((:instance sqrtrounded-correct-prim
                            (env (env-replace-static static-env env)))))))

(fgl::remove-fgl-rewrite acl2::sqrtrounded)

(fgl::def-fgl-rewrite check-sqrtrounded-rewrites
  (implies
   (and (fgl::bind-fn-annotation annot 'eval-sqrtrounded-*t-fn)
        (not annot))
   (equal (eval-sqrtrounded-*t env params args)
          (b* ((res (fgl::annotate '(:check-sqrtrounded-rewrites)
                                   (eval-sqrtrounded-*t env params args))))
            (fgl::fgl-prog2
             (and (fgl::syntax-bind res-is-call
                                    (fgl::fgl-object-case res
                                                          :g-apply (eq res.fn 'eval-sqrtrounded-*t-fn)
                                                          :otherwise nil))
                  (fgl::fgl-error :msg (msg "eval-sqrtrounded-*t call failed to rewrite")
                                  :debug-obj res))
             res)))))


;; ####################### Divide by power of 2 ############################
(encapsulate nil
  (local (include-book "centaur/bitops/ihsext-basics" :dir :system))
  (local (include-book "ihs/quotient-remainder-lemmas" :dir :system))
  (local (include-book "arithmetic/top" :dir :system))
  (fgl::def-fgl-rewrite multiply-by-frac-power-of-2
    (implies (and (rationalp frac)
                  (not (equal 0 frac))
                  (equal exp (acl2::rational-exponent frac))
                  (< exp 0)
                  (equal frac (expt 2 exp))
                  (integerp x)
                  (equal inv (/ frac))
                  (equal (mod x inv) 0))
             (equal (* x (fgl::concrete frac))
                    (floor x inv)))))





;; ####################### INTEGER-IF hack ############################
;; NOTE: This is a hack to get around a merge of an arbitary integer with 0.
;; It gets us a little further in the symbolic simulation. We might need more
;; rules like (and worse than) equal-of-integer-if to go further. It's
;; unfortunate that the prototype ever calls for an arbitrary, unbounded
;; integer.

(define integer-if (test then else)
  :returns (ite integerp :rule-classes :type-prescription)
  (if test
      (ifix then)
    (ifix else))
  ///
  (fgl::remove-fgl-rewrite integer-if)
  (fgl::add-fgl-rewrite integerp-of-integer-if)
  (fgl::def-fgl-rewrite equal-of-integer-if
    (equal (equal x (integer-if test then else))
           (if test (equal x (ifix then))
             (equal x (ifix else)))))

  (fgl::def-fgl-rewrite minus-of-integer-if
    (equal (- (integer-if test then else))
           (integer-if test (- (ifix then)) (- (ifix else)))))

  (fgl::def-fgl-rewrite lognot-of-integer-if
    (equal (lognot (integer-if test then else))
           (b* ((then (lognot then))
                (else  (lognot else))
                ((when (fgl::syntax-bind both-intcdrs
                                         (and (fgl::fgl-object-case then
                                                :g-apply (eq then.fn 'lognot)
                                                :otherwise nil)
                                              (fgl::fgl-object-case else
                                                :g-apply (eq else.fn 'lognot)
                                                :otherwise nil))))
                 (integer-if test then else)))
             (if test then else))))

  (fgl::def-fgl-rewrite intcar-of-integer-if
    (equal (fgl::intcar (integer-if test then else))
           (if test (and (fgl::intcar then) t) (and (fgl::intcar else) t))))

  (fgl::def-fgl-rewrite intcdr-of-integer-if
    (equal (fgl::intcdr (integer-if test then else))
           (b* ((then (fgl::intcdr then))
                (else  (fgl::intcdr else))
                ((when (fgl::syntax-bind both-intcdrs
                                         (and (fgl::fgl-object-case then
                                                :g-apply (eq then.fn 'fgl::intcdr)
                                                :otherwise nil)
                                              (fgl::fgl-object-case else
                                                :g-apply (eq else.fn 'fgl::intcdr)
                                                :otherwise nil))))
                 (integer-if test then else)))
             (if test then else)))))

;; We normalize INTEGER-IF calls to merge "nice" (g-integer/concrete) objects
;; in the THEN branch and non-nice objects in the ELSE branch. So when
;; operating on an integer-if, try checking whether test is always true and if
;; so, just operate on the then branch.
(fgl::def-fgl-rewrite logapp-of-integer-if-1
  (implies (fgl::fgl-validity-check (fgl::make-fgl-ipasir-config) test)
           (equal (logapp (integer-if test then else) x y)
                  (logapp then x y)))
  :hints(("Goal" :in-theory (enable integer-if))))

(fgl::def-fgl-rewrite logapp-of-integer-if-2
  (implies (fgl::fgl-validity-check (fgl::make-fgl-ipasir-config) test)
           (equal (logapp w (integer-if test then else) y)
                  (logapp w then y)))
  :hints(("Goal" :in-theory (enable integer-if))))

(fgl::def-fgl-rewrite logapp-of-integer-if-3
  (implies (fgl::fgl-validity-check (fgl::make-fgl-ipasir-config) test)
           (equal (logapp w x (integer-if test then else))
                  (logapp w x then)))
  :hints(("Goal" :in-theory (enable integer-if))))


(fgl::def-fgl-rewrite logtail-of-integer-if-1
  (implies (fgl::fgl-validity-check (fgl::make-fgl-ipasir-config) test)
           (equal (logtail (integer-if test then else) x)
                  (logtail then x)))
  :hints(("Goal" :in-theory (enable integer-if))))

(fgl::def-fgl-rewrite logtail-of-integer-if-2
  (implies (fgl::fgl-validity-check (fgl::make-fgl-ipasir-config) test)
           (equal (logtail w (integer-if test then else))
                  (logtail w then)))
  :hints(("Goal" :in-theory (enable integer-if))))

(fgl::def-fgl-rewrite plus-of-integer-if-1
  (implies (fgl::fgl-validity-check (fgl::make-fgl-ipasir-config) test)
           (equal (+ (integer-if test then else) y)
                  (+ (ifix then) y)))
  :hints(("Goal" :in-theory (enable integer-if))))

(fgl::def-fgl-rewrite plus-of-integer-if-2
  (implies (fgl::fgl-validity-check (fgl::make-fgl-ipasir-config) test)
           (equal (+ y (integer-if test then else))
                  (+ y (ifix then))))
  :hints(("Goal" :in-theory (enable integer-if))))

(fgl::def-fgl-rewrite times-of-integer-if-1
  (implies (fgl::fgl-validity-check (fgl::make-fgl-ipasir-config) test)
           (equal (* (integer-if test then else) y)
                  (* (ifix then) y)))
  :hints(("Goal" :in-theory (enable integer-if))))

(fgl::def-fgl-rewrite times-of-integer-if-2
  (implies (fgl::fgl-validity-check (fgl::make-fgl-ipasir-config) test)
           (equal (* y (integer-if test then else))
                  (* y (ifix then))))
  :hints(("Goal" :in-theory (enable integer-if))))


(fgl::def-fgl-rewrite integer-if-on-last-chance
  (implies (and (fgl::syntax-bind last-chancep
                                  (fgl::interp-flags->if-merge-last-chance (fgl::interp-st->flags fgl::interp-st)))
                (integerp then) (integerp else))
           (equal (if test then else)
                  (integer-if test then else)))
  :hints(("Goal" :in-theory (enable integer-if))))

(fgl::def-fgl-rewrite intcar-of-minus
  (equal (fgl::intcar (- x))
         (fgl::intcar x)))

(fgl::def-fgl-rewrite intcdr-of-minus
  (implies (integerp x)
           (equal (fgl::intcdr (- x))
                  (+ (b-not (logcar x)) (lognot (fgl::intcdr x)))))
  :hints(("Goal" :in-theory (enable bitops::minus-to-lognot))))

(fgl::def-fgl-rewrite intcar-of-lognot
  (equal (fgl::intcar (lognot x))
         (not (fgl::intcar x))))

(fgl::def-fgl-rewrite intcdr-of-lognot
  (equal (fgl::intcdr (lognot x))
         (lognot (fgl::intcdr x))))

(fgl::def-fgl-rewrite intcar-of-plus
  (implies (and (integerp x) (integerp y))
           (equal (fgl::intcar (+ x y))
                  (xor (fgl::intcar x) (fgl::intcar y)))))

(fgl::def-fgl-rewrite intcdr-of-plus
  (implies (and (integerp x) (integerp y))
           (equal (fgl::intcdr (+ x y))
                  (+ (b-and (logcar x) (logcar y))
                     (fgl::intcdr x) (fgl::intcdr y) ))))

(fgl::def-fgl-rewrite integerp-of-plus
  (implies (and (integerp x) (integerp y))
           (integerp (+ x y))))

(fgl::def-fgl-rewrite integerp-of-minus
  (implies (integerp x)
           (integerp (- x))))

(fgl::def-fgl-rewrite plus-of-0
  (implies (integerp x)
           (and (equal (+ 0 x) x)
                (equal (+ x 0) x))))

(fgl::def-fgl-rewrite plus-reassoc
  (implies (syntaxp (and (fgl::fgl-object-case x '(:g-integer :g-concrete))
                         (fgl::fgl-object-case y '(:g-integer :g-concrete))))
           (equal (+ x y z)
                  (+ (+ x y) z))))

(fgl::def-fgl-rewrite plus-assoc
  (equal (+ (+ x y) z)
         (+ x y z)))

(fgl::def-fgl-rewrite plus-order
  (implies (syntaxp (and (fgl::fgl-object-case x '(:g-integer :g-concrete))
                         (not (fgl::fgl-object-case y '(:g-integer :g-concrete)))))
           (equal (+ y x) (+ x y))))

(fgl::def-fgl-rewrite times-of-0
  (and (equal (* 0 x) 0)
       (equal (* x 0) 0)))

(fgl::def-fgl-rewrite times-of-1
  (implies (integerp x)
           (and (equal (* 1 x) x)
                (equal (* x 1) x))))

(fgl::def-fgl-rewrite times-reassoc
  (implies (syntaxp (and (fgl::fgl-object-case x '(:g-integer :g-concrete))
                         (fgl::fgl-object-case y '(:g-integer :g-concrete))))
           (equal (* x y z)
                  (* (* x y) z))))


(local (include-book "arithmetic/top" :dir :system))
(fgl::def-fgl-rewrite minus-of-times
  (implies (syntaxp (fgl::fgl-object-case x '(:g-integer :g-concrete)))
           (equal (- (* x y))
                  (* (- x) y))))

(fgl::def-fgl-rewrite times-of-minus
  (implies (syntaxp (fgl::fgl-object-case x '(:g-integer :g-concrete)))
           (equal (* x (- y))
                  (* (- x) y))))

(fgl::def-fgl-rewrite times-assoc
  (equal (* (* x y) z)
         (* x y z)))

(fgl::def-fgl-rewrite times-order
  (implies (syntaxp (and (fgl::fgl-object-case x '(:g-integer :g-concrete))
                         (not (fgl::fgl-object-case y '(:g-integer :g-concrete)))))
           (equal (* y x) (* x y))))

(fgl::def-fgl-rewrite integerp-of-times
  (implies (and (integerp x) (integerp y))
           (integerp (* x y))))

(fgl::def-fgl-rewrite integerp-of-logapp
  (integerp (logapp x y z)))

(fgl::def-fgl-brewrite check-integerp-when-integerp
  (implies (and (integerp x)
                (equal ans t))
           (equal (fgl::check-integerp ans x) ans))
  :hints(("Goal" :in-theory (enable fgl::check-integerp))))



(fgl::def-fgl-branch-merge merge-v_int->val-and-other-integer
  (implies (integerp y)
           (equal (if test (v_int->val x) y)
                  (integer-if test (fgl::fgl-hide (v_int->val x)) y)))
  :hints(("Goal" :in-theory (enable integer-if))))


(fgl::def-fgl-rewrite intcdr-of-logtail
  (equal (fgl::intcdr (logtail n x))
         (logtail n (fgl::intcdr x)))
  :hints(("Goal" :in-theory (enable bitops::logtail**))))


(fgl::def-fgl-rewrite integer-if-reorder
  (implies (syntaxp (and (fgl::fgl-object-case else '(:g-integer :g-concrete))
                         (not (fgl::fgl-object-case then '(:g-integer :g-concrete)))))
           (equal (integer-if test then else)
                  (integer-if (not test) else then)))
  :hints(("Goal" :in-theory (enable integer-if))))

(fgl::def-fgl-rewrite integer-if-reassoc1
  (implies (syntaxp (and (fgl::fgl-object-case then1 '(:g-integer :g-concrete))))
           (equal (integer-if test (integer-if test1 then1 else1) else)
                  (integer-if (and test test1) then1 (integer-if test else1 else))))
  :hints(("Goal" :in-theory (enable integer-if))))

(fgl::def-fgl-rewrite integer-if-reassoc2
  (implies (syntaxp (and (fgl::fgl-object-case then2 '(:g-integer :g-concrete))
                         (not (fgl::fgl-object-case then '(:g-integer :g-concrete)))))
           (equal (integer-if test then (integer-if test2 then2 else2))
                  (integer-if (and (not test) test2) then2 (integer-if test then else2))))
  :hints(("Goal" :in-theory (enable integer-if))))


(fgl::def-fgl-branch-merge merge-integer-if-with-other-integer
  (implies (integerp y)
           (equal (if test (integer-if test2 then else) y)
                  (integer-if test (fgl::fgl-hide (integer-if test2 then else)) y)))
  :hints(("Goal" :in-theory (enable integer-if))))

(fgl::def-fgl-branch-merge merge-integer-if-0-with-0
  (equal (if test (integer-if test2 val 0) 0)
         (integer-if (and test test2) val 0))
  :hints(("Goal" :in-theory (enable integer-if))))

(fgl::def-fgl-branch-merge merge-integer-if
  (implies (integerp else)
           (equal (if test (integer-if test2 then1 else1) else)
                  (integer-if test (integer-if test2 then1 else1) else)))
  :hints(("Goal" :in-theory (enable integer-if))))

(fgl::def-fgl-rewrite integer-if-of-const-tests
  (and (equal (integer-if nil then else) (ifix else))
       (equal (integer-if t then else) (ifix then)))
  :hints(("Goal" :in-theory (enable integer-if))))


(fgl::def-fgl-rewrite minus-to-+carry-restricted
  (implies (and (syntaxp (fgl::fgl-object-case x :g-integer))
                (integerp x))
           (equal (- x)
                  (fgl::+carry t 0 (lognot x))))
  :hints(("Goal" :in-theory (enable fgl::+carry lognot))))
(fgl::remove-fgl-rewrite fgl::minus-to-+carry)


(fgl::def-fgl-rewrite fgl-*-restricted
  (implies (and (syntaxp (and (fgl::fgl-object-case x :g-integer t :g-concrete t :otherwise nil)
                              (fgl::fgl-object-case y :g-integer t :g-concrete t :otherwise nil)))
                (integerp x)
                (integerp y))
           (EQUAL (* X Y)
                  (IF (FGL::CHECK-INT-ENDP! FGL::X-ENDP X)
                      (IF (FGL::INTCAR X) (- Y) 0)
                      (+ (IF (FGL::INTCAR X) Y 0)
                         (FGL::INTCONS NIL (* (FGL::INTCDR X) Y))))))
  :hints(("Goal" :use ((:instance fgl::fgl-* (x x) (y y))))))



(fgl::def-fgl-rewrite intcdr-of-logapp
  #!fgl
  (equal (intcdr (logapp w x y))
         (if (zp w)
             (intcdr y)
           (logapp (1- w) (intcdr x) y))))

(fgl::def-fgl-rewrite intcar-of-logapp
  (equal (fgl::intcar (logapp w x y))
         (if (zp w)
             (fgl::intcar y)
           (fgl::intcar x))))


(fgl::def-fgl-rewrite distrib-consts
  (implies (syntaxp (and (fgl::fgl-object-case x :g-integer t :g-concrete t :otherwise nil)
                         (fgl::fgl-object-case y :g-integer t :g-concrete t :otherwise nil)))
           (equal (* x (+ y z))
                  (+ (* x y) (* x z)))))

(fgl::def-fgl-rewrite *-const-integer-if
  (implies (and (syntaxp (fgl::fgl-object-case x :g-integer t :g-concrete t :otherwise nil))
                (integerp x) (integerp then) (integerp else))
           (equal (* x (integer-if test then else))
                  (integer-if test (* x then) (* x else))))
  :hints(("Goal" :in-theory (enable integer-if))))

(fgl::def-fgl-rewrite +-const-integer-if
  (implies (and (syntaxp (fgl::fgl-object-case x :g-integer t :g-concrete t :otherwise nil))
                (integerp x) (integerp then) (integerp else))
           (equal (+ x (integer-if test then else))
                  (integer-if test (+ x then) (+ x else))))
  :hints(("Goal" :in-theory (enable integer-if))))

;; (fgl::remove-fgl-branch-merge intcons-merge)



(fgl::def-fgl-rewrite integer-if-same-1
  (equal (integer-if test then (integer-if test2 then else2))
         (integer-if (or test test2) then else2))
  :hints(("Goal" :in-theory (enable integer-if))))

(fgl::def-fgl-rewrite integer-if-same-2
  (equal (integer-if test then (integer-if test2 then2 then))
         (integer-if (or test (not test2)) then then2))
  :hints(("Goal" :in-theory (enable integer-if))))

(fgl::def-fgl-rewrite integer-if-same-3
  (equal (integer-if test (integer-if test1 then1 else1) then1)
         (integer-if (or (not test) test1) then1 else1))
  :hints(("Goal" :in-theory (enable integer-if))))

(fgl::def-fgl-rewrite integer-if-same-4
  (equal (integer-if test (integer-if test1 then1 else1) else1)
         (integer-if (and test test1) then1 else1))
  :hints(("Goal" :in-theory (enable integer-if))))

(fgl::def-fgl-rewrite integer-if-same-5
  (equal (integer-if test (integer-if test1 then1 else1)
                     (integer-if test2 then1 else2))
         (integer-if (if test test1 test2) then1 (integer-if test else1 else2)))
  :hints(("Goal" :in-theory (enable integer-if))))

(fgl::def-fgl-rewrite integer-if-same-6
  (equal (integer-if test (integer-if test1 then1 else1)
                     (integer-if test2 then2 then1))
         (integer-if (if test test1 (not test2)) then1 (integer-if test else1 then2)))
  :hints(("Goal" :in-theory (enable integer-if))))

(fgl::def-fgl-rewrite integer-if-same-7
  (equal (integer-if test (integer-if test1 then1 else1)
                     (integer-if test2 else1 else2))
         (integer-if (if test (not test1) test2) else1 (integer-if test then1 else2)))
  :hints(("Goal" :in-theory (enable integer-if))))

(fgl::def-fgl-rewrite integer-if-same-8
  (equal (integer-if test (integer-if test1 then1 else1)
                     (integer-if test2 then2 else1))
         (integer-if (if test (not test1) (not test2)) else1 (integer-if test then1 then2)))
  :hints(("Goal" :in-theory (enable integer-if))))


(fgl::def-fgl-rewrite integerp-of-logtail
  (integerp (logtail n x)))

(fgl::def-fgl-rewrite intcar-of-logtail
  (equal (fgl::intcar (logtail n x))
         (logbitp n x)))


(define my-nth ((n natp) (x true-listp))
  :returns (nth (equal nth (nth n x)))
  (if (endp x)
      nil
    (if (zp n)
        (car x)
      (my-nth (1- n) (cdr x)))))

(table fgl::magitastic-ev-definitions
       'nth '((n x) (my-nth n x)))

(table fgl::magitastic-ev-definitions
       'v_array-nth '((n x) (VAL-FIX$INLINE (MY-NTH N (V_ARRAY->ARR$INLINE X)))))



(fgl::remove-fgl-rewrite fgl::fgl-*)

;; ####################### IsAligned check ############################
;; This forces a SAT check on the result of IsAligned. In many cases we
;; want to assume is true, but even when we do it's hard for FGL to determine
;; that it is without a SAT check. This forces the SAT check and errors out with
;; a counterexample if it's not proved.

(encapsulate nil


  (local (defthmd cons-of-equal
           (implies (and (consp x)
                         (equal a (car x)))
                    (equal (cons a (cdr x)) x))))


  (local (defthmd v_bool->val-rewrite
           (implies (and (equal (val-kind x) :v_bool)
                         (val-p x))
                    (iff (v_bool->val x)
                         (equal x (v_bool t))))
           :hints(("Goal" :in-theory (e/d (v_bool->val)
                                          ((v_bool)))))))

  (local (defthm eval_subprogram-mvs
           (equal (list (mv-nth 0 (eval_subprogram env fn params args))
                        (mv-nth 1 (eval_subprogram env fn params args)))
                  (eval_subprogram env fn params args))
           :hints (("goal" :expand ((eval_subprogram env fn params args))))))

  (fgl::def-fgl-rewrite eval_subprogram-of-isaligned
    (implies (and (fgl::bind-fn-annotation annot 'eval_subprogram-fn)
                  (not (assoc-keyword :eval_subprogram-of-isaligned annot)))
             (equal (eval_subprogram env "IsAligned" params args)
                    (b* (((mv res orac) (fgl::annotate `(:eval_subprogram-of-isaligned t . ,annot)
                                                       (eval_subprogram env "IsAligned" params args)))
                         (res
                          (eval_result-case res
                            :ev_normal (b* (((func_result res.res)))
                                         (if (consp res.res.vals)
                                             (b* ((resval (car res.res.vals))
                                                  (config (fgl::make-fgl-ipasir-config)))
                                               (val-case resval
                                                 :v_bool (cond ((fgl::check-true validp (fgl::fgl-validity-check config resval.val))
                                                                (fgl::fgl-prog2 (cw "isaligned: valid~%")
                                                                                (ev_normal (func_result (cons (v_bool t) (cdr res.res.vals))
                                                                                                        res.res.env))))
                                                               (t (fgl::fgl-prog2 (counterexample-with-backtrace config "IsAligned counterexample")
                                                                                  res)))
                                                 :otherwise res))
                                           res))
                            :otherwise res)))
                      (mv res orac))))
    :hints((And stable-under-simplificationp
                '(:in-theory (enable v_bool->val-rewrite)))
           (And stable-under-simplificationp
                '(:in-theory (enable cons-of-equal)))))

  


  (local (defthmd v_bool->val-rewrite2
           (implies (and (equal (val-kind x) :v_bool)
                         (val-p x))
                    (iff (v_bool->val x)
                         (not (equal x (v_bool nil)))))
           :hints(("Goal" :in-theory (e/d (v_bool->val)
                                          ((v_bool)))))))

  (fgl::def-fgl-rewrite eval_subprogram-of-s1enabled
    (implies (and (fgl::bind-fn-annotation annot 'eval_subprogram-fn)
                  (not (assoc-keyword :eval_subprogram-of-s1enabled annot)))
             (equal (eval_subprogram env "AArch64_S1Enabled" params args)
                    (b* (((mv res orac) (fgl::annotate `(:eval_subprogram-of-s1enabled t . ,annot)
                                                       (eval_subprogram env "AArch64_S1Enabled" params args)))
                         (res
                          (eval_result-case res
                            :ev_normal (b* (((func_result res.res)))
                                         (if (consp res.res.vals)
                                             (b* ((resval (car res.res.vals))
                                                  (config (fgl::make-fgl-ipasir-config)))
                                               (val-case resval
                                                 :v_bool (cond ((fgl::check-true unsatp (fgl::fgl-validity-check config (not resval.val)))
                                                                (fgl::fgl-prog2 (cw "s1enabled: unsat~%")
                                                                                (ev_normal (func_result (cons (v_bool nil) (cdr res.res.vals))
                                                                                                        res.res.env))))
                                                               (t (fgl::fgl-prog2 (counterexample-with-backtrace config "S1Enabled counterexample")
                                                                                  res)))
                                                 :otherwise res))
                                           res))
                            :otherwise res)))
                      (mv res orac))))
    :hints((And stable-under-simplificationp
                '(:in-theory (enable v_bool->val-rewrite2)))
           (And stable-under-simplificationp
                '(:in-theory (enable cons-of-equal))))))




  ;; ;; Make sure eval_subprogram-print and save-oracle-on-outermost-eval_subprogram
  ;; ;; are the first rules to be tried for eval_subprogram.
  ;; (fgl::remove-fgl-rewrite eval_subprogram-print)
  ;; (fgl::add-fgl-rewrite eval_subprogram-print)
  ;; (fgl::remove-fgl-rewrite save-oracle-on-outermost-eval_subprogram)
  ;; (fgl::add-fgl-rewrite save-oracle-on-outermost-eval_subprogram)



;; ####################### Counterexample-on-pass-error ############################

;; NOTE: This rule causes symbolic simulation to stop at the first satisfiable
;; error condition.  It can speed up finding error cases considerably because
;; you don't have to symbolically simulate the whole function, just up until
;; you find the error case. If you don't want this (and it could be expensive,
;; especially if there are no errors and it's cheaper to just check that at the
;; end with monolithic sat), turn this rule off with
;; (fgl::remove-fgl-rewrite counterexample-on-pass-error).

(define replace-envs-with-local-storage-in-backtrace ((bt fgl::backtrace-p))
  :verify-guards nil
  (if (atom bt)
      nil
    (cons (b* (((fgl::backtrace-frame fr) (car bt))
               (look (hons-assoc-equal 'env fr.objs))
               ((unless look) fr))
            (fgl::change-backtrace-frame fr :objs (put-assoc-equal 'env
                                                                   (list (local-env->storage (env->local (cdr look)))
                                                                         (global-env->storage (env->global (cdr look))))
                                                                   fr.objs)))
          (replace-envs-with-local-storage-in-backtrace (cdr bt)))))
               


(fgl::def-fgl-rewrite counterexample-on-pass-error
  (implies (and (equal sat-config (fgl::make-fgl-ipasir-config))
                (fgl::fgl-sat-check sat-config t)) ;; checks path condition can be satisfied
           (equal (pass-error error orac)
                  (b* (((unless (fgl::syntax-bind
                                 ctrex-enabledp
                                 (fgl::fgl-config->counterexample-analysis-enabledp
                                  (fgl::interp-st->config 'interp-st))))
                        (fgl::fgl-prog2
                         (fgl::fgl-error
                          :msg "Counterexample-on-pass-error encountered (but not analyzing counterexamples)")
                         (fgl::abort-rewrite (pass-error error orac))))
                       (backtrace
                        (fgl::syntax-bind
                         bt
                         (b* ((backtrace (fgl::interp-st-stack-backtrace (list (fgl::backtrace-spec
                                                                                '(:FORMULA EVAL_SUBPROGRAM-FN)
                                                                                '(name vparams vargs))
                                                                               (fgl::backtrace-spec
                                                                                '(:formula eval_call-fn)
                                                                                '(pos env)))
                                                                         'interp-st))
                              (stmt-specs (fgl::function-rules-to-backtrace-spec 'eval_stmt-fn
                                                                                 '(env s) 'interp-st (w 'state)))
                              (stmt-frame (fgl::interp-st-stack-backtrace-top stmt-specs 'interp-st)))
                           (fgl::backtrace-to-obj (if stmt-frame
                                                      (cons stmt-frame backtrace)
                                                    backtrace)))))
                       (?ignore (and (oracle-marker orac) t))
                       (?ignore2 (fgl::fgl-sat-check sat-config t)))
                    (fgl::fgl-prog2
                     (fgl::fgl-prog2
                      (fgl::syntax-interp
                       (b* ((- (fgl::interp-st-sat-counterexample  (fgl::g-concrete->val sat-config) 'interp-st 'state))
                            ((mv bindings ?var-vals &) (fgl::interp-st-counterex-bindings/print-errors `((backtrace . ,backtrace)) 'interp-st 'state))
                            (- ;; (cw "Counterexample: ~x0~%" var-vals)
                             (cw "Backtrace (from FGL stack): ~x0~%" (replace-envs-with-local-storage-in-backtrace
                                                                      (cdr (hons-assoc-equal 'backtrace bindings))))))
                         (fgl::interp-st-run-ctrex (fgl::g-concrete->val sat-config) 'interp-st 'state)))
                      (fgl::fgl-error :msg "Found pass-error"))
                     (fgl::abort-rewrite (pass-error error orac)))))))

(fgl::def-fgl-rewrite counterexample-on-pass-error-*t
  (implies (and (not (equal (ev_error->desc error) "Trace abort"))
                (equal sat-config (fgl::make-fgl-ipasir-config))
                (fgl::fgl-sat-check sat-config t)) ;; checks path condition can be satisfied
           (equal (pass-error-*t error orac trace)
                  (b* (((unless (fgl::syntax-bind
                                 ctrex-enabledp
                                 (fgl::fgl-config->counterexample-analysis-enabledp
                                  (fgl::interp-st->config 'interp-st))))
                        (fgl::fgl-prog2
                         (fgl::fgl-error
                          :msg "Counterexample-on-pass-error-*t encountered (but not analyzing counterexamples)")
                         (fgl::abort-rewrite (pass-error-*t error orac))))
                       (backtrace
                        (fgl::syntax-bind
                         bt
                         (b* ((backtrace (fgl::interp-st-stack-backtrace (list (fgl::backtrace-spec
                                                                                '(:FORMULA EVAL_SUBPROGRAM-*T-FN)
                                                                                '(name vparams vargs))
                                                                               (fgl::backtrace-spec
                                                                                '(:formula eval_call-*T-fn)
                                                                                '(pos env)))
                                                                         'interp-st))
                              (stmt-specs (fgl::function-rules-to-backtrace-spec 'eval_stmt-*t-fn
                                                                                 '(env s) 'interp-st (w 'state)))
                              (stmt-frame (fgl::interp-st-stack-backtrace-top stmt-specs 'interp-st)))
                           (fgl::backtrace-to-obj (if stmt-frame
                                                      (cons stmt-frame backtrace)
                                                    backtrace)))))
                       (?ignore (and (oracle-marker orac) t))
                       (?ignore2 (fgl::fgl-sat-check sat-config t)))
                    (fgl::fgl-prog2
                     (fgl::fgl-prog2
                      (fgl::syntax-interp
                       (b* ((- (fgl::interp-st-sat-counterexample  (fgl::g-concrete->val sat-config) 'interp-st 'state))
                            ((mv bindings ?var-vals &) (fgl::interp-st-counterex-bindings/print-errors `((backtrace . ,backtrace)) 'interp-st 'state))
                            (- ;; (cw "Counterexample: ~x0~%" var-vals)
                             (cw "Backtrace (from FGL stack): ~x0~%" (replace-envs-with-local-storage-in-backtrace
                                                                      (cdr (hons-assoc-equal 'backtrace bindings))))))
                         (fgl::interp-st-run-ctrex (fgl::g-concrete->val sat-config) 'interp-st 'state)))
                      (fgl::fgl-error :msg "Found pass-error-*t"))
                     (fgl::abort-rewrite (pass-error-*t error orac)))))))

(fgl::remove-fgl-rewrite counterexample-on-pass-error-*t)






(fgl::def-fgl-rewrite abort-check-reachable
  (implies (and (fgl::bind-fn-annotation annot 'eval_subprogram-*t-fn)
                (printed-annotation-index annot)
                (fgl::fgl-sat-check (fgl::make-fgl-ipasir-config) t))
           (equal (eval_subprogram-*t env "AArch64_Abort" params args)
                  (fgl::abort-rewrite (eval_subprogram-*t env "AArch64_Abort" params args)))))





;; ####################### IsAligned check ############################
;; This forces a SAT check on the result of IsAligned. In many cases we
;; want to assume is true, but even when we do it's hard for FGL to determine
;; that it is without a SAT check. This forces the SAT check and errors out with
;; a counterexample if it's not proved.

(encapsulate nil
  (local (defthmd cons-of-equal
           (implies (and (consp x)
                         (equal a (car x)))
                    (equal (cons a (cdr x)) x))))


  (local (defthmd v_bool->val-rewrite
           (implies (and (equal (val-kind x) :v_bool)
                         (val-p x))
                    (iff (v_bool->val x)
                         (equal x (v_bool t))))
           :hints(("Goal" :in-theory (e/d (v_bool->val)
                                          ((v_bool)))))))


  (local (in-theory (disable eval_subprogram-*t-equals-original)))
  
  (local (defthm eval_subprogram-*t-mvs
           (equal (list (mv-nth 0 (eval_subprogram-*t env fn params args))
                        (mv-nth 1 (eval_subprogram-*t env fn params args))
                        (mv-nth 2 (eval_subprogram-*t env fn params args)))
                  (eval_subprogram-*t env fn params args))
           :hints (("goal" :expand ((eval_subprogram-*t env fn params args))))))

  (fgl::def-fgl-rewrite eval_subprogram-*t-of-isaligned
    (implies (and (fgl::bind-fn-annotation annot 'eval_subprogram-*t-fn)
                  (not (assoc-keyword :eval_subprogram-*t-of-isaligned annot)))
             (equal (eval_subprogram-*t env "IsAligned" params args)
                    (b* (((mv res orac trace) (fgl::annotate `(:eval_subprogram-*t-of-isaligned t . ,annot)
                                                       (eval_subprogram-*t env "IsAligned" params args)))
                         (res
                          (eval_result-case res
                            :ev_normal (b* (((func_result res.res)))
                                         (if (consp res.res.vals)
                                             (b* ((resval (car res.res.vals))
                                                  (config (fgl::make-fgl-ipasir-config)))
                                               (val-case resval
                                                 :v_bool (cond ((fgl::check-true validp (fgl::fgl-validity-check config resval.val))
                                                                (fgl::fgl-prog2 (cw "isaligned: valid~%")
                                                                                (ev_normal (func_result (cons (v_bool t) (cdr res.res.vals))
                                                                                                        res.res.env))))
                                                               (t (fgl::fgl-prog2 (counterexample-with-backtrace config "IsAligned counterexample")
                                                                                  res)))
                                                 :otherwise res))
                                           res))
                            :otherwise res)))
                      (mv res orac trace))))
    :hints((And stable-under-simplificationp
                '(:in-theory (enable v_bool->val-rewrite)))
           (And stable-under-simplificationp
                '(:in-theory (enable cons-of-equal)))))

  


  ;; (local (defthmd v_bool->val-rewrite2
  ;;          (implies (and (equal (val-kind x) :v_bool)
  ;;                        (val-p x))
  ;;                   (iff (v_bool->val x)
  ;;                        (not (equal x (v_bool nil)))))
  ;;          :hints(("Goal" :in-theory (e/d (v_bool->val)
  ;;                                         ((v_bool)))))))
)

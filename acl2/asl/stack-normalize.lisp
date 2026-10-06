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

(include-book "trace-interp")
(include-book "stack-preserved")
(include-book "termination-error")
(local (include-book "centaur/vl/util/default-hints" :dir :system))




(defsection stack-preserved-*t
  (local (in-theory (acl2::e/d* (env-pop-stack env-push-stack)
                                ((tau-system)
                                 (:rules-of-class :congruence :here)
                                 (:rules-of-class :type-prescription :here)))))
  ;; (local (in-theory (acl2::enable* asl-*t-equals-original-rules)))
  (with-output
    ;; makes it so it won't take forever to print the induction scheme
    :evisc (:gag-mode (evisc-tuple 3 4 nil nil))
    :off (event)
    (make-event (append *stack-preserved-form*
                        '(:hints ((vl::big-mutrec-default-hint 'eval_expr-*t-fn id nil world))
                          :mutual-recursion asl-interpreter-mutual-recursion-*t)))))

(encapsulate nil
  ;; (local (defthm ev_normal->res-of-equal-to-ev_normal
  ;;          (implies (equal x (ev_normal res))
  ;;                   (equal (ev_normal->res x) res))))
  ;; (local (defthm ev_throwing->throwdata-of-equal-to-ev_throwing
  ;;          (implies (equal x (ev_throwing throwdata env bt))
  ;;                   (equal (ev_throwing->throwdata x)
  ;;                          (maybe-throwdata-fix throwdata)))))
  ;; (local (defthm ev_throwing->env-of-equal-to-ev_throwing
  ;;          (implies (equal x (ev_throwing throwdata env bt))
  ;;                   (equal (ev_throwing->env x)
  ;;                          (env-fix env)))))
  ;; (local (defthm ev_throwing->backtrace-of-equal-to-ev_throwing
  ;;          (implies (equal x (ev_throwing throwdata env bt))
  ;;                   (equal (ev_throwing->backtrace x)
  ;;                          bt))))
  ;; (local (defthm termination-error-p-of-check_recurse_limit
  ;;          (implies (not (termination-error-p (check_recurse_limit env name limit)))
  ;;                   (equal (check_recurse_limit env name limit)
  ;;                          (ev_normal nil)))
  ;;          :rule-classes :forward-chaining
  ;;          :hints(("Goal" :in-theory (enable check_recurse_limit)))))
  ;; (local (defthm ev_normal-of-check_recurse_limit
  ;;          (implies (equal (eval_result-kind (check_recurse_limit env name limit)) :ev_normal)
  ;;                   (equal (check_recurse_limit env name limit)
  ;;                          (ev_normal nil)))
  ;;          :rule-classes :forward-chaining
  ;;          :hints(("Goal" :in-theory (enable check_recurse_limit)))))

  (local (defthm stack_size-lookup-of-nil
           (equal (stack_size-lookup name nil) 0)
           :hints(("Goal" :in-theory (enable stack_size-lookup)))))
  (local (defthm vallist-fix-of-take
           (implies (<= (nfix n) (len x))
                    (equal (vallist-fix (take n x))
                           (take n (vallist-fix x))))
           :hints(("Goal" :in-theory (enable take)))))

  (local (defthm call-trace-output-of-nil
           (equal (call-trace-output nil name vparams vargs pos res trace)
                  (asl-tracelist-fix trace))
           :hints(("Goal" :in-theory (enable call-trace-output)))))
  (local (defthm stmt-trace-output-of-nil
           (equal (stmt-trace-output nil env s res trace)
                  (asl-tracelist-fix trace))
           :hints(("Goal" :in-theory (enable stmt-trace-output)))))

  (local (defthm tracespec-emptyp-of-call-interior-tracespec
           (implies (tracespec-emptyp x)
                    (tracespec-emptyp (call-interior-tracespec nil x)))
           :hints(("Goal" :in-theory (enable call-interior-tracespec combine-tracespecs)))))
  (local (defthm stmt-interior-tracespec-of-nil
           (equal (stmt-interior-tracespec nil x)
                  (tracespec-fix x))
           :hints(("Goal" :in-theory (enable stmt-interior-tracespec combine-tracespecs)))))

  ;; (local (defthm call-abort-after-of-nil
  ;;          (not (call-abort-after nil res))
  ;;          :hints(("Goal" :in-theory (enable call-abort-after)))))

  ;; (local (defthm stmt-abort-after-of-nil
  ;;          (not (stmt-abort-after nil res))
  ;;          :hints(("Goal" :in-theory (enable stmt-abort-after)))))
  
  (local (in-theory (acl2::e/d* (call-abort-after
                                 stmt-abort-after
                                 ;; global-replace-static
                                 ;; env-replace-static
                                 push_scope pop_scope
                                 ev_error-fix
                                 eval_for_step
                                 env-assign-local
                                 rethrow_implicit
                                 declare_local_identifier
                                 declare_local_identifiers
                                 remove_local_identifier
                                 env-find-global
                                 env-assign-global
                                 env-assign
                                 env-assign-local
                                 check_recurse_limit
                                 get_stack_size
                                 env-push-stack
                                 env-pop-stack
                                 env-find
                                 ;; call-trace-output
                                 ;; stmt-trace-output
                                 ;; call-interior-tracespec
                                 ;; stmt-interior-tracespec
                                 )
                                (asl-*t-equals-original-rules
                                 env-replace-static-with-self
                                 acl2::append-to-nil
                                 append
                                 true-listp
                                 set::sets-are-true-lists-cheap
                                 cons-equal
                                 not xor floor mod loghead
                                 equal-of-ev_normal
                                 equal-of-ev_throwing
                                 equal-of-ev_error
                                 equal-of-env
                                 equal-of-global-env
                                 global-replace-static-with-self
                                 assoc-equal len take update-nth nth hons-assoc-equal make-list-ac not
                                 ;; equal-of-ev_throwing
                                 ;; equal-of-ev_error
                                 ;; eval_result-fix-when-eval_result-p
                                 ;; eql
                                 (tau-system)
                                 (:rules-of-class :congruence :here)
                                 (:rules-of-class :type-prescription :here)
                                 )
                                ((:t stack_size-lookup)
                                 (:t len)))))

  
  (define env-equiv-except-stack-size ((x env-p) (y env-p))
    (and (equal (env->local x) (env->local y))
         (equal (global-env->static (env->global x)) (global-env->static (env->global y)))
         (equal (global-env->storage (env->global x)) (global-env->storage (env->global y))))
    ///
    (defthm env-equiv-except-stack-size-necc
      (implies (and (equal (env->local x) (env->local y))
                    (equal (global-env->static (env->global x)) (global-env->static (env->global y)))
                    (equal (global-env->storage (env->global x)) (global-env->storage (env->global y))))
               (equal (env-equiv-except-stack-size x y) t)))

    (defthm rewrite-when-equiv-except-stacksize
      (implies (env-equiv-except-stack-size x y)
               (and (equal (env->local x)
                           (env->local y))
                    (equal (global-env->static (env->global x)) (global-env->static (env->global y)))
                    (equal (global-env->storage (env->global x)) (global-env->storage (env->global y))))))

    ;; (defthm env-equiv-except-stack-size-of-env-replace-static
    ;;   (implies (env-equiv-except-stack-size x y)
    ;;            (env-equiv-except-stack-size (env-replace-static static-env x) (env-replace-static static-env y)))
    ;;   :hints(("Goal" :in-theory (enable env-replace-static global-replace-static))))
    )

  (define global-replace-stacksize ((stack_size pos-imap-p) (env global-env-p))
    :returns (new-env global-env-p)
    :hooks (:fix)
    (change-global-env env :stack_size stack_size)
    ///
    (defret global-env->static-of-<fn>
      (equal (global-env->static new-env)
             (global-env->static env)))
    (defret global-env->storage-of-<fn>
      (equal (global-env->storage new-env)
             (global-env->storage env)))
    (defret global-env->stack_size-of-<fn>
      (equal (global-env->stack_size new-env)
             (pos-imap-fix stack_size)))

    (defthm global-replace-stacksize-identity
      (equal (global-replace-stacksize (global-env->stack_size env) env)
             (global-env-fix env)))

    (defthm global-replace-stacksize-when-env-equiv-except-stack-size
      (implies (env-equiv-except-stack-size env2 env)
               (equal (global-replace-stacksize (global-env->stack_size (env->global env2)) (env->global env))
                      (env->global env2)))
      :hints(("Goal" :in-theory (enable env-equiv-except-stack-size
                                        global-replace-stacksize
                                        equal-of-global-env)))))

  (define env-replace-stacksize ((stack_size pos-imap-p) (env env-p))
    :returns (new-env env-p)
    :hooks (:fix)
    (change-env env :global (global-replace-stacksize stack_size (env->global env)))
    ///
    (defret env->local-of-env-replace-stacksize
      (equal (env->local new-env)
             (env->local env)))
    (defret env->global-of-env-replace-stacksize
      (equal (env->global new-env)
             (global-replace-stacksize stack_size (env->global env))))

    (defthm env-replace-stacksize-identity
      (equal (env-replace-stacksize (global-env->stack_size (env->global env)) env)
             (env-fix env)))

    ;; (defthm env-replace-stacksize-when-env-equiv-except-stack-size
    ;;   (implies (env-equiv-except-stack-size env2 env)
    ;;            (equal (env-replace-stacksize (global-env->stack_size (env->global env2)) env)
    ;;                   (env-fix env2)))
    ;;   :hints(("Goal" :in-theory (enable env-equiv-except-stack-size
    ;;                                     equal-of-env))))

    ;; (defthmd env-of-global-replace-stacksize
    ;;   (equal (env (global-replace-stacksize stacksize global) local)
    ;;          (env-replace-stacksize stacksize (env global local))))
    )

  (local (defthm global-replace-stacksize-of-global-replace-static
           (equal (global-replace-stacksize stacksize (global-replace-static static-env env))
                  (global-replace-static static-env (global-replace-stacksize stacksize env)))
           :hints(("Goal" :in-theory (e/d (global-replace-static global-replace-stacksize))))))

  (local (defthm env-replace-stacksize-of-env-replace-static
           (equal (env-replace-stacksize stacksize (env-replace-static static-env env))
                  (env-replace-static static-env (env-replace-stacksize stacksize env)))
           :hints(("Goal" :in-theory (e/d (env-replace-static env-replace-stacksize))))))

  ;; (local (defthm env-of-global-replace-static
  ;;          (equal (env (global-replace-static static-env global) local)
  ;;                 (env-replace-static static-env (env global local)))
  ;;          :hints(("Goal" :in-theory (enable env-replace-static global-replace-static)))))

  ;; (local (in-theory (enable env-of-global-replace-stacksize)))

  (defun-sk stack-sizes-lte (stack_size1 stack_size2)
    (forall fn
            (<= (stack_size-lookup fn stack_size1)
                (stack_size-lookup fn stack_size2)))
    :rewrite :direct)

  (in-theory (disable stack-sizes-lte))

  (defthm stack-sizes-lte-linear
    (implies (stack-sizes-lte stack_size1 stack_size2)
             (<= (stack_size-lookup fn stack_size1)
                 (stack_size-lookup fn stack_size2)))
    :rule-classes :linear)

  (defthm stack-sizes-lte-of-increment-stack
    (implies (stack-sizes-lte stack_size1 stack_size2)
             (stack-sizes-lte (increment-stack name stack_size1)
                              (increment-stack name stack_size2)))
    :hints (("goal" :expand ((stack-sizes-lte (increment-stack name stack_size1)
                                              (increment-stack name stack_size2))))))

  
  ;; (local (defthm env->global-when-equal-to-env
  ;;          (implies (Equal x (env global local))
  ;;                   (equal (env->global x) (global-env-fix global)))))

  ;; (local (defthm env->local-when-equal-to-env
  ;;          (implies (Equal x (env global local))
  ;;                   (equal (env->local x) (local-env-fix local)))))

  ;; (in-theory (enable env-replace-static
  ;;                    global-replace-static
  ;;                    ;; env-replace-stacksize
  ;;                    ;; global-replace-stacksize
  ;;                    ))
  
  (with-output
    ;; makes it so it won't take forever to print the induction scheme
    :evisc (:gag-mode (evisc-tuple 3 4 nil nil))
    :off (event)
    (std::defret-mutual-generate <fn>-stack_size-independent
      :universally-quantify (init-env)
      :rules ((t (:add-bindings
                  ((call2 (let ((env init-env)) <call>))
                   ((mv res2 new-orac2 trace2) call2)
                   (stacksize (global-env->stack_size (env->global init-env)))
                   (env-stacksize (global-env->stack_size (env->global env)))))
                 (:add-hyp (and (env-equiv-except-stack-size init-env env)
                                (syntaxp (not (equal init-env env)))
                                (stack-sizes-lte stacksize env-stacksize)
                                (not (termination-error-p res))))
                 (:add-concl (and (equal (eval_result-kind res2)
                                         (eval_result-kind res))
                                  (equal new-orac2 new-orac)
                                  (implies (tracespec-emptyp tracespec)
                                           (equal trace2 trace))
                                  (implies (eval_result-case res :ev_throwing)
                                           (equal res2
                                                  (b* (((ev_throwing res)))
                                                    (change-ev_throwing
                                                     res
                                                     :env (env-replace-stacksize stacksize res.env)))))
                                  (implies (eval_result-case res :ev_error)
                                           (equal res2 res))))
                 (:add-keyword :rule-classes nil))
              ((:has-return (and (:name res) (:type expr_eval_result-p)))
               (:add-concl (implies (eval_result-case res :ev_normal)
                                    (equal res2
                                           (b* (((ev_normal res))
                                                ((expr_result res.res)))
                                             (change-ev_normal
                                              res
                                              :res
                                              (change-expr_result
                                               res.res
                                               :env (env-replace-stacksize stacksize res.res.env))))))))
              ((:has-return (and (:name res) (:type exprlist_eval_result-p)))
               (:add-concl (implies (eval_result-case res :ev_normal)
                                    (equal res2
                                           (b* (((ev_normal res))
                                                ((exprlist_result res.res)))
                                             (change-ev_normal
                                              res
                                              :res
                                              (change-exprlist_result
                                               res.res
                                               :env (env-replace-stacksize stacksize res.res.env))))))))
              ((:has-return (and (:name res) (:type func_eval_result-p)))
               (:add-concl (implies (eval_result-case res :ev_normal)
                                    (equal res2
                                           (b* (((ev_normal res))
                                                ((func_result res.res)))
                                             (change-ev_normal
                                              res
                                              :res
                                              (change-func_result
                                               res.res
                                               :env (global-replace-stacksize stacksize res.res.env))))))))
              ((:has-return (and (:name res) (:type env_eval_result-p)))
               (:add-concl (implies (eval_result-case res :ev_normal)
                                    (equal res2
                                           (b* (((ev_normal res)))
                                             (change-ev_normal
                                              res
                                              :res (env-replace-stacksize stacksize res.res)))))))
              ((:has-return (and (:name res) (:type stmt_eval_result-p)))
               (:add-concl (implies (eval_result-case res :ev_normal)
                                    (equal res2
                                           (b* (((ev_normal res)))
                                             (control_flow_state-case
                                               res.res
                                               :returning
                                               (change-ev_normal
                                                res
                                                :res (change-returning
                                                      res.res
                                                      :env (global-replace-stacksize stacksize res.res.env)))
                                               :continuing
                                               (change-ev_normal
                                                res
                                                :res (change-continuing
                                                      res.res
                                                      :env (env-replace-stacksize stacksize res.res.env)))))))))

              ((:has-return (and (:name res) (:type slice_eval_result-p)))
               (:add-concl (implies (eval_result-case res :ev_normal)
                                    (equal res2
                                           (b* (((ev_normal res))
                                                ((intpair/env res.res)))
                                             (change-ev_normal
                                              res
                                              :res
                                              (change-intpair/env
                                               res.res
                                               :env (env-replace-stacksize stacksize res.res.env))))))))
              ((:has-return (and (:name res) (:type slices_eval_result-p)))
               (:add-concl (implies (eval_result-case res :ev_normal)
                                    (equal res2
                                           (b* (((ev_normal res))
                                                ((intpairlist/env res.res)))
                                             (change-ev_normal
                                              res
                                              :res
                                              (change-intpairlist/env
                                               res.res
                                               :env (env-replace-stacksize stacksize res.res.env))))))))
              ((:has-return (and (:name res)
                                 (not (:type slices_eval_result-p))
                                 (not (:type slice_eval_result-p))
                                 (not (:type stmt_eval_result-p))
                                 (not (:type func_eval_result-p))
                                 (not (:type exprlist_eval_result-p))
                                 (not (:type expr_eval_result-p))
                                 (not (:type env_eval_result-p))))
               (:add-concl (implies (eval_result-case res :ev_normal)
                                    (equal res2 res))))
              ((:fnname eval_limit-*t)
               (:add-keyword :hints ((and stable-under-simplificationp
                                          (let ((lit (car (last clause))))
                                            `(:expand (,lit
                                                       (:free (env pos otherwise is_while x) <call>))
                                              :do-not-induct t)))
                                     (and stable-under-simplificationp
                                          '(:in-theory (enable env-replace-static
                                                               global-replace-static
                                                               env-replace-stacksize
                                                               global-replace-stacksize))))))
              ((not (:fnname eval_limit-*t))
               (:add-keyword :hints ((and stable-under-simplificationp
                                          (let ((lit (car (last clause))))
                                            `(:expand (,lit
                                                       (:free (env pos otherwise is_while) <call>))
                                              :do-not-induct t)))
                                     (and stable-under-simplificationp
                                          '(:in-theory (enable env-replace-static
                                                               global-replace-static
                                                               env-replace-stacksize
                                                               global-replace-stacksize)))))))
      :mutual-recursion asl-interpreter-mutual-recursion-*t)))

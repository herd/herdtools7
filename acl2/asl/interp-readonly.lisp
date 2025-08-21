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

(include-book "interp")
(include-book "readonly")

(local
 (defthm constraint_kind-count-parametrized-reduction
   (IMPLIES
    (EQUAL (CONSTRAINT_KIND-KIND X)
           :PARAMETRIZED)
    (<
     (CONSTRAINT_KIND-COUNT
      (WELLCONSTRAINED (list (CONSTRAINT_EXACT (EXPR (E_VAR name) pos))) prec))
     (CONSTRAINT_KIND-COUNT X)))
   :hints(("Goal" :in-theory (enable constraint_kind-count
                                     int_constraintlist-count
                                     int_constraint-count
                                     expr-count
                                     expr_desc-count)))))




(defthm readonly-named_exprlist-p-implies-exprs
  (implies (readonly-named_exprlist-p x)
           (readonly-exprlist-p (named_exprlist->exprs x)))
  :hints(("Goal" :in-theory (enable named_exprlist->exprs)
          :induct (named_exprlist->exprs x)
          :expand ((:free (a B) (readonly-exprlist-p (cons a b)))
                   (readonly-exprlist-p nil)
                   (readonly-named_exprlist-p x)
                   (readonly-named_expr-p (car x))))))


(with-output
   ;; makes it so it won't take forever to print the induction scheme
   :evisc (:gag-mode (evisc-tuple 3 4 nil nil))
   (std::defret-mutual readonly-preserves-env-and-orac
     (defret <fn>-readonly-preserves-env-and-orac
       (implies (readonly-expr-p e)
                (and (equal new-orac orac)
                     (implies (eval_result-case res :ev_normal)
                              (equal (expr_result->env (ev_normal->res res))
                                     (env-fix env)))))
       :hints ('(:do-not-induct t
                 :expand ((readonly-expr-p e)
                          (readonly-expr_desc-p (expr->desc e))
                          (readonly-expr_desc-p-aux (expr->desc e))
                          <call>)))
       :fn eval_expr)
     (defret <fn>-readonly-preserves-env-and-orac
       (implies (readonly-exprlist-p e)
                (and (equal new-orac orac)
                     (implies (eval_result-case res :ev_normal)
                              (equal (exprlist_result->env (ev_normal->res res))
                                     (env-fix env)))))
       :hints ('(:do-not-induct t
                 :expand ((readonly-exprlist-p e)
                          <call>)))
       :fn eval_expr_list)
     (defret <fn>-readonly-preserves-env-and-orac
       (implies (readonly-slicelist-p sl)
                (and (equal new-orac orac)
                     (implies (eval_result-case res :ev_normal)
                              (equal (intpairlist/env->env (ev_normal->res res))
                                     (env-fix env)))))
       :hints ('(:do-not-induct t
                 :expand ((readonly-slicelist-p sl)
                          <call>)))
       :fn eval_slice_list)
     (defret <fn>-readonly-preserves-env-and-orac
       (implies (readonly-slice-p s)
                (and (equal new-orac orac)
                     (implies (eval_result-case res :ev_normal)
                              (equal (intpair/env->env (ev_normal->res res))
                                     (env-fix env)))))
       :hints ('(:do-not-induct t
                 :expand ((readonly-slice-p s)
                          <call>)))
       :fn eval_slice)
     (defret <fn>-readonly-preserves-orac
       (implies (readonly-pattern-p p)
                (equal new-orac orac))
       :hints ('(:do-not-induct t
                 :expand ((readonly-pattern-p p)
                          (readonly-pattern_desc-p (pattern->desc p))
                          <call>)))
       :fn eval_pattern)
     (defret <fn>-readonly-preserves-orac
       (implies (readonly-patternlist-p p)
                (equal new-orac orac))
       :hints ('(:do-not-induct t
                 :expand ((readonly-patternlist-p p)
                          <call>)))
       :fn eval_pattern_tuple)
     (defret <fn>-readonly-preserves-orac
       (implies (readonly-patternlist-p p)
                (equal new-orac orac))
       :hints ('(:do-not-induct t
                 :expand ((readonly-patternlist-p p)
                          <call>)))
       :fn eval_pattern-any)
     (defret <fn>-readonly-preserves-orac
       (implies (readonly-ty-p ty)
                (equal new-orac orac))
       :hints ('(:do-not-induct t
                 :expand ((readonly-ty-p ty)
                          (readonly-type_desc-p (ty->desc ty))
                          (:free (x) (readonly-constraint_kind-p x))
                          <call>)))
       :fn is_val_of_type)
     (defret <fn>-readonly-preserves-orac
       (implies (readonly-tylist-p types)
                (equal new-orac orac))
       :hints ('(:do-not-induct t
                 :expand ((readonly-tylist-p types)
                          <call>)))
       :fn is_val_of_type_tuple)
     (defret <fn>-readonly-preserves-orac
       (implies (readonly-int_constraintlist-p constrs)
                (equal new-orac orac))
       :hints ('(:do-not-induct t
                 :expand ((readonly-int_constraintlist-p constrs)
                          (readonly-int_constraint-p (car constrs))
                          <call>)))
       :fn check_int_constraints)
     :skip-others t
     :mutual-recursion asl-interpreter-mutual-recursion))


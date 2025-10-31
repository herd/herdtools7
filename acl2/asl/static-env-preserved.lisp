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
(include-book "std/util/defret-mutual-generate" :dir :System)



(defret <fn>-static-env-preserved
  (equal (global-env->static (env->global new-env))
         (global-env->static (env->global env)))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn env-assign-global)

(defret <fn>-global-env-preserved
  (equal (env->global new-env) (env->global env))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn env-assign-local)

(defret <fn>-static-env-preserved
  (and (implies (env_result-case res :lk_local)
                (equal (global-env->static (env->global (lk_local->val res)))
                       (global-env->static (env->global env))))
       (implies (env_result-case res :lk_global)
                (equal (global-env->static (env->global (lk_global->val res)))
                       (global-env->static (env->global env)))))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn env-assign)

(defret <fn>-global-env-preserved
  (equal (env->global new-env) (env->global env))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn pop_scope)

(defret <fn>-global-env-preserved
  (equal (env->global new-env) (env->global env))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn push_scope)

(defret <fn>-global-env-preserved
  (equal (env->global new-env) (env->global env))
  :hints(("Goal" :in-theory (enable <fn>
                                    env-assign-local)))
  :fn eval_for_step)

(defret <fn>-global-env-preserved
  (equal (env->global new-env) (env->global env))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn declare_local_identifier)

(defret <fn>-global-env-preserved
  (equal (env->global new-env) (env->global env))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn declare_local_identifiers)

(defret <fn>-global-env-preserved
  (equal (env->global new-env) (env->global env))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn remove_local_identifier)


(defret <fn>-env-preserved
  (equal (ev_throwing->env new-x)
         (ev_throwing->env x))
  :hints(("Goal" :in-theory (enable <fn>
                                    ev_throwing->env-when-wrong-kind)))
  :fn init-backtrace)

(defret <fn>-eval_result-kind
  (equal (eval_result-kind res)
         (eval_result-kind blkres))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn rethrow_implicit)

(defret <fn>-env-preserved-throw
  (implies (eval_result-case blkres :ev_throwing)
           (equal (ev_throwing->env res)
                  (ev_throwing->env blkres)))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn rethrow_implicit)

(defret <fn>-env-preserved-normal
  (implies (eval_result-case blkres :ev_normal)
           (b* ((res (ev_normal->res res))
                (blkres (ev_normal->res blkres)))
             (and (equal (control_flow_state-kind res)
                         (control_flow_state-kind blkres))
                  (implies (control_flow_state-case blkres :returning)
                           (equal (returning->env res)
                                  (returning->env blkres)))
                  (implies (control_flow_state-case blkres :continuing)
                           (equal (continuing->env res)
                                  (continuing->env blkres))))))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn rethrow_implicit)

(defret <fn>-not-throwing
  (and (not (equal (eval_result-kind i) :ev_throwing))
       (implies (not (equal (eval_result-kind i) :ev_normal))
                (equal (eval_result-kind i) :ev_error)))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn v_to_bool)

(defret <fn>-not-throwing
  (and (not (equal (eval_result-kind i) :ev_throwing))
       (implies (not (equal (eval_result-kind i) :ev_normal))
                (equal (eval_result-kind i) :ev_error)))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn v_to_int)

(defret <fn>-not-throwing
  (and (not (equal (eval_result-kind i) :ev_throwing))
       (implies (not (equal (eval_result-kind i) :ev_normal))
                (equal (eval_result-kind i) :ev_error)))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn v_to_label)

(defret <fn>-not-throwing
  (and (not (equal (eval_result-kind res) :ev_throwing))
       (implies (not (equal (eval_result-kind res) :ev_normal))
                (equal (eval_result-kind res) :ev_error)))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn tick_loop_limit)

(defret <fn>-not-throwing
  (not (equal (eval_result-kind res) :ev_throwing))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn env-find-global)

(defret <fn>-not-throwing
  (and (not (equal (eval_result-kind v) :ev_throwing))
       (implies (not (equal (eval_result-kind v) :ev_normal))
                (equal (eval_result-kind v) :ev_error)))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn bitvec_fields_to_record!)

(defret <fn>-not-throwing
  (and (not (equal (eval_result-kind v) :ev_throwing))
       (implies (not (equal (eval_result-kind v) :ev_normal))
                (equal (eval_result-kind v) :ev_error)))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn bitvec_fields_to_record)

(defret <fn>-not-throwing
  (and (not (equal (eval_result-kind err) :ev_throwing))
       (implies (not (equal (eval_result-kind err) :ev_normal))
                (equal (eval_result-kind err) :ev_error)))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn check_two_ranges_non_overlapping)

(defret <fn>-not-throwing
  (and (not (equal (eval_result-kind err) :ev_throwing))
       (implies (not (equal (eval_result-kind err) :ev_normal))
                (equal (eval_result-kind err) :ev_error)))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn check_non_overlapping_slices-1)

(defret <fn>-not-throwing
  (and (not (equal (eval_result-kind err) :ev_throwing))
       (implies (not (equal (eval_result-kind err) :ev_normal))
                (equal (eval_result-kind err) :ev_error)))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn check_non_overlapping_slices)

(local (in-theory (disable loghead logtail floor logior logmask lognot)))

(defret <fn>-not-throwing
  (and (not (equal (eval_result-kind res) :ev_throwing))
       (implies (not (equal (eval_result-kind res) :ev_normal))
                (equal (eval_result-kind res) :ev_error)))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn check-bad-slices)

(defret <fn>-not-throwing
  (and (not (equal (eval_result-kind res) :ev_throwing))
       (implies (not (equal (eval_result-kind res) :ev_normal))
                (equal (eval_result-kind res) :ev_error)))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn vbv-to-int)

(defret <fn>-not-throwing
  (and (not (equal (eval_result-kind res) :ev_throwing))
       (implies (not (equal (eval_result-kind res) :ev_normal))
                (equal (eval_result-kind res) :ev_error)))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn write_to_bitvector)

(defret <fn>-not-throwing
  (not (equal (eval_result-kind eval) :ev_throwing))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn check_recurse_limit)

(defret <fn>-not-throwing
  (not (equal (eval_result-kind res) :ev_throwing))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn eval_primitive)

;; (defret <fn>-not-throwing
;;   (not (equal (eval_result-kind new-env) :ev_throwing))
;;   :hints(("Goal" :in-theory (enable <fn>)))
;;   :fn env-push-stack)

(defret <fn>-not-throwing
  (and (not (equal (eval_result-kind res) :ev_throwing))
       (implies (not (equal (eval_result-kind res) :ev_normal))
                (equal (eval_result-kind res) :ev_error)))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn eval_binop)

(defret <fn>-not-throwing
  (and (not (equal (eval_result-kind res) :ev_throwing))
       (implies (not (equal (eval_result-kind res) :ev_normal))
                (equal (eval_result-kind res) :ev_error)))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn eval_unop)

(defret <fn>-not-throwing
  (not (equal (eval_result-kind res) :ev_throwing))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn eval_pattern_mask)

(defret <fn>-not-throwing
  (and (not (equal (eval_result-kind v) :ev_throwing))
       (implies (not (equal (eval_result-kind v) :ev_normal))
                (equal (eval_result-kind v) :ev_error)))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn get_field!)

(defret <fn>-not-throwing
  (and (not (equal (eval_result-kind v) :ev_throwing))
       (implies (not (equal (eval_result-kind v) :ev_normal))
                (equal (eval_result-kind v) :ev_error)))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn map-get_field!)

(defret <fn>-not-throwing
  (and (not (equal (eval_result-kind v) :ev_throwing))
       (implies (not (equal (eval_result-kind v) :ev_normal))
                (equal (eval_result-kind v) :ev_error)))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn map-get_field)

(defret <fn>-not-throwing
  (and (not (equal (eval_result-kind v) :ev_throwing))
       (implies (not (equal (eval_result-kind v) :ev_normal))
                (equal (eval_result-kind v) :ev_error)))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn concat_bitvectors)

(defret <fn>-not-throwing
  (and (not (equal (eval_result-kind v) :ev_throwing))
       (implies (not (equal (eval_result-kind v) :ev_normal))
                (equal (eval_result-kind v) :ev_error)))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn get_field)

(defret <fn>-preserves-static-env
  (equal (global-env->static (env->global new-env))
         (global-env->static (env->global env)))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn env-push-stack)

(defret <fn>-preserves-static-env
  (equal (global-env->static (env->global new-env))
         (global-env->static call-env))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn env-pop-stack)

(local (include-book "centaur/vl/util/default-hints" :dir :system))

(progn 
  (defconst *static-env-preserved-form*
    '(std::defret-mutual-generate <fn>-static-env-preserved
       :return-concls (((expr_eval_result-p res)
                        (implies (eval_result-case res :ev_normal)
                                 (equal (global-env->static
                                         (env->global
                                          (expr_result->env (ev_normal->res res))))
                                        (global-env->static (env->global env)))))
                       ((exprlist_eval_result-p res)
                        (implies (eval_result-case res :ev_normal)
                                 (equal (global-env->static
                                         (env->global
                                          (exprlist_result->env (ev_normal->res res))))
                                        (global-env->static (env->global env)))))
                       ((func_eval_result-p res)
                        (implies (eval_result-case res :ev_normal)
                                 (equal (global-env->static
                                         (func_result->env (ev_normal->res res)))
                                        (global-env->static (env->global env)))))
                       ((env_eval_result-p res)
                        (implies (eval_result-case res :ev_normal)
                                 (equal (global-env->static
                                         (env->global (ev_normal->res res)))
                                        (global-env->static (env->global env)))))
                       ((stmt_eval_result-p res)
                        (implies (eval_result-case res :ev_normal)
                                 (b* ((res (ev_normal->res res)))
                                   (and (implies (control_flow_state-case res :returning)
                                                 (equal (global-env->static (returning->env res))
                                                        (global-env->static (env->global env))))
                                        (implies (control_flow_state-case res :continuing)
                                                 (equal (global-env->static (env->global (continuing->env res)))
                                                        (global-env->static (env->global env))))))))
                       ((slice_eval_result-p res)
                        (implies (eval_result-case res :ev_normal)
                                 (equal (global-env->static
                                         (env->global
                                          (intpair/env->env (ev_normal->res res))))
                                        (global-env->static (env->global env)))))
                       ((slices_eval_result-p res)
                        (implies (eval_result-case res :ev_normal)
                                 (equal (global-env->static
                                         (env->global
                                          (intpairlist/env->env (ev_normal->res res))))
                                        (global-env->static (env->global env))))))
                  
       :rules ((t (:add-concl (implies (eval_result-case res :ev_throwing)
                                       (equal (global-env->static
                                               (env->global (ev_throwing->env res)))
                                              (global-env->static (env->global env))))))
               ;; (t (:add-keyword :hints ('(:expand (<call>)
               ;;                            :do-not-induct t))))
               )))
  (with-output
    ;; makes it so it won't take forever to print the induction scheme
    :evisc (:gag-mode (evisc-tuple 3 4 nil nil))
    :off (event)
    (make-event (append *static-env-preserved-form*
                        '(:hints ((vl::big-mutrec-default-hint 'eval_expr-fn id nil world))
                          :mutual-recursion asl-interpreter-mutual-recursion)))))


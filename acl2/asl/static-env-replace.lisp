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
(include-book "std/util/defconsts" :dir :system)
(local (include-book "interp-theory"))



(include-book "static-env-preserved")

(local (std::add-default-post-define-hook :fix))

(define global-replace-static ((static static_env_global-p)
                               (global global-env-p))
  :returns (new-global global-env-p)
  (change-global-env global :static static)
  ///
  (defret global-env->storage-of-<fn>
    (equal (global-env->storage new-global)
           (global-env->storage global)))

  (defret global-env->stack_size-of-<fn>
    (equal (global-env->stack_size new-global)
           (global-env->stack_size global)))

  (defret global-env->static-of-<fn>
    (equal (global-env->static new-global)
           (static_env_global-fix static)))

  (defret <fn>-of-<fn>
    (equal (global-replace-static static1 new-global)
           (global-replace-static static1 global)))

  (defret <fn>-with-self
    (implies (equal (static_env_global-fix static) (global-env->static global))
             (equal (global-replace-static static global)
                    (global-env-fix global)))))


(define env-replace-static ((static static_env_global-p)
                            (env env-p))
  :returns (new-env env-p)
  (change-env env :global (global-replace-static static (env->global env)))
  ///
  (defret env->global-of-<fn>
    (equal (env->global new-env)
           (global-replace-static static (env->global env))))

  (defret env->local-of-<fn>
    (equal (env->local new-env)
           (env->local env)))

  (defthm env-replace-static-of-env
    (equal (env-replace-static static (env global local))
           (env (global-replace-static static global) local)))

  (defret <fn>-of-<fn>
    (equal (env-replace-static static1 new-env)
           (env-replace-static static1 env)))

  (defret <fn>-with-self
    (implies (equal (static_env_global-fix static) (global-env->static (env->global env)))
             (equal (env-replace-static static env)
                    (env-fix env)))))

(local (defconst *asl-*staticro-xdoc*
         '(:parents (asl-tracing)
           :short "Modified version of @(see asl-interpreter-mutual-recursion) that passes
the static env as a separate, read-only argument."
           :long "")))


(local
 (defun replace-static-envs (x)
   (if (atom x)
       x
     (case-match x
       (('global-env->static ('env->global . &) . &) 'static-env)
       (& (cons (replace-static-envs (car x))
                (replace-static-envs (cdr x))))))))

(local (include-book "interp-mods"))

(defconsts *asl-interp-fns*
  (acl2::strip-cadrs
   (keep-define-forms-in-list
    (find-form-by-car
     'defines *asl-interpreter-mutual-recursion-command*))))

(defconsts *eval-staticro-substitution*
  (pair-suffixed (append *asl-interp-fns*
                         '(asl-interpreter-mutual-recursion))
                 '-*staticro))

(defmacro bind-env-with-static (body)
  `(b* ((env (mbe :logic (env-replace-static static-env env)
                  :exec env)))
     ,body))

(with-output
  :off (event)
  (make-event
   (b* ((form *asl-interpreter-mutual-recursion-command*)
        ;; Strip out the events after the /// (theorem about resolved-p-of-resolve-ty)
        (form (strip-post-/// form))
        ;; Strip out xdoc
        (form (strip-xdoc form))
        ;; Add xdoc topic for mutual recursion
        (form (add-mutrec-xdoc *asl-*staticro-xdoc* form))
        ;; Add xdoc topic for each function
        (form (add-define-xdoc
               "Version of @(see <NAME>) with static env passed as a separate read-only argument;
                see @(see asl-interpreter-mutual-recursion-*staticro) for overview."
               form))
        ;; Substitute function names with their -*t suffixed forms.
        (form (sublis *eval-staticro-substitution* form))
        ;; Replace all invocations of (global-env->static (env->global env)) with the variable static-env.
        (form (replace-static-envs form))
        ;; Add guard saying static-env equals the one in env.
        (form (add-define-guard '(equal (global-env->static (env->global env)) static-env) form))
        ;; Wrap each form with binding of env to one with static-env inserted
        (form (wrap-define-bodies 'bind-env-with-static form))
        ;; Add the tracespec formal to each define form.
        (form (add-define-formals '(((static-env static_env_global-p) 'static-env)) form))
        ;; Disable the functions and verify guards.
        (form (insert-after-///
               (list
                '(make-event
                  `(in-theory (disable . ,(fgetprop 'eval_expr-*t-fn 'acl2::recursivep nil (w state)))))
                ;; (equals-original-thm '*t (w state))
                ;; '(verify-guards eval_expr-*staticro-fn)
                )
               form)))
     `(progn (defconst *asl-interpreter-mutual-recursion-*staticro-form* ',form)
             ,form))))









(defret <fn>-of-env-replace-static
  (equal (let ((env (env-replace-static static env))) <call>)
         (env-replace-static static new-env))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn env-assign-global)

(defret <fn>-of-env-replace-static
  (equal (let ((env (env-replace-static static env))) <call>)
         (env-replace-static static new-env))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn env-assign-local)

(defret <fn>-of-env-replace-static
  (b* ((new-res (let ((env (env-replace-static static env))) <call>)))
    (equal new-res
           (env_result-case res
             :lk_local (lk_local (env-replace-static static (lk_local->val res)))
             :lk_global (lk_global (env-replace-static static (lk_global->val res)))
             :otherwise res)))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn env-assign)

(defret <fn>-of-env-replace-static
  (equal (let ((env (env-replace-static static env))) <call>)
         (env-replace-static static new-env))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn pop_scope)

(defret <fn>-of-env-replace-static
  (equal (let ((env (env-replace-static static env))) <call>)
         (env-replace-static static new-env))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn push_scope)

(defret <fn>-of-env-replace-static
  (equal (let ((env (env-replace-static static env))) <call>)
         (mv v_step (env-replace-static static new-env)))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn eval_for_step)



(defret <fn>-global-env-replace-static
  (equal (let ((env (env-replace-static static env))) <call>)
         (env-replace-static static new-env))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn declare_local_identifier)

(defret <fn>-global-env-replace-static
  (equal (let ((env (env-replace-static static env))) <call>)
         (env-replace-static static new-env))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn declare_local_identifiers)

(defret <fn>-global-env-replace-static
  (equal (let ((env (env-replace-static static env))) <call>)
         (env-replace-static static new-env))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn remove_local_identifier)



(defret <fn>-replace-static
  (equal (let ((env (env-replace-static static env))) <call>)
         (env-replace-static static new-env))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn env-push-stack)

(defret <fn>-call-env-replace-static
  (equal (let ((call-env (global-replace-static static call-env))) <call>)
         (env-replace-static static new-env))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn env-pop-stack)

(defret <fn>-prev-env-replace-static
  (equal (let ((prev-env (env-replace-static static prev-env))) <call>)
         new-env)
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn env-pop-stack)




(defconst *eval_result-replace-static-template*
  '(define <evalresult-name>-replace-static ((static static_env_global-p)
                                            (x <evalresult-name>-p))
     :returns (new-x <evalresult-name>-p)
     (eval_result-case x
       :ev_normal
       (:@ :no-result (eval_result-fix x))
       (:@ (not :no-result) (ev_normal (<result-name>-replace-static static x.res)))
       :ev_throwing (change-ev_throwing x :env (env-replace-static static x.env))
       :otherwise (eval_result-fix x))
     ///
     (defret eval_result-kind-of-<fn>
       (equal (eval_result-kind new-x)
              (eval_result-kind x)))

     (:@ (not :no-result)
      (defret ev_normal->res-of-<fn>
        (implies (eval_result-case x :ev_normal)
                 (equal (ev_normal->res new-x)
                        (<result-name>-replace-static static (ev_normal->res x))))))

     ;; (:@ (not :no-result)
     ;;  (defret ev_normal-of-<result-name>-replace-static
     ;;    (equal (ev_normal (<result-name>-replace-static static res))
     ;;           (<evalresult-name>-replace-static static (ev_normal res)))))
     
     (defret ev_throwing->env-of-<fn>
       (implies (eval_result-case x :ev_throwing)
                (equal (ev_throwing->env new-x)
                       (env-replace-static static (ev_throwing->env x)))))

     (defret ev_throwing->throwdata-of-<fn>
       (implies (eval_result-case x :ev_throwing)
                (equal (ev_throwing->throwdata new-x)
                       (ev_throwing->throwdata x))))

     (defret ev_throwing->backtrace-of-<fn>
       (implies (eval_result-case x :ev_throwing)
                (equal (ev_throwing->backtrace new-x)
                       (ev_throwing->backtrace x))))

     ;; (defret ev_error->desc-of-<fn>
     ;;   (implies (eval_result-case x :ev_error)
     ;;            (equal (ev_error->desc new-x)
     ;;                   (ev_error->desc x))))

     ;; (defret ev_error->data-of-<fn>
     ;;   (implies (eval_result-case x :ev_error)
     ;;            (equal (ev_error->data new-x)
     ;;                   (ev_error->data x))))

     ;; (defret ev_error->backtrace-of-<fn>
     ;;   (implies (eval_result-case x :ev_error)
     ;;            (equal (ev_error->backtrace new-x)
     ;;                   (ev_error->backtrace x))))

     
     (defret <fn>-of-ev_error
       (and (let ((x (ev_error data desc backtrace)))
              (equal <call> x))
            (implies (and (syntaxp (Quotep x))
                          (eval_result-case x :ev_error))
                     (equal new-x
                            (eval_result-fix x)))))

     (:@ (not :no-result)
      (defret <fn>-when-not-normal
        (implies (not (eval_result-case x :ev_normal))
                 (equal new-x
                        (eval_result-replace-static static x)))
        :hints(("Goal" :in-theory (enable eval_result-replace-static)))))

     (:@ (not :no-result)
      (defret <fn>-when-error
        (implies (eval_result-case x :ev_error)
                 (equal new-x
                        (eval_result-fix x)))))

     (:@ :no-result
      (defret <fn>-when-not-throwing
        (implies (not (eval_result-case x :ev_throwing))
                 (equal new-x
                        (eval_result-fix x)))))))

(defmacro def-eval_result-replace-static (evalresult-name
                                          result-name)
  (acl2::template-subst *eval_result-replace-static-template*
                        :str-alist `(("<EVALRESULT-NAME>" . ,(symbol-name evalresult-name))
                                     ("<RESULT-NAME>" . ,(symbol-name result-name)))
                        :features (and (not result-name) '(:no-result))
                        :pkg-sym 'asl-pkg))

(def-eval_result-replace-static eval_result nil)

(defthm eval_result-replace-static-of-ev_normal
  (and (equal (eval_result-replace-static static (ev_normal res))
              (ev_normal res))
       (implies (and (syntaxp (Quotep x))
                     (eval_result-case x :ev_normal))
                (equal (eval_result-replace-static static x)
                       (eval_result-fix x))))
  :hints(("Goal" :in-theory (enable eval_result-replace-static))))




(define expr_result-replace-static ((static static_env_global-p)
                                    (x expr_result-p))
  :returns (new-x expr_result-p)
  (change-expr_result x :env (env-replace-static static (expr_result->env x)))
  ///
  (defret expr_result->val-of-<fn>
    (equal (expr_result->val new-x)
           (expr_result->val x)))

  (defret expr_result->env-of-<fn>
    (equal (expr_result->env new-x)
           (env-replace-static static (expr_result->env x)))))

(def-eval_result-replace-static expr_eval_result expr_result)

(define exprlist_result-replace-static ((static static_env_global-p)
                                    (x exprlist_result-p))
  :returns (new-x exprlist_result-p)
  (change-exprlist_result x :env (env-replace-static static (exprlist_result->env x)))
  ///
  (defret exprlist_result->val-of-<fn>
    (equal (exprlist_result->val new-x)
           (exprlist_result->val x)))

  (defret exprlist_result->env-of-<fn>
    (equal (exprlist_result->env new-x)
           (env-replace-static static (exprlist_result->env x)))))

(def-eval_result-replace-static exprlist_eval_result exprlist_result)

(define func_result-replace-static ((static static_env_global-p)
                                    (x func_result-p))
  :returns (new-x func_result-p)
  (change-func_result x :env (global-replace-static static (func_result->env x)))
  ///
  (defret func_result->vals-of-<fn>
    (equal (func_result->vals new-x)
           (func_result->vals x)))

  (defret func_result->env-of-<fn>
    (equal (func_result->env new-x)
           (global-replace-static static (func_result->env x)))))

(def-eval_result-replace-static func_eval_result func_result)


(define control_flow_state-replace-static ((static static_env_global-p)
                                           (x control_flow_state-p))
  :returns (new-x control_flow_state-p)
  (control_flow_state-case x
    :returning (change-returning x :env (global-replace-static static x.env))
    :continuing (change-continuing x :env (env-replace-static static x.env)))
  ///
  (defret control_flow_state-kind-of-<fn>
    (equal (control_flow_state-kind new-x)
           (control_flow_state-kind x)))

  (defret returning->vals-of-<fn>
    (equal (returning->vals new-x)
           (returning->vals x))
    :hints(("Goal" :in-theory (enable returning->vals-when-wrong-kind))))

  (defret returning->env-of-<fn>
    (implies (control_flow_state-case x :returning)
             (equal (returning->env new-x)
                    (global-replace-static static (returning->env x)))))

  (defret continuing->env-of-<fn>
    (implies (control_flow_state-case x :continuing)
             (equal (continuing->env new-x)
                    (env-replace-static static (continuing->env x))))))

(def-eval_result-replace-static stmt_eval_result control_flow_state)

(def-eval_result-replace-static env_eval_result env)


(define intpair/env-replace-static ((static static_env_global-p)
                                    (x intpair/env-p))
  :returns (new-x intpair/env-p)
  (change-intpair/env x :env (env-replace-static static (intpair/env->env x)))
  ///
  (defret intpair/env->pair-of-<fn>
    (equal (intpair/env->pair new-x)
           (intpair/env->pair x)))

  (defret intpair/env->env-of-<fn>
    (equal (intpair/env->env new-x)
           (env-replace-static static (intpair/env->env x)))))

(def-eval_result-replace-static slice_eval_result intpair/env)

(define intpairlist/env-replace-static ((static static_env_global-p)
                                        (x intpairlist/env-p))
  :returns (new-x intpairlist/env-p)
  (change-intpairlist/env x :env (env-replace-static static (intpairlist/env->env x)))
  ///
  (defret intpairlist/env->pairlist-of-<fn>
    (equal (intpairlist/env->pairlist new-x)
           (intpairlist/env->pairlist x)))

  (defret intpairlist/env->env-of-<fn>
    (equal (intpairlist/env->env new-x)
           (env-replace-static static (intpairlist/env->env x)))))

(def-eval_result-replace-static slices_eval_result intpairlist/env)

(local (include-book "centaur/vl/util/default-hints" :dir :system))

(local (defthm mv-nth-when-equal-cons
         (implies (equal x (cons a b))
                  (equal (mv-nth n x)
                         (mv-nth n (cons a b))))))

;; (local (defthm is_val_of_type_tuple-mv-nths
;;          (let ((call (is_val_of_type_tuple env vals types)))
;;            (equal (list (mv-nth 0 call) (mv-nth 1 call)) call))
;;          :hints (("goal" :Expand ((is_val_of_type_tuple env vals types))))))


(defret <fn>-of-stmt_eval_result-replace-static
  (equal (let ((blkres (stmt_eval_result-replace-static static blkres))) <call>)
         (stmt_eval_result-replace-static static res))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn rethrow_implicit)

(defret <fn>-of-eval_result-replace-static
  (implies (not (eval_result-case blkres :ev_normal))
           (equal (let ((blkres (eval_result-replace-static static blkres))) <call>)
                  (eval_result-replace-static static res)))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn rethrow_implicit)


(defthm rethrow_implicit-when-not-throw
  (implies (not (eval_result-case blkres :ev_throwing))
           (equal (rethrow_implicit throw blkres backtrace)
                  blkres))
  :hints(("Goal" :in-theory (enable rethrow_implicit))))

(defret env-find-global-of-env-replace-static
  (equal (let ((env (env-replace-static static env))) <call>)
         res)
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn env-find-global)


(defthm eval_limit-*staticro-mv-nths
  (let ((res (eval_limit-*staticro env limit)))
    (equal (list (mv-nth 0 res) (mv-nth 1 res)) res))
  :hints (("goal" :expand ((eval_limit-*staticro env limit)))))

(defthm stmt_eval_result-replace-static-when-ev_normal
  (implies (eval_result-case x :ev_normal)
           (equal (stmt_eval_result-replace-static static x)
                  (ev_normal (control_flow_state-replace-static static (ev_normal->res x)))))
  :hints(("Goal" :in-theory (enable stmt_eval_result-replace-static))))


(defthm check_recurse_limit-of-global-replace-static
  (equal (check_recurse_limit (env (global-replace-static static global) local) name limit)
         (check_recurse_limit (env global local) name limit))
  :hints(("Goal" :in-theory (enable check_recurse_limit get_stack_size))))




(local
 (defun staticro-equals-original-thms (names suffix wrld)
   (if (atom names)
       nil
     (cons
      (let* ((name (car names))
             (name-mod (intern-in-package-of-symbol
                        (concatenate 'string (symbol-name name) "-"
                                     (symbol-name suffix))
                        name))
             (macro-args (macro-args name wrld))
             (nonkey-formals (take (- (len macro-args)
                                      (len (member '&key macro-args)))
                                   macro-args)))
        `(defret ,(intern-in-package-of-symbol
                   (concatenate 'string "<FN>" "-EQUALS-ORIGINAL")
                   'asl-pkg)
           (equal (,name-mod . ,nonkey-formals)
                  (let ((env (env-replace-static static-env env)))
                    (,name . ,nonkey-formals)))
           :hints ((let ((expand (acl2::just-expand-cp-parse-hints
                                  '((:free (,@nonkey-formals clk orac) (,name-mod . ,nonkey-formals))
                                    (:free (,@nonkey-formals clk orac) (,name . ,nonkey-formals)))
                                  world)))
                     `(:computed-hint-replacement
                       ((acl2::expand-marked))
                       :clause-processor (acl2::mark-expands-cp
                                          clause
                                          '(t ;; last-only
                                            t ;; lambdas
                                            ,expand))
                       :do-not-induct t)))
           :fn ,name-mod))
      (staticro-equals-original-thms (cdr names) suffix wrld)))))

(local
 (defun staticro-equals-original-thm (suffix wrld)
   `(encapsulate nil
      (local (deflabel before-equals-original))
      (std::defret-mutual
        ,(intern-in-package-of-symbol (concatenate 'string (symbol-name suffix) "EQUALS-ORIGINAL") 'asl-pkg)
        . ,(staticro-equals-original-thms *asl-interp-fns* suffix wrld))
      (acl2::def-ruleset! asl-*staticro-equals-original-rules
        (set-difference-theories (current-theory :here)
                                 (current-theory 'before-equals-original))))))

(with-output
  :off (event)
  :evisc (:gag-mode (evisc-tuple 3 4 nil nil))
  (make-event
   (staticro-equals-original-thm '*staticro (w state))))


(local (in-theory (disable (tau-system)
                           len assoc-equal append true-listp loghead hons-assoc-equal floor mod expt take
                           acl2::repeat)))

(local
 (defthm eval_result-kind-of-rethrow_implicit
   (equal (eval_result-kind (rethrow_implicit throw blkres backtrace))
          (eval_result-kind blkres))
   :hints(("Goal" :in-theory (enable rethrow_implicit)))))

(local
 (defthm ev_error->desc-of-rethrow_implicit
   (implies (eval_result-case blkres :ev_error)
            (equal (ev_error->desc (rethrow_implicit throw blkres backtrace))
                   (ev_error->desc blkres)))
   :hints(("Goal" :in-theory (enable rethrow_implicit)))))

(with-output
  :off (event)
  (verify-guards eval_expr-*staticro-fn
    :guard-debug t))
             









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

(include-book "arbvals")
(include-book "trace-interp")
(include-book "std/util/defconsts" :dir :system)
(include-book "clause-processors/just-expand" :dir :System)
(local (include-book "interp-theory"))
(local (include-book "interp-mods"))
(include-book "static-env-replace")

(local (in-theory (disable integer-listp))) ;; doubles the time for some deftypes if not disabled

(local (include-book "centaur/vl/util/default-hints" :dir :system))
(local (std::add-default-post-define-hook :fix))

(defconst *arboffset-for-form-table-+t*
  ;; FIXME copy-paste of *arboffset-for-form-table*
  '((eval_expr-+t                    (expr-arbaddr-offset 1))
    (resolve-int_constraints-+t      (int_constraintlist-arbaddr-offset 1))
    (resolve-constraint_kind-+t      (constraint_kind-arbaddr-offset 1))
    (resolve-tylist-+t               (tylist-arbaddr-offset 1))
    (resolve-typed_identifierlist-+t (typed_identifierlist-arbaddr-offset 1))
    (resolve-ty-+t                   (ty-arbaddr-offset 1))
    (eval_pattern-+t                 (pattern-arbaddr-offset 2))
    (eval_pattern_list-+t            (patternlist-arbaddr-offset 2))
    (eval_pattern_matcher-+t         (pattern_matcher-arbaddr-offset 2))
    (eval_expr_list-+t               (exprlist-arbaddr-offset 1))
    (eval_call-+t                    (fnname-arbaddr-offset 0)
                                     (exprlist-arbaddr-offset 2)
                                     (exprlist-arbaddr-offset 3))
    (eval_subprogram-+t              )
    (eval_lexpr-+t                   (lexpr-arbaddr-offset 1))
    (eval_lexpr_list-+t              (lexprlist-arbaddr-offset 1))
    (eval_limit-+t                   (maybe-expr-arbaddr-offset 1))
    (eval_stmt-+t                    (stmt-arbaddr-offset 1))
    (eval_catchers-+t                (catcherlist-arbaddr-offset 1)
                                     (maybe-stmt-arbaddr-offset 2))
    (eval_slice-+t                   (slice-arbaddr-offset 1))
    (eval_slice_list-+t              (slicelist-arbaddr-offset 1))
    (eval_block-+t                   (stmt-arbaddr-offset 1))
    (is_val_of_type_tuple-+t         (tylist-arbaddr-offset 2))
    (check_int_constraints-+t        (int_constraintlist-arbaddr-offset 2))
    (is_val_of_type-+t               (ty-arbaddr-offset 2))))

(defmacro add-arboffset-for-form-+t (arboffset form)
  (let ((form-arboffset (add-arboffsets-for-form-args
                         (cdr form)
                         (cdr (assoc (car form) *arboffset-for-form-table-+t*)))))
    (if form-arboffset
        `(arbaddr-offset-sum ,form-arboffset ,arboffset)
      arboffset)))

(defmacro evo_normal-+t (arg)
  `(mv (ev_normal ,arg) trace))

(define pass-error-+t ((val eval_result-p) &optional (trace 'trace))
  :guard (eval_result-case val :ev_error)
  :inline t
  :enabled t
  (mv (ev_error-fix val) trace))

(defmacro evo_error-+t (&rest args)
  `(pass-error-+t (ev_error . ,args) trace))

(defmacro evo_throwing-+t (&rest args)
  `(mv (ev_throwing . ,args) trace))

(defmacro evo-return-+t (arg)
  `(mv ,arg trace))

(defmacro evtailcall-+t (call)
  `(b* (((evbind-+t res) ,call))
     (evo-return-+t res)))

(acl2::def-b*-binder evbind-+t
  :body
  `(b* (((mv ,(car acl2::args) trace-tmp) . ,acl2::forms)
        (?arboffset (add-arboffset-for-form-+t arboffset . ,acl2::forms))
        (trace (append trace-tmp trace)))
     ,acl2::rest-expr))

(acl2::def-b*-binder evbind-nonrec-+t
  :body
  `(b* (((mv ,(car acl2::args) trace-tmp) . ,acl2::forms)
        (trace (append trace-tmp trace)))
     ,acl2::rest-expr))

(acl2::def-b*-binder evo-+t
  :body
  `(b* ((evresult ,(car acl2::forms)))
     (eval_result-case evresult
       :ev_normal (b* ,(and (not (eq (car acl2::args) '&))
                           `((,(car acl2::args) evresult.res)))
                    ,acl2::rest-expr)
       :otherwise (mv (eval_result-nonnormal-fix evresult) trace))))

(acl2::def-b*-binder evoo-+t
  :body
  `(b* (((evbind-+t evoo-+t-tmp) . ,acl2::forms)
        ((evo-+t ,(car acl2::args)) evoo-+t-tmp))
     (acl2::check-vars-not-free
      (evoo-+t-tmp)
     ,acl2::rest-expr)))

(acl2::def-b*-binder evs-+t
  :body
  `(b* (((evoo-+t cflow) ,(car acl2::forms)))
     (control_flow_state-case cflow
       :returning (evo_normal-+t
                   (mbe :logic (returning cflow.vals cflow.env)
                        :exec cflow))
       :continuing (b* ,(and (not (eq (car acl2::args) '&))
                             `((,(car acl2::args) cflow.env)))
                     ,acl2::rest-expr))))

(acl2::def-b*-binder evob-+t
  :body
  `(b* ((evresult ,(car acl2::forms)))
     (eval_result-case evresult
       :ev_normal (b* ,(and (not (eq (car acl2::args) '&))
                            `((,(car acl2::args) evresult.res)))
                    ,acl2::rest-expr)
       :ev_throwing (mv (init-backtrace
                         (ev_throwing-fix evresult)
                         (local-env->storage (env->local env))
                         pos)
                        trace)
       :otherwise (pass-error-+t
                   (init-backtrace
                    (ev_error-fix evresult)
                    (local-env->storage (env->local env))
                    pos)))))

(defmacro evbody-+t (body)
  `(let ((trace nil))
     ,body))


(acl2::def-b*-binder evoo-+t-special
  :body
  `(b* (((evbind-nonrec-+t evoo-+t-tmp) . ,acl2::forms) ;; don't rebind arboffset
        ((evo-+t ,(car acl2::args)) evoo-+t-tmp))
     (acl2::check-vars-not-free
      (evoo-+t-tmp)
      ,acl2::rest-expr)))

(acl2::def-b*-binder evs-+t-special
  :body
  `(b* (((evoo-+t-special cflow) ,(car acl2::forms)))
     (control_flow_state-case cflow
       :returning (evo_normal-+t
                   (mbe :logic (returning cflow.vals cflow.env)
                        :exec cflow))
       :continuing (b* ,(and (not (eq (car acl2::args) '&))
                             `((,(car acl2::args) cflow.env)))
                     ,acl2::rest-expr))))



(defconsts *eval-*a+t-substitution*
  (list* (cons 'evoo-*a-special 'evoo-+t-special)
         (cons 'evs-*a-special 'evs-+t-special)
         (cons 'add-arboffset-for-form 'add-arboffset-for-form-+t)
         (pair-both-suffixed (append *asl-interp-fns*
                                     '(evo_normal pass-error evo_error evo_throwing evo-return
                                                  evbind evbind-nonrec evoo evo evob evs evtailcall
                                                  asl-interpreter-mutual-recursion))
                             '-*a
                             '-+t)))
  

(local
 (defconst *eval_subprogram-+t-def*
   '(define eval_subprogram-+t ((env env-p)
                                (name identifier-p)
                                (vparams vallist-p)
                                (vargs vallist-p)
                                &key
                                ((clk natp) 'clk)
                                ((ARBADDR ARBADDR-P) 'ARBADDR)
                                ((ARBMAP ARBMAP-P) 'ARBMAP)
                                ((pos posn-p) 'pos)
                                ((static-env static_env_global-p) 'static-env)
                                ((tracespec tracespec-p) 'tracespec))
      :short "Tracing version of @(see eval_subprogram); see @(see
asl-interpreter-mutual-recursion-+t) for overview."
      :guard (equal (global-env->static (env->global env)) static-env)
      :measure (nats-measure clk 1 0 1)
      :returns (mv (res func_eval_result-p)
                   (trace asl-tracelist-p))
      (b* ((ts-entry (find-call-tracespec name pos tracespec))
           ((when (eq (maybe-call-tracespec->abort ts-entry) :before))
            (b* ((trace (call-trace-abort-before-output ts-entry name vparams vargs pos)))
              (pass-error-+t
               (ev_error "Trace abort" ts-entry (list (posn-fix pos))))))
           (tracespec (call-interior-tracespec ts-entry tracespec))
           ((mv res trace) (eval_subprogram-+t1 env name vparams vargs))
           (trace (call-trace-output
                   ts-entry name vparams vargs pos res trace))
           ((when (call-abort-after ts-entry res))
            (pass-error-+t
             (ev_error "Trace abort" ts-entry (list (posn-fix pos))))))
        (mv res trace)))))


(local
 (defconst *eval_stmt-+t-def*
   '(define eval_stmt-+t ((env env-p)
                          (s stmt-p)
                          &key
                          ((clk natp) 'clk)
                          ((arboffset arbaddr-offset-p) 'arboffset)
                          ((ARBADDR ARBADDR-P) 'ARBADDR)
                          ((ARBMAP ARBMAP-P) 'ARBMAP)
                          ((static-env static_env_global-p) 'static-env)
                          ((tracespec tracespec-p) 'tracespec))
      :short "Tracing version of @(see eval_stmt); see @(see
asl-interpreter-mutual-recursion-+t) for overview."
      :guard (equal (global-env->static (env->global env)) static-env)
      :measure (nats-measure clk 0 (stmt-count* s) 1)
      :returns (mv (res stmt_eval_result-p)
                   (trace asl-tracelist-p))
      (b* ((ts-entry (find-stmt-tracespec s tracespec))
           ((when (eq (maybe-stmt-tracespec->abort ts-entry) :before))
            (b* ((trace (stmt-trace-abort-before-output ts-entry env s)))
              (pass-error-+t
               (ev_error "Trace abort" ts-entry (list (stmt->pos_start s))))))
           (tracespec (stmt-interior-tracespec ts-entry tracespec))
           ((mv res trace) (eval_stmt-+t1 env s))
           (trace (stmt-trace-output ts-entry env s res trace))
           ((when (stmt-abort-after ts-entry res))
            (pass-error-+t
             (ev_error "Trace abort" ts-entry (list (stmt->pos_start s))))))
        (mv res trace)))))
   


(local
 (defun add-trace-to-returns (x)
   (if (atom x)
       x
     (case-match x
       ((':returns x . rest)
        `(:returns (mv ,x (trace asl-tracelist-p)) . ,rest))
       (& (cons (add-trace-to-returns (car x))
                (add-trace-to-returns (cdr x))))))))




(local
 (defun replace-static-envs (x)
   (if (atom x)
       x
     (case-match x
       (('global-env->static ('env->global . &) . &) 'static-env)
       (& (cons (replace-static-envs (car x))
                (replace-static-envs (cdr x))))))))


;; (defund-nx is-trace-abort (x)
;;   (equal (ev_error->desc (mv-nth 0 x)) "Trace abort"))

;; (defthm is-trace-abort-when-not-error
;;   (implies (not (eval_result-case (mv-nth 0 x) :ev_error))
;;            (not (is-trace-abort x)))
;;   :hints(("Goal" :in-theory (enable is-trace-abort
;;                                     ev_error->desc-when-wrong-kind))))

;; (defthm is-trace-abort-when-not-error
;;   (implies (not (equal (ev_error->desc (mv-nth 0 x)) "Trace abort"))
;;            (not (is-trace-abort x)))
;;   :hints(("Goal" :in-theory (enable is-trace-abort))))
  


(local
 (defun return-equiv-thm-and-corollaries (name name-mod nonkey-formals key-formals no-expand-name)
   (let* ((thmname (intern-in-package-of-symbol
                   (concatenate 'string "<FN>" "-EQUALS-ORIGINAL")
                   'asl-pkg))
         (name-mod-fn (intern-in-package-of-symbol
                       (concatenate 'string (symbol-name name-mod) "-FN")
                       'asl-pkg))
         (updated-env '(env-replace-static static-env env))
         (updated-nonkey-formals (subst updated-env 'env nonkey-formals)))
     (mv `(defret ,thmname
            (b* (((mv res-mod &) (,name-mod . ,nonkey-formals))
                 (res ;; (let ((env (env-replace-static static-env env)))
                  (,name . ,updated-nonkey-formals)))
              (implies (not (and (eval_result-case res-mod :ev_error)
                                 (equal (ev_error->desc res-mod) "Trace abort")))
                       (equal res-mod res)))
            :hints ((let ((expand (acl2::just-expand-cp-parse-hints
                                   '((:free (,@nonkey-formals clk) (,name-mod . ,nonkey-formals))
                                     ,@(and (not no-expand-name)
                                            `((:free (,@nonkey-formals clk) (,name . ,nonkey-formals)))))
                                   world)))
                      `(:computed-hint-replacement
                        ((acl2::expand-marked))
                        :clause-processor (acl2::mark-expands-cp
                                           clause
                                           '(t ;; last-only
                                             t ;; lambdas
                                             ,expand))
                        :do-not-induct t)))
            ;; :rule-classes nil
            :fn ,name-mod)
         `((defret ,(intern-in-package-of-symbol
                     (concatenate 'string "<FN>" "-EQUALS-ORIGINAL-KIND")
                     'asl-pkg)
             (b* (((mv res-mod &) (,name-mod . ,nonkey-formals))
                  (res ;; (let ((env (env-replace-static static-env env)))
                   (,name . ,updated-nonkey-formals)))
               (implies (and (syntaxp (or (acl2::rewriting-negative-literal-fn
                                           `(equal (eval_result-kind$inline (mv-nth '0 (,',name-mod-fn . ,,(xxxjoin 'cons (append nonkey-formals key-formals '('nil)))))) ,kind) mfc state)
                                          (acl2::rewriting-negative-literal-fn
                                           `(equal ,kind (eval_result-kind$inline (mv-nth '0 (,',name-mod-fn . ,,(xxxjoin 'cons (append nonkey-formals key-formals '('nil))))))) mfc state)))
                             (not (equal kind :ev_error)))
                        (iff (equal (eval_result-kind res-mod) kind)
                             (and (equal (eval_result-kind res) kind)
                                  (equal res-mod res)
                                  (equal (eval_result-kind (hide res-mod)) kind)))))
             :hints (("goal" :use ,thmname
                      :in-theory (disable ,thmname)
                      :expand ((:free (x) (hide x)))))
             ;; :rule-classes nil
             :fn ,name-mod)

           (defret ,(intern-in-package-of-symbol
                     (concatenate 'string "<FN>" "-EQUALS-ORIGINAL-DESC")
                     'asl-pkg)
             (b* (((mv res-mod &) (,name-mod . ,nonkey-formals))
                  (res ;; (let ((env (env-replace-static static-env env)))
                   (,name . ,updated-nonkey-formals)))
               (implies (and (syntaxp (or (acl2::rewriting-positive-literal-fn
                                           `(equal (ev_error->desc$inline (mv-nth '0 (,',name-mod-fn . ,,(xxxjoin 'cons (append nonkey-formals key-formals '('nil)))))) '"Trace abort") mfc state)
                                          (acl2::rewriting-positive-literal-fn
                                           `(equal '"Trace abort" (ev_error->desc$inline (mv-nth '0 (,',name-mod-fn . ,,(xxxjoin 'cons (append nonkey-formals key-formals '('nil))))))) mfc state))))
                        (iff (equal (ev_error->desc res-mod) "Trace abort")
                             (not (and (equal res-mod res)
                                       (not (equal (ev_error->desc (hide res-mod)) "Trace abort")))))))
             :hints (("goal" :use ,thmname
                      :in-theory (disable ,thmname)
                      :expand ((:free (x) (hide x)))))
             :fn ,name-mod))))))


(local
 (defun eval-return-equiv-thms (names suffix wrld)
   (if (atom names)
       (mv nil nil)
     (b* (((mv main-thms corollaries)
           (eval-return-equiv-thms (cdr names) suffix wrld))
          (name (car names))
          (name1 (intern-in-package-of-symbol
                  (concatenate 'string (symbol-name name) "-*A")
                  name))
          (name-mod (intern-in-package-of-symbol
                     (concatenate 'string (symbol-name name) "-"
                                  (if (or (eq name 'eval_subprogram)
                                          (eq name 'eval_stmt))
                                      (concatenate 'string (symbol-name suffix) "1")
                                    (symbol-name suffix)))
                     name))
          (macro-args (macro-args name1 wrld))
          (nonkey-formals (take (- (len macro-args)
                                   (len (member '&key macro-args)))
                                macro-args))
          (key-formals (append
                        (strip-cars (nthcdr (+ 1 (- (len macro-args)
                                                    (len (member '&key macro-args))))
                                            macro-args))
                        '(static-env tracespec)))
          ((mv thm corrs)
           (return-equiv-thm-and-corollaries name1 name-mod nonkey-formals key-formals nil)))
       (mv (cons
            thm main-thms)
           (append corrs corollaries))))))

(local
 (defun equals-original-thm (suffix wrld)
   (b* ((eval_subprogram-mod (intern-in-package-of-symbol
                              (concatenate 'string "EVAL_SUBPROGRAM-" (symbol-name suffix))
                              'eval_subprogram))
        (eval_stmt-mod (intern-in-package-of-symbol
                        (concatenate 'string "EVAL_STMT-" (symbol-name suffix))
                        'eval_stmt))
        ((mv main-thms corollaries)
         (eval-return-equiv-thms *asl-interp-fns* suffix wrld))
        ((mv eval_sub-main-thm eval_sub-corrs)
         (return-equiv-thm-and-corollaries
          'eval_subprogram-*a eval_subprogram-mod '(env name vparams vargs)
          '(clk arbaddr arbmap static-env tracespec) t))
        ((mv eval_stmt-main-thm eval_stmt-corrs)
         (return-equiv-thm-and-corollaries
          'eval_stmt-*a eval_stmt-mod '(env s)
          '(clk arboffset arbaddr arbmap static-env tracespec) t)))
     `(encapsulate nil
        (local (deflabel before-equals-original))
        (std::defret-mutual
          ,(intern-in-package-of-symbol
            (concatenate 'string (symbol-name suffix) "-EQUALS-ORIGINAL")
            'asl-pkg)
          ,eval_sub-main-thm
          ,eval_stmt-main-thm
          . ,main-thms)
        ,@eval_sub-corrs
        ,@eval_stmt-corrs
        ,@corollaries
        (acl2::def-ruleset! asl-+t-equals-original-rules
          (set-difference-theories (current-theory :here)
                                   (current-theory 'before-equals-original)))))))









(local (in-theory (disable (tau-system)
                           len assoc-equal append true-listp loghead hons-assoc-equal floor mod expt take
                           acl2::repeat)))

;; Assumptions about the syntax of the interpeter definition form:
;;  - Only the one occurrence of ///
;;  - No auxiliary functions defined in :prepwork
;;  - Each define form has its body last (after all keyword args).




(local (defconst *asl-+t-xdoc*
         '(:parents (asl-tracing)
           :short "Modified version of @(see asl-interpreter-mutual-recursion-*a) that collects a
trace of a specified set of subprogram calls."
           :long "
<p>This is an automatically generated derived version of the ASL interpreter,
@(see asl-interpreter-mutual-recursion). Each function in the original mutual
recursion has an analogous function in this version, suffixed with
@('-+t') (the \"tracing version\" of the function).  The tracing version of
each function takes the same arguments as the original version, plus an
additional keyword argument @('tracespec'), of @(see tracespec) type, which
determines what events (calls and statements) are traced. The tracing version
of each function also returns the same (two) values as the original function,
plus a third of type @(see asl-tracelist) giving the trace data from events
within that call.</p>

<p>The first two return values of the tracing version of each function are
provably the same as those from the original function, except when the tracing
has caused an abort due to the @(':abort') field in a triggered @(see
call-tracespec) or @(see stmt-tracespec). In that case the result is an
@('ev_error') with descriptor \"Trace abort\"; otherwise, the result output
is the same as from the original functions. This is proved in theorems
@('eval_expr-+t-equals-original'), etc.</p>

<p>The functions @(see eval_subprogram-+t) and @(see eval_stmt-+t) are special
in that they perform the collection of trace data for calls and statements,
respectively. They are implemented as wrappers around the autogenerated
versions @(see eval_subprogram-+t1) and @(see eval_stmt-+t1).</p>")))




(local (xdoc::set-default-parents asl-interpreter-mutual-recursion-+t))

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


(local (defthm if-t-nil
         (and (equal (if t x y) x)
              (equal (if nil x y) y))))


(with-output
    ;; makes it so it won't take forever to print the induction scheme
    :evisc (:gag-mode (evisc-tuple 3 4 nil nil))
    :off (event)
    (make-event (append *static-env-preserved-form*
                        '(:hints ((vl::big-mutrec-default-hint 'eval_expr-*a-fn id nil world))
                          :mutual-recursion asl-interpreter-mutual-recursion-*a))))


(encapsulate nil
  (local (in-theory (disable (:t eval_result-kind)
                             (:t append)
                             (:t pass-error)
                             (:t val-kind)
                             (:t acl2::true-listp-append)
                             (:t eval_expr)
                             (:t expr_result->env)
                             (:t expr_result->val)
                             (:t ev_error)
                             (:t pass-error-+t)
                             (:t ev_normal)
                             default-car
                             default-cdr
                             static_env_global-fix-when-static_env_global-p
                             ;; global-replace-static-with-self
                             ;; env-replace-static-with-self
                             )))

  ;; ---------------------------------------------------------------------------
  ;; Definition of the Tracing ASL Interpreter (suffixed with *t)
  (with-output
    :off (event)
    (make-event
     (b* ((form *asl-interpreter-mutual-recursion-*a-form*)
          ;; Strip out the events after the /// (theorem about resolved-p-of-resolve-ty)
          (form (strip-post-/// form))
          ;; Strip out xdoc
          (form (strip-xdoc form))
          ;; Add xdoc topic for mutual recursion
          (form (add-mutrec-xdoc *asl-+t-xdoc* form))
          ;; Add xdoc topic for each function
          (form (add-define-xdoc
                 "Tracing/arbvals version of @(see <NAME>); see @(see asl-interpreter-mutual-recursion-+t) for overview."
                 form))
          ;; Substitute function names with their -+t suffixed forms.
          (form (sublis *eval-*a+t-substitution* form))
          ;; Replace '(define eval_subprogram ...' with '(define eval_subprogram-*ft1'
          ;; since it's going to be wrapped in a call that deals with collecting the trace data.
          (form (find-def-and-rename 'eval_subprogram-+t "1" form))
          (form (find-def-and-rename 'eval_stmt-+t "1" form))
          ;; Replace all invocations of (global-env->static (env->global env)) with the variable static-env.
          (form (replace-static-envs form))
          ;; Add guard saying static-env equals the one in env.
          (form (add-define-guard '(equal (global-env->static (env->global env)) static-env) form))
          ;; Wrap each define body in a call of evbody-+t.
          (form (wrap-define-bodies 'evbody-+t form))
          (form (wrap-define-bodies 'bind-env-with-static form))
          ;; Add (trace asl-tracelist-p) to all the :returns forms.
          (form (add-trace-to-returns form))
          ;; Replace all invocations of (global-env->static (env->global env)) with the variable static-env.
          ;; (form (replace-static-envs form))
          ;; Add the tracespec formal to each define form.
          (form (add-define-formals '(((static-env static_env_global-p) 'static-env)
                                      ((tracespec tracespec-p) 'tracespec)) form))
          ;; Add the definition of eval_subprogram-+t which wraps around eval_subprogram-+t1.
          (form (add-define-to-defines *eval_subprogram-+t-def* form))
          (form (add-define-to-defines *eval_stmt-+t-def* form))
          ;; Disable the functions, prove the non-trace return values equal to the originals, and verify guards.
          (form (insert-after-///
                 (list
                  '(make-event
                    `(in-theory (disable . ,(fgetprop 'eval_expr-+t-fn 'acl2::recursivep nil (w state)))))
                  (equals-original-thm '+t (w state))
                  ;; '(verify-guards eval_expr-+t-fn)
                  )
                 form)))
       `(progn (defconst *asl-interpreter-mutual-recursion-+t-form* ',form)
               ,form)))))
;; ---------------------------------------------------------------------------

;; (local (acl2::use-trivial-ancestors-check))

(with-output
  ;; makes it so it won't take forever to print the induction scheme
  :evisc (:gag-mode (evisc-tuple 3 4 nil nil))
  :off (event)
  (encapsulate nil
    (local (in-theory (acl2::disable*
                       xor not
                       ;; asl-+t-equals-original-ruleso
                       )))

    (local
     (defthm true-listp-when-asl-tracelist-p
       (implies (asl-tracelist-p x)
                (true-listp x))
       :rule-classes ((:rewrite :backchain-limit-lst 1))))


    (std::defret-mutual resolved-p-of-resolve-ty
      (defret resolved-p-of-<fn>
        (implies (eval_result-case res :ev_normal)
                 (int_constraintlist-resolved-p (ev_normal->res res)))
        :hints ('(:expand (<call>)
                  :in-theory (enable int_constraintlist-resolved-p
                                     int_constraint-resolved-p
                                     int-literal-expr-p)))
        :fn resolve-int_constraints-*a)
      (defret resolved-p-of-<fn>
        (implies (eval_result-case res :ev_normal)
                 (constraint_kind-resolved-p (ev_normal->res res)))
        :hints ('(:expand (<call>)
                  :in-theory (enable constraint_kind-resolved-p)))
        :fn resolve-constraint_kind-*a)

      (defret resolved-p-of-<fn>
        (implies (eval_result-case res :ev_normal)
                 (tylist-resolved-p (ev_normal->res res)))
        :hints ('(:expand (<call>)
                  :in-theory (enable tylist-resolved-p)))
        :fn resolve-tylist-*a)

      (defret resolved-p-of-<fn>
        (implies (eval_result-case res :ev_normal)
                 (typed_identifierlist-resolved-p (ev_normal->res res)))
        :hints ('(:expand (<call>)
                  :in-theory (enable typed_identifierlist-resolved-p
                                     typed_identifier-resolved-p)))
        :fn resolve-typed_identifierlist-*a)

      (defret resolved-p-of-<fn>
        (implies (eval_result-case res :ev_normal)
                 (ty-resolved-p (ev_normal->res res)))
        :hints ('(:expand ((:free (clk) <call>))
                  :in-theory (enable ty-resolved-p
                                     int-literal-expr-p))
                (and stable-under-simplificationp
                     '(:expand ((ty-resolved-p x)))))
        :fn resolve-ty-*a)
      :skip-others t
      :mutual-recursion asl-interpreter-mutual-recursion-*a)

  (std::defret-mutual len-of-eval_expr_list-*a
    (defret len-of-eval_expr_list-*a
      (implies (eval_result-case res :ev_normal)
               (equal (len (exprlist_result->val (ev_normal->res res)))
                      (len e)))
      :hints ('(:expand ((eval_expr_list-*a env e))))
      :fn eval_expr_list-*a)
    :mutual-recursion asl-interpreter-mutual-recursion-*a
    :skip-others t)
  
  (verify-guards eval_expr-+t-fn
    :guard-debug t
    ;; :hints ((and stable-under-simplificationp
    ;;              '(:in-theory (acl2::enable*
    ;;                            asl-+t-equals-original-rules))))
    )))


;; (find-define 'eval_subprogram-+t *asl-interpreter-mutual-recursion-+t-form*)


(defmacro trace-eval_expr-+t ()
  '(trace$ (eval_expr-+t-fn :entry (list 'eval_expr-+t e)
                         :exit (cons 'eval_expr-+t
                                     (let ((value (car values)))
                                       (eval_result-case value
                                         :ev_normal (list 'ev_normal (expr_result->val value.res))
                                         :ev_error value
                                         :ev_throwing (list 'ev_throwing value.throwdata)))))))

(defmacro trace-eval_stmt-+t (&key locals)
  `(trace$ (eval_stmt-+t-fn :entry (list 'eval_stmt-+t s
                                      . ,(and locals '((local-env->storage (env->local env)))))
                         :exit (cons 'eval_stmt-+t
                                     (let ((value (car values)))
                                       (eval_result-case value
                                         :ev_normal (cons 'ev_normal
                                                          (control_flow_state-case value.res
                                                            :returning `(:returning ,value.res.vals)
                                                            :continuing ,(if locals
                                                                             `(list :continuing (local-env->storage (env->local value.res.env)))
                                                                           ''(:continuing))))
                                         :ev_error value
                                         :ev_throwing (list 'ev_throwing
                                                            value.throwdata
                                                            . ,(and locals '((local-env->storage (env->local value.env)))))))))))


(defmacro trace-eval_subprogram-+t (&optional (evisc-tuple '(nil 7 12 nil)))
  `(trace$ (eval_subprogram-+t-fn :entry (list 'eval_subprogram-+t name vparams vargs)
                               :exit (list 'eval_subprogram-+t
                                           name
                                           (let ((value (car values)))
                                             (eval_result-case value
                                               :ev_normal (b* (((func_result value.res)))
                                                            (list 'ev_normal value.res.vals))
                                               :otherwise value)))
                               :evisc-tuple ',evisc-tuple)))





(encapsulate nil
  (local (in-theory (acl2::e/d* (ev_error->desc-when-wrong-kind
                                 call-abort-after
                                 stmt-abort-after
                                 call-interior-tracespec
                                 stmt-interior-tracespec
                                 call-trace-output
                                 stmt-trace-output)
                                (asl-+t-equals-original-rules
                                 tracespec-emptyp-implies
                                 not xor atom eql
                                 env-replace-static-with-self
                                 eval_result-fix-when-eval_result-p
                                 acl2::append-to-nil
                                 eval_result-kind-possibilities
                                 (tau-system)
                                 (:rules-of-class :congruence :here)
                                 (:rules-of-class :type-prescription :here)))))
  (local (include-book "trace-aborts"))

  (with-output
    ;; makes it so it won't take forever to print the induction scheme
    :evisc (:gag-mode (evisc-tuple 3 4 nil nil))
    (std::defret-mutual-generate <fn>-no-trace-when-empty-tracespec
      :rules ((t (:add-hyp (tracespec-emptyp tracespec))
                 (:add-concl (not trace))
                 (:add-concl (not (equal (ev_error->desc res) "Trace abort")))))
      :hints ((vl::big-mutrec-default-hint 'eval_expr-+t-fn id nil world))
      :mutual-recursion asl-interpreter-mutual-recursion-+t)))


(make-event
 `(defthm eval_subprogram-+t-without-trace-independent-of-pos
    (implies (and (syntaxp (not (equal pos '',*dummy-position*)))
                  (tracespec-emptyp tracespec))
             (equal (eval_subprogram-+t env name vparams vargs)
                    (eval_subprogram-+t env name vparams vargs :pos *dummy-position*)))
    :hints(("Goal" :expand ((:free (pos) (eval_subprogram-+t env name vparams vargs)))
            :in-theory (enable call-trace-output
                               call-abort-after)))))

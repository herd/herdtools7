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

(include-book "../../openers-trace")
(include-book "stdlib")
(include-book "../../proof-utils")
(include-book "../../static-env-preserved-trace")

(local (include-book "ast-theory"))

(local (include-book "std/lists/sets" :dir :system))

(define call-tracespec-excludes-stdlib-fns ((fns identifierlist-p)
                                            (x call-tracespec-p))
  :returns (ok)
  (b* (((call-tracespec x)))
    (and x.fn
         (not (member-equal x.fn (identifierlist-fix fns)))))
  ///
  (defret check-call-tracespec-when-<fn>
    (implies (and ok
                  (member-equal (identifier-fix fn) (identifierlist-fix fns)))
             (not (check-call-tracespec fn pos x)))
    :hints(("Goal" :in-theory (enable check-call-tracespec))))

  (defret <fn>-of-subset
    (implies (and (call-tracespec-excludes-stdlib-fns fns1 x)
                  (subsetp-equal (identifierlist-fix fns)
                                 (identifierlist-fix fns1)))
             ok)))

(define call-tracespeclist-excludes-stdlib-fns ((fns identifierlist-p)
                                                (x call-tracespeclist-p))
  :returns (ok)
  (if (atom x)
      t
    (and (call-tracespec-excludes-stdlib-fns fns (car x))
         (call-tracespeclist-excludes-stdlib-fns fns (cdr x))))
  ///
  (defret call-tracespeclist-find-when-<fn>
    (implies (and ok
                  (member-equal (identifier-fix fn) (identifierlist-fix fns)))
             (not (call-tracespeclist-find fn pos x)))
    :hints(("Goal" :in-theory (enable call-tracespeclist-find))))
  
  (defret <fn>-of-subset
    (implies (and (call-tracespeclist-excludes-stdlib-fns fns1 x)
                  (subsetp-equal (identifierlist-fix fns)
                                 (identifierlist-fix fns1)))
             ok)))

(define stmt-tracespec-excludes-stdlib ((x stmt-tracespec-p))
  :returns (ok)
  (b* (((stmt-tracespec x)))
    (and x.fname
         (not (equal x.fname "ASL Standard Library"))
         (not (equal x.fname "ASL Standard Library (V0 compatibility)"))))
  ///
  (defret check-stmt-tracespec-when-<fn>
    (implies (and ok
                  (or (equal (posn->fname (stmt->pos_start stmt)) "ASL Standard Library")
                      (equal (posn->fname (stmt->pos_start stmt)) "ASL Standard Library (V0 compatibility)")))
             (not (check-stmt-tracespec stmt x)))
    :hints(("Goal" :in-theory (enable check-stmt-tracespec)))))

(define stmt-tracespeclist-excludes-stdlib ((x stmt-tracespeclist-p))
  :returns (ok)
  (if (atom x)
      t
    (and (stmt-tracespec-excludes-stdlib (car x))
         (stmt-tracespeclist-excludes-stdlib (cdr x))))
  ///
  (defret stmt-tracespeclist-find-when-<fn>
    (implies (and ok
                  (or (equal (posn->fname (stmt->pos_start stmt)) "ASL Standard Library")
                      (equal (posn->fname (stmt->pos_start stmt)) "ASL Standard Library (V0 compatibility)")))
             (not (stmt-tracespeclist-find stmt x)))
    :hints(("Goal" :in-theory (enable stmt-tracespeclist-find)))))

(define tracespec-excludes-stdlib-fns ((fns identifierlist-p)
                                       (x tracespec-p))
  :returns (ok)
  (b* (((tracespec x)))
    (and (call-tracespeclist-excludes-stdlib-fns fns x.call-specs-permanent)
         (call-tracespeclist-excludes-stdlib-fns fns x.call-specs-transient)
         (stmt-tracespeclist-excludes-stdlib x.stmt-specs-permanent)
         (stmt-tracespeclist-excludes-stdlib x.stmt-specs-transient)))
  ///
  (defret find-call-tracespec-when-<fn>
    (implies (and ok
                  (member-equal (identifier-fix fn) (identifierlist-fix fns)))
             (not (find-call-tracespec fn pos x)))
    :hints(("Goal" :in-theory (enable find-call-tracespec))))

  (defret find-stmt-tracespec-when-<fn>
    (implies (and ok
                  (or (equal (posn->fname (stmt->pos_start stmt)) "ASL Standard Library")
                      (equal (posn->fname (stmt->pos_start stmt)) "ASL Standard Library (V0 compatibility)")))
             (not (find-stmt-tracespec stmt x)))
    :hints(("Goal" :in-theory (enable find-stmt-tracespec))))

  (defret find-call-tracespec-when-<fn>-rw
    (implies (and (tracespec-excludes-stdlib-fns fns other)
                  ok
                  (member-equal (identifier-fix fn) (identifierlist-fix fns)))
             (not (find-call-tracespec fn pos x)))
    :hints(("Goal" :in-theory (enable find-call-tracespec))))

  (defret find-stmt-tracespec-when-<fn>-rw
    (implies (and (tracespec-excludes-stdlib-fns fns other)
                  ok
                  (or (equal (posn->fname (stmt->pos_start stmt)) "ASL Standard Library")
                      (equal (posn->fname (stmt->pos_start stmt)) "ASL Standard Library (V0 compatibility)")))
             (not (find-stmt-tracespec stmt x)))
    :hints(("Goal" :in-theory (enable find-stmt-tracespec))))

  (defret <fn>-of-combine-tracespecs
    (implies ok
             (tracespec-excludes-stdlib-fns fns (combine-tracespecs callp nil x)))
    :hints(("Goal" :in-theory (enable combine-tracespecs)
            :expand ((call-tracespeclist-excludes-stdlib-fns fns nil)))))

  (defret <fn>-of-subset
    (implies (and (tracespec-excludes-stdlib-fns fns1 x)
                  (subsetp-equal (identifierlist-fix fns)
                                 (identifierlist-fix fns1)))
             ok)))



(defret ev_error->desc-of-init-backtrace
  (implies (eval_result-case x :ev_error)
           (equal (ev_error->desc (init-backtrace x pos))
                  (ev_error->desc x)))
  :hints(("Goal" :in-theory (enable init-backtrace)))
  :fn init-backtrace)


                         





(defund not-trace (x)
  (atom x))

(defund-nx no-trace-p (x)
  (not-trace (mv-nth 2 x)))

(defthm no-trace-p-of-mv
  (equal (no-trace-p (list* res orac traces tail))
         (not-trace traces))
  :hints(("Goal" :in-theory (enable no-trace-p))))

(defthm not-trace-of-mv-nth
  (equal (not-trace (mv-nth 2 x))
         (no-trace-p x))
  :hints(("Goal" :in-theory (enable no-trace-p))))

(defthm no-trace-p-of-if
  (equal (no-trace-p (if x y z))
         (if x (no-trace-p y) (no-trace-p z))))

(defthm not-trace-of-append
  (equal (not-trace (append x y))
         (and (not-trace x)
              (not-trace y)))
  :hints(("Goal" :in-theory (enable not-trace))))

(defund not-trace-abort-p (x)
  (not (equal (ev_error->desc x) "Trace abort")))

(defund-nx no-trace-abort-p (x)
  (not-trace-abort-p (mv-nth 0 x)))

(defthm no-trace-abort-p-of-mv
  (equal (no-trace-abort-p (list* res tail))
         (not-trace-abort-p res))
  :hints(("Goal" :in-theory (enable no-trace-abort-p))))

(defthm not-trace-abort-p-of-mv-nth
  (equal (not-trace-abort-p (mv-nth 0 x))
         (no-trace-abort-p x))
  :hints(("Goal" :in-theory (enable no-trace-abort-p))))

(defthm no-trace-abort-p-of-if
  (equal (no-trace-abort-p (if x y z))
         (if x (no-trace-abort-p y) (no-trace-abort-p z))))

(defthm not-trace-abort-of-eval_binop
  (not-trace-abort-p (eval_binop op val1 val2))
  :hints(("Goal" :in-theory (enable eval_binop
                                    ev_error->desc-when-wrong-kind
                                    not-trace-abort-p))))

(defthm not-trace-abort-of-eval_unop
  (not-trace-abort-p (eval_unop op val1))
  :hints(("Goal" :in-theory (enable eval_unop
                                    ev_error->desc-when-wrong-kind
                                    not-trace-abort-p))))

(defthm not-trace-abort-of-check-bad-slices
  (not-trace-abort-p (check-bad-slices width slices))
  :hints(("Goal" :in-theory (enable check-bad-slices
                                    ev_error->desc-when-wrong-kind
                                    not-trace-abort-p))))

(defthm not-trace-abort-of-ev_normal
  (not-trace-abort-p (ev_normal res))
  :hints(("Goal" :in-theory (enable not-trace-abort-p
                                    ev_error->desc-when-wrong-kind))))

(defthm not-trace-abort-of-ev_throwing
  (not-trace-abort-p (ev_throwing x y z))
  :hints(("Goal" :in-theory (enable not-trace-abort-p
                                    ev_error->desc-when-wrong-kind))))

(defthm not-trace-abort-of-ev_error
  (implies (syntaxp (not (case-match desc
                           (('mv-nth ''0 &) t)
                           (& nil))))
           (equal (not-trace-abort-p (ev_error desc data bt))
                  (not (equal (acl2::str-fix desc) "Trace abort"))))
  :hints(("Goal" :in-theory (enable not-trace-abort-p))))

(defthm not-trace-abort-of-ev_error-mv-nth-0
  (equal (not-trace-abort-p (ev_error (ev_error->desc (mv-nth 0 x)) data bt))
         (no-trace-abort-p x))
  :hints(("Goal" :in-theory (e/d (not-trace-abort-p
                                  no-trace-abort-p)
                                 (not-trace-abort-p-of-mv-nth)))))

(defthm not-trace-abort-p-of-if
  (equal (not-trace-abort-p (if x y z))
         (if x (not-trace-abort-p y) (not-trace-abort-p z))))


;; (DEFTHM EVAL_SUBPROGRAM-*T-STATIC-ENV-PRESERVED-force
;;   (B* (((MV ?RES ?NEW-ORAC COMMON-LISP::?TRACE)
;;         (EVAL_SUBPROGRAM-*T-FN
;;          ENV NAME
;;          VPARAMS VARGS CLK ORAC POS TRACESPEC)))
;;     (AND
;;      (IMPLIES
;;       (force (EVAL_RESULT-CASE RES :EV_NORMAL))
;;       (EQUAL (GLOBAL-ENV->STATIC
;;               (FUNC_RESULT->ENV (EV_NORMAL->RES RES)))
;;              (GLOBAL-ENV->STATIC (ENV->GLOBAL ENV))))
;;      (IMPLIES
;;       (force (EVAL_RESULT-CASE RES :EV_THROWING))
;;       (EQUAL
;;        (GLOBAL-ENV->STATIC (ENV->GLOBAL (EV_THROWING->ENV RES)))
;;        (GLOBAL-ENV->STATIC (ENV->GLOBAL ENV))))))
;;   :HINTS
;;   (("Goal"
;;     :use eval_subprogram-*t-static-env-preserved
;;     :in-theory (disable eval_subprogram-*t-static-env-preserved))))



(defmacro force? (x)
  ;; `(force ,x)
  x)

(acl2::add-to-ruleset asl-code-proof-enables env-find-vars)

(defconst *def-asl-no-trace-template*
  '(progn
     (LOCAL (IN-THEORY (ACL2::E/D* (
                                    trace-openers
                                    open-eval_stmt-*t1-no-cond
                                    open-eval_expr-*t-no-cond
                                    ASL-CODE-PROOF-ENABLES
                                    <user-enables>)
                                   (ASL-CODE-PROOF-DISABLES
                                    open-eval_stmt-*t1
                                    open-eval_expr-*t
                                    ;; env-find
                                    ;; check_recurse_limit
                                    ;; env-push-stack
                                    ;; env-pop-stack
                                    ;; open-eval_binop
                                    ;; open-eval_unop
                                    <user-disables>))))
     (defthm <name>-no-trace
       (implies (and (force? (subprograms-match '<subprograms>
                                        (global-env->static (env->global env))
                                        <static-env>))
                     (force? (tracespec-excludes-stdlib-fns '<subprograms> tracespec)))
                (no-trace-p (eval_subprogram-*t env <fn> params args)))
       :hints (("goal" :expand ((eval_subprogram-*t env <fn> params args)
                                ;; :lambdas
                                ))))

     (defthm <name>-no-trace-rw
       (implies (and (force? (subprograms-match '<subprograms>
                                               (global-env->static (env->global env))
                                               <static-env>))
                     (force? (tracespec-excludes-stdlib-fns '<subprograms> tracespec)))
                (equal (mv-nth 2 (eval_subprogram-*t env <fn> params args)) nil))
       :hints (("goal" :use (<name>-no-trace
                             (:instance return-type-of-eval_subprogram-*t.trace
                              (name <fn>) (vparams params) (vargs args)))
                :in-theory (e/d (no-trace-p not-trace)
                                (<name>-no-trace
                                 not-trace-of-mv-nth
                                 return-type-of-eval_subprogram-*t.trace)))))

     (defthm <name>-no-trace-abort
       (implies (and (force? (subprograms-match '<subprograms>
                                               (global-env->static (env->global env))
                                               <static-env>))
                     (force? (tracespec-excludes-stdlib-fns '<subprograms> tracespec)))
                (no-trace-abort-p (eval_subprogram-*t env <fn> params args)))
       :hints (("goal" :expand ((eval_subprogram-*t env <fn> params args)
                                ;; :lambdas
                                ))))

     (defthm <name>-no-trace-abort-rw
       (implies (and (force? (subprograms-match '<subprograms>
                                               (global-env->static (env->global env))
                                               <static-env>))
                     (force? (tracespec-excludes-stdlib-fns '<subprograms> tracespec)))
                (not (equal (ev_error->desc (mv-nth 0 (eval_subprogram-*t env <fn> params args)))
                            "Trace abort")))
       :hints (("goal" :use <name>-no-trace-abort
                :in-theory (e/d (no-trace-abort-p not-trace-abort-p)
                                (<name>-no-trace-abort
                                 not-trace-abort-p-of-mv-nth)))))
     
     (:@ :table-update
      (table <table-name>
             <fn> (list '<subprograms>
                        '<direct-subprograms>
                        nil
                        '<name>)))))




(define def-asl-no-trace-fn (function args state)
  :mode :program
  (b* (((std::extract-keyword-args
         :other-args bad-args
         :allowed-keys '(:prepwork)
         ;; :kwd-alist kwd-alist
         ;; function
         ;; safe-clock
         
         ;; return-values
         ;; (hyps 't)
         
         enable
         disable
         hints
         no-expand-hint
         ;; prepwork

         ;; ;; Either provide normal-cond/nonnormal-res,
         ;; ;; no normal/error/throwing-conds,
         ;; ;; or no normal-cond but error and or throwing-conds.
         ;; normal-cond
         ;; nonnormal-res
         ;; error-cond
         ;; error-res
         ;; throwing-cond
         ;; throwing-res
         
         (static-env '(stdlib-static-env)))
        args)

       ((unless (stringp function))
        (er soft 'def-asl-no-trace "Function name must be a string"))

       (name (if (equal static-env '(stdlib-static-env))
                 (intern-in-package-of-symbol (str::upcase-string function) 'asl-pkg)
               (intern-in-package-of-symbol (concatenate 'string (str::upcase-string function) "-PRIM") 'asl-pkg)))
       
       ((when bad-args)
        (er soft 'def-asl-no-trace "Bad arguments: ~x0" bad-args))
       ((unless (stringp function))
        (er soft 'def-asl-no-trace "Function should be a string: ~x0" function))
       ((acl2::er (cons & static-env-val))
        (acl2::simple-translate-and-eval static-env nil nil
                                         (msg "static env ~x0" static-env)
                                         'def-asl-no-trace (w state) state t))
       ((unless (static_env_global-p static-env-val))
        (er soft 'def-asl-no-trace "Bad static env (evaluation of ~x0): doesn't satisfy static_env_global-p" static-env))
       (fn-struct (cdr (hons-assoc-equal function
                                         (static_env_global->subprograms static-env-val))))
       ((unless fn-struct)
        (er soft 'def-asl-no-trace "Bad function ~x0: not found in static env" function))
       ((func-ses fn-struct))
       ((func f) fn-struct.fn)
       ;; (primitivep (subprogram_body-case f.body :sb_primitive))

       (direct-subprograms (collect-direct-subprograms f.body nil))
       (table-name (if (equal static-env '(stdlib-static-env))
                       'asl-no-trace-subprogram-table
                     'asl-no-trace-prim-subprogram-table))
       (table  (table-alist table-name (w state)))
       (subprograms (cons function (collect-transitive-subprograms direct-subprograms table nil)))
       (table-update t)
       
       (template (acl2::make-tmplsubst
                  :atoms `((<name> . ,name)
                           (<subprograms> . ,subprograms)
                           (<direct-subprograms> . ,direct-subprograms)
                           (<static-env> . ,static-env)
                           (<fn> . ,function)
                           (<table-name> . ,table-name))
                  :strs `(("<NAME>" . ,(symbol-name name)))
                  :pkg-sym 'asl-pkg
                  :splices `((<user-enables> . ,enable)
                             (<user-disables> . ,disable)
                             (<hints> . ,hints))
                  :features (append (and no-expand-hint '(:no-expand-hint))
                                    (and table-update '(:table-update))))))

    (value (acl2::template-subst-top *def-asl-no-trace-template* template))))




(defmacro def-asl-no-trace (function &rest args)
  (let* ((prepwork (cadr (assoc-keyword :prepwork args))))
    `(defsection ,(intern-in-package-of-symbol
                   (concatenate 'string (str::upcase-string function) "-NO-TRACE")
                   'asl-pkg)
       ,@prepwork
       (make-event (def-asl-no-trace-fn ',function ',args state)))))



;; (local
;;  (defthm eval_expr-*t-no-trace-base-cases
;;    (implies (member-eq (expr_desc-kind (expr->desc e)) '(:e_literal :e_var))
;;             (and (equal (mv-nth 2 (eval_expr-*t env e)) nil)
;;                  (b* ((res (mv-nth 0 (eval_expr-*t env e))))
;;                    (implies (eval_result-case res :ev_error)
;;                             (not (equal (ev_error->desc res) "Trace abort"))))))
;;    :hints (("goal" :expand ((eval_expr-*t env e))))))


;; (defopener open-eval_expr-*t-1 eval_expr-*t :hyp (and (syntaxp (quotep e))
;;                                                       (not (member-eq (expr_desc-kind (expr->desc e)) '(:e_literal :e_var)))))


(local (in-theory (acl2::disable* (:ruleset asl-*t-equals-original-rules)
                                  floor
                                  len
                                  take
                                  omap::assoc-when-emptyp
                                  emptyp-of-val-imap-fix-to-not-val-imap-or-emptyp
                                  omap::assoc-when-assoc-tail
                                  logapp
                                  loghead
                                  logand logior ash logtail lognot logbitp
                                  append)))


(local (defthm ev_error->desc-of-ev_normal
         (equal (ev_error->desc (ev_normal x)) "")
         :hints(("Goal" :in-theory (enable ev_error->desc-when-wrong-kind)))))

(local (defthm ev_error->desc-of-ev_throwing
         (equal (ev_error->desc (ev_throwing x y z)) "")
         :hints(("Goal" :in-theory (enable ev_error->desc-when-wrong-kind)))))


(defret not-trace-abort-of-<fn>
  (not-trace-abort-p eval)
  :hints(("Goal" :in-theory (enable <fn> not-trace-abort-p)))
  :fn check_recurse_limit)

(defret no-trace-of-<fn>
  (not-trace-abort-p i)
  :hints(("Goal" :in-theory (enable <fn> not-trace-abort-p)))
  :fn v_to_int)

(defret no-trace-of-<fn>
  (not (equal (ev_error->desc i) "Trace abort"))
  :hints(("Goal" :in-theory (enable <fn>)))
  :fn v_to_bool)

(defret no-trace-of-<fn>
  (not-trace-abort-p i)
  :hints(("Goal" :in-theory (enable <fn> not-trace-abort-p)))
  :fn v_to_label)

(defret no-tracep-of-init-backtrace
  (equal (not-trace-abort-p new-x)
         (not-trace-abort-p x))
  :hints(("Goal" :in-theory (enable <fn> not-trace-abort-p
                                    ev_error->desc-when-wrong-kind)))
  :fn init-backtrace)

(local (defthm len-of-take
         (equal (len (take n x)) (nfix n))
         :hints(("Goal" :in-theory (enable take len)))))

;; (local (defthm append-nil-under-iff
;;          (iff (append x nil) (consp x))))

;; (local (defthm not-trace-of-append-nil
;;          (implies (not-trace x)
;;                   (not-trace (append x nil)))
;;          :hints(("Goal" :in-theory (enable not-trace)))))





(defthm eval_stmt-*t1-s_cond-redef
  (implies (and (syntaxp (quotep s))
                (stmt_desc-case (stmt->desc s) :s_cond))
           (equal (eval_stmt-*t1 env s)
                  (b* ((trace nil)
                       (pos (stmt->pos_start s))
                       (s (stmt->desc s))
                       ((s_cond s))
                       ((evoo-*t (expr_result test))
                        (eval_expr-*t env s.test))
                       ((evo-*t testval)
                        (val-case test.val
                          :v_bool (ev_normal test.val.val)
                          :otherwise (ev_error "Non-boolean test result"
                                               s.test (list pos)))))
                    (if testval
                        (evtailcall-*t (eval_block-*t test.env s.then))
                      (evtailcall-*t (eval_block-*t test.env s.else))))))
  :hints (("goal" :expand ((eval_stmt-*t1 env s)))))

(defthm eval_expr-*t-e_cond-redef
  (implies (and (syntaxp (quotep e))
                (expr_desc-case (expr->desc e) :e_cond))
           (equal (eval_expr-*t env e)
                  (b* ((trace nil)
                       (pos (expr->pos_start e))
                       (desc (expr->desc e))
                       ((e_cond desc))
                       ((evoo-*t (expr_result test))
                        (eval_expr-*t env desc.test))
                       ((evo-*t testval)
                        (val-case test.val
                          :v_bool (ev_normal test.val.val)
                          :otherwise (ev_error "bad test in e_cond"
                                               test.val (list pos)))))
                    (if testval
                        (evtailcall-*t (eval_expr-*t test.env desc.then))
                      (evtailcall-*t (eval_expr-*t test.env desc.else))))))
  :hints (("goal" :expand ((eval_expr-*t env e)))))





(encapsulate nil
  (local (in-theory (acl2::disable* openers trace-openers)))
  (defopener open-eval_stmt-*t1-no-cond eval_stmt-*t1 :hyp (and (syntaxp (quotep s))
                                                                (not (stmt_desc-case (stmt->desc s) :s_cond))))
  
  (defopener open-eval_expr-*t-no-cond eval_expr-*t :hyp (and (syntaxp (quotep e))
                                                              (not (expr_desc-case (expr->desc e) :e_cond)))))



;; Induction scheme for while loops
(define loop-induct-*t ((env env-p) (whilep) (limit acl2::maybe-integerp) (test expr-p) (body stmt-p)
                     &key ((clk natp) 'clk) (orac 'orac) (tracespec 'tracespec))
  :verify-guards nil
  :non-executable t
  :measure (nfix clk)
  (b* (((mv (ev_normal cev) orac ?traces) (eval_expr-*t env test))
       ((expr_result cev.res))
       ((unless (iff (ev_normal->res (v_to_bool cev.res.val)) whilep))
        env)
       (limit (ev_normal->res (tick_loop_limit limit)))
       ((mv (ev_normal blkev) orac ?traces) (eval_block-*t cev.res.env body))
       ((continuing blkev.res))
       ((when (zp clk))
        blkev.res.env))
    (loop-induct-*t blkev.res.env whilep limit test body :clk (1- clk))))
(in-theory (enable (:i loop-induct-*t)))


(define for-induct-*t ((env env-p) (index_name identifier-p) (limit acl2::maybe-integerp) (start integerp) (dir for_direction-p) (end integerp) (body stmt-p) &key ((clk natp) 'clk) (orac 'orac) (tracespec 'tracespec))
  :verify-guards nil
  :non-executable t
  :measure (for_loop-measure start end dir)
  (b* (((when (for_loop-test start end dir))
        env)
       (limit (ev_normal->res (tick_loop_limit limit)))
       ((mv (ev_normal blkev) orac ?traces) (eval_block-*t env body))
       ((continuing blkev.res))
       ((mv step env2) (eval_for_step blkev.res.env index_name start dir)))
    (for-induct-*t env2 index_name limit step dir end body)))
(in-theory (enable (:i for-induct-*t)))




(defconst *defloop-no-trace-template*
  '(defsection <name>
     (defconst *<name>*
       (find-nth-form <nth> <looptype>
                      (hons-assoc-equal <fn>
                                        (static_env_global->subprograms <static-env>))))

     (:@ (not :s_for)
      (defconst *<name>-test*
        (<looptype>->test *<name>*)))

     (defconst *<name>-body*
       (<looptype>->body *<name>*))

     <prepwork>
     
     (local (in-theory (acl2::e/d* (<defloop-enables>
                                    <user-enables>)
                                   (<defloop-disables>
                                    <user-disables>))))
     (defthm <name>-no-trace
       (implies (and (tracespec-excludes-stdlib-fns <fns> tracespec)
                     <hyp>)
                (no-trace-p <loop-form>))
       :hints (;; copied from just-induct-and-expand
               (if (equal (car id) '(0))
                   (let* ((expand-hints (acl2::just-expand-cp-parse-hints
                                         '(<loop-form-expand>) (w state)))
                          (cproc `(acl2::mark-expands-cp clause '(nil t ,expand-hints))))
                     `(:computed-hint-replacement
                       ((and (equal (car id) '(0)) '(:clause-processor acl2::clause-to-term))
                        (and (equal (car id) '(0)) '(:induct <induction>)))
                       :clause-processor ,cproc))
                 (and (equal (car id) '(0 1))
                      (acl2::expand-marked :last-only t)))
               <user-hints>))

     (defthm <name>-no-trace-abort
       (implies (and (tracespec-excludes-stdlib-fns <fns> tracespec)
                     <hyp>)
                (no-trace-abort-p <loop-form>))
       :hints (;; copied from just-induct-and-expand
               (if (equal (car id) '(0))
                   (let* ((expand-hints (acl2::just-expand-cp-parse-hints
                                         '(<loop-form-expand>) (w state)))
                          (cproc `(acl2::mark-expands-cp clause '(nil t ,expand-hints))))
                     `(:computed-hint-replacement
                       ((and (equal (car id) '(0)) '(:clause-processor acl2::clause-to-term))
                        (and (equal (car id) '(0)) '(:induct <induction>)))
                       :clause-processor ,cproc))
                 (and (equal (car id) '(0 1))
                      (acl2::expand-marked :last-only t)))
               <user-hints>))))





(define defloop-no-trace-fn (name args state)
  :mode :program
  (b* (((std::extract-keyword-args
         :other-args bad-args
         ;; :kwd-alist kwd-alist
         function
         (looptype :while)
         (nth 0)
         
         (start-var 'start)
         (end-var 'end)
         (hyp 't)
         (fns 'nil)
         
         enable
         disable
         hints
         prepwork
         (static-env '(stdlib-static-env)))
        args)
       ((when bad-args)
        (er soft 'defloop-no-trace "Bad arguments: ~x0" bad-args))
       ((unless (stringp function))
        (er soft 'defloop-no-trace "Function should be a string: ~x0" function))
       (orig-looptype looptype)
       (looptype (case orig-looptype
                   ((:while :s_while) :s_while)
                   ((:for :s_for) :s_for)
                   ((:repeat :s_repeat) :s_repeat)
                   (t nil)))
       ((unless looptype)
        (er soft 'defloop-no-trace "Bad looptype: ~x0" orig-looptype))
       ((unless (natp nth))
        (er soft 'defloop-no-trace "Bad nth: ~x0" nth))
       ((acl2::er (cons & static-env-val))
        (acl2::simple-translate-and-eval static-env nil nil
                                         (msg "static env ~x0" static-env)
                                         'defloop-no-trace (w state) state t))
       ((unless (static_env_global-p static-env-val))
        (er soft 'defloop-no-trace "Bad static env (evaluation of ~x0): doesn't satisfy static_env_global-p" static-env))
       (fn-struct (hons-assoc-equal function
                                    (static_env_global->subprograms static-env-val)))
       ((unless fn-struct)
        (er soft 'defloop-no-trace "Bad function ~x0: not found in static env" function))
       ;; if found, then it's the right type
       (form (find-nth-form nth looptype fn-struct))
       ((unless form)
        (er soft 'defloop-no-trace "Loop not found: function ~x0 looptype ~x1 nth ~x2" function looptype nth))

       ;; ((when (and (eq looptype :s_for)
       ;;             (not index-var)))
       ;;  (er soft 'defloop-no-trace "Index var must be specified for for loops"))
       ;; ((when (and index-var (not (eq looptype :s_for))))
       ;;  (er soft 'defloop-no-trace "Index var specified for non-for loop"))
                       
              
       ((acl2::tmplsubst template)
        (acl2::make-tmplsubst
         :atoms `((<name> . ,name)
                  (<nth> . ,nth)
                  (<looptype> . ,looptype)
                  (<fn> . ,function)
                  (<fns> . ',fns)
                  (<hyp> . ,hyp)
                  (<static-env> . ,static-env))
         :splices `((<defloop-enables> . (asl-code-proof-enables
                                          trace-openers))
                    (<defloop-disables> . (asl-code-proof-disables))
                    (<user-enables> . ,enable)
                    (<user-disables> . ,disable)
                    (<user-hints> . ,hints)
                    (<prepwork> . ,prepwork))
         :strs `(("<NAME>" . ,(symbol-name name))
                 ("<LOOPTYPE>" . ,(symbol-name looptype)))
         :features (list looptype)
         :pkg-sym 'asl-pkg))

       (body-const (acl2::template-subst-top '*<name>-body* template))
       (test-const (acl2::template-subst-top '*<name>-test* template))
       
       ((mv loop-form expand induction)
        (case looptype
          (:s_for
           (let ((args `(env ,(s_for->index_name form)
                             ,(and (s_for->limit form) 'limit)
                             ,start-var ,(s_for->dir form) ,end-var
                             ,body-const)))
             (mv `(eval_for-*t  . ,args)
                 `(:free (limit ,start-var ,end-var)
                   (eval_for-*t . ,args))
                 `(for-induct-*t . ,args))))
          (t
           (let ((args `(env ,(eq looptype :s_while)
                             ,(and (if (eq looptype :s_while)
                                       (s_while->limit form)
                                     (s_repeat->limit form))
                                   'limit)
                             ,test-const ,body-const)))
             (mv `(eval_loop-*t . ,args)
                 `(:free (limit) (eval_loop-*t . ,args))
                 `(loop-induct-*t . ,args))))))

       (template (acl2::change-tmplsubst
                  template :atoms `((<loop-form> . ,loop-form)
                                    (<induction> . ,induction)
                                    (<loop-form-expand> . ,expand)
                                    . ,template.atoms)))
                      
                      
       (event
        (acl2::template-subst-top *defloop-no-trace-template* template)))
    (value event)))

(defmacro defloop-no-trace (name &rest args)
  `(make-event (defloop-no-trace-fn ',name ',args state)))


(local (defthm nth-of-take
         (implies (< (nfix n) (nfix m))
                  (equal (nth n (take m x))
                         (nth n x)))
         :hints(("Goal" :in-theory (enable nth take)))))

(local (in-theory (disable acl2::append-of-nil
                           acl2::append-when-not-consp
                           true-listp
                           integer-listp)))


(local
 (progn
   
   (defthm combine-tracespecs-when-no-new-ts
     (equal (combine-tracespecs nil nil x)
            (tracespec-fix x))
     :hints(("Goal" :in-theory (enable combine-tracespecs))))

   (defthm from-lists-empty
     (equal (omap::from-lists nil vals) nil)
     :hints(("Goal" :in-theory (enable omap::from-lists))))

   (defthm take-of-0
     (equal (take 0 x) nil)
     :hints(("Goal" :in-theory (enable take))))

   (defthm update*-none
     (equal (omap::update* nil x)
            (omap::mfix x))
     :hints(("Goal" :in-theory (enable omap::update))))))

(local (in-theory (disable logand)))

(include-book "bits")
(include-book "align")

(def-asl-no-trace "Zeros-1")
(def-asl-no-trace "Ones-1")
(def-asl-no-trace "AlignDownSize-1")
(def-asl-no-trace "AlignDownP2-1")
(def-asl-no-trace "AlignDownP2")
(defloop-no-trace replicate-1-loop
  :function "Replicate-1"
  :looptype :s_for)

(def-asl-no-trace "Replicate-1")
(def-asl-no-trace "Replicate")


(local (in-theory (disable mv-nth)))

(defloop-no-trace uint-loop
  :function "UInt"
  :looptype :s_for)



(def-asl-no-trace "UInt")

(def-asl-no-trace "UInt" :static-env (stdlib-prim-static-env))

(def-asl-no-trace "AlignDownSize")
(def-asl-no-trace "AlignDownSize" :static-env (stdlib-prim-static-env))


(local (in-theory (enable eval_subprogram-*t-equals-original)))

(DEFTHM ALIGNDOWNP2-1-CORRECT-*t
  (B* (((V_INT X)) ((V_INT P2)))
    (IMPLIES
     (AND
      (SUBPROGRAMS-MATCH '("AlignDownP2-1" "AlignDownSize-1")
                         (GLOBAL-ENV->STATIC (ENV->GLOBAL ENV))
                         (STDLIB-STATIC-ENV))
      (tracespec-excludes-stdlib-fns
       '("AlignDownP2-1" "AlignDownSize-1") tracespec)
      (<= 0 X.VAL)
      (<= 0 P2.VAL)
      (EQUAL (VAL-KIND X) :V_INT)
      (EQUAL (VAL-KIND P2) :V_INT)
      (<= 1 (IFIX CLK)))
     (B*
         (((MV RES NEW-ORAC traces)
           (EVAL_SUBPROGRAM-*t ENV "AlignDownP2-1" (LIST)
                            (LIST X P2)))
          (SPEC
           (EV_NORMAL
            (FUNC_RESULT
             (LIST (V_INT (* (EXPT 2 P2.VAL)
                             (FLOOR X.VAL (EXPT 2 P2.VAL)))))
             (ENV->GLOBAL ENV)))))
       (AND (EQUAL NEW-ORAC ORAC)
            (EQUAL RES SPEC)
            (equal traces nil))))))

(DEFTHM ALIGNDOWNP2-CORRECT-*t
  (B* (((V_INT N))
       ((V_BITVECTOR X))
       ((V_INT P2)))
    (IMPLIES
     (AND (SUBPROGRAMS-MATCH '("AlignDownP2" "Zeros-1")
                             (GLOBAL-ENV->STATIC (ENV->GLOBAL ENV))
                             (STDLIB-STATIC-ENV))
          (tracespec-excludes-stdlib-fns '("AlignDownP2" "Zeros-1") tracespec)
          (< 0 N.VAL)
          (EQUAL (VAL-KIND N) :V_INT)
          (EQUAL (VAL-KIND X) :V_BITVECTOR)
          (EQUAL X.LEN N.VAL)
          (EQUAL (VAL-KIND P2) :V_INT)
          (<= 0 P2.VAL)
          (<= P2.VAL X.LEN)
          (<= 1 (IFIX CLK)))
     (B*
         (((MV RES NEW-ORAC traces)
           (EVAL_SUBPROGRAM-*t ENV "AlignDownP2" (LIST N)
                            (LIST X P2)))
          (SPEC
           (EV_NORMAL
            (FUNC_RESULT (LIST (V_BITVECTOR N.VAL
                                            (* (EXPT 2 P2.VAL)
                                               (FLOOR X.VAL (EXPT 2 P2.VAL)))))
                         (ENV->GLOBAL ENV)))))
       (AND (EQUAL NEW-ORAC ORAC)
            (EQUAL RES SPEC)
            (equal traces nil))))))


(DEFTHM REPLICATE-1-CORRECT-*t
  (B* (((V_INT N))
       ((V_INT M))
       ((V_BITVECTOR X)))
    (IMPLIES
     (AND
      (SUBPROGRAMS-MATCH '("Replicate-1" "Ones-1" "Zeros-1")
                         (GLOBAL-ENV->STATIC (ENV->GLOBAL ENV))
                         (STDLIB-STATIC-ENV))
      (tracespec-excludes-stdlib-fns
       '("Replicate-1" "Ones-1" "Zeros-1") tracespec)
      (<= 0 N.VAL)
      (< 0 M.VAL)
      
      (EQUAL (VAL-KIND N) :V_INT)
      (EQUAL (VAL-KIND M) :V_INT)
      (EQUAL (VAL-KIND X) :V_BITVECTOR)
      (EQUAL X.LEN M.VAL)
      (<= 2 (IFIX CLK)))
     (B*
         (((MV RES NEW-ORAC traces)
           (EVAL_SUBPROGRAM-*t ENV "Replicate-1" (LIST N M)
                               (LIST X)))
          (SPEC
           (EV_NORMAL
            (FUNC_RESULT
             (LIST (V_BITVECTOR N.VAL
                                (LOGREPEAT (/ N.VAL M.VAL)
                                           M.VAL X.VAL)))
             (ENV->GLOBAL ENV)))))
       (AND (EQUAL NEW-ORAC ORAC)
            (EQUAL RES
                   (if (INTEGERP (* (/ M.VAL) N.VAL))
                       spec
                     (b* ((err0 (EV_ERROR "Unsupported binop"
                                          (LIST :DIV (VAL-FIX N) (VAL-FIX M))
                                          NIL))
                          (err1 (INIT-BACKTRACE err0
                                                ;; FIXME
                                                '((FNAME . "ASL Standard Library")
                                                  (LNUM . 378)
                                                  (BOL . 9530)
                                                  (CNUM . 9546)))))
                       (change-ev_error err1 :backtrace (cons (list "Replicate-1"
                                                                    (list (val-fix n) (val-fix m))
                                                                    (list (val-fix x)))
                                                              (ev_error->backtrace err1))))))
            (equal traces nil))))))

(DEFTHM REPLICATE-CORRECT-*t
  (B* (((V_INT M))
       ((V_INT N))
       ((V_BITVECTOR X)))
    (IMPLIES
     (AND (SUBPROGRAMS-MATCH
           '("Replicate" "Ones-1" "Zeros-1" "Replicate-1")
           (GLOBAL-ENV->STATIC (ENV->GLOBAL ENV))
           (STDLIB-STATIC-ENV))
          (tracespec-excludes-stdlib-fns
           '("Replicate" "Ones-1" "Zeros-1" "Replicate-1") tracespec)
          (<= 0 N.VAL)
          (< 0 M.VAL)
          (EQUAL (VAL-KIND M) :V_INT)
          (EQUAL (VAL-KIND N) :V_INT)
          (EQUAL (VAL-KIND X) :V_BITVECTOR)
          (EQUAL X.LEN M.VAL)
          (<= 3 (IFIX CLK)))
     (B*
         (((MV RES NEW-ORAC traces)
           (EVAL_SUBPROGRAM-*t ENV "Replicate" (LIST M N)
                               (LIST X N)))
          (SPEC
           (EV_NORMAL
            (FUNC_RESULT
             (LIST (V_BITVECTOR (* N.VAL M.VAL)
                                (LOGREPEAT N.VAL M.VAL X.VAL)))
             (ENV->GLOBAL ENV)))))
       (AND (EQUAL NEW-ORAC ORAC)
            (EQUAL RES SPEC)
            (equal traces nil))))))


(DEFTHM ALIGNDOWNSIZE-1-CORRECT-*t
  (B* (((V_INT X)) ((V_INT SIZE)))
    (IMPLIES
     (AND
      (SUBPROGRAMS-MATCH '("AlignDownSize-1")
                         (GLOBAL-ENV->STATIC (ENV->GLOBAL ENV))
                         (STDLIB-STATIC-ENV))
      (tracespec-excludes-stdlib-fns
           '("AlignDownSize-1") tracespec)
      (<= 0 X.VAL)
      (< 0 SIZE.VAL)
      (EQUAL (VAL-KIND X) :V_INT)
      (EQUAL (VAL-KIND SIZE) :V_INT)
      T)
     (B*
         (((MV RES NEW-ORAC traces)
           (EVAL_SUBPROGRAM-*t ENV "AlignDownSize-1" (LIST)
                            (LIST X SIZE)))
          (SPEC
           (EV_NORMAL
            (FUNC_RESULT
             (LIST (V_INT (* SIZE.VAL (FLOOR X.VAL SIZE.VAL))))
             (ENV->GLOBAL ENV)))))
       (AND (EQUAL NEW-ORAC ORAC)
            (EQUAL RES SPEC)
            (equal traces nil))))))

(DEFTHM UINT-CORRECT-*t
  (B* (((V_INT N)) ((V_BITVECTOR VAL)))
    (IMPLIES
     (AND
      (SUBPROGRAMS-MATCH '("UInt")
                         (GLOBAL-ENV->STATIC (ENV->GLOBAL ENV))
                         (STDLIB-STATIC-ENV))
      (tracespec-excludes-stdlib-fns '("UInt") tracespec)
      T (EQUAL (VAL-KIND N) :V_INT)
      (EQUAL (VAL-KIND VAL) :V_BITVECTOR)
      (EQUAL VAL.LEN N.VAL)
      T)
     (B* (((MV RES NEW-ORAC traces)
           (EVAL_SUBPROGRAM-*t ENV "UInt" (LIST N)
                            (LIST VAL)))
          (SPEC (EV_NORMAL (FUNC_RESULT (LIST (V_INT VAL.VAL))
                                        (ENV->GLOBAL ENV)))))
       (AND (EQUAL NEW-ORAC ORAC)
            (EQUAL RES SPEC)
            (equal traces nil))))))

(DEFTHM UINT-CORRECT-prim-*t
  (B* (((V_INT N)) ((V_BITVECTOR VAL)))
    (IMPLIES
     (AND
      (SUBPROGRAMS-MATCH '("UInt")
                         (GLOBAL-ENV->STATIC (ENV->GLOBAL ENV))
                         (STDLIB-prim-STATIC-ENV))
      (tracespec-excludes-stdlib-fns '("UInt") tracespec)
      T (EQUAL (VAL-KIND N) :V_INT)
      (EQUAL (VAL-KIND VAL) :V_BITVECTOR)
      (EQUAL VAL.LEN N.VAL)
      T)
     (B* (((MV RES NEW-ORAC traces)
           (EVAL_SUBPROGRAM-*t ENV "UInt" (LIST N)
                            (LIST VAL)))
          (SPEC (EV_NORMAL (FUNC_RESULT (LIST (V_INT VAL.VAL))
                                        (ENV->GLOBAL ENV)))))
       (AND (EQUAL NEW-ORAC ORAC)
            (EQUAL RES SPEC)
            (equal traces nil))))))



(DEFTHM ALIGNDOWNSIZE-CORRECT-*t
  (B* (((V_INT N))
       ((V_BITVECTOR X))
       ((V_INT SIZE)))
    (IMPLIES
     (AND (SUBPROGRAMS-MATCH
           '("AlignDownSize" "AlignDownSize-1" "UInt")
           (GLOBAL-ENV->STATIC (ENV->GLOBAL ENV))
           (STDLIB-STATIC-ENV))
          (tracespec-excludes-stdlib-fns
           '("AlignDownSize" "AlignDownSize-1" "UInt") tracespec)
          T (EQUAL (VAL-KIND N) :V_INT)
          (EQUAL (VAL-KIND X) :V_BITVECTOR)
          (EQUAL X.LEN N.VAL)
          (EQUAL (VAL-KIND SIZE) :V_INT)
          (<= 1 SIZE.VAL)
          (<= SIZE.VAL (EXPT 2 X.LEN))
          (<= 1 (IFIX CLK)))
     (B*
         (((MV RES NEW-ORAC traces)
           (EVAL_SUBPROGRAM-*t ENV "AlignDownSize" (LIST N)
                            (LIST X SIZE)))
          (SPEC
           (EV_NORMAL
            (FUNC_RESULT
             (LIST
              (V_BITVECTOR N.VAL
                           (* SIZE.VAL (FLOOR X.VAL SIZE.VAL))))
             (ENV->GLOBAL ENV)))))
       (AND (EQUAL NEW-ORAC ORAC)
            (EQUAL RES SPEC)
            (equal traces nil))))))

(DEFTHM ALIGNDOWNSIZE-CORRECT-prim-*t
  (B* (((V_INT N))
       ((V_BITVECTOR X))
       ((V_INT SIZE)))
    (IMPLIES
     (AND (SUBPROGRAMS-MATCH
           '("AlignDownSize" "AlignDownSize-1" "UInt")
           (GLOBAL-ENV->STATIC (ENV->GLOBAL ENV))
           (STDLIB-prim-STATIC-ENV))
          (tracespec-excludes-stdlib-fns
           '("AlignDownSize" "AlignDownSize-1" "UInt") tracespec)
          T (EQUAL (VAL-KIND N) :V_INT)
          (EQUAL (VAL-KIND X) :V_BITVECTOR)
          (EQUAL X.LEN N.VAL)
          (EQUAL (VAL-KIND SIZE) :V_INT)
          (<= 1 SIZE.VAL)
          (<= SIZE.VAL (EXPT 2 X.LEN))
          (<= 1 (IFIX CLK)))
     (B*
         (((MV RES NEW-ORAC traces)
           (EVAL_SUBPROGRAM-*t ENV "AlignDownSize" (LIST N)
                            (LIST X SIZE)))
          (SPEC
           (EV_NORMAL
            (FUNC_RESULT
             (LIST
              (V_BITVECTOR N.VAL
                           (* SIZE.VAL (FLOOR X.VAL SIZE.VAL))))
             (ENV->GLOBAL ENV)))))
       (AND (EQUAL NEW-ORAC ORAC)
            (EQUAL RES SPEC)
            (equal traces nil))))))








#|

(def-asl-no-trace "Abs")
(def-asl-no-trace "Abs-1")
(def-asl-no-trace "Min")
(def-asl-no-trace "Min-1")
(def-asl-no-trace "Max")
(def-asl-no-trace "Max-1")
(def-asl-no-trace "IsEven")
(def-asl-no-trace "IsOdd")
(def-asl-no-trace "Real")
(def-asl-no-trace "Zeros")
(def-asl-no-trace "Ones")
(def-asl-no-trace "ReplicateBit-1")
(def-asl-no-trace "ReplicateBit")
(def-asl-no-trace "Len")
(def-asl-no-trace "IsZero")
(def-asl-no-trace "IsOnes")
(def-asl-no-trace "ZeroExtend-1")
(def-asl-no-trace "ZeroExtend")





;; (local (defthm val-imap-p-of-update*
;;          (implies (and (val-imap-p x)
;;                        (val-imap-p y))
;;                   (val-imap-p (omap::update* x y)))
;;          :hints(("Goal" :in-theory (enable omap::update*)))))



;; (local (defthm len-of-member
;;          (<= (len (member-equal k x)) (len x))
;;          :hints(("Goal" :in-theory (enable len)))
;;          :rule-classes :linear))

;; (local (defthm assoc-of-from-lists
;;          (equal (omap::assoc key (omap::from-lists keys vals))
;;                 (let ((mem (member-equal key keys)))
;;                   (and mem
;;                        (cons key (nth (- (len keys) (len mem)) vals)))))
;;          :hints(("Goal" :in-theory (enable omap::from-lists len)))))
                    



;; (local (defthm len-equal-0
;;          (equal (equal (len x) 0)
;;                 (not (consp x)))
;;          :hints(("Goal" :in-theory (enable len)))))

;; (local (defthm env-find-of-declare-local-identifiers
;;          (equal (env-find var (declare_local_identifiers
;;                                env names vals))
;;                 (let ((mem (member-equal (identifier-fix var)
;;                                          (identifierlist-fix names))))
;;                   (if mem
;;                       (lk_local (val-fix (nth (- (len names) (len mem)) vals)))
;;                     (env-find var env))))
;;          :hints(("Goal" :in-theory (enable env-find declare_local_identifiers
;;                                            val-imap-p-of-from-lists
;;                                            val-imaplist-assoc)))))

;; (local (in-theory (disable env-find declare_local_identifiers)))

(def-asl-no-trace "AlignUpSize-1")
(def-asl-no-trace "AlignUpP2-1")
(def-asl-no-trace "AlignDown")
(def-asl-no-trace "AlignUp")
(def-asl-no-trace "AlignDownSize")
(def-asl-no-trace "AlignUpSize")
(def-asl-no-trace "AlignUpP2")
(def-asl-no-trace "IsAlignedSize-1")
(def-asl-no-trace "IsAlignedSize")
(def-asl-no-trace "IsAlignedP2-1")
(def-asl-no-trace "IsAlignedP2")



(defloop-no-trace bitcount-loop
  :function "BitCount"
  :looptype :s_for)

(def-asl-no-trace "BitCount")

(defloop-no-trace lowestsetbit-loop
  :function "LowestSetBit"
  :looptype :s_for)

(def-asl-no-trace "LowestSetBit")
(def-asl-no-trace "LowestSetBitNZ")

(defloop-no-trace highestsetbit-loop
  :function "HighestSetBit"
  :looptype :s_for)

(def-asl-no-trace "HighestSetBit")
(def-asl-no-trace "HighestSetBitNZ")

(def-asl-no-trace "SignExtend-1")
(def-asl-no-trace "SignExtend")

(def-asl-no-trace "Extend-1")
(def-asl-no-trace "Extend")

(def-asl-no-trace "CountLeadingZeroBits")
(def-asl-no-trace "CountLeadingSignBits")

(defloop-no-trace ilog2-loop-1
  :function "ILog2"
  :looptype :s_while
  :nth 0)

(defloop-no-trace ilog2-loop-2
  :function "ILog2"
  :looptype :s_while
  :nth 1)

(defloop-no-trace ilog2-loop-3
  :function "ILog2"
  :looptype :s_while
  :nth 2)

(def-asl-no-trace "ILog2")


(defloop-no-trace floorlog2-loop
  :function "FloorLog2"
  :looptype :s_while)

(def-asl-no-trace "FloorLog2")

(defloop-no-trace ceillog2-loop
  :function "CeilLog2"
  :looptype :s_while)

(def-asl-no-trace "CeilLog2")

(defloop-no-trace floorpow2-loop
  :function "FloorPow2"
  :looptype :s_while)

(def-asl-no-trace "FloorPow2")
(def-asl-no-trace "CeilPow2")
(def-asl-no-trace "IsPow2")

(defloop-no-trace roundtowardszero-loop
  :function "RoundTowardsZero"
  :looptype :s_for
  :fns ("Real")
  :hyp (subprograms-match '("Real")
                          (global-env->static (env->global env))
                          (stdlib-static-env)))

(def-asl-no-trace "RoundTowardsZero")

(def-asl-no-trace "RoundUp")
(def-asl-no-trace "RoundDown")


(def-asl-no-trace "SInt")

(defloop-no-trace sqrtrounded-loop
  :function "SqrtRounded"
  :looptype :s_for)

;; This one takes a very long time...
(def-asl-no-trace "SqrtRounded")

(def-asl-no-trace "LSL")
(def-asl-no-trace "LSL_C")
(def-asl-no-trace "LSR")
(def-asl-no-trace "LSR_C")
(def-asl-no-trace "ASR")
(def-asl-no-trace "ASR_C")
(def-asl-no-trace "ROR")
(def-asl-no-trace "ROR_C")
(def-asl-no-trace "ROL")
(def-asl-no-trace "ROL_C")


|#

;;****************************************************************************;;
;;                              ACL2 ASL Library                              ;;
;;****************************************************************************;;
;;
;; SPDX-FileCopyrightText: Copyright 2026 Arm Limited and/or its affiliates <open-source-office@arm.com>
;; SPDX-License-Identifier: BSD-3-Clause
;; 
;; ****************************************************************************;;

(in-package "ASL")

(include-book "defs")
(include-book "error-free" :dir :acl2asl)
(include-book "trace-free" :dir :acl2asl)
(include-book "centaur/fgl/annotation" :dir :system)
(include-book "centaur/fgl/helper-utils" :dir :system)
(include-book "centaur/bitops/limited-shifts" :dir :system)
(local (include-book "centaur/bitops/ihsext-basics" :dir :system))
(local (include-book "trace-fixequiv" :dir :acl2asl))
(local (std::add-default-post-define-hook :fix))
;; Big speedup for symbolic simulations:
(memoize 'static_env_global-p)

(fgl-disable-asl-no-error-defs)

;; ----------------------------------------------------------------------------------
;; Avoid merging conditional branch statements/expressions:
;; The interpreter is defined such that a conditional branch resolves as
;; (eval (if test then else))
;; where then/else are the subexpressions/substatements. We don't want this
;; because we want the program we're evaluating to be a constant, not symbolic. So
;; we define two rules that just rephrase these as
;; (if test (eval then) (eval else)).

(fgl::def-fgl-rewrite eval_stmt-avoid-merging-s_cond-branches
  (implies (stmt_desc-case (stmt->desc s) :s_cond)
           (equal (eval_stmt env (fgl::concrete s))
                  (b* ((pos (stmt->pos_start s))
                       (s (stmt->desc s))
                       ((s_cond s))
                       ((mv (evo (expr_result test)) orac) (eval_expr env s.test))
                       ((evo testval) (val-case test.val
                                       :v_bool (ev_normal test.val.val)
                                       :otherwise (ev_error "Non-boolean test result in s_cond"
                                                            s.test (list pos)))))
                    (if testval
                        (eval_block test.env s.then)
                      (eval_block test.env s.else)))))
  :hints (("Goal" :expand ((eval_stmt env s)))))

(fgl::def-fgl-rewrite eval_stmt-*t1-avoid-merging-s_cond-branches
  (implies (stmt_desc-case (stmt->desc s) :s_cond)
           (equal (eval_stmt-*t1 env (fgl::concrete s))
                  (b* ((trace nil)
                       (pos (stmt->pos_start s))
                       (s (stmt->desc s))
                       ((s_cond s))
                       (env (env-replace-static static-env env))
                       ((evoo-*t (expr_result test))
                        (eval_expr-*t env s.test))
                       ((evo-*t testval)
                        (val-case test.val
                          :v_bool (ev_normal test.val.val)
                          :otherwise (ev_error "Non-boolean test result in s_cond"
                                               s.test (list pos)))))
                    (if testval
                        (evtailcall-*t (eval_block-*t test.env s.then))
                      (evtailcall-*t (eval_block-*t test.env s.else))))))
  :hints (("Goal" :expand ((eval_stmt-*t1 env s)))))

(local (in-theory (acl2::disable* asl-*t-equals-original-rules )))

(fgl::def-fgl-rewrite eval_stmt-*t1-when-error-free-avoid-merging-s_cond-branches
  (IMPLIES (and (EVAL_STMT-*T1-NO-ERROR ENV S)
                (stmt_desc-case (stmt->desc s) :s_cond))
           (EQUAL (EVAL_STMT-*T1 ENV (fgl::concrete S))
                  (BIND-ENV-WITH-STATIC
                   (EVBODY-*T
                    (B* ((POS (STMT->POS_START S))
                         (S (STMT->DESC S))
                         ((s_cond s))
                         ((EVOO-*TEF _CONDVAR-8 (EXPR_RESULT TEST))
                          (EVAL_EXPR-*T ENV S.TEST))
                         ((EVO-*TEF _CONDVAR-9 TESTVAL)
                          (VAL-CASE
                            TEST.VAL
                            :V_BOOL (EV_NORMAL TEST.VAL.VAL)
                            :OTHERWISE
                            (EV_ERROR "Non-boolean test result in s_cond"
                                      S.TEST (LIST POS)))))
                      (if testval
                          (EVTAILCALL-*TEF _CONDVAR-10
                                           (EVAL_BLOCK-*T TEST.ENV s.then))
                        (EVTAILCALL-*TEF _CONDVAR-11
                                         (EVAL_BLOCK-*T TEST.ENV s.else))))))))
  :HINTS ((ACL2::JUST-EXPAND ((EVAL_STMT-*T1-NO-ERROR ENV S)))
          (ACL2::JUST-EXPAND ((EVAL_STMT-*T1 ENV S)))
          (and stable-under-simplificationp
               (ACL2::JUST-EXPAND ((EVAL_STMT-*T1 ENV S))))
          '(:IN-THEORY (ENABLE FGL::CONDITIONALIZE1 fgl::conditionalize2))))

(fgl::def-fgl-rewrite eval_expr-avoid-merging-e_cond-branches
  (implies (expr_desc-case (expr->desc e) :e_cond)
           (equal (eval_expr env (fgl::concrete e))
                  (b* ((pos (expr->pos_start e))
                       (e (expr->desc e))
                       ((e_cond e))
                       ((mv (evo (expr_result test)) orac) (eval_expr env e.test))
                       ((evo testval) (val-case test.val
                                       :v_bool (ev_normal test.val.val)
                                       :otherwise (ev_error "bad test in e_cond" test.val (list pos))))
                       )
                    (if testval
                        (eval_expr test.env e.then)
                      (eval_expr test.env e.else))
                    )))
  :hints (("Goal" :expand ((eval_expr env e)))))

(fgl::def-fgl-rewrite eval_expr-*t-avoid-merging-e_cond-branches
  (implies (expr_desc-case (expr->desc e) :e_cond)
           (equal (eval_expr-*t env (fgl::concrete e))
                  (b* ((trace nil)
                       (pos (expr->pos_start e))
                       (desc (expr->desc e))
                       ((e_cond desc))
                       (env (env-replace-static static-env env))
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
  :hints (("Goal" :expand ((eval_expr-*t env e)))))

(fgl::def-fgl-rewrite EVAL_EXPR-*T-WHEN-ERROR-FREE-avoid-merging-e_cond-branches
  (IMPLIES (and (EVAL_EXPR-*T-NO-ERROR ENV E)
                (expr_desc-case (expr->desc e) :e_cond))
           (EQUAL
            (EVAL_EXPR-*T ENV E)
            (BIND-ENV-WITH-STATIC
             (EVBODY-*T
              (B* ((DESC (EXPR->DESC E))
                   (POS (EXPR->POS_START E))
                   ((e_cond desc))
                   ((EVOO-*TEF _CONDVAR-21 (EXPR_RESULT TEST))
                    (EVAL_EXPR-*T ENV DESC.TEST))
                   ((EVO-*TEF _CONDVAR-22 valval)
                    (VAL-CASE
                      TEST.VAL
                      :V_BOOL
                      (EV_NORMAL test.val.val)
                      :OTHERWISE (EV_ERROR "bad test in e_cond"
                                           TEST.VAL (LIST POS)))))
                (if valval
                    (EVTAILCALL-*TEF _CONDVAR-23
                                     (EVAL_EXPR-*T TEST.ENV desc.then))
                  (EVTAILCALL-*TEF _CONDVAR-24
                                   (EVAL_EXPR-*T TEST.ENV desc.else))))))))
  :HINTS ((ACL2::JUST-EXPAND ((EVAL_EXPR-*T-NO-ERROR ENV E)))
          (ACL2::JUST-EXPAND ((EVAL_EXPR-*T ENV E)))
          '(:IN-THEORY (ENABLE FGL::CONDITIONALIZE1 fgl::conditionalize2))))


;; ----------------------------------------------------------------------------------
;; Ty-oracle-val returns either a value or NIL, and the eval_expr of e_arbitrary case
;; checks whether the returned object is nil to see if there was an error. But for rewriting
;; (in FGL) it's better to check val-p, since otherwise we need to have a rule for
;; every value constructor that could be produced that it's non-nil.
(fgl::def-fgl-rewrite eval_expr-arbitrary-redef
  (implies (expr_desc-case (expr->desc e) :e_arbitrary)
           (equal (eval_expr env (fgl::concrete e))
                  (b* ((pos (expr->pos_start e))
                       ((e_arbitrary e) (expr->desc e))
                       ((mv (evo ty) orac) (resolve-ty env e.type))
                       ((mv val orac) (ty-oracle-val ty orac))
                       ((unless (val-p val))
                        (evo_error "DE_AET: " e (list pos))))
                    (evo_normal (expr_result val env)))))
  :hints (("Goal" :expand ((eval_expr env e)))))

(fgl::def-fgl-rewrite eval_expr-*t-arbitrary-redef
  (implies (expr_desc-case (expr->desc e) :e_arbitrary)
           (equal (eval_expr-*t env (fgl::concrete e))
                  (b* ((trace nil)
                       (pos (expr->pos_start e))
                       ((e_arbitrary desc) (expr->desc e))
                       (env (env-replace-static static-env env))
                       ((evoo-*t ty)
                        (resolve-ty-*t env desc.type))
                       ((mv val orac) (ty-oracle-val ty orac))
                       ((unless (val-p val))
                        (evo_error-*t "DE_AET: "
                                       desc (list pos))))
                    (evo_normal-*t (expr_result val env)))))
  :hints (("Goal" :expand ((eval_expr-*t env e)))))


(fgl::def-fgl-rewrite eval_expr-*t-when-error-free-arbitrary-redef
  (implies (and (eval_expr-*t-no-error env e)
                (expr_desc-case (expr->desc e) :e_arbitrary))
           (equal (eval_expr-*t env (fgl::concrete e))
                  (b* ((trace nil)
                       (pos (expr->pos_start e))
                       ((e_arbitrary desc) (expr->desc e))
                       (env (env-replace-static static-env env))
                       ((evoo-*tef _condvar-0 ty)
                        (resolve-ty-*t env desc.type))
                       ((mv val orac) (ty-oracle-val ty orac))
                       ((unless (val-p val))
                        (evo_error-*t "DE_AET: "
                                       desc (list pos))))
                    (evo_normal-*t (expr_result val env)))))
  :HINTS ((ACL2::JUST-EXPAND ((EVAL_EXPR-*T-NO-ERROR ENV E)))
          (ACL2::JUST-EXPAND ((EVAL_EXPR-*T ENV E)))
          (and stable-under-simplificationp
               (ACL2::JUST-EXPAND ((EVAL_EXPR-*T ENV E))))
          '(:IN-THEORY (ENABLE FGL::CONDITIONALIZE1 fgl::conditionalize2))))





;; ----------------------------------------------------------------------------------
;; Subprogram tracking
;; This uses a cool annotation trick to "trace" each subprogram the rewriter
;; descends into and returns from.  The annotation basically lets us keep track
;; of the trace depth and (most importantly) whether we've already applied this
;; rule to trace the current call of eval_subprogram, so that we can include
;; the LHS of this rule in the RHS without it looping.


(define find-previous-annotation-index ((tries natp) (n natp) fgl::interp-st)
  :hooks nil
  (if (zp tries)
      nil
    (b* ((annot (fgl::interp-st-nth-fn-annotation n 'eval_subprogram-fn nil fgl::interp-st))
         ((unless (fgl::fgl-object-case annot :g-concrete))
          (find-previous-annotation-index (1- tries) (1+ n) fgl::interp-st))
         (idx (ec-call (printed-annotation-index (fgl::g-concrete->val annot))))
         ((when idx) idx))
      (find-previous-annotation-index (1- tries) (1+ n) fgl::interp-st))))

;; NOTE: This (and other functions that use annotations) should be placed LAST
;; in the file relative to other rules rewriting eval_subprogram, so that they
;; will be tried first when rewriting an eval_subprogram call.
(fgl::def-fgl-rewrite eval_subprogram-print
  (implies (and (fgl::bind-fn-annotation annot 'eval_subprogram-fn)
                ;; make sure this call hasn't already been printed
                (not (printed-annotation-index annot))
                ;; make sure there is a previous call
                ;; (outermost call needs to be rewrittten by save-oracle-on-outermost-eval_subprogram)
                (fgl::syntax-bind prev-call (fgl::interp-st-scan-for-nth-fnsym-occ 0 1 'eval_subprogram-fn 'interp-st))
                (fgl::syntax-bind old-index (or (find-previous-annotation-index 10 1 'interp-st) 0))
                (equal index (+ 1 old-index)))
           (equal (eval_subprogram env fn params args)
                  (fgl::fgl-prog2
                   (fgl::syntax-interp (cw "~t0~x1> Eval_subprogram ~x2~%" (1+ index) index fn))
                   (let ((res (fgl::annotate `(:printed ,index)
                                             (eval_subprogram env fn params args))))
                     (fgl::fgl-prog2
                      (fgl::syntax-interp (cw "~t0<~x1 ~s2Eval_subprogram ~x3~%"
                                              (1+ index) index (if (fgl::interp-st->errmsg 'interp-st) "**" "") fn))
                      res))))))

(define find-previous-annotation-index-*t ((tries natp) (n natp) fgl::interp-st)
  :hooks nil
  (if (zp tries)
      nil
    (b* ((annot (fgl::interp-st-nth-fn-annotation n 'eval_subprogram-*t-fn nil fgl::interp-st))
         ((unless (fgl::fgl-object-case annot :g-concrete))
          (find-previous-annotation-index-*t (1- tries) (1+ n) fgl::interp-st))
         (idx (ec-call (printed-annotation-index (fgl::g-concrete->val annot))))
         ((when idx) idx))
      (find-previous-annotation-index-*t (1- tries) (1+ n) fgl::interp-st))))

;; NOTE: This (and other functions that use annotations) should be placed LAST
;; in the file relative to other rules rewriting eval_subprogram-*t, so that they
;; will be tried first when rewriting an eval_subprogram-*t call.
(fgl::def-fgl-rewrite eval_subprogram-*t-print
  (implies (and (fgl::bind-fn-annotation annot 'eval_subprogram-*t-fn)
                ;; make sure this call hasn't already been printed
                (not (printed-annotation-index annot))
                ;; make sure there is a previous call
                ;; (outermost call needs to be rewrittten by save-oracle-on-outermost-eval_subprogram-*t)
                (fgl::syntax-bind prev-call (fgl::interp-st-scan-for-nth-fnsym-occ 0 1 'eval_subprogram-*t-fn 'interp-st))
                (fgl::syntax-bind old-index (or (find-previous-annotation-index-*t 10 1 'interp-st) 0))
                (equal index (+ 1 old-index)))
           (equal (eval_subprogram-*t env fn params args)
                  (fgl::fgl-prog2
                   (fgl::syntax-interp (cw "~t0~x1> eval_subprogram-*t ~x2~%" (1+ index) index fn))
                   (let ((res (fgl::annotate `(:printed ,index)
                                             (eval_subprogram-*t env fn params args))))
                     (fgl::fgl-prog2
                      (fgl::syntax-interp
                       (let ((errmsg (fgl::interp-st->errmsg 'interp-st)))
                         (cw "~t0<~x1 ~s2eval_subprogram-*t ~x3 ~@4~%"
                                              (1+ index) index (if errmsg "**" "") fn
                                              (if errmsg
                                                  (if (msgp errmsg)
                                                      errmsg
                                                    (msg "~x0" errmsg))
                                                ""))))
                      res))))))

(defmacro fgl-reorder-eval_subprogram-*t-rules ()
  '(progn (fgl::remove-fgl-rewrites save-oracle-on-outermost-eval_subprogram-*t
                                    eval_subprogram-*t-print)
          (fgl::add-fgl-rewrites eval_subprogram-*t-print
                                 save-oracle-on-outermost-eval_subprogram-*t)))

;; NOTE: To enable crude profiling of symbolic evaluation of ASL functions,
;; enable this theorem in FGL instead of eval_subprogram-*t-print.
;; Then at the end of an FGL proof you can check how much time was spent in evaluation of
;; the various functions using, e.g., (time-tracker '|EVAL-HaveEL| :print? :min-time 0).
(defthm eval_subprogram-*t-print-with-time-tracking
  (implies (and (fgl::bind-fn-annotation annot 'eval_subprogram-*t-fn)
                ;; make sure this call hasn't already been printed
                (not (printed-annotation-index annot))
                ;; make sure there is a previous call
                ;; (outermost call needs to be rewrittten by save-oracle-on-outermost-eval_subprogram-*t)
                (fgl::syntax-bind prev-call (fgl::interp-st-scan-for-nth-fnsym-occ 0 1 'eval_subprogram-*t-fn 'interp-st))
                (fgl::syntax-bind old-index (or (find-previous-annotation-index-*t 10 1 'interp-st) 0))
                (equal index (+ 1 old-index))
                (syntaxp (stringp fn))
                (equal tag (intern-in-package-of-symbol
                            (concatenate 'string "EVAL-" fn)
                            'asl-pkg))
                (equal tracking-state
                       ;; The tracking table holds the state of the function wrt time tracking:
                       ;; if it has been seen before (therefore is initialized), then it is bound in
                       ;; the table, and if we're currently within a call of it, then it is bound to t in the table.
                       (fgl::syntax-bind
                        tracking-state1
                        (b* ((table (Cdr (hons-get 'time-tracking-table (fgl::interp-st->user-scratch 'fgl::interp-st))))
                             (look (hons-get tag table))
                             (table (if look table (hons-acons tag t table))))
                          (and (not look)
                               (fgl::interp-st-put-user-scratch 'time-tracking-table table 'fgl::interp-st))
                          (fgl::g-concrete
                           (list (not look) ;; needs init
                                 (cdr look) ;; inside call
                                 )))))
                (equal needs-init (car tracking-state))
                (equal inside-call (cadr tracking-state)))
           (equal (eval_subprogram-*t env fn params args)
                  (fgl::fgl-progn
                   (fgl::syntax-interp
                    (progn$ (and needs-init
                                 (progn$ (acl2::time-tracker tag :end)
                                         (acl2::time-tracker tag :init :times '(0))))
                            (and (not inside-call)
                                 (acl2::time-tracker tag :start))))
                   (fgl::syntax-interp (cw "~t0~x1> eval_subprogram-*t ~x2~%" (1+ index) index fn))
                   (let ((res (fgl::annotate `(:printed ,index)
                                             (eval_subprogram-*t env fn params args))))
                     (fgl::fgl-progn
                      (fgl::syntax-interp
                       (let ((errmsg (fgl::interp-st->errmsg 'interp-st)))
                         (cw "~t0<~x1 ~s2eval_subprogram-*t ~x3 ~@4~%"
                             (1+ index) index (if errmsg "**" "") fn
                             (if errmsg
                                 (if (msgp errmsg)
                                     errmsg
                                   (msg "~x0" errmsg))
                               ""))))
                      (fgl::syntax-interp
                       (and (not inside-call)
                            (progn$ (acl2::time-tracker tag :stop)
                                    (b* ((table (cdr (hons-get 'time-tracking-table
                                                               (fgl::interp-st->user-scratch 'fgl::interp-st)))))
                                      (fgl::interp-st-put-user-scratch
                                       'time-tracking-table (hons-acons tag nil table) 'fgl::interp-st)))))
                      res)))))
  :rule-classes nil)

(defun toggle-eval-time-tracking-fn (mode ;; :on, :off, or nil = toggle
                                     wrld)
  (b* ((rules (cdr (assoc 'eval_subprogram-*t-fn
                          (table-alist 'fgl::fgl-rewrite-rules wrld))))
       (tail (or (member-equal '(:formula eval_subprogram-*t-print) rules)
                 (member-equal '(:formula eval_subprogram-*t-print-with-time-tracking) rules)))
       ((unless tail)
        (er hard? 'toggle-eval-time-tracking-fn
            "Neither eval_subprogram-*t-print nor eval_subprogram-*t-print-with-time-tracking is enabled~%")
        '(value-triple :bad))
       (turn-off (equal (car tail) '(:formula eval_subprogram-*t-print-with-time-tracking)))
       ((when (or (and (eq mode :on) turn-off)
                  (and (eq mode :off) (not turn-off))))
        '(value-triple :no-op))
       (enable (if turn-off
                   '(:formula eval_subprogram-*t-print)
                 '(:formula eval_subprogram-*t-print-with-time-tracking)))
       (idx (- (len rules) (len tail)))
       (rules-to-remove (take (+ 1 idx) rules))
       (rules-to-add (reverse (take idx rules)))
       (ops (list `(fgl::remove-fgl-rewrites . ,rules-to-remove)
                  `(fgl::add-fgl-rewrite ,enable)
                  `(fgl::add-fgl-rewrites . ,rules-to-add))))
    (cons 'progn ops)))

(defmacro toggle-eval-time-tracking ()
  `(make-event (toggle-eval-time-tracking-fn nil (w state))))

(defmacro turn-on-eval-time-tracking ()
  `(make-event (toggle-eval-time-tracking-fn :on (w state))))

(defmacro turn-off-eval-time-tracking ()
  `(make-event (toggle-eval-time-tracking-fn :off (w state))))


(defun time-tracking-print-tags (tags)
  (if (atom tags)
      nil
    (progn$ (time-tracker (car tags) :print? :min-time 0)
            (time-tracking-print-tags (cdr tags)))))

(define time-tracking-print (&key (fgl::interp-st 'fgl::interp-st))
  :verify-guards nil
  (time-tracking-print-tags
   (set::mergesort (acl2::alist-keys (cdr (hons-get 'time-tracking-table
                                                    (fgl::interp-st->user-scratch fgl::interp-st)))))))



;; ----------------------------------------------------------------------------------
;; Wrapper to allow counterexamples to run eval_subprogram fast with a stobj.

;; We need to translate the fake "oracle" produced by the counterexample to a
;; real oracle stobj that eval_subprogram can execute with.


;; The "oracle" produced by a counterexample is a list of ty . val pairs. To
;; make this into a real oracle we run typed-val-to-oracle on each pair and
;; append them all.

(define ty-val-pairs-p (x)
  (if (atom x)
      t
    (and (consp (car x))
         (ty-p (caar x))
         (ty-resolved-p (caar x))
         (val-p (cdar x))
         (ty-satisfied (cdar x) (caar x))
         (ty-val-pairs-p (cdr x)))))

(define ty-val-pairs-to-orac-st ((x ty-val-pairs-p))
  :guard-hints (("Goal" :in-theory (enable ty-val-pairs-p)))
  (if (atom x)
      nil
    (append (typed-val-to-oracle (caar x) (cdar x))
            (ty-val-pairs-to-orac-st (cdr x)))))



(define constant-values->storage ((x literal-storage-p))
  :returns (storage val-imap-p)
  (if (atom x)
      nil
    (if (mbt (consp (car x)))
        (omap::update (identifier (caar x))
                      (v_of_literal (cdar x))
                      (constant-values->storage (cdr x)))
      (constant-values->storage (cdr x))))
  ///
  (local (in-theory (enable literal-storage-fix))))


;; BOZO move
#!omap
(define update*-fast ((new mapp)
                      (old mapp))
  :returns (res)
  :measure (+ (acl2-count new) (acl2-count old))
  (cond ((emptyp new) (mfix old))
        ((emptyp old) (mfix new))
        (t (mv-let (nkey nval)
               (head new)
             (mv-let (okey oval)
                 (head old)
               (cond ((equal okey nkey)
                      (cons (cons nkey nval)
                            (update*-fast (tail new) (tail old))))
                     ((<< okey nkey)
                      (cons (cons okey oval)
                            (update*-fast new (tail old))))
                     (t (cons (cons nkey nval)
                              (update*-fast (tail new) old))))))))
  ///
  (local (defret <<-caar
           (implies (and (implies (not (emptyp new))
                                  (<< x (caar new)))
                         (implies (not (emptyp old))
                                  (<< x (caar old)))
                         (not (and (emptyp new) (emptyp old))))
                    (<< x (caar res)))
           :hints (("Goal" :in-theory (enable emptyp mapp head tail)))))

  (defret mapp-res-of-<fn>
    (mapp res)
    :hints (("Goal" :in-theory (enable mapp emptyp head tail mfix))))

  (local (defund-nx ordered-before (key map)
           (implies (not (emptyp map))
                    (<< key (mv-nth 0 (head map))))))

  (local (defret <<-head
           (implies (and (ordered-before x new)
                         (ordered-before x old))
                    (ordered-before x res))
           :hints (("Goal" :in-theory (e/d (emptyp mapp head tail
                                                   ordered-before)
                                           (<<-caar))
                           :do-not-induct t
                           :use <<-caar))))



  (local (defthm acons-to-when-mapp
           (implies (and (mapp x)
                         (ordered-before k x))
                    (equal (cons (cons k v) x)
                           (update k v x)))
           :hints (("Goal" :in-theory (enable update ordered-before)))))

  (local (defthm head-ordered-before-tail
           (implies (and (not (emptyp x))
                         (equal k (mv-nth 0 (head x))))
                    (ordered-before k (tail x)))
           :hints (("Goal" :in-theory (enable emptyp mapp ordered-before head tail mfix)))))

  (local (defthm ordered-before-when-<<-head
           (implies (and (not (emptyp x))
                         (<< k (mv-nth 0 (head x))))
                    (ordered-before k x))
           :hints (("Goal" :in-theory (enable ordered-before)))))


  (local (defret assoc-when-<<
           (implies (<< k (mv-nth 0 (head x)))
                    (not (assoc k x)))
           :hints (("Goal" :in-theory (enable assoc mapp emptyp head tail mfix)))))

  (defret assoc-of-<fn>
    (equal (assoc key res)
           (or (assoc key new)
               (assoc key old)))
    :hints (("Goal" :in-theory (enable assoc ;; mapp emptyp head tail mfix
                                       ))))

  (defthm update*-fast-is-update*
    (equal (update*-fast new old) (update* new old))
    :hints (("Goal" :use ((:instance diff-key-when-unequal
                                     (x (update*-fast new old))
                                     (y (update* new old))))))))



(define storage-add-default-values ((storage val-imap-p) (config-env env-p))
  :returns (new-storage val-imap-p)
  (omap::update*-fast
   (val-imap-fix storage)
   (omap::update*-fast
    (global-env->storage (env->global config-env))
    (constant-values->storage (static_env_global->constant_values
                               (global-env->static
                                (env->global config-env)))))))

(define env-storage-add-default-values ((env env-p) (config-env env-p))
  :returns (new-env env-p)
  (B* (((env env))
       ((global-env env.global))
       (new-storage (storage-add-default-values env.global.storage config-env)))
    ;; (mv (change-env env :global (change-global-env env.global :storage new-storage)) orac)
    (change-env env :global (change-global-env env.global :storage new-storage))))



(define ctrex-fixup-storage ((storage val-imap-p)
                             (static-env static_env_global-p)
                             (config-env env-p))
  :returns (new-storage val-imap-p)
  (declare (ignorable static-env))
  (storage-add-default-values storage config-env))

(define ctrex-fixup-orac (orac-list)
  :non-executable t
  :verify-guards nil
  (let* ((orac (create-orac))
         (orac (acl2::update-oracle-mode 3 orac))
         (orac-st (ty-val-pairs-to-orac-st orac-list))
         (orac (acl2::update-oracle-st orac-st orac)))
    orac))




(define eval_subprogram-wrap ((env env-p)
                              (config-env env-p)
                              (name identifier-p)
                              (vparams vallist-p)
                              (vargs vallist-p)
                              (clk natp)
                              (orac-list))
  :guard (ty-val-pairs-p orac-list)
  :returns (mv ans new-orac-st)
  (b* ((orac-st (ty-val-pairs-to-orac-st orac-list))
       ((acl2::local-stobjs orac)
        (mv result new-orac-st orac))
       (orac (acl2::update-oracle-mode 3 orac))
       (orac (acl2::update-oracle-st orac-st orac))
       ;; Second hack: add default values for all global variables declared in
       ;; the static env but not present in the storage
       (env (env-storage-add-default-values env config-env))
       ((mv ans orac)
        (eval_subprogram env name vparams vargs))
       (new-orac-st (acl2::oracle-st orac)))
    (mv ans new-orac-st orac))
  ///
  )


(defthm static-env-of-env-storage-add-default-values
  (equal (global-env->static
          (env->global
           (env-storage-add-default-values env config-env)))
         (global-env->static (env->global env)))
  :hints (("Goal" :in-theory (enable env-storage-add-default-values))))

(fgl::def-fgl-rewrite integerp-of-val-imap-lookup
  (not (integerp (val-imap-lookup x env))))


(define eval_subprogram-*t-wrap ((env env-p)
                                 (config-env env-p)
                                 (static-env static_env_global-p)
                                 (name identifier-p)
                                 (vparams vallist-p)
                                 (vargs vallist-p)
                                 (clk natp)
                                 (orac-list)
                                 (pos)
                                 (tracespec))
  :guard (ty-val-pairs-p orac-list)
  :returns (mv ans new-orac-st trace)
  (b* ((orac-st (ty-val-pairs-to-orac-st orac-list))
       (pos (with-guard-checking :none (ec-call (posn-fix pos))))
       (tracespec (with-guard-checking :none (ec-call (tracespec-fix tracespec))))
       ((acl2::local-stobjs orac)
        (mv result new-orac-st trace orac))
       (orac (acl2::update-oracle-mode 3 orac))
       (orac (acl2::update-oracle-st orac-st orac))
       (env (env-replace-static static-env env))
       ;; Second hack: add default values for all global variables declared in
       ;; the static env but not present in the storage
       (env (env-storage-add-default-values env config-env))
       ((mv ans orac trace)
        (eval_subprogram-*t env name vparams vargs))
       (new-orac-st (acl2::oracle-st orac)))
    (mv ans new-orac-st trace orac))
  ///
  (local (defthm eval_subprogram-*t-mv-list
           (let ((ans (eval_subprogram-*t env name vparams vargs)))
             (equal (list (mv-nth 0 ans)
                          (mv-nth 1 ans)
                          (mv-nth 2 ans))
                    ans))
           :hints (("Goal" :expand ((eval_subprogram-*t env name vparams vargs))))))


  (make-event
   `(defthm eval_subprogram-*t-of-ctrex-fixup
      ;; (implies (EQUAL (EVAL_RESULT-KIND (TY-GLOBAL_DECL_KEYWORD-IMAP-RESOLVE-NAMES
      ;;                                    (STATIC_ENV_GLOBAL->STORAGE_TYPES STATIC-ENV)
      ;;                                    (STATIC_ENV_GLOBAL-FIX STATIC-ENV)))
      ;;                 :EV_NORMAL)
      (b* ((ans
            (eval_subprogram-*t
             (MAKE-ENV :GLOBAL (MAKE-GLOBAL-ENV :STATIC static-env
                                                :STORAGE (ctrex-fixup-storage storage static-env config-env))
                       :LOCAL ',(EMPTY-LOCAL-ENV))
             name vparams vargs
             :orac (ctrex-fixup-orac orac-list)))
           (orac-spec (hide (mv-nth 1 ans)))
           ((mv res-imp & trace-imp)
            (eval_subprogram-*t-wrap (MAKE-ENV :GLOBAL (MAKE-GLOBAL-ENV :STATIC static-env
                                                                        :STORAGE storage)
                                               :LOCAL (EMPTY-LOCAL-ENV))
                                     config-env
                                     static-env
                                     name vparams vargs clk orac-list
                                     pos tracespec)))
        (equal ans
               (mv res-imp orac-spec trace-imp)))
      :hints (("Goal" :in-theory (e/d (eval_subprogram-*t-wrap
                                       ctrex-fixup-orac
                                       ctrex-fixup-storage
                                       env-storage-add-default-values)
                                      (GLOBAL-ENV-UNDER-NONSTATIC-GLOBAL-ENV-EQUIV))
                      :expand ((:free (x) (hide x))))))))


(define eval_subprogram-*t-wrap-orac ((env env-p)
                                      (static-env static_env_global-p)
                                      (name identifier-p)
                                      (vparams vallist-p)
                                      (vargs vallist-p)
                                      (clk natp)
                                      (orac-list)
                                      (pos)
                                      (tracespec))
  :guard (ty-val-pairs-p orac-list)
  :returns (mv ans new-orac-st trace)
  (b* ((orac-st (ty-val-pairs-to-orac-st orac-list))
       (pos (with-guard-checking :none (ec-call (posn-fix pos))))
       (tracespec (with-guard-checking :none (ec-call (tracespec-fix tracespec))))
       ((acl2::local-stobjs orac)
        (mv result new-orac-st trace orac))
       (orac (acl2::update-oracle-mode 3 orac))
       (orac (acl2::update-oracle-st orac-st orac))
       (env (env-replace-static static-env env))
       ((mv ans orac trace)
        (eval_subprogram-*t env name vparams vargs))
       (new-orac-st (acl2::oracle-st orac)))
    (mv ans new-orac-st trace orac))
  ///
  (local (defthm eval_subprogram-*t-mv-list
           (let ((ans (eval_subprogram-*t env name vparams vargs)))
             (equal (list (mv-nth 0 ans)
                          (mv-nth 1 ans)
                          (mv-nth 2 ans))
                    ans))
           :hints (("Goal" :expand ((eval_subprogram-*t env name vparams vargs))))))


  (make-event
   `(defthm eval_subprogram-*t-of-orac-fixup
      (implies (equal (global-env->static (env->global env)) static-env)
               ;; (implies (EQUAL (EVAL_RESULT-KIND (TY-GLOBAL_DECL_KEYWORD-IMAP-RESOLVE-NAMES
               ;;                                    (STATIC_ENV_GLOBAL->STORAGE_TYPES STATIC-ENV)
               ;;                                    (STATIC_ENV_GLOBAL-FIX STATIC-ENV)))
               ;;                 :EV_NORMAL)
               (b* ((ans
                     (eval_subprogram-*t env name vparams vargs
                                         :orac (ctrex-fixup-orac orac-list)))
                    (orac-spec (hide (mv-nth 1 ans))))
                 (equal ans
                        (b* (((mv res-imp & trace-imp)
                              (eval_subprogram-*t-wrap-orac env static-env
                                                            name vparams vargs clk orac-list
                                                            pos tracespec)))
                          (mv res-imp orac-spec trace-imp)))))
      :hints (("Goal" :in-theory (e/d (eval_subprogram-*t-wrap-orac
                                       ctrex-fixup-orac
                                       env-storage-add-default-values)
                                      (GLOBAL-ENV-UNDER-NONSTATIC-GLOBAL-ENV-EQUIV))
               :expand ((:free (x) (hide x))))))))



(define storage-with-default-values ((x val-imap-p))
  :short "Compensate for our strange treatment of global storage in counterexamples."
  :long "
<p>We use a complicated treatment for the global storage of environments in
counterexamples, in order to try and satisfy the following goals:</p>
<ul>
<li>We want to be able to use global assumptions such as \"all global variables
have the correct types\" and \"all config, constant, and LET global variables
match their values from a correctly initialized storage state\".</li>

<li>We don't want to print all the irrelevant global variables when we show a
counterexample, just the ones that were used in the proof.</li>

<li>We still want to be able to run counterexamples correctly.</li>
</ul>

<p>When we generate a counterexample, we set up initial global storage
containing the variables according to what specific global variables are
referenced in the SAT counterexample. This results in the storage basically
containing those variables whose initial values are relevant to the proof, not
including the global assumptions of well-typedness and non-variables matchining
the initialized state.</p>

<p>When we run @('eval_subprogram')/@('eval_subprogram-*t') as part of the
counterexample, we use special treatment that initializes all other values of
storage according to the values from some initialized environment. The macro
@(see install-eval_subprogram-wrappers) determines which initialized
environment.</p>

<p>Now, this is generally adequate for proofs where we are examining some
results from the ASL function or some global variables that were written by the
function. But if we are comparing the resulting environment to a specification,
we will basically always have a mismatch: the specification will not include
the default values from the initialized environment whereas the one returned
from the implementation will.</p>

<p>Using this function or @(see env-with-storage-default-values) compensates
for this. Both are defined simply as fixing functions for their input types,
but @(see install-eval_subprogram-wrappers) causes them to add default values
from the initialized environment just as the eval_subprogram counterexample
implementations do.</p>"
  :returns (new-x val-imap-p)
  (val-imap-fix x))

(define env-with-storage-default-values-impl ((env env-p))
  (B* (((env env))
       ((global-env env.global))
       (new-storage (storage-with-default-values env.global.storage)))
    ;; (mv (change-env env :global (change-global-env env.global :storage new-storage)) orac)
    (change-env env :global (change-global-env env.global :storage new-storage))))


(define env-with-storage-default-values ((env env-p))
  :short "Full-env wrapper function for @(see storage-with-default-values) to compensate for our strange treatment of global storage in counterexamples."
  
  :returns (new-env env-p)
  (env-fix env)
  ///
  (make-event
   `(table fgl::magitastic-ev-definitions
           'env-with-storage-default-values
           (list ',(acl2::formals 'env-with-storage-default-values-impl (w state))
                 ',(acl2::body 'env-with-storage-default-values-impl nil (w state))))))
  


;; ----------------------------------------------------------------------------------
;; Bitvector concatenations -- the definition in eval_binop is not the best for FGL because
;; it does a logapp and then a loghead. Using limshift-loghead-of-logapp instead lets
;; us use the (usually) fixed width to limit the shift amount to something reasonable.
(fgl::def-fgl-rewrite eval_binop-bitvector-concat
  (implies (and (equal width (+ (nfix l1) (nfix l2)))
                (equal config (fgl::make-fgl-ipasir-config))
                (equal w-concrete (fgl::bind-var w-conc (fgl::find-evaluation width config)))
                (fgl::fgl-validity-check config (equal width w-concrete)))
           (equal (eval_binop :bv_concat (v_bitvector l1 v1) (v_bitvector l2 v2))
                  (ev_normal (v_bitvector w-concrete
                                          (bitops::limshift-loghead-of-logapp
                                           w-concrete l2 v2 v1)))))
  :hints (("Goal" :in-theory (enable bitops::loghead-of-logapp-split eval_binop))))



;; ----------------------------------------------------------------------------------
;; This rule only applies if we're about to have an if-then-else merge failure.
;; It checks whether we're in an reachable state (by checking if T is
;; satisfiable under the path condition), which causes FGL to backtrack to a
;; previous branch if it isn't. If we are in a reachable state, this rule
;; doesn't do anything and we'll have the if-then-else merge failure anyway.
(fgl::def-fgl-rewrite if-last-resort
  (implies (and (fgl::syntax-bind last-chancep
                                  (fgl::interp-flags->if-merge-last-chance (fgl::interp-st->flags fgl::interp-st)))
                (syntaxp (progn$ (cw "If merge last chance -- checking if unreachable~%") t))
                (fgl::fgl-sat-check (fgl::make-fgl-ipasir-config) t))
           (equal (if test x y)
                  (fgl::abort-rewrite (if test x y)))))




(define for_loop-test+ ((v_start integerp)
                        (v_end integerp)
                        (dir for_direction-p)
                        (count natp)
                        (pos posn-p))
  (declare (ignore count pos))
  (if (eq (for_direction-fix dir) :up)
      (< (lifix v_end) (lifix v_start))
      (> (lifix v_end) (lifix v_start)))
  ///
  (defthm for_loop-test+-in-terms-of-for_loop-test
    (equal (for_loop-test+ v_start v_end dir count pos)
           (for_loop-test v_start v_end dir))
    :hints (("Goal" :in-theory (enable for_loop-test)))))



;; Adds a count to eval_for-*t so that we can track how many times we've called
;; it. See for_loop-test+-fgl-special comment below for one use
(define eval_for-*t-count ((count natp)
                           (env env-p)
                           (index_name identifier-p)
                           (limit acl2::maybe-integerp)
                           (v_start integerp)
                           (dir for_direction-p)
                           (v_end integerp)
                           (body stmt-p)
                           &key ((clk natp) 'clk)
                           (orac 'orac)
                           ((static-env static_env_global-p)
                            'static-env)
                           ((tracespec tracespec-p) 'tracespec))
  (declare (ignorable count))
  :prepwork ((local (in-theory (acl2::enable* asl-*t-equals-original-rules))))
  :guard (equal (global-env->static (env->global env))
                static-env)
  :verify-guards nil
  :measure (nats-measure clk 0 (stmt-count* body)
                         (+ 1 (for_loop-measure v_start v_end dir)))
  :returns (mv (res stmt_eval_result-p)
               new-orac (trace asl-tracelist-p))
  (bind-env-with-static
   (evbody-*t (b* (((when (for_loop-test+ v_start v_end dir count (stmt->pos_start body)))
                    (evo_normal-*t (continuing env)))
                   ((evo-*t limit1)
                    (tick_loop_limit limit))
                   ((evs-*t env1) (eval_block-*t env body))
                   ((mv v_step env2)
                    (eval_for_step env1 index_name v_start dir)))
                (evtailcall-*t (eval_for-*t-count (+ 1 (lnfix count)) env2 index_name
                                                  limit1 v_step dir v_end body)))))
  ///
  (defthm eval_for-*t-count-in-terms-of-eval_for-*t
    (equal (eval_for-*t-count count env index_name limit v_start dir v_end body)
           (eval_for-*t env index_name limit v_start dir v_end body))
    :hints (("Goal" :induct (eval_for-*t-count count env index_name limit v_start dir v_end body)
                    :expand ((eval_for-*t-count count env index_name limit v_start dir v_end body)
                             (eval_for-*t env index_name limit v_start dir v_end body)))))

  (verify-guards eval_for-*t-count-fn)

  (fgl::def-fgl-rewrite eval_for-*t-in-terms-of-eval_for-*t-count
    (equal (eval_for-*t env index_name limit v_start dir v_end body)
           (eval_for-*t-count 0 env index_name limit v_start dir v_end body))))

(define eval_for-*t-count-no-error ((count natp)
                                    (env env-p)
                                    (index_name identifier-p)
                                    (limit acl2::maybe-integerp)
                                    (v_start integerp)
                                    (dir for_direction-p)
                                    (v_end integerp)
                                    (body stmt-p)
                                    &key ((clk natp) 'clk)
                                    (orac 'orac)
                                    ((static-env static_env_global-p)
                                     'static-env)
                                    ((tracespec tracespec-p) 'tracespec))
  :verify-guards nil
  :non-executable t
  :enabled t
  (not (eval_result-case (mv-nth 0 (eval_for-*t-count count env index_name limit v_start dir v_end body))
         :ev_error))
  ///
  (fgl::remove-fgl-rewrite eval_for-*t-count-no-error)

  (fgl::def-fgl-rewrite eval_for-*t-no-error-in-terms-of-count
    (implies (eval_for-*t-no-error env index_name limit v_start dir v_end body)
             (eval_for-*t-count-no-error 0 env index_name limit v_start dir v_end body)))

  (fgl::def-fgl-rewrite eval_for-*t-count-when-error-free
    (implies (eval_for-*t-count-no-error count env index_name limit v_start dir v_end body)
             (equal (eval_for-*t-count count env index_name limit v_start dir v_end body)
                    (bind-env-with-static
                     (evbody-*t (b* (((when (for_loop-test+ v_start v_end dir count (stmt->pos_start body)))
                                      (evo_normal-*t (continuing env)))
                                     ((evo-*tef _condvar-0 limit1)
                                      (tick_loop_limit limit))
                                     ((evs-*tef _condvar-1 env1) (eval_block-*t env body))
                                     ((mv v_step env2)
                                      (eval_for_step env1 index_name v_start dir)))
                                  (evtailcall-*tef _condvar-2
                                                   (eval_for-*t-count (+ 1 (lnfix count)) env2 index_name
                                                                      limit1 v_step dir v_end body)))))))
    :hints (("goal" :expand ((eval_for-*t-count count env index_name limit v_start dir v_end body))
             :in-theory (e/d (fgl::conditionalize1 fgl::conditionalize2)
                             (eval_for-*t-count-in-terms-of-eval_for-*t))))))

(fgl::def-fgl-rewrite for_loop-test+-fgl
  (equal (for_loop-test+ v_start v_end dir count posn)
         (b* ((dir (for_direction-fix dir))
              (ans (if (eq dir :up)
                       (< (lifix v_end) (lifix v_start))
                     (> (lifix v_end) (lifix v_start))))
              (conf (fgl::make-fgl-ipasir-config)))
           (fgl::fgl-validity-check conf ans)))
  :hints (("Goal" :in-theory (enable for_loop-test))))

;; (fgl::def-fgl-rewrite for_loop-test+-fgl-special
;;   (equal (for_loop-test+ v_start v_end dir count posn)
;;          (b* ((dir (for_direction-fix dir))
;;               (ans (if (eq dir :up)
;;                        (< (lifix v_end) (lifix v_start))
;;                      (> (lifix v_end) (lifix v_start))))
;;               (conf  (fgl::make-fgl-ipasir-config))
;;               (valid
;;                (fgl::fgl-validity-check conf ans))
;;               ((when (fgl::syntax-bind validp (eq valid t)))
;;                valid)
;;               ((unless (and (equal posn ...)
;;                             (< 16 count)))
;;                valid))
;;            (fgl::fgl-progn
;;             (counterexample-with-backtrace conf "For loop test counterexample")
;;             valid))))

(fgl::remove-fgl-rewrite for_loop-test+)


;; In the case where we want to check that only certain variables are modified,
;; we have hyps like (and (stringp s) (not (member-equal s '("_R" "PSTATE")))).
;; We then need a way to generate a good value for s on a counterexample. This
;; is not a good rule, but it's good enough for our current purposes.  Since
;; it's a fixup rule (low priority), it shouldn't mess up anything that has a
;; better rule for it.
(fgl::def-ctrex-rule stringp-fixup-to-some-string
  :match ((pred (stringp x)))
  :assign-cond pred
  :assigned-var x
  :assign "some-string"
  :ruletype :fixup)



;; ----------------------------------------------------------------------------------
;; Misc
(defcong iff equal (v_bool x) 1)
(fgl::add-fgl-congruence iff-implies-equal-v_bool-1)
(memoize 'fgl::fgl-object-p)
(memoize 'fgl::fgl-object-bfrs-ok-fn)
(memoize 'resolve-storage-types)
(memoize 'ty-timeframe-imap-p)
(memoize 'cmr::lambda-nest-to-bindinglist)
(memoize 'fgl::fgl-function-mode-lookup)
(memoize 'trace-free-ty-timeframe-imap-p-fn)
(memoize 'trace-free-fnname-p-fn)
(memoize 'val-imap-p :recursive nil)
(memoize 'global-env-p)

(encapsulate
  nil

  (local (include-book "ihs/quotient-remainder-lemmas" :dir :system))

  (fgl::def-fgl-rewrite eval_binop-of-div
    (implies (and (val-case v1 :v_int)
                  (val-case v2 :v_int))
             (equal (eval_binop :div v1 v2)
                    (b* (((v_int v1))
                         ((v_int v2)))
                      (if (and (< 0 v2.val)
                               (eql (mod v1.val v2.val) 0))
                          (ev_normal (v_int (floor v1.val v2.val)))
                          (ev_error "Unsupported binop" (list :div
                                                              (val-fix v1)
                                                              (val-fix v2))
                                    nil)))))
    :hints (("Goal" :in-theory (e/d (eval_binop) (floor mod))))))





;; ----------------------------------------------------------------------------------
;; In floating-point operations in particular (but maybe other cases), it seems
;; important to narrow down the possible values of slice indices and vector
;; lengths.  The following scheme lets you wrap enumerate-possible-values
;; around some symbolic value, which changes the form of the symbolic value
;; (say x) into a nesting of IFS of the form:
;; (if (equal x const0)
;;     const0
;;   (if (equal x const1)
;;       const1
;;     ... (if (equal x constn-1) constn-1 constn) ...))
;; Critically, when x is a symbolic integer, this results in a symbolic integer
;; that has accurate bit length and signedness. We apply this to bitvector
;; widths on concatenation operations and on slice indices in slices_sub.
;;
;; This might not always be a good strategy. Hopefully it will be easy enough
;; to notice when we get bogged down in enumerate-possible-values and turn off
;; the triggering rule, or more drastically, enable the definition of
;; enumerate-possible-values to cancel all uses of it.
(define enumerate-possible-values-aux (sat-config limit x)
  (declare (ignorable sat-config limit))
  :enabled t
  x
  ///
  (fgl::remove-fgl-rewrite enumerate-possible-values-aux)

  (fgl::def-fgl-rewrite enumerate-possible-values-aux-def
    (equal (enumerate-possible-values-aux sat-config limit x)
           (b* (((when (zp limit))
                 (fgl::fgl-prog2 (cw "enumerate-possible-values limit exceeded~%")
                                 x))
                (?ok ;; (fgl::fgl-progn (cw "enumerate-possible-values ~x0~%" limit)
                 (fgl::fgl-sat-check sat-config t))
                ((list (list error bindings ?vars) &)
                 (fgl::bind-var ctrex-val (fgl::get-counterexample sat-config)))
                ((when error)
                 (fgl::fgl-prog2 (cw "enumerate-possible-values error: ~x0~%" error)
                                 x))
                (x-value (cdr (assoc 'x bindings)))
                (res
                 (if (equal x x-value)
                     x-value
                   (enumerate-possible-values-aux sat-config (1- limit) x))))
             ;; (fgl::fgl-progn (fgl::syntax-interp (cw "enumerate-possible-values res: ~x0~%" res))
             res)))
  )

(define enumerate-possible-values (sat-config limit x)
  (declare (ignorable sat-config limit))
  :enabled t
  x
  ///
  (fgl::remove-fgl-rewrite enumerate-possible-values-aux)

  (fgl::def-fgl-rewrite enumerate-possible-values-def
    (equal (enumerate-possible-values sat-config limit x)
           (fgl::fgl-prog2 (fgl::syntax-interp (cw "Enumerate-possible-values input: ~x0~%" x))
                           (let ((res (enumerate-possible-values-aux sat-config limit x)))
                             (fgl::fgl-prog2 (fgl::syntax-interp (cw "Enumerate-possible-values result: ~x0~%" res))
                                             res))))))

(fgl::def-fgl-rewrite slices_sub-enumerate-slices
  (equal (slices_sub srcval vslices)
         (IF (ATOM VSLICES)
             (INTPAIR 0 0)
             (B* (((intpair FIRST_VSLICE)
                   (enumerate-possible-values
                    (fgl::make-fgl-ipasir-config) 10 (CAR VSLICES)))
                  (REST (CDR VSLICES))
                  ((INTPAIR DSTVAL_REST)
                   (SLICES_SUB SRCVAL REST))
                  (START (NFIX FIRST_VSLICE.FIRST))
                  (LEN (NFIX FIRST_VSLICE.SECOND))
                  (SRCPART (PART-SELECT SRCVAL
                                        :LOW START
                                        :WIDTH LEN))
                  (VAL (LOGAPP DSTVAL_REST.FIRST
                               DSTVAL_REST.SECOND SRCPART)))
               (INTPAIR (+ LEN DSTVAL_REST.FIRST)
                        VAL))))
  :hints (("Goal" :expand ((slices_sub srcval vslices)))))

(fgl::def-fgl-rewrite bv_concat-enumerate-widths
  (equal (eval_binop :bv_concat v1 v2)
         (if (and (val-case v1 :v_bitvector)
                  (val-case v2 :v_bitvector))
             (b* (((v_bitvector v1))
                  ((v_bitvector v2))
                  (sat-config (fgl::make-fgl-ipasir-config))
                  (v2.len
                   (enumerate-possible-values sat-config 20 v2.len))
                  (sum
                   (enumerate-possible-values sat-config 20 (+ v1.len v2.len))))
               (ev_normal (v_bitvector sum (logapp v2.len v2.val v1.val))))
           (ev_error "Unsupported binop" (list :bv_concat (val-fix v1) (val-fix v2)) nil)))
  :hints (("Goal" :expand ((eval_binop :bv_concat v1 v2)))))

(fgl::def-fgl-rewrite shl-enumerate-shft
  (equal (eval_binop :shl v1 v2)
         (if (and (val-case v1 :v_int)
                  (val-case v2 :v_int)
                  (<= 0 (v_int->val v2)))
             (b* (((v_int v1))
                  ((v_int v2))
                  (sat-config (fgl::make-fgl-ipasir-config))
                  (v2.val (enumerate-possible-values sat-config 20 v2.val)))
               (ev_normal (v_int (ash v1.val v2.val))))
             (ev_error "Unsupported binop"
                       (list :shl (val-fix v1) (val-fix v2))
                       nil)))
  :hints (("Goal" :expand ((eval_binop :shl v1 v2)))))

(fgl::def-fgl-rewrite part-install-enumerate
  (equal (bitops::part-install val x :width width :low low)
         (b* ((sat-config (fgl::make-fgl-ipasir-config))
              (width (enumerate-possible-values sat-config 20 width))
              (low (enumerate-possible-values sat-config 20 low))
              (sum (enumerate-possible-values sat-config 20 (+ (nfix low) (nfix width)))))
           (logapp low x (logapp width val (logtail sum x)))))
  :hints (("Goal" :in-theory (e/d (bitops::part-install-in-terms-of-logapp)
                                  (logapp bitops::part-install)))))

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

(include-book "../trace-interp")
(include-book "../termination-error")
(include-book "std/util/defret-mutual-generate" :dir :System)
(include-book "centaur/fgl/def-fgl-rewrite" :dir :system)
(local (include-book "centaur/vl/util/default-hints" :dir :system))
(local (std::add-default-post-define-hook :fix))



(defthm termination-error-p-of-rethrow_implicit
  (equal (termination-error-p (rethrow_implicit throw blkres backtrace))
         (termination-error-p blkres))
  :hints(("Goal" :in-theory (enable rethrow_implicit))))



(encapsulate nil
  
  (local (in-theory (acl2::e/d* (call-abort-after
                                 stmt-abort-after)
                                (asl-*t-equals-original-rules
                                 env-replace-static-with-self
                                 acl2::append-to-nil
                                 append
                                 true-listp
                                 set::sets-are-true-lists-cheap
                                 cons-equal
                                 not xor floor mod loghead
                                 eval_result-fix-when-eval_result-p
                                 eql
                                 (tau-system)
                                 (:rules-of-class :congruence :here)
                                 (:rules-of-class :type-prescription :here)))))
  (with-output
  ;; makes it so it won't take forever to print the induction scheme
  :evisc (:gag-mode (evisc-tuple 3 4 nil nil))
  (std::defret-mutual-generate <fn>-clock-independent
    :rules ((t (:add-concl (let ((call <call>)
                                 (call2 (let ((clk (+ n (nfix clk)))) <call>)))
                             (implies (and (not (termination-error-p (mv-nth 0 call)))
                                           (natp n))
                                      (equal call2 call))))
               (:add-keyword     :rule-classes nil)))
    :hints ((vl::big-mutrec-default-hint 'eval_expr-*t-fn id nil world))
    :mutual-recursion asl-interpreter-mutual-recursion-*t)))


(acl2::def-ruleset! normalizes-clock-when-terminates nil)
(acl2::def-ruleset! interp-*t-clock-functions nil)
(acl2::def-ruleset! interp-*t-terminates-functions nil)
(acl2::def-ruleset! forward-normalize-clock-when-terminates nil)
(acl2::def-ruleset! forward-normalize-clock-when-normal nil)
(acl2::def-ruleset! split-on-tracespecs nil)

(local (defthmd not-termination-error-when-trace-abort
         (implies (equal (ev_error->desc res) "Trace abort")
                  (not (termination-error-p res)))
         :hints(("Goal" :in-theory (enable termination-error-p)))))

;; (defthm eval_expr-*t-not-termination-error-p-due-to-tracespec
;;   (implies (and (syntaxp (not (equal tracespec ''(nil nil nil nil))))
;;                 (not (termination-error-p (mv-nth 0 (eval_expr-*t env e :tracespec (make-tracespec))))))
;;            (not (termination-error-p (mv-nth 0 (eval_expr-*t env e)))))
;;   :hints (("goal" :in-theory (acl2::enable* asl-*t-equals-original-rules
;;                                             not-termination-error-when-trace-abort)
;;            :cases ((and (eval_result-case (mv-nth 0 (eval_expr-*t env e))
;;                           :ev_error)
;;                         (equal (ev_error->desc (mv-nth 0 (eval_expr-*t env e))) "Trace abort"))))))

(local (defthmd error-when-termination-error-p
         (implies (termination-error-p x)
                  (equal (eval_result-kind x) :ev_error))
         :hints(("Goal" :in-theory (enable termination-error-p)))))

;; We have two special cases for results, both of which are ev_errors -- trace
;; abort and termination error.  When not a trace abort, we want to normalize
;; away the tracespec for result and output oracle forms.  When not a
;; termination error (and tracespec is normalized away), we want to also
;; normalize away the clock.

;; - When not a trace abort, we can normalize away the tracespec (except for trace output):
;;      <evalfn>-basic-tracespec-normalize
;; - When the tracespec-free form is not a termination error, we can normalize
;;      away the clock (even if we do have a tracespec):
;;      <evalfn>-normalizes-clock-when-terminates.

;; The problem with both of these is that various forms of hyps can imply these
;; conditions (not a trace abort/not a termination error), and


;; cases:
;; Not trace abort or termination error: we can normalize away tracespec and clock (except for trace output).
;; Termination error: we can normalize away the tracespec
;; Trace abort: we can't 


;; An annoying problem here is that the hyps that establish that

(defconst *interp-clock-and-termination-template*
  '(progn
     (defthmd <evalfn>-basic-tracespec-normalize
       (b* (((mv res orac-out &) (<evalfn> <formals>))
            ((mv res-nt orac-out-nt &) (<evalfn> <formals> :tracespec (make-tracespec))))
         (implies (and (syntaxp (not (equal tracespec ''(nil nil nil nil))))
                       (not (equal (ev_error->desc res) "Trace abort")))
                  (and (equal res res-nt)
                       (equal orac-out orac-out-nt))))
       :hints (("goal" :in-theory (acl2::disable* asl-*t-equals-original-rules)
                :expand ((:free (x) (hide x)))
                :use ((:instance <evalfn>-equals-original (tracespec (make-tracespec)))
                      (:instance <evalfn>-equals-original)))))
     (acl2::add-to-ruleset split-on-tracespecs <evalfn>-basic-tracespec-normalize)

     (defthm <evalfn>-tracespec-normalize-forward-trace-abort
       (implies (not (equal (ev_error->desc (mv-nth 0 (<evalfn> <formals>))) "Trace abort"))
                (not (equal (ev_error->desc (mv-nth 0 (<evalfn> <formals>
                                                                :tracespec '(nil nil nil nil)
                                                                <dummy-pos>))) "Trace abort")))
       :hints (("goal" :use <evalfn>-basic-tracespec-normalize
                :in-theory (disable <evalfn>-basic-tracespec-normalize)))
       :rule-classes :forward-chaining)

     (defthm <evalfn>-tracespec-normalize-forward-non-error
       (implies (not (equal (eval_result-kind (mv-nth 0 (<evalfn> <formals>))) :ev_error))
                (not (equal (eval_result-kind (mv-nth 0 (<evalfn> <formals>
                                                                  :tracespec '(nil nil nil nil)
                                                                  <dummy-pos>))) :ev_error)))
       :hints (("goal" :use <evalfn>-basic-tracespec-normalize
                :in-theory (disable <evalfn>-basic-tracespec-normalize)))
       :rule-classes :forward-chaining)

     (defthm <evalfn>-tracespec-normalize-forward-normal
       (implies (equal (eval_result-kind (mv-nth 0 (<evalfn> <formals>))) :ev_normal)
                (equal (eval_result-kind (mv-nth 0 (<evalfn> <formals>
                                                             :tracespec '(nil nil nil nil)
                                                             <dummy-pos>))) :ev_normal))
       :hints (("goal" :use <evalfn>-basic-tracespec-normalize
                :in-theory (disable <evalfn>-basic-tracespec-normalize)))
       :rule-classes :forward-chaining)

     (defthm <evalfn>-tracespec-normalize-forward-throwing
       (implies (equal (eval_result-kind (mv-nth 0 (<evalfn> <formals>))) :ev_throwing)
                (equal (eval_result-kind (mv-nth 0 (<evalfn> <formals>
                                                             :tracespec '(nil nil nil nil)
                                                             <dummy-pos>))) :ev_throwing))
       :hints (("goal" :use <evalfn>-basic-tracespec-normalize
                :in-theory (disable <evalfn>-basic-tracespec-normalize)))
       :rule-classes :forward-chaining)

     (defthm <evalfn>-tracespec-normalize-forward-termination-error
       (implies (termination-error-p (mv-nth 0 (<evalfn> <formals>)))
                (termination-error-p (mv-nth 0 (<evalfn> <formals>
                                                         :tracespec '(nil nil nil nil)
                                                         <dummy-pos>))))
       :hints (("goal" :use <evalfn>-basic-tracespec-normalize
                :in-theory (disable <evalfn>-basic-tracespec-normalize)))
       :rule-classes :forward-chaining)

     
     ;; (defthmd <evalfn>-split-on-tracespec
     ;;   (b* (((mv res orac-out &) (<evalfn> <formals>))
     ;;        ((mv res-nt orac-out-nt &) (<evalfn> <formals> :tracespec (make-tracespec))))
     ;;     (implies (and (syntaxp (not (equal tracespec ''(nil nil nil nil))))
     ;;                   (case-split (not (equal (ev_error->desc res) "Trace abort"))))
     ;;              (and (equal res res-nt)
     ;;                   (equal orac-out orac-out-nt))))
     ;;   :hints (("goal" :in-theory (acl2::disable* asl-*t-equals-original-rules)
     ;;            :expand ((:free (x) (hide x)))
     ;;            :use ((:instance <evalfn>-equals-original (tracespec (make-tracespec)))
     ;;                  (:instance <evalfn>-equals-original)))))

     ;; (acl2::add-to-ruleset split-on-tracespecs <evalfn>-split-on-tracespec)

     
     (defthmd <evalfn>-not-termination-error-p-due-to-tracespec
       (implies (and (syntaxp (not (equal tracespec ''(nil nil nil nil))))
                     (not (termination-error-p (mv-nth 0 (<evalfn> <formals> :tracespec (make-tracespec))))))
                (not (termination-error-p (mv-nth 0 (<evalfn> <formals>)))))
       :hints (("goal" :in-theory (acl2::enable* asl-*t-equals-original-rules
                                                 not-termination-error-when-trace-abort)
                :cases ((and (eval_result-case (mv-nth 0 (<evalfn> <formals>))
                               :ev_error)
                             (equal (ev_error->desc (mv-nth 0 (<evalfn> <formals>))) "Trace abort"))))))

     (defthmd <evalfn>-not-normal-due-to-tracespec
       (implies (and (syntaxp (not (equal tracespec ''(nil nil nil nil))))
                     (not (eval_result-case (mv-nth 0 (<evalfn> <formals> :tracespec (make-tracespec))) :ev_normal)))
                (not (equal (eval_result-kind (mv-nth 0 (<evalfn> <formals>))) :ev_normal)))
       :hints (("goal" :in-theory (acl2::disable* asl-*t-equals-original-rules)
                :use ((:instance <evalfn>-equals-original (tracespec (make-tracespec)))
                      (:instance <evalfn>-equals-original)))))

     (defthm <evalfn>-normalize-res-tracespec-when-normal
       (implies (and (syntaxp (not (equal tracespec ''(nil nil nil nil))))
                     (not (eval_result-case (mv-nth 0 (<evalfn> <formals> :tracespec (make-tracespec))) :ev_normal)))
                (not (equal (eval_result-kind (mv-nth 0 (<evalfn> <formals>))) :ev_normal)))
       :hints (("goal" :in-theory (acl2::disable* asl-*t-equals-original-rules)
                :use ((:instance <evalfn>-equals-original (tracespec (make-tracespec)))
                      (:instance <evalfn>-equals-original)))))
     
     (defchoose <evalfn>-clock1 (clk) (<formals> orac static-env)
       (and (natp clk)
            (not (termination-error-p (mv-nth 0 (<evalfn> <formals> :tracespec (make-tracespec)
                                                          <dummy-pos>))))))

     (define <evalfn>-clock (<formals-types> &key (orac 'orac) (static-env 'static-env))
       :returns (clk natp :rule-classes :type-prescription)
       :hooks nil
       (nfix (non-exec (<evalfn>-clock1 <formals> orac static-env)))
       ///
       (defthm <evalfn>-clock-when-terminates
         (implies (not (termination-error-p (mv-nth 0 (<evalfn> <formals> :tracespec '(nil nil nil nil)))))
                  (not (termination-error-p (mv-nth 0 (<evalfn> <formals> :clk (<evalfn>-clock <formals>))))))
         :hints (("goal" :use ((:instance <evalfn>-clock1 (clk (nfix clk))))
                  :in-theory (enable <evalfn>-not-termination-error-p-due-to-tracespec)))))

     (fgl::remove-fgl-rewrite <evalfn>-clock)

     (acl2::add-to-ruleset interp-*t-clock-functions <evalfn>-clock)

     (define <evalfn>-terminates (<formals-types> &key (orac 'orac) (static-env 'static-env))
       :hooks nil
       (let ((clk (non-exec (<evalfn>-clock <formals>))))
         (not (termination-error-p (non-exec (mv-nth 0 (<evalfn> <formals> :tracespec (make-tracespec) <dummy-pos>))))))
       ///
       (defthmd not-<evalfn>-terminates-implies
         (implies (not (<evalfn>-terminates <formals>))
                  (termination-error-p (mv-nth 0 (<evalfn> <formals> :tracespec (make-tracespec)))))))

     (fgl::remove-fgl-rewrite <evalfn>-terminates)
     
     (acl2::add-to-ruleset interp-*t-terminates-functions <evalfn>-terminates)

     ;; (fgl::remove-fgl-rewrites <evalfn>-clock)
     
     (defthmd <evalfn>-normalizes-clock-when-terminates
       (implies (and (syntaxp (not (and (consp clk)
                                        (eq (car clk) '<evalfn>-clock-fn)
                                        (equal (cdr clk) (list <formals> orac static-env)))))
                     (not (termination-error-p (mv-nth 0 (<evalfn> <formals> :tracespec (make-tracespec))))))
                (equal (<evalfn> <formals>)
                       (<evalfn> <formals> :clk (<evalfn>-clock <formals>))))
       :hints (("goal" :use ((:instance <evalfn>-clock-independent
                              (clk (<evalfn>-clock <formals>))
                              (n (- (nfix clk) (<evalfn>-clock <formals>))))
                             (:instance <evalfn>-clock-independent
                              (clk (nfix clk))
                              (n (- (<evalfn>-clock <formals>) (nfix clk)))))
                :in-theory (enable <evalfn>-not-termination-error-p-due-to-tracespec))))

     (acl2::add-to-ruleset normalizes-clock-when-terminates
                           <evalfn>-normalizes-clock-when-terminates)

     (defthm <evalfn>-not-termination-error-forward-normalize-clock
       (implies (not (termination-error-p
                      (mv-nth 0 (<evalfn> <formals> :tracespec '(nil nil nil nil)))))
                (not (termination-error-p
                      (mv-nth 0 (<evalfn> <formals> :clk (<evalfn>-clock <formals>) :tracespec '(nil nil nil nil))))))
       :rule-classes :forward-chaining)
     
     (defthm <evalfn>-normal-forward-normalize-clock
       (implies (equal (eval_result-kind
                        (mv-nth 0 (<evalfn> <formals> :tracespec '(nil nil nil nil))))
                       :ev_normal)
                (equal (eval_result-kind
                        (mv-nth 0 (<evalfn> <formals> :clk (<evalfn>-clock <formals>) :tracespec '(nil nil nil nil))))
                       :ev_normal))
       :hints (("goal"
                :in-theory (enable error-when-termination-error-p)
                :use ((:instance <evalfn>-normalizes-clock-when-terminates
                       (tracespec '(nil nil nil nil))))))
       :rule-classes :forward-chaining)

     (defthm <evalfn>-throwing-forward-normalize-clock
       (implies (equal (eval_result-kind
                        (mv-nth 0 (<evalfn> <formals> :tracespec '(nil nil nil nil))))
                       :ev_throwing)
                (equal (eval_result-kind
                        (mv-nth 0 (<evalfn> <formals> :clk (<evalfn>-clock <formals>) :tracespec '(nil nil nil nil))))
                       :ev_throwing))
       :hints (("goal" :use ((:instance <evalfn>-normalizes-clock-when-terminates
                              (tracespec '(nil nil nil nil)))
                             (:instance <evalfn>-equals-original (tracespec (make-tracespec)))
                             ;; (:instance <evalfn>-equals-original)
                             )
                :in-theory (acl2::disable* asl-*t-equals-original-rules)))
       :rule-classes :forward-chaining)

     (defthm <evalfn>-not-error-forward-normalize-clock
       (implies (not (equal (eval_result-kind
                             (mv-nth 0 (<evalfn> <formals> :tracespec '(nil nil nil nil))))
                            :ev_error))
                (not (equal (eval_result-kind
                             (mv-nth 0 (<evalfn> <formals> :clk (<evalfn>-clock <formals>) :tracespec '(nil nil nil nil))))
                            :ev_error)))
       :hints (("goal" :use ((:instance <evalfn>-normalizes-clock-when-terminates
                              (tracespec '(nil nil nil nil))))))
       :rule-classes :forward-chaining)

     (defthm <evalfn>-trace-abort-forward-normalize-clock
       (implies (equal (ev_error->desc
                        (mv-nth 0 (<evalfn> <formals> :tracespec '(nil nil nil nil))))
                       "Trace abort")
                (equal (ev_error->desc
                        (mv-nth 0 (<evalfn> <formals> :clk (<evalfn>-clock <formals>) :tracespec '(nil nil nil nil))))
                       "Trace abort"))
       :hints (("goal" :use ((:instance <evalfn>-normalizes-clock-when-terminates
                              (tracespec '(nil nil nil nil))))))
       :rule-classes :forward-chaining)

     (acl2::add-to-ruleset forward-normalize-clock-when-normal
                           '(<evalfn>-normal-forward-normalize-clock
                             <evalfn>-throwing-forward-normalize-clock
                             <evalfn>-not-error-forward-normalize-clock))
     
     ;; (defthmd <evalfn>-normalizes-clock-when-terminates-normal-assum
     ;;   (implies (and (syntaxp
     ;;                  (or (acl2::rewriting-negative-literal-fn
     ;;                       `(equal (eval_result-kind$inline
     ;;                                (mv-nth '0 (<evalfn>-fn ,@(list <formals>)
     ;;                                                        ,clk ,orac)))
     ;;                               ':ev_normal)
     ;;                       mfc state)
     ;;                      (acl2::rewriting-negative-literal-fn
     ;;                       `(equal ':ev_normal
     ;;                               (eval_result-kind$inline
     ;;                                (mv-nth '0 (<evalfn>-fn ,@(list <formals>)
     ;;                                                        ,clk ,orac))))
     ;;                       mfc state)))
     ;;                 (syntaxp (not (and (consp clk)
     ;;                                    (eq (car clk) '<evalfn>-clock-fn)
     ;;                                    (equal (cdr clk) (list <formals> orac))))))
     ;;            (iff (equal (eval_result-kind (mv-nth 0 (<evalfn> <formals>))) :ev_normal)
     ;;                 (and (hide (equal (eval_result-kind (mv-nth 0 (<evalfn> <formals>))) :ev_normal))
     ;;                      (equal (eval_result-kind (mv-nth 0 (<evalfn> <formals> :clk (<evalfn>-clock <formals>))))
     ;;                             :ev_normal))))
     ;;   :hints (("goal" :expand ((:free (x) (hide x)))
     ;;            :use <evalfn>-normalizes-clock-when-terminates
     ;;            :in-theory (disable <evalfn>-normalizes-clock-when-terminates))))

     ;; (acl2::add-to-ruleset normalizes-clock-when-terminates-normal-assum
     ;;                       <evalfn>-normalizes-clock-when-terminates-normal-assum
     ;;                       )

     ;; (fgl::def-fgl-rewrite <evalfn>-normalize-termination
     ;;   (implies (syntaxp (fgl::fgl-object-case clk
     ;;                       :g-apply (not (eq clk.fn '<evalfn>-clock))
     ;;                       :otherwise t))
     ;;            (equal (<evalfn> <formals>)
     ;;                   (b* (((mv res new-orac) (fgl::fgl-hide (<evalfn> <formals>))))
     ;;                     (if (fgl::fgl-hide (termination-error-p res))
     ;;                         (mv (fgl::fgl-hide (termination-error-fix res))
     ;;                             (nonterm-orac new-orac))
     ;;                       (<evalfn> <formals> :clk (<evalfn>-clock <formals>)))))))
     ))

(defun def-interp-clock-and-termination-fn (fn wrld)
  (declare (xargs :mode :program))
  (b* (((std::defguts guts) (cdr (assoc fn (std::get-define-guts-alist wrld))))
       (key-parts (member '&key guts.raw-formals))
       (formals-prefix-len (- (len guts.raw-formals) (len key-parts)))
       (formals-types (take formals-prefix-len guts.raw-formals))
       (formals (std::formallist->names (take formals-prefix-len guts.formals))))
    (acl2::template-subst *interp-clock-and-termination-template*
                          :str-alist `(("<EVALFN>" . ,(symbol-name fn)))
                          :atom-alist `((<evalfn> . ,fn))
                          :splice-alist `((<formals> . ,formals)
                                          (<formals-types> . ,formals-types)
                                          (<dummy-pos> . ,(and (eq fn 'eval_subprogram-*T)
                                                               '(:pos *dummy-position*))))
                          :pkg-sym 'asl-pkg)))

(defmacro def-interp-clock-and-termination (fn)
  `(make-event (def-interp-clock-and-termination-fn ',fn (w state))))

(defun def-interp-clock-and-terminations-fn (fns)
  (if (atom fns)
      nil
    (cons `(def-interp-clock-and-termination ,(car fns))
          (def-interp-clock-and-terminations-fn (cdr fns)))))


(with-output :off :all :on (error)
  (encapsulate nil
    (local (in-theory (acl2::disable* asl-*t-equals-original-rules)))
    (local (include-book "../trace-fixequiv"))
    (local (in-theory (enable ev_error->desc-when-wrong-kind
                              not-termination-error-when-trace-abort)))
    
    (make-event
     (cons 'progn
           (def-interp-clock-and-terminations-fn
             (std::collect-names-from-guts
              (std::defines-guts->gutslist
                (cdr (assoc 'asl-interpreter-mutual-recursion-*t
                            (std::get-defines-alist (w state)))))))))))
     
;; (def-interp-clock-and-termination resolve-ty)
;; (def-interp-clock-and-termination eval_call)
;; (def-interp-clock-and-termination eval_loop)

;; (defthm resolve-ty-clock-posp-when-terminates
;;   (implies (and (resolve-ty-terminates env x orac)
;;                 (type_desc-case (ty->desc x) :t_named)
;;                 (hons-assoc-equal (t_named->name (ty->desc x))
;;                                   (static_env_global->declared_types
;;                                    (global-env->static (env->global env)))))
;;            (posp (resolve-ty-clock env x orac)))
;;   :hints(("Goal" :in-theory (enable resolve-ty-terminates)
;;           :expand ((:free (clk) (resolve-ty env x))))))


;; (defthm eval_call-clock-posp-when-terminates
;;   (let ((clk (eval_call-clock name env params args pos orac)))
;;     (implies (and (eval_call-terminates name env params args pos orac)
;;                   (eval_result-case (mv-nth 0 (eval_expr_list env params)) :ev_normal)
;;                   (eval_result-case
;;                     (mv-nth 0 (eval_expr_list (exprlist_result->env
;;                                                (ev_normal->res (mv-nth 0 (eval_expr_list env params))))
;;                                               args
;;                                               :orac (mv-nth 1 (eval_expr_list env params))))
;;                     :ev_normal))
;;              (posp clk)))
;;   :hints(("Goal" :in-theory (enable eval_call-terminates)
;;           :expand ((:free (clk) (eval_call name env params args pos))))))
                



(local (include-book "../interp-mods"))

(define termination-wrap-recursive-calls (body
                                          fns
                                          varcount
                                          wrld)
  :mode :program
  (if (atom body)
      (mv body varcount)
    (cond ((member-eq (car body) fns)
           (b* (((mv args varcount) (termination-wrap-recursive-calls (cdr body) fns varcount wrld))
                (fn (car body))
                ((mv & trans-call) (acl2::macroexpand1-cmp `(,fn . ,args) `(wrap . ,fn)  wrld (default-state-vars nil)))
                ((std::defguts guts) (cdr (assoc fn (std::get-define-guts-alist wrld))))
                (formals (std::formallist->names guts.formals))
                (key-parts (member '&key guts.raw-formals))
                (formals-prefix-len (- (len guts.raw-formals) (len key-parts)))
                (nonkey-formals (take formals-prefix-len formals))
                (bindings (remove-assoc 'clk (pairlis$ formals (pairlis$ (cdr trans-call) nil))))
                (clock (acl2::template-subst '<evalfn>-clock :str-alist `(("<EVALFN>" . ,(symbol-name fn))) :pkg-sym 'asl-pkg))
                (terminates (acl2::template-subst '<evalfn>-terminates :str-alist `(("<EVALFN>" . ,(symbol-name fn))) :pkg-sym 'asl-pkg))
                (freevar (acl2::template-subst '__termination-freevar-<n>
                                         :str-alist `(("<N>" . ,(str::natstr varcount)))
                                         :pkg-sym 'asl-pkg)))
             (mv `(let ,bindings
                    (fgl::conditionalize
                     ,freevar
                     (,terminates . ,nonkey-formals)
                     (,fn ,@nonkey-formals :clk (,clock . ,nonkey-formals))))
                 (+ 1 varcount))))
          ((and (consp (car body)) (equal (caar body) '(when (zp clk))))
           (termination-wrap-recursive-calls (cdr body) fns varcount wrld))
          (t (b* (((mv car varcount) (termination-wrap-recursive-calls (car body) fns varcount wrld))
                  ((mv cdr varcount) (termination-wrap-recursive-calls (cdr body) fns varcount wrld)))
               (mv (cons car cdr) varcount))))))
                
                
(encapsulate nil
  (local (in-theory (acl2::e/d* (interp-*t-terminates-functions
                                 fgl::conditionalize1
                                 ev_error->desc-when-wrong-kind
                                 normalizes-clock-when-terminates
                                 forward-normalize-clock-when-terminates
                                 forward-normalize-clock-when-normal
                                 split-on-tracespecs)
                                (asl-*t-equals-original-rules
                                 cons-equal
                                 equal-of-ev_error
                                 equal-of-ev_normal
                                 expt floor mod loghead
                                 acl2::expt-with-violated-guards
                                 ;; not-eval_expr-*t-terminates-implies
                                 env-replace-static-with-self
                                 global-replace-static-with-self
                                 binary-append
                                 true-listp
                                 acl2::append-to-nil
                                 not
                                 assoc-equal len
                                 default-car default-cdr
                                 identifier-fix-when-identifier-p
                                 id_eval_result-p-implies
                                 (tau-system)
                                 (:rules-of-class :type-prescription :here)
                                 (:rules-of-class :congruence :here))
                                ((:t exprlist_result->val)
                                 (:t v_bool->val)
                                 stmt-abort-after
                                 call-abort-after))))
  (local (include-book "clause-processors/autohide" :dir :system))

  (local (defun def-when-terminates-fgl-rule (fn wrld)
           (declare (xargs :mode :program))
           (b* ((clock (acl2::template-subst '<evalfn>-clock :str-alist `(("<EVALFN>" . ,(symbol-name fn))) :pkg-sym 'asl-pkg))
                (terminates (acl2::template-subst '<evalfn>-terminates :str-alist `(("<EVALFN>" . ,(symbol-name fn))) :pkg-sym 'asl-pkg))
                ((std::defguts guts) (cdr (assoc fn (std::get-define-guts-alist wrld))))
                (key-parts (member '&key guts.raw-formals))
                (formals-prefix-len (- (len guts.raw-formals) (len key-parts)))
                ;; (formals-types (take formals-prefix-len guts.raw-formals))
                (formals (std::formallist->names (take formals-prefix-len guts.formals)))
                (body (car (last (find-define fn *asl-interpreter-mutual-recursion-*t-form*))))
                ((mv body-with-replaced-calls &)
                 (termination-wrap-recursive-calls
                  body 
                  (std::collect-names-from-guts
                   (std::defines-guts->gutslist
                     (cdr (assoc 'asl-interpreter-mutual-recursion-*t
                                 (std::get-defines-alist wrld)))))
                  0 wrld)))
             (acl2::template-subst
              '(fgl::def-fgl-rewrite <evalfn>-when-terminates
                 (implies (<terminates> <formals>)
                          (equal (<evalfn> <formals> :clk (<clock> <formals>))
                                 <body>))
                 :hints (("goal"
                          ;; These proofs work best/fastest when we expand the
                          ;; <terminates> form first, including its call of
                          ;; <evalfn> with normal clock and empty tracespec,
                          ;; before allowing any attention to the other
                          ;; <evalfn> call and body.  To accomplish this we
                          ;; apply autohide to hide the conclusion's EQUAL
                          ;; while expanding the <terminates> term and its
                          ;; <evalfn> call, then when
                          ;; stable-under-simplification we expand the other
                          ;; two.
                          :clause-processor (acl2::autohide-cp clause '(equal))
                           :expand
                           ((:free (pos x otherwise is_while) (<evalfn> <formals> :clk (<clock> <formals>)
                                                                        :tracespec '(nil nil nil nil)))))
                         (and stable-under-simplificationp
                              '(:expand
                                ((:free (pos tracespec x otherwise is_while)
                                  (<evalfn> <formals> :clk (<clock> <formals>)))
                                 (:free (x) (hide x))
                                 )))))
              :str-alist `(("<EVALFN>" . ,(symbol-name fn)))
              :atom-alist `((<evalfn> . ,fn)
                            (<terminates> . ,terminates)
                            (<clock> . ,clock)
                            (<body> . ,body-with-replaced-calls))
              :splice-alist `((<formals> . ,formals))
              :pkg-sym 'asl-pkg))))
  (local (defun def-when-terminates-fgl-rules (fns wrld)
           (declare (xargs :mode :program))
           (if (atom fns)
               nil
             (cons (def-when-terminates-fgl-rule (car fns) wrld)
                   (def-when-terminates-fgl-rules (cdr fns) wrld)))))

  (with-output :off :all :on (error summary)
    :gag-mode nil
    :summary-off :all :summary-on (acl2::form time)
    (make-event
     (cons 'progn
           (def-when-terminates-fgl-rules
             (std::collect-names-from-guts
              (std::defines-guts->gutslist
                (cdr (assoc 'asl-interpreter-mutual-recursion-*t
                            (std::get-defines-alist (w state))))))
             (w state))))))




;; In some of our rewrite rules, we want to leave the clock of the call in the LHS free
;; but have the assumption that it terminates with that clock (and without tracespec).
;; This then implies that it it terminates and that the clock is big enough.

(define eval_subprogram-*t-terminating-call ((env env-p)
                                             (name identifier-p)
                                             (vparams vallist-p)
                                             (vargs vallist-p)
                                             &key
                                             ((clk natp) 'clk)
                                             (orac 'orac)
                                             ((pos posn-p) 'pos)
                                             ((static-env static_env_global-p) 'static-env))
  (b* ((res (non-exec (mv-nth 0 (eval_subprogram-*t
                                 env name vparams vargs
                                 :tracespec (make-tracespec))))))
    (not (termination-error-p res)))
  ///
  (fgl::def-fgl-rewrite eval_subprogram-*t-when-terminating-call
    (implies (and (equal new-clk (eval_subprogram-*t-clock env name vparams vargs))
                  (syntaxp (not (equal clk new-clk)))
                  (eval_subprogram-*t-terminating-call
                   env name vparams vargs))
             (equal (eval_subprogram-*t env name vparams vargs)
                    (fgl::conditionalize
                     __terminates
                     (eval_subprogram-*t-terminates env name vparams vargs)
                     (eval_subprogram-*t env name vparams vargs
                                         :clk new-clk))))
    :hints(("Goal" :in-theory (acl2::e/d* (fgl::conditionalize1
                                           eval_subprogram-*t-terminates
                                           eval_Subprogram-*t-normalizes-clock-when-terminates)
                                          (asl-*t-equals-original-rules)))))

  (fgl::remove-fgl-rewrite eval_subprogram-*t-terminating-call)

  (fgl::def-fgl-rewrite eval_subprogram-*t-terminating-call-when-terminates-same-clock
    (implies (eval_subprogram-*t-terminates env name vparams vargs)
             (eval_subprogram-*t-terminating-call
              env name vparams vargs :clk (eval_subprogram-*t-clock env name vparams vargs)))
    :hints(("Goal" :in-theory (enable eval_subprogram-*t-terminates)))))





;; Suppose we have functions fa, ga, fb, gb.  Ga calls fa, and analogously gb
;; calls fb.  We can rewrite fb -> fa if they both terminate.  We want to show
;; that we can rewrite gb -> ga if they both terminate.  The call of fb inside
;; gb rewrites to the same call of fa that is inside ga.  Since we're assuming
;; the calls of ga and gb terminate, this implies the calls of fb and fa
;; terminate.

;; The problem is that at the point where we get to the call of fb inside gb,
;; we know that the call of fb terminates, but we don't know that the call of
;; fa that we want to rewrite it to also terminates, at least until we rewrite
;; the call of ga.  If the fb->fa rewrite needs to know that they both
;; terminate, then how do we get that information? 

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

(include-book "termination")
(include-book "interp-redefs")
(include-book "arrays")
(include-book "imaps")

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
                                 id_eval_result-p-implies
                                 (tau-system)
                                 (:rules-of-class :type-prescription :here)
                                 (:rules-of-class :congruence :here))
                                ((:t exprlist_result->val)
                                 (:t v_bool->val)
                                 stmt-abort-after
                                 call-abort-after))))

(local (include-book "clause-processors/autohide" :dir :system))

(defun strip-out-fgl-concrete-wrapper (x)
  (if (atom x)
      nil
    (cons (let ((car (car x)))
            (case-match car
              (('fgl::concrete car1) car1)
              (& car)))
          (strip-out-fgl-concrete-wrapper (cdr x)))))
    

(defun def-termination-fgl-redef-fn (thmname wrld)
  (declare (xargs :mode :program))
  (b* ((sub-wrld (acl2::decode-logical-name thmname wrld))
       (form (acl2::access-event-tuple-form (cddar sub-wrld))))
    (case-match form
      (('defthm & ('implies hyps ('equal (fn . formals) rhs)) . &)
       (b* ((clock (acl2::template-subst '<evalfn>-clock :str-alist `(("<EVALFN>" . ,(symbol-name fn))) :pkg-sym 'asl-pkg))
            (terminates (acl2::template-subst '<evalfn>-terminates :str-alist `(("<EVALFN>" . ,(symbol-name fn))) :pkg-sym 'asl-pkg))
            (formals (strip-out-fgl-concrete-wrapper formals))
            (new-lhs `(,fn ,@formals :clk (,clock . ,formals)))
            ((mv new-rhs &)
             (termination-wrap-recursive-calls
              rhs *trace-interp-fns* 0 wrld))
            (new-hyps (case-match hyps
                        (('and . old-hyps) `(and (,terminates . ,formals) . ,old-hyps))
                        (& `(and (,terminates . ,formals) ,hyps)))))
         (acl2::template-subst
          '(fgl::def-fgl-rewrite <thm>-when-terminates
            (implies <new-hyps>
                     (equal <new-lhs> <new-rhs>))
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
          :str-alist `(("<EVALFN>" . ,(symbol-name fn))
                       ("<THM>" . ,(symbol-name thmname)))
          :atom-alist `((<evalfn> . ,fn)
                        (<terminates> . ,terminates)
                        (<clock> . ,clock)
                        (<new-hyps> . ,new-hyps)
                        (<new-lhs> . ,new-lhs)
                        (<new-rhs> . ,new-rhs))
          :splice-alist `((<formals> . ,formals)))))
       
      (& (er hard 'def-termination-fgl-redef-fn
             "Unexpected form of theorem body")))))

(defun def-termination-fgl-redefs-fn (fns wrld)
  (declare (xargs :mode :program))
  (if (atom fns)
      nil
    (cons (def-termination-fgl-redef-fn (car fns) wrld)
          (def-termination-fgl-redefs-fn (cdr fns) wrld))))

(defmacro def-termination-fgl-redefs (&rest fns)
  `(make-event
    (cons 'progn (def-termination-fgl-redefs-fn ',fns (w state)))))

(local (in-theory (enable v_array-len v_array-nth v_array-update-nth
                          val-imap-lookup val-imap-has-key val-imap-put
                          val-imap-add-pairs-is-from-lists*)))

(local (fty::deffixcong val-equiv vallist-equiv (update-nth n v x) v
         :hints(("Goal" :in-theory (enable vallist-fix)))))

(local (defthm v_array-of-update-nth-val-fix
         (equal (v_array (update-nth n (val-fix v) x))
                (v_array (update-nth n v x)))))

(local (defthm len-of-eval_expr_list-*t
         (b* (((mv res ?new-orac ?trace)
               (eval_expr_list-*t env e)))
           (implies (eval_result-case res :ev_normal)
                    (equal (len (exprlist_result->val (ev_normal->res res)))
                           (len e))))
         :hints(("Goal" :in-theory (acl2::enable* asl-*t-equals-original-rules)))))

(def-termination-fgl-redefs
  eval_stmt-*t1-avoid-merging-s_cond-branches
  eval_expr-*t-avoid-merging-e_cond-branches
  eval_expr-*t-arbitrary-redef
  eval_expr-*t-of-e_getarray-in-terms-of-v_array-nth
  eval_lexpr-*t-of-le_setarray-in-terms-of-v_array-update-nth
  eval_expr-*t-record-redef
  eval_lexpr-*t-setfield-redef)


;; do something about eval_for-*t-count
(local (defun sub-out-count-fns (count-fn-map body)
         (if (atom body)
             body
           (b* ((look (assoc (car body) count-fn-map))
                ((unless look)
                 (cons (sub-out-count-fns count-fn-map (car body))
                       (sub-out-count-fns count-fn-map (cdr body)))))
             (cons (cdr look)
                   ;; eliminate first argument
                   (cddr body))))))
                
                  
(make-event
 (b* ((thmname 'eval_for-*t-count-fn)
      (wrld (w state))
      (sub-wrld (acl2::decode-logical-name thmname wrld))
      (define (acl2::pe-event-form (cddar sub-wrld) wrld))
      (body (car (last (take (- (len define) (len (member '/// define))) define))))
      ((mv new-body1 &) (termination-wrap-recursive-calls body (cons 'eval_for-*t-count *trace-interp-fns*) 0 wrld))
      (new-body (sub-out-count-fns
                 '((eval_for-*t-count-terminates . eval_for-*t-terminates)
                   (eval_for-*t-count-clock . eval_for-*t-clock))
                 new-body1)))
 `(fgl::def-fgl-rewrite eval_for-*t-count-when-terminates
    (implies (eval_for-*t-terminates env index_name limit v_start dir v_end body)
             (equal (eval_for-*t-count
                     count env index_name limit v_start dir v_end body
                     :clk (eval_for-*t-clock env index_name limit v_start dir v_end body))
                    ,new-body))
    :hints(("Goal" :in-theory (enable eval_for-*t-when-terminates
                                      eval_for-*t-count-in-terms-of-eval_for-*t))))))


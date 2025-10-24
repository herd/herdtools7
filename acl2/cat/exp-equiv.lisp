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

(in-package "CAT")

(include-book "eval-ins")
(include-book "freevars")
(include-book "reclimit")
(include-book "centaur/misc/universal-equiv" :dir :system)
(local (include-book "std/util/termhints" :dir :system))

(local (std::add-default-post-define-hook :fix))

(define err/res-equiv (x y)
  :verify-guards nil
  (and (iff (car x) (car y))
       (equal (cdr x) (cdr y)))
  ///
  (defequiv err/res-equiv)

  (defthm err/res-equiv-implies-iff-of-mv-nth-0
    (implies (err/res-equiv x y)
             (iff (mv-nth 0 x) (mv-nth 0 y)))
    :hints(("Goal" :in-theory (enable mv-nth)))
    :rule-classes :congruence)

  (defthm err/res-equiv-implies-equal-of-mv-nth-1
    (implies (err/res-equiv x y)
             (equal (mv-nth 1 x) (mv-nth 1 y)))
    :hints(("Goal" :in-theory (enable mv-nth)))
    :rule-classes :congruence)

  (defthm err/res-equiv-implies-equal-of-mv-nth-2
    (implies (err/res-equiv x y)
             (equal (mv-nth 2 x) (mv-nth 2 y)))
    :hints(("Goal" :in-theory (enable mv-nth)))
    :rule-classes :congruence)

  (defcong iff err/res-equiv (cons err rest) 1))

(defsection exp-eval-equiv
  (acl2::def-universal-equiv exp-eval-equiv
    :qvars (ex env reclimit)
    :equiv-terms ((err/res-equiv (eval-exp acl2::x))))

  (defcong exp-eval-equiv err/res-equiv (eval-exp x) 1
    :hints(("Goal" :in-theory (enable exp-eval-equiv-necc)))))

(defsection binding-eval-equiv
  (acl2::def-universal-equiv binding-eval-equiv
    :equiv-terms ((exp-eval-equiv (binding->exp acl2::x))
                  (equal (binding->pat acl2::x))))

  (defcong binding-eval-equiv exp-eval-equiv (binding->exp x) 1
    :hints(("Goal" :in-theory (enable binding-eval-equiv))))
  (defcong binding-eval-equiv equal (binding->pat x) 1
    :hints(("Goal" :in-theory (enable binding-eval-equiv))))

  (defcong binding-eval-equiv err/res-equiv (eval-binding x) 1
    :hints (("goal" :expand ((eval-binding x)
                             (eval-binding x-equiv))
             :in-theory (enable err/res-equiv))))

  (defcong binding-eval-equiv err/res-equiv (eval-fixpoint-binding x) 1
    :hints (("goal" :expand ((eval-fixpoint-binding x)
                             (eval-fixpoint-binding x-equiv))
             :in-theory (enable err/res-equiv))))

  (defcong exp-equiv binding-eval-equiv (binding loc var exp) 3))


(define bindinglist-eval-equiv (x y)
  :verify-guards nil
  (if (atom x)
      (atom y)
    (and (consp y)
         (binding-eval-equiv (car x) (car y))
         (bindinglist-eval-equiv (cdr x) (cdr y))))
  ///
  (defequiv bindinglist-eval-equiv)
  (defcong bindinglist-eval-equiv binding-eval-equiv (car x) 1)
  (defcong bindinglist-eval-equiv bindinglist-eval-equiv (cdr x) 1)

  (local (defun-nx eval-bindings-equiv-ind (x y ex env reclimit)
           (if (atom x)
               (list y ex env reclimit)
             (eval-bindings-equiv-ind
              (cdr x) (cdr y) ex env reclimit))))
  
  (defcong bindinglist-eval-equiv err/res-equiv (eval-bindings x) 1
    :hints(("Goal" :induct (eval-bindings-equiv-ind x x-equiv ex env reclimit)
            :expand ((Eval-bindings x)
                     (eval-bindings x-equiv)))))

  (local (defun-nx eval-fixpoint1-equiv-ind (x y ex env reclimit)
           (if (atom x)
               (list y ex env reclimit)
             (eval-fixpoint1-equiv-ind
              (cdr x) (cdr y) ex
              (mv-nth 1 (eval-fixpoint-binding (car x))) reclimit))))

  (defcong bindinglist-eval-equiv err/res-equiv (eval-fixpoint1 x) 1
    :hints(("Goal" :induct (eval-fixpoint1-equiv-ind x x-equiv ex env reclimit)
            :expand ((Eval-fixpoint1 x)
                     (eval-fixpoint1 x-equiv)))))

  (local (defun-nx eval-fixpoint-equiv-ind (x ex env reclimit)
           (if (zp reclimit)
               (list x ex env)
             (eval-fixpoint-equiv-ind x ex (mv-nth 1 (eval-fixpoint1 x)) (1- reclimit)))))
  
  (defcong bindinglist-eval-equiv err/res-equiv (eval-fixpoint x) 1
    :hints (("goal" :induct (eval-fixpoint-equiv-ind x ex env reclimit)
             :expand ((eval-fixpoint x)
                      (eval-fixpoint x-equiv))))))



(local (defthm errmsg-under-iff
         (iff (errmsg name msg) t)))

(local (defthm evtlist-p-of-union-equal
         (implies (and (evtlist-p x)
                       (evtlist-p y))
                  (evtlist-p (union-equal x y)))))

(local (defthm vallist-p-of-union-equal
         (implies (and (vallist-p x)
                       (vallist-p y))
                  (vallist-p (union-equal x y)))))

(local (include-book "std/lists/sets" :dir :system))

(local (defthm union-equal-assoc
         (equal (union-equal (union-equal x y) z)
                (union-equal x (union-equal y z)))
         :hints(("Goal" :in-theory (enable union-equal)))))

(local (defthmd equal-cons-strong
         (equal (equal (cons a b) c)
                (and (consp c)
                     (equal (car c) a)
                     (equal (cdr c) b)))))
(local (defthm len-equal-0
         (equal (Equal (len x) 0)
                (not (consp x)))))
(local (defthmd v_empty-when-v_empty
         (implies (and (acl2::rewriting-negative-literal `(equal (val-kind$inline ,x) ':v_empty))
                       (val-p x))
                  (equal (equal (val-kind x) :v_empty)
                         (equal x (v_empty))))
         :hints(("Goal" :in-theory (enable val-p val-kind equal-cons-strong)))))

(defthm exp-equiv-of-union-assoc
  (exp-eval-equiv (e_op loc1 :union (list (e_op loc2 :union (list x y)) z))
                  (e_op nil :union (list x y z)))
  :hints(("Goal" :in-theory (enable exp-eval-equiv
                                    v_empty-when-v_empty
                                    eval-op2)
          :expand ((:free (loc x y ex env reclimit) (eval-exp (e_op loc :union (cons x y))))
                   (:free (x y ex env reclimit) (eval-explist (cons x y)))
                   (:free (ex env reclimit) (eval-explist nil))))))





(defsection ins-eval-equiv
  (acl2::def-universal-equiv ins-eval-equiv
    :qvars (ex result reclimit)
    :equiv-terms ((err/res-equiv (eval-ins acl2::x))))

  (defcong ins-eval-equiv err/res-equiv (eval-ins x) 1
    :hints (("goal" :in-theory (enable ins-eval-equiv-necc)))))


(local (defthm pat0-fix-when-nonnil
         (implies var
                  (equal (pat0-fix var) (var-fix var)))
         :hints(("Goal" :in-theory (enable pat0-fix)))))


(defsection eval-fixpoint-is-transitive-closure
  (local (include-book "varset"))
  (local (defun-nx eval-fixpoint-ind (x ex env reclimit)
           (declare (Xargs :measure (nfix reclimit)))
           (b* (((expval new-env changedp) (eval-fixpoint1 x))
                ((unless changedp) (norm new-env))
                ((when (zp reclimit))
                 (err "Hit recursion limit" "in eval-fixpoint")))
             (eval-fixpoint-ind x ex new-env (1- reclimit)))))

  (defthm env-equiv-on-var-set-of-cons-when-not-in
    (implies (not (in (var-fix v) (varlist-fix vars)))
             (env-equiv-on-var-set vars (env-update v val env) env))
    :hints(("Goal" :in-theory (enable env-equiv-on-var-set
                                      in))))
  
  (defthm eval-exp-of-cons-non-free-var
    (implies (not (in (var-fix v) (exp-free-vars base)))
             (equal (eval-exp base :env (env-update v val env))
                    (eval-exp base :env env)))
    :hints (("goal" :use ((:instance eval-exp-of-free-var-equiv-envs
                           (env1 (env-update v val env))
                           (x base))))))

  (defthm eval-exp-of-greater-reclimit-rw
    (b* (((mv err res) (eval-exp x))
         ((mv err2 res2) (eval-exp x :reclimit reclimit2)))
      (implies (and (not err)
                    (syntaxp (not (equal reclimit reclimit2)))
                    (<= (nfix reclimit) (nfix reclimit2)))
               (and (equal err2 err)
                    (equal res2 res))))
    :hints (("goal" :use eval-exp-of-greater-reclimit
             :in-theory (disable eval-exp-of-greater-reclimit))))

  (defthm transitive-closure-is-superset-no-fix
    (implies (relation-p rel)
             (subset rel (transitive-closure rel)))
    :hints (("goal" :use ((:instance transitive-closure-is-superset (x rel)))
             :in-theory (disable transitive-closure-is-superset))))

  (defthm subset-of-transitive-closure-when-subset
    (implies (subset x (relation-fix rel))
             (subset x (transitive-closure rel)))
    :hints(("Goal" :in-theory (enable set::subset-transitive))))

  (defthm transitive-closure-when-emptyp
    (implies (and (emptyp x)
                  (relation-p x))
             (equal (transitive-closure x)
                    x))
    :hints(("Goal" :in-theory (enable transitive-closure
                                      compose))))

  (defthm subset-of-union
    (iff (subset (union x y) z)
         (and (subset x z) (subset y z)))
    :hints(("goal" :in-theory (enable union subset))))

  (defthm subset-compose-1
    (implies (subset (relation-fix x1) (relation-fix x2))
             (subset (compose x1 y) (compose x2 y)))
    :hints(("Goal" :in-theory (enable pick-a-point-subset-strategy
                                      member-of-compose-rw
                                      set::subset-in))
           (SET::PICK-A-POINT-SUBSET-HINT ID acl2::CLAUSE
                                          WORLD STABLE-UNDER-SIMPLIFICATIONP)
           (and stable-under-simplificationp
                '(:use ((:instance member-of-compose-suff
                         (x x2) (pair set::arbitrary-element)
                         (mid (compose-midpoint
                               (evtpair->from set::arbitrary-element)
                               (evtpair->to set::arbitrary-element)
                               x1 y))))))))


  (defthm subset-compose-2
    (implies (subset (relation-fix y1) (relation-fix y2))
             (subset (compose x y1) (compose x y2)))
    :hints(("Goal" :in-theory (enable pick-a-point-subset-strategy
                                      member-of-compose-rw
                                      set::subset-in))
           (SET::PICK-A-POINT-SUBSET-HINT ID acl2::CLAUSE
                                          WORLD STABLE-UNDER-SIMPLIFICATIONP)
           (and stable-under-simplificationp
                '(:use ((:instance member-of-compose-suff
                         (y y2) (pair set::arbitrary-element)
                         (mid (compose-midpoint
                               (evtpair->from set::arbitrary-element)
                               (evtpair->to set::arbitrary-element)
                               x y1))))))))

  (defthm subset-compose-both
    (implies (and (subset (relation-fix y1) (relation-fix y2))
                  (subset (relation-fix x1) (relation-fix x2)))
             (subset (compose x1 y1) (compose x2 y2)))
    :hints (("goal" :use ((:instance subset-compose-1
                           (y y1)))
             :in-theory (e/d (set::subset-transitive)
                             (subset-compose-1)))))
  
  (defthm subset-compose-special
    (implies (and (subset (compose x x) y)
                  (subset (relation-fix z) (relation-fix x)))
             (subset (compose z z) y))
    :hints(("Goal" :in-theory (enable set::subset-transitive))))
  
  (defthm compose-is-subset-of-closed
    (implies (and (subset (compose y y) (relation-fix y))
                  (subset (relation-fix x) (relation-fix y)))
             (subset (compose x x) (relation-fix y)))
    :hints (("goal" :use ((:instance subset-compose-both
                           (x1 x) (y1 x) (x2 y) (y2 y)))
             :in-theory (e/d (set::subset-transitive)
                             (subset-compose-both)))))

  (defthm compose-subset-of-transitive-closure
    (implies (subset (relation-fix x) (transitive-closure rel))
             (subset (compose x x) (transitive-closure rel)))
    :hints (("goal" :use ((:instance compose-is-subset-of-closed
                           (y (transitive-closure rel))))
             :in-theory (disable compose-is-subset-of-closed))))


  (defthm union-with-subset
    (implies (subset y x)
             (equal (union x y)
                    (sfix x)))
    :hints(("Goal" :in-theory (enable double-containment
                                      pick-a-point-subset-strategy))))

  (defthm compose-when-emptyp
    (implies (emptyp (relation-fix x))
             (equal (compose x y) nil))
    :hints(("Goal" :in-theory (enable compose))))

  (defthm compose-when-emptyp-2
    (implies (emptyp (relation-fix x))
             (equal (compose y x) nil))
    :hints(("Goal" :in-theory (enable compose compose1))))

  (defthm compose-of-union-1
    (implies (and (relation-p x) (relation-p y))
             (equal (compose (union x y) z)
                    (union (compose x z) (compose y z))))
    :hints(("Goal" :in-theory (enable set::double-containment-no-backchain-limit
                                      pick-a-point-subset-strategy
                                      member-of-compose-rw)
            :do-not-induct t)
           (SET::PICK-A-POINT-SUBSET-HINT ID acl2::CLAUSE
                                          WORLD STABLE-UNDER-SIMPLIFICATIONP)
           (and stable-under-simplificationp
                '(:use ((:instance member-of-compose-suff
                         (pair set::arbitrary-element)
                         (mid (compose-midpoint (evtpair->from set::arbitrary-element)
                                                (evtpair->to set::arbitrary-element)
                                                (union x y) z))
                         (y z) (x x))
                        (:instance member-of-compose-suff
                         (pair set::arbitrary-element)
                         (mid (compose-midpoint (evtpair->from set::arbitrary-element)
                                                (evtpair->to set::arbitrary-element)
                                                (union x y) z))
                         (y z) (x y))))))
    :otf-flg t)

  (defthm compose-of-union-2
    (implies (and (relation-p y) (relation-p z))
             (equal (compose x (union y z))
                    (union (compose x y) (compose x z))))
    :hints(("Goal" :in-theory (enable set::double-containment-no-backchain-limit
                                      pick-a-point-subset-strategy
                                      member-of-compose-rw)
            :do-not-induct t)
           (SET::PICK-A-POINT-SUBSET-HINT ID acl2::CLAUSE
                                          WORLD STABLE-UNDER-SIMPLIFICATIONP)
           (and stable-under-simplificationp
                '(:use ((:instance member-of-compose-suff
                         (pair set::arbitrary-element)
                         (mid (compose-midpoint (evtpair->from set::arbitrary-element)
                                                (evtpair->to set::arbitrary-element)
                                                x (union y z)))
                         (y y) (x x))
                        (:instance member-of-compose-suff
                         (pair set::arbitrary-element)
                         (mid (compose-midpoint (evtpair->from set::arbitrary-element)
                                                (evtpair->to set::arbitrary-element)
                                                x (union y z)))
                         (y z) (x x))))))
    :otf-flg t)

  (defthm subset-of-union-1
    (implies (subset x y)
             (subset x (union y z)))
    :hints(("Goal" :in-theory (enable double-containment
                                      pick-a-point-subset-strategy
                                      set::subset-in))))
  
  (defthm subset-of-union-2
    (implies (subset x z)
             (subset x (union y z)))
    :hints(("Goal" :in-theory (enable double-containment
                                      pick-a-point-subset-strategy
                                      set::subset-in))))

  (defthm closed-superset-is-superset-of-closure
    (implies (and (subset base v)
                  (subset (compose v v) v)
                  (relation-p base)
                  (relation-p v))
             (subset (transitive-closure base) v))
    :hints (("goal" :in-theory (enable transitive-closure))))

  (defthm closed-superset-when-subset-of-closure
    (implies (and (subset base v)
                  (subset (compose v v) v)
                  (relation-p base)
                  (relation-p v)
                  (subset v (transitive-closure base)))
             (equal (transitive-closure base) v))
    :hints (("goal" :in-theory (enable set::double-containment-no-backchain-limit))))

  (defthm transitive-closure-correct-rw
    (implies (acl2::rewriting-negative-literal
              `(in ,pair (transitive-closure ,x)))
             (iff (in pair (transitive-closure x))
                  (and (evtpair-p pair)
                       (exists-path (evtpair->from pair)
                                    (evtpair->to pair)
                                    x))))
    :hints(("Goal" :in-theory (enable transitive-closure-correct))))
  
  (defthm union-of-compose-closure
    (implies (relation-p x)
             (equal (union x (compose (transitive-closure x)
                                      (transitive-closure x)))
                    (transitive-closure x)))
    :hints(("Goal" :in-theory (enable set::double-containment-no-backchain-limit
                                      pick-a-point-subset-strategy
                                      exists-path))
           (SET::PICK-A-POINT-SUBSET-HINT ID acl2::CLAUSE
                                          WORLD STABLE-UNDER-SIMPLIFICATIONP)
           (and stable-under-simplificationp
                (acl2::use-termhint
                 (b* (((evtpair pair) set::arbitrary-element)
                      (witness (exists-path-witness pair.from pair.to x))
                      ((when (atom (cddr witness)))
                       ;; only one hop, so pair should be in x, immediate from relation-path-p
                       `(:expand ((relation-path-p ,(acl2::hq witness) x)))))
                   ;; multiple hops -- first hop and rest of hops both in transitive closure, so path is in compose
                   ;; of closures
                   `(:expand ((relation-path-p ,(acl2::hq witness) x)
                              ;; (last (cdr ,(acl2::hq witness)))
                              (:free (A b) (relation-path-p (list a b) x)))
                     :use ((:instance member-of-compose-suff
                            (pair ,(acl2::hq pair))
                            (mid (cadr ,(acl2::hq witness)))
                            (x (transitive-closure x))
                            (y (transitive-closure x)))
                           (:instance in-transitive-closure-when-path
                            (path (list (car ,(acl2::hq witness))
                                        (cadr ,(acl2::hq witness)))))
                           (:instance in-transitive-closure-when-path
                            (path (cdr ,(acl2::hq witness)))))))))))

  (defthm union-of-compose-closure-free
    (implies (and (relation-p x)
                  (equal v (transitive-closure x)))
             (equal (union x (compose v v))
                    (transitive-closure x))))
                            
  
  (defthm closed-union-cond
    (implies (and (subset base v)
                  (subset (compose v v) v)
                  (relation-p base)
                  (relation-p v))
             (let ((u (union base (compose v v))))
               (subset (compose u u) u)))
    :hints(("Goal" :in-theory (enable set::subset-transitive))))

  (local (in-theory (disable  compose-of-union-1
                              compose-of-union-2)))
  
  (defthm transitive-closure-of-union
    (implies (and (subset base v)
                  (subset (compose v v) v)
                  (relation-p base)
                  (relation-p v))
             (let ((u (union base (compose v v))))
               (equal (transitive-closure u) u)))
    :hints (("goal" :expand ((transitive-closure (union base (compose v v)))))))


  (defthm transitive-closure-of-union-compose
    (implies (relation-p x)
             (equal (transitive-closure (union x (compose x x)))
                    (transitive-closure x)))
    :hints (("goal" :expand ((transitive-closure x)))))

  (defthm env-update-of-env-update
    (equal (env-update var val (env-update var val1 env))
           (env-update var val env))
    :hints (("goal" :use ((:instance env-diff-key-when-unequal
                           (x (env-update var val (env-update var val1 env)))
                           (y (env-update var val env)))))))
  
  (defthm eval-fixpoint-is-transitive-closure
    (b* (((mv err res) (eval-fixpoint
                        (list (binding loc2 (pvar var)
                                       (e_op loc3 :union
                                             (list base
                                                   (e_op loc4 :seq (list (e_var loc5 var)
                                                                         (e_var loc6 var)))))))))
         (baseval (mv-nth 1 (eval-exp base :reclimit (+ 1 (nfix reclimit)))))
         (baserel (v_rel->rel baseval))
         (initval (env-lookup var env))
         (initrel (if (val-case initval :v_rel)
                      (v_rel->rel initval)
                    nil))
         (closure (transitive-closure baserel)))
      (implies (and var
                    initval
                    (val-case baseval :v_rel)
                    (subset initrel closure)
                    (not err)
                    (not (in (var-fix var) (exp-free-vars base)))
                    (not (member-equal (var-fix var) (special-vars))))
               (equal res
                      (env-update var (v_rel closure) env))))
    :hints (("goal" :induct (eval-fixpoint-ind (list (binding loc2 (pvar var)
                                                              (e_op loc3 :union
                                                                    (list base
                                                                          (e_op loc4 :seq (list (e_var loc5 var)
                                                                                                (e_var loc6 var)))))))
                                               ex env reclimit)
             :in-theory (enable eval-op2 eval-var
                                ;; CHECK-FIXPOINT-BINDING
                                set::subset-transitive)
             :expand ((:free (x) (eval-fixpoint x))
                      (:free (x) (eval-fixpoint1 (list x)))
                      (:free (x env) (eval-fixpoint1 nil))
                      (:free (x) (eval-fixpoint-binding x))
                      (:free (loc x y ex env reclimit) (eval-exp (e_op loc :union (list x y))))
                      (:free (loc x y ex env reclimit) (eval-exp (e_op loc :seq (list x y))))
                      (:free (loc v ex env reclimit) (eval-exp (e_var loc v)))
                      (:free (x y ex env reclimit) (eval-explist (cons x y)))
                      (:free (ex env reclimit) (eval-explist nil)))
             :do-not-induct t)
            (and stable-under-simplificationp
                 '(:in-theory (enable check-fixpoint-binding)))))

  (defthm eval-fixpoint-empty
    (b* (((mv err res) (eval-fixpoint
                        (list (binding loc2 (pvar var)
                                       (e_op loc3 :union
                                             (list base
                                                   (e_op loc4 :seq (list (e_var loc5 var)
                                                                         (e_var loc6 var)))))))))
         (baseval (mv-nth 1 (eval-exp base :reclimit (+ 1 (nfix reclimit)))))
         (initval (env-lookup var env)))
      (implies (and var
                    initval
                    (val-case baseval :v_empty)
                    (val-case initval :v_empty)
                    (not err)
                    (not (in (var-fix var) (exp-free-vars base)))
                    (not (member-equal (var-fix var) (special-vars))))
               (equal res
                      (env-update var (v_empty) env))))
    :hints (("goal"
             :in-theory (enable eval-op2 eval-var
                                ;; CHECK-FIXPOINT-BINDING
                                set::subset-transitive)
             :expand ((:free (x) (eval-fixpoint x))
                      (:free (x) (eval-fixpoint1 (list x)))
                      (:free (x env) (eval-fixpoint1 nil))
                      (:free (x) (eval-fixpoint-binding x))
                      (:free (loc x y ex env reclimit) (eval-exp (e_op loc :union (list x y))))
                      (:free (loc x y ex env reclimit) (eval-exp (e_op loc :seq (list x y))))
                      (:free (loc v ex env reclimit) (eval-exp (e_var loc v)))
                      (:free (x y ex env reclimit) (eval-explist (cons x y)))
                      (:free (ex env reclimit) (eval-explist nil)))
             :do-not-induct t)
            (and stable-under-simplificationp
                 '(:in-theory (enable check-fixpoint-binding))))))








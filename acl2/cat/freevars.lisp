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

(include-book "eval-exp")
(include-book "std/alists/alist-keys" :Dir :System)
(local (include-book "std/util/termhints" :dir :system))
(local (include-book "std/basic/arith-equivs" :dir :system))
(local (include-book "std/lists/sets" :dir :System))
(local (include-book "varset"))
(local (include-book "../asl/utils/omaps"))
(local (std::add-default-post-define-hook :fix))


(define pat0-vars ((x pat0-p))
  :returns (vars (and (varlist-p vars)
                      (setp vars))
                 :hints(("Goal" :in-theory (enable setp))))
  (and x (insert (var-fix x) nil))
  ///
  (local (in-theory (enable pat0-fix))))

(define pat0-list-vars ((x pat0-list-p))
  :returns (vars (and (varlist-p vars)
                      (setp vars)))
  (if (atom x)
      nil
    (union (pat0-vars (car x))
           (pat0-list-vars (cdr x)))))

(define pat-vars ((x pat-p))
  :returns (vars (and (varlist-p vars)
                      (setp vars)))
  (pat-case x
    :pvar (pat0-vars x.var)
    :ptuple (pat0-list-vars x.tuple)))

(define bindinglist-bound-vars ((x bindinglist-p))
  :returns (vars (and (varlist-p vars)
                      (setp vars)))
  (if (atom x)
      nil
    (union (pat-vars (binding->pat (car x)))
           (bindinglist-bound-vars (cdr x)))))

(define pat-singleton-vars ((x pat-p))
  :returns (vars (and (varlist-p vars)
                      (setp vars)))
  (and (pat-case x :pvar)
       (pat0-vars (pvar->var x))))

(define bindinglist-singleton-bound-vars ((x bindinglist-p))
  :returns (vars (and (varlist-p vars)
                      (setp vars)))
  (if (atom x)
      nil
    (b* (((binding x1) (car x)))
      (union (pat-singleton-vars x1.pat)
             (bindinglist-singleton-bound-vars (cdr x))))))

(local (defthm maybe-exp-count-greater
         (implies x
                  (< (exp-count x) (maybe-exp-count x)))
         :hints(("Goal" :expand ((maybe-exp-count x))
                 :in-theory (enable maybe-exp-some->val)))))


(defines exp-free-vars
  (define exp-free-vars ((x exp-p))
    :measure (exp-count x)
    :returns (vars (and (varlist-p vars)
                        (setp vars)))
    :verify-guards nil
    (exp-case x
      :e_var (insert x.var nil)
      :e_op1 (exp-free-vars x.arg)
      :e_op (explist-free-vars x.args)
      :e_app (union (exp-free-vars x.fn)
                    (exp-free-vars x.arg))
      :e_bind (union (bindinglist-free-vars x.bindings)
                     (difference (exp-free-vars x.body)
                                 (bindinglist-bound-vars x.bindings)))
      :e_bindrec (difference (union (bindinglist-free-vars x.bindings)
                                    (exp-free-vars x.body))
                             (bindinglist-singleton-bound-vars x.bindings))
      :e_fun (mergesort x.free)
      :e_explicitset (explist-free-vars x.elems)
      :e_match (union (exp-free-vars x.arg)
                      (union (matchclauselist-free-vars x.clauses)
                             (maybe-exp-free-vars x.otherwise)))
      :e_matchset (union (exp-free-vars x.arg)
                         (union (exp-free-vars x.empty)
                                (set_clause-free-vars x.nonempty)))
      :e_try (union (exp-free-vars x.arg1)
                    (exp-free-vars x.arg2))
      :e_if (union (condition-free-vars x.test)
                   (union (exp-free-vars x.then)
                          (exp-free-vars x.else)))
      :otherwise nil))

  (define explist-free-vars ((x explist-p))
    :measure (explist-count x)
    :returns (vars (and (varlist-p vars)
                        (setp vars)))
    (if (atom x)
        nil
      (union (exp-free-vars (car x))
             (explist-free-vars (cdr x)))))

  (define maybe-exp-free-vars ((x maybe-exp-p))
    :measure (maybe-exp-count x)
    :returns (vars (and (varlist-p vars)
                        (setp vars)))
    (and x (exp-free-vars x)))

  (define set_clause-free-vars ((x set_clause-p))
    :measure (set_clause-count x)
    :returns (vars (and (varlist-p vars)
                        (setp vars)))
    (set_clause-case x
      :eltrem (difference (difference (exp-free-vars x.exp)
                                      (pat0-vars x.elt))
                          (pat0-vars x.rem))
      :preeltpost (difference (difference (difference (exp-free-vars x.exp)
                                                      (pat0-vars x.pre))
                                          (pat0-vars x.elt))
                              (pat0-vars x.post))))

  (define condition-free-vars ((x condition-p))
    :measure (condition-count x)
    :returns (vars (and (varlist-p vars)
                        (setp vars)))
    (condition-case x
        :cond_eq (union (exp-free-vars x.arg1)
                        (exp-free-vars x.arg2))
        :cond_subset (union (exp-free-vars x.arg1)
                            (exp-free-vars x.arg2))
        :cond_in (union (exp-free-vars x.arg1)
                        (exp-free-vars x.arg2))
        :otherwise nil))

  (define matchclause-free-vars ((x matchclause-p))
    :measure (matchclause-count x)
    :returns (vars (and (varlist-p vars)
                        (setp vars)))
    (b* (((matchclause x)))
      (exp-free-vars x.exp)))

  (define matchclauselist-free-vars ((x matchclauselist-p))
    :measure (matchclauselist-count x)
    :returns (vars (and (varlist-p vars)
                        (setp vars)))
    (if (atom x)
        nil
      (union (matchclause-free-vars (car x))
             (matchclauselist-free-vars (cdr x)))))

  (define binding-free-vars ((x binding-p))
    :measure (binding-count x)
    :returns (vars (and (varlist-p vars)
                        (setp vars)))
    (exp-free-vars (binding->exp x)))

  (define bindinglist-free-vars ((x bindinglist-p))
    :measure (bindinglist-count x)
    :returns (vars (and (varlist-p vars)
                        (setp vars)))
    (if (atom x)
        nil
      (union (binding-free-vars (car x))
             (bindinglist-free-vars (cdr x)))))

  ///
  (verify-guards exp-free-vars)
  (local (in-theory (enable maybe-exp-fix)))
  (fty::deffixequiv-mutual exp-free-vars))


(local (defthm len-of-tail-when-not-emptyp
         (implies (not (emptyp x))
                  (< (len (tail x)) (len x)))
         :hints(("Goal" :in-theory (enable emptyp tail)))
         :rule-classes :linear))




(define env-equiv-on-var-set ((vars varlist-p)
                              (env1 env-p)
                              (env2 env-p))
  :guard (setp vars)
  :measure (len (varlist-fix vars))
  (b* ((vars (varlist-fix vars)))
    (if (emptyp vars)
        t
      (and (equal (env-lookup (head vars) env1)
                  (env-lookup (head vars) env2))
           (env-equiv-on-var-set (tail vars) env1 env2))))
  ///
  (defthm env-equiv-on-var-set-implies
    (implies (and (env-equiv-on-var-set vars env1 env2)
                  (in (var-fix v) (varlist-fix vars)))
             (equal (equal (env-lookup v env1)
                           (env-lookup v env2))
                    t)))

  (defthmd env-equiv-on-var-set-implies-rw
    (implies (and (env-equiv-on-var-set vars env1 env2)
                  (in (var-fix v) (varlist-fix vars)))
             (equal (env-lookup v env1)
                    (env-lookup v env2)))
    :rule-classes ((:rewrite :match-free :all)))

  (defthm env-equiv-on-var-set-implies-extract
    (implies (and (env-equiv-on-var-set vars env1 env2)
                  (subset (mergesort (varlist-fix vars1)) (varlist-fix vars)))
             (equal (env-extract vars1 env1)
                    (env-extract vars1 env2)))
    :hints (("goal" 
             :in-theory (e/d (env-equiv-on-var-set-implies-rw
                              set::subset-in)
                             ())
             :use ((:instance env-diff-key-when-unequal
                    (x (env-extract vars1 env1))
                    (y (env-extract vars1 env2))))))))



(define env-equiv-on-var-set-badguy ((vars varlist-p)
                                     (env1 env-p)
                                     (env2 env-p))
  :guard (setp vars)
  :measure (len (varlist-fix vars))
  :returns (badguy (iff (var-p badguy) badguy))
  (b* ((vars (varlist-fix vars)))
    (if (emptyp vars)
      nil
      (if (equal (env-lookup (head vars) env1)
                 (env-lookup (head vars) env2))
          (env-equiv-on-var-set-badguy (tail vars) env1 env2)
        (head vars))))
  ///
  (defret <fn>-when-not-equiv
    (implies (not (env-equiv-on-var-set vars env1 env2))
             (and (in badguy (varlist-fix vars))
                  badguy
                  (not (equal (env-lookup badguy env1)
                              (env-lookup badguy env2)))))
    :hints(("Goal" :in-theory (enable env-equiv-on-var-set)))))

(defthm env-equiv-on-var-set-of-union
  (implies (and (varlist-p vars1)
                (varlist-p vars2))
           (iff (env-equiv-on-var-set (union vars1 vars2) env1 env2)
                (and (env-equiv-on-var-set vars1 env1 env2)
                     (env-equiv-on-var-set vars2 env1 env2))))
  :hints ((acl2::use-termhint
           (b* ((union (union vars1 vars2))
                (equiv1 (env-equiv-on-var-set union env1 env2))
                (equiv2 (env-equiv-on-var-set vars1 env1 env2)))
             (if equiv1
                 (if equiv2
                     (b* ((badguy (env-equiv-on-var-set-badguy vars2 env1 env2)))
                       `(:use ((:instance env-equiv-on-var-set-implies
                                (vars (union vars1 vars2))
                                (v ,(acl2::hq badguy)))
                               (:instance env-equiv-on-var-set-badguy-when-not-equiv
                                (vars vars2)))
                   :in-theory (disable env-equiv-on-var-set-implies
                                       env-equiv-on-var-set-badguy-when-not-equiv)))
                   (b* ((badguy (env-equiv-on-var-set-badguy vars1 env1 env2)))
                     `(:use ((:instance env-equiv-on-var-set-implies
                              (vars (union vars1 vars2))
                              (v ,(acl2::hq badguy)))
                             (:instance env-equiv-on-var-set-badguy-when-not-equiv
                              (vars vars1)))
                   :in-theory (disable env-equiv-on-var-set-implies
                                       env-equiv-on-var-set-badguy-when-not-equiv))))
               (b* ((badguy (env-equiv-on-var-set-badguy union env1 env2)))
                 `(:use ((:instance env-equiv-on-var-set-implies
                          (vars vars1) (v ,(acl2::hq badguy)))
                         (:instance env-equiv-on-var-set-implies
                          (vars vars2) (v ,(acl2::hq badguy)))
                         (:instance env-equiv-on-var-set-badguy-when-not-equiv
                          (vars ,(acl2::hq union))))
                   :in-theory (disable env-equiv-on-var-set-implies
                                       env-equiv-on-var-set-badguy-when-not-equiv))))))))





;; (define eval-binding-new-env ((x binding-p)
;;                               &key
;;                               ((ex execgraph-p) 'ex)
;;                               ((env env-p) 'env)
;;                               ((reclimit natp) 'reclimit))
;;   :returns (res env-p)
;;   :verify-guards nil
;;   (b* (((binding x))
;;        ((when (pat-case x.pat :pvar (not x.pat.var) :otherwise nil))
;;         nil)
;;        ((mv ?err val) (eval-exp x.exp))
;;        ((mv ?err bindings) (match-pat x.pat val)))
;;     bindings)
;;   ///
;;   (defretd <fn>-in-terms-of-eval-binding-new-env
;;     (implies (not err)
;;              (equal res
;;                     (append (eval-binding-new-env x) (env-fix env))))
;;     :hints(("Goal" :in-theory (enable eval-binding)))
;;     :fn eval-binding))

(local (defthm assoc-of-append
         (equal (append (append x y) z)
                (append x y z))))


(defthm env-combine-associative
  (equal (env-combine (env-combine a b) c)
         (env-combine a (env-combine b c)))
  :hints (("goal" :use ((:instance env-diff-key-when-unequal
                         (x (env-combine (env-combine a b) c))
                         (y (env-combine a (env-combine b c))))))))

(defthm env-combine-of-update-associative
  (equal (env-combine (env-update k v b) c)
         (env-update k v (env-combine b c)))
  :hints (("goal" :use ((:instance env-diff-key-when-unequal
                         (x (env-combine (env-update k v b) c))
                         (y (env-update k v (env-combine b c))))))))

(define eval-bindings-new-env ((x bindinglist-p)
                               &key
                               ((ex execgraph-p) 'ex)
                               ((env env-p) 'env)
                               ((reclimit natp) 'reclimit))
  :returns (res env-p)
  :verify-guards nil
  (b* (((when (atom x)) nil)
       ((mv err new-env1) (eval-binding (car x)))
       ((when err) nil)
       (new-env-rest (eval-bindings-new-env (cdr x))))
    (env-combine new-env1 new-env-rest))
  ///
  (defretd <fn>-in-terms-of-eval-bindings-new-env
    (implies (not err)
             (equal res
                    (env-combine (eval-bindings-new-env x) (env-fix env))))
    :hints(("Goal" :in-theory (enable eval-bindings)))
    :fn eval-bindings))


(define eval-fixpoint-binding-new-env ((x binding-p)
                                       &key
                                       ((ex execgraph-p) 'ex)
                                       ((env env-p) 'env)
                                       ((reclimit natp) 'reclimit))
  :returns (res env-p)
  :verify-guards nil
  (b* (((binding x))
       ((pvar x.pat))
       ((unless x.pat.var)
        nil)
       ((mv err val) (eval-exp x.exp)))
    (and (not err)
         (env-update x.pat.var val nil)))
  ///
  (defretd <fn>-in-terms-of-eval-fixpoint-binding-new-env
    (implies (not err)
             (equal res
                    (env-combine (eval-fixpoint-binding-new-env x) (env-fix env))))
    :hints(("Goal" :in-theory (enable eval-fixpoint-binding)))
    :fn eval-fixpoint-binding))

(define eval-fixpoint1-new-env ((x bindinglist-p)
                                &key
                                ((ex execgraph-p) 'ex)
                                ((env env-p) 'env)
                                ((reclimit natp) 'reclimit))
  :returns (res env-p)
  :verify-guards nil
  (b* (((when (atom x)) nil)
       (env1 (eval-fixpoint-binding-new-env (car x)))
       (env2 (eval-fixpoint1-new-env (cdr x) :env (env-combine env1 (env-fix env)))))
    (env-combine env2 env1))
  ///
  (defretd <fn>-in-terms-of-eval-fixpoint1-new-env
    (implies (not err)
             (equal res
                    (env-combine (eval-fixpoint1-new-env x) (env-fix env))))
    :hints(("Goal" :in-theory (enable eval-fixpoint1
                                      eval-fixpoint-binding-in-terms-of-eval-fixpoint-binding-new-env)))
    :fn eval-fixpoint1))


(define eval-fixpoint-new-env ((x bindinglist-p)
                               &key
                               ((ex execgraph-p) 'ex)
                               ((env env-p) 'env)
                               ((reclimit natp) 'reclimit))
  :returns (res env-p)
  :verify-guards nil
  :measure (nfix reclimit)
  (b* ((new-env (eval-fixpoint1-new-env x))
       ((mv & & changedp) (eval-fixpoint1 x))
       ((unless changedp) new-env)
       ((when (zp reclimit)) nil)
       (new-env2
        (eval-fixpoint-new-env x :env (env-combine new-env (env-fix env)) :reclimit (1- reclimit))))
    (env-combine new-env2 new-env))
  ///
  (defretd <fn>-in-terms-of-eval-fixpoint-new-env
    (implies (not err)
             (equal res
                    (env-combine (eval-fixpoint-new-env x) (env-fix env))))
    :hints(("Goal" :in-theory (enable eval-fixpoint
                                      eval-fixpoint1-in-terms-of-eval-fixpoint1-new-env)
            :induct (eval-fixpoint-new-env x)
            :expand ((eval-fixpoint x))))
    :fn eval-fixpoint))



;;(defsection eval-exp-of-free-var-equiv-envs
(local
 (progn
   ;; (defthm env-keys-member-env-lookup
   ;;   (implies (and (var-p v)
   ;;                 (env-p env))
   ;;            (iff (member-equal v (env-keys env))
   ;;                 (env-lookup v env)))
   ;;   :hints(("Goal" :in-theory (enable env-lookup))))
     
   ;; (in-theory (disable env-keys-member-hons-assoc-equal))
     
   (defthm difference-of-union
     (equal (difference (union x y) z)
            (union (difference x z)
                   (difference y z)))
     :hints(("Goal" :in-theory (enable double-containment
                                       pick-a-point-subset-strategy))))
       
     
   (defthm env-equiv-on-var-set-implies-check-fixpoint-binding
     (implies (and (env-equiv-on-var-set (pat-singleton-vars pat) env1 env)
                   ;; (env-p env1)
                   (pat-case pat :pvar)
                   (pvar->var pat))
              (equal (check-fixpoint-binding (pvar->var pat) exp env1)
                     (check-fixpoint-binding (pvar->var pat) exp env)))
     :hints(("Goal" :in-theory (enable check-fixpoint-binding
                                       pat-singleton-vars pat0-vars
                                       env-equiv-on-var-set-implies-rw))))

   ;; (defthm hons-assoc-equal-of-append
   ;;   (equal (hons-assoc-equal k (append x y))
   ;;          (or (hons-assoc-equal k x)
   ;;              (hons-assoc-equal k y))))

   
  
   ;; (local (defthm cdr-of-omap-assoc-env
   ;;         (implies (env-p env)
   ;;                  (iff (cdr (omap::assoc k env))
   ;;                       (omap::assoc k env)))
   ;;         :hints(("Goal" :in-theory (enable omap::assoc)))))
     
   (defthm env-equiv-on-var-set-of-update*-same
     (implies (env-equiv-on-var-set vars env1 env2)
              (env-equiv-on-var-set vars (env-combine new env1) (env-combine new env2)))
     :hints(("Goal" :in-theory (enable env-equiv-on-var-set))))

   (defthmd env-equiv-on-var-set-of-append-same-diff-lemma
     (implies (and (env-equiv-on-var-set vars2 env1 env2)
                   (subset (difference (varlist-fix vars)
                                       (mergesort (env-keys (env-fix new))))
                           (varlist-fix vars2)))
              (env-equiv-on-var-set vars (env-combine new env1) (env-combine new env2)))
     :hints(("Goal" :in-theory (enable env-equiv-on-var-set
                                       env-equiv-on-var-set-implies-rw
                                       in-env-keys-iff-env-lookup)
             :induct (env-equiv-on-var-set vars (env-combine new env1) (env-combine new env2))
             :expand ((:free (keys) (difference (varlist-fix vars) keys))))))

   (defthm env-equiv-on-var-set-of-append-same-diff
     (implies (env-equiv-on-var-set (difference (varlist-fix vars)
                                                (mergesort (env-keys (env-fix new))))
                                    env1 env2)
              (env-equiv-on-var-set vars (env-combine new env1) (env-combine new env2)))
     :hints (("goal" :use ((:instance env-equiv-on-var-set-of-append-same-diff-lemma
                            (vars2 (difference (varlist-fix vars)
                                               (mergesort (env-keys (env-fix new))))))))))

   (defthm env-keys-of-pair-pat0-tuple
     (equal (mergesort (env-keys (pair-pat0-tuple x elts)))
            (pat0-list-vars x))
     :hints(("Goal" :in-theory (enable pair-pat0-tuple pat0-list-vars pat0-vars
                                       mergesort))))

   (defthm env-keys-of-eval-binding
     (implies (not (mv-nth 0 (eval-binding x)))
              (equal (mergesort (env-keys (mv-nth 1 (eval-binding x))))
                     (pat-vars (binding->pat x))))
     :hints(("Goal" :in-theory (enable match-pat
                                       mergesort
                                       pat-vars pat0-vars)
             :expand ((eval-binding x)))))
                
     
   (defun-sk eval-exp-of-free-var-equiv-envs-cond (x ex env reclimit)
     (forall env1
             (implies (and (env-equiv-on-var-set (exp-free-vars x) env1 env))
                      (equal (eval-exp x :env env1)
                             (eval-exp x))))
     :rewrite :direct)

   (defun-sk eval-explist-of-free-var-equiv-envs-cond (x ex env reclimit)
     (forall env1
             (implies (and (env-equiv-on-var-set (explist-free-vars x) env1 env))
                      (equal (eval-explist x :env env1)
                             (eval-explist x))))
     :rewrite :direct)

   (defun-sk eval-bindings-of-free-var-equiv-envs-cond (x ex env reclimit)
     (forall env1
             (implies (and (env-equiv-on-var-set (bindinglist-free-vars x) env1 env))
                      (and (equal (mv-nth 0 (eval-bindings x :env env1))
                                  (mv-nth 0 (eval-bindings x)))
                           (implies (not (mv-nth 0 (eval-bindings x)))
                                    (equal
                                     (eval-bindings-new-env x :env env1)
                                     (eval-bindings-new-env x))))))
     :rewrite :direct)

   (defun-sk eval-binding-of-free-var-equiv-envs-cond (x ex env reclimit)
     (forall env1
             (implies (and (env-equiv-on-var-set (binding-free-vars x) env1 env))
                      (equal (eval-binding x :env env1)
                             (eval-binding x))))
     :rewrite :direct)

   (defun-sk eval-fixpoint-of-free-var-equiv-envs-cond (x ex env reclimit)
     (forall env1
             (implies (and (env-equiv-on-var-set (bindinglist-free-vars x) env1 env)
                           (env-equiv-on-var-set (bindinglist-singleton-bound-vars x) env1 env))
                      (and (equal (mv-nth 0 (eval-fixpoint x :env env1))
                                  (mv-nth 0 (eval-fixpoint x)))
                           (implies (not (mv-nth 0 (eval-fixpoint x)))
                                    (equal (eval-fixpoint-new-env x :env env1)
                                           (eval-fixpoint-new-env x))))))
     :rewrite :direct)

   (defun-sk eval-fixpoint1-of-free-var-equiv-envs-cond (x ex env reclimit)
     (forall env1
             (implies (and (env-equiv-on-var-set (bindinglist-free-vars x) env1 env)
                           (env-equiv-on-var-set (bindinglist-singleton-bound-vars x) env1 env))
                      (and (equal (mv-nth 0 (eval-fixpoint1 x :env env1))
                                  (mv-nth 0 (eval-fixpoint1 x)))
                           (equal (mv-nth 2 (eval-fixpoint1 x :env env1))
                                  (mv-nth 2 (eval-fixpoint1 x)))
                           (implies (not (mv-nth 0 (eval-fixpoint1 x)))
                                    (equal (eval-fixpoint1-new-env x :env env1)
                                           (eval-fixpoint1-new-env x))))))
     :rewrite :direct)

   (defun-sk eval-fixpoint-binding-of-free-var-equiv-envs-cond (x ex env reclimit)
     (forall env1
             (implies (and (env-equiv-on-var-set (binding-free-vars x) env1 env)
                           (env-equiv-on-var-set (pat-singleton-vars (binding->pat x)) env1 env))
                      (and (equal (mv-nth 0 (eval-fixpoint-binding x :env env1))
                                  (mv-nth 0 (eval-fixpoint-binding x)))
                           (equal (mv-nth 2 (eval-fixpoint-binding x :env env1))
                                  (mv-nth 2 (eval-fixpoint-binding x)))
                           (implies (not (mv-nth 0 (eval-fixpoint-binding x)))
                                    (equal (eval-fixpoint-binding-new-env x :env env1)
                                           (eval-fixpoint-binding-new-env x))))))
     :rewrite :direct)

   (defun-sk eval-condition-of-free-var-equiv-envs-cond (x ex env reclimit)
     (forall env1
             (implies (and (env-equiv-on-var-set (condition-free-vars x) env1 env))
                      (equal (eval-condition x :env env1)
                             (eval-condition x))))
     :rewrite :direct)
                        
   (in-theory (disable eval-exp-of-free-var-equiv-envs-cond
                       eval-explist-of-free-var-equiv-envs-cond
                       eval-bindings-of-free-var-equiv-envs-cond
                       eval-binding-of-free-var-equiv-envs-cond
                       eval-fixpoint-of-free-var-equiv-envs-cond
                       eval-fixpoint1-of-free-var-equiv-envs-cond
                       eval-fixpoint-binding-of-free-var-equiv-envs-cond
                       eval-condition-of-free-var-equiv-envs-cond))

   (local (in-theory (disable ACL2::ZP-FORWARD-TO-NAT-EQUIV-0)))

   (defthm env-keys-of-add-empty-bindings-to-env
     (equal (mergesort (env-keys (add-empty-bindings-to-env x env)))
            (union (bindinglist-singleton-bound-vars x) (mergesort (env-keys (env-fix env)))))
     :hints(("Goal" :in-theory (enable add-empty-bindings-to-env
                                       bindinglist-singleton-bound-vars
                                       mergesort pat0-vars pat-singleton-vars))))

   (defthm add-empty-bindings-to-env-normalize
     (implies (syntaxp (not (equal env ''nil)))
              (equal (add-empty-bindings-to-env x env)
                     (env-combine (add-empty-bindings-to-env x nil) (env-fix env))))
     :hints(("Goal" :in-theory (enable add-empty-bindings-to-env))))

   (defthm mergesort-of-append
     (Equal (mergesort (append x y))
            (union (mergesort x) (mergesort y)))
     :hints(("Goal" :in-theory (enable double-containment
                                       pick-a-point-subset-strategy))))


     
   (defthm env-keys-of-eval-bindings-new-env
     (implies (not (mv-nth 0 (eval-bindings x)))
              (equal (mergesort (env-keys (eval-bindings-new-env x)))
                     (bindinglist-bound-vars x)))
     :hints(("Goal" :in-theory (enable eval-bindings-new-env
                                       eval-bindings
                                       bindinglist-bound-vars
                                       set::union-symmetric)
             :expand ((Eval-bindings x))
             :induct (eval-bindings-new-env x))))

   (defthm env-keys-of-recursive-function-bindings-to-closures-aux
     (implies (function-bindings-p x)
              (equal (mergesort (env-keys (recursive-function-bindings-to-closures-aux
                                                   x defs env)))
                     (bindinglist-bound-vars x)))
     :hints(("Goal" :in-theory (enable function-bindings-p
                                       bindinglist-bound-vars
                                       recursive-function-bindings-to-closures-aux
                                       mergesort
                                       pat-vars
                                       pat0-vars))))

   (defthm env-keys-of-recursive-function-bindings-to-closures
     (implies (function-bindings-p x)
              (equal (mergesort (env-keys (recursive-function-bindings-to-closures x env)))
                     (bindinglist-bound-vars x)))
     :hints(("Goal" :in-theory (enable recursive-function-bindings-to-closures))))

   (defthm eval-var-when-env-equiv-on-var-set
     (implies (and (env-equiv-on-var-set (insert var nil) env1 env)
                   (var-p var))
              (equal (eval-var var ex env1)
                     (eval-var var ex env)))
     :hints(("Goal" :in-theory (enable env-equiv-on-var-set-implies-rw
                                       eval-var))))

   (defthm eval-fixpoint-binding-when-not-pvar
     (implies (not (pat-case (binding->pat x) :pvar))
              (mv-nth 0 (eval-fixpoint-binding x)))
     :hints (("goal" :expand ((eval-fixpoint-binding x)))))

   (defun varset-ind (vars)
     (declare (xargs :measure (len (varlist-fix vars))))
     (if (emptyp (varlist-fix vars))
         vars
       (varset-ind (tail (varlist-fix vars)))))
   
   (defthm env-equiv-on-var-set-of-append-same-vars
     (implies (subset (varlist-fix vars) (mergesort (env-keys (env-fix new))))
              (env-equiv-on-var-set vars (env-combine new env1) (env-combine new env2)))
     :hints(("Goal"
             :in-theory (enable in-env-keys-iff-env-lookup)
             :induct (varset-ind vars)
             :expand ((:free (keys) (subset (varlist-fix vars) keys))
                      (:free (env1 env2) (env-equiv-on-var-set vars env1 env2))))))


   (defthm fndeflist->names-of-function-bindings-to-fndefs
     (implies (function-bindings-p x)
              (equal (mergesort (fndeflist->names (function-bindings-to-fndefs x)))
                     (bindinglist-singleton-bound-vars x)))
     :hints(("Goal" :in-theory (enable function-bindings-to-fndefs
                                       function-bindings-p
                                       fndeflist->names
                                       bindinglist-singleton-bound-vars
                                       pat-singleton-vars
                                       pat0-vars
                                       mergesort))))

   (defthm keys-of-recursive-function-bindings-to-closures-aux
     (implies (function-bindings-p x)
              (equal (env-keys (recursive-function-bindings-to-closures-aux x defs env))
                     (bindinglist-singleton-bound-vars x)))
     :hints(("Goal" :in-theory (enable function-bindings-p
                                       recursive-function-bindings-to-closures-aux
                                       ;; env-keys
                                       bindinglist-singleton-bound-vars
                                       pat-singleton-vars pat0-vars pat-vars
                                       bindinglist-bound-vars
                                       mergesort))))

   (defthm keys-of-recursive-function-bindings-to-closures
     (implies (function-bindings-p x)
              (equal (mergesort (env-keys (recursive-function-bindings-to-closures x env)))
                     (bindinglist-singleton-bound-vars x)))
     :hints(("Goal" :in-theory (enable recursive-function-bindings-to-closures))))

   (defthm mergesort-of-set-difference-equal
     (equal (mergesort (set-difference-equal x y))
            (difference (mergesort x) (mergesort y)))
     :hints(("Goal" :in-theory (enable double-containment
                                       pick-a-point-subset-strategy))))
   
   (defthm keys-of-recursive-function-bindings-to-closures-aux-when-env-equiv-on-var-set
     (implies (and (env-equiv-on-var-set some-vars env1 env) ;; free variable binding hack
                   (env-equiv-on-var-set
                    (difference (bindinglist-free-vars x)
                                (mergesort (fndeflist->names defs)))
                    env1 env)
                   (function-bindings-p x))
              (equal (recursive-function-bindings-to-closures-aux x defs env1)
                     (recursive-function-bindings-to-closures-aux x defs env)))
     :hints(("Goal" :in-theory (enable function-bindings-p
                                       binding-free-vars
                                       exp-free-vars
                                       recursive-function-bindings-to-closures-aux
                                       bindinglist-free-vars))))

   (defthm keys-of-recursive-function-bindings-to-closures-when-env-equiv-on-var-set
     (implies (and (env-equiv-on-var-set
                    (difference (bindinglist-free-vars x)
                                (bindinglist-singleton-bound-vars x))
                    env1 env)
                   (function-bindings-p x))
              (equal (recursive-function-bindings-to-closures x env1)
                     (recursive-function-bindings-to-closures x env)))
     :hints(("Goal" :in-theory (enable recursive-function-bindings-to-closures))))


   
   ))

(local
 (with-output :evisc (:gag-mode (evisc-tuple 3 4 nil nil))
   (defthm-eval-exp-flag eval-exp-of-free-var-equiv-envs-lemma
     (defthm eval-exp-of-free-var-equiv-envs-lemma
       (eval-exp-of-free-var-equiv-envs-cond x ex env reclimit)
       :hints ((and stable-under-simplificationp
                    `(:expand (,(car (last clause))
                               (:Free (env) (eval-exp x))
                               (exp-free-vars x))
                      :in-theory (enable eval-fixpoint-in-terms-of-eval-fixpoint-new-env
                                         eval-bindings-in-terms-of-eval-bindings-new-env))))
       :flag eval-exp)

     (defthm eval-explist-of-free-var-equiv-envs-lemma
       (eval-explist-of-free-var-equiv-envs-cond x ex env reclimit)
       :hints ((and stable-under-simplificationp
                    `(:expand (,(car (last clause))
                               (:Free (env) (eval-explist x))
                               (explist-free-vars x)))))
       :flag eval-explist)

     (defthm eval-bindings-of-free-var-equiv-envs-lemma
       (eval-bindings-of-free-var-equiv-envs-cond x ex env reclimit)
       :hints ((and stable-under-simplificationp
                    `(:expand (,(car (last clause))
                               (:Free (env) (eval-bindings x))
                               (bindinglist-free-vars x)
                               (:free (env) (eval-bindings-new-env x)))
                      ;; :in-theory (enable eval-binding-in-terms-of-eval-binding-new-env)
                      )))
       :flag eval-bindings)

     (defthm eval-binding-of-free-var-equiv-envs-lemma
       (eval-binding-of-free-var-equiv-envs-cond x ex env reclimit)
       :hints ((and stable-under-simplificationp
                    `(:expand (,(car (last clause))
                               (:Free (env) (eval-binding x))
                               (binding-free-vars x)))))
       :flag eval-binding)

     (defthm eval-fixpoint-of-free-var-equiv-envs-lemma
       (eval-fixpoint-of-free-var-equiv-envs-cond x ex env reclimit)
       :hints ((and stable-under-simplificationp
                    `(:expand (,(car (last clause))
                               (:Free (env) (eval-fixpoint x))
                               (eval-fixpoint-new-env x)
                               (eval-fixpoint-new-env x :env (eval-fixpoint-of-free-var-equiv-envs-cond-witness
                                                              x ex env reclimit)))
                      :in-theory (enable eval-fixpoint1-in-terms-of-eval-fixpoint1-new-env))))
       :flag eval-fixpoint)

     (defthm eval-fixpoint1-of-free-var-equiv-envs-lemma
       (eval-fixpoint1-of-free-var-equiv-envs-cond x ex env reclimit)
       :hints ((and stable-under-simplificationp
                    `(:expand (,(car (last clause))
                               (:Free (env) (eval-fixpoint1 x))
                               (:free (env) (eval-fixpoint1-new-env x))
                               (bindinglist-free-vars x)
                               (bindinglist-singleton-bound-vars x))
                      :in-theory (enable eval-fixpoint-binding-in-terms-of-eval-fixpoint-binding-new-env
                                         pat-vars))))
       :flag eval-fixpoint1)

     (defthm eval-fixpoint-binding-of-free-var-equiv-envs-lemma
       (eval-fixpoint-binding-of-free-var-equiv-envs-cond x ex env reclimit)
       :hints ((and stable-under-simplificationp
                    `(:expand (,(car (last clause))
                               (:Free (env) (eval-fixpoint-binding x))
                               (:free (env) (eval-fixpoint-binding-new-env x))
                               (binding-free-vars x))
                      ;; :in-theory (enable pat-singleton-vars pat0-vars)
                      ;; :in-theory (enable check-fixpoint-binding
                      ;;                    env-equiv-on-var-set-implies-rw
                      ;;                    pat-vars pat0-vars)
                      )))
       :flag eval-fixpoint-binding)

     (defthm eval-condition-of-free-var-equiv-envs-lemma
       (eval-condition-of-free-var-equiv-envs-cond x ex env reclimit)
       :hints ((and stable-under-simplificationp
                    `(:expand (,(car (last clause))
                               (:Free (env) (eval-condition x))
                               (condition-free-vars x)))))
       :flag eval-condition)
     :hints ((and stable-under-simplificationp
                  '(:do-not-induct t)))
     )))




(defthm eval-exp-of-free-var-equiv-envs
  (implies (env-equiv-on-var-set (exp-free-vars x) env1 env)
           (equal (eval-exp x :env env1)
                  (eval-exp x)))
  :hints (("goal" :use ((:instance eval-exp-of-free-var-equiv-envs-lemma))
           :in-theory (disable eval-exp-of-free-var-equiv-envs-lemma))))

(defthm eval-explist-of-free-var-equiv-envs
  (implies (env-equiv-on-var-set (explist-free-vars x) env1 env)
           (equal (eval-explist x :env env1)
                  (eval-explist x)))
  :hints (("goal" :use ((:instance eval-explist-of-free-var-equiv-envs-lemma))
           :in-theory (disable eval-explist-of-free-var-equiv-envs-lemma))))

(defthm eval-bindings-of-free-var-equiv-envs
  (implies (env-equiv-on-var-set (bindinglist-free-vars x) env1 env)
           (and (equal (mv-nth 0 (eval-bindings x :env env1))
                       (mv-nth 0 (eval-bindings x)))
                (implies (not (mv-nth 0 (eval-bindings x)))
                         (equal
                          (eval-bindings-new-env x :env env1)
                          (eval-bindings-new-env x)))))
  :hints (("goal" :use ((:instance eval-bindings-of-free-var-equiv-envs-lemma))
           :in-theory (disable eval-bindings-of-free-var-equiv-envs-lemma))))

(defthm eval-binding-of-free-var-equiv-envs
  (implies (env-equiv-on-var-set (binding-free-vars x) env1 env)
           (equal (eval-binding x :env env1)
                  (eval-binding x)))
  :hints (("goal" :use ((:instance eval-binding-of-free-var-equiv-envs-lemma))
           :in-theory (disable eval-binding-of-free-var-equiv-envs-lemma))))

(defthm eval-fixpoint-of-free-var-equiv-envs
  (implies (and (env-equiv-on-var-set (bindinglist-free-vars x) env1 env)
                (env-equiv-on-var-set (bindinglist-singleton-bound-vars x) env1 env))
           (and (equal (mv-nth 0 (eval-fixpoint x :env env1))
                       (mv-nth 0 (eval-fixpoint x)))
                (implies (not (mv-nth 0 (eval-fixpoint x)))
                         (equal (eval-fixpoint-new-env x :env env1)
                                (eval-fixpoint-new-env x)))))
  :hints (("goal" :use ((:instance eval-fixpoint-of-free-var-equiv-envs-lemma))
           :in-theory (disable eval-fixpoint-of-free-var-equiv-envs-lemma))))

(defthm eval-fixpoint1-of-free-var-equiv-envs
  (implies (and (env-equiv-on-var-set (bindinglist-free-vars x) env1 env)
                (env-equiv-on-var-set (bindinglist-singleton-bound-vars x) env1 env))
           (and (equal (mv-nth 0 (eval-fixpoint1 x :env env1))
                       (mv-nth 0 (eval-fixpoint1 x)))
                (equal (mv-nth 2 (eval-fixpoint1 x :env env1))
                       (mv-nth 2 (eval-fixpoint1 x)))
                (implies (not (mv-nth 0 (eval-fixpoint1 x)))
                         (equal (eval-fixpoint1-new-env x :env env1)
                                (eval-fixpoint1-new-env x)))))
  :hints (("goal" :use ((:instance eval-fixpoint1-of-free-var-equiv-envs-lemma))
           :in-theory (disable eval-fixpoint1-of-free-var-equiv-envs-lemma))))

(defthm eval-fixpoint-binding-of-free-var-equiv-envs
  (implies (and (env-equiv-on-var-set (binding-free-vars x) env1 env)
                (env-equiv-on-var-set (pat-singleton-vars (binding->pat x)) env1 env))
           (and (equal (mv-nth 0 (eval-fixpoint-binding x :env env1))
                       (mv-nth 0 (eval-fixpoint-binding x)))
                (equal (mv-nth 2 (eval-fixpoint-binding x :env env1))
                       (mv-nth 2 (eval-fixpoint-binding x)))
                (implies (not (mv-nth 0 (eval-fixpoint-binding x)))
                         (equal (eval-fixpoint-binding-new-env x :env env1)
                                (eval-fixpoint-binding-new-env x)))))
  :hints (("goal" :use ((:instance eval-fixpoint-binding-of-free-var-equiv-envs-lemma))
           :in-theory (disable eval-fixpoint-binding-of-free-var-equiv-envs-lemma))))

(defthm eval-condition-of-free-var-equiv-envs
  (implies (env-equiv-on-var-set (condition-free-vars x) env1 env)
           (equal (eval-condition x :env env1)
                  (eval-condition x)))
  :hints (("goal" :use ((:instance eval-condition-of-free-var-equiv-envs-lemma))
           :in-theory (disable eval-condition-of-free-var-equiv-envs-lemma))))


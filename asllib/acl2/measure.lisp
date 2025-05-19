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

(include-book "ast")
(local (std::add-default-post-define-hook :fix))


(defines lexpr-count*
  (define lexpr_desc-count* ((x lexpr_desc-p))
    :measure (lexpr_desc-count x)
    :returns (count posp :rule-classes :type-prescription)
    (lexpr_desc-case x
      :le_slice (+ 3
                   (lexpr-count* x.base)
                   (slicelist-count x.slices))
      :le_setarray (+ 3 (lexpr-count* x.base)
                      (expr-count x.index))
      :le_setenumarray (+ 3 (lexpr-count* x.base)
                          (expr-count x.index))
      :le_setfield (+ 3 (lexpr-count* x.base))
      :le_setfields (+ 3 (lexpr-count* x.base))
      :le_destructuring (+ 2 (lexprlist-count* x.elts))
      :otherwise 1))

  (define lexpr-count* ((x lexpr-p))
    :measure (lexpr-count x)
    :returns (count posp :rule-classes :type-prescription)
    (+ 2 (lexpr_desc-count* (lexpr->val x))))

  (define lexprlist-count* ((x lexprlist-p))
    :measure (lexprlist-count x)
    :returns (count posp :rule-classes :type-prescription)
    (if (atom x)
        1
      (+ 1 (lexpr-count* (car x))
         (lexprlist-count* (cdr x)))))
  ///
  (defthm lexpr_desc-count*-le_slice
    (implies (lexpr_desc-case x :le_slice)
             (b* (((le_slice x)))
               (< (+ (lexpr-count* x.base)
                     (slicelist-count x.slices))
                  (lexpr_desc-count* x))))
    :hints (("goal" :expand ((lexpr_desc-count* x))))
    :rule-classes :linear)

  (defthm lexpr_desc-count*-le_setarray
    (implies (lexpr_desc-case x :le_setarray)
             (b* (((le_setarray x)))
               (< (+ (lexpr-count* x.base)
                     (expr-count x.index))
                  (lexpr_desc-count* x))))
    :hints (("goal" :expand ((lexpr_desc-count* x))))
    :rule-classes :linear)

  (defthm lexpr_desc-count*-le_setenumarray
    (implies (lexpr_desc-case x :le_setenumarray)
             (b* (((le_setenumarray x)))
               (< (+ (lexpr-count* x.base)
                     (expr-count x.index))
                  (lexpr_desc-count* x))))
    :hints (("goal" :expand ((lexpr_desc-count* x))))
    :rule-classes :linear)

  (defthm lexpr_desc-count*-le_setfield
    (implies (lexpr_desc-case x :le_setfield)
             (b* (((le_setfield x)))
               (< (lexpr-count* x.base)
                  (lexpr_desc-count* x))))
    :hints (("goal" :expand ((lexpr_desc-count* x))))
    :rule-classes :linear)

  (defthm lexpr_desc-count*-le_setfields
    (implies (lexpr_desc-case x :le_setfields)
             (b* (((le_setfields x)))
               (< (lexpr-count* x.base)
                  (lexpr_desc-count* x))))
    :hints (("goal" :expand ((lexpr_desc-count* x))))
    :rule-classes :linear)

  (defthm lexpr_desc-count*-le_destructuring
    (implies (lexpr_desc-case x :le_destructuring)
             (b* (((le_destructuring x)))
               (< (lexprlist-count* x.elts)
                  (lexpr_desc-count* x))))
    :hints (("goal" :expand ((lexpr_desc-count* x))))
    :rule-classes :linear)

  (defthm lexpr-count*-lexpr->val
    (< (lexpr_desc-count* (lexpr->val x))
       (lexpr-count* x))
    :hints (("goal" :expand ((lexpr-count* x))))
    :rule-classes :linear)

  (defthm lexprlist-count*-strong
    (implies (consp x)
             (< (+ (lexpr-count* (car x))
                   (lexprlist-count* (cdr x)))
                (lexprlist-count* x)))
    :hints (("goal" :expand ((lexprlist-count* x))))
    :rule-classes :linear)

  (fty::deffixequiv-mutual lexpr-count*))


(defines expr_of_lexpr
  :ruler-extenders (expr)
  (define expr_of_lexpr ((x lexpr-p))
    :returns (res expr-p)
    :measure (lexpr-count x)
    :verify-guards nil
    (b* ((x (lexpr->val x)))
      (expr
       (lexpr_desc-case x
         :le_var (e_var x.name)
         :le_slice (e_slice (expr_of_lexpr x.base) x.slices)
         :le_setarray (e_getarray (expr_of_lexpr x.base) x.index)
         :le_setenumarray (e_getenumarray (expr_of_lexpr x.base) x.index)
         :le_setfield (e_getfield (expr_of_lexpr x.base) x.field)
         :le_setfields (e_getfields (expr_of_lexpr x.base) x.fields)
         :le_setcollectionfields (e_getcollectionfields x.base x.fields)
         :le_discard (e_var "-") ;; ??? i think this is supposed to be prevented by type safety
         :le_destructuring (e_tuple (exprlist_of_lexprlist x.elts))))))
  (define exprlist_of_lexprlist ((x lexprlist-p))
    :returns (res exprlist-p)
    :measure (lexprlist-count x)
    (if (atom x)
        nil
      (cons (expr_of_lexpr (car x))
            (exprlist_of_lexprlist (cdr x)))))
  ///
  (verify-guards expr_of_lexpr)

  (std::defret-mutual expr-count-of-<fn>
    (defret expr-count-of-<fn>
      (<= (expr-count (expr_of_lexpr x))
          (lexpr-count* x))
      :hints ('(:expand ((expr_of_lexpr x)
                         (lexpr-count* x)
                         (lexpr_desc-count* (lexpr->val x))
                         (:free (x) (expr-count (expr x)))
                         (:free (x) (expr_desc-count (e_var x)))
                         (:free (x y) (expr_desc-count (e_slice x y)))
                         (:free (x y) (expr_desc-count (e_getarray x y)))
                         (:free (x y) (expr_desc-count (e_getenumarray x y)))
                         (:free (x y) (expr_desc-count (e_getfield x y)))
                         (:free (x y) (expr_desc-count (e_getfields x y)))
                         (:free (x y) (expr_desc-count (e_getcollectionfields x y)))
                         (:free (x) (expr_desc-count (e_tuple x))))))
      :rule-classes :linear
      :fn expr_of_lexpr)
    (defret exprlist-count-of-<fn>
      (<= (exprlist-count (exprlist_of_lexprlist x))
          (lexprlist-count* x))
      :hints ('(:expand ((exprlist_of_lexprlist x)
                         (lexprlist-count* x)
                         (:free (x y) (exprlist-count (cons x y)))
                         (exprlist-count nil))))
      :rule-classes :linear
      :fn exprlist_of_lexprlist)))



(define maybe-expr-count ((x maybe-expr-p))
  :returns (count posp :rule-classes :type-prescription)
  (if x (+ 1 (expr-count x)) 1)
  ///
  (defthm maybe-expr-count-linear
    (implies x
             (< (expr-count x) (maybe-expr-count x)))
    :rule-classes :linear))

(define expr*maybe-ty-count ((x expr*maybe-ty-p))
  :returns (count posp :rule-classes :type-prescription)
  (b* (((expr*maybe-ty x)))
    (+ 1 (expr-count x.expr) (maybe-ty-count x.ty)))
  ///
  (defthm expr*maybe-ty-count-linear
    (b* (((expr*maybe-ty x)))
      (< (+ (expr-count x.expr) (maybe-ty-count x.ty))
         (expr*maybe-ty-count x)))
    :rule-classes :linear))

(define maybe-[expr*maybe-ty]-count ((x maybe-[expr*maybe-ty]-p))
  :returns (count posp :rule-classes :type-prescription)
  (if x (+ 1 (expr*maybe-ty-count x)) 1)
  ///
  (defthm maybe-[expr*maybe-ty]-count-linear
    (implies x
             (< (expr*maybe-ty-count x) (maybe-[expr*maybe-ty]-count x)))
    :rule-classes ((:linear
                    :trigger-terms
                    ((maybe-[expr*maybe-ty]-count x)
                     (expr*maybe-ty-count x))))))


(defines stmt-count*
  :hints (("goal" :expand ((maybe-stmt-count x)
                           (maybe-stmt-some->val x))))
  (define stmt_desc-count* ((x stmt_desc-p))
    :measure (stmt_desc-count x)
    :returns (count posp :rule-classes :type-prescription)
    (stmt_desc-case x
      :s_seq (+ 1 (stmt-count* x.first)
                (stmt-count* x.second))
      :s_decl (+ 1
                 (maybe-ty-count x.ty)
                 (maybe-expr-count x.expr))
      :s_assign (+ 1 (lexpr-count* x.lexpr)
                   (expr-count x.expr))
      :s_call (+ 1 (call-count x.call))
      :s_return (+ 1 (maybe-expr-count x.expr))
      :s_cond (+ 1 (expr-count x.test)
                 (stmt-count* x.then)
                 (stmt-count* x.else))
      :s_assert (+ 1 (expr-count x.expr))
      :s_for (+ 1 (expr-count x.start_e)
                (expr-count x.end_e)
                (stmt-count* x.body)
                (maybe-expr-count x.limit))
      :s_while (+ 1 (expr-count x.test)
                  (maybe-expr-count x.limit)
                  (stmt-count* x.body))
      :s_repeat (+ 1 (stmt-count* x.body)
                   (expr-count x.test)
                   (maybe-expr-count x.limit))
      :s_throw (+ 1 (maybe-[expr*maybe-ty]-count x.val))
      :s_try (+ 1 (stmt-count* x.body)
                (catcherlist-count* x.catchers)
                (maybe-stmt-count* x.otherwise))
      :s_print (+ 1 (exprlist-count x.args))
      :s_pragma (+ 1 (exprlist-count x.exprs))
      :otherwise 1))

  (define stmt-count* ((x stmt-p))
    :measure (stmt-count x)
    :returns (count posp :rule-classes :type-prescription)
    (+ 1 (stmt_desc-count* (stmt->val x))))

  (define maybe-stmt-count* ((x maybe-stmt-p))
    :measure (maybe-stmt-count x)
    :returns (count posp :rule-classes :type-prescription)
    (if x (+ 1 (stmt-count* x)) 1))

  (define catcher-count* ((x catcher-p))
    :measure (catcher-count x)
    :returns (count posp :rule-classes :type-prescription)
    (b* (((catcher x)))
      (+ 1 (ty-count x.ty) (stmt-count* x.stmt))))

  (define catcherlist-count* ((x catcherlist-p))
    :measure (catcherlist-count x)
    :returns (count posp :rule-classes :type-prescription)
    (if (atom x)
        1
      (+ 1 (catcher-count* (car x))
         (catcherlist-count* (cdr x)))))
  ///
  (local (set-default-hints
          '('(:expand ((stmt_desc-count* x)
                       (stmt-count* x)
                       (maybe-stmt-count* x)
                       (catcher-count* x)
                       (catcherlist-count* x))))))

  (local (in-theory (disable stmt_desc-count*
                             stmt-count*
                             maybe-stmt-count*
                             catcherlist-count*
                             catcher-count*)))
  
  (defthm stmt_desc-count*-s_seq
    (implies (stmt_desc-case x :s_seq)
             (b* (((s_seq x)))
               (< (+ (stmt-count* x.first)
                     (stmt-count* x.second))
                  (stmt_desc-count* x))))
    :rule-classes :linear)

  (defthm stmt_desc-count*-s_decl
    (implies (stmt_desc-case x :s_decl)
             (b* (((s_decl x)))
               (< (+ (maybe-ty-count x.ty)
                 (maybe-expr-count x.expr))
                  (stmt_desc-count* x))))
    :rule-classes :linear)

  (defthm stmt_desc-count*-s_assign
    (implies (stmt_desc-case x :s_assign)
             (b* (((s_assign x)))
               (< (+ (lexpr-count* x.lexpr)
                     (expr-count x.expr))
                  (stmt_desc-count* x))))
    :rule-classes :linear)

  (defthm stmt_desc-count*-s_call
    (implies (stmt_desc-case x :s_call)
             (b* (((s_call x)))
               (< (call-count x.call)
                  (stmt_desc-count* x))))
    :rule-classes :linear)

  (defthm stmt_desc-count*-s_return
    (implies (stmt_desc-case x :s_return)
             (b* (((s_return x)))
               (< (maybe-expr-count x.expr)
                  (stmt_desc-count* x))))
    :rule-classes :linear)

  (defthm stmt_desc-count*-s_cond
    (implies (stmt_desc-case x :s_cond)
             (b* (((s_cond x)))
               (< (+ (expr-count x.test)
                     (stmt-count* x.then)
                     (stmt-count* x.else))
                  (stmt_desc-count* x))))
    :rule-classes :linear)

  (defthm stmt_desc-count*-s_assert
    (implies (stmt_desc-case x :s_assert)
             (b* (((s_assert x)))
               (< (expr-count x.expr)
                  (stmt_desc-count* x))))
    :rule-classes :linear)

  (defthm stmt_desc-count*-s_for
    (implies (stmt_desc-case x :s_for)
             (b* (((s_for x)))
               (< (+ (expr-count x.start_e)
                     (expr-count x.end_e)
                     (stmt-count* x.body)
                     (maybe-expr-count x.limit))
                  (stmt_desc-count* x))))
    :rule-classes :linear)

  (defthm stmt_desc-count*-s_while
    (implies (stmt_desc-case x :s_while)
             (b* (((s_while x)))
               (< (+ (expr-count x.test)
                     (maybe-expr-count x.limit)
                     (stmt-count* x.body))
                  (stmt_desc-count* x))))
    :rule-classes :linear)

  (defthm stmt_desc-count*-s_repeat
    (implies (stmt_desc-case x :s_repeat)
             (b* (((s_repeat x)))
               (< (+ (stmt-count* x.body)
                     (expr-count x.test)
                     (maybe-expr-count x.limit))
                  (stmt_desc-count* x))))
    :rule-classes :linear)

  (defthm stmt_desc-count*-s_throw
    (implies (stmt_desc-case x :s_throw)
             (b* (((s_throw x)))
               (< (maybe-[expr*maybe-ty]-count x.val)
                  (stmt_desc-count* x))))
    :rule-classes :linear)
  
  (defthm stmt_desc-count*-s_try
    (implies (stmt_desc-case x :s_try)
             (b* (((s_try x)))
               (< (+ (stmt-count* x.body)
                     (catcherlist-count* x.catchers)
                     (maybe-stmt-count* x.otherwise))
                  (stmt_desc-count* x))))
    :rule-classes :linear)

  (defthm stmt_desc-count*-s_print
    (implies (stmt_desc-case x :s_print)
             (b* (((s_print x)))
               (< (exprlist-count x.args)
                  (stmt_desc-count* x))))
    :rule-classes :linear)

  (defthm stmt_desc-count*-s_pragma
    (implies (stmt_desc-case x :s_pragma)
             (b* (((s_pragma x)))
               (< (exprlist-count x.exprs)
                  (stmt_desc-count* x))))
    :rule-classes :linear)

  (defthm stmt-count*-linear
    (< (stmt_desc-count* (stmt->val x)) (stmt-count* x))
    :rule-classes :linear)

  (defthm maybe-stmt-count*-linear
    (implies x
             (< (stmt-count* x) (maybe-stmt-count* x)))
    :rule-classes ((:linear :trigger-terms ((maybe-stmt-count* x)
                                            (stmt-count* x)))))

  (defthm catcher-count*-linear
    (b* (((catcher x)))
      (< (+ (ty-count x.ty) (stmt-count* x.stmt)) (catcher-count* x)))
    :rule-classes :linear)

  (defthm catcherlist-count*-linear
    (implies (consp x)
             (< (+ (catcher-count* (car x))
                   (catcherlist-count* (cdr x)))
                (catcherlist-count* x)))
    :rule-classes :linear)

  (defthm maybe-stmt-fix-when-x
    (implies x
             (equal (maybe-stmt-fix x) (stmt-fix x)))
    :hints(("Goal" :in-theory (enable maybe-stmt-fix))))

  (local (set-default-hints nil))
  (fty::deffixequiv-mutual stmt-count*))

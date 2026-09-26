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

(include-book "orac-to-arbvals")
(include-book "arbvals-scope")
(local (std::add-default-post-define-hook :fix))



(local (in-theory (acl2::disable* asl-*ota-equals-original-rules)))






(define arbmap-in-scope ((x arbmap-p)
                         (base arbaddr-p)
                         (offset arbaddr-offset-p)
                         (scope arbaddr-offset-p))
  (if (atom x)
      t
    (and (or (not (mbt (and (consp (car x))
                            (arbaddr-p (caar x)))))
             (arbaddr-in-scope (caar x) base offset scope))
         (arbmap-in-scope (cdr x) base offset scope)))
  ///
  (defthm arbmap-in-scope-of-append
    (iff (arbmap-in-scope (append x y) base offset scope)
         (and (arbmap-in-scope x base offset scope)
              (arbmap-in-scope y base offset scope))))

  (defthm arbmap-in-scope-when-in-narrower-scope
    (implies (and (arbmap-in-scope x base offset1 scope1)
                  (arbaddr-offset-lte offset offset1)
                  (arbaddr-offset-lte scope1 scope))
             (arbmap-in-scope x base offset scope)))

  (defthm arbmap-in-scope-of-nil
    (arbmap-in-scope nil base offset scope))

  (defthm arbmap-in-scope-when-in-deeper-scope
    (implies (and (arbmap-in-scope arbmap (cons component arbaddr) arboffset1 scope1)
                  (arbaddr-offset-lte-component arboffset component)
                  (not (arbaddr-offset-lte-component scope component)))
             (arbmap-in-scope arbmap arbaddr arboffset scope)))

  (defthm arbmap-in-scope-of-cons
    (implies (arbaddr-p a)
             (equal (arbmap-in-scope (cons (cons a v) arbmap) base offset scope)
                    (and (arbaddr-in-scope a base offset scope)
                         (arbmap-in-scope arbmap base offset scope)))))
  
  (local (in-theory (enable arbmap-fix))))


(define arbmap-in-loop-scope ((x arbmap-p)
                              (base arbaddr-p)
                              (is_while)
                              (loop-iter natp)
                              (loop-idx natp))
  (if (atom x)
      t
    (and (or (not (mbt (and (consp (car x))
                            (arbaddr-p (caar x)))))
             (arbaddr-in-loop-scope (caar x) base is_while loop-iter loop-idx))
         (arbmap-in-loop-scope (cdr x) base is_while loop-iter loop-idx)))
  ///
  
  (defthm arbmap-in-loop-scope-of-append
    (iff (arbmap-in-loop-scope (append x y) base is_while loop-iter loop-idx)
         (and (arbmap-in-loop-scope x base is_while loop-iter loop-idx)
              (arbmap-in-loop-scope y base is_while loop-iter loop-idx))))

  (defthm arbmap-in-loop-scope-of-nil
    (arbmap-in-loop-scope nil base is_while loop-iter loop-idx))

  
  (defthm arbmap-in-loop-scope-when-in-scope-while
    (implies (and (arbmap-in-scope arbmap (cons (arbaddr-component-while loop-idx loop-iter)
                                                arbaddr) offset scope)
                  is_while)
             (arbmap-in-loop-scope arbmap arbaddr is_while loop-iter loop-idx))
    :hints(("Goal" :in-theory (enable arbmap-in-scope))))

  (defthm arbmap-in-loop-scope-when-in-scope-repeat
    (implies (and (arbmap-in-scope arbmap (cons (arbaddr-component-repeat loop-idx loop-iter)
                                                arbaddr) offset scope))
             (arbmap-in-loop-scope arbmap arbaddr nil loop-iter loop-idx))
    :hints(("Goal" :in-theory (enable arbmap-in-scope))))

  (defthm arbmap-in-loop-scope-of-incr
    (implies (arbmap-in-loop-scope arbmap arbaddr is_while (+ 1 (nfix loop-iter)) loop-idx)
             (arbmap-in-loop-scope arbmap arbaddr is_while loop-iter loop-idx)))

  (defthm arbmap-in-scope-when-arbmap-in-loop-scope-of-repeat
    (implies (arbmap-in-loop-scope arbmap arbaddr nil iter (arbaddr-offset->repeatoffset arboffset))
             (arbmap-in-scope arbmap arbaddr arboffset (arbaddr-offset-sum arboffset
                                                                          '(nil 0 0 0 1))))
    :hints(("Goal" :in-theory (enable arbmap-in-scope))))

  (defthm arbaddr-in-scope-when-arbaddr-in-loop-scope-of-repeat-gen
    (implies (and (arbaddr-in-loop-scope addr arbaddr nil iter (arbaddr-offset->repeatoffset loopoffset))
                  (arbaddr-offset-lte arboffset loopoffset)
                  (arbaddr-offset-lte (arbaddr-offset-sum loopoffset '(nil 0 0 0 1)) scope))
             (arbaddr-in-scope addr arbaddr arboffset scope))
    :hints(("Goal" :in-theory (enable arbaddr-in-scope
                                      arbaddr-in-loop-scope
                                      arbaddr-offset-lte
                                      arbaddr-offset-lte-component
                                      arbaddr-offset-sum))))
  
  (defthm arbmap-in-scope-when-arbmap-in-loop-scope-of-repeat-gen
    (implies (and (arbmap-in-loop-scope arbmap arbaddr nil iter (arbaddr-offset->repeatoffset loopoffset))
                  (arbaddr-offset-lte arboffset loopoffset)
                  (arbaddr-offset-lte (arbaddr-offset-sum loopoffset '(nil 0 0 0 1)) scope))
             (arbmap-in-scope arbmap arbaddr arboffset scope))
    :hints(("Goal" :in-theory (enable arbmap-in-scope))))

  (defthm arbaddr-in-scope-when-arbaddr-in-loop-scope-of-while-gen
    (implies (and (arbaddr-in-loop-scope addr arbaddr t iter (arbaddr-offset->whileoffset loopoffset))
                  (arbaddr-offset-lte arboffset loopoffset)
                  (arbaddr-offset-lte (arbaddr-offset-sum loopoffset '(nil 0 0 1 0)) scope))
             (arbaddr-in-scope addr arbaddr arboffset scope))
    :hints(("Goal" :in-theory (enable arbaddr-in-scope
                                      arbaddr-in-loop-scope
                                      arbaddr-offset-lte
                                      arbaddr-offset-lte-component
                                      arbaddr-offset-sum))))
  
  (defthm arbmap-in-scope-when-arbmap-in-loop-scope-of-while-gen
    (implies (and (arbmap-in-loop-scope arbmap arbaddr t iter (arbaddr-offset->whileoffset loopoffset))
                  (arbaddr-offset-lte arboffset loopoffset)
                  (arbaddr-offset-lte (arbaddr-offset-sum loopoffset '(nil 0 0 1 0)) scope))
             (arbmap-in-scope arbmap arbaddr arboffset scope))
    :hints(("Goal" :in-theory (enable arbmap-in-scope))))

  (defthm arbmap-in-scope-when-arbmap-in-loop-scope-of-while
    (implies (arbmap-in-loop-scope arbmap arbaddr t iter (arbaddr-offset->whileoffset arboffset))
             (arbmap-in-scope arbmap arbaddr arboffset (arbaddr-offset-sum arboffset
                                                                           '(nil 0 0 1 0))))
    :hints(("Goal" :in-theory (enable arbmap-in-scope))))
  
  (local (in-theory (enable arbmap-fix))))

(define arbmap-in-for-scope ((x arbmap-p)
                             (base arbaddr-p)
                             (loop-iter natp)
                             (loop-idx natp))
  (if (atom x)
      t
    (and (or (not (mbt (and (consp (car x))
                            (arbaddr-p (caar x)))))
             (arbaddr-in-for-scope (caar x) base loop-iter loop-idx))
         (arbmap-in-for-scope (cdr x) base loop-iter loop-idx)))
  ///
  
  (defthm arbmap-in-for-scope-of-append
    (iff (arbmap-in-for-scope (append x y) base loop-iter loop-idx)
         (and (arbmap-in-for-scope x base loop-iter loop-idx)
              (arbmap-in-for-scope y base loop-iter loop-idx))))

  (defthm arbmap-in-for-scope-of-nil
    (arbmap-in-for-scope nil base loop-iter loop-idx))

  (defthm arbmap-in-for-scope-when-in-scope
    (implies (and (arbmap-in-scope arbmap (cons (arbaddr-component-for loop-idx loop-iter)
                                               arbaddr) offset scope))
             (arbmap-in-for-scope arbmap arbaddr loop-iter loop-idx))
    :hints(("Goal" :in-theory (enable arbmap-in-scope))))

  (defthm arbmap-in-for-scope-of-incr
    (implies (arbmap-in-for-scope arbmap arbaddr (+ 1 (nfix loop-iter)) loop-idx)
             (arbmap-in-for-scope arbmap arbaddr loop-iter loop-idx)))

  (defthm arbmap-in-scope-when-arbmap-in-for-scope
    (implies (arbmap-in-for-scope arbmap arbaddr iter (arbaddr-offset->foroffset arboffset))
             (arbmap-in-scope arbmap arbaddr arboffset (arbaddr-offset-sum arboffset
                                                                           '(nil 0 1 0 0))))
    :hints(("Goal" :in-theory (enable arbmap-in-scope))))

  (defthm arbaddr-in-scope-when-arbaddr-in-for-scope-gen
    (implies (and (arbaddr-in-for-scope addr arbaddr iter (arbaddr-offset->foroffset loopoffset))
                  (arbaddr-offset-lte arboffset loopoffset)
                  (arbaddr-offset-lte  (arbaddr-offset-sum loopoffset '(nil 0 1 0 0)) scope))
             (arbaddr-in-scope addr arbaddr arboffset scope))
  :hints(("Goal" :in-theory (enable arbaddr-in-scope
                                    arbaddr-in-for-scope
                                    arbaddr-offset-lte
                                    arbaddr-offset-lte-component
                                    arbaddr-offset-sum))))
  
  (defthm arbmap-in-scope-when-arbmap-in-for-scope-gen
    (implies (and (arbmap-in-for-scope arbmap arbaddr iter (arbaddr-offset->foroffset loopoffset))
                  (arbaddr-offset-lte arboffset loopoffset)
                  (arbaddr-offset-lte  (arbaddr-offset-sum loopoffset '(nil 0 1 0 0)) scope))
             (arbmap-in-scope arbmap arbaddr arboffset scope))
    :hints(("Goal" :in-theory (enable arbmap-in-scope))))

  (local (in-theory (enable arbmap-fix))))

(define arbmap-in-arbaddr-scope ((x arbmap-p)
                                 (base arbaddr-p))
  (if (atom x)
      t
    (and (or (not (mbt (and (consp (car x))
                            (arbaddr-p (caar x)))))
             (arbaddr-suffixp base (caar x)))
         (arbmap-in-arbaddr-scope (cdr x) base)))
  ///
  (defthm arbmap-in-arbaddr-scope-of-append
    (iff (arbmap-in-arbaddr-scope (append x y) base)
         (and (arbmap-in-arbaddr-scope x base)
              (arbmap-in-arbaddr-scope y base))))

  (defthm arbmap-in-arbaddr-scope-of-nil
    (arbmap-in-arbaddr-scope nil base))

  (defthm arbmap-in-arbaddr-scope-when-in-scope
    (implies (arbmap-in-scope arbmap arbaddr arboffset scope)
             (arbmap-in-arbaddr-scope arbmap arbaddr))
    :hints(("Goal" :in-theory (enable arbmap-in-scope))))

  (defthm arbmap-in-scope-when-call-suffixp
    (implies (and (arbmap-in-arbaddr-scope
                   arbmap (cons (arbaddr-component-call
                                 name
                                 (call-count-map-entry name
                                                       (arbaddr-offset->callmap calloffset)))
                                arbaddr))
                  (arbaddr-offset-lte arboffset calloffset)
                  (arbaddr-offset-lte (arbaddr-offset-sum
                                       (fnname-arbaddr-offset name)
                                       calloffset)
                                      scope))
             (arbmap-in-scope arbmap arbaddr arboffset scope))
    :hints(("Goal" :in-theory (enable arbmap-in-scope))))
  
  (local (in-theory (enable arbmap-fix))))

(defthm arbaddr-offset-lte-component-of-call
  (arbaddr-offset-lte-component arboffset
                                (arbaddr-component-call
                                 name
                                 (call-count-map-entry name (arbaddr-offset->callmap arboffset))))
  :hints(("Goal" :in-theory (enable arbaddr-offset-lte-component))))

(local (defthm call-count-map-entry-of-fnname-arbaddr-offset
         (equal (call-count-map-entry
                 name
                 (arbaddr-offset->callmap
                  (fnname-arbaddr-offset name)))
                1)
         :hints(("Goal" :in-theory (enable fnname-arbaddr-offset
                                           call-count-map-entry)))))

(defthm not-arbaddr-offset-lte-component-of-call
  (not (arbaddr-offset-lte-component (arbaddr-offset-sum arboffset (fnname-arbaddr-offset name))
                                     (arbaddr-component-call
                                      name
                                      (call-count-map-entry name (arbaddr-offset->callmap arboffset)))))
  :hints(("Goal" :in-theory (enable arbaddr-offset-lte-component
                                    arbaddr-offset-sum))))






(defmacro expand-hint-for-arboffset-*ota (form)
  (let ((offset-form (arboffset-for-form-*ota-fn form)))
    (if (eq (car form) 'arbaddr-offset-sum)
        nil
      `'(:expand (,offset-form)))))

(local (include-book "centaur/vl/util/default-hints" :dir :system))

(encapsulate nil
  (local (in-theory (acl2::disable* equal-of-ev_normal
                                    equal-of-ev_throwing
                                    equal-of-ev_error
                                    equal-of-continuing
                                    equal-of-returning
                                    (tau-system)
                                    append acl2::append-to-nil not
                                    (:rules-of-class :type-prescription :here))))
  (local (deflabel before-in-scope))
  (local
   (with-output
     :evisc (:gag-mode (evisc-tuple 3 4 nil nil))
     (std::defret-mutual-generate <fn>-in-scope-lemma
       :rules (((not (or (:fnname eval_subprogram-*ota)
                         (:fnname eval_loop-*ota)
                         (:fnname eval_for-*ota)))
                (:add-concl (arbmap-in-scope arbmap arbaddr arboffset
                                             (arbaddr-offset-sum arboffset (arboffset-for-form-*ota <call>)))))
               ((:fnname eval_subprogram-*ota)
                (:add-concl (arbmap-in-arbaddr-scope arbmap arbaddr)))
               ((:fnname eval_loop-*ota)
                (:add-concl (arbmap-in-loop-scope arbmap arbaddr
                                                  is_while
                                                  loop-iter loop-idx)))
               ((:fnname eval_for-*ota)
                (:add-concl (arbmap-in-for-scope arbmap arbaddr
                                                 loop-iter loop-idx)))
              
               ((:fnname is_val_of_type-*ota)
                (:add-keyword
                 :hints ((and stable-under-simplificationp
                              '(:expand ((ty-arbaddr-offset ty)
                                         (ty-arbaddr-offset-aux ty)
                                         (type_desc-arbaddr-offset (ty->desc ty))
                                         (CONSTRAINT_KIND-ARBADDR-OFFSET (T_INT->CONSTRAINT (TY->DESC TY)))))))))
               ((:fnname resolve-ty-*ota)
                (:add-keyword
                 :hints ((and stable-under-simplificationp
                              '(:expand ((ty-arbaddr-offset x)
                                         (ty-arbaddr-offset-aux x)
                                         (type_desc-arbaddr-offset (ty->desc x))
                                         ;; (CONSTRAINT_KIND-ARBADDR-OFFSET (T_INT->CONSTRAINT (TY->DESC TY)))
                                         ))))))
               ((:fnname check_int_constraints-*ota)
                (:add-keyword
                 :hints ((and stable-under-simplificationp
                              '(:expand ((int_constraintlist-arbaddr-offset constrs)
                                         (INT_CONSTRAINT-ARBADDR-OFFSET (CAR CONSTRS))))))))
               ((:fnname eval_stmt-*ota)
                (:add-keyword
                 :hints ((and stable-under-simplificationp
                              '(:expand ((stmt-arbaddr-offset s)
                                         (stmt-arbaddr-offset-aux s)
                                         (stmt_desc-arbaddr-offset (stmt->desc s)))))
                         (and stable-under-simplificationp
                              '(:expand ((expr-arbaddr-offset
                                          (s_return->expr (stmt->desc s)))
                                         (expr-arbaddr-offset-aux
                                          (s_return->expr (stmt->desc s)))
                                         (expr_desc-arbaddr-offset
                                          (expr->desc (s_return->expr (stmt->desc s))))
                                         (call-arbaddr-offset (s_call->call (stmt->desc s)))
                                         (call-arbaddr-offset-aux (s_call->call (stmt->desc s)))))))))
               ((:fnname eval_lexpr-*ota)
                (:add-keyword
                 :hints ((and stable-under-simplificationp
                              '(:expand ((lexpr-arbaddr-offset lx)
                                         (lexpr-arbaddr-offset-aux lx)
                                         (lexpr_desc-arbaddr-offset (lexpr->desc lx))))))))
               ((:fnname eval_pattern-*ota)
                (:add-keyword
                 :hints ((and stable-under-simplificationp
                              '(:expand ((pattern-arbaddr-offset p)
                                         (pattern_desc-arbaddr-offset (pattern->desc p))))))))
               ((:fnname resolve-typed_identifierlist-*ota)
                (:add-keyword
                 :hints ((and stable-under-simplificationp
                              '(:expand ((typed_identifierlist-arbaddr-offset x)
                                         (typed_identifier-arbaddr-offset (car x))))))))
               ((:fnname resolve-int_constraints-*ota)
                (:add-keyword
                 :hints ((and stable-under-simplificationp
                              '(:expand ((int_constraintlist-arbaddr-offset x)
                                         (int_constraint-arbaddr-offset (car x))))))))
               ((:fnname eval_expr-*ota)
                (:add-keyword
                 :hints ((and stable-under-simplificationp
                              '(:expand ((expr-arbaddr-offset e)
                                         (expr-arbaddr-offset-aux e)
                                         (expr_desc-arbaddr-offset (expr->desc e))
                                         (call-arbaddr-offset-aux
                                          (e_call->call (expr->desc e)))
                                         (call-arbaddr-offset
                                          (e_call->call (expr->desc e)))))))))
               ((not (or (:fnname is_val_of_type-*ota)
                         (:fnname check_int_constraints-*ota)
                         (:fnname eval_catchers-*ota)
                         (:fnname eval_stmt-*ota)
                         (:fnname eval_lexpr-*ota)
                         (:fnname eval_call-*ota)
                         (:fnname eval_pattern-*ota)
                         (:fnname resolve-ty-*ota)
                         (:fnname resolve-typed_identifierlist-*ota)
                         (:fnname resolve-int_constraints-*ota)
                         (:fnname eval_expr-*ota)))
                (:add-keyword
                 :hints ((and stable-under-simplificationp
                              (expand-hint-for-arboffset-*ota <call>))))))
       :hints ((vl::big-mutrec-default-hint 'eval_expr-*ota-fn id nil (w state)))
       :mutual-recursion asl-interpreter-mutual-recursion-*ota)))


  (with-output
    :evisc (:gag-mode (evisc-tuple 3 4 nil nil))
    (std::defret-mutual-generate <fn>-in-scope
      :rules (((not (or (:fnname eval_subprogram-*ota)
                        (:fnname eval_loop-*ota)
                        (:fnname eval_for-*ota)))
               (:add-hyp (equal scope (arbaddr-offset-sum arboffset (arboffset-for-form-*ota <call>))))
               (:add-concl (arbmap-in-scope arbmap arbaddr arboffset scope)))
              ((:fnname eval_subprogram-*ota)
               (:add-concl (arbmap-in-arbaddr-scope arbmap arbaddr)))
              ((:fnname eval_loop-*ota)
               (:add-concl (arbmap-in-loop-scope arbmap arbaddr
                                                 is_while
                                                 loop-iter loop-idx)))
              ((:fnname eval_for-*ota)
               (:add-concl (arbmap-in-for-scope arbmap arbaddr
                                                loop-iter loop-idx))))
      :no-induction-hint t
      :mutual-recursion asl-interpreter-mutual-recursion-*ota))

  (acl2::def-ruleset! asl-*ota-in-scope
    (set-difference-equal (current-theory :here)
                          (current-theory 'before-in-scope))))










(define arbmap-disjoint-from-scope ((x arbmap-p)
                                    (base arbaddr-p)
                                    (offset arbaddr-offset-p)
                                    (scope arbaddr-offset-p))
  (if (atom x)
      t
    (and (or (not (mbt (and (consp (car x))
                            (arbaddr-p (caar x)))))
             (not (arbaddr-in-scope (caar x) base offset scope)))
         (arbmap-disjoint-from-scope (cdr x) base offset scope)))
  ///
  (defthm arbmap-disjoint-from-scope-of-append
    (iff (arbmap-disjoint-from-scope (append x y) base offset scope)
         (and (arbmap-disjoint-from-scope x base offset scope)
              (arbmap-disjoint-from-scope y base offset scope))))

  (defthm arbmap-disjoint-from-scope-when-disjoint-from-wider-scope
    (implies (and (arbmap-disjoint-from-scope x base offset1 scope1)
                  (arbaddr-offset-lte offset1 offset)
                  (arbaddr-offset-lte scope scope1))
             (arbmap-disjoint-from-scope x base offset scope)))

  (defthm arbmap-disjoint-from-scope-of-nil
    (arbmap-disjoint-from-scope nil base offset scope))

  (fty::deffixequiv arbmap-disjoint-from-scope
    :hints(("Goal" :in-theory (enable arbmap-fix))))
  
  (defthm arbmap-disjoint-from-scope-of-cons
    (implies (arbaddr-p a)
             (equal (arbmap-disjoint-from-scope (cons (cons a v) arbmap) base offset scope)
                    (and (not (arbaddr-in-scope a base offset scope))
                         (arbmap-disjoint-from-scope arbmap base offset scope))))))



(defthmd arbvals-equiv-in-scope-transitive
  (implies (and (arbvals-equiv-in-scope x y base offset scope)
                (arbvals-equiv-in-scope y z base offset scope))
           (arbvals-equiv-in-scope x z base offset scope))
  :hints (("goal" :expand ((arbvals-equiv-in-scope x z base offset scope)))))

(defthm arbvals-equiv-in-scope-reflexive
  (arbvals-equiv-in-scope x x base offset scope)
  :hints (("goal" :expand ((arbvals-equiv-in-scope x x base offset scope)))))

(define arbmap-boundp ((key arbaddr-p) (x arbmap-p))
  (consp (hons-assoc-equal (arbaddr-fix key) (arbmap-fix x)))
  ///
  (local (defthm hons-assoc-equal-of-append
           (equal (hons-assoc-equal key (Append x y))
                  (or (hons-assoc-equal key x)
                      (hons-assoc-equal key y)))))

  (defthm arbmap-boundp-of-append
    (equal (arbmap-boundp key (append x y))
           (or (arbmap-boundp key x)
               (arbmap-boundp key y))))
  (defthm arbmap-lookup-of-append
    (equal (arbmap-lookup key ty (append x y))
           (if (arbmap-boundp key x)
               (arbmap-lookup key ty x)
             (arbmap-lookup key ty y)))
    :hints(("Goal" :in-theory (enable arbmap-lookup)))))

(defthm arbmap-boundp-when-disjoint-from-scope
  (implies (and (arbmap-disjoint-from-scope arbmap base offset scope)
                (arbaddr-in-scope key base offset scope))
           (not (Arbmap-boundp key arbmap)))
  :hints(("Goal" :in-theory (enable arbmap-boundp arbmap-fix arbmap-disjoint-from-scope))))

(defthmd arbmap-lookup-when-not-boundp
  (implies (not (arbmap-boundp key x))
           (equal (arbmap-lookup key ty x)
                  (and (ty-satisfiable ty)
                       (ty-fix-val nil ty))))
  :hints(("Goal" :in-theory (enable arbmap-boundp
                                    arbmap-lookup))))

(defthm arbvals-equiv-in-scope-of-append-disjoint-1
  (implies (and (arbmap-disjoint-from-scope y base offset scope)
                (arbvals-equiv-in-scope x z base offset scope))
           (arbvals-equiv-in-scope (append x y) z base offset scope))
  :hints(("Goal" :in-theory (e/d (arbvals-equiv-in-scope-transitive
                                  arbmap-lookup-when-not-boundp)
                                 (arbvals-equiv-in-scope-necc))
          :expand ((arbvals-equiv-in-scope (append x y) z base offset scope))
          :use ((:instance arbvals-equiv-in-scope-necc
                 (key (mv-nth 0 (arbvals-equiv-in-scope-witness (append x y) z base offset scope)))
                 (ty (mv-nth 1 (arbvals-equiv-in-scope-witness (append x y) z base offset scope)))
                 (arbmap1 x) (arbmap2 z) (arbaddr base) (arboffset offset)))
          :do-not-induct t)))

(defthm arbvals-equiv-in-scope-of-append-disjoint-2
  (implies (and (arbmap-disjoint-from-scope x base offset scope)
                (arbvals-equiv-in-scope y z base offset scope))
           (arbvals-equiv-in-scope (append x y) z base offset scope))
  :hints(("Goal" :in-theory (e/d (arbvals-equiv-in-scope-transitive
                                  arbmap-lookup-when-not-boundp)
                                 (arbvals-equiv-in-scope-necc))
          :expand ((arbvals-equiv-in-scope (append x y) z base offset scope))
          :use ((:instance arbvals-equiv-in-scope-necc
                 (key (mv-nth 0 (arbvals-equiv-in-scope-witness (append x y) z base offset scope)))
                 (ty (mv-nth 1 (arbvals-equiv-in-scope-witness (append x y) z base offset scope)))
                 (arbmap1 y) (arbmap2 z) (arbaddr base) (arboffset offset)))
          :do-not-induct t)))

(defthm arbvals-equiv-in-scope-of-cons-non-scope
  (implies (and (not (arbaddr-in-scope x base offset scope))
                (arbvals-equiv-in-scope y z base offset scope))
           (arbvals-equiv-in-scope (cons (cons x val) y) z base offset scope))
  :hints(("Goal" :expand ((arbvals-equiv-in-scope (cons (cons x val) y) z base offset scope))
          :in-theory (e/d (arbmap-lookup)
                          (arbvals-equiv-in-scope-necc))
          :use ((:instance arbvals-equiv-in-scope-necc
                 (key (mv-nth 0 (arbvals-equiv-in-scope-witness (cons (cons x val) y) z base offset scope)))
                 (ty (mv-nth 1 (arbvals-equiv-in-scope-witness (cons (cons x val) y) z base offset scope)))
                 (arbmap1 y) (arbmap2 z) (arbaddr base) (arboffset offset))))))


(defthm arbaddr-not-in-scope-when-in-lesser-scope
  (implies (and (arbaddr-in-scope addr base offset1 scope1)
                (arbaddr-offset-lte scope1 offset))
           (not (arbaddr-in-scope addr base offset scope)))
  :hints(("Goal" :in-theory (enable arbaddr-in-scope))))

(defthm arbaddr-not-in-scope-when-in-greater-scope
  (implies (and (arbaddr-in-scope addr base offset1 scope1)
                (arbaddr-offset-lte scope offset1))
           (not (arbaddr-in-scope addr base offset scope)))
  :hints(("Goal" :in-theory (enable arbaddr-in-scope))))

(defun ota-form-scope-bindings (call mfc state)
  (declare (ignorable mfc)
           (xargs :stobjs state
                  :mode :program))
  (b* ((wrld (w state))
       ((unless (consp call)) nil)
       (fn (car call))
       (formals (acl2::formals fn wrld))
       (args (pairlis$ formals (cdr call)))
       (arbaddr (cdr (assoc 'arbaddr args)))
       (arboffset (cdr (assoc 'arboffset args)))
       (add-scope (arboffset-for-form-*ota-fn call)))
    (and arbaddr arboffset add-scope
         `((arbaddr . ,arbaddr)
           (arboffset . ,arboffset)
           (scope . (arbaddr-offset-sum ,arboffset ,add-scope))))))


(defthm arbmap-disjoint-from-scope-when-in-lesser-scope
  (implies (and (arbmap-in-scope arbmap addr offset1 scope1)
                (arbaddr-offset-lte scope1 offset))
           (arbmap-disjoint-from-scope arbmap addr offset scope))
  :hints(("Goal" :in-theory (enable arbmap-in-scope
                                    arbmap-disjoint-from-scope))))

(defthm arbmap-disjoint-from-scope-when-in-greater-scope
  (implies (and (arbmap-in-scope arbmap addr offset1 scope1)
                (arbaddr-offset-lte scope offset1))
           (arbmap-disjoint-from-scope arbmap addr offset scope))
  :hints(("Goal" :in-theory (enable arbmap-in-scope
                                    arbmap-disjoint-from-scope))))

(defthm arbmap-disjoint-from-scope-when-in-disjoint-scope-special
  (implies (and (bind-free (ota-form-scope-bindings ota-call mfc state)
                           (arbaddr arboffset scope))
                (arbmap-in-scope (mv-nth 2 ota-call) arbaddr arboffset scope)
                (equal sc-addr arbaddr)
                (or (arbaddr-offset-lte scope sc-offset)
                    (arbaddr-offset-lte sc-scope arboffset)))
           (arbmap-disjoint-from-scope
            (mv-nth 2 ota-call)
            sc-addr sc-offset sc-scope)))


(defthm arbaddr-not-in-scope-when-in-greater-subscope
  (implies (and (arbaddr-in-scope addr (cons component arbaddr) offset1 scope1)
                (arbaddr-offset-lte-component scope component))
           (not (arbaddr-in-scope addr arbaddr offset scope)))
  :hints(("Goal" :in-theory (enable arbaddr-in-scope))))

(defthm arbmap-disjoint-from-scope-when-in-greater-component
  (implies (and (arbmap-in-scope arbmap (cons component arbaddr) offset1 scope1)
                (arbaddr-offset-lte-component scope component))
           (arbmap-disjoint-from-scope arbmap arbaddr offset scope))
  :hints(("Goal" :in-theory (enable arbmap-in-scope
                                    arbmap-disjoint-from-scope))))

(defthm arbaddr-not-in-scope-when-in-lesser-subscope
  (implies (and (arbaddr-in-scope addr (cons component arbaddr) offset1 scope1)
                (not (arbaddr-offset-lte-component offset component)))
           (not (arbaddr-in-scope addr arbaddr offset scope)))
  :hints(("Goal" :in-theory (enable arbaddr-in-scope))))

(defthm arbmap-disjoint-from-scope-when-in-lesser-component
  (implies (and (arbmap-in-scope arbmap (cons component arbaddr) offset1 scope1)
                (not (arbaddr-offset-lte-component offset component)))
           (arbmap-disjoint-from-scope arbmap arbaddr offset scope))
  :hints(("Goal" :in-theory (enable arbmap-in-scope
                                    arbmap-disjoint-from-scope))))


(defthm arbmap-disjoint-from-scope-when-in-disjoint-subscope-special
  (implies (and (bind-free (ota-form-scope-bindings ota-call mfc state)
                           (arbaddr arboffset scope))
                (arbmap-in-scope (mv-nth 2 ota-call) arbaddr arboffset scope)
                (consp arbaddr)
                (equal sc-addr (cdr arbaddr))
                (or (arbaddr-offset-lte-component sc-scope (car arbaddr))
                    (not (arbaddr-offset-lte-component sc-offset (car arbaddr)))))
           (arbmap-disjoint-from-scope
            (mv-nth 2 ota-call)
            sc-addr sc-offset sc-scope)))


(define arbmap-disjoint-from-loop-scope ((x arbmap-p)
                                         (base arbaddr-p)
                                         (is_while)
                                         (loop-iter natp)
                                         (loop-idx natp))
  (if (atom x)
      t
    (and (or (not (mbt (and (consp (car x))
                            (arbaddr-p (caar x)))))
             (not (arbaddr-in-loop-scope (caar x) base is_while loop-iter loop-idx)))
         (arbmap-disjoint-from-loop-scope (cdr x) base is_while loop-iter loop-idx)))
  ///
  
  (defthm arbmap-disjoint-from-loop-scope-of-append
    (iff (arbmap-disjoint-from-loop-scope (append x y) base is_while loop-iter loop-idx)
         (and (arbmap-disjoint-from-loop-scope x base is_while loop-iter loop-idx)
              (arbmap-disjoint-from-loop-scope y base is_while loop-iter loop-idx))))

  (defthm arbmap-disjoint-from-loop-scope-of-nil
    (arbmap-disjoint-from-loop-scope nil base is_while loop-iter loop-idx))

  (fty::deffixequiv arbmap-disjoint-from-loop-scope
    :hints(("Goal" :in-theory (enable arbmap-fix)))))

(define arbmap-disjoint-from-for-scope ((x arbmap-p)
                             (base arbaddr-p)
                             (loop-iter natp)
                             (loop-idx natp))
  (if (atom x)
      t
    (and (or (not (mbt (and (consp (car x))
                            (arbaddr-p (caar x)))))
             (not (arbaddr-in-for-scope (caar x) base loop-iter loop-idx)))
         (arbmap-disjoint-from-for-scope (cdr x) base loop-iter loop-idx)))
  ///
  
  (defthm arbmap-disjoint-from-for-scope-of-append
    (iff (arbmap-disjoint-from-for-scope (append x y) base loop-iter loop-idx)
         (and (arbmap-disjoint-from-for-scope x base loop-iter loop-idx)
              (arbmap-disjoint-from-for-scope y base loop-iter loop-idx))))

  (defthm arbmap-disjoint-from-for-scope-of-nil
    (arbmap-disjoint-from-for-scope nil base loop-iter loop-idx))

  (fty::deffixequiv arbmap-disjoint-from-for-scope
    :hints(("Goal" :in-theory (enable arbmap-fix)))))

(define arbmap-disjoint-from-arbaddr-scope ((x arbmap-p)
                                 (base arbaddr-p))
  (if (atom x)
      t
    (and (or (not (mbt (and (consp (car x))
                            (arbaddr-p (caar x)))))
             (not (arbaddr-suffixp base (caar x))))
         (arbmap-disjoint-from-arbaddr-scope (cdr x) base)))
  ///
  (defthm arbmap-disjoint-from-arbaddr-scope-of-append
    (iff (arbmap-disjoint-from-arbaddr-scope (append x y) base)
         (and (arbmap-disjoint-from-arbaddr-scope x base)
              (arbmap-disjoint-from-arbaddr-scope y base))))

  (defthm arbmap-disjoint-from-arbaddr-scope-of-nil
    (arbmap-disjoint-from-arbaddr-scope nil base))

  (fty::deffixequiv arbmap-disjoint-from-arbaddr-scope
    :hints(("Goal" :in-theory (enable arbmap-fix)))))


(defthmd arbvals-equiv-in-loop-scope-transitive
  (implies (and (arbvals-equiv-in-loop-scope x y base is_while loop-iter loop-idx)
                (arbvals-equiv-in-loop-scope y z base is_while loop-iter loop-idx))
           (arbvals-equiv-in-loop-scope x z base is_while loop-iter loop-idx))
  :hints (("goal" :expand ((arbvals-equiv-in-loop-scope x z base is_while loop-iter loop-idx)))))

(defthm arbvals-equiv-in-loop-scope-reflexive
  (arbvals-equiv-in-loop-scope x x base is_while loop-iter loop-idx)
  :hints (("goal" :expand ((arbvals-equiv-in-loop-scope x x base is_while loop-iter loop-idx)))))

(defthm arbmap-boundp-when-disjoint-from-loop-scope
  (implies (and (arbmap-disjoint-from-loop-scope arbmap base is_while loop-iter loop-idx)
                (arbaddr-in-loop-scope key base is_while loop-iter loop-idx))
           (not (Arbmap-boundp key arbmap)))
  :hints(("Goal" :in-theory (enable arbmap-boundp arbmap-fix arbmap-disjoint-from-loop-scope))))


(defthm arbvals-equiv-in-loop-scope-of-append-disjoint-1
  (implies (and (arbmap-disjoint-from-loop-scope y base is_while loop-iter loop-idx)
                (arbvals-equiv-in-loop-scope x z base is_while loop-iter loop-idx))
           (arbvals-equiv-in-loop-scope (append x y) z base is_while loop-iter loop-idx))
  :hints(("Goal" :in-theory (e/d (arbvals-equiv-in-loop-scope-transitive
                                  arbmap-lookup-when-not-boundp)
                                 (arbvals-equiv-in-loop-scope-necc))
          :expand ((arbvals-equiv-in-loop-scope (append x y) z base is_while loop-iter loop-idx))
          :use ((:instance arbvals-equiv-in-loop-scope-necc
                 (key (mv-nth 0 (arbvals-equiv-in-loop-scope-witness (append x y) z base is_while loop-iter loop-idx)))
                 (ty (mv-nth 1 (arbvals-equiv-in-loop-scope-witness (append x y) z base is_while loop-iter loop-idx)))
                 (arbmap1 x) (arbmap2 z) (arbaddr base)))
          :do-not-induct t)))

(defthm arbvals-equiv-in-loop-scope-of-append-disjoint-2
  (implies (and (arbmap-disjoint-from-loop-scope x base is_while loop-iter loop-idx)
                (arbvals-equiv-in-loop-scope y z base is_while loop-iter loop-idx))
           (arbvals-equiv-in-loop-scope (append x y) z base is_while loop-iter loop-idx))
  :hints(("Goal" :in-theory (e/d (arbvals-equiv-in-loop-scope-transitive
                                  arbmap-lookup-when-not-boundp)
                                 (arbvals-equiv-in-loop-scope-necc))
          :expand ((arbvals-equiv-in-loop-scope (append x y) z base is_while loop-iter loop-idx))
          :use ((:instance arbvals-equiv-in-loop-scope-necc
                 (key (mv-nth 0 (arbvals-equiv-in-loop-scope-witness (append x y) z base is_while loop-iter loop-idx)))
                 (ty (mv-nth 1 (arbvals-equiv-in-loop-scope-witness (append x y) z base is_while loop-iter loop-idx)))
                 (arbmap1 y) (arbmap2 z) (arbaddr base)))
          :do-not-induct t)))





(defthmd arbvals-equiv-in-for-scope-transitive
  (implies (and (arbvals-equiv-in-for-scope x y base loop-iter loop-idx)
                (arbvals-equiv-in-for-scope y z base loop-iter loop-idx))
           (arbvals-equiv-in-for-scope x z base loop-iter loop-idx))
  :hints (("goal" :expand ((arbvals-equiv-in-for-scope x z base loop-iter loop-idx)))))

(defthm arbvals-equiv-in-for-scope-reflexive
  (arbvals-equiv-in-for-scope x x base loop-iter loop-idx)
  :hints (("goal" :expand ((arbvals-equiv-in-for-scope x x base loop-iter loop-idx)))))

(defthm arbmap-boundp-when-disjoint-from-for-scope
  (implies (and (arbmap-disjoint-from-for-scope arbmap base loop-iter loop-idx)
                (arbaddr-in-for-scope key base loop-iter loop-idx))
           (not (Arbmap-boundp key arbmap)))
  :hints(("Goal" :in-theory (enable arbmap-boundp arbmap-fix arbmap-disjoint-from-for-scope))))


(defthm arbvals-equiv-in-for-scope-of-append-disjoint-1
  (implies (and (arbmap-disjoint-from-for-scope y base loop-iter loop-idx)
                (arbvals-equiv-in-for-scope x z base loop-iter loop-idx))
           (arbvals-equiv-in-for-scope (append x y) z base loop-iter loop-idx))
  :hints(("Goal" :in-theory (e/d (arbvals-equiv-in-for-scope-transitive
                                  arbmap-lookup-when-not-boundp)
                                 (arbvals-equiv-in-for-scope-necc))
          :expand ((arbvals-equiv-in-for-scope (append x y) z base loop-iter loop-idx))
          :use ((:instance arbvals-equiv-in-for-scope-necc
                 (key (mv-nth 0 (arbvals-equiv-in-for-scope-witness (append x y) z base loop-iter loop-idx)))
                 (ty (mv-nth 1 (arbvals-equiv-in-for-scope-witness (append x y) z base loop-iter loop-idx)))
                 (arbmap1 x) (arbmap2 z) (arbaddr base)))
          :do-not-induct t)))

(defthm arbvals-equiv-in-for-scope-of-append-disjoint-2
  (implies (and (arbmap-disjoint-from-for-scope x base loop-iter loop-idx)
                (arbvals-equiv-in-for-scope y z base loop-iter loop-idx))
           (arbvals-equiv-in-for-scope (append x y) z base loop-iter loop-idx))
  :hints(("Goal" :in-theory (e/d (arbvals-equiv-in-for-scope-transitive
                                  arbmap-lookup-when-not-boundp)
                                 (arbvals-equiv-in-for-scope-necc))
          :expand ((arbvals-equiv-in-for-scope (append x y) z base loop-iter loop-idx))
          :use ((:instance arbvals-equiv-in-for-scope-necc
                 (key (mv-nth 0 (arbvals-equiv-in-for-scope-witness (append x y) z base loop-iter loop-idx)))
                 (ty (mv-nth 1 (arbvals-equiv-in-for-scope-witness (append x y) z base loop-iter loop-idx)))
                 (arbmap1 y) (arbmap2 z) (arbaddr base)))
          :do-not-induct t)))





(defthmd arbvals-equiv-in-arbaddr-scope-transitive
  (implies (and (arbvals-equiv-in-arbaddr-scope x y base)
                (arbvals-equiv-in-arbaddr-scope y z base))
           (arbvals-equiv-in-arbaddr-scope x z base))
  :hints (("goal" :expand ((arbvals-equiv-in-arbaddr-scope x z base)))))

(defthm arbvals-equiv-in-arbaddr-scope-reflexive
  (arbvals-equiv-in-arbaddr-scope x x base)
  :hints (("goal" :expand ((arbvals-equiv-in-arbaddr-scope x x base)))))

(defthm arbmap-boundp-when-disjoint-from-arbaddr-scope
  (implies (and (arbmap-disjoint-from-arbaddr-scope arbmap base)
                (arbaddr-suffixp base key))
           (not (Arbmap-boundp key arbmap)))
  :hints(("Goal" :in-theory (enable arbmap-boundp arbmap-fix arbmap-disjoint-from-arbaddr-scope))))


(defthm arbvals-equiv-in-arbaddr-scope-of-append-disjoint-1
  (implies (and (arbmap-disjoint-from-arbaddr-scope y base)
                (arbvals-equiv-in-arbaddr-scope x z base))
           (arbvals-equiv-in-arbaddr-scope (append x y) z base))
  :hints(("Goal" :in-theory (e/d (arbvals-equiv-in-arbaddr-scope-transitive
                                  arbmap-lookup-when-not-boundp)
                                 (arbvals-equiv-in-arbaddr-scope-necc))
          :expand ((arbvals-equiv-in-arbaddr-scope (append x y) z base))
          :use ((:instance arbvals-equiv-in-arbaddr-scope-necc
                 (key (mv-nth 0 (arbvals-equiv-in-arbaddr-scope-witness (append x y) z base)))
                 (ty (mv-nth 1 (arbvals-equiv-in-arbaddr-scope-witness (append x y) z base)))
                 (arbmap1 x) (arbmap2 z) (arbaddr base)))
          :do-not-induct t)))

(defthm arbvals-equiv-in-arbaddr-scope-of-append-disjoint-2
  (implies (and (arbmap-disjoint-from-arbaddr-scope x base)
                (arbvals-equiv-in-arbaddr-scope y z base))
           (arbvals-equiv-in-arbaddr-scope (append x y) z base))
  :hints(("Goal" :in-theory (e/d (arbvals-equiv-in-arbaddr-scope-transitive
                                  arbmap-lookup-when-not-boundp)
                                 (arbvals-equiv-in-arbaddr-scope-necc))
          :expand ((arbvals-equiv-in-arbaddr-scope (append x y) z base))
          :use ((:instance arbvals-equiv-in-arbaddr-scope-necc
                 (key (mv-nth 0 (arbvals-equiv-in-arbaddr-scope-witness (append x y) z base)))
                 (ty (mv-nth 1 (arbvals-equiv-in-arbaddr-scope-witness (append x y) z base)))
                 (arbmap1 y) (arbmap2 z) (arbaddr base)))
          :do-not-induct t)))





;; (defthm arbmap-disjoint-from-scope-when-in-lesser-scope
;;   (implies (and (arbmap-in-scope arbmap addr offset1 scope1)
;;                 (arbaddr-offset-lte scope1 offset))
;;            (arbmap-disjoint-from-scope arbmap addr offset scope))
;;   :hints(("Goal" :in-theory (enable arbmap-in-scope
;;                                     arbmap-disjoint-from-scope))))

;; (defthm arbmap-disjoint-from-scope-when-in-greater-scope
;;   (implies (and (arbmap-in-scope arbmap addr offset1 scope1)
;;                 (arbaddr-offset-lte scope offset1))
;;            (arbmap-disjoint-from-scope arbmap addr offset scope))
;;   :hints(("Goal" :in-theory (enable arbmap-in-scope
;;                                     arbmap-disjoint-from-scope))))

(defthm arbaddr-not-in-scope-of-while-when-in-loop-scope
  (implies (arbaddr-in-loop-scope addr arbaddr t (+ 1 (nfix loop-iter)) loop-idx)
           (not (arbaddr-in-scope addr (cons (arbaddr-component-while loop-idx loop-iter) arbaddr) offset scope)))
  :hints(("Goal" :in-theory (enable arbaddr-in-scope arbaddr-in-loop-scope))))



(defthm arbmap-disjoint-from-scope-of-while-when-in-disjoint-scope
  (implies (and ;; (bind-free (ota-form-loop-scope-bindings ota-call mfc state)
                ;;            (arbaddr arboffset scope))
            (arbmap-in-loop-scope arbmap sc-addr t (+ 1 (nfix loop-iter)) loop-idx))
           (arbmap-disjoint-from-scope
            arbmap
            (cons (arbaddr-component-while loop-idx loop-iter) sc-addr)
            sc-offset sc-scope))
  :hints(("Goal" :in-theory (enable arbmap-in-loop-scope
                                    arbmap-disjoint-from-scope))))

(defthm arbaddr-not-in-scope-of-while-when-in-loop-scope-for-greater-index
  (implies (And (arbaddr-in-loop-scope addr arbaddr t loop-iter (arbaddr-offset->whileoffset offset1))
                (arbaddr-offset-lte scope offset1))
           (not (arbaddr-in-scope addr arbaddr offset scope)))
  :hints(("Goal" :in-theory (enable arbaddr-in-scope arbaddr-in-loop-scope
                                    arbaddr-offset-lte
                                    arbaddr-offset-lte-component))))

(defthm arbmap-disjoint-from-scope-of-while-when-in-disjoint-scope-for-greater-index
  (implies (and ;; (bind-free (ota-form-loop-scope-bindings ota-call mfc state)
                ;;            (arbaddr arboffset scope))
            (arbmap-in-loop-scope arbmap sc-addr t loop-iter (arbaddr-offset->whileoffset offset1))
            (arbaddr-offset-lte sc-scope offset1))
           (arbmap-disjoint-from-scope
            arbmap sc-addr
            sc-offset sc-scope))
  :hints(("Goal" :in-theory (enable arbmap-in-loop-scope
                                    arbmap-disjoint-from-scope))))




(defthm arbaddr-not-in-scope-of-repeat-when-in-loop-scope
  (implies (arbaddr-in-loop-scope addr arbaddr nil (+ 1 (nfix loop-iter)) loop-idx)
           (not (arbaddr-in-scope addr (cons (arbaddr-component-repeat loop-idx loop-iter) arbaddr) offset scope)))
  :hints(("Goal" :in-theory (enable arbaddr-in-scope arbaddr-in-loop-scope))))

(defthm arbmap-disjoint-from-scope-of-repeat-when-in-disjoint-scope
  (implies (and ;; (bind-free (ota-form-loop-scope-bindings ota-call mfc state)
                ;;            (arbaddr arboffset scope))
            (arbmap-in-loop-scope arbmap sc-addr nil (+ 1 (nfix loop-iter)) loop-idx))
           (arbmap-disjoint-from-scope
            arbmap
            (cons (arbaddr-component-repeat loop-idx loop-iter) sc-addr)
            sc-offset sc-scope))
  :hints(("Goal" :in-theory (enable arbmap-in-loop-scope
                                    arbmap-disjoint-from-scope))))

(defthm arbaddr-not-in-scope-of-repeat-when-in-loop-scope-for-greater-index
  (implies (And (arbaddr-in-loop-scope addr arbaddr nil loop-iter (arbaddr-offset->repeatoffset offset1))
                (arbaddr-offset-lte scope offset1))
           (not (arbaddr-in-scope addr arbaddr offset scope)))
  :hints(("Goal" :in-theory (enable arbaddr-in-scope arbaddr-in-loop-scope
                                    arbaddr-offset-lte
                                    arbaddr-offset-lte-component))))

(defthm arbmap-disjoint-from-scope-of-repeat-when-in-disjoint-scope-for-greater-index
  (implies (and ;; (bind-free (ota-form-loop-scope-bindings ota-call mfc state)
                ;;            (arbaddr arboffset scope))
            (arbmap-in-loop-scope arbmap sc-addr nil loop-iter (arbaddr-offset->repeatoffset offset1))
            (arbaddr-offset-lte sc-scope offset1))
           (arbmap-disjoint-from-scope
            arbmap sc-addr
            sc-offset sc-scope))
  :hints(("Goal" :in-theory (enable arbmap-in-loop-scope
                                    arbmap-disjoint-from-scope))))

(defthm arbaddr-not-in-scope-of-for-when-in-loop-scope-for-greater-index
  (implies (And (arbaddr-in-for-scope addr arbaddr loop-iter (arbaddr-offset->foroffset offset1))
                (arbaddr-offset-lte scope offset1))
           (not (arbaddr-in-scope addr arbaddr offset scope)))
  :hints(("Goal" :in-theory (enable arbaddr-in-scope arbaddr-in-for-scope
                                    arbaddr-offset-lte
                                    arbaddr-offset-lte-component))))

(defthm arbmap-disjoint-from-scope-of-for-when-in-disjoint-scope-for-greater-index
  (implies (and ;; (bind-free (ota-form-for-scope-bindings ota-call mfc state)
                ;;            (arbaddr arboffset scope))
            (arbmap-in-for-scope arbmap sc-addr loop-iter (arbaddr-offset->foroffset offset1))
            (arbaddr-offset-lte sc-scope offset1))
           (arbmap-disjoint-from-scope
            arbmap sc-addr
            sc-offset sc-scope))
  :hints(("Goal" :in-theory (enable arbmap-in-for-scope
                                    arbmap-disjoint-from-scope))))


(defthm arbaddr-not-in-scope-of-for-when-in-for-scope
  (implies (arbaddr-in-for-scope addr arbaddr (+ 1 (nfix loop-iter)) loop-idx)
           (not (arbaddr-in-scope addr (cons (arbaddr-component-for loop-idx loop-iter) arbaddr) offset scope)))
  :hints(("Goal" :in-theory (enable arbaddr-in-scope arbaddr-in-for-scope))))

(defthm arbmap-disjoint-from-scope-of-for-when-in-disjoint-scope
  (implies (and ;; (bind-free (ota-form-for-scope-bindings ota-call mfc state)
                ;;            (arbaddr arboffset scope))
            (arbmap-in-for-scope arbmap sc-addr (+ 1 (nfix loop-iter)) loop-idx))
           (arbmap-disjoint-from-scope
            arbmap
            (cons (arbaddr-component-for loop-idx loop-iter) sc-addr)
            sc-offset sc-scope))
  :hints(("Goal" :in-theory (enable arbmap-in-for-scope
                                    arbmap-disjoint-from-scope))))


(defthm arbmap-not-in-while-loop-scope-when-in-disjoint-scope
  (implies (and (arbaddr-in-scope addr arbaddr arboffset scope)
                (consp arbaddr)
                (equal (cdr arbaddr) sc-addr)
                (arbaddr-component-case (car arbaddr) :while)
                (< (arbaddr-component-while->iter (car arbaddr)) (nfix loop-iter)))
           (not (arbaddr-in-loop-scope
                 addr
                 sc-addr t loop-iter loop-idx)))
  :hints(("Goal" :in-theory (enable arbaddr-in-loop-scope
                                    arbaddr-in-scope))))

(defthm arbmap-disjoint-from-while-loop-scope-when-in-disjoint-scope
  (implies (and (arbmap-in-scope arbmap arbaddr arboffset scope)
                (consp arbaddr)
                (equal (cdr arbaddr) sc-addr)
                (arbaddr-component-case (car arbaddr) :while)
                (< (arbaddr-component-while->iter (car arbaddr)) (nfix loop-iter)))
           (arbmap-disjoint-from-loop-scope
            arbmap
            sc-addr t loop-iter loop-idx))
  :hints(("Goal" :in-theory (enable arbmap-in-scope
                                    arbmap-disjoint-from-loop-scope))))


(defthm arbmap-disjoint-from-while-loop-scope-when-in-disjoint-scope-special
  (implies (and (bind-free (ota-form-scope-bindings ota-call mfc state)
                           (arbaddr arboffset scope))
                (arbmap-in-scope (mv-nth 2 ota-call) arbaddr arboffset scope)
                (consp arbaddr)
                (equal (cdr arbaddr) sc-addr)
                (arbaddr-component-case (car arbaddr) :while)
                (< (arbaddr-component-while->iter (car arbaddr)) (nfix loop-iter)))
           (arbmap-disjoint-from-loop-scope
            (mv-nth 2 ota-call)
            sc-addr t loop-iter loop-idx)))


(defthm arbmap-not-in-repeat-loop-scope-when-in-disjoint-scope
  (implies (and (arbaddr-in-scope addr arbaddr arboffset scope)
                (consp arbaddr)
                (equal (cdr arbaddr) sc-addr)
                (arbaddr-component-case (car arbaddr) :repeat)
                (< (arbaddr-component-repeat->iter (car arbaddr)) (nfix loop-iter)))
           (not (arbaddr-in-loop-scope
                 addr
                 sc-addr nil loop-iter loop-idx)))
  :hints(("Goal" :in-theory (enable arbaddr-in-loop-scope
                                    arbaddr-in-scope))))

(defthm arbmap-disjoint-from-repeat-loop-scope-when-in-disjoint-scope
  (implies (and (arbmap-in-scope arbmap arbaddr arboffset scope)
                (consp arbaddr)
                (equal (cdr arbaddr) sc-addr)
                (arbaddr-component-case (car arbaddr) :repeat)
                (< (arbaddr-component-repeat->iter (car arbaddr)) (nfix loop-iter)))
           (arbmap-disjoint-from-loop-scope
            arbmap
            sc-addr nil loop-iter loop-idx))
  :hints(("Goal" :in-theory (enable arbmap-in-scope
                                    arbmap-disjoint-from-loop-scope))))


(defthm arbmap-disjoint-from-repeat-loop-scope-when-in-disjoint-scope-special
  (implies (and (bind-free (ota-form-scope-bindings ota-call mfc state)
                           (arbaddr arboffset scope))
                (arbmap-in-scope (mv-nth 2 ota-call) arbaddr arboffset scope)
                (consp arbaddr)
                (equal (cdr arbaddr) sc-addr)
                (arbaddr-component-case (car arbaddr) :repeat)
                (< (arbaddr-component-repeat->iter (car arbaddr)) (nfix loop-iter)))
           (arbmap-disjoint-from-loop-scope
            (mv-nth 2 ota-call)
            sc-addr nil loop-iter loop-idx)))


(defthm arbmap-not-in-for-loop-scope-when-in-disjoint-scope
  (implies (and (arbaddr-in-scope addr arbaddr arboffset scope)
                (consp arbaddr)
                (equal (cdr arbaddr) sc-addr)
                (arbaddr-component-case (car arbaddr) :for)
                (< (arbaddr-component-for->iter (car arbaddr)) (nfix loop-iter)))
           (not (arbaddr-in-for-scope
                 addr
                 sc-addr loop-iter loop-idx)))
  :hints(("Goal" :in-theory (enable arbaddr-in-for-scope
                                    arbaddr-in-scope))))

(defthm arbmap-disjoint-from-for-loop-scope-when-in-disjoint-scope
  (implies (and (arbmap-in-scope arbmap arbaddr arboffset scope)
                (consp arbaddr)
                (equal (cdr arbaddr) sc-addr)
                (arbaddr-component-case (car arbaddr) :for)
                (< (arbaddr-component-for->iter (car arbaddr)) (nfix loop-iter)))
           (arbmap-disjoint-from-for-scope
            arbmap
            sc-addr loop-iter loop-idx))
  :hints(("Goal" :in-theory (enable arbmap-in-scope
                                    arbmap-disjoint-from-for-scope))))


(defthm arbmap-disjoint-from-for-loop-scope-when-in-disjoint-scope-special
  (implies (and (bind-free (ota-form-scope-bindings ota-call mfc state)
                           (arbaddr arboffset scope))
                (arbmap-in-scope (mv-nth 2 ota-call) arbaddr arboffset scope)
                (consp arbaddr)
                (equal (cdr arbaddr) sc-addr)
                (arbaddr-component-case (car arbaddr) :for)
                (< (arbaddr-component-for->iter (car arbaddr)) (nfix loop-iter)))
           (arbmap-disjoint-from-for-scope
            (mv-nth 2 ota-call)
            sc-addr loop-iter loop-idx)))


(defun ota-form-loop-scope-bindings (call mfc state)
  (declare (ignorable mfc)
           (xargs :stobjs state
                  :mode :program))
  (b* ((wrld (w state))
       ((unless (consp call)) nil)
       (fn (car call))
       (formals (acl2::formals fn wrld))
       (args (pairlis$ formals (cdr call)))
       (loop-idx (cdr (assoc 'loop-idx args)))
       (loop-iter (cdr (assoc 'loop-iter args)))
       (is_while (cdr (assoc 'is_while args)))
       (loop-idx-offset (and (consp loop-idx)
                             (or (eq (car loop-idx) 'ARBADDR-OFFSET->REPEATOFFSET$INLINE)
                                 (eq (car loop-idx) 'ARBADDR-OFFSET->whileOFFSET$INLINE))
                             (cadr loop-idx))))
    (and loop-idx loop-iter is_while loop-idx-offset
         `((loop-idx . ,loop-idx)
           (loop-iter . ,loop-iter)
           (loop-idx-offset . ,loop-idx-offset)
           (is_while . ,is_while)))))



(defthm arbmap-disjoint-from-scope-of-repeat-when-in-disjoint-scope-for-greater-index-special
  (implies (and (bind-free (ota-form-loop-scope-bindings ota-call mfc state)
                           (loop-iter loop-idx is_while loop-idx-offset))
                (not is_while)
                (equal loop-idx (arbaddr-offset->repeatoffset loop-idx-offset))
                (arbmap-in-loop-scope (mv-nth 2 ota-call) sc-addr nil loop-iter loop-idx)
                (arbaddr-offset-lte sc-scope loop-idx-offset))
           (arbmap-disjoint-from-scope
            (mv-nth 2 ota-call)
            sc-addr
            sc-offset sc-scope)))

(defthm arbmap-disjoint-from-scope-of-while-when-in-disjoint-scope-for-greater-index-special
  (implies (and (bind-free (ota-form-loop-scope-bindings ota-call mfc state)
                           (loop-iter loop-idx is_while loop-idx-offset))
                is_while
                (equal loop-idx (arbaddr-offset->whileoffset loop-idx-offset))
                (arbmap-in-loop-scope (mv-nth 2 ota-call) sc-addr t loop-iter loop-idx)
                (arbaddr-offset-lte sc-scope loop-idx-offset))
           (arbmap-disjoint-from-scope
            (mv-nth 2 ota-call)
            sc-addr
            sc-offset sc-scope)))

(defun ota-form-for-scope-bindings (call mfc state)
  (declare (ignorable mfc)
           (xargs :stobjs state
                  :mode :program))
  (b* ((wrld (w state))
       ((unless (consp call)) nil)
       (fn (car call))
       (formals (acl2::formals fn wrld))
       (args (pairlis$ formals (cdr call)))
       (loop-idx (cdr (assoc 'loop-idx args)))
       (loop-iter (cdr (assoc 'loop-iter args)))
       (loop-idx-offset (and (consp loop-idx)
                             (eq (car loop-idx) 'ARBADDR-OFFSET->FOROFFSET$INLINE)
                             (cadr loop-idx))))
    (and loop-idx loop-iter loop-idx-offset
         `((loop-idx . ,loop-idx)
           (loop-iter . ,loop-iter)
           (loop-idx-offset . ,loop-idx-offset)))))

(defthm arbmap-disjoint-from-scope-of-for-when-in-disjoint-scope-for-greater-index-special
  (implies (and (bind-free (ota-form-for-scope-bindings ota-call mfc state)
                           (loop-iter loop-idx loop-idx-offset))
                (equal loop-idx (arbaddr-offset->foroffset loop-idx-offset))
                (arbmap-in-for-scope (mv-nth 2 ota-call) sc-addr loop-iter loop-idx)
                (arbaddr-offset-lte sc-scope loop-idx-offset))
           (arbmap-disjoint-from-scope
            (mv-nth 2 ota-call)
            sc-addr
            sc-offset sc-scope)))





(defthm arbmap-disjoint-from-repeat-loop-scope-by-idx
  (implies (and (arbmap-in-scope arbmap arbaddr arboffset scope)
                (equal arbaddr sc-addr)
                (arbaddr-offset-lte scope loop-idx-offset))
           (arbmap-disjoint-from-loop-scope
            arbmap
            sc-addr nil loop-iter (arbaddr-offset->repeatoffset loop-idx-offset)))
  :hints(("Goal" :in-theory (enable arbmap-disjoint-from-loop-scope
                                    arbmap-in-scope))))

(defthm arbmap-disjoint-from-repeat-loop-scope-by-idx-special
  (implies (and (bind-free (ota-form-scope-bindings ota-call mfc state)
                           (arbaddr arboffset scope))
                (arbmap-in-scope (mv-nth 2 ota-call) arbaddr arboffset scope)
                (equal arbaddr sc-addr)
                (arbaddr-offset-lte scope loop-idx-offset))
           (arbmap-disjoint-from-loop-scope
            (mv-nth 2 ota-call)
            sc-addr nil loop-iter (arbaddr-offset->repeatoffset loop-idx-offset))))


(defun ota-form-call-scope-bindings (call mfc state)
  (declare (ignorable mfc)
           (xargs :stobjs state
                  :mode :program))
  (b* ((wrld (w state))
       ((unless (consp call)) nil)
       (fn (car call))
       (formals (acl2::formals fn wrld))
       (args (pairlis$ formals (cdr call)))
       (call-addr (cdr (assoc 'arbaddr args))))
    (case-match call-addr
      (('cons ('arbaddr-component-call
               name ('call-count-map-entry name ('arbaddr-offset->callmap$inline arboffset)))
              arbaddr)
       `((arbaddr . ,arbaddr)
         (name . ,name)
         (arboffset . ,arboffset)))
      (& nil))))


(defthm arbaddr-call-not-suffixp-when-in-scope
  (implies (and (arbaddr-in-scope addr arbaddr offset scope)
                (arbaddr-offset-lte scope calloffset))
           (not (arbaddr-suffixp (cons (arbaddr-component-call
                                        name (call-count-map-entry name (arbaddr-offset->callmap calloffset)))
                                       arbaddr)
                                 addr)))
  :hints(("Goal" :in-theory (enable arbaddr-in-scope
                                    arbaddr-offset-lte
                                    arbaddr-offset-lte-component
                                    call-count-map-entry-bound-by-call-count-map-lte))))

(defthm arbmap-disjoint-from-scope-of-call-when-in-disjoint-scope
  (implies (and (arbmap-in-arbaddr-scope
                 arbmap
                 (cons (arbaddr-component-call
                        name (call-count-map-entry name
                                                   (arbaddr-offset->callmap arboffset)))
                       sc-addr))
                (arbaddr-offset-lte sc-scope arboffset))
           (arbmap-disjoint-from-scope
            arbmap
            sc-addr
            sc-offset sc-scope))
  :hints(("Goal" :in-theory (enable arbmap-in-arbaddr-scope
                                    arbmap-disjoint-from-scope))))

(defthm arbmap-disjoint-from-scope-of-call-when-in-disjoint-scope-special
  (implies (and (bind-free (ota-form-call-scope-bindings ota-call mfc state)
                           (arbaddr name arboffset))
                (equal arbaddr sc-addr)
                (arbmap-in-arbaddr-scope
                 (mv-nth 2 ota-call)
                 (cons (arbaddr-component-call
                        name (call-count-map-entry name
                                                   (arbaddr-offset->callmap arboffset)))
                       sc-addr))
                (arbaddr-offset-lte sc-scope arboffset))
           (arbmap-disjoint-from-scope
            (mv-nth 2 ota-call)
            sc-addr
            sc-offset sc-scope)))



(defthm arbmap-disjoint-from-arbaddr-scope-when-in-disjoint-scope
  (implies (and (arbmap-in-scope
                 arbmap calladdr arboffset scope)
                (arbaddr-offset-lte scope calloffset))
           (arbmap-disjoint-from-arbaddr-scope
            arbmap (cons (arbaddr-component-call
                          name
                          (call-count-map-entry
                           name (arbaddr-offset->callmap calloffset)))
                         calladdr)))
  :hints(("Goal" :in-theory (enable arbmap-in-scope
                                    arbmap-disjoint-from-arbaddr-scope))))

(defthm arbmap-disjoint-from-arbaddr-scope-when-in-disjoint-scope-special
  (implies (and (bind-free (ota-form-scope-bindings ota-call mfc state)
                           (arbaddr arboffset scope))
                (equal arbaddr calladdr)
                (arbmap-in-scope
                 (mv-nth 2 ota-call) arbaddr arboffset scope)
                (arbaddr-offset-lte scope calloffset))
           (arbmap-disjoint-from-arbaddr-scope
            (mv-nth 2 ota-call) (cons (arbaddr-component-call
                                       name
                                       (call-count-map-entry
                                        name (arbaddr-offset->callmap calloffset)))
                                      calladdr))))


(defthm arb-arbaddr-not-in-scope
  (implies (arbaddr-offset-lte scope arb-arboffset)
           (not (arbaddr-in-scope (cons (arbaddr-component-arb (arbaddr-offset->arboffset arb-arboffset))
                                        arbaddr)
                                  arbaddr arboffset scope)))
  :hints(("Goal" :in-theory (enable arbaddr-in-scope
                                    arbaddr-offset-lte
                                    arbaddr-offset-lte-component
                                    arbaddr-last-nonbase-component))))

(defthm arbmap-boundp-when-arb-disjoint
  (implies (and (arbmap-in-scope
                 arbmap arbaddr arboffset scope)
                (arbaddr-offset-lte scope arb-arboffset))
           (not (arbmap-boundp (cons (arbaddr-component-arb
                                      (arbaddr-offset->arboffset arb-arboffset))
                                     arbaddr)
                               arbmap)))
  :hints(("Goal" :in-theory (enable arbmap-in-scope arbmap-boundp hons-assoc-equal arbmap-fix))))

(defthm arbmap-boundp-when-arb-disjoint-special
  (implies (and (bind-free (ota-form-scope-bindings ota-call mfc state)
                           (arbaddr arboffset scope))
                (equal arbaddr arb-arbaddr)
                (arbmap-in-scope
                 (mv-nth 2 ota-call) arbaddr arboffset scope)
                (arbaddr-offset-lte scope arb-arboffset))
           (not (arbmap-boundp (cons (arbaddr-component-arb
                                      (arbaddr-offset->arboffset arb-arboffset))
                                     arb-arbaddr)
                               (mv-nth 2 ota-call)))))





;; (defthm arbmap-disjoint-from-scope-of-while-when-in-disjoint-scope-special
;;   (implies (and ;; (bind-free (ota-form-loop-scope-bindings ota-call mfc state)
;;                 ;;            (arbaddr arboffset scope))
;;                 (arbmap-in-loop-scope (mv-nth 2 ota-call) sc-addr t loop-idx (+ 1 (nfix loop-iter))))
;;            (arbmap-disjoint-from-scope
;;             (mv-nth 2 ota-call)
;;             (cons (arbaddr-component-while loop-idx loop-iter) sc-addr)
;;             sc-offset sc-scope))
;;   :hints(("Goal" :in-theory (enable arbmap-in-loop-scope
;;                                     arbmap-disjoint-from-scope))))



(with-output
  :evisc (:gag-mode (evisc-tuple 3 4 nil nil))
  (std::defret-mutual eval_loop-*ota-is_while-boolean-fix
    (std::defret eval_loop-*ota-of-boolean-fix
      (equal (let ((is_while (acl2::bool-fix is_while))) <call>)
             <call>)
      :hints ('(:expand ((:free (is_while) <call>))))
      :fn eval_loop-*ota)
    :skip-others t
    :mutual-recursion asl-interpreter-mutual-recursion-*ota))

(with-output
  :evisc (:gag-mode (evisc-tuple 3 4 nil nil))
  (std::defret-mutual eval_loop-*a-is_while-boolean-fix
    (std::defret eval_loop-*a-of-boolean-fix
      (equal (let ((is_while (acl2::bool-fix is_while))) <call>)
             <call>)
      :hints ('(:expand ((:free (is_while) <call>))))
      :fn eval_loop-*a)
    :skip-others t
    :mutual-recursion asl-interpreter-mutual-recursion-*a))

;; (fty::deffixequiv eval_loop-*ota :args ((is_while booleanp)))
(defthm eval_loop-*ota-when-is_while
  (implies (and (syntaxp (not (equal is_while ''t)))
                is_while)
           (equal (eval_loop-*ota env is_while limit e_cond body)
                  (eval_loop-*ota env t limit e_cond body)))
  :hints (("goal" :use ((:instance eval_loop-*ota-of-boolean-fix))
           :in-theory (disable eval_loop-*ota-of-boolean-fix))))

(defthm eval_loop-*a-when-is_while
  (implies (and (syntaxp (not (equal is_while ''t)))
                is_while)
           (equal (eval_loop-*a env is_while limit e_cond body)
                  (eval_loop-*a env t limit e_cond body)))
  :hints (("goal" :use ((:instance eval_loop-*a-of-boolean-fix))
           :in-theory (disable eval_loop-*a-of-boolean-fix))))

(defthm arbmap-lookup-when-not-satisfiable
  (implies (not (ty-satisfiable ty))
           (not (arbmap-lookup addr ty arbmap)))
  :hints(("Goal" :in-theory (enable arbmap-lookup))))

(defthm arbmap-lookup-of-cons
  (implies (ty-satisfiable ty)
           (equal (arbmap-lookup addr ty (cons (cons arbaddr val) arbmap))
                  (if (equal (arbaddr-fix addr) arbaddr)
                      (ty-fix-val val ty)
                    (arbmap-lookup addr ty arbmap))))
  :hints(("Goal" :in-theory (enable arbmap-lookup))))

(defthm arbaddr-not-in-scope-of-cons-arb
  (implies (and (arbaddr-equiv arbaddr arbaddr1)
                (arbaddr-offset-lte scope arb-arboffset))
           (not (arbaddr-in-scope
                 (cons (arbaddr-component-arb
                        (arbaddr-offset->arboffset arb-arboffset))
                       arbaddr1)
                 arbaddr arboffset scope)))
  :hints(("Goal" :in-theory (enable arbaddr-in-scope
                                    arbaddr-last-nonbase-component
                                    arbaddr-offset-lte-component
                                    arbaddr-offset-lte))))

(encapsulate nil
  (local (in-theory (acl2::e/d*
                     (arbmap-lookup-when-not-boundp)
                     (equal-of-ev_normal
                      equal-of-ev_throwing
                      equal-of-ev_error
                      equal-of-continuing
                      equal-of-returning
                      (tau-system)))))
  (local (deflabel before-equals-*a))
  (with-output
    :evisc (:gag-mode (evisc-tuple 3 4 nil nil))
    (std::defret-mutual-generate <fn>-equals-*a
      :rules ((t (:add-concl (B* ((a-res (ota-call-to-*a <call>)))
                               (equal a-res res))))
              ((not (:fnname eval_limit-*ota))
               (:add-keyword
                :hints ((and stable-under-simplificationp
                             '(:expand ((:free (pos otherwise is_while clk) <call>)
                                        (:Free (pos otherwise is_while clk arbmap) (ota-call-to-*a <call>)))
                               :do-not-induct t
                               )))))
              ((:fnname eval_limit-*ota)
               (:add-keyword
                :hints ((and stable-under-simplificationp
                             '(:expand ((:free (pos otherwise is_while x) <call>)
                                        (:Free (pos otherwise is_while x arbmap) (ota-call-to-*a <call>)))))))))
      :mutual-recursion asl-interpreter-mutual-recursion-*ota))

  (acl2::def-ruleset! asl-*ota-equals-*a
    (set-difference-equal (current-theory :here)
                          (current-theory 'before-equals-*a))))


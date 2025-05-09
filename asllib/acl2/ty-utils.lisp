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

(include-book "interp-types")
(include-book "centaur/fty/multicase" :dir :system)
(include-book "std/alists/alist-defuns" :dir :system)
(local (include-book "centaur/misc/arith-equivs" :dir :System))
(local (include-book "std/lists/repeat" :dir :system))
(local (include-book "centaur/bitops/ihsext-basics" :dir :System))

(local (std::add-default-post-define-hook :fix))
(local (in-theory (disable ifix unsigned-byte-p)))




(define int-literal-expr-p ((x expr-p))
  (b* ((v (expr->desc x)))
    (expr_desc-case v
      :e_literal (literal-case v.val :l_int)
      :otherwise nil)))

(define int-literal-expr->val ((x expr-p))
  :guard (int-literal-expr-p x)
  :guard-hints (("goal" :in-theory (enable int-literal-expr-p)))
  :returns (val integerp :rule-classes :type-prescription)
  (l_int->val (e_literal->val (expr->desc x))))


(define int_constraint-resolved-p ((x int_constraint-p))
  (int_constraint-case x
    :constraint_exact (int-literal-expr-p x.val)
    :constraint_range (and (int-literal-expr-p x.from)
                           ;; also require from <= to?
                           (int-literal-expr-p x.to)))
  ///
  (defthm int_constraint-resolved-p-implies
    (implies (int_constraint-resolved-p x)
             (and (implies (int_constraint-case x :constraint_exact)
                           (int-literal-expr-p (constraint_exact->val x)))
                  (implies (int_constraint-case x :constraint_range)
                           (and (int-literal-expr-p (constraint_range->from x))
                                (int-literal-expr-p (constraint_range->to x))))))))

(define int_constraintlist-resolved-p ((x int_constraintlist-p))
  (if (atom x)
      t
    (and (int_constraint-resolved-p (car x))
         (int_constraintlist-resolved-p (cdr x))))
  ///
  (defthm int_constraintlist-resolved-p-implies
    (implies (int_constraintlist-resolved-p x)
             (and (int_constraintlist-resolved-p (cdr x))
                  (implies (consp x)
                           (int_constraint-resolved-p (car x)))))))
  
(define constraint_kind-resolved-p ((x constraint_kind-p))
  (constraint_kind-case x
    :unconstrained t
    :wellconstrained (int_constraintlist-resolved-p x.constraints)
    :otherwise nil)
  ///
  (defthm constraint_kind-resolved-p-implies
    (implies (constraint_kind-resolved-p x)
             (and (not (constraint_kind-case x :pendingconstrained))
                  (not (constraint_kind-case x :parametrized))
                  (implies (constraint_kind-case x :wellconstrained)
                           (int_constraintlist-resolved-p (wellconstrained->constraints x)))))))

(define array_index-resolved-p ((x array_index-p))
  (array_index-case x
    :arraylength_expr (int-literal-expr-p x.length)
    :otherwise t)
  ///
  (defthm array_index-resolved-p-implies
    (implies (and (array_index-resolved-p x)
                  (array_index-case x :arraylength_expr))
             (int-literal-expr-p (arraylength_expr->length x)))))


(defines ty-resolved-p
  (define ty-resolved-p ((x ty-p))
    :measure (ty-count x)
    (b* ((x (ty->val x)))
      (type_desc-case x
        :t_int (constraint_kind-resolved-p x.constraint)
        :t_bits (int-literal-expr-p x.expr)
        :t_tuple (tylist-resolved-p x.types)
        :t_array (and (array_index-resolved-p x.index)
                      (ty-resolved-p x.type))
        :t_record (typed_identifierlist-resolved-p x.fields)
        :t_exception (typed_identifierlist-resolved-p x.fields)
        :t_collection (typed_identifierlist-resolved-p x.fields)
        :t_named nil
        :otherwise t))
    ///
    (defthm ty_resolved-p-implies
      (implies (ty-resolved-p x)
               (b* ((x (ty->val x)))
                 (and (implies (type_desc-case x :t_int)
                               (constraint_kind-resolved-p (t_int->constraint x)))
                      (implies (type_desc-case x :t_bits)
                               (int-literal-expr-p (t_bits->expr x)))
                      (implies (type_desc-case x :t_tuple)
                               (tylist-resolved-p (t_tuple->types x)))
                      (implies (type_desc-case x :t_array)
                               (and (array_index-resolved-p (t_array->index x))
                                    (ty-resolved-p (t_array->type x))))
                      (implies (type_desc-case x :t_record)
                               (typed_identifierlist-resolved-p (t_record->fields x)))
                      (implies (type_desc-case x :t_exception)
                               (typed_identifierlist-resolved-p (t_exception->fields x)))
                      (implies (type_desc-case x :t_collection)
                               (typed_identifierlist-resolved-p (t_collection->fields x)))
                      (not (type_desc-case x :t_named)))))))

  (define tylist-resolved-p ((x tylist-p))
    :measure (tylist-count x)
    (if (atom x)
        t
      (and (ty-resolved-p (car x))
           (tylist-resolved-p (cdr x))))
    ///
    (defthm tylist-resolved-p-implies
      (implies (tylist-resolved-p x)
               (and (tylist-resolved-p (cdr x))
                    (implies (consp x)
                             (ty-resolved-p (car x)))))))

  (define typed_identifier-resolved-p ((x typed_identifier-p))
    :measure (typed_identifier-count x)
    (ty-resolved-p (typed_identifier->type x))
    ///
    (defthm typed_identifier-resolved-p-implies
      (implies (typed_identifier-resolved-p x)
               (ty-resolved-p (typed_identifier->type x)))))
  
  (define typed_identifierlist-resolved-p ((x typed_identifierlist-p))
    :measure (typed_identifierlist-count x)
    (if (atom x)
        t
      (and (typed_identifier-resolved-p (car x))
           (typed_identifierlist-resolved-p (cdr x))))
    ///
    (defthm typed_identifierlist-resolved-p-implies
      (implies (typed_identifierlist-resolved-p x)
               (and (typed_identifierlist-resolved-p (cdr x))
                    (implies (consp x)
                             (typed_identifier-resolved-p (car x)))))))
  ///
  (fty::deffixequiv-mutual ty-resolved-p))


(define int_constraint-satisfied ((x integerp)
                                  (c int_constraint-p))
  :guard (int_constraint-resolved-p c)
  (int_constraint-case c
    :constraint_exact (eql (lifix x) (int-literal-expr->val c.val))
    :constraint_range
    (and (<= (int-literal-expr->val c.from) (lifix x))
         (<= (lifix x) (int-literal-expr->val c.to)))))

(define int_constraintlist-satisfied ((x integerp)
                                      (c int_constraintlist-p))
  :guard (int_constraintlist-resolved-p c)
  (if (atom c)
      nil
    (or (int_constraint-satisfied x (car c))
        (int_constraintlist-satisfied x (cdr c)))))


(define constraint_kind-satisfied ((x integerp)
                                   (c constraint_kind-p))
  :guard (constraint_kind-resolved-p c)
  (constraint_kind-case c
    :unconstrained t
    :wellconstrained (int_constraintlist-satisfied x c.constraints)
    :otherwise nil))


(local (defthm vallist-p-vals-of-val-imap-p
         (implies (val-imap-p x)
                  (vallist-p (acl2::alist-vals x)))
         :hints(("Goal" :in-theory (enable acl2::alist-vals)))))

(defines ty-satisfied
  :flag-local nil
  (define ty-satisfied ((x val-p)
                          (ty ty-p))
    :guard (ty-resolved-p ty)
    :measure (acl2::two-nats-measure (ty-count ty) 0)
    (b* ((ty (ty->val ty)))
      (fty::multicase ((type_desc-case ty)
                       (val-case x))
        ((:t_int :v_int) (constraint_kind-satisfied x.val ty.constraint))
        ((:t_bits :v_bitvector) (eql x.len (int-literal-expr->val ty.expr)))
        ((:t_real :v_real) t)
        ((:t_string :v_string) t)
        ((:t_bool :v_bool) t)
        ((:t_enum :v_label) (member-equal x.val ty.elts))
        ((:t_tuple :v_array) (tuple-type-satisfied x.arr ty.types))
        ((:t_array :v_array)
         :when (array_index-case ty.index :arraylength_expr)
         (and (eql (len x.arr) (int-literal-expr->val (arraylength_expr->length ty.index)))
              (array-type-satisfied x.arr ty.type)))
        ((:t_array :v_record)
         :when (array_index-case ty.index :arraylength_enum)
         (and (equal (acl2::alist-keys x.rec) (arraylength_enum->elts ty.index))
              (array-type-satisfied (acl2::alist-vals x.rec) ty.type)))
        ((:t_record :v_record)
         (record-type-satisfied x.rec ty.fields))
        ((:t_exception :v_record)
         (record-type-satisfied x.rec ty.fields))
        ((:t_collection :v_record)
         (record-type-satisfied x.rec ty.fields))
        (- nil))))

  (define tuple-type-satisfied ((x vallist-p)
                                (types tylist-p))
    :guard (tylist-resolved-p types)
    :measure (acl2::two-nats-measure (tylist-count types) 0)
    (if (atom types)
        (atom x)
      (and (consp x)
           (ty-satisfied (car x) (car types))
           (tuple-type-satisfied (cdr x) (Cdr types)))))

  (define array-type-satisfied ((x vallist-p)
                                (ty ty-p))
    :guard (ty-resolved-p ty)
    :measure (acl2::two-nats-measure (ty-count ty) (len x))
    (if (atom x)
        t
      (and (ty-satisfied (car x) ty)
           (array-type-satisfied (cdr x) ty))))

  (define record-type-satisfied ((x val-imap-p)
                                 (fields typed_identifierlist-p))
    :guard (typed_identifierlist-resolved-p fields)
    :measure (acl2::two-nats-measure (typed_identifierlist-count fields) (len x))
    (if (atom x)
        (atom fields)
      (b* (((unless (mbt (and (consp (car x))
                              (identifier-p (caar x)))))
            (record-type-satisfied (cdr x) fields))
           ((unless (consp fields)) nil)
           ((cons key val) (car x))
           ((typed_identifier f1) (car fields)))
        (and (equal key f1.name)
             (ty-satisfied val f1.type)
             (record-type-satisfied (cdr x) (cdr fields))))))
  ///
  (local (in-theory (enable val-imap-fix)))
  (fty::deffixequiv-mutual ty-satisfied))


(define int_constraint-satisfying-val ((x int_constraint-p))
  :guard (int_constraint-resolved-p x)
  :returns (val acl2::maybe-integerp :rule-classes :type-prescription)
  (int_constraint-case x
    :constraint_exact (int-literal-expr->val x.val)
    :constraint_range (b* ((from (int-literal-expr->val x.from))
                           (to (int-literal-expr->val x.to)))
                        (and (<= from to)
                             from)))
  ///
  (defret <fn>-correct
    (implies val
             (int_constraint-satisfied val x))
    :hints(("Goal" :in-theory (enable int_constraint-satisfied))))

  (defret <fn>-sufficient
    (implies (int_constraint-satisfied someval x)
             val)
    :hints(("Goal" :in-theory (enable int_constraint-satisfied)))))

(define int_constraintlist-satisfying-val ((x int_constraintlist-p))
  :guard (int_constraintlist-resolved-p x)
  :returns (val acl2::maybe-integerp :rule-classes :type-prescription)
  (if (atom x)
      nil
    (or (int_constraint-satisfying-val (car x))
        (int_constraintlist-satisfying-val (cdr x))))
  ///
  (defret <fn>-correct
    (implies val
             (int_constraintlist-satisfied val x))
    :hints(("Goal" :in-theory (enable int_constraintlist-satisfied))))

  (defret <fn>-sufficient
    (implies (int_constraintlist-satisfied someval x)
             val)
    :hints(("Goal" :in-theory (enable int_constraintlist-satisfied)))))

(define constraint_kind-satisfying-val ((x constraint_kind-p))
  :guard (constraint_kind-resolved-p x)
  :returns (val acl2::maybe-integerp :rule-classes :type-prescription)
  (constraint_kind-case x
    :unconstrained 0
    :wellconstrained (int_constraintlist-satisfying-val x.constraints)
    :otherwise nil)
  ///
  (defret <fn>-correct
    (implies val
             (constraint_kind-satisfied val x))
    :hints(("Goal" :in-theory (enable constraint_kind-satisfied))))

  (defret <fn>-sufficient
    (implies (constraint_kind-satisfied someval x)
             val)
    :hints(("Goal" :in-theory (enable constraint_kind-satisfied)))))


(defines ty-satisfying-val
  (define ty-satisfying-val ((x ty-p))
    :guard (ty-resolved-p x)
    :verify-guards nil
    :measure (ty-count x)
    :returns (val maybe-val-p)
    (b* ((x (ty->val x)))
      (type_desc-case x
        :t_int (b* ((val (constraint_kind-satisfying-val x.constraint)))
                 (and val (v_int val)))
        :t_bits (b* ((val (int-literal-expr->val x.expr)))
                  (and (<= 0 val)
                       (v_bitvector val 0)))
        :t_real (v_real 0)
        :t_string (v_string "")
        :t_bool (v_bool nil)
        :t_enum (and (consp x.elts) (v_label (car x.elts)))
        :t_tuple (b* (((mv ok vals) (tylist-satisfying-val x.types)))
                   (and ok
                        (v_array vals)))
        :t_array (b* ((val (ty-satisfying-val x.type)))
                   (array_index-case x.index
                     :arraylength_expr
                     (b* ((len (int-literal-expr->val x.index.length)))
                       (if (eql 0 len)
                           (v_array nil)
                         (and (<= 0 len)
                              val
                              (v_array (make-list len :initial-element val)))))
                     :arraylength_enum
                     (if (atom x.index.elts)
                         (v_record nil)
                       (and val
                            (v_record (pairlis$ x.index.elts
                                                (make-list (len x.index.elts) :initial-element val)))))))
        :t_record (b* (((mv ok val) (typed_identifierlist-satisfying-val x.fields)))
                    (and ok
                         (v_record val)))
        :t_exception (b* (((mv ok val) (typed_identifierlist-satisfying-val x.fields)))
                       (and ok
                            (v_record val)))
        :t_collection (b* (((mv ok val) (typed_identifierlist-satisfying-val x.fields)))
                        (and ok
                             (v_record val)))
        :t_named nil)))

  (define tylist-satisfying-val ((x tylist-p))
    :guard (tylist-resolved-p x)
    :measure (tylist-count x)
    :returns (mv ok (val vallist-p))
    (b* (((when (atom x)) (mv t nil))
         (val1 (ty-satisfying-val (car x)))
         ((unless val1)
          (mv nil nil))
         ((mv ok rest) (tylist-satisfying-val (cdr x))))
      (mv ok (and ok (cons val1 rest)))))

  (define typed_identifierlist-satisfying-val ((x typed_identifierlist-p))
    :guard (typed_identifierlist-resolved-p x)
    :measure (typed_identifierlist-count x)
    :returns (mv ok (val val-imap-p))
    (b* (((When (atom x)) (mv t nil))
         ((typed_identifier f1) (car x))
         (val1 (ty-satisfying-val f1.type))
         ((unless val1)
          (mv nil nil))
         ((mv ok rest) (typed_identifierlist-satisfying-val (cdr x))))
      (mv ok (and ok (cons (cons f1.name val1) rest)))))
  ///
  (verify-guards ty-satisfying-val)


  (defthm array-type-satisfied-of-repeat
    (iff (array-type-satisfied (acl2::repeat n val) ty)
         (or (zp n)
             (ty-satisfied val ty)))
    :hints(("Goal" :in-theory (enable acl2::repeat)
            :induct t
            :expand ((:free (x y) (array-type-satisfied (cons x y) ty))
                     (array-type-satisfied nil ty)))))

  (local (defthm alist-vals-of-pairlis$
           (implies (and (equal (len x) (len y))
                         (true-listp y))
                    (equal (acl2::alist-vals (pairlis$ x y)) y))
           :hints(("Goal" :in-theory (enable acl2::alist-vals)))))

  (local (defthm alist-keys-of-pairlis$
           (implies (and (equal (len x) (len y))
                         (true-listp x))
                    (equal (acl2::alist-keys (pairlis$ x y)) x))
           :hints(("Goal" :in-theory (enable acl2::alist-keys)))))

  (local (defthm consp-of-alist-vals
           (iff (consp (acl2::alist-vals x))
                (consp (acl2::Alist-keys x)))
           :hints(("Goal" :in-theory (enable acl2::alist-keys
                                             acl2::alist-vals)))))
  
  (std::defret-mutual <fn>-correct
    (defret <fn>-correct
      (implies val
               (ty-satisfied val x))
      :hints ('(:expand (<call>
                         (:free (v) (ty-satisfied v x))
                         (:free (ty) (array-type-satisfied nil ty)))))
      :fn ty-satisfying-val)
    (defret <fn>-correct
      (implies ok
               (tuple-type-satisfied val x))
      :hints ('(:expand (<call>)
                :in-theory (enable tuple-type-satisfied)))
      :fn tylist-satisfying-val)
    (defret <fn>-correct
      (implies ok
               (record-type-satisfied val x))
      :hints ('(:expand (<call>)
                :in-theory (enable record-type-satisfied)))
      :fn typed_identifierlist-satisfying-val))

  (defthm-ty-satisfied-flag ty-satisfying-val-sufficient
    (defthm ty-satisfying-val-sufficient
      (implies (ty-satisfied x ty)
               (ty-satisfying-val ty))
      :hints ('(:expand ((ty-satisfying-val ty)
                         (ty-satisfied x ty)
                         (:free (ty) (array-type-satisfied nil ty)))))
      :flag ty-satisfied)
    (defthm tylist-satisfying-val-sufficient
      (implies (tuple-type-satisfied x types)
               (mv-nth 0 (tylist-satisfying-val types)))
      :hints ('(:expand ((tuple-type-satisfied x types)
                         (tylist-satisfying-val types))))
      :flag tuple-type-satisfied)
    (defthm typed_identifierlist-satisfying-val-sufficient
      (implies (record-type-satisfied x fields)
               (mv-nth 0 (typed_identifierlist-satisfying-val fields)))
      :hints ('(:expand ((record-type-satisfied x fields)
                         (typed_identifierlist-satisfying-val fields))))
      :flag record-type-satisfied)
    (defthm tmp
      (implies (and (array-type-satisfied x ty)
                    (consp x))
               (ty-satisfying-val ty))
      :hints ('(:expand ((array-type-satisfied x ty))))
      :flag array-type-satisfied
      :skip t)
    :skip-others t)

  (fty::deffixequiv-mutual ty-satisfying-val))


(defines ty-satisfiable
  (define ty-satisfiable ((x ty-p))
    :guard (ty-resolved-p x)
    :verify-guards nil
    :measure (ty-count x)
    :returns (ok)
    (b* ((x (ty->val x)))
      (type_desc-case x
        :t_int (b* ((val (constraint_kind-satisfying-val x.constraint)))
                 (and val t))
        :t_bits (b* ((val (int-literal-expr->val x.expr)))
                  (<= 0 val))
        :t_real t
        :t_string t
        :t_bool t
        :t_enum (consp x.elts)
        :t_tuple (tylist-satisfiable x.types)
        :t_array (array_index-case x.index
                   :arraylength_expr
                   (b* ((len (int-literal-expr->val x.index.length)))
                     (or (eql 0 len)
                         (and (<= 0 len)
                              (ty-satisfiable x.type))))
                   :arraylength_enum
                   (or (atom x.index.elts)
                       (ty-satisfiable x.type)))
        :t_record (typed_identifierlist-satisfiable x.fields)
        :t_exception (typed_identifierlist-satisfiable x.fields)
        :t_collection (typed_identifierlist-satisfiable x.fields)
        :t_named nil)))

  (define tylist-satisfiable ((x tylist-p))
    :guard (tylist-resolved-p x)
    :measure (tylist-count x)
    :returns (ok)
    (if (atom x)
        t
      (and (ty-satisfiable (car x))
           (tylist-satisfiable (cdr x)))))

  (define typed_identifierlist-satisfiable ((x typed_identifierlist-p))
    :guard (typed_identifierlist-resolved-p x)
    :measure (typed_identifierlist-count x)
    :returns (ok)
    (b* (((When (atom x)) t)
         ((typed_identifier f1) (car x)))
      (and (ty-satisfiable f1.type)
           (typed_identifierlist-satisfiable (cdr x)))))
  ///
  (verify-guards ty-satisfiable)


  (std::defret-mutual <fn>-correct
    (defret <fn>-correct
      (iff (ty-satisfying-val x)
           ok)
      :hints ('(:expand (<call>
                         (ty-satisfying-val x))))
      :fn ty-satisfiable)
    (defret <fn>-correct
      (iff (mv-nth 0 (tylist-satisfying-val x))
           ok)
      :hints ('(:expand (<call>
                         (tylist-satisfying-val x))))
      :fn tylist-satisfiable)
    (defret <fn>-correct
      (iff (mv-nth 0 (typed_identifierlist-satisfying-val x))
           ok)
      :hints ('(:expand (<call>
                         (typed_identifierlist-satisfying-val x))))
      :fn typed_identifierlist-satisfiable))

  (fty::deffixequiv-mutual ty-satisfiable))






(define int_constraint-width ((x int_constraint-p))
  :guard (int_constraint-resolved-p x)
  :returns (val natp :rule-classes :type-prescription)
  (int_constraint-case x
    :constraint_exact 1
    :constraint_range (b* ((from (int-literal-expr->val x.from))
                           (to (int-literal-expr->val x.to)))
                        (if (<= from to)
                            (+ 1 (- to from))
                          0)))
  ///
  (defret <fn>-zero
    (iff (equal val 0)
         (not (int_constraint-satisfying-val x)))
    :hints(("Goal" :in-theory (enable int_constraint-satisfying-val)))))

(define int_constraintlist-width ((x int_constraintlist-p))
  :guard (int_constraintlist-resolved-p x)
  :returns (val natp :rule-classes :type-prescription)
  (if (atom x)
      0
    (+ (int_constraint-width (car x))
       (int_constraintlist-width (cdr x))))
  ///
  (defret <fn>-zero
    (iff (equal val 0)
         (not (int_constraintlist-satisfying-val x)))
    :hints(("Goal" :in-theory (enable int_constraintlist-satisfying-val))))

  (defret <fn>-posp
    (implies (int_constraintlist-satisfying-val x)
             (posp val))
    :rule-classes :type-prescription))






(define int_constraint-value-fix ((x integerp)
                                  (c int_constraint-p))
  :guard (int_constraint-resolved-p c)
  :returns (new-x integerp :rule-classes :type-prescription)
  (int_constraint-case c
    :constraint_exact (int-literal-expr->val c.val)
    :constraint_range
    (if (<= (int-literal-expr->val c.from) (lifix x))
        (if (<= (lifix x) (int-literal-expr->val c.to))
            (lifix x)
          (int-literal-expr->val c.to))
      (int-literal-expr->val c.from)))
  ///
  (defthm int_constraint-value-fix-satisfying
    (implies (int_constraint-satisfying-val c)
             (int_constraint-satisfied (int_constraint-value-fix x c) c))
    :hints(("Goal" :in-theory (enable int_constraint-satisfied
                                      int_constraint-satisfying-val))))
  
  (defthm int_constraint-value-fix-when-satisfied
    (implies (int_constraint-satisfied x c)
             (equal (int_constraint-value-fix x c)
                    (ifix x)))
    :hints(("Goal" :in-theory (enable int_constraint-satisfied)))))

(define int_constraintlist-value-fix ((x integerp)
                                      (c int_constraintlist-p))
  :guard (int_constraintlist-resolved-p c)
  :verify-guards nil
  :guard-hints (("goal" :in-theory (enable int_constraintlist-resolved-p)))
  :returns (new-x integerp :rule-classes :type-prescription)
  ;; a little complicated and inefficient
  (if (atom c)
      0
    (if (int_constraint-satisfied x (car c))
        (lifix x)
      (let ((new-x (int_constraintlist-value-fix x (cdr c))))
        (if (int_constraintlist-satisfied new-x (cdr c))
            new-x
          (int_constraint-value-fix x (car c))))))
  ///
  (verify-guards int_constraintlist-value-fix)
  
  (defthm int_constraintlist-value-fix-satisfying
    (implies (int_constraintlist-satisfying-val c)
             (int_constraintlist-satisfied (int_constraintlist-value-fix x c) c))
    :hints(("Goal" :in-theory (enable int_constraintlist-satisfied
                                      int_constraintlist-satisfying-val))))
  
  (defthm int_constraintlist-value-fix-when-satisfied
    (implies (int_constraintlist-satisfied x c)
             (equal (int_constraintlist-value-fix x c)
                    (ifix x)))
    :hints(("Goal" :in-theory (enable int_constraintlist-satisfied)))))


(define constraint_kind-value-fix ((x integerp)
                                   (c constraint_kind-p))
  :guard (constraint_kind-resolved-p c)
  :returns (new-x integerp :rule-classes :type-prescription)
  (constraint_kind-case c
    :wellconstrained (int_constraintlist-value-fix x c.constraints)
    :otherwise (lifix x))
  ///
  
  (defthm constraint_kind-value-fix-satisfying
    (implies (constraint_kind-satisfying-val c)
             (constraint_kind-satisfied (constraint_kind-value-fix x c) c))
    :hints(("Goal" :in-theory (enable constraint_kind-satisfied
                                      constraint_kind-satisfying-val))))
  
  (defthm constraint_kind-value-fix-when-satisfied
    (implies (constraint_kind-satisfied x c)
             (equal (constraint_kind-value-fix x c)
                    (ifix x)))
    :hints(("Goal" :in-theory (enable constraint_kind-satisfied)))))



(define unsigned-byte-p* ((n integerp) x)
  ;; True if either (unsigned-byte-p n x) or if n is
  ;; ill-typed (negative or non-integer) and x is 0.
  (unsigned-byte-p (nfix n) x)
  ///
  (defthm unsigned-byte-p*-implies-natp
    (implies (unsigned-byte-p* n x)
             (natp x))
    :rule-classes :forward-chaining)

  (fty::deffixequiv unsigned-byte-p* :args ((n natp)))

  (defthm unsigned-byte-p*-of-0
    (unsigned-byte-p* n 0)))

(define loghead* ((n integerp) (x integerp))
  :returns (new-x natp :rule-classes :type-prescription)
  (loghead (nfix n) x)
  ///
  (defthm unsigned-byte-p*-of-loghead*
    (unsigned-byte-p* n (loghead* n x))
    :hints(("Goal" :in-theory (enable unsigned-byte-p*))))

  (defthm loghead*-when-unsigned-byte-p*
    (implies (unsigned-byte-p* n x)
             (equal (loghead* n x) x))
    :hints(("Goal" :in-theory (enable unsigned-byte-p*
                                      nfix))))

  (fty::deffixequiv loghead* :args ((n natp) (x integerp))))

(local (include-book "centaur/vl/util/default-hints" :dir :system))

(local (defthm alist-keys-of-pairlis$
         (equal (acl2::alist-keys (pairlis$ x y))
                (true-list-fix x))
         :hints(("Goal" :in-theory (enable acl2::alist-keys pairlis$)))))

(local (defthm alist-vals-of-pairlis$
         (implies (equal (len x) (len y))
                  (equal (acl2::alist-vals (pairlis$ x y))
                         (true-list-fix y)))
         :hints(("Goal" :in-theory (enable acl2::alist-vals pairlis$)))))                       

(local (defthm pairlis$-keys-vals-when-alistp
         (implies (alistp x)
                  (equal (pairlis$ (acl2::alist-keys x)
                                   (acl2::alist-vals x))
                         x))
         :hints(("Goal" :in-theory (enable acl2::alist-vals
                                           acl2::alist-keys)))))

(local (defthm alistp-when-val-imap-p-rw
         (implies (Val-imap-p x)
                  (alistp x))))


(local (defthm len-of-alist-vals
         (equal (len (acl2::alist-vals x))
                (len (acl2::alist-keys x)))
         :hints(("Goal" :in-theory (enable acl2::alist-keys
                                           acl2::alist-vals)))))

(local (defthm len-equal-0
         (equal (equal (len x) 0)
                (atom x))))




(defines ty-fix-val
  :flag-local nil
  (define ty-fix-val ((x val-p) (ty ty-p))
    :guard (and (ty-resolved-p ty)
                (ty-satisfied x ty))
    :verify-guards nil
    :measure (acl2::two-nats-measure (ty-count ty) 0)
    :returns (new-x (and (val-p new-x)
                         (implies (ty-satisfiable ty)
                                  (ty-satisfied new-x ty)))
                    :hints ('(:expand ((ty-satisfiable ty)
                                       (:free (x) (ty-satisfied x ty))
                                       (:free (ty) (array-type-satisfied nil ty))))))
    (b* ((ty (ty->val ty)))
      (type_desc-case ty
        (:t_int (v_int (constraint_kind-value-fix (v_int->val x) ty.constraint)))
        (:t_bits (v_bitvector (int-literal-expr->val ty.expr)
                              (loghead* (int-literal-expr->val ty.expr) (v_bitvector->val x))))
        (:t_real (v_real (v_real->val x)))
        (:t_string (v_string (v_string->val x)))
        (:t_bool (v_bool (v_bool->val x)))
        (:t_enum (v_label (if (member-equal (v_label->val x) ty.elts)
                              (v_label->val x)
                            (car ty.elts))))
        (:t_tuple (v_array (tuple-type-fix-val (v_array->arr x) ty.types)))
        (:t_array (array_index-case ty.index
                    :arraylength_expr
                    (v_array (array-type-fix-val (nfix (int-literal-expr->val ty.index.length))
                                                 (v_array->arr x)
                                                 ty.type))
                    :arraylength_enum
                    (v_record (pairlis$ ty.index.elts
                                        (array-type-fix-val
                                         (len ty.index.elts)
                                         (acl2::alist-vals (v_record->rec x))
                                         ty.type)))))
        (:t_record (v_record (record-type-fix-val (v_record->rec x) ty.fields)))
        (:t_exception (v_record (record-type-fix-val (v_record->rec x) ty.fields)))
        (:t_collection (v_record (record-type-fix-val (v_record->rec x) ty.fields)))
        (otherwise (v_int 0)))))

  (define tuple-type-fix-val ((x vallist-p) (types tylist-p))
    :guard (and (tylist-resolved-p types)
                (tuple-type-satisfied x types))
    :measure (acl2::two-nats-measure (tylist-count types) 0)
    :returns (new-x (and (vallist-p new-x)
                         (implies (tylist-satisfiable types)
                                  (tuple-type-satisfied new-x types)))
                    :hints ('(:expand ((tylist-satisfiable types)
                                       (:free (x) (tuple-type-satisfied x types))))))
    (if (atom types)
        nil
      (cons (ty-fix-val (and (consp x) (car x)) (car types))
            (tuple-type-fix-val (and (consp x) (cdr x)) (Cdr types)))))

  (define array-type-fix-val ((len natp) (x vallist-p) (ty ty-p))
    :guard (and (ty-resolved-p ty)
                (equal (len x) len)
                (array-type-satisfied x ty))
    :measure (acl2::two-nats-measure (ty-count ty) len)
    :returns (new-x (and (vallist-p new-x)
                         (equal (len new-x) (nfix len))
                         (implies (ty-satisfiable ty)
                                  (array-type-satisfied new-x ty)))
                    :hints ((and stable-under-simplificationp
                                 (prog2$ (cw "giving expand hint~%")
                                 '(:expand ((:free (ty) (array-type-satisfied nil ty))
                                            (:free (a b) (array-type-satisfied (cons a b) ty))))))))
    (if (zp len)
        nil
      (cons (ty-fix-val (and (consp x) (car x)) ty)
            (array-type-fix-val (1- len) (and (consp x) (cdr x)) ty))))

  (define record-type-fix-val ((x val-imap-p) (fields typed_identifierlist-p))
    :guard (and (typed_identifierlist-resolved-p fields)
                (record-type-satisfied x fields))
    :measure (acl2::two-nats-measure (typed_identifierlist-count fields) (len x))
    :returns (new-x (implies (typed_identifierlist-satisfiable fields)
                             (record-type-satisfied new-x fields))
                    :hints ('(:expand ((typed_identifierlist-satisfiable fields)
                                       (:free (x y) (record-type-satisfied (cons x y) fields))
                                       (record-type-satisfied nil fields)))))
    (b* (((when (atom fields)) nil)
         ((when (and (consp x)
                     (or (atom (car x))
                         (not (identifier-p (caar x))))))
          (record-type-fix-val (cdr x) fields))
         ((typed_identifier f1) (car fields))
         (val (ty-fix-val (and (consp x) (cdar x)) f1.type)))
      (cons (cons f1.name val)
            (record-type-fix-val (and (consp x) (cdr x)) (cdr fields)))))
  ///

  (local (defthm val-imap-fix-when-atom
           (implies (not (consp x))
                    (equal (val-imap-fix x) nil))
           :hints(("Goal" :in-theory (enable val-imap-fix)))))

  ;; (local (in-theory (enable val-imap-fix)))
  (fty::deffixequiv-mutual ty-fix-val
    :hints ((and stable-under-simplificationp
                 '(:expand ((val-imap-fix x))))))

  (local (defthm reduce-val-imap-hack
           (implies (and (val-imap-p x)
                         (consp x)
                         (equal id (caar x)))
                    (equal (cons (cons id (cdar x)) (cdr x))
                           x))))

  (local (defthm record-type-satisfied-when-atom
           (implies (atom fields)
                    (iff (record-type-satisfied x fields)
                         (not (val-imap-fix x))))
           :hints(("Goal"
                   :induct (len x)
                   :expand ((record-type-satisfied x fields)
                            (val-imap-fix x))))))
  
  (std::defret-mutual ty-fix-val-when-satisfied
    (defret <fn>-when-satisfied
      (implies (ty-satisfied x ty)
               (equal new-x (val-fix x)))
      :hints ('(:expand ((ty-satisfied x ty)
                         <call>)
                :in-theory (enable loghead*)))
      :fn ty-fix-val)
    (defret <fn>-when-satisfied
      (implies (tuple-type-satisfied x types)
               (equal new-x (vallist-fix x)))
      :hints ('(:expand ((tuple-type-satisfied x types)
                         <call>)))
      :fn tuple-type-fix-val)
    (defret <fn>-when-satisfied
      (implies (and (array-type-satisfied x ty)
                    (equal (len x) (nfix len)))
               (equal new-x (vallist-fix x)))
      :hints ('(:expand ((array-type-satisfied x ty)
                         <call>
                         (:free (x ty) (array-type-fix-val 0 x ty)))))
      :fn array-type-fix-val)
    (defret <fn>-when-satisfied
      (implies (and (record-type-satisfied x fields))
               (equal new-x (val-imap-fix x)))
      :hints ('(:expand ((record-type-satisfied x fields)
                         (val-imap-fix x)
                         <call>)))
      :fn record-type-fix-val))
  
  (verify-guards ty-fix-val
    :hints (("goal" :expand ((ty-satisfied x ty)
                             (tuple-type-satisfied x types)
                             (record-type-satisfied x nil)
                             (record-type-satisfied x fields)
                             (array-type-satisfied x ty)
                             (tuple-type-satisfied nil types)
                             (record-type-satisfied nil fields))
             :in-theory (enable loghead*)
             :do-not-induct t))))






(defmacro nats-measure (&rest args)
  `(acl2::nat-list-measure (list . ,args)))


(defines name-resolve-ty
  :verify-guards nil
    
  (define name-resolve-ty ((env static_env_global-p)
                           (x ty-p)
                           &key ((clk natp) 'clk))
    :returns (res (and (eval_result-p res)
                           (implies (eval_result-case res :ev_normal)
                                    (ty-p (ev_normal->res res)))))
    :measure (nats-measure clk 0 (ty-count x) 0)
    (b* ((ty (ty->val x)))
      (type_desc-case ty
        :t_tuple (b* (((ev tys) (name-resolve-tylist env ty.types)))
                   (ev_normal (ty (t_tuple tys))))
        :t_array (b* (((ev base) (name-resolve-ty env ty.type)))
                   (ev_normal (ty (t_array ty.index base))))
        :t_record (b* (((ev fields)
                        (name-resolve-typed_identifierlist env ty.fields)))
                    (ev_normal (ty (t_record fields))))
        :t_exception (b* (((ev fields)
                           (name-resolve-typed_identifierlist env ty.fields)))
                       (ev_normal (ty (t_exception fields))))
        :t_collection (b* (((ev fields)
                            (name-resolve-typed_identifierlist env ty.fields)))
                        (ev_normal (ty (t_collection fields))))
        :t_named  (b* ((decl_types (static_env_global->declared_types env))
                       (look (hons-assoc-equal ty.name decl_types))
                       ((unless look)
                        (ev_error "Named type not found" x))
                       ((when (zp clk))
                        (ev_error "Clock ran out resolving named type" x))
                       (type (ty-timeframe->ty (cdr look))))
                    (name-resolve-ty env type :clk (1- clk)))
        :otherwise (ev_normal (ty ty)))))
  
  (define name-resolve-tylist ((env static_env_global-p)
                               (x tylist-p)
                               &key ((clk natp) 'clk))
    :returns (res (and (eval_result-p res)
                           (implies (eval_result-case res :ev_normal)
                                    (tylist-p (ev_normal->res res)))))
    :measure (nats-measure clk 0 (tylist-count x) 0)
    (if (atom x)
        (ev_normal nil)
      (b* (((ev first) (name-resolve-ty env (car x)))
           ((ev rest) (name-resolve-tylist env (cdr x))))
        (ev_normal (cons first rest)))))

  (define name-resolve-typed_identifierlist ((env static_env_global-p)
                                             (x typed_identifierlist-p)
                                             &key ((clk natp) 'clk))
    :returns (res (and (eval_result-p res)
                           (implies (eval_result-case res :ev_normal)
                                    (typed_identifierlist-p (ev_normal->res res)))))
    :measure (nats-measure clk 0 (typed_identifierlist-count x) 0)
    (b* (((when (atom x)) (ev_normal nil))
         ((typed_identifier x1) (car x))
         ((ev first) (name-resolve-ty env x1.type))
         ((ev rest) (name-resolve-typed_identifierlist env (cdr x))))
      (ev_normal (cons (typed_identifier x1.name first) rest))))
  ///
  (Verify-guards name-resolve-ty-fn))

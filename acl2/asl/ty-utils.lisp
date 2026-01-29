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

(define int-literal-expr ((val integerp))
  :returns (e expr-p)
  (expr (e_literal (l_int val))
        (posn "" 0 0 0))
  ///
  (defret int-literal-expr-p-of-<fn>
    (int-literal-expr-p e)
    :hints(("Goal" :in-theory (enable int-literal-expr-p))))
  
  (defret int-literal-expr->val-of-<fn>
    (equal (int-literal-expr->val e)
           (ifix val))
    :hints(("Goal" :in-theory (enable int-literal-expr->val)))))


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
    (b* ((x (ty->desc x)))
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
               (b* ((x (ty->desc x)))
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



(local (defthm vallist-p-of-insert
         (implies (and (val-p x)
                       (vallist-p y))
                  (vallist-p (insert x y)))
         :hints(("Goal" :in-theory (enable insert
                                           head
                                           tail)))))

(defthm vallist-p-of-values-when-val-imap-p
  (implies (val-imap-p x)
           (vallist-p (omap::values x)))
  :hints(("Goal" :in-theory (enable omap::values val-imap-p))))


(define typed_identifierlist->names ((x typed_identifierlist-p))
  :parents (typed_identifierlist)
  :returns (names identifierlist-p)
  (if (atom x)
      nil
    (cons (typed_identifier->name (car x))
          (typed_identifierlist->names (cdr x))))
  ///
  (defret len-of-<fn>
    (equal (len names) (len x))))

(local (defthm member-of-insert
         (iff (member-equal k (insert x y))
              (or (equal k x) (member-equal k (sfix y))))
         :hints(("Goal" :in-theory (enable insert head tail emptyp setp sfix)))))

(local (defthm assoc-when-member-keys
         (implies (member-equal k (omap::keys x))
                  (omap::assoc k x))
         :hints(("Goal" :in-theory (enable omap::keys omap::assoc)))))

(local (include-book "std/lists/sets" :dir :system))


(defines ty-satisfied
  :flag-local nil
  :prepwork ((local (defthm equal-mergesort-implies-set-equiv
                      (implies (equal x (mergesort y))
                               (acl2::set-equiv x y))
                      :rule-classes :forward-chaining)))
  (define ty-satisfied ((x val-p)
                          (ty ty-p))
    :guard (ty-resolved-p ty)
    :measure (acl2::two-nats-measure (ty-count ty) 0)
    (b* ((ty (ty->desc ty)))
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
         (and (equal (omap::keys x.rec) (set::mergesort (arraylength_enum->elts ty.index)))
              (array-type-satisfied (omap::values x.rec) ty.type)))
        ((:t_record :v_record)
         (and (no-duplicatesp-equal (typed_identifierlist->names ty.fields))
              (equal (omap::keys x.rec) (set::mergesort (typed_identifierlist->names ty.fields)))
              (record-type-satisfied x.rec ty.fields)))
        ((:t_exception :v_record)
         (and (no-duplicatesp-equal (typed_identifierlist->names ty.fields))
              (equal (omap::keys x.rec) (set::mergesort (typed_identifierlist->names ty.fields)))
              (record-type-satisfied x.rec ty.fields)))
        ((:t_collection :v_record)
         (and (no-duplicatesp-equal (typed_identifierlist->names ty.fields))
              (equal (omap::keys x.rec) (set::mergesort (typed_identifierlist->names ty.fields)))
              (record-type-satisfied x.rec ty.fields)))
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
    :guard (and (typed_identifierlist-resolved-p fields)
                (subsetp-equal (typed_identifierlist->names fields) (omap::keys (val-imap-fix x))))
    :guard-hints (("goal" ;; :in-theory (enable typed_identifierlist->names)
                   :expand ((typed_identifierlist->names fields))))
    :measure (acl2::two-nats-measure (typed_identifierlist-count fields) 0)
    (b* (((when (atom fields)) t)
         ((typed_identifier f1) (car fields))
         (look (omap::assoc f1.name (val-imap-fix x)))
         ((unless look) nil)
         (val (cdr look)))
      (and (ty-satisfied val f1.type)
           (record-type-satisfied x (cdr fields)))))
  ///
  (local (in-theory (enable val-imap-fix)))
  (fty::deffixequiv-mutual ty-satisfied)

  (defthmd array-type-satisfied-when-bad-member
    (implies (and (member-equal bad x)
                  (not (ty-satisfied bad ty)))
             (not (array-type-satisfied x ty)))
    :hints(("Goal" :induct (member-equal bad x)
            :expand ((array-type-satisfied x ty)))))

  (defthmd array-type-satisfied-when-subsetp
    (implies (and (array-type-satisfied x ty)
                  (subsetp-equal y x))
             (array-type-satisfied y ty))
    :hints (("goal" :induct (subsetp-equal y x)
             :in-theory (e/d (array-type-satisfied-when-bad-member
                              (:i subsetp-equal))
                             (member-equal))
             :expand ((subsetp-equal y x)
                      (array-type-satisfied y ty))))))


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

(defthm omap-values-subset-of-from-lists
  (implies (equal (len x) (len y))
           (subsetp-equal (omap::values (omap::from-lists x y))
                          y))
  :hints(("Goal" :in-theory (enable omap::values omap::from-lists)
          :induct (omap::from-lists x y))
         (and stable-under-simplificationp
              '(:use ((:instance omap::values-of-update
                       (key (car x)) (val (car y))
                       (x (omap::from-lists (cdr x) (cdr y)))))
                :in-theory (disable omap::values-of-update)))))



(defines ty-satisfying-val
  (define ty-satisfying-val ((x ty-p))
    :guard (ty-resolved-p x)
    :verify-guards nil
    :measure (ty-count x)
    :returns (val maybe-val-p)
    (b* ((x (ty->desc x)))
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
                            (v_record (omap::from-lists
                                       x.index.elts
                                       (make-list (len x.index.elts) :initial-element val)))))))
        :t_record
        (and (no-duplicatesp-equal (typed_identifierlist->names x.fields))
             (b* (((mv ok val) (typed_identifierlist-satisfying-val x.fields)))
               (and ok
                    (v_record val))))
        :t_exception
        (and (no-duplicatesp-equal (typed_identifierlist->names x.fields))
             (b* (((mv ok val) (typed_identifierlist-satisfying-val x.fields)))
               (and ok
                    (v_record val))))
        :t_collection
        (and (no-duplicatesp-equal (typed_identifierlist->names x.fields))
             (b* (((mv ok val) (typed_identifierlist-satisfying-val x.fields)))
               (and ok
                    (v_record val))))
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
      (mv ok (and ok (omap::update f1.name val1 rest)))))
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

  (std::defret-mutual keys-of-typed_identifierlist-satisfying-val
    (defret keys-of-typed_identifierlist-satisfying-val
      (implies ok
               (equal (omap::keys val)
                      (set::mergesort (typed_identifierlist->names x))))
      :hints('(:expand (<call>
                        (typed_identifierlist->names x)
                        (:free (a b) (mergesort (cons a b))))))
      :fn typed_identifierlist-satisfying-val)
    :skip-others t)

  (local (defthm repeat-under-set-equiv
           (acl2::set-equiv (acl2::repeat n x)
                            (if (zp n)
                                nil
                              (list x)))
           :hints(("Goal" :in-theory (enable acl2::set-unequal-witness-rw)))))
  
  (defthm array-type-satisfied-of-values-of-repeat
    (implies (ty-satisfied val ty)
             (array-type-satisfied
              (omap::values
               (omap::from-lists keys (acl2::repeat (len keys) val)))
              ty))
    :hints (("goal" :use ((:instance array-type-satisfied-when-subsetp
                           (y (omap::values
                               (omap::from-lists keys (acl2::repeat (len keys) val))))
                           (x (list val)))
                          (:instance omap-values-subset-of-from-lists
                           (x keys) (y (acl2::repeat (len keys) val))))
             :expand ((array-type-satisfied (list val) ty)
                      (array-type-satisfied nil ty))
             :in-theory (disable omap-values-subset-of-from-lists)
             :do-not-induct t)))

  (defthm record-type-satisfied-of-update-when-not-member
    (implies (and (record-type-satisfied rest x)
                  (not (member-equal key (typed_identifierlist->names x)))
                  (identifier-p key)
                  (val-p val)
                  (val-imap-p rest))
             (record-type-satisfied (omap::update key val rest) x))
    :hints (("goal" :induct (typed_identifierlist->names x)
             :in-theory (enable (:i typed_identifierlist->names))
             :expand ((typed_identifierlist->names x)
                      (:free (y) (record-type-satisfied y x))))))
  
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
      (implies (and ok
                    (no-duplicatesp-equal (typed_identifierlist->names x)))
               (record-type-satisfied val x))
      :hints ('(:expand (<call>
                         (typed_identifierlist->names x))
                :in-theory (enable record-type-satisfied
                                   omap::lookup)))
      :fn typed_identifierlist-satisfying-val))

  (local (defthm mergesort-under-iff
           (iff (mergesort x)
                (consp x))
           :hints(("Goal" :in-theory (enable mergesort)))))

  (local (defthm consp-of-values
           (equal (consp (omap::values x))
                  (not (omap::emptyp x)))
           :hints(("Goal" :in-theory (enable omap::values)))))
  
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
    (b* ((x (ty->desc x)))
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
        :t_record (and (no-duplicatesp-equal (typed_identifierlist->names x.fields))
                       (typed_identifierlist-satisfiable x.fields))
        :t_exception (and (no-duplicatesp-equal (typed_identifierlist->names x.fields))
                          (typed_identifierlist-satisfiable x.fields))
        :t_collection (and (no-duplicatesp-equal (typed_identifierlist->names x.fields))
                           (typed_identifierlist-satisfiable x.fields))
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


(local (defthm identifierlist-fix-when-not-consp
         (implies (not (consp x))
                  (equal (identifierlist-fix x) nil))))

(local (defthm array-type-satisfied-of-nil
         (array-type-satisfied nil ty)
         :hints (("goal" :expand ((array-type-satisfied nil ty))))))

(local (defthm insert-identifier-fix-mergesort
         (equal (insert (identifier-fix k1)
                        (mergesort (identifierlist-fix keys)))
                (mergesort (identifierlist-fix (cons k1 keys))))
         :hints(("Goal" :in-theory (enable identifierlist-fix)
                 :expand ((:free (a b) (mergesort (cons a b))))))))

(local (defcong acl2::set-equiv equal (array-type-satisfied x ty) 1
         :hints (("goal" :use ((:instance (:functional-instance
                                           acl2::element-list-p-set-equiv-congruence
                                           (acl2::element-list-p (lambda (x) (array-type-satisfied x ty)))
                                           (acl2::element-list-final-cdr-p (lambda (x) t))
                                           (acl2::element-p (lambda (x) (ty-satisfied x ty))))
                                (x x) (y x-equiv)))
                  :in-theory (enable array-type-satisfied)))))


(local (defthm array-type-satisfied-of-values-of-update
         (implies (and (array-type-satisfied (omap::values x) ty)
                       (ty-satisfied val ty))
                  (array-type-satisfied (omap::values (omap::update key val x)) ty))
         :hints (("goal" :use ((:instance omap::values-of-update (key key) (val val) (x x))
                               (:instance array-type-satisfied-when-subsetp
                                (y (omap::values (omap::update key val x)))
                                (x (cons val (omap::values x)))))
                  :in-theory (disable omap::values-of-update
                                      array-type-satisfied-when-subsetp)
                  :expand ((array-type-satisfied (cons val (omap::values x)) ty))
                  :do-not-induct t))))

(local (defthm array-type-satisfied-of-values-of-from-lists
         (implies (and (array-type-satisfied vals ty)
                       (equal (len keys) (len vals)))
                  (array-type-satisfied (omap::values (omap::from-lists keys vals)) ty))
         :hints (("goal" :use ((:instance array-type-satisfied-when-subsetp
                                (y (omap::values (omap::from-lists keys vals)))
                                (x vals)))
                  :in-theory (disable array-type-satisfied-when-subsetp)
                  :do-not-induct t))))


;; (local (defthm key-ord-values-of-update-under-set-equiv
;;          (acl2::set-equiv (omap::key-ord-values (omap::update key val x))
;;                           (if (omap::assoc key x)
;;                               (cons val (remove (omap::lookup key x) (omap::key-ord-values x)))
;;                             (cons val (omap::key-ord-values x))))
;;          :hints(("Goal" :in-theory (enable omap::update omap::key-ord-values))


(local
 #!omap
 (defthm restrict-of-insert
   (equal (restrict (set::insert k keys) x)
          (if (assoc k x)
              (update k (lookup k x) (restrict keys x))
            (restrict keys x)))
   :hints (("goal" :use ((:instance diff-key-when-unequal
                          (x (restrict (set::insert k keys) x))
                          (y (if (assoc k x)
                                 (update k (lookup k x) (restrict keys x))
                               (restrict keys x)))))
            :in-theory (enable assoc-of-restrict lookup)))))

(local (defthm lookup-member-of-values
         (implies (omap::assoc k x)
                  (member-equal (omap::lookup k x) (omap::values x)))
         :hints(("Goal" :use ((:instance omap::in-values-when-assoc
                               (a k) (m x) (b (omap::lookup k x))))
                 :in-theory (enable omap::lookup
                                    set::in-to-member)))))

(local (defthm ty-satisfied-of-member-when-array-type-satisfied
         (implies (and (array-type-satisfied lst ty)
                       (member-equal x lst))
                  (ty-satisfied x ty))
         :hints(("Goal" :in-theory (enable array-type-satisfied)))))


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
    (b* ((ty (ty->desc ty)))
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
                    (v_record (let ((keys (set::mergesort ty.index.elts)))
                                (enumarray-type-fix-val
                                 keys (v_record->rec x) ty.type)))))
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

  (define enumarray-type-fix-val ((keys identifierlist-p) (x val-imap-p) (ty ty-p))
    :guard (and (ty-resolved-p ty)
                (subsetp-equal keys (omap::keys x))
                (array-type-satisfied (omap::values x) ty))
    :measure (acl2::two-nats-measure (ty-count ty) (len keys))
    :returns (new-x (And (val-imap-p new-x)
                         (equal (omap::keys new-x) (mergesort (identifierlist-fix keys)))
                         (implies (ty-satisfiable ty)
                                  (array-type-satisfied (omap::values new-x) ty))))
    (if (atom keys)
        nil
      (omap::update (identifier-fix (car keys))
                    (ty-fix-val (omap::lookup (identifier-fix (car keys))
                                              (val-imap-fix x)) ty)
                    (enumarray-type-fix-val (cdr keys) x ty))))

  (define record-type-fix-val ((x val-imap-p) (fields typed_identifierlist-p))
    :guard (and (typed_identifierlist-resolved-p fields)
                (subsetp-equal (typed_identifierlist->names fields)
                               (omap::keys x))
                (record-type-satisfied x fields))
    :measure (acl2::two-nats-measure (typed_identifierlist-count fields) 0)
    :returns (new-x (and (val-imap-p new-x)
                         (equal (omap::keys new-x) (mergesort (typed_identifierlist->names fields)))
                         (implies (and (typed_identifierlist-satisfiable fields)
                                       (no-duplicatesp-equal (typed_identifierlist->names fields)))
                                  (record-type-satisfied new-x fields)))
                    :hints ('(:expand ((typed_identifierlist-satisfiable fields)
                                       (typed_identifierlist->names fields)
                                       (:free (x) (record-type-satisfied x fields))
                                       (:free (a b) (mergesort (cons a b))))
                              :in-theory (enable omap::lookup))))
    (b* (((when (atom fields)) nil)
         ((typed_identifier f1) (car fields))
         (val (ty-fix-val (omap::lookup f1.name (val-imap-fix x)) f1.type)))
      (omap::update f1.name val
                    (record-type-fix-val x (cdr fields)))))
  ///

  ;; (local (defthm val-imap-fix-when-atom
  ;;          (implies (not (consp x))
  ;;                   (equal (val-imap-fix x) nil))
  ;;          :hints(("Goal" :in-theory (enable val-imap-fix)))))

  ;; (local (in-theory (enable val-imap-fix)))
  (fty::deffixequiv-mutual ty-fix-val
    :hints ((and stable-under-simplificationp
                 '(:expand ((val-imap-fix x))))))

  ;; (local (defthm reduce-val-imap-hack
  ;;          (implies (and (val-imap-p x)
  ;;                        (consp x)
  ;;                        (equal id (caar x)))
  ;;                   (equal (cons (cons id (cdar x)) (cdr x))
  ;;                          x))))

  ;; (local (defthm record-type-satisfied-when-atom
  ;;          (implies (atom fields)
  ;;                   (iff (record-type-satisfied x fields)
  ;;                        (not (val-imap-fix x))))
  ;;          :hints(("Goal"
  ;;                  :induct (len x)
  ;;                  :expand ((record-type-satisfied x fields)
  ;;                           (val-imap-fix x))))))

  ;; (local (defthm record-type-satisfied-when-x-atom
  ;;          (implies (atom (Val-imap-fix x))
  ;;                   (iff (record-type-satisfied x fields)
  ;;                        (atom fields)))
  ;;          :hints(("Goal"
  ;;                  :induct (len x)
  ;;                  :expand ((record-type-satisfied x fields)
  ;;                           (val-imap-fix x))))))


  ;; ;; (local (defthm nthcdr-of-len
  ;; ;;          (atom (nthcdr (len x) x))
  ;; ;;          :rule-classes :type-prescription))

  ;; (local (defun record-type-satisfied-ind (x fields)
  ;;          (if (atom x)
  ;;              fields
  ;;            (if (and (consp (car x))
  ;;                     (identifier-p (caar x)))
  ;;                (record-type-satisfied-ind (cdr x) (cdr fields))
  ;;              (record-type-satisfied-ind (cdr x) fields)))))

  ;; (local (defthm lookup-when-record-type-satisfied
  ;;          (implies (and (record-type-satisfied x fields)
  ;;                        (consp fields))
  ;;                   (and (hons-assoc-equal (typed_identifier->name (car fields)) x)
  ;;                        (val-equiv (cdr (hons-assoc-equal (typed_identifier->name (car fields)) x))
  ;;                                   (cdar (val-imap-fix x)))))
  ;;          :hints(("Goal" :in-theory (enable record-type-satisfied)
  ;;                  :expand ((val-imap-fix x))
  ;;                  :induct (record-type-satisfied-ind x fields)))))
           
  
  ;; (local (defthm record-type-satisfied-implies-ty-satisfied
  ;;          (implies (and (record-type-satisfied x fields)
  ;;                        (consp fields))
  ;;                   (ty-satisfied (cdar (val-imap-fix x))
  ;;                                 (typed_identifier->type (car fields))))
  ;;          :hints(("Goal" :in-theory (enable record-type-satisfied
  ;;                                            val-imap-fix)
  ;;                  :induct (record-type-satisfied-ind x fields)))))

  ;; (local (defthm record-type-satisfied-implies-remove1-assoc-equal
  ;;          (implies (and (record-type-satisfied x fields)
  ;;                        (consp fields))
  ;;                   (equal (remove1-assoc-equal (typed_identifier->name (car fields))
  ;;                                               (val-imap-fix x))
  ;;                          (cdr (val-imap-fix x))))
  ;;          :hints(("Goal" :in-theory (enable record-type-satisfied
  ;;                                            val-imap-fix)
  ;;                  :induct (record-type-satisfied-ind x fields)))))

  ;; (local (defthm record-type-satisfied-implies-cdr
  ;;          (implies (and (record-type-satisfied x fields)
  ;;                        (consp fields))
  ;;                   (record-type-satisfied (cdr (val-imap-fix x)) (cdr fields)))
  ;;          :hints(("Goal" :in-theory (enable record-type-satisfied
  ;;                                            val-imap-fix)
  ;;                  :induct (record-type-satisfied-ind x fields)))))

  ;; (local (defthm record-type-satisfied-implies-consp
  ;;          (implies (and (record-type-satisfied x fields)
  ;;                        (consp fields))
  ;;                   (and (consp (val-imap-fix x))
  ;;                        (equal (caar (val-imap-fix x))
  ;;                               (typed_identifier->name (car fields)))))
  ;;          :hints(("Goal" :in-theory (enable record-type-satisfied
  ;;                                            val-imap-fix)
  ;;                  :induct (record-type-satisfied-ind x fields)))))

  ;; (local (defthm equal-of-cons
  ;;          (equal (equal (cons a b) c)
  ;;                 (and (consp c)
  ;;                      (Equal (car c) a)
  ;;                      (equal (cdr c) b)))))

  
  
  (local (defthm ty-satisfied-of-lookup-when-array-type-satisfied
           (implies (and (array-type-satisfied (omap::values x) ty)
                         (member-equal k (omap::keys x)))
                    (ty-satisfied (omap::lookup k x) ty))))

  (local (defthm ty-satisfied-of-cdr-assoc-when-array-type-satisfied
           (implies (and (array-type-satisfied (omap::values x) ty)
                         (member-equal k (omap::keys x)))
                    (ty-satisfied (cdr (omap::assoc k x)) ty))
           :hints (("Goal" :use ty-satisfied-of-lookup-when-array-type-satisfied
                    :in-theory (e/d (omap::lookup)
                                    (ty-satisfied-of-lookup-when-array-type-satisfied))))))
  
  
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
      (implies (and (array-type-satisfied (omap::values (val-imap-fix x)) ty)
                    (subsetp (identifierlist-fix keys) (omap::keys (val-imap-fix x))))
               (equal new-x (omap::restrict (mergesort (identifierlist-fix keys))
                                            (val-imap-fix x))))
      :hints ('(:expand ((array-type-satisfied x ty)
                         (identifierlist-fix keys)
                         (:free (a b) (mergesort (cons a b)))
                         <call>
                         (:free (x ty) (array-type-fix-val 0 x ty)))
                :in-theory (disable INSERT-IDENTIFIER-FIX-MERGESORT)))
      :fn enumarray-type-fix-val)
    (defret <fn>-when-satisfied-aux
      (implies (record-type-satisfied x fields)
               (equal new-x
                      (omap::restrict (mergesort (typed_identifierlist->names fields))
                                      (val-imap-fix x))))
      :hints ('(:expand ((record-type-satisfied x fields)
                         ;; (val-imap-fix x)
                         (typed_identifierlist->names fields)
                         (:free (a b) (mergesort (cons a b)))
                         <call>)
                :in-theory (enable omap::restrict-of-insert-split
                                   omap::lookup)
                :do-not-induct t))
      :fn record-type-fix-val))
  
  (verify-guards ty-fix-val
    :hints (("goal" :expand ((ty-satisfied x ty)
                             (tuple-type-satisfied x types)
                             (record-type-satisfied x nil)
                             (record-type-satisfied x fields)
                             (array-type-satisfied x ty)
                             (tuple-type-satisfied nil types)
                             (record-type-satisfied nil fields)
                             (typed_identifierlist->names fields))
             :in-theory (enable loghead* omap::lookup)
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
    (b* ((pos (ty->pos_start x))
         (ty (ty->desc x)))
      (type_desc-case ty
        :t_tuple (b* (((ev tys) (name-resolve-tylist env ty.types)))
                   (ev_normal (ty (t_tuple tys) pos)))
        :t_array (b* (((ev base) (name-resolve-ty env ty.type)))
                   (ev_normal (ty (t_array ty.index base) pos)))
        :t_record (b* (((ev fields)
                        (name-resolve-typed_identifierlist env ty.fields)))
                    (ev_normal (ty (t_record fields) pos)))
        :t_exception (b* (((ev fields)
                           (name-resolve-typed_identifierlist env ty.fields)))
                       (ev_normal (ty (t_exception fields) pos)))
        :t_collection (b* (((ev fields)
                            (name-resolve-typed_identifierlist env ty.fields)))
                        (ev_normal (ty (t_collection fields) pos)))
        :t_named  (b* ((decl_types (static_env_global->declared_types env))
                       (look (hons-assoc-equal ty.name decl_types))
                       ((unless look)
                        (ev_error "Named type not found" x (list pos)))
                       ((when (zp clk))
                        (ev_error "Clock ran out resolving named type" x (list pos)))
                       (type (ty-timeframe->ty (cdr look))))
                    (name-resolve-ty env type :clk (1- clk)))
        :otherwise (ev_normal (ty ty pos)))))
  
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



(defines ty-remove-bitfields
  :verify-guards nil
    
  (define ty-remove-bitfields ((x ty-p))
    :returns (res ty-p)
    :measure (ty-count x)
    (b* ((pos (ty->pos_start x))
         (ty (ty->desc x)))
      (type_desc-case ty
        :t_bits (ty (change-t_bits ty :fields nil) pos)
        :t_tuple (ty (t_tuple (tylist-remove-bitfields ty.types)) pos)
        :t_array (ty (t_array ty.index (ty-remove-bitfields ty.type)) pos)
        :t_record (ty (t_record (typed_identifierlist-remove-bitfields ty.fields)) pos)
        :t_exception (ty (t_exception (typed_identifierlist-remove-bitfields ty.fields)) pos)
        :t_collection (ty (t_collection (typed_identifierlist-remove-bitfields ty.fields)) pos)
        :otherwise (ty-fix x))))
  
  (define tylist-remove-bitfields ((x tylist-p))
    :returns (res tylist-p)
    :measure (tylist-count x)
    (if (atom x)
        nil
      (cons (ty-remove-bitfields (car x))
            (tylist-remove-bitfields (cdr x)))))

  (define typed_identifierlist-remove-bitfields ((x typed_identifierlist-p))
    :returns (res typed_identifierlist-p)
    :measure (typed_identifierlist-count x)
    (b* (((when (atom x)) nil)
         ((typed_identifier x1) (car x)))
      (cons (typed_identifier x1.name (ty-remove-bitfields x1.type))
            (typed_identifierlist-remove-bitfields (cdr x)))))
  ///
  (Verify-guards ty-remove-bitfields))


(define typed_identifierlist-lookup ((name identifier-p)
                                     (x typed_identifierlist-p))
  :returns (ty maybe-ty-p)
  (if (atom x)
      nil
    (if (equal (typed_identifier->name (car x)) (identifier-fix name))
        (typed_identifier->type (car x))
      (typed_identifierlist-lookup name (cdr x))))
  ///
  (defret <fn>-member
    (implies ty
             (member-equal (typed_identifier name ty) (typed_identifierlist-fix x))))

  (defret <fn>-under-iff
    (iff ty
         (member-equal (identifier-fix name) (typed_identifierlist->names x)))
    :hints(("Goal" :in-theory (enable typed_identifierlist->names))))

  (defret count-of-<fn>
    (implies ty
             (< (ty-count ty)
                (typed_identifierlist-count x)))
    :hints (("goal" :induct <call>
             :expand (<call>
                      (typed_identifierlist->names x)
                      (typed_identifierlist-count x))))
    :rule-classes :linear)

  (defret ty-resolved-p-of-<fn>
    (implies (and (typed_identifierlist-resolved-p x)
                  ty)
             (ty-resolved-p ty))
    :hints(("Goal" :induct <call>
            :expand (<call>
                     (typed_identifierlist-resolved-p x))))))


(defthmd record-type-satisfied-implies-lookup
  (implies (and (record-type-satisfied x fields)
                (identifier-p name)
                (member-equal name (typed_identifierlist->names fields)))
           (and (omap::assoc name (val-imap-fix x))
                (ty-satisfied (cdr (omap::assoc name (val-imap-fix x)))
                              (typed_identifierlist-lookup name fields))))
  :hints(("Goal" :in-theory (enable (:i typed_identifierlist-lookup))
          :induct (typed_identifierlist-lookup name fields)
          :expand ((record-type-satisfied x fields)
                   (typed_identifierlist->names fields)
                   (typed_identifierlist-lookup name fields)))))

(define record-type-satisfied-witness ((x val-imap-p)
                                       (fields typed_identifierlist-p))
  :returns (name identifier-p)
  :guard (typed_identifierlist-resolved-p fields)
  (b* (((when (atom fields)) "")
       ((typed_identifier f1) (car fields))
       (look (omap::assoc f1.name (val-imap-fix x)))
       ((unless look) f1.name)
       (val (cdr look)))
    (if (ty-satisfied val f1.type)
        (record-type-satisfied-witness x (cdr fields))
      f1.name))
  ///
  (defretd <fn>-when-not-record-type-satisfied
    (implies (and (not (record-type-satisfied x fields))
                  (no-duplicatesp-equal (typed_identifierlist->names fields)))
             (and (member-equal name (typed_identifierlist->names fields))
                  (implies (omap::assoc name (val-imap-fix x))
                           (not (ty-satisfied (cdr (omap::assoc name (val-imap-fix x)))
                                              (typed_identifierlist-lookup name fields))))))
  :hints(("Goal" 
          :induct <call>
          :expand ((record-type-satisfied x fields)
                   (typed_identifierlist->names fields)
                   (record-type-satisfied-witness x fields)
                   (:free (name) (typed_identifierlist-lookup name fields))))))

  (defretd record-type-satisfied-iff-<fn>
    (implies (and (acl2::rewriting-positive-literal `(record-type-satisfied ,x ,fields))
                  (no-duplicatesp-equal (typed_identifierlist->names fields)))
             (iff (record-type-satisfied x fields)
                  (not
                   (and (member-equal name (typed_identifierlist->names fields))
                        (implies (omap::assoc name (val-imap-fix x))
                                 (not (ty-satisfied (cdr (omap::assoc name (val-imap-fix x)))
                                                    (typed_identifierlist-lookup name fields))))))))
    :hints (("goal" :in-theory (e/d (record-type-satisfied-implies-lookup)
                                    (<fn>))
             :use <fn>-when-not-record-type-satisfied))))
    
                  
(defconst *fake-posn* (posn "" 0 0 0))

(define int-literal-expr-normalize ((x expr-p))
  :guard (int-literal-expr-p x)
  :returns (new-x expr-p)
  :prepwork ((local (in-theory (enable int-literal-expr-p
                                       int-literal-expr->val))))
  (if (mbt (int-literal-expr-p x))
      (expr (e_literal (l_int (int-literal-expr->val x))) *fake-posn*)
    (expr-fix x))
  ///
  (defret int-literal-expr-p-of-<fn>
    (iff (int-literal-expr-p new-x)
         (int-literal-expr-p x)))

  (defret int-literal-expr->val-of-<fn>
    (equal (int-literal-expr->val new-x)
           (int-literal-expr->val x))))

(define int_constraint-normalize ((x int_constraint-p))
  :guard (int_constraint-resolved-p x)
  :returns (new-x int_constraint-p)
  :prepwork ((local (in-theory (enable int_constraint-resolved-p))))
  (if (mbt (int_constraint-resolved-p x))
      (int_constraint-case x
        :constraint_exact (constraint_exact (int-literal-expr-normalize x.val))
        :constraint_range (constraint_range (int-literal-expr-normalize x.from)
                                            (int-literal-expr-normalize x.to)))
    (int_constraint-fix x))
  ///
  (defret int_constraint-resolved-p-of-<fn>
    (iff (int_constraint-resolved-p new-x)
         (int_constraint-resolved-p x)))

  (defret int_constraint-satisfied-of-<fn>
    (iff (int_constraint-satisfied v new-x)
         (int_constraint-satisfied v x))
    :hints(("Goal" :in-theory (enable int_constraint-satisfied))))

  (defret int_constraint-value-fix-of-<fn>
    (equal (int_constraint-value-fix v new-x)
           (int_constraint-value-fix v x))
    :hints(("Goal" :in-theory (enable int_constraint-value-fix)))))

(define int_constraintlist-normalize ((x int_constraintlist-p))
  :guard (int_constraintlist-resolved-p x)
  :returns (new-x int_constraintlist-p)
  :prepwork ((local (in-theory (enable int_constraintlist-resolved-p))))
  (if (atom x)
      nil
    (cons (int_constraint-normalize (car x))
          (int_constraintlist-normalize (cdr x))))
  ///
  (defret int_constraintlist-resolved-p-of-<fn>
    (iff (int_constraintlist-resolved-p new-x)
         (int_constraintlist-resolved-p x)))
  
  (defret int_constraintlist-satisfied-of-<fn>
    (iff (int_constraintlist-satisfied v new-x)
         (int_constraintlist-satisfied v x))
    :hints(("Goal" :in-theory (enable int_constraintlist-satisfied))))
  
  (defret int_constraintlist-value-fix-of-<fn>
    (equal (int_constraintlist-value-fix v new-x)
           (int_constraintlist-value-fix v x))
    :hints(("Goal" :in-theory (enable int_constraintlist-value-fix)))))

(define constraint_kind-normalize ((x constraint_kind-p))
  :guard (constraint_kind-resolved-p x)
  :returns (new-x constraint_kind-p)
  :prepwork ((local (in-theory (enable constraint_kind-resolved-p))))
  (constraint_kind-case x
    :unconstrained (unconstrained)
    :wellconstrained (change-wellconstrained x :constraints (int_constraintlist-normalize x.constraints))
    :otherwise (constraint_kind-fix x))
  ///
  (defret constraint_kind-resolved-p-of-<fn>
    (iff (constraint_kind-resolved-p new-x)
         (constraint_kind-resolved-p x)))

  (defret constraint_kind-satisfied-of-<fn>
    (iff (constraint_kind-satisfied v new-x)
         (constraint_kind-satisfied v x))
    :hints(("Goal" :in-theory (enable constraint_kind-satisfied))))

  (defret constraint_kind-value-fix-of-<fn>
    (equal (constraint_kind-value-fix v new-x)
           (constraint_kind-value-fix v x))
    :hints(("Goal" :in-theory (enable constraint_kind-value-fix)))))


(defines ty-normalize
  :flag-local nil
  :ruler-extenders :all
  :verify-guards nil
  (define ty-normalize ((x ty-p))
    :guard (ty-resolved-p x)
    :measure (acl2::two-nats-measure (ty-count x) 0)
    :returns (new-x ty-p)
    (b* ((x (ty->desc x)))
      (ty
       (type_desc-case x
         :t_int (t_int (constraint_kind-normalize x.constraint))
         :t_bits (t_bits (int-literal-expr-normalize x.expr) nil)
         :t_enum (t_enum (mergesort x.elts))
         :t_tuple (t_tuple (tuple-type-normalize x.types))
         :t_array (array_index-case x.index
                    :arraylength_expr (t_array (arraylength_expr (int-literal-expr-normalize x.index.length))
                                               (ty-normalize x.type))
                    :arraylength_enum (t_array (arraylength_enum x.index.name
                                                                 (mergesort x.index.elts))
                                               (if (consp x.index.elts)
                                                   (ty-normalize x.type)
                                                 ;; unimportant what the type is
                                                 (change-ty x.type
                                                            :desc (t_bool)))))
         :t_record
         (t_record
          (record-type-normalize (mergesort (typed_identifierlist->names x.fields))
                                 x.fields))
         :t_exception
         (t_exception
          (record-type-normalize (mergesort (typed_identifierlist->names x.fields))
                                 x.fields))
         :t_collection
         (t_collection
          (record-type-normalize (mergesort (typed_identifierlist->names x.fields))
                                 x.fields))
         :otherwise (type_desc-fix x))
       *fake-posn*)))

  (define tuple-type-normalize ((x tylist-p))
    :guard (tylist-resolved-p x)
    :measure (acl2::two-nats-measure (tylist-count x) 0)
    :returns (new-x tylist-p)
    (if (atom x)
        nil
      (cons (ty-normalize (car x))
            (tuple-type-normalize (cdr x)))))

  (define record-type-normalize ((keys identifierlist-p) (fields typed_identifierlist-p))
    :measure (acl2::two-nats-measure (typed_identifierlist-count fields) (len keys))
    :guard (and (subsetp-equal keys (typed_identifierlist->names fields))
                (typed_identifierlist-resolved-p fields))
    :returns (new-fields typed_identifierlist-p)
    (b* (((when (atom keys))
          nil)
         (ty (typed_identifierlist-lookup (car keys) fields)))
      (if (mbt (and ty t))
          (cons (typed_identifier (car keys) (ty-normalize ty))
                (record-type-normalize (cdr keys) fields))
        (record-type-normalize (cdr keys) fields))))
  ///
  (verify-guards ty-normalize)
  

  (local (defthm key-ord-values-whe-emptyp
           (implies (omap::emptyp x)
                    (equal (omap::key-ord-values x) nil))
           :hints(("Goal" :in-theory (enable omap::key-ord-values)))))
  
  (local (defthmd not-record-type-satisfied-when-not-lookup
           (implies (and (bind-free `((name . (TYPED_IDENTIFIER->NAME$INLINE (CAR FIELDS))))
                                    (name))
                         (not (omap::assoc name (val-imap-fix x)))
                         (identifier-p name)
                         (member-equal name (typed_identifierlist->names fields)))
                    (not (record-type-satisfied x fields)))
           :hints(("Goal" :in-theory (enable record-type-satisfied-implies-lookup)))))

  (local (defthmd not-record-type-satisfied-when-not-satisfied
           (implies (and (bind-free `((name . (TYPED_IDENTIFIER->NAME$INLINE (CAR FIELDS))))
                                    (name))
                         (not (ty-satisfied (cdr (omap::assoc name (val-imap-fix x)))
                                            (typed_identifierlist-lookup name fields)))
                         (identifier-p name)
                         (member-equal name (typed_identifierlist->names fields)))
                    (not (record-type-satisfied x fields)))
           :hints(("Goal" :in-theory (enable record-type-satisfied-implies-lookup)))))

  (defthm typed_identifierlist->names-of-record-type-normalize
    (equal (typed_identifierlist->names (record-type-normalize keys fields))
           (intersection-equal (identifierlist-fix keys)
                               (typed_identifierlist->names fields)))
    :hints(("Goal" :in-theory (enable typed_identifierlist->names)
            :induct (len keys)
            :expand ((record-type-normalize keys fields)))))
  
  (defthm typed_identifierlist-lookup-of-record-type-normalize
    (equal (typed_identifierlist-lookup key (record-type-normalize keys fields))
           (and (member-equal (identifier-fix key) (identifierlist-fix keys))
                (member-equal (identifier-fix key) (typed_identifierlist->names fields))
                (ty-normalize (typed_identifierlist-lookup key fields))))
    :hints(("Goal" :in-theory (enable typed_identifierlist-lookup
                                      typed_identifierlist->names)
            :induct (len keys)
            :expand ((typed_identifierlist-lookup key fields)
                     (record-type-normalize keys fields)))))
                     
                                      
  (local (defthm member-of-mergesort
           (iff (member-equal k (mergesort x))
                (member-equal k x))))

  (local (defthm no-duplicates-of-intersection
           (implies (no-duplicatesp-equal a)
                    (no-duplicatesp-equal (intersection-equal a b)))
           :hints(("Goal" :in-theory (enable no-duplicatesp-equal intersection-equal)))))

  (local (defthm not-member-when-setp-<<
           (implies (and (<< a (car b))
                         (setp b))
                    (not (member a b)))
           :hints(("Goal" :in-theory (enable setp)))))

  (local (defthm not-member-car-cdr-when-setp
           (implies (setp b)
                    (not (member (car b) (cdr b))))
           :hints(("Goal" :expand ((setp b))))))

  (local (defthm no-duplicatesp-when-setp
           (implies (setp y)
                    (no-duplicatesp y))
           :hints(("Goal" :in-theory (enable setp)))))
  
  (local (defthm record-type-satisfied-of-normalize-mergesort-cons
           (implies (and (identifier-p a)
                         (identifierlist-p b))
                    (iff (record-type-satisfied x (record-type-normalize
                                                   (mergesort (cons a b)) fields))
                  
                         (and (or (not (typed_identifierlist-lookup a fields))
                                  (and (omap::assoc (identifier-fix a) (val-imap-fix x))
                                       (ty-satisfied (cdr (omap::assoc (identifier-fix a) (val-imap-fix x)))
                                                     (ty-normalize (typed_identifierlist-lookup a fields)))))
                              (record-type-satisfied x (record-type-normalize (mergesort b) fields)))))
           :hints(("Goal" :in-theory (e/d ()
                                          (identifier-p-when-assoc-val-imap-p-binds-free-x)))
                  (and stable-under-simplificationp
                       (b* ((lit (assoc 'record-type-satisfied clause))
                            (arg (second (second (third lit))))
                            (other (if (eq arg 'b) '(cons a b) 'b)))
                         (if lit
                             `(:in-theory (e/d (record-type-satisfied-iff-record-type-satisfied-witness)
                                               (identifier-p-when-assoc-val-imap-p-binds-free-x))
                               :use ((:instance record-type-satisfied-implies-lookup
                                      (fields (record-type-normalize (mergesort ,other) fields))
                                      (name (record-type-satisfied-witness
                                             x (record-type-normalize (mergesort ,arg) fields))))))
                           `(:use ((:instance record-type-satisfied-implies-lookup
                                    (fields (record-type-normalize (mergesort (cons a b)) fields))
                                    (name a))))))))))

  (local (defthmd record-type-normalize-of-cons-non-member
           (implies (not (member-equal (typed_identifier->name (car fields))
                                       (identifierlist-fix keys)))
                    (equal (record-type-normalize keys fields)
                           (record-type-normalize keys (cdr fields))))
           :hints (("goal" :induct (len keys)
                    :expand ((:free (fields) (record-type-normalize keys fields))
                             (typed_identifierlist->names fields)
                             (:free (key) (typed_identifierlist-lookup key fields)))))))

  (local (defthm intersection-equal-when-subsetp
           (implies (subsetp x y)
                    (equal (intersection-equal x y)
                           (true-list-fix x)))))
  
  (local (defthm intersection-equal-self
           (equal (intersection-equal x x)
                  (true-list-fix x))
           :hints(("Goal" :in-theory (enable intersection-equal)))))

  (local (defthm intersection-identity
           (implies (equal keys (mergesort names))
                    (equal (mergesort (intersection-equal keys names)) keys))))
  
  (defthm-ty-satisfied-flag
    (defthm ty-satisfied-of-ty-normalize
      (implies (ty-satisfiable ty)
               (iff (ty-satisfied x (ty-normalize ty))
                    (ty-satisfied x ty)))
      :hints ('(:expand ((ty-normalize ty)
                         (ty-satisfiable ty)
                         (:free (ty) (ty-satisfied x ty))
                         (:free (ty) (array-type-satisfied nil ty)))))
      :flag ty-satisfied)
    (defthm tuple-type-satisfied-of-tuple-type-normalize
      (implies (tylist-satisfiable types)
               (iff (tuple-type-satisfied x (tuple-type-normalize types))
                    (tuple-type-satisfied x types)))
      :hints ('(:expand ((tuple-type-normalize types)
                         (tylist-satisfiable types)
                         (:free (ty) (tuple-type-satisfied x ty)))))
      :flag tuple-type-satisfied)

    (defthm array-type-satisfied-of-ty-normalize
      (implies (ty-satisfiable ty)
               (iff (array-type-satisfied x (ty-normalize ty))
                    (array-type-satisfied x ty)))
      :hints ('(:expand ((:free (ty) (array-type-satisfied x ty)))))
      :flag array-type-satisfied)

    (defthm record-type-satisfied-of-record-type-normalize
      (implies (and (typed_identifierlist-satisfiable fields)
                    (no-duplicatesp-equal (typed_identifierlist->names fields)))
               (iff (record-type-satisfied x (record-type-normalize (mergesort
                                                                     (typed_identifierlist->names fields))
                                                                    fields))
                    (record-type-satisfied x fields)))
      :hints ('(:expand ((record-type-satisfied x fields)
                         (record-type-satisfied x nil)
                         (typed_identifierlist-satisfiable fields)
                         (typed_identifierlist->names fields)
                         (:free (key) (typed_identifierlist-lookup key fields)))
                :in-theory (enable record-type-normalize-of-cons-non-member)
                ;; :in-theory (enable not-record-type-satisfied-when-not-lookup
                ;;                    not-record-type-satisfied-when-not-satisfied)
                ))
      :flag record-type-satisfied))

  (defthm ty-satisfiable-of-ty-normalize
    (implies (ty-satisfiable x)
             (ty-satisfiable (ty-normalize x)))
    :hints (("goal" :use ((:instance ty-satisfying-val-correct)
                          (:instance ty-satisfying-val-sufficient
                           (ty (ty-normalize x))
                           (x (ty-satisfying-val x))))
             :in-theory (disable ty-satisfying-val-correct
                                 ty-satisfying-val-sufficient))))

  (local (defthm ty-satisfiable-by-satisfied
           (implies (ty-satisfied x ty)
                    (ty-satisfiable ty))
           :hints (("goal" :use ((:instance ty-satisfying-val-sufficient))
                    :in-theory (disable ty-satisfying-val-sufficient)))))
  
  (defthm ty-fix-val-of-ty-normalize
    (implies (ty-satisfied x ty)
             (equal (ty-fix-val x (ty-normalize ty))
                    (val-fix x)))
    :hints (("goal" :use ((:instance ty-fix-val-when-satisfied
                           (ty (ty-normalize ty))
                           (x (ty-fix-val x ty)))
                          (:instance ty-fix-val-when-satisfied))
             :in-theory (disable ty-fix-val-when-satisfied)))))



(defines ty-norm-posns
  :flag-local nil
  :ruler-extenders :all
  :verify-guards nil
  (define ty-norm-posns ((x ty-p))
    :guard (ty-resolved-p x)
    :measure (ty-count x)
    :returns (new-x ty-p)
    (b* ((x (ty->desc x)))
      (ty
       (type_desc-case x
         :t_int (t_int (constraint_kind-normalize x.constraint))
         :t_bits (t_bits (int-literal-expr-normalize x.expr) nil)
         :t_tuple (t_tuple (tuple-type-norm-posns x.types))
         :t_array (t_array (array_index-case x.index
                             :arraylength_expr (arraylength_expr (int-literal-expr-normalize x.index.length))
                             :arraylength_enum x.index)
                           (ty-norm-posns x.type))
         :t_record
         (t_record
          (record-type-norm-posns x.fields))
         :t_exception
         (t_exception
          (record-type-norm-posns x.fields))
         :t_collection
         (t_collection
          (record-type-norm-posns x.fields))
         :otherwise (type_desc-fix x))
       *fake-posn*)))

  (define tuple-type-norm-posns ((x tylist-p))
    :guard (tylist-resolved-p x)
    :measure (tylist-count x)
    :returns (new-x tylist-p)
    (if (atom x)
        nil
      (cons (ty-norm-posns (car x))
            (tuple-type-norm-posns (cdr x)))))

  (define record-type-norm-posns ((fields typed_identifierlist-p))
    :measure (typed_identifierlist-count fields)
    :guard (typed_identifierlist-resolved-p fields)
    :returns (new-fields typed_identifierlist-p)
    (b* (((when (atom fields))
          nil)
         ((typed_identifier f1) (car fields)))
      (cons (typed_identifier f1.name (ty-norm-posns f1.type))
            (record-type-norm-posns (cdr fields)))))
  ///
  (std::defret-mutual ty-fix-val-of-ty-norm-posns
    (defret <fn>-of-ty-norm-posns
      (equal (ty-fix-val x (ty-norm-posns ty))
             new-x)
      :hints ('(:expand ((:free (ty) <call>)
                         (ty-norm-posns ty))))
      :fn ty-fix-val)
    (defret <fn>-of-ty-norm-posns
      (equal (tuple-type-fix-val x (tuple-type-norm-posns types))
             new-x)
      :hints ('(:expand ((:free (types) <call>)
                         (tuple-type-norm-posns types))))
      :fn tuple-type-fix-val)
    (defret <fn>-of-ty-norm-posns
      (equal (array-type-fix-val len x (ty-norm-posns ty))
             new-x)
      :hints ('(:expand ((:free (len ty) <call>))))
      :fn array-type-fix-val)
    (defret <fn>-of-ty-norm-posns
      (equal (enumarray-type-fix-val keys x (ty-norm-posns ty))
             new-x)
      :hints ('(:expand ((:free (ty) <call>))))
      :fn enumarray-type-fix-val)
    (defret <fn>-of-ty-norm-posns
      (equal (record-type-fix-val x (record-type-norm-posns fields))
             new-x)
      :hints ('(:expand (<call>
                         (record-type-fix-val x nil)
                         (:free (a b) (record-type-fix-val x (cons a b)))
                         (record-type-norm-posns fields))))
      :fn record-type-fix-val)
    :mutual-recursion ty-fix-val)

  (defthm typed_identifierlist->names-of-record-type-norm-posns
    (equal (typed_identifierlist->names (record-type-norm-posns fields))
           (typed_identifierlist->names fields))
    :hints(("Goal" :in-theory (enable typed_identifierlist->names)
            :induct t
            :expand ((record-type-norm-posns fields)))))
  
  (defthm-ty-satisfied-flag ty-satisfied-of-ty-norm-posns
    (defthm ty-satisfied-of-ty-norm-posns
      (iff (ty-satisfied x (ty-norm-posns ty))
           (ty-satisfied x ty))
      :hints ('(:expand ((:free (ty) (ty-satisfied x ty))
                         (ty-norm-posns ty))))
      :flag ty-satisfied)
    (defthm tuple-type-satisfied-of-ty-norm-posns
      (iff (tuple-type-satisfied x (tuple-type-norm-posns types))
           (tuple-type-satisfied x types))
      :hints ('(:expand ((:free (types) (tuple-type-satisfied x types))
                         (tuple-type-norm-posns types))))
      :flag tuple-type-satisfied)
    (defthm array-type-satisfied-of-ty-norm-posns
      (iff (array-type-satisfied x (ty-norm-posns ty))
           (array-type-satisfied x ty))
      :hints ('(:expand ((:free ( ty) (array-type-satisfied x ty)))))
      :flag array-type-satisfied)
    (defthm record-type-satisfied-of-ty-norm-posns
      (iff (record-type-satisfied x (record-type-norm-posns fields))
           (record-type-satisfied x fields))
      :hints ('(:expand ((record-type-satisfied x fields)
                         (record-type-satisfied x nil)
                         (:free (a b) (record-type-satisfied x (cons a b)))
                         (record-type-norm-posns fields))))
      :flag record-type-satisfied)))
  

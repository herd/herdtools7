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

(include-book "ty-utils")
(include-book "utils/oracle")
(include-book "std/stobjs/clone" :dir :system)
(include-book "std/lists/index-of" :dir :system)
(local (include-book "std/stobjs/absstobjs" :dir :system))
(local (include-book "std/lists/repeat" :dir :system))
(local (include-book "centaur/misc/arith-equivs" :dir :System))
(local (include-book "centaur/bitops/ihsext-basics" :dir :System))
(local (include-book "std/lists/sets" :dir :system))
(local (std::add-default-post-define-hook :fix))

(local (in-theory (disable ifix unsigned-byte-p)))

(stobjs::defstobj-clone orac acl2::orac :pkg asl-pkg)

(defxdoc orac
  :short "Clone of @(see acl2::orac), used for generating arbitrary values of ASL types"
  :long "<p>To support @('e_arbitrary') expressions with full generality, we need a way
of generating arbitrary data. We use the @(see acl2::orac) stobj for this,
wrapping its functions for generating basic typed data with specialized ones
for producing arbitrary data of ASL types.</p>

<p>The function @(see ty-oracle-val) reads a typed value from the oracle,
producing a value that provably satisfies the type (or nil if the type is
unsatisfiable) and a new oracle.</p>

<p>Any value satisfying a type can indeed by produced by @(see
ty-oracle-val). We show this by defining @(see typed-val-to-oracle), which maps
an arbitrary value satisfying a type to data that can be placed in the
@('orac') that will ensure that @(see ty-oracle-val) of that type will produce
that value.</p>

<p>See also @(see e_arbitrary) and @(see eval_expr).</p>")

(define int_constraint-oracle-val ((rel-val natp)
                                 (x int_constraint-p))
  :guard (and (int_constraint-resolved-p x)
              (< rel-val (int_constraint-width x)))
  :returns (val integerp :rule-classes :type-prescription)
  (int_constraint-case x
    :constraint_exact (int-literal-expr->val x.val)
    :constraint_range (b* ((from (int-literal-expr->val x.from)))
                        (+ from (lnfix rel-val))))
  ///
  (defret <fn>-correct
    (implies (and (< (nfix rel-val) (int_constraint-width x)))
             (int_constraint-satisfied val x))
    :hints(("Goal" :in-theory (enable int_constraint-width
                                      int_constraint-satisfied)))))

(define int_constraintlist-oracle-val-aux ((rel-val natp)
                                           (x int_constraintlist-p))
  :guard (and (int_constraintlist-resolved-p x)
              (< rel-val (int_constraintlist-width x)))
  :guard-hints (("goal" :in-theory (enable int_constraintlist-width)))
  :returns (val integerp :rule-classes :type-prescription)
  :measure (len x)
  (if (mbt (consp x))
      (b* ((w1 (int_constraint-width (car x)))
           (rel-val (lnfix rel-val)))
        (if (< rel-val w1)
            (int_constraint-oracle-val rel-val (car x))
          (int_constraintlist-oracle-val-aux (- rel-val w1) (cdr x))))
    0)
  ///
  (defret <fn>-correct
    (implies (and (< (nfix rel-val) (int_constraintlist-width x)))
             (int_constraintlist-satisfied val x))
    :hints(("Goal" :in-theory (enable int_constraintlist-width
                                      int_constraintlist-satisfied)))))



(define int_constraintlist-oracle-val ((x int_constraintlist-p)
                                       orac)
  :guard (int_constraintlist-resolved-p x)
  :returns (mv (val acl2::maybe-integerp :rule-classes :type-prescription)
               new-orac)
  (b* ((width (int_constraintlist-width x))
       ((when (eql width 0)) (mv nil orac))
       ((mv rel-val orac) (acl2::orac-read width orac)))
    (mv (int_constraintlist-oracle-val-aux rel-val x) orac))
  ///
  (defret <fn>-correct
    (implies (int_constraintlist-satisfying-val x)
             (and val
                  (int_constraintlist-satisfied val x)))))


(define constraint_kind-oracle-val ((x constraint_kind-p)
                                    orac)
  :guard (constraint_kind-resolved-p x)
  :returns (mv (val acl2::maybe-integerp :rule-classes :type-prescription)
               new-orac)
  (constraint_kind-case x
    :unconstrained (acl2::orac-read-int orac)
    :wellconstrained (int_constraintlist-oracle-val x.constraints orac)
    :otherwise (mv nil orac))
  ///
  (defret <fn>-correct
    (implies (constraint_kind-satisfying-val x)
             (and val
                  (constraint_kind-satisfied val x)))
    :hints(("Goal" :in-theory (enable constraint_kind-satisfied
                                      constraint_kind-satisfying-val)))))


(local (defthm identifierlist-p-of-insert
         (implies (and (identifierlist-p x)
                       (identifier-p k))
                  (identifierlist-p (insert k x)))
         :hints(("Goal" :in-theory (enable insert
                                           tail emptyp head)))))

(local (defthm identifierlist-p-of-mergesort
         (implies (identifierlist-p x)
                  (identifierlist-p (mergesort x)))
         :hints(("Goal" :in-theory (enable mergesort)))))

(defines ty-oracle-val
  :flag-local nil
  (define ty-oracle-val ((x ty-p) orac)
    :parents (orac)
    :short "Produce an arbitrary value satisfying the given type using values read from the
@(see orac)."
    :long "<p>The following theorem states that this function always produces a
well-typed value if the type is satsifiable:</p> @(def ty-oracle-val-correct)

<p>Additionally, the following theorem states that any value satisfying a type
can be produced by this function if the oracle is set up appropriately (see
@(see typed-val-to-oracle)):</p> @(def typed-val-to-oracle-correct)"
    :guard (ty-resolved-p x)
    :verify-guards nil
    :measure (acl2::two-nats-measure (ty-count x) 0)
    :returns (mv (val maybe-val-p) new-orac)
    (b* ((x (ty->desc x)))
      (type_desc-case x
        :t_int (b* (((mv val orac) (constraint_kind-oracle-val x.constraint orac)))
                 (mv (and val (v_int val)) orac))
        :t_bits (b* ((width (int-literal-expr->val x.expr))
                     ((unless (<= 0 width)) (mv nil orac))
                     ((mv val orac) (acl2::orac-read-bits width orac)))
                  (mv (v_bitvector width val) orac))
        :t_real (b* (((mv val orac) (acl2::orac-read-rational orac)))
                  (mv (v_real val) orac))
        :t_string (b* (((mv val orac) (acl2::orac-read-string orac)))
                    (mv (v_string val) orac))
        :t_bool (b* (((mv bit orac) (acl2::orac-read-bits 1 orac)))
                  (mv (v_bool (eql bit 1)) orac))
        :t_enum (b* (((unless (consp x.elts))
                      (mv nil orac))
                     ((mv idx orac) (acl2::orac-read (len x.elts) orac)))
                  (mv (v_label (nth idx x.elts)) orac))
        :t_tuple (b* (((mv ok vals orac) (tylist-oracle-val x.types orac)))
                   (mv (and ok
                            (v_array vals))
                       orac))
        :t_array (array_index-case x.index
                   :arraylength_expr
                   (b* ((len (int-literal-expr->val x.index.length))
                        ((when (<= len 0))
                         (mv (v_array nil) orac))
                        ((mv ok vals orac) (ty-oracle-vals len x.type orac)))
                     (mv (and ok (v_array vals)) orac))
                   :arraylength_enum
                   (if (atom x.index.elts)
                       (mv (v_record nil) orac)
                     (b* ((keys (mergesort x.index.elts))
                          ((mv ok vals orac) (ty-oracle-vals (len keys) x.type orac)))
                       (mv (and ok (v_record (omap::from-lists keys vals)))
                           orac))))
        :t_record (b* (((mv ok vals orac) (typed_identifierlist-oracle-val x.fields orac)))
                    (mv (and ok
                             (v_record vals))
                        orac))
        :t_exception (b* (((mv ok vals orac) (typed_identifierlist-oracle-val x.fields orac)))
                       (mv (and ok
                                (v_record vals))
                           orac))
        :t_collection (b* (((mv ok vals orac) (typed_identifierlist-oracle-val x.fields orac)))
                        (mv (and ok
                                 (v_record vals))
                            orac))
        :t_named (mv nil orac))))

  (define tylist-oracle-val ((x tylist-p) orac)
    :guard (tylist-resolved-p x)
    :measure (acl2::two-nats-measure (tylist-count x) 0)
    :returns (mv ok (val vallist-p) new-orac)
    (b* (((when (atom x)) (mv t nil orac))
         ((mv val1 orac) (ty-oracle-val (car x) orac))
         ((unless val1)
          (mv nil nil orac))
         ((mv ok rest orac) (tylist-oracle-val (cdr x) orac)))
      (mv ok (and ok (cons val1 rest)) orac)))

  (define ty-oracle-vals ((n natp) (x ty-p) orac)
    :guard (ty-resolved-p x)
    :measure (acl2::two-nats-measure (ty-count x) n)
    :returns (mv ok (vals vallist-p) new-orac)
    (b* (((when (zp n)) (mv t nil orac))
         ((mv val1 orac) (ty-oracle-val x orac))
         ((unless val1)
          (mv nil nil orac))
         ((mv ok rest orac) (ty-oracle-vals (1- n) x orac)))
      (mv ok (and ok (cons val1 rest)) orac)))

  (define typed_identifierlist-oracle-val ((x typed_identifierlist-p) orac)
    :guard (typed_identifierlist-resolved-p x)
    :measure (acl2::two-nats-measure (typed_identifierlist-count x) 0)
    :returns (mv ok (val val-imap-p) new-orac)
    (b* (((When (atom x)) (mv t nil orac))
         ((typed_identifier f1) (car x))
         ((mv val1 orac) (ty-oracle-val f1.type orac))
         ((unless val1)
          (mv nil nil orac))
         ((mv ok rest orac) (typed_identifierlist-oracle-val (cdr x) orac)))
      (mv ok (and ok (omap::update f1.name val1 rest)) orac)))
  ///
  (local (defthm len-equal-0
           (equal (equal (len x) 0)
                  (not (consp x)))))

  (local (defthm len-gt-0
           (equal (> (len x) 0)
                  (consp x))))

  (local (defthm nth-of-identifierlist
           (implies (and (identifierlist-p x)
                         (< (nfix n) (len x)))
                    (identifier-p (nth n x)))
           :hints(("Goal" :in-theory (enable nth)))))

  (std::defret-mutual len-of-ty-oracle-vals
    (defret len-of-ty-oracle-vals
      (implies ok
               (equal (len vals) (nfix n)))
      :hints ('(:expand (<call>)))
      :fn ty-oracle-vals)
    :skip-others t)

  (verify-guards ty-oracle-val)


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

  (local (defthm member-of-nth
           (implies (< (nfix n) (len x))
                    (member-equal (nth n x) x))
           :hints(("Goal" :in-theory (enable nth)))))

  (local (defthm true-listp-when-vallist-p-rw
           (implies (vallist-p x)
                    (true-listp x))))
  
  (std::defret-mutual <fn>-correct
    (defret <fn>-correct
      (implies (ty-satisfiable x)
               (and val
                    (ty-satisfied val x)))
      :hints ('(:expand (<call>
                         (ty-satisfiable x)
                         (:free (v) (ty-satisfied v x))
                         (:free (ty) (array-type-satisfied nil ty)))
                :in-theory (enable array-type-satisfied-when-subsetp)))
      :fn ty-oracle-val)
    (defret <fn>-correct
      (implies (tylist-satisfiable x)
               (and ok
                    (tuple-type-satisfied val x)))
      :hints ('(:expand (<call>
                         (tylist-satisfiable x))
                :in-theory (enable tuple-type-satisfied)))
      :fn tylist-oracle-val)
    (defret <fn>-correct
      (implies (ty-satisfiable x)
               (and ok
                    (array-type-satisfied vals x)))
      :hints ('(:expand (<call>
                         (ty-satisfiable x))
                :in-theory (enable array-type-satisfied)))
      :fn ty-oracle-vals)
    (defret <fn>-correct
      (implies (and (typed_identifierlist-satisfiable x)
                    (no-duplicatesp-equal (typed_identifierlist->names x)))
               (and ok
                    (equal (omap::keys val)
                           (mergesort (typed_identifierlist->names x)))
                    (record-type-satisfied val x)))
      :hints ('(:expand (<call>
                         (typed_identifierlist-satisfiable x)
                         (typed_identifierlist->names x)
                         (:free (a b) (mergesort (cons a b))))
                :in-theory (enable record-type-satisfied)))
      :fn typed_identifierlist-oracle-val))

  (fty::deffixequiv-mutual ty-oracle-val))


(define int_constraint-val-to-oracle ((x int_constraint-p)
                                      (val integerp))
  :guard (and (int_constraint-resolved-p x)
              (int_constraint-satisfied val x))
  :guard-hints (("goal" :in-theory (enable int_constraint-satisfied)))
  :returns (orac-offset natp :rule-classes :type-prescription)
  (int_constraint-case x
    :constraint_exact 0
    :constraint_range (b* ((from (int-literal-expr->val x.from)))
                        (lnfix (- (lifix val) from))))
  ///
  (defret <fn>-correct
    (implies (int_constraint-satisfied val x)
             (equal (int_constraint-oracle-val orac-offset x)
                    (ifix val)))
    :hints(("Goal" :in-theory (enable int_constraint-oracle-val
                                      int_constraint-satisfied))))

  (defret <fn>-bound
    (implies (int_constraint-satisfied val x)
             (< orac-offset (int_constraint-width x)))
    :hints(("Goal" :in-theory (enable int_constraint-width
                                      int_constraint-satisfied)))
    :rule-classes :linear))

(define int_constraintlist-val-to-oracle ((x int_constraintlist-p)
                                          (val integerp))
  :guard (and (int_constraintlist-resolved-p x)
              (int_constraintlist-satisfied val x))
  :verify-guards nil
  :returns (orac-offset natp :rule-classes :type-prescription)
  (if (mbt (consp x))
      (if (int_constraint-satisfied val (car x))
          (int_constraint-val-to-oracle (car x) val)
        (+ (int_constraint-width (car x))
           (int_constraintlist-val-to-oracle (cdr X) val)))
    0)
  ///
  (verify-guards int_constraintlist-val-to-oracle
    :hints (("goal" :in-theory (enable int_constraintlist-satisfied))))

  (defret <fn>-correct-aux
    (implies (int_constraintlist-satisfied val x)
             (equal (int_constraintlist-oracle-val-aux orac-offset x)
                    (ifix val)))
    :hints(("Goal" :in-theory (enable int_constraintlist-oracle-val-aux
                                      int_constraintlist-satisfied))))

  (defret <fn>-bound
    (implies (int_constraintlist-satisfied val x)
             (< orac-offset (int_constraintlist-width x)))
    :hints(("Goal" :in-theory (enable int_constraintlist-width
                                      int_constraintlist-satisfied)))
    :rule-classes :linear))




(define constraint_kind-val-to-oracle ((x constraint_kind-p)
                                       (val integerp))
  :guard (and (constraint_kind-resolved-p x)
              (constraint_kind-satisfied val x))
  :guard-hints (("goal" :in-theory (enable constraint_kind-satisfied)))
  :returns (orac-val integerp :rule-classes :type-prescription)
  (constraint_kind-case x
    :unconstrained (lifix val)
    :otherwise (int_constraintlist-val-to-oracle (wellconstrained->constraints x) val))
  ///
  (defret <fn>-correct
    (implies (and (constraint_kind-satisfied val x)
                  (equal (acl2::oracle-mode orac) 3)
                  (equal (car (acl2::oracle-st orac)) orac-val))
             (equal (constraint_kind-oracle-val x orac)
                    (mv (ifix val)
                        (acl2::update-oracle-st (cdr (acl2::oracle-st orac)) orac))))
    :hints(("Goal" :in-theory (enable constraint_kind-oracle-val
                                      constraint_kind-satisfied
                                      int_constraintlist-oracle-val
                                      acl2::orac-read
                                      acl2::orac-read-int
                                      acl2::orac-st-read
                                      acl2::orac-st-read-int)))))



(defines typed-val-to-oracle
  (define typed-val-to-oracle ((x ty-p) (val val-p))
    :short "Produce a list of values that, when placed in the @(see orac)'s @('oracle-st')
field and its mode set appropriately, makes @('(ty-oracle-val x orac)') produce
@('val')."
    :long "<p>Correctness theorem:</p>@(def typed-val-to-oracle-correct)"
    :guard (and (ty-resolved-p x)
                (ty-satisfied val x))
    :verify-guards nil
    :measure (acl2::two-nats-measure (ty-count x) 0)
    :returns (orac-st true-listp :rule-classes :type-prescription)
    (b* ((x (ty->desc x)))
      (type_desc-case x
        :t_int (list (constraint_kind-val-to-oracle x.constraint (v_int->val val)))
        :t_bits (list (v_bitvector->val val))
        :t_real (list (v_real->val val))
        :t_string (list (v_string->val val))
        :t_bool (list (bool->bit (v_bool->val val)))
        :t_enum (list (acl2::index-of (v_label->val val) x.elts))
        :t_tuple (typed-vallist-to-oracle x.types (v_array->arr val))
        :t_array (array_index-case x.index
                   :arraylength_expr
                   (typed-val-array-to-oracle x.type (v_array->arr val))
                   :arraylength_enum
                   (typed-val-array-to-oracle x.type (omap::key-ord-values (v_record->rec val))))
        :t_record (typed-val-record-to-oracle x.fields (v_record->rec val))
        :t_exception (typed-val-record-to-oracle x.fields (v_record->rec val))
        :t_collection (typed-val-record-to-oracle x.fields (v_record->rec val))
        :otherwise nil)))

  (define typed-vallist-to-oracle ((x tylist-p) (vals vallist-p))
    :guard (and (tylist-resolved-p x)
                (equal (len x) (len vals))
                (tuple-type-satisfied vals x))
    :measure (acl2::two-nats-measure (tylist-count x) 0)
    :returns (orac-st true-listp :rule-classes :type-prescription)
    (if (atom x)
        nil
      (append (typed-val-to-oracle (car x) (car vals))
              (typed-vallist-to-oracle (cdr x) (cdr vals)))))

  (define typed-val-array-to-oracle ((x ty-p) (vals vallist-p))
    :guard (and (ty-resolved-p x)
                (array-type-satisfied vals x))
    :measure (acl2::two-nats-measure (ty-count x) (len vals))
    :returns (orac-st true-listp :rule-classes :type-prescription)
    (if (atom vals)
        nil
      (append (typed-val-to-oracle x (car vals))
              (typed-val-array-to-oracle x (cdr vals)))))

  (define typed-val-record-to-oracle ((x typed_identifierlist-p) (vals val-imap-p))
    :guard (and (typed_identifierlist-resolved-p x)
                (subsetp-equal (typed_identifierlist->names x) (omap::keys vals))
                (record-type-satisfied vals x))
    :measure (acl2::two-nats-measure (typed_identifierlist-count x) 0)
    :returns (orac-st true-listp :rule-classes :type-prescription)
    (if (atom x)
        nil
      (append (typed-val-to-oracle (typed_identifier->type (car x))
                                   (omap::lookup (typed_identifier->name (car x))
                                                 (val-imap-fix vals)))
              (typed-val-record-to-oracle (cdr x) (val-imap-fix vals)))))
  ///
  (local (defun ind (x y)
           (if (atom x)
               y
             (ind (cdr x) (cdr y)))))
  (local (defthm tuple-type-satisfied-implies-len-equal
           (implies (tuple-type-satisfied x y)
                    (equal (len x) (len y)))
           :hints(("Goal" :in-theory (enable tuple-type-satisfied
                                             len)
                   :expand ((tuple-type-satisfied x y))
                   :induct (ind x y)))
           :rule-classes :forward-chaining))

  ;; (local (defthm vallist-p-of-alist-vals
  ;;          (implies (val-imap-p x)
  ;;                   (vallist-p (acl2::alist-vals x)))
  ;;          :hints(("Goal" :in-theory (enable acl2::alist-vals)))))

  (local (defthm equal-mergesort-implies-set-equiv
           (implies (equal x (mergesort y))
                    (acl2::set-equiv x y))
           :hints(("Goal" :in-theory (disable acl2::set-equiv)))
           :rule-classes :forward-chaining))

  (verify-guards typed-val-to-oracle
    :hints (("goal" :expand ((ty-satisfied val x)
                             (array-type-satisfied vals x)
                             (record-type-satisfied vals x)
                             (tuple-type-satisfied vals x)
                             (typed_identifierlist->names x))
             :in-theory (enable omap::lookup))))

  (local (defthm prefixp-nil
           (acl2::prefixp nil x)
           :hints(("Goal" :in-theory (enable acl2::prefixp)))))

  ;; (local (defthm val-imap-fix-when-not-consp
  ;;          (implies (not (consp x))
  ;;                   (not (val-imap-fix x)))
  ;;          :hints(("Goal" :in-theory (enable val-imap-fix)))))

  (defun-nx nthcdr-orac (n orac)
    (if (zp n)
        orac
      (acl2::update-oracle-st (nthcdr n (acl2::oracle-st orac)) orac)))

  ;; (local (defthm-ty-satisfied-flag
  ;;          (defthm record-type-satisfied-when-not-consp
  ;;            (implies (and (val-imap-fix x)
  ;;                          (not (consp fields)))
  ;;                     (not (record-type-satisfied x fields)))
  ;;            :hints ('(:expand ((record-type-satisfied x fields)
  ;;                               (val-imap-fix x))))
  ;;            :flag record-type-satisfied)
  ;;          :skip-others t))

  ;; (local (defthm-ty-satisfied-flag
  ;;          (defthm record-type-satisfied-in-terms-of-fix
  ;;            (equal (record-type-satisfied x fields)
  ;;                   (let ((x (val-imap-fix x)))
  ;;                     (if (atom x)
  ;;                         (atom fields)
  ;;                       (and (consp fields)
  ;;                            (b* (((cons key val) (car x))
  ;;                                 ((typed_identifier f1) (car fields)))
  ;;                              (and (equal key f1.name)
  ;;                                   (ty-satisfied val f1.type)
  ;;                                   (record-type-satisfied (cdr x) (cdr fields))))))))
  ;;            :hints ('(:expand ((record-type-satisfied x fields)
  ;;                               (:free (fields) (record-type-satisfied nil fields))
  ;;                               (val-imap-fix x))))
  ;;            :rule-classes ((:definition :controller-alist ((record-type-satisfied t t))))
  ;;            :flag record-type-satisfied)
  ;;          :skip-others t))

  (local (defthm prefixp-of-append
           (equal (acl2::prefixp (append x y) z)
                  (and (acl2::prefixp x z)
                       (acl2::prefixp y (nthcdr (len x) z))))
           :hints(("Goal" :in-theory (enable acl2::prefixp)))))

  (local (defthm len-of-append
           (equal (len (append x y)) (+ (len x) (len y)))))

  (local (defun-sk typed-val-to-oracle-corr-cond (val x)
           (forall orac
                   (b* ((orac-st (typed-val-to-oracle x val)))
                     (implies (and (ty-satisfied val x)
                                   (equal (acl2::oracle-mode orac) 3)
                                   (acl2::prefixp orac-st (acl2::oracle-st orac)))
                              (equal (ty-oracle-val x orac)
                                     (mv (val-fix val)
                                         (nthcdr-orac (len orac-st) orac))))))
           :rewrite :direct))

  (local (defun-sk typed-vallist-to-oracle-corr-cond (vals x)
           (forall orac
                   (b* ((orac-st (typed-vallist-to-oracle x vals)))
                     (implies (and (tuple-type-satisfied vals x)
                                   (equal (acl2::oracle-mode orac) 3)
                                   (acl2::prefixp orac-st (acl2::oracle-st orac)))
                              (equal (tylist-oracle-val x orac)
                                     (mv t
                                         (vallist-fix vals)
                                         (nthcdr-orac (len orac-st) orac))))))
           :rewrite :direct))

  (local (defun-sk typed-val-array-to-oracle-corr-cond (vals x)
           (forall (orac len)
                   (b* ((orac-st (typed-val-array-to-oracle x vals)))
                     (implies (and (array-type-satisfied vals x)
                                   (equal (acl2::oracle-mode orac) 3)
                                   (acl2::prefixp orac-st (acl2::oracle-st orac))
                                   (equal len (len vals)))
                              (equal (ty-oracle-vals len x orac)
                                     (mv t
                                         (vallist-fix vals)
                                         (nthcdr-orac (len orac-st) orac))))))
           :rewrite :direct))

  (local (defun-sk typed-val-record-to-oracle-corr-cond (vals x)
           (forall orac
                   (b* ((orac-st (typed-val-record-to-oracle x vals)))
                     (implies (and (record-type-satisfied vals x)
                                   (equal (acl2::oracle-mode orac) 3)
                                   (acl2::prefixp orac-st (acl2::oracle-st orac)))
                              (equal (typed_identifierlist-oracle-val x orac)
                                     (mv t
                                         (omap::restrict (mergesort (typed_identifierlist->names x)) (val-imap-fix vals))
                                         (nthcdr-orac (len orac-st) orac))))))
           :rewrite :direct))

  (local (in-theory (disable typed-val-to-oracle-corr-cond
                             typed-vallist-to-oracle-corr-cond
                             typed-val-array-to-oracle-corr-cond
                             typed-val-record-to-oracle-corr-cond)))

  ;; (local (defthm consp-val-imap-fix
  ;;          (implies (consp (val-imap-fix x))
  ;;                   (cdar (val-imap-fix x)))
  ;;          :hints(("Goal"
  ;;                  :expand ((val-imap-fix x))
  ;;                  :induct (len x)))
  ;;          :rule-classes :forward-chaining))

  (local
   (defthm-ty-oracle-val-flag
     (defthm booleanp-of-typed_identifierlist-oracle-val
       (booleanp (mv-nth 0 (typed_identifierlist-oracle-val x orac)))
       :hints ('(:expand ((:with typed_identifierlist-oracle-val
                           (typed_identifierlist-oracle-val x orac)))))
       :rule-classes :type-prescription
       :flag typed_identifierlist-oracle-val)
     :skip-others t))

  (local (defthmd equal-of-cons
           (equal (equal (cons x y) z)
                  (and (consp z)
                       (equal x (car z))
                       (equal y (cdr z))))))

  (local (in-theory (disable nth update-nth)))

  (local (defthm update-nth-of-update-nth
           (equal (update-nth n v1 (update-nth n v2 x))
                  (update-nth n v1 x))
           :hints(("Goal" :in-theory (enable update-nth)))))
  (local (defthm nthcdr-of-nthcdr
           (equal (nthcdr n (nthcdr m x))
                  (nthcdr (+ (nfix n) (nfix m)) x))
           :hints(("Goal" :in-theory (enable nthcdr)))))

  (local (defthm consp-alist-keys-when-val-imap-p
           (implies (val-imap-p x)
                    (iff (consp (acl2::alist-keys x))
                         x))
           :hints(("Goal" :in-theory (enable val-imap-p
                                             acl2::alist-keys)))))

  (local (defthm len-alist-vals
           (equal (len (acl2::alist-vals x))
                  (len (acl2::alist-keys x)))
           :hints(("Goal" :in-theory (enable acl2::alist-keys
                                             acl2::alist-vals)))))

  (local (defthm pairlis-keys-vals-when-val-imap-p
           (implies (val-imap-p x)
                    (equal (pairlis$ (acl2::alist-keys x)
                                     (acl2::alist-vals x))
                           x))
           :hints(("Goal" :in-theory (enable val-imap-p
                                             acl2::alist-keys
                                             acl2::alist-vals)))))

  (local (defthm val-fix-equal-v_array
           (equal (equal (val-fix val) '(:v_array nil))
                  (and (equal (val-kind val) :v_array)
                       (not (consp (v_array->arr val)))))
           :hints(("Goal" :in-theory (enable val-fix-when-v_array)))))

  (local (defthm len-equal-0
           (equal (equal (len x) 0)
                  (not (consp x)))))

  (local (defthm prefixp-of-cons
           (equal (acl2::prefixp (cons x y) z)
                  (and (consp z)
                       (equal (car z) x)
                       (acl2::prefixp y (cdr z))))
           :hints(("Goal" :in-theory (enable acl2::prefixp)))))

  (local (defthm car-less-when-equal-index-of
           (implies (and (equal (car x) (acl2::index-of k l))
                         (integerp (car x)))
                    (< (car x) (len l)))
           :rule-classes :forward-chaining))

  (local (defthm len-when-consp
           (implies (consp x)
                    (posp (len x)))
           :rule-classes :type-prescription))
  
  (local (defthm ash-1-posp
           (implies (natp x)
                    (posp (ash 1 x)))
           :hints(("Goal" :in-theory (enable bitops::ash-is-expt-*-x)))
           :rule-classes :type-prescription))

  (local (defthm v_bitvector->val-less-than-ash
           (< (v_bitvector->val x) (ash 1 (v_bitvector->len x)))
           :hints (("goal" :use v_bitvector-requirements
                    :in-theory (e/d (unsigned-byte-p
                                     bitops::ash-is-expt-*-x)
                                    (v_bitvector-requirements))))))

  (local (defthm cdr-assoc-iff-assoc-when-val-imap-p
           (implies (val-imap-p x)
                    (iff (cdr (omap::assoc k x))
                         (omap::assoc k x)))
           :hints (("goal" :use ((:instance val-p-of-cdr-of-assoc-val-imap-p))
                    :in-theory (disable val-p-of-cdr-of-assoc-val-imap-p)))))

  (local (defthm val-fix-when-empty-record
           (implies (and (val-case x :v_record)
                         (omap::emptyp (v_record->rec x)))
                    (equal (val-fix x) (v_record nil)))
           :hints(("Goal" :in-theory (e/d (val-fix-when-v_record)
                                          (v_record-of-fields))))))


  (local
   (std::defret-mutual <fn>-correct-lemma
     (defret <fn>-correct-lemma
       (typed-val-to-oracle-corr-cond val x)
       :hints ((and stable-under-simplificationp
                    `(:expand (,(car (last clause))
                               <call>
                               (:free (orac) (ty-oracle-val x orac))
                               (ty-satisfied val x))
                :in-theory (enable acl2::orac-read-bits
                                   acl2::orac-read-rational
                                   acl2::orac-read-string
                                   acl2::orac-read
                                   acl2::orac-st-read
                                   acl2::orac-st-read-rational
                                   acl2::orac-st-read-string)
                      :do-not-induct t))
               (And stable-under-simplificationp
                    '(:in-theory (enable val-fix-when-empty-record)))
               )
       :fn typed-val-to-oracle)
     (defret <fn>-correct-lemma
       (typed-vallist-to-oracle-corr-cond vals x)
       :hints ((and stable-under-simplificationp
                    `(:expand (,(car (last clause))
                               <call>
                               (:free (orac) (tylist-oracle-val x orac))
                               (tuple-type-satisfied vals x))
                      :do-not-induct t)))
       :fn typed-vallist-to-oracle)
     (defret <fn>-correct-lemma
       (typed-val-array-to-oracle-corr-cond vals x)
       :hints ((and stable-under-simplificationp
                    `(:expand (,(car (last clause))
                               <call>
                               (:free (orac) (ty-oracle-vals (len vals) x orac))
                               (:free (orac) (ty-oracle-vals (+ 1 (len (cdr vals))) x orac))
                               (:free (orac) (ty-oracle-vals 0 x orac))
                               (array-type-satisfied vals x))
                      :do-not-induct t)))
       :fn typed-val-array-to-oracle)
     (defret <fn>-correct-lemma
       (typed-val-record-to-oracle-corr-cond vals x)
       :hints ((and stable-under-simplificationp
                    `(:expand (,(car (last clause))
                               <call>
                               (:free (orac) (typed_identifierlist-oracle-val x orac))
                               (typed_identifierlist->names x)
                               (record-type-satisfied vals x)
                               (:free (a b) (mergesort (Cons a b)))
                               ;; (:with record-type-satisfied-in-terms-of-fix
                               ;;  (record-type-satisfied vals x))
                               )
                      :in-theory (enable omap::lookup
                                         omap::restrict-of-insert-split)
                      :do-not-induct t)))
       :fn typed-val-record-to-oracle)))

  (std::defret-mutual <fn>-correct
    :no-induction-hint t
    (defret <fn>-correct
      (implies (and (ty-satisfied val x)
                    (equal (acl2::oracle-mode orac) 3)
                    (acl2::prefixp orac-st (acl2::oracle-st orac)))
               (equal (ty-oracle-val x orac)
                      (mv (val-fix val)
                          (nthcdr-orac (len orac-st) orac))))
      :hints ('(:use <fn>-correct-lemma
                :in-theory (disable <fn>-correct-lemma)))
      :fn typed-val-to-oracle)
    (defret <fn>-correct
      (implies (and (tuple-type-satisfied vals x)
                    (equal (acl2::oracle-mode orac) 3)
                    (acl2::prefixp orac-st (acl2::oracle-st orac)))
               (equal (tylist-oracle-val x orac)
                      (mv t
                          (vallist-fix vals)
                          (nthcdr-orac (len orac-st) orac))))
      :hints ('(:use <fn>-correct-lemma
                :in-theory (disable <fn>-correct-lemma)))
      :fn typed-vallist-to-oracle)
    (defret <fn>-correct
      (implies (and (array-type-satisfied vals x)
                    (equal (acl2::oracle-mode orac) 3)
                    (acl2::prefixp orac-st (acl2::oracle-st orac)))
               (equal (ty-oracle-vals (len vals) x orac)
                      (mv t
                          (vallist-fix vals)
                          (nthcdr-orac (len orac-st) orac))))
      :hints ('(:use <fn>-correct-lemma
                :in-theory (disable <fn>-correct-lemma)))
      :fn typed-val-array-to-oracle)
    (defret <fn>-correct
      (implies (and (record-type-satisfied vals x)
                    (equal (acl2::oracle-mode orac) 3)
                    (acl2::prefixp orac-st (acl2::oracle-st orac)))
               (equal (typed_identifierlist-oracle-val x orac)
                      (mv t
                          (omap::restrict (mergesort (typed_identifierlist->names x))
                                          (val-imap-fix vals))
                          (nthcdr-orac (len orac-st) orac))))
      :hints ('(:use <fn>-correct-lemma
                :in-theory (disable <fn>-correct-lemma)))
      :fn typed-val-record-to-oracle)))

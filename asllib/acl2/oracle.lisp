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
(local (include-book "std/stobjs/absstobjs" :dir :system))
(local (include-book "std/lists/repeat" :dir :system))
(local (include-book "centaur/misc/arith-equivs" :dir :System))
(local (include-book "centaur/bitops/ihsext-basics" :dir :System))
(local (std::add-default-post-define-hook :fix))

(local (in-theory (disable ifix unsigned-byte-p)))

(stobjs::defstobj-clone orac acl2::orac :pkg asl-pkg)

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


(defines ty-oracle-val
  :flag-local nil
  (define ty-oracle-val ((x ty-p) orac)
    :guard (ty-resolved-p x)
    :verify-guards nil
    :measure (acl2::two-nats-measure (ty-count x) 0)
    :returns (mv (val maybe-val-p) new-orac)
    (b* ((x (ty->val x)))
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
                     (b* (((mv ok vals orac) (ty-oracle-vals (len x.index.elts) x.type orac)))
                       (mv (and ok (v_record (pairlis$ x.index.elts vals)))
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
      (mv ok (and ok (cons (cons f1.name val1) rest)) orac)))
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
                         (:free (ty) (array-type-satisfied nil ty)))))
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
      (implies (typed_identifierlist-satisfiable x)
               (and ok
                    (record-type-satisfied val x)))
      :hints ('(:expand (<call>
                         (typed_identifierlist-satisfiable x))
                :in-theory (enable record-type-satisfied)))
      :fn typed_identifierlist-oracle-val))

  (fty::deffixequiv-mutual ty-oracle-val))

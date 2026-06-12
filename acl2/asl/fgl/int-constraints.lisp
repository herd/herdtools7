;;****************************************************************************;;
;;                              ACL2 ASL Library                              ;;
;;****************************************************************************;;
;;
;; SPDX-FileCopyrightText: Copyright 2026 Arm Limited and/or its affiliates <open-source-office@arm.com>
;; SPDX-License-Identifier: BSD-3-Clause
;; 
;; ****************************************************************************;;



(in-package "ASL")
(include-book "centaur/fgl/def-fgl-rewrite" :dir :system)
(include-book "centaur/fgl/annotation" :dir :system)
(include-book "defs")
(include-book "interp-readonly" :dir :acl2asl)
(include-book "trace-interp" :dir :acl2asl)
(local (include-book "centaur/bitops/ihsext-basics" :dir :System))
(local (include-book "arithmetic/top" :Dir :system))

;; ----------------------------------------------------------------------------------
;; E_ATC -- I thought it would be nice if "foo as integer{0..56}" would fix whatever foo was
;; to at most 6 bits unsigned. This isn't working yet but the theorems below do prove at least.

(defthmd resolve-int_constraints-related-to-check_int_constraints
  (b* (((mv resolve-res ?resolve-orac)
        (resolve-int_constraints env constrs pos))
       ((mv check-res ?check-orac)
        (check_int_constraints env i constrs)))
    (implies (and (readonly-int_constraintlist-p constrs)
                  ;; (eval_result-case check-res :ev_normal)
                  
                  (eval_result-case resolve-res :ev_normal))
             (and (eval_result-case check-res :ev_normal)
                  (implies (ev_normal->res check-res)
                           (int_constraintlist-satisfied i (ev_normal->res resolve-res))))))
  :hints (("goal" :in-theory (enable (:i readonly-int_constraintlist-p)
                                     int-literal-expr->val)
           :induct (len constrs)
           :expand ((readonly-int_constraintlist-p constrs)
                    (readonly-int_constraint-p (car constrs))
                    (:Free (pos) (resolve-int_constraints env constrs pos))
                    (check_int_constraints env i constrs)
                    (:free (a b) (int_constraintlist-satisfied i (Cons a b)))
                    (:free (c) (int_constraint-satisfied i c)))))
  :rule-classes
  ((:rewrite :corollary
    (b* (((mv resolve-res ?resolve-orac)
          (resolve-int_constraints env constrs pos))
         ((mv check-res ?check-orac)
          (check_int_constraints env i constrs)))
      (implies (and (readonly-int_constraintlist-p constrs)
                    ;; (eval_result-case check-res :ev_normal)
                    (bind-free `((pos . ',(posn "" 0 0 0))) (pos))
                    (eval_result-case resolve-res :ev_normal))
               (eval_result-case check-res :ev_normal))))
   (:rewrite :corollary
    (b* (((mv resolve-res ?resolve-orac)
          (resolve-int_constraints env constrs pos))
         ((mv check-res ?check-orac)
          (check_int_constraints env i constrs)))
      (implies (and (readonly-int_constraintlist-p constrs)
                    (eval_result-case resolve-res :ev_normal))
               (implies (ev_normal->res check-res)
                        (int_constraintlist-satisfied i (ev_normal->res resolve-res))))))))
                


(defthmd eval_expr-e_atc-integer-range-type-fix
  (implies (expr_desc-case (expr->desc e) :e_atc)
           (equal (eval_expr env (fgl::concrete e))
                  (b* ((pos (expr->pos_start e))
                       (desc (expr->desc e))
                       ((e_atc desc))
                       ((mv (evo (expr_result v)) orac) (eval_expr env desc.expr))
                       (old-orac orac)
                       ((mv (evo b) orac) (is_val_of_type v.env v.val desc.type))
                       ((unless b)
                        (evo_error "DE_TAF: " desc (list pos)))
                       (tydesc (ty->desc desc.type))
                       ((when (and (type_desc-case tydesc
                                     :t_int (constraint_kind-case tydesc.constraint
                                              :wellconstrained (readonly-int_constraintlist-p
                                                                tydesc.constraint.constraints)
                                              :otherwise nil)
                                     :otherwise nil)
                                   (val-case v.val :v_int)))
                        (b* (((t_int tint) tydesc)
                             ((wellconstrained cstr) tint.constraint)
                             ((mv cstrs-res ?orac2) (resolve-int_constraints v.env cstr.constraints
                                                                             (posn "" 0 0 0)
                                                                             :orac old-orac))
                             ((unless (eval_result-case cstrs-res :ev_normal))
                              (evo_normal v)))
                          (evo_normal
                           (expr_result (v_int (int_constraintlist-value-fix
                                                (v_int->val v.val)
                                                (ev_normal->res cstrs-res)))
                                        v.env)))))
                    (evo_normal v))))
  :hints (("goal" :expand ((eval_expr env e))
           :in-theory (enable resolve-int_constraints-related-to-check_int_constraints))
          (and stable-under-simplificationp
               '(:expand ((:free (env x orac)
                           (is_val_of_type env x
                                           (e_atc->type (expr->desc e)))))))))







;; ----------------------------------------------------------------------------------
;; Int_constraintlist-value-fix FGL definition.
;; This lets us represent bounded (wellconstrained) integers with bitvectors.
;; The definition of int_constraintlist-value-fix doesn't do this by itself,
;; but we can change its input to a fixed bitwidth using LOGSAT (for a width
;; wider than that of the range's min/max values) and this makes the resulting
;; fixed value also fit in that bitwidth.

;; We need a lot of auxiliary definitions/lemmas to get this through, beginning with
;; int_constraintlist-min/max.
(define int_constraint-max ((x int_constraint-p))
  :guard (int_constraint-resolved-p x)
  :returns (max integerp :rule-classes :type-prescription)
  (int_constraint-case x
    :constraint_exact (int-literal-expr->val x.val)
    :constraint_range (max (int-literal-expr->val x.to)
                           (int-literal-expr->val x.from)))
  ///
  (defret int_constraint-satisfied-implies-lte-max
    (implies (int_constraint-satisfied i x)
             (<= (ifix i) max))
    :hints(("Goal" :in-theory (enable int_constraint-satisfied))))

  (defret int_constraint-value-fix-lte-max
    (<= (int_constraint-value-fix i x) max)
    :hints(("Goal" :in-theory (enable int_constraint-value-fix)))
    :rule-classes :linear))

(define int_constraint-min ((x int_constraint-p))
  :guard (int_constraint-resolved-p x)
  :returns (min integerp :rule-classes :type-prescription)
  (int_constraint-case x
    :constraint_exact (int-literal-expr->val x.val)
    :constraint_range (min (int-literal-expr->val x.from)
                           (int-literal-expr->val x.to)))
  ///
  (defret int_constraint-satisfied-implies-gte-min
    (implies (int_constraint-satisfied i x)
             (<= min (ifix i)))
    :hints(("Goal" :in-theory (enable int_constraint-satisfied))))

  (defret int_constraint-value-fix-gte-min
    (<= min (int_constraint-value-fix i x))
    :hints(("Goal" :in-theory (enable int_constraint-value-fix)))
    :rule-classes :linear)

  (defret int_constraint-min-<=-max
    (<= (int_constraint-min x) (int_constraint-max x))
    :hints(("Goal" :in-theory (enable int_constraint-max)))
    :rule-classes :linear))

(define int_constraintlist-max ((x int_constraintlist-p))
  :guard (and (consp x)
              (int_constraintlist-resolved-p x))
  :returns (max integerp :rule-classes :type-prescription)
  (if (atom (cdr x))
      (int_constraint-max (car x))
    (max (int_constraint-max (car x))
         (int_constraintlist-max (cdr x))))
  ///
  (defret int_constraintlist-satisfied-implies-lte-max
    (implies (int_constraintlist-satisfied i x)
             (<= (ifix i) max))
    :hints(("Goal" :in-theory (enable int_constraintlist-satisfied)
            :induct t)
           (and stable-under-simplificationp
                '(:use ((:instance int_constraint-satisfied-implies-lte-max
                         (x (car x))))
                  :in-theory (disable int_constraint-satisfied-implies-lte-max)))))

  (defret int_constraintlist-value-fix-lte-max
    (<= (int_constraintlist-value-fix i x) max)
    :hints(("Goal" :in-theory (enable int_constraintlist-value-fix)
            :induct t)
           (and stable-under-simplificationp
                '(:expand ((int_constraintlist-satisfied 0 (cdr x))
                           (int_constraint-satisfied i (car x))
                           (int_constraint-max (car x))))))
    :rule-classes :linear))

(define int_constraintlist-min ((x int_constraintlist-p))
  :guard (and (consp x)
              (int_constraintlist-resolved-p x))
  :returns (min integerp :rule-classes :type-prescription)
  (if (atom (cdr x))
      (int_constraint-min (car x))
    (min (int_constraint-min (car x))
         (int_constraintlist-min (cdr x))))
  ///
  (defret int_constraintlist-satisfied-implies-gte-min
    (implies (int_constraintlist-satisfied i x)
             (<= min (ifix i)))
    :hints(("Goal" :in-theory (enable int_constraintlist-satisfied)
            :induct t)
           (and stable-under-simplificationp
                '(:use ((:instance int_constraint-satisfied-implies-gte-min
                         (x (car x))))
                  :in-theory (disable int_constraint-satisfied-implies-gte-min)))))

  (defret int_constraintlist-value-fix-gte-min
    (<= min (int_constraintlist-value-fix i x))
    :hints(("Goal" :in-theory (enable int_constraintlist-value-fix)
            :induct t)
           (and stable-under-simplificationp
                '(:expand ((int_constraintlist-satisfied 0 (cdr x))
                           (int_constraint-satisfied i (car x))
                           (int_constraint-min (car x))))))
    :rule-classes :linear)

  (defret int_constraintlist-min-<=-max
    (<= (int_constraintlist-min x) (int_constraintlist-max x))
    :hints (("Goal" :in-theory (enable int_constraintlist-max)))
    :rule-classes :linear))


(local (defthmd signed-byte-p-by-greater-than-integer-length
         (implies (and (integerp w)
                       (integerp x)
                       (< (integer-length x) w))
                  (signed-byte-p w x))
         :hints(("Goal" :in-theory (acl2::enable* bitops::ihsext-inductions
                                                  bitops::ihsext-recursive-redefs)))))

(local (defthm posp-expt
         (implies (natp i)
                  (posp (Expt 2 i)))
         :hints(("Goal" :in-theory (enable expt)))
         :rule-classes :type-prescription))

(defsection int_constraintlist-value-fix-redef
  (defthm int_constraint-fix-of-logsat
    (implies (and (signed-byte-p w (+ -1 (int_constraint-min c)))
                  (signed-byte-p w (+ 1 (int_constraint-max c))))
             (equal (int_constraint-value-fix (acl2::logsat w x) c)
                    (int_constraint-value-fix x c)))
    :hints(("Goal" :in-theory (e/d (int_constraint-value-fix
                                    int_constraint-min
                                    int_constraint-max))
            :do-not-induct t)))

  (defthm int_constraint-satisfied-of-logsat
    (implies (and (signed-byte-p w (+ -1 (int_constraint-min c)))
                  (signed-byte-p w (+ 1 (int_constraint-max c))))
             (equal (int_constraint-satisfied (acl2::logsat w x) c)
                    (int_constraint-satisfied x c)))
    :hints(("Goal" :in-theory (e/d (int_constraint-satisfied
                                    int_constraint-min
                                    int_constraint-max))
            :do-not-induct t)))

  (defthmd logsat-when-int_constraint-satisfied
    (implies (and (int_constraint-satisfied x c)
                  (signed-byte-p w (+ -1 (int_constraint-min c)))
                  (signed-byte-p w (+ 1 (int_constraint-max c))))
             (equal (acl2::logsat w x) (ifix x)))
    :hints(("Goal" :in-theory (e/d (int_constraint-satisfied
                                    int_constraint-min
                                    int_constraint-max))
            :do-not-induct t)))

  (defthm int_constraintlist-satisfied-of-logsat
    (implies (and (signed-byte-p w (+ -1 (int_constraintlist-min c)))
                  (signed-byte-p w (+ 1 (int_constraintlist-max c))))
             (equal (int_constraintlist-satisfied (acl2::logsat w x) c)
                    (int_constraintlist-satisfied x c)))
    :hints(("Goal" :in-theory (e/d (int_constraintlist-satisfied
                                    int_constraintlist-min
                                    int_constraintlist-max)
                                   (acl2::logsat))
            :induct (int_constraintlist-satisfied x c))))

  (local (defthm logsat-when-signed-byte-p
           (implies (signed-byte-p w (ifix x))
                    (equal (acl2::logsat w x) (ifix x)))))

  (defthm int_constraintlist-fix-of-logsat
    (implies (and (signed-byte-p w (+ -1 (int_constraintlist-min c)))
                  (signed-byte-p w (+ 1 (int_constraintlist-max c))))
             (equal (int_constraintlist-value-fix (acl2::logsat w x) c)
                    (int_constraintlist-value-fix x c)))
    :hints(("Goal" :in-theory (e/d (int_constraintlist-value-fix
                                    int_constraintlist-min
                                    int_constraintlist-max
                                    logsat-when-int_constraint-satisfied)
                                   (acl2::logsat)))))


  (fgl::def-fgl-rewrite int_constraintlist-fix-redef
    (implies (and (fgl::bind-fn-annotation annot 'int_constraintlist-value-fix)
                  (not (assoc-keyword :fixing annot)))
             (equal (int_constraintlist-value-fix x c)
                    (let* ((min (+ -1 (int_constraintlist-min c)))
                           (max (+ 1 (int_constraintlist-max c)))
                           (w (+ 1 (max (integer-length min)
                                        (integer-length max)))))
                      (fgl::annotate '(:fixing t) (int_constraintlist-value-fix
                                                   (acl2::logsat w x) c)))))
    :hints (("goal" :do-not-induct t
             :in-theory (e/d (signed-byte-p-by-greater-than-integer-length)
                             (acl2::logsat signed-byte-p))))))




(local (defthm my-logext-identity
         (implies (signed-byte-p w (ifix x))
                  (equal (logext w x) (ifix x)))
         :hints (("goal" :use ((:instance acl2::logext-identity
                                (size w) (i (ifix x))))
                  :in-theory (disable acl2::logext-identity)))))



;; We also need a good FGL definition of logsat to make this work.
(fgl::def-fgl-rewrite logsat-fgl
  (equal (acl2::logsat w x)
         (b* ((x (ifix x))
              (sh (nfix (1- w)))
              (upper (ash 1 sh))
              ((when (>= x upper)) (1- upper))
              (lower (ash -1 sh))
              ((when (< x lower)) lower))
           (logext w x)))
  :hints(("Goal" :in-theory (enable acl2::logsat
                                    bitops::ash-is-expt-*-x
                                    signed-byte-p
                                    nfix))))


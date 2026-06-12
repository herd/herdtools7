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
(include-book "defs")
(include-book "error-free" :dir :acl2asl)
(include-book "centaur/fgl/fgl-object" :dir :system)
(include-book "centaur/fgl/checks" :dir :system)
(include-book "centaur/fgl/config" :dir :system)
(include-book "centaur/fgl/constraint-db" :dir :system)
(include-book "centaur/fgl/ctrex-utils" :dir :system)
(include-book "centaur/fgl/helper-utils" :dir :system)
(include-book "centaur/fgl/reduce-alist" :dir :system)
(local (include-book "centaur/bitops/ihsext-basics" :dir :System))
(local (include-book "std/lists/take" :dir :system))

;; ----------------------------------------------------------------------------------
;; Abstraction for value arrays
;; We express everything in terms of v_array-nth, v_array-len, and
;; v_array-update-nths.  V_array-update-nths takes an alist argument mapping
;; (possibly) updated indices to their values.  The tricky part is (as always)
;; merging IFs -- if we have two branches in which different updates have been
;; made, we need to merge these alists. The basic operation here is to assocate
;; each possibly-updated index with the (merged) value, which in a branch where
;; the index hasn't been updated, is the initial value from the original array.
;; Surprisingly, it looks like it's more common to see a merge where one branch
;; has updates and the other has none.

;; It's always tricky to figure out what to do with symbolic indices. For now
;; updates are cheap and lookups may have to compare a (symbolic) key with
;; multiple (symbolic) update indices.

(define identical-lst-p ((x true-listp))
  :returns (res booleanp :rule-classes :type-prescription)
  (if (atom x)
      (eq x nil)
      (if (atom (cdr x))
          (eq (cdr x) nil)
          (and (equal (car x) (cadr x))
               (identical-lst-p (cdr x)))))
  ///
  (defret nth-of-identical-lst
    (implies (and res
                  (natp n)
                  (< n (len x)))
             (equal (nth n x) (car x)))
    :hints (("Goal" :induct (nth n x)))))

(define-continue v_array-len
  (fgl::remove-fgl-rewrite v_array-len)

  (defthm v_array-len-of-v_array
    (equal (v_array-len (v_array (v_array->arr x)))
           (v_array-len x))
    :hints(("Goal" :in-theory (enable v_array-len))))

  (fgl::def-fgl-rewrite v_array-len-of-v_array-rwr
    (equal (v_array-len (v_array arr))
           (len arr))
    :hints(("Goal" :in-theory (enable v_array-len))))
  )


(define-continue v_array-nth
  (fgl::def-fgl-rewrite eval_expr-of-e_getarray-in-terms-of-v_array-nth
    (implies (and (syntaxp (fgl::fgl-object-case e :g-concrete))
                  (expr_desc-case (expr->desc e) :e_getarray))
             (equal (eval_expr env e)
                    (b* ((desc (expr->desc e))
                         (pos (expr->pos_start e))
                         ((e_getarray desc)))
                      (b* (((mv (evo (expr_result arr)) orac) (eval_expr env desc.base))
                           ((mv (evo (expr_result idx)) orac) (eval_expr arr.env desc.index))
                           ((evo idxv) (val-case idx.val
                                         :v_int (ev_normal idx.val.val)
                                         :otherwise (ev_error "DE_BI: getarray non-integer index" desc (list pos))))
                           ((evo &) (val-case arr.val
                                      :v_array (ev_normal nil)
                                      :otherwise (ev_error "getarray non-array value" desc (list pos)))))
                        (if (and (<= 0 idxv)
                                 (fgl-mark 'idxv-less-than-len
                                           (< idxv (v_array-len arr.val))))
                            (evo_normal (expr_result (v_array-nth idxv arr.val) idx.env))
                          (evo_error "DE_BI: getarray index out of range" desc (list pos)))))))
    :hints (("Goal" :expand ((eval_expr env e))
             :in-theory (enable v_array-len))))

  (fgl::def-fgl-rewrite eval_expr-*t-of-e_getarray-in-terms-of-v_array-nth
    (implies (and (syntaxp (fgl::fgl-object-case e :g-concrete))
                  (expr_desc-case (expr->desc e) :e_getarray))
             (equal (eval_expr-*t env e)
                    (b* ((trace nil)
                         (desc (expr->desc e))
                         (pos (expr->pos_start e))
                         ((e_getarray desc))
                         (env (env-replace-static static-env env)))
                      (B* (((evoo-*t (expr_result arr))
                            (eval_expr-*t env desc.base))
                           ((evoo-*t (expr_result idx))
                            (eval_expr-*t arr.env desc.index))
                           ((evo-*t idxv)
                            (val-case idx.val
                              :v_int (ev_normal idx.val.val)
                              :otherwise (ev_error "DE_BI: getarray non-integer index"
                                                   desc (list pos))))
                           ((evo-*t &)
                            (val-case arr.val
                              :v_array (ev_normal nil)
                              :otherwise (ev_error "getarray non-array value"
                                                   desc (list pos)))))
                        (if (and (<= 0 idxv)
                                 (fgl-mark 'idxv-less-than-len
                                           (< idxv (v_array-len arr.val))))
                            (evo_normal-*t (expr_result (v_array-nth idxv arr.val) idx.env))
                          (evo_error-*t "DE_BI: getarray index out of range" desc (list pos)))))))
    :hints (("goal" :expand ((eval_expr-*t env e))
             :in-theory (enable v_array-nth v_array-len))))

  (fgl::def-fgl-rewrite eval_expr-*t-when-error-free-of-e_getarray-in-terms-of-v_array-nth
    (implies (and (eval_expr-*t-no-error env e)
                  (syntaxp (fgl::fgl-object-case e :g-concrete))
                  (expr_desc-case (expr->desc e) :e_getarray))
             (equal (eval_expr-*t env e)
                    (b* ((trace nil)
                         (desc (expr->desc e))
                         (pos (expr->pos_start e))
                         ((e_getarray desc))
                         (env (env-replace-static static-env env)))
                      (B* (((evoo-*tef _condvar-0 (expr_result arr))
                            (eval_expr-*t env desc.base))
                           ((evoo-*tef _condvar-1 (expr_result idx))
                            (eval_expr-*t arr.env desc.index))
                           ((evo-*tef _condvar-2 idxv)
                            (val-case idx.val
                              :v_int (ev_normal idx.val.val)
                              :otherwise (ev_error "DE_BI: getarray non-integer index"
                                                   desc (list pos))))
                           ((evo-*tef _condvar-3 &)
                            (val-case arr.val
                              :v_array (ev_normal nil)
                              :otherwise (ev_error "getarray non-array value"
                                                   desc (list pos)))))
                        (fgl::conditionalize
                         _condvar-4
                         (and (<= 0 idxv)
                              (fgl-mark 'idxv-less-than-len
                                        (< idxv (v_array-len arr.val))))
                         (evo_normal-*t (expr_result (v_array-nth idxv arr.val) idx.env)))))))
    :hints (("goal" :expand ((eval_expr-*t env e))
             :in-theory (acl2::e/d* (v_array-nth v_array-len fgl::conditionalize1 fgl::conditionalize2)
                                    (asl-*t-equals-original-rules)))))

  (fgl::remove-fgl-rewrite v_array-nth)

  (fgl::def-fgl-rewrite v_array-nth-of-val-fix
    (equal (v_array-nth n (val-fix x))
           (v_array-nth n x)))

  (local (defthm nth-of-vallist-fix-under-val-equiv
           (val-equiv (nth n (vallist-fix arr))
                      (nth n arr))
           :hints(("Goal" :in-theory (enable nth vallist-fix)))))
  
  (fgl::def-fgl-rewrite v_array-nth-of-const-idx
    (implies (and (syntaxp (fgl::fgl-object-case n :g-concrete))
                  (natp (fgl::g-concrete->val n))
                  (consp arr))
             (equal (v_array-nth n (v_array arr))
                    (val-fix (nth n arr))))
    :hints (("goal" :do-not-induct t
             :in-theory (disable nth-of-vallist-fix))))

  (fgl::def-fgl-rewrite v_array-nth-of-identical-lst
    (implies (and (syntaxp (fgl::fgl-object-case x :g-concrete))
                  (val-case x :v_array)
                  (identical-lst-p (v_array->arr x))
                  (natp n)
                  (< n (v_array-len x)))
             (equal (v_array-nth n x)
                    (val-fix (car (v_array->arr x)))))
    :hints (("Goal" :in-theory (enable v_array-len))))

  (local (defun dec-dec-cdr (n m x)
           (if (zp n)
               (list m x)
             (dec-dec-cdr (1- n) (1- m) (cdr x)))))

  (local (defthm nth-of-array-type-fix-val
           (implies (< (nfix n) (nfix m))
                    (equal (nth n (array-type-fix-val m x ty))
                           (ty-fix-val (nth n x) ty)))
           :hints(("Goal" :expand ((array-type-fix-val m x ty)
                                   (:free (x) (nth n x)))
                   :induct (dec-dec-cdr n m x)))))

  ;; (fgl::def-fgl-rewrite v_array-nth-of-ty-fix
  ;;   (implies (and (syntaxp (fgl::fgl-object-case ty :g-concrete))
  ;;                 (natp n)
  ;;                 (type_desc-case (ty->desc ty) :t_array)
  ;;                 (b* (((t_array ty) (ty->desc ty))) (array_index-case ty.index :arraylength_expr))
  ;;                 (b* (((t_array ty) (ty->desc ty))
  ;;                      ((arraylength_expr ty.index))) (int-literal-expr-p ty.index.length))
  ;;                 (fgl-mark 'n-less-than-ty.index.length
  ;;                           (and (b* (((t_array ty) (ty->desc ty))
  ;;                                     ((arraylength_expr ty.index))) (< n (int-literal-expr->val ty.index.length)))
  ;;                                t)))
  ;;            (equal (v_array-nth n (ty-fix-val x ty))
  ;;                   (ty-fix-val (nth n (v_array->arr x))
  ;;                               (t_array->type (ty->desc ty)))))
  ;;   :hints (("goal" :expand ((ty-fix-val x ty))
  ;;            :in-theory (disable nth))))

  (fty::deffixequiv v_array-nth)

  (defthm v_array-nth-of-v_array
    (equal (v_array-nth n (v_array (v_array->arr x)))
           (v_array-nth n x))
    :hints (("Goal" :in-theory (enable v_array-nth))))

  (fgl::def-fgl-rewrite v_array-nth-of-v_array-rwr
    (equal (v_array-nth n (v_array arr))
           (val-fix (nth n arr)))
    :hints(("Goal" :in-theory (enable v_array-nth))))

  )

(local (fty::deffixcong val-equiv vallist-equiv (update-nth n v x) v
         :hints(("Goal" :in-theory (enable vallist-fix)))))

(define-continue v_array-update-nth
  (fgl::def-fgl-rewrite eval_lexpr-of-le_setarray-in-terms-of-v_array-update-nth
    (implies (and (syntaxp (fgl::fgl-object-case lx :g-concrete))
                  (lexpr_desc-case (lexpr->desc lx) :le_setarray))
             (equal (eval_lexpr env lx v)
                    (b* ((pos (lexpr->pos_start lx))
                         (lx (lexpr->desc lx))
                         ((le_setarray lx)))
                      (b* ((rbase (expr_of_lexpr lx.base))
                           ((mv (evo (expr_result rbv)) orac) (eval_expr env rbase))
                           ((mv (evo (expr_result idx)) orac) (eval_expr rbv.env lx.index))
                           ((evob idxv) (v_to_int idx.val))
                           ((evo newarray)
                            (val-case rbv.val
                              :v_array (if (and (<= 0 idxv)
                                                (< idxv (v_array-len rbv.val)))
                                           (ev_normal (v_array-update-nth idxv (val-fix v) rbv.val))
                                         (ev_error "DE_BI: le_setarray index out of obunds" lx (list pos)))
                              :otherwise (ev_error "le_setarray non array base" lx (list pos)))))
                        (eval_lexpr idx.env lx.base newarray)))))
    :hints (("goal" :expand ((eval_lexpr env lx v))
             :in-theory (enable v_array-len))))

  (fgl::def-fgl-rewrite eval_lexpr-*t-of-le_setarray-in-terms-of-v_array-update-nth
    (implies (and (syntaxp (fgl::fgl-object-case lx :g-concrete))
                  (lexpr_desc-case (lexpr->desc lx) :le_setarray))
             (equal (eval_lexpr-*t env lx v)
                    (b* ((trace nil)
                         (pos (lexpr->pos_start lx))
                         (lx (lexpr->desc lx))
                         ((le_setarray lx))
                         (rbase (expr_of_lexpr lx.base))
                         (env (env-replace-static static-env env))
                         ((evoo-*t (expr_result rbv))
                          (eval_expr-*t env rbase))
                         ((evoo-*t (expr_result idx))
                          (eval_expr-*t rbv.env lx.index))
                         ((evob-*t idxv) (v_to_int idx.val))
                         ((evo-*t newarray)
                          (val-case rbv.val :v_array
                            (if (and (<= 0 idxv)
                                     (< idxv (v_array-len rbv.val)))
                                (ev_normal (v_array-update-nth idxv v rbv.val))
                              (ev_error "DE_BI: le_setarray index out of obunds"
                                        lx (list pos)))
                            :otherwise (ev_error "le_setarray non array base"
                                                 lx (list pos)))))
                      (evtailcall-*t (eval_lexpr-*t idx.env lx.base newarray)))))
    :hints (("goal" :expand ((eval_lexpr-*t env lx v))
             :in-theory (enable v_array-update-nth v_array-len))))

  (fgl::def-fgl-rewrite eval_lexpr-*t-when-error-free-of-le_setarray-in-terms-of-v_array-update-nth
    (implies (and (eval_lexpr-*t-no-error env lx v)
                  (syntaxp (fgl::fgl-object-case lx :g-concrete))
                  (lexpr_desc-case (lexpr->desc lx) :le_setarray))
             (equal (eval_lexpr-*t env lx v)
                    (b* ((trace nil)
                         (pos (lexpr->pos_start lx))
                         (lx (lexpr->desc lx))
                         ((le_setarray lx))
                         (rbase (expr_of_lexpr lx.base))
                         (env (env-replace-static static-env env))
                         ((evoo-*tef _condvar-0 (expr_result rbv))
                          (eval_expr-*t env rbase))
                         ((evoo-*tef _condvar-1 (expr_result idx))
                          (eval_expr-*t rbv.env lx.index))
                         ((evob-*tef _condvar-2 idxv) (v_to_int idx.val))
                         ((evo-*tef _condvar-3 newarray)
                          (fgl::conditionalize
                           _condvar-4
                           (and (val-case rbv.val :v_array)
                                (<= 0 idxv)
                                (< idxv (v_array-len rbv.val)))
                           (ev_normal (v_array-update-nth idxv v rbv.val))
                           :on-unreachable
                           (ev_normal (v_array-update-nth idxv v rbv.val)))))
                      (evtailcall-*tef _condvar-5 (eval_lexpr-*t idx.env lx.base newarray)))))
    :hints (("goal" :expand ((eval_lexpr-*t env lx v))
             :in-theory (acl2::e/d* (v_array-update-nth v_array-len fgl::conditionalize1 fgl::conditionalize2)
                                    (asl-*t-equals-original-rules)))))

  (local (defthm nth-when-len-greater
           (implies (<= (len x) (nfix n))
                    (equal (nth n x) nil))
           :hints(("Goal" :in-theory (enable nth)))))

  (fgl::def-fgl-rewrite v_array-nth-of-v_array-update-nth
    (equal (v_array-nth n (v_array-update-nth m v x))
           (if (eql (nfix n) (nfix m))
               (val-fix v)
             (v_array-nth n x)))
    :hints(("Goal" :in-theory (enable v_array-nth))))

  (fgl::def-fgl-rewrite v_array-len-of-v_array-update-nth
    (equal (v_array-len (v_array-update-nth m v x))
           (max (+ 1 (nfix m)) (v_array-len x)))
    :hints(("Goal" :in-theory (enable v_array-len))))

  (in-theory (enable v_array-nth-of-v_array-update-nth
                     v_array-len-of-v_array-update-nth))

  (fgl::remove-fgl-rewrite v_array-update-nth)

  (fgl::add-fgl-rewrite val-p-of-v_array-update-nth)

  (defret val-kind-of-<fn>
    (equal (val-kind new-x)
           :v_array))

  (fgl::add-fgl-rewrite val-kind-of-v_array-update-nth)

  (local (defthm vallist-fix-of-update-nth-val-fix-val
           (equal (vallist-fix (update-nth n (val-fix v) x))
                  (vallist-fix (update-nth n v x)))
           :hints(("Goal" :in-theory (enable update-nth vallist-fix)))))

  (local (defthm vallist-fix-of-update-nth-vallist-fix
           (equal (vallist-fix (update-nth n v (vallist-fix x)))
                  (vallist-fix (update-nth n v x)))))

  
  
  (local (defthm update-nth-swap
           (implies (not (equal (nfix n1) (nfix n2)))
                    (equal (update-nth n1 v1 (update-nth n2 v2 x))
                           (update-nth n2 v2 (update-nth n1 v1 x))))
           :hints(("Goal" :in-theory (enable update-nth)))))

  (local (defthm redundant-update-nth
           (equal (update-nth n v (update-nth n v2 x))
                  (update-nth n v x))
           :hints(("Goal" :in-theory (enable update-nth)))))
  
  (defthmd v_array-update-nth-of-update-nth
    (implies (syntaxp (or (equal n2 n1) (<< n2 n1)))
             (equal (v_array-update-nth n2 v2 (v_array-update-nth n1 v1 x))
                    (if (equal (nfix n2) (nfix n1))
                        (v_array-update-nth n2 v2 x)
                      (v_array-update-nth n1 v1 (v_array-update-nth n2 v2 x)))))
    :hints(("Goal" :in-theory (enable v_array-update-nth))))

  (local (defthm update-nth-of-nth
           (implies (< (nfix n) (len x))
                    (equal (update-nth n (nth n x) x)
                           x))))
  
  (defthm v_array-update-nth-redundant
    (implies (and (equal val (v_array-nth idx x))
                  (< (nfix idx) (v_array-len x)))
             (equal (v_array-update-nth idx val x)
                    (v_array (v_array->arr x))))
    :hints(("Goal" :in-theory (e/d (v_array-nth
                                    v_array-len)
                                   ((force))))))

  
  (fty::deffixequiv v_array-update-nth)
  

  (defthm v_array-update-nth-of-v_array
    (equal (v_array-update-nth n v (v_array (v_array->arr x)))
           (v_array-update-nth n v x))
    :hints(("Goal" :in-theory (enable v_array-update-nth)))))




(local (defthm suffixp-of-nthcdr
         (implies (and (<= (nfix n) (len x))
                       (not (acl2::suffixp val x)))
                  (not (acl2::suffixp val (nthcdr n x))))
         :hints(("Goal" :in-theory (enable nthcdr acl2::suffixp)))))

(fgl::def-fgl-brewrite find-common-suffix-1
  (implies (and (equal len1 (len alist1))
                (syntaxp (fgl::fgl-object-case len1 :g-concrete))
                (equal len2 (len alist2))
                (syntaxp (fgl::fgl-object-case len2 :g-concrete))
                (equal new-alist1 (if (<= len1 len2) alist1 (nthcdr (- len1 len2) alist1)))
                (equal new-alist2 (if (<= len2 len1) alist2 (nthcdr (- len2 len1) alist2)))
                
                (equal suff (common-suffix-binder2 suff2 new-alist1 new-alist2)))
           (equal (common-suffix-binder suff alist1 alist2)
                  suff))
  :hints(("Goal" :in-theory (enable common-suffix-binder
                                    common-suffix-binder2))))

(fgl::def-fgl-brewrite find-common-suffix-2
  (implies (equal suff (if (fgl::check-equal eq alist1 alist2)
                           alist1
                         (common-suffix-binder2 suff2 (cdr alist1) (cdr alist2))))
           (equal (common-suffix-binder2 suff alist1 alist2)
                  suff))
  :hints(("Goal" :in-theory (enable fgl::check-equal
                                    common-suffix-binder2
                                    acl2::suffixp))))

(define-continue v_array-update-nths
  (fgl::remove-fgl-rewrite v_array-update-nths)
  (fgl::disable-if-merge-args v_array-update-nths)
  (fgl::def-fgl-rewrite v_array-update-nth-to-update-nths
    (implies (< (nfix n) (v_array-len x))
             (equal (v_array-update-nth n val x)
                    (v_array-update-nths (list (cons (nfix n) val)) x)))
    :hints(("Goal" :in-theory (enable v_array-update-nth))))

  (fgl::def-fgl-rewrite v_array-len-of-v_array-update-nths
    (equal (v_array-len (v_array-update-nths lst x))
           (v_array-len x))
    :hints(("Goal" :in-theory (enable v_array-len
                                      v_array-update-nth))))

  (fgl::def-fgl-rewrite v_array-nth-of-v_array-update-nths
    (equal (v_array-nth n (v_array-update-nths lst x))
           (b* ((look (hons-assoc-equal (nfix n) lst)))
             (if (and look
                      (< (nfix n) (v_array-len x)))
                 (val-fix (cdr look))
                 (v_array-nth n x))))
    :hints (("Goal" :in-theory (enable hons-assoc-equal)
                    :induct (v_array-update-nths lst x))))

  (fgl::def-fgl-rewrite val-kind-of-v_array-update-nths
    (equal (val-kind (v_array-update-nths lst x))
           :v_array))

  (fgl::add-fgl-rewrite val-p-of-v_array-update-nths)

  (defthm v_array-update-nths-of-true-list-fix
    (equal (v_array-update-nths (true-list-fix lst) x)
           (v_array-update-nths lst x))
    :hints(("Goal" :in-theory (enable v_array-update-nths))))

  (defthm v_array-update-nths-of-v_array
    (equal (v_array-update-nths lst (v_array (v_array->arr x)))
           (v_array-update-nths lst x)))

  (in-theory (enable v_array-len-of-v_array-update-nths
                     v_array-nth-of-v_array-update-nths
                     val-kind-of-v_array-update-nths))
  
  (defthm v_array-update-nths-of-append
    (equal (v_array-update-nths (append a b) x)
           (v_array-update-nths a (v_array-update-nths b x))))

  (defthm v_array-update-nth-of-update-nths-of-update-nth
    (implies (< (nfix k) (v_array-len x))
             (equal (v_array-update-nth k v1 (v_array-update-nths lst (v_array-update-nth k v x)))
                    (v_array-update-nth k v1 (v_array-update-nths lst x))))
    :hints(("Goal" :in-theory (enable v_array-update-nths
                                      hons-assoc-equal
                                      v_array-update-nth-of-update-nth))))
  
  (defthm v_array-update-nths-of-update-nth
    (implies (and (hons-assoc-equal (nfix k) lst)
                  (< (nfix k) (v_array-len x)))
             (equal (v_array-update-nths lst (v_array-update-nth k v x))
                    (v_array-update-nths lst x)))
    :hints(("Goal" :in-theory (enable v_array-update-nths
                                      hons-assoc-equal))))

  (defthm v_array-update-nths-of-update-nth-swap
    (implies (and (not (hons-assoc-equal (nfix k) lst))
                  (< (nfix k) (v_array-len x)))
             (equal (v_array-update-nths lst (v_array-update-nth k v x))
                    (v_array-update-nth k v (v_array-update-nths lst x))))
    :hints(("Goal" :in-theory (enable v_array-update-nth-of-update-nth))))
  
  (defthm v_array-update-nths-of-fast-alist-fork
    (equal (v_array-update-nths (fast-alist-fork a b) x)
           (v_array-update-nths b (v_array-update-nths a x)))
    :hints(("Goal" :in-theory (enable fast-alist-fork
                                      v_array-update-nth-of-update-nth))))

  (local (defthm atom-of-cdr-last
           (not (consp (cdr (last x))))
           :rule-classes :type-prescription))
  
  (defthm v_array-update-nths-of-fast-alist-clean
    (equal (v_array-update-nths (fast-alist-clean lst) x)
           (v_array-update-nths lst x)))

  (fgl::def-fgl-rewrite v_array-update-nths-of-v_array-update-nths
    (equal (v_array-update-nths lst1 (v_array-update-nths lst2 x))
           (v_array-update-nths (append lst1 lst2) x)))

  (defthm v_array-update-nths-of-nil
    (equal (v_array-update-nths nil x)
           (v_array (v_array->arr x)))))

(encapsulate nil
  (local (include-book "std/lists/nth" :dir :system))

  (local (defthm v_array-nth-rw
           (implies (< (nfix n) (v_array-len x))
                    (equal (nth n (v_array->arr x))
                           (v_array-nth n x)))
           :hints(("Goal" :in-theory (enable v_array-nth v_array-len)))))

  (local (defthm v_array-len-rw
           (equal (len (v_array->arr x))
                  (v_array-len x))
           :hints(("Goal" :in-theory (enable v_array-len)))))

  (local
   (defthm v_array-update-nths-when-alist-equiv-lemma
     (implies (acl2::alist-equiv lst1 lst2)
              (equal (v_array->arr (v_array-update-nths lst1 x))
                     (v_array->arr (v_array-update-nths lst2 x))))
     :hints ((acl2::equal-by-nths-hint))
     :rule-classes nil))

  (defcong acl2::alist-equiv equal (v_array-update-nths lst x) 1
    :hints (("goal" :use ((:instance v_array-of-fields
                           (x (v_array-update-nths lst x)))
                          (:instance v_array-of-fields
                           (x (v_array-update-nths lst-equiv x)))
                          (:instance v_array-update-nths-when-alist-equiv-lemma
                           (lst1 lst) (lst2 lst-equiv)))
             :in-theory (disable v_array-of-fields
                                 equal-of-v_array))))

  (defthm fal-extract-superset-under-alist-equiv
    (implies (subsetp-equal (acl2::alist-keys x) (acl2::double-rewrite keys))
             (acl2::alist-equiv (acl2::fal-extract keys x)
                                x))
    :hints(("Goal" :in-theory (enable acl2::alist-equiv-iff-agree-on-bad-guy))))
  
  (defthm v_array-update-nths-of-fal-extract-superset
    (implies (subsetp-equal (acl2::alist-keys lst) (acl2::double-rewrite keys))
             (equal (v_array-update-nths (acl2::fal-extract keys lst) x)
                    (v_array-update-nths lst x)))))



(local (defthmd append-take-suffix
         (implies (and (acl2::suffixp suff lst)
                       (equal len (- (len lst) (len suff))))
                  (equal (append (take len lst) suff) lst))
         :hints(("Goal" :in-theory (enable acl2::suffixp)))))


(define v_array-updatelst-conditionalize2 (keys test then-lst else-lst orig)
  :returns (new-lst)
  :verify-guards nil
  (if (atom keys)
      nil
    (b* ((idx (car keys))
         ((unless (and (natp idx)
                       (< idx (v_array-len orig))))
          (v_array-updatelst-conditionalize2 (cdr keys) test then-lst else-lst orig))
         (val (if test
                  (let ((look (hons-assoc-equal idx then-lst)))
                    (if look
                        (val-fix (cdr look))
                      (v_array-nth idx orig)))
                (let ((look (hons-assoc-equal idx else-lst)))
                  (if look
                      (val-fix (cdr look))
                    (v_array-nth idx orig))))))
      (cons (cons idx val)
            (v_array-updatelst-conditionalize2 (cdr keys) test then-lst else-lst orig))))
  ///
  (defret hons-assoc-equal-of-<fn>
    (equal (hons-assoc-equal k new-lst)
           (and (natp k)
                (member-equal k keys)
                (< k (v_array-len orig))
                (cons k (b* ((lst (if test then-lst else-lst))
                             (look (hons-assoc-equal k lst)))
                          (if look
                              (val-fix (cdr look))
                            (v_array-nth k orig))))))
    :hints (("goal" :induct <call>)))
  
  (defthm v_array-updatelst-conditionalize2-correct
    (equal (v_array-update-nths
            (v_array-updatelst-conditionalize2 keys test then-lst else-lst orig) orig)
           (if test
               (v_array-update-nths (acl2::fal-extract keys then-lst) orig)
             (v_array-update-nths (acl2::fal-extract keys else-lst) orig)))
    :hints(("Goal" :in-theory (e/d (v_array-update-nths
                                    v_array-update-nth-redundant)
                                   (equal-of-v_array))
            :induct (v_array-updatelst-conditionalize2 keys test then-lst else-lst orig))))

  (fgl::def-fgl-rewrite v_array-updatelst-conditionalize2-under-v_array-update-nths
    (equal (v_array-update-nths
            (v_array-updatelst-conditionalize2 keys test then-lst else-lst orig) orig)
           (v_array-update-nths
            (if (atom keys)
                nil
              (b* ((idx (car keys))
                   (val (fgl::conditionalize
                         val-under-bounds
                         (and (natp idx)
                              (< idx (v_array-len orig)))
                         (if test
                             (let ((look (hons-assoc-equal idx then-lst)))
                               (if look
                                   (val-fix (cdr look))
                                 (v_array-nth idx orig)))
                           (let ((look (hons-assoc-equal idx else-lst)))
                             (if look
                                 (val-fix (cdr look))
                               (v_array-nth idx orig)))))))
                (cons (cons idx val)
                      (v_array-updatelst-conditionalize2 (cdr keys) test then-lst else-lst orig))))
            orig))
    :hints(("Goal" :in-theory (enable v_array-updatelst-conditionalize2
                                      v_array-update-nths))))

  (fgl::remove-fgl-rewrite v_array-updatelst-conditionalize2-under-v_array-update-nths)

  (fgl::remove-fgl-rewrite v_array-updatelst-conditionalize2))

(define v_array-updatelst-conditionalize2-binder (ans keys test then-lst else-lst orig)
  :verify-guards nil
  :returns (updatelst)
  (let ((spec (v_array-updatelst-conditionalize2 keys test then-lst else-lst orig)))
    (if (equal (v_array-update-nths ans orig)
               (v_array-update-nths spec orig))
        ans
      spec))
  ///
  (defret v_array-update-nths-of-<fn>
    (equal (v_array-update-nths updatelst orig)
           (v_array-update-nths
            (v_array-updatelst-conditionalize2 keys test then-lst else-lst orig)
            orig)))


  (fgl::def-fgl-rewrite v_array-update-nths-of-v_array-updatelst-conditionalize2
    (equal (v_array-update-nths (v_array-updatelst-conditionalize2
                                 keys test then-lst else-lst orig) orig)
           (v_array-update-nths
            (v_array-updatelst-conditionalize2-binder
             ans keys test (make-fast-alist then-lst) (make-fast-alist else-lst) orig) orig)))

  (fgl::def-fgl-brewrite v_array-updatelst-conditionalize2-binder-impl
    (implies (equal ans
                    (if (atom keys)
                        nil
                      (b* ((idx (car keys))
                           (val (fgl::conditionalize
                                 val-under-bounds
                                 (and (natp idx)
                                      (< idx (v_array-len orig)))
                                 (if test
                                     (let ((look (hons-get idx then-lst)))
                                       (if look
                                           (val-fix (cdr look))
                                         (v_array-nth idx orig)))
                                   (let ((look (hons-get idx else-lst)))
                                     (if look
                                         (val-fix (cdr look))
                                       (v_array-nth idx orig)))))))
                        (cons (cons idx val)
                              (v_array-updatelst-conditionalize2-binder
                               ans1 (cdr keys) test then-lst else-lst orig)))))
             (equal (v_array-updatelst-conditionalize2-binder
                     ans keys test then-lst else-lst orig)
                    ans))
    :hints(("Goal" :in-theory (enable v_array-updatelst-conditionalize2
                                      v_array-update-nths)))))
              
             



          

(local (include-book "centaur/misc/hons-sets" :dir :system))

(local (in-theory (disable fast-alist-clean)))

(define syntactic-set-union ((ans true-listp) (x true-listp) (y true-listp))
  ;; FGL binder that tries to express something set-equiv to the union of x and y
  ;; with fewer duplicates than just the naive append. Implementation uses check-memberp
  ;; to determine whether an element is syntactically known to be already included.
  ;; Doesn't reduce duplicates within the individual list inputs, just between them.
  (if (acl2::set-equiv ans (union-equal x y))
      ans
    (union-equal x y))
  ///
  (defthm syntactic-set-union-under-set-equiv
    (acl2::Set-equiv (syntactic-set-union ans x y)
                     (append x y)))

  (local (defthm member-of-set-equiv-to-append
           (implies (acl2::set-equiv x (append y z))
                    (iff (member-equal k x)
                         (or (member-equal k y)
                             (member-equal k z))))))
  
  (fgl::def-fgl-brewrite syntactic-set-union-impl
    (implies (equal ans (b* (((when (fgl::syntax-bind concrete (and (fgl::fgl-object-case x :g-concrete)
                                                                    (fgl::fgl-object-case y :g-concrete))))
                              (acl2::hons-union x y))
                             ((when (fgl::check-non-consp x-atom x)) y)
                             ((unless (fgl::check-consp x-consp x)) (append x y))
                             (rest (syntactic-set-union ans1 (cdr x) y)))
                          (if (fgl::check-memberp memp (car x) y)
                              rest
                            (cons (car x) rest))))
             (equal (syntactic-set-union ans x y)
                    ans))
    :hints(("Goal" :in-theory (enable syntactic-set-union
                                      fgl::check-non-consp
                                      fgl::check-consp
                                      fgl::check-memberp)))))
                        


(fgl::def-fgl-branch-merge merge-v_array-update-nths-2
  (equal (if test (v_array-update-nths lst1 x)
           (v_array-update-nths lst2 x))
         (b* ((suff (common-suffix-binder suff lst1 lst2))
              (suff-len (len suff))
              (base (v_array-update-nths suff x))
              (lst1-prefix (fgl::reduce-alist lst1-prefix (take (- (len lst1) suff-len) lst1)))
              (lst2-prefix (fgl::reduce-alist lst2-prefix (take (- (len lst2) suff-len) lst2)))
              (keys1 (acl2::alist-keys lst1-prefix))
              (keys2 (acl2::alist-keys lst2-prefix))
              ;; ((unless (fgl::syntax-bind keys-concrete (and (fgl::fgl-object-case keys1 :g-concrete)
              ;;                                               (fgl::fgl-object-case keys2 :g-concrete))))
              ;;  ;;(fgl::fgl-progn (fgl::fgl-error! :msg "merge-v_array-update-nths-2 -- non-concrete keys")
              ;;  (fgl::abort-rewrite
              ;;   (if test (v_array-update-nths lst1 x)
              ;;     (v_array-update-nths lst2 x))))
              (keys (syntactic-set-union keys keys1 keys2))
              (new-lst (v_array-updatelst-conditionalize2 keys test lst1-prefix lst2-prefix base)))
           (v_array-update-nths new-lst base)))
  :hints(("Goal" :in-theory (enable common-suffix-binder))
         (and stable-under-simplificationp
              '(:in-theory (e/d (v_array-update-nths-of-v_array-update-nths
                                 append-take-suffix)
                                (v_array-update-nths-of-append))))))

(fgl::def-fgl-branch-merge merge-v_array-update-nths-1
  (implies (and (val-p x) (equal (val-kind x) :v_array))
           (equal (if test (v_array-update-nths lst1 x) x)
                  (b* ((base x)
                       (lst1 (fgl::reduce-alist lst1-reduce lst1))
                       (keys1 (acl2::alist-keys lst1))
                       (new-lst (v_array-updatelst-conditionalize2 keys1 test lst1 nil base)))
                    (v_array-update-nths new-lst base)))))


(fgl::def-fgl-branch-merge v_array-nth-merge
  (implies (val-p y)
           (equal (if test (v_array-nth k x) y)
                  (let ((look (v_array-nth k x)))
                    (fty::multicase
                      ((val-case look) (val-case y))
                      ((:v_bool :v_bool) (v_bool (if test look.val y.val)))
                      ((:v_bitvector :v_bitvector)
                       :when (equal look.len y.len)
                       (v_bitvector look.len (if test look.val y.val)))
                      ((:v_int :v_int)
                       (v_int (if test look.val y.val)))
                      ((:v_real :v_real) (v_real (if test look.val y.val)))
                      ((:v_string :v_string) (v_string (if test look.val y.val)))
                      ((:v_label :v_label) (v_label (choose-value-if test look.val y.val)))
                      (- (fgl::abort-rewrite (if test look y)))))))
  :hints(("Goal" :in-theory (enable choose-value-if))))



(fgl::def-fgl-rewrite equal-of-v_array-nth
  (implies (val-p y)
           (equal (equal (v_array-nth k x) y)
                  (let ((look (v_array-nth k x)))
                    (fty::multicase
                      ((val-case look) (val-case y))
                      ((:v_bool :v_bool) (iff look.val y.val))
                      ((:v_bitvector :v_bitvector)
                       :when (equal look.len y.len)
                       (equal look.val y.val))
                      ((:v_int :v_int)
                       (equal look.val y.val))
                      ((:v_real :v_real) (equal look.val y.val))
                      ((:v_string :v_string) (equal look.val y.val))
                      ((:v_label :v_label) (equal look.val y.val))
                      ((:v_record :v_record) (equal look.rec y.rec))
                      ((:v_array :v_array) (equal look.arr y.arr))
                      (- nil)))))
  :hints (("goal" :use ((:instance val-fix-redef
                         (x (v_array-nth k x)))
                        (:instance val-fix-redef
                         (x y)))
           :in-theory (disable v_int-of-fields
                               v_bitvector-of-fields
                               v_real-of-fields
                               v_record-of-fields
                               v_bool-of-fields
                               v_label-of-fields
                               v_string-of-fields
                               v_array-of-fields
                               equal-of-v_int
                               (:REWRITE EQUAL-OF-V_ARRAY)
                               (:REWRITE EQUAL-OF-V_BITVECTOR)
                               (:REWRITE EQUAL-OF-V_BOOL)
                               (:REWRITE EQUAL-OF-V_LABEL)
                               (:REWRITE EQUAL-OF-V_REAL)
                               (:REWRITE EQUAL-OF-V_RECORD)
                               (:REWRITE EQUAL-OF-V_STRING)))))




(define v_array-compare-update-nths (keys lst1 lst2 x xlen)
  :verify-guards nil
  (if (atom keys)
      t
    (and (if (and (natp (car keys))
                  (< (car keys) xlen))
             (equal (let ((look (hons-assoc-equal (car keys) lst1)))
                  (if look (val-fix (cdr look)) (v_array-nth (car keys) x)))
                (let ((look (hons-assoc-equal (car keys) lst2)))
                  (if look (val-fix (cdr look)) (v_array-nth (car keys) x))))
           t)
         (v_array-compare-update-nths (cdr keys) lst1 lst2 x xlen)))
  ///
  (defthmd v_array-compare-update-nths-correct
    (implies (and (v_array-compare-update-nths keys lst1 lst2 x xlen)
                  (equal xlen (v_array-len x))
                  (natp key)
                  (member-equal key keys))
             (equal (equal (v_array-nth key (v_array-update-nths lst1 x))
                           (v_array-nth key (v_array-update-nths lst2 x)))
                    t))
    :hints (("goal" :induct (v_array-compare-update-nths keys lst1 lst2 x xlen))))

  (local (in-theory (enable v_array-compare-update-nths-correct)))

  (local (include-book "std/lists/nth" :dir :system)) ;; fr equal-by-nths
  (local (defthm equal-of-v_arrays
           (implies (and (val-p x) (val-p y)
                         (val-case x :v_array)
                         (val-case y :v_array))
                    (equal (Equal x y)
                           (equal (v_array->arr x) (v_array->arr y))))
           :hints (("goal" :use ((:instance equal-of-v_array
                                  (arr (v_array->arr y))))
                    :in-theory (disable equal-of-v_array)))))

  (local (defthm nth-of-v_array->arr
           (implies (< (nfix n) (v_array-len x))
                    (equal (nth n (v_array->arr x))
                           (v_array-nth n x)))
           :hints(("Goal" :in-theory (enable v_array-nth
                                             v_array-len)))))
  (local (defthm len-of-v_array->arr
           (equal (len (v_array->arr x))
                  (v_array-len x))
           :hints(("Goal" :in-theory (enable v_array-len)))))

  (local (in-theory (disable v_array-nth-of-v_array-update-nths)))

  (local (Defthm member-append
           (iff (member-equal k (append x y))
                (or (member-equal k x) (member-equal k y)))))
  (local (include-book "std/alists/alist-keys" :dir :System))
  
  (defthmd v_array-update-nths-equal-by-compare
    (implies (v_array-compare-update-nths (append (acl2::alist-keys lst1)
                                                  (acl2::alist-keys lst2))
                                          lst1 lst2 x (v_array-len x))
             (equal (v_array-update-nths lst1 x)
                    (v_array-update-nths lst2 x)))
    :hints ((and stable-under-simplificationp
                 (acl2::equal-by-nths-hint))
            (and stable-under-simplificationp
                 '(:cases ((member-equal acl2::n (append (acl2::alist-keys lst1) (acl2::alist-keys lst2))))))
            (and stable-under-simplificationp
                 '(:in-theory (enable v_array-nth-of-v_array-update-nths))))))

(define v_array-compare-update-nths-badguy (keys lst1 lst2 x xlen)
  :verify-guards nil
  (if (atom keys)
      nil
    (if (if (and (natp (car keys))
                 (< (car keys) xlen))
            (equal (let ((look (hons-assoc-equal (car keys) lst1)))
                     (if look (val-fix (cdr look)) (v_array-nth (car keys) x)))
                   (let ((look (hons-assoc-equal (car keys) lst2)))
                     (if look (val-fix (cdr look)) (v_array-nth (car keys) x))))
          t)
        (v_array-compare-update-nths-badguy (cdr keys) lst1 lst2 x xlen)
      (car keys)))
  ///
  (defthm v_array-compare-update-nths-badguy-correct
    (implies (and (not (v_array-compare-update-nths keys lst1 lst2 x xlen))
                  (equal xlen (v_array-len x)))
             (let ((key (v_array-compare-update-nths-badguy keys lst1 lst2 x xlen)))
               (and (natp key)
                    (member-equal key keys)
                    (not (equal (v_array-nth key (v_array-update-nths lst1 x))
                                (v_array-nth key (v_array-update-nths lst2 x)))))))
    :hints (("goal" :induct (v_array-compare-update-nths keys lst1 lst2 x xlen)
             :in-theory (enable (:d v_array-compare-update-nths)
                                (:i v_array-compare-update-nths))))))


(fgl::def-fgl-rewrite v_array-update-nths-compare-base
  (implies (and (val-p x)
                (val-case x :v_array))
           (equal (equal (v_array-update-nths lst x) x)
                  (v_array-compare-update-nths (acl2::alist-keys lst) lst nil x (v_array-len x))))
  :hints (("goal" :use ((:instance v_array-update-nths-equal-by-compare (lst1 lst) (lst2 nil))
                        (:instance v_array-compare-update-nths-badguy-correct
                         (keys (acl2::alist-keys lst))
                         (lst1 lst) (lst2 nil) (xlen (v_array-len x)))))))

(fgl::def-fgl-rewrite v_array-update-nths-compare-writes
  (equal (equal (v_array-update-nths lst1 x) (v_array-update-nths lst2 x))
         (v_array-compare-update-nths (append (acl2::alist-keys lst1) (acl2::alist-keys lst2)) lst1 lst2 x (v_array-len x)))
  :hints (("goal" :use ((:instance v_array-update-nths-equal-by-compare)
                        (:instance v_array-compare-update-nths-badguy-correct
                         (keys (append (acl2::alist-keys lst1) (acl2::alist-keys lst2))) (xlen (v_array-len x)))))))



(local (defthm ty-satisfied-of-nth-when-array-type-satisfied
         (implies (and (array-type-satisfied arr ty)
                       (< (nfix n) (len arr)))
                  (ty-satisfied (nth n arr) ty))
         :hints(("Goal" :in-theory (enable nth array-type-satisfied)))))

(fgl::def-fgl-brewrite bind-ty-satisfied-of-v_array-nth
  (b* (((t_array a) (ty->desc ty)))
    (implies (and (bind-ty-satisfied ty x)
                  (syntaxp (fgl::fgl-object-case ty :g-concrete))
                  (type_desc-case (ty->desc ty) :t_array)
                  (array_index-case a.index :arraylength_expr)
                  (b* (((arraylength_expr len) a.index)) (int-literal-expr-p len.length))
                  (b* (((arraylength_expr len) a.index))
                    (fgl-mark 'n-less-than-len.length (fgl::fgl-validity-check (fgl::make-fgl-ipasir-config)
                                                                               (and (< (nfix n) (int-literal-expr->val len.length)) t))))
                  ;; Note: This triggers the constraint generation for
                  ;; resolving equal indices -- see below.
                  (not (fgl::trigger-constraints (v_array-nth-resolve-equal-indices n x a.type)))
                  (equal type a.type))
             (equal (bind-ty-satisfied type (v_array-nth n x)) type)))
  :hints(("Goal" :in-theory (enable bind-ty-satisfied
                                    v_array-nth)
          :expand ((ty-satisfied x ty)))))




;; ----------------------------------------------------------------------------------
;; Try to constrain lookups of symbolic indices in arrays so that if multiple
;; indices are equal, then the values of the lookups are equal.
;; This is tricky because the symbolic forms of the lookups depend on their types.
;; So we need to replicate some of the work done in bind-ty-satisfied-of-v_array-nth
;; in order to figure out how to represent the values.

(define v_array-nth-constraint-body (n m x type)
  :enabled t
  :verify-guards nil
  (if (equal n m)
      (let ((xn (ty-fix-val (v_array-nth n x) type))
            (xm (ty-fix-val (v_array-nth m x) type)))
        ;; (fgl::fgl-prog2 (fgl::syntax-interp
        ;;                  (fmt-to-comment-window
        ;;                   "Adding v_array-nth constraint for ~x0 indices ~x1, ~x2 (type ~x3)~%"
        ;;                   `((#\0 . ,x) (#\1 . ,n) (#\2 . ,m) (#\3 . ,type)) 0 '(nil 7 12 nil) nil))
        (equal xn xm))
    t))

(fgl::def-fgl-boolean-constraint v_array-nth-equal-indices-constraint
  :bindings ((tr1 (v_array-nth-resolve-equal-indices n x type))
             (tr2 (v_array-nth-resolve-equal-indices m x type)))
  :body (and tr1 tr2
             (if (fgl::syntax-bind skip (or (equal n m)
                                            (<< m n)
                                            (and (fgl::fgl-object-case n :g-concrete)
                                                 (fgl::fgl-object-case m :g-concrete))))
                 t
               (v_array-nth-constraint-body n m x type))))






(fgl::def-fgl-rewrite v_array-len-when-bind-ty-satisfied
  (b* (((t_array ta) (ty->desc ty))
       ((arraylength_expr ta.index)))
    (implies (and (bind-ty-satisfied ty x)
                  (syntaxp (fgl::fgl-object-case ty :g-concrete))
                  (type_desc-case ta :t_array)
                  (array_index-case ta.index :arraylength_expr)
                  (int-literal-expr-p ta.index.length))
             (equal (v_array-len x)
                    (int-literal-expr->val ta.index.length))))
  :hints(("Goal" :in-theory (enable bind-ty-satisfied
                                    v_array-len)
          :expand ((ty-satisfied x ty)))))

(local (defthmd nth-when-out-of-bounds
         (implies (not (< (nfix n) (len x)))
                  (equal (nth n x) nil))))

(fgl::def-fgl-rewrite v_array-nth-val-p
  (val-p (v_array-nth n x))
  :hints(("Goal" :in-theory (enable v_array-nth))))



(fgl::add-fgl-rewrite vallist-p-of-v_array->arr)

(fgl::add-fgl-rewrite v_array-of-fields)

(fgl::def-ctrex-rule v_array-nth-ctrex-rule
  :match ((val (v_array-nth n x)))
  :assign (v_array (if (< n (expt 2 18))
                       (progn$ (and (<= (expt 2 16) n)
                                    (fmt-to-comment-window
                                     "Problem in ctrex? Tried to update index ~x0 of array ~x1 with value ~x2. Targets: ~x3~%"
                                     `((#\0 . ,n) (#\1 . ,x) (#\2 . ,val) (#\3 . ,fgl::ctrex-targets)) 0 '(nil 7 12 nil) nil))
                               (update-nth n val (v_array->arr x)))
                     (progn$ (cw "Problem in ctrex? Tried to update index ~x0 of an array~%" n)
                             (v_array->arr x))))
  :assigned-var x
  :ruletype :property)

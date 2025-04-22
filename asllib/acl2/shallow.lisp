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

(include-book "proof-utils")
(include-book "openers")
(include-book "centaur/meta/let-abs" :dir :System)
(local (include-book "centaur/bitops/ihsext-basics" :dir :System))
(local (include-book "centaur/vl/util/default-hints" :dir :system))
(local (in-theory (disable loghead unsigned-byte-p)))
(local (std::add-default-post-define-hook :fix))
(local (in-theory (acl2::disable* openers)))

(define keyword-fix ((x keywordp))
  :returns (new-x keywordp
                  :hints(("Goal" :in-theory (enable member-symbol-name))))
  (mbe :logic (intern-in-package-of-symbol
               (symbol-name (acl2::symbol-fix x))
               :keyword-pkg)
       :exec x)
  ///
  (defthm keyword-fix-when-keywordp
    (implies (keywordp x)
             (equal (keyword-fix x) x)))

  (fty::deffixtype keyword :pred keywordp :fix keyword-fix :equiv keyword-equiv :define t))

(define id-to-native ((x identifier-p))
  :returns (sym keywordp
                :hints(("Goal" :in-theory (enable member-symbol-name))))
  (intern-in-package-of-symbol
   (identifier->val x) :keyword-pkg)
  ///
  (defret symbol-name-of-id-to-native
    (equal (symbol-name sym)
           (identifier-fix x))
    :hints(("Goal" :in-theory (enable identifier-fix
                                      identifier->val)))))

(local (in-theory (disable keywordp)))

(fty::deflist keywordlist :elt-type keyword :true-listp t)

(fty::defalist keyword-alist :key-type keyword :true-listp t)

(define idlist-to-native ((x identifierlist-p))
  :returns (new-x keywordlist-p)
  (if (atom x)
      nil
    (cons (id-to-native (car x))
          (idlist-to-native (cdr x))))
  ///
  (defret len-of-<fn>
    (equal (len new-x) (len x))))

(make-event
 (b* (((er &) (set-evisc-tuple '(nil 6 8 nil) :sites :gag-mode :iprint :same)))
   (value '(value-triple :evisc))))






(defines val-to-native
  (define val-to-native ((x val-p))
    :measure (val-count x)
    (val-case x
      (:v_int x.val)
      (:v_real x.val)
      (:v_bool x.val)
      (:v_string x.val)
      (:v_bitvector x.val)
      (:v_label (id-to-native x.val))
      (:v_record (val-imap-to-native x.rec))
      (:v_array (vallist-to-native x.arr))))
  (define val-imap-to-native ((x val-imap-p))
    :measure (val-imap-count x)
    :returns (new-x keyword-alist-p)
    (b* ((x (val-imap-fix x))
         ((when (atom x)) nil)
         ((cons key val) (car x)))
      (cons (cons (id-to-native key) (val-to-native val))
            (val-imap-to-native (cdr x)))))
  (define vallist-to-native ((x vallist-p))
    :measure (vallist-count x)
    :Returns (new-x true-listp :rule-classes :type-prescription)
    (if (atom x)
        nil
      (cons (val-to-native (car x))
            (vallist-to-native (cdr x)))))
  ///

  (defret len-of-vallist-to-native
    (equal (len new-x) (len x))
    :hints(("Goal" :induct (len x)
            :expand ((vallist-to-native x))))
    :fn vallist-to-native)

  (std::defret-mutual keys-of-val-imap-to-native
    (defret keys-of-val-imap-to-native
      (equal (acl2::alist-keys new-x)
             (idlist-to-native (acl2::alist-keys (val-imap-fix x))))
      :hints ('(:in-theory (enable acl2::alist-keys
                                   val-imap-fix)
                :expand ((val-imap-to-native x)
                         (val-imap-fix x)
                         (:free (a b) (idlist-to-native (cons a b))))))
      :fn val-imap-to-native)
    :skip-others t)

  (std::defret-mutual vals-of-val-imap-to-native
    (defret vals-of-val-imap-to-native
      (equal (acl2::alist-vals new-x)
             (vallist-to-native (acl2::alist-vals (val-imap-fix x))))
      :hints ('(:in-theory (enable acl2::alist-vals
                                   val-imap-fix)
                :expand ((val-imap-to-native x)
                         (val-imap-fix x)
                         (:free (a b) (vallist-to-native (cons a b))))))
      :fn val-imap-to-native)
    :skip-others t)

  (fty::deffixequiv-mutual val-to-native)
  
  (local (in-theory (enable val-to-native)))

  (acl2::defopen val-to-native-when-v_int (val-to-native x) :hyp (val-case x :v_int) :hint (:expand ((val-to-native x))))
  (acl2::defopen val-to-native-when-v_real (val-to-native x) :hyp (val-case x :v_real) :hint (:expand ((val-to-native x))))
  (acl2::defopen val-to-native-when-v_bool (val-to-native x) :hyp (val-case x :v_bool) :hint (:expand ((val-to-native x))))
  (acl2::defopen val-to-native-when-v_string (val-to-native x) :hyp (val-case x :v_string) :hint (:expand ((val-to-native x))))
  (acl2::defopen val-to-native-when-v_bitvector (val-to-native x) :hyp (val-case x :v_bitvector) :hint (:expand ((val-to-native x))))
  (acl2::defopen val-to-native-when-v_label (val-to-native x) :hyp (val-case x :v_label) :hint (:expand ((val-to-native x))))
  (acl2::defopen val-to-native-when-v_record (val-to-native x) :hyp (val-case x :v_record) :hint (:expand ((val-to-native x))))
  (acl2::defopen val-to-native-when-v_array (val-to-native x) :hyp (val-case x :v_array) :hint (:expand ((val-to-native x))))
  
  (defthm val-to-native-of-v_int
    (equal (val-to-native (v_int x)) (ifix x)))

  (defthm val-to-native-of-v_real
    (equal (val-to-native (v_real x)) (rfix x)))

  (defthm val-to-native-of-v_bool
    (equal (val-to-native (v_bool x)) (acl2::bool-fix x)))

  (defthm val-to-native-of-v_string
    (equal (val-to-native (v_string x)) (acl2::str-fix x)))

  (defthm val-to-native-of-v_bitvector
    (equal (val-to-native (v_bitvector n x)) (loghead n x)))

  (defthm val-to-native-of-v_label
    (equal (val-to-native (v_label x)) (id-to-native x)))

  (defthm val-to-native-of-v_record
    (equal (val-to-native (v_record x)) (val-imap-to-native x))
    :hints (("goal" :expand ((val-to-native (v_record x))))))

  (defthm val-to-native-of-v_array
    (equal (val-to-native (v_array x)) (vallist-to-native x))
    :hints (("goal" :expand ((val-to-native (v_array x))))))

  (defthm vallist-to-native-of-cons
    (equal (vallist-to-native (cons a b))
           (cons (val-to-native a)
                 (vallist-to-native b)))
    :hints (("goal" :expand ((vallist-to-native (cons a b)))))))



(local
 (defthm vallist-p-alist-vals-of-val-imap
   (implies (val-imap-p x)
            (vallist-p (acl2::alist-vals X)))
   :hints(("Goal" :in-theory (enable acl2::alist-vals)))))



(defines weak-ty-satisfied
  :flag-local nil
  (define weak-ty-satisfied ((x val-p)
                             (ty ty-p))
    :measure (acl2::two-nats-measure (ty-count ty) 0)
    (b* ((ty (ty->val ty)))
      (fty::multicase ((type_desc-case ty)
                       (val-case x))
        ((:t_int :v_int) t)
        ((:t_bits :v_bitvector) t)
        ((:t_real :v_real) t)
        ((:t_string :v_string) t)
        ((:t_bool :v_bool) t)
        ((:t_enum :v_label) (member-equal x.val ty.elts))
        ((:t_tuple :v_array) (weak-tuple-type-satisfied x.arr ty.types))
        ((:t_array :v_array)
         :when (array_index-case ty.index :arraylength_expr)
         (weak-array-type-satisfied x.arr ty.type))
        ((:t_array :v_record)
         :when (array_index-case ty.index :arraylength_enum)
         (and (equal (acl2::alist-keys x.rec) (arraylength_enum->elts ty.index))
              (weak-array-type-satisfied (acl2::alist-vals x.rec) ty.type)))
        ((:t_record :v_record)
         (weak-record-type-satisfied x.rec ty.fields))
        ((:t_exception :v_record)
         (weak-record-type-satisfied x.rec ty.fields))
        ((:t_collection :v_record)
         (weak-record-type-satisfied x.rec ty.fields))
        (- nil))))

  (define weak-tuple-type-satisfied ((x vallist-p)
                                     (types tylist-p))
    :measure (acl2::two-nats-measure (tylist-count types) 0)
    (if (atom types)
        (atom x)
      (and (consp x)
           (weak-ty-satisfied (car x) (car types))
           (weak-tuple-type-satisfied (cdr x) (Cdr types)))))

  (define weak-array-type-satisfied ((x vallist-p)
                                     (ty ty-p))
    :measure (acl2::two-nats-measure (ty-count ty) (len x))
    (if (atom x)
        t
      (and (weak-ty-satisfied (car x) ty)
           (weak-array-type-satisfied (cdr x) ty))))

  (define weak-record-type-satisfied ((x val-imap-p)
                                      (fields typed_identifierlist-p))
    :measure (acl2::two-nats-measure (typed_identifierlist-count fields) 0)
    (b* ((x (val-imap-fix x)))
      (if (atom fields)
          (atom x)
        (and (consp x)
             (consp (car x))
             (b* (((cons key val) (car x))
                  ((typed_identifier f1) (car fields)))
               (and (equal key f1.name)
                    (weak-ty-satisfied val f1.type)))
             (weak-record-type-satisfied (cdr x) (cdr fields))))))
  ///
  (fty::deffixequiv-mutual weak-ty-satisfied)

  (defopener open-weak-ty-satisfied weak-ty-satisfied
    :hyp (syntaxp (or (quotep ty)
                      (case-match ty
                        (('ty (ctor . &))
                         (member-eq ctor
                                    '(t_int t_bits t_real t_string t_bool t_enum
                                            t_tuple t_array t_record t_exception
                                            t_collection t_named)))
                        (& nil)))))
  (defopener open-weak-tuple-type-satisfied weak-tuple-type-satisfied :hyp (syntaxp (or (quotep types)
                                                                                        (case-match types
                                                                                          (('cons . &) t) (& nil)))))
  (defopener open-weak-array-type-satisfied weak-array-type-satisfied)
  (defopener open-weak-record-type-satisfied weak-record-type-satisfied :hyp (syntaxp (or (quotep fields)
                                                                                          (case-match fields
                                                                                            (('cons . &) t) (& nil))))))




(local (in-theory (acl2::disable* openers)))


(defines weak-ty-satisfied-native
  :flag-local nil
  (define weak-ty-satisfied-native (x (ty ty-p))
    :measure (acl2::two-nats-measure (ty-count ty) 0)
    ;; :prepwork ((local (in-theory (enable integerp*))))
    (b* ((ty (ty->val ty)))
      (type_desc-case ty
        (:t_int (integerp x))
        (:t_bits ;;(and (integerp* x)
         (integerp x)) ;; ? or do we want just integerp?
        (:t_real (rationalp x))
        (:t_string (stringp x))
        (:t_bool (booleanp x))
        (:t_enum (and (keywordp x)
                      (member-equal (symbol-name x) ty.elts)))
        (:t_tuple (weak-tuple-type-satisfied-native x ty.types))
        (:t_array (array_index-case ty.index
                    :arraylength_expr
                    (and (true-listp x)
                         (weak-array-type-satisfied-native x ty.type))
                    :arraylength_enum
                    (and (keyword-alist-p x)
                         (equal (acl2::alist-keys x) (idlist-to-native (arraylength_enum->elts ty.index)))
                         (weak-array-type-satisfied-native (acl2::alist-vals x) ty.type))))
        (:t_record (and (keyword-alist-p x)
                        (weak-record-type-satisfied-native x ty.fields)))
        (:t_exception (and (keyword-alist-p x)
                           (weak-record-type-satisfied-native x ty.fields)))
        (:t_collection (and (keyword-alist-p x)
                            (weak-record-type-satisfied-native x ty.fields)))
        (otherwise nil))))

  (define weak-tuple-type-satisfied-native (x (types tylist-p))
    :measure (acl2::two-nats-measure (tylist-count types) 0)
    (if (atom types)
        (eq x nil)
      (and (consp x)
           (weak-ty-satisfied-native (car x) (car types))
           (weak-tuple-type-satisfied-native (cdr x) (Cdr types)))))

  (define weak-array-type-satisfied-native ((x true-listp)
                                       (ty ty-p))
    :measure (acl2::two-nats-measure (ty-count ty) (len x))
    (if (atom x)
        t
      (and (weak-ty-satisfied-native (car x) ty)
           (weak-array-type-satisfied-native (cdr x) ty))))

  (define weak-record-type-satisfied-native ((x keyword-alist-p)
                                        (fields typed_identifierlist-p))
    :measure (acl2::two-nats-measure (typed_identifierlist-count fields) 0)
    (b* ((x (keyword-alist-fix x)))
      (if (atom fields)
          (atom x)
        (and (consp x)
             (consp (car x))
             (b* (((cons key val) (car x))
                  ((typed_identifier f1) (car fields)))
               (and (equal key (id-to-native f1.name))
                    (weak-ty-satisfied-native val f1.type)))
             (weak-record-type-satisfied-native (cdr x) (cdr fields))))))
  ///
  (fty::deffixequiv-mutual weak-ty-satisfied-native)

  (defthm-weak-ty-satisfied-flag
    (defthm weak-ty-satisfied-native-of-val-to-native
      (implies (and (weak-ty-satisfied x ty))
               (weak-ty-satisfied-native (val-to-native x) ty))
      :hints ('(:expand ((val-to-native x)
                         (:free (x) (weak-ty-satisfied x ty))
                         (:free (x) (weak-ty-satisfied-native x ty)))))
      :flag weak-ty-satisfied)
    (defthm weak-tuple-type-satisfied-native-of-val-to-native
      (implies (and (weak-tuple-type-satisfied x types))
               (weak-tuple-type-satisfied-native (vallist-to-native x) types))
      :hints ('(:expand ((vallist-to-native x)
                         (:free (x) (weak-tuple-type-satisfied x types))
                         (:free (x) (weak-tuple-type-satisfied-native x types)))))
      :flag weak-tuple-type-satisfied)

    (defthm weak-array-type-satisfied-native-of-val-to-native
      (implies (and (weak-array-type-satisfied x ty))
               (weak-array-type-satisfied-native (vallist-to-native x) ty))
      :hints ('(:expand ((vallist-to-native x)
                         (weak-array-type-satisfied x ty)
                         (weak-array-type-satisfied-native nil ty)
                         (:free (a b) (weak-array-type-satisfied-native (cons a b) ty)))))
      :flag weak-array-type-satisfied)

    (defthm weak-record-type-satisfied-native-of-val-to-native
      (implies (weak-record-type-satisfied x fields)
               (weak-record-type-satisfied-native (val-imap-to-native x) fields))
      :hints ('(:expand ((val-imap-to-native x)
                         (:free (x) (weak-record-type-satisfied x fields))
                         (:free (x) (weak-record-type-satisfied-native x fields)))))
      :flag weak-record-type-satisfied))


  (defthm-weak-ty-satisfied-native-flag
    (defthm weak-tuple-type-satisfied-native-implies-true-listp
      (implies (weak-tuple-type-satisfied-native x types)
               (and (true-listp x)
                    (equal (len x) (len types))))
      :hints ('(:expand ((weak-tuple-type-satisfied-native x types))))
      :rule-classes :forward-chaining
      :flag weak-tuple-type-satisfied-native)
    :skip-others t))


(defines typed-val-to-native
  (define typed-val-to-native ((x val-p) (ty ty-p))
    :guard (weak-ty-satisfied x ty)
    :guard-hints (("goal" :expand ((weak-tuple-type-satisfied x types)
                                   (weak-array-type-satisfied x ty)
                                   (weak-record-type-satisfied x fields)
                                   (weak-ty-satisfied x ty))))
    :measure (acl2::two-nats-measure (ty-count ty) 0)
    :returns (val)
    (b* ((ty (ty->val ty)))
      (type_desc-case ty
        :t_int (v_int->val x)
        :t_bits (v_bitvector->val x)
        :t_real (v_real->val x)
        :t_bool (v_bool->val x)
        :t_string (v_string->val x)
        :t_enum (id-to-native (v_label->val x))
        :t_tuple (tuple-val-to-native (v_array->arr x) ty.types)
        :t_array (array_index-case ty.index
                   :arraylength_expr (typed-vallist-to-native (v_array->arr x) ty.type)
                   :arraylength_enum (pairlis$ (idlist-to-native ty.index.elts)
                                               (typed-vallist-to-native (acl2::alist-vals (v_record->rec x)) ty.type)))
        :t_record (typed-val-imap-to-native (v_record->rec x) ty.fields)
        :t_exception (typed-val-imap-to-native (v_record->rec x) ty.fields)
        :t_collection (typed-val-imap-to-native (v_record->rec x) ty.fields)
        :t_named nil)))
  (define tuple-val-to-native ((x vallist-p)
                               (types tylist-p))
    :guard (weak-tuple-type-satisfied x types)
    :measure (acl2::two-nats-measure (tylist-count types) 0)
    :returns (val)
    (if (atom types)
        nil
      (cons (typed-val-to-native (car x) (car types))
            (tuple-val-to-native (cdr x) (cdr types)))))
  (define typed-vallist-to-native ((x vallist-p)
                                   (ty ty-p))
    :measure (acl2::two-nats-measure (ty-count ty) (len x))
    :guard (weak-array-type-satisfied x ty)
    :returns (val)
    (if (atom x)
        nil
      (cons (typed-val-to-native (car x) ty)
            (typed-vallist-to-native (cdr x) ty))))
  (define typed-val-imap-to-native ((x val-imap-p) (fields typed_identifierlist-p))
    :measure (acl2::two-nats-measure (typed_identifierlist-count fields) 0)
    :guard (weak-record-type-satisfied x fields)
    :returns (val)
    (b* (((when (atom fields)) nil)
         ((typed_identifier f1) (car fields))
         (val (cdar x)))
      (cons (cons (id-to-native f1.name) (typed-val-to-native val f1.type))
            (typed-val-imap-to-native (cdr x) (cdr fields)))))
      
  ///

  (local (defthm pairlis$-equals-val-imap-to-native
           (implies (val-imap-p x)
                    (equal (pairlis$ (idlist-to-native (acl2::alist-keys x))
                                     (vallist-to-native (acl2::alist-vals x)))
                           (val-imap-to-native x)))
           :hints(("Goal" :in-theory (enable acl2::alist-vals
                                             acl2::alist-keys
                                             val-imap-to-native
                                             idlist-to-native
                                             vallist-to-native)))))
  
  (std::defret-mutual <fn>-is-val-to-native
    (defretd <fn>-is-val-to-native
      (implies (weak-ty-satisfied x ty)
               (equal val (val-to-native x)))
      :hints ('(:expand ((weak-ty-satisfied x ty)
                         (val-to-native x)
                         <call>)))
      :fn typed-val-to-native)
    (defretd <fn>-is-val-to-native
      (implies (weak-tuple-type-satisfied x types)
               (equal val (vallist-to-native x)))
      :hints ('(:expand ((weak-tuple-type-satisfied x types)
                         (vallist-to-native x)
                         <call>)))
      :fn tuple-val-to-native)
    (defretd <fn>-is-val-to-native
      (implies (weak-array-type-satisfied x ty)
               (equal val (vallist-to-native x)))
      :hints ('(:expand ((weak-array-type-satisfied x ty)
                         (vallist-to-native x)
                         <call>)))
      :fn typed-vallist-to-native)
    (defretd <fn>-is-val-to-native
      (implies (and (weak-record-type-satisfied x fields)
                    (val-imap-p x))
               (equal val (val-imap-to-native x)))
      :hints ('(:expand ((weak-record-type-satisfied x fields)
                         (val-imap-to-native x)
                         <call>)))
      :fn typed-val-imap-to-native))

  (defopener open-typed-val-to-native typed-val-to-native
    :hyp (syntaxp (or (quotep ty)
                      (case-match ty
                        (('ty (ctor . &))
                         (member-eq ctor
                                    '(t_int t_bits t_real t_string t_bool t_enum
                                            t_tuple t_array t_record t_exception
                                            t_collection t_named)))
                        (& nil)))))
  
  (defopener open-tuple-val-to-native tuple-val-to-native
    :hyp (syntaxp (or (quotep types)
                      (and (consp types) (eq (car types) 'cons))))))







(defines partial-resolve-ty
  ;; Resolves named types, bitvector widths, and array lengths but not integer constraints.
  :verify-guards nil
  (define partial-resolve-tylist ((env env-p)
                                  (x tylist-p)
                                  &key ((clk natp) 'clk) (orac 'orac))
    :returns (mv (res (and (eval_result-p res)
                           (implies (eval_result-case res :ev_normal)
                                    (tylist-p (ev_normal->res res)))))
                 new-orac)
    :measure (nats-measure clk 0 (tylist-count x) 0)
    (if (atom x)
        (evo_normal nil)
      (b* (((mv (evo first) orac) (partial-resolve-ty env (car x)))
           ((mv (evo rest) orac) (partial-resolve-tylist env (cdr x))))
        (evo_normal (cons first rest)))))

  (define partial-resolve-typed_identifierlist ((env env-p)
                                                (x typed_identifierlist-p)
                                                &key ((clk natp) 'clk) (orac 'orac))
    :returns (mv (res (and (eval_result-p res)
                           (implies (eval_result-case res :ev_normal)
                                    (typed_identifierlist-p (ev_normal->res res)))))
                 new-orac)
    :measure (nats-measure clk 0 (typed_identifierlist-count x) 0)
    (b* (((when (atom x)) (evo_normal nil))
         ((typed_identifier x1) (car x))
         ((mv (evo first) orac) (partial-resolve-ty env x1.type))
         ((mv (evo rest) orac) (partial-resolve-typed_identifierlist env (cdr x))))
      (evo_normal (cons (typed_identifier x1.name first) rest))))
    
  (define partial-resolve-ty ((env env-p)
                              (x ty-p)
                              &key ((clk natp) 'clk) (orac 'orac))
    :returns (mv (res (and (eval_result-p res)
                           (implies (eval_result-case res :ev_normal)
                                    (ty-p (ev_normal->res res)))))
                 new-orac)
    :measure (nats-measure clk 0 (ty-count x) 0)
    (b* ((ty (ty->val x)))
      (type_desc-case ty
        :t_int (evo_normal (ty (t_int (unconstrained))))
        :t_bits (b* (((mv (evo (expr_result width)) orac) (eval_expr env ty.expr)))
                  (val-case width.val
                    :v_int ;;(if (<= 0 width.val.val)
                    (evo_normal (ty (t_bits
                                     (expr (e_literal (l_int width.val.val)))
                                     ty.fields)))
                    ;; NOTE -- separation of concerns: we once threw an error if we resolved
                    ;; the bitvector width to a negative value. But instead we'll
                    ;; rely on the consumer of this type to deal with it.
                    ;; (evo_error "Negative bitvector width resolving type" x))
                    :otherwise (evo_error "Unexpected type of bitvector width type" x)))
        :t_tuple (b* (((mv (evo tys) orac) (partial-resolve-tylist env ty.types)))
                   (evo_normal (ty (t_tuple tys))))
        :t_array (b* (((mv (evo base) orac) (partial-resolve-ty env ty.type)))
                   (array_index-case ty.index
                     :arraylength_expr (b* (((mv (evo (expr_result len)) orac) (eval_expr env ty.index.length)))
                                         (val-case len.val
                                           :v_int ;;(if (<= 0 len.val.val)
                                           (evo_normal (ty (t_array
                                                            (arraylength_expr
                                                             (expr (e_literal (l_int len.val.val))))
                                                            base)))
                                           ;; (evo_error "Negative array length resolving type" x))
                                           :otherwise (evo_error "Unexpected type of array length" x)))
                     :arraylength_enum (evo_normal (ty (t_array ty.index base)))))
        :t_record (b* (((mv (evo fields) orac)
                        (partial-resolve-typed_identifierlist env ty.fields)))
                    (evo_normal (ty (t_record fields))))
        :t_exception (b* (((mv (evo fields) orac)
                           (partial-resolve-typed_identifierlist env ty.fields)))
                       (evo_normal (ty (t_exception fields))))
        :t_collection (b* (((mv (evo fields) orac)
                            (partial-resolve-typed_identifierlist env ty.fields)))
                        (evo_normal (ty (t_collection fields))))
        :t_named  (b* ((decl_types (static_env_global->declared_types
                                    (global-env->static (env->global env))))
                       (look (hons-assoc-equal ty.name decl_types))
                       ((unless look)
                        (evo_error "Named type not found" x))
                       ((when (zp clk))
                        (evo_error "Clock ran out resolving named type" x))
                       (type (ty-timeframe->ty (cdr look))))
                    (partial-resolve-ty env type :clk (1- clk)))
        :otherwise (evo_normal (ty ty)))))
  ///
  (verify-guards partial-resolve-ty-fn)
  (defopener open-partial-resolve-ty partial-resolve-ty :hyp (syntaxp (quotep x)))
  (defopener open-partial-resolve-tylist partial-resolve-tylist :hyp (syntaxp (quotep x)))
  (defopener open-partial-resolve-typed_identifierlist partial-resolve-typed_identifierlist :hyp (syntaxp (quotep x))))

(local (in-theory (acl2::disable* openers)))








;; (define integerp* (x)
;;   (integerp x)
;;   ///
;;   (defthm integerp-when-integerp*
;;     (implies (integerp* x)
;;              (integerp x))))

(define loghead* ((n integerp) (x integerp))
  :returns (new-x integerp :rule-classes :type-prescription)
  (loghead (nfix n) x)
  ///
  (defret unsigned-byte-p-of-loghead*
    (implies (and (<= (ifix n) m)
                  (natp m))
             (unsigned-byte-p m new-x)))
  (defret loghead*-when-unsigned-byte-p
    (implies (unsigned-byte-p n x)
             (equal new-x x))))
;; (define unsigned-byte-p* (n x)
;;   (unsigned-byte-p (nfix n) x)
;;   ///
;;   (local (defthm ifix-nfix
;;            (<= (ifix x) (nfix x))
;;            :hints(("Goal" :in-theory (enable ifix nfix)))))
;;   (defthm unsigned-byte-p*-of-loghead*
;;     (unsigned-byte-p* n (loghead* n x)))

;;   (defthm loghead*-when-unsigned-byte-p*
;;     (implies (unsigned-byte-p* n x)
;;              (equal (loghead* n x) x))
;;     :hints(("Goal" :in-theory (e/d (loghead*)
;;                                    (acl2::loghead-identity))
;;             :use ((:instance acl2::loghead-identity
;;                    (size (nfix n)) (i x)))))))

(defines ty-satisfied-native
  :flag-local nil
  (define ty-satisfied-native (x (ty ty-p))
    :guard (ty-resolved-p ty)
    :measure (acl2::two-nats-measure (ty-count ty) 0)
    ;; :prepwork ((local (in-theory (enable integerp*))))
    (b* ((ty (ty->val ty)))
      (type_desc-case ty
        (:t_int (and (integerp x)
                     (constraint_kind-satisfied x ty.constraint)))
        (:t_bits ;;(and (integerp* x)
         (unsigned-byte-p (int-literal-expr->val ty.expr) x))
        (:t_real (rationalp x))
        (:t_string (stringp x))
        (:t_bool (booleanp x))
        (:t_enum (and (keywordp x)
                      (member-equal (symbol-name x) ty.elts)))
        (:t_tuple (tuple-type-satisfied-native x ty.types))
        (:t_array (array_index-case ty.index
                    :arraylength_expr
                    (and (true-listp x)
                         (eql (len x) (int-literal-expr->val ty.index.length))
                         (array-type-satisfied-native x ty.type))
                    :arraylength_enum
                    (and (keyword-alist-p x)
                         (equal (acl2::alist-keys x) (idlist-to-native (arraylength_enum->elts ty.index)))
                         (array-type-satisfied-native (acl2::alist-vals x) ty.type))))
        (:t_record (and (keyword-alist-p x)
                        (record-type-satisfied-native x ty.fields)))
        (:t_exception (and (keyword-alist-p x)
                           (record-type-satisfied-native x ty.fields)))
        (:t_collection (and (keyword-alist-p x)
                            (record-type-satisfied-native x ty.fields)))
        (otherwise nil))))

  (define tuple-type-satisfied-native (x (types tylist-p))
    :guard (tylist-resolved-p types)
    :measure (acl2::two-nats-measure (tylist-count types) 0)
    (if (atom types)
        (eq x nil)
      (and (consp x)
           (ty-satisfied-native (car x) (car types))
           (tuple-type-satisfied-native (cdr x) (Cdr types)))))

  (define array-type-satisfied-native ((x true-listp)
                                       (ty ty-p))
    :guard (ty-resolved-p ty)
    :measure (acl2::two-nats-measure (ty-count ty) (len x))
    (if (atom x)
        t
      (and (ty-satisfied-native (car x) ty)
           (array-type-satisfied-native (cdr x) ty))))

  (define record-type-satisfied-native ((x keyword-alist-p)
                                        (fields typed_identifierlist-p))
    :guard (typed_identifierlist-resolved-p fields)
    :measure (acl2::two-nats-measure (typed_identifierlist-count fields) 0)
    (b* ((x (keyword-alist-fix x)))
      (if (atom fields)
          (atom x)
        (and (consp x)
             (consp (car x))
             (b* (((cons key val) (car x))
                  ((typed_identifier f1) (car fields)))
               (and (equal key (id-to-native f1.name))
                    (ty-satisfied-native val f1.type)))
             (record-type-satisfied-native (cdr x) (cdr fields))))))
  ///
  (fty::deffixequiv-mutual ty-satisfied-native)


  (defthm-ty-satisfied-flag
    (defthm ty-satisfied-native-of-val-to-native
      (implies (and (ty-satisfied x ty)
                    (ty-resolved-p ty))
               (ty-satisfied-native (val-to-native x) ty))
      :hints ('(:expand ((val-to-native x)
                         (:free (x) (ty-satisfied x ty))
                         (:free (x) (ty-satisfied-native x ty)))))
      :flag ty-satisfied)
    (defthm tuple-type-satisfied-native-of-val-to-native
      (implies (and (tuple-type-satisfied x types)
                    (tylist-resolved-p types))
               (tuple-type-satisfied-native (vallist-to-native x) types))
      :hints ('(:expand ((vallist-to-native x)
                         (:free (x) (tuple-type-satisfied x types))
                         (:free (x) (tuple-type-satisfied-native x types)))))
      :flag tuple-type-satisfied)

    (defthm array-type-satisfied-native-of-val-to-native
      (implies (and (array-type-satisfied x ty)
                    (ty-resolved-p ty))
               (array-type-satisfied-native (vallist-to-native x) ty))
      :hints ('(:expand ((vallist-to-native x)
                         (array-type-satisfied x ty)
                         (array-type-satisfied-native nil ty)
                         (:free (a b) (array-type-satisfied-native (cons a b) ty)))))
      :flag array-type-satisfied)

    (defthm record-type-satisfied-native-of-val-to-native
      (implies (and (record-type-satisfied x fields)
                    (typed_identifierlist-resolved-p fields))
               (record-type-satisfied-native (val-imap-to-native x) fields))
      :hints ('(:expand ((val-imap-to-native x)
                         (:free (x) (record-type-satisfied x fields))
                         (:free (x) (record-type-satisfied-native x fields)))))
      :flag record-type-satisfied))


  (defthm-ty-satisfied-native-flag
    (defthm tuple-type-satisfied-native-implies-true-listp
      (implies (tuple-type-satisfied-native x types)
               (and (true-listp x)
                    (equal (len x) (len types))))
      :hints ('(:expand ((tuple-type-satisfied-native x types))))
      :rule-classes :forward-chaining
      :flag tuple-type-satisfied-native)
    :skip-others t))






(local (defthm keyword-alist-p-of-pairlis$
         (implies (keywordlist-p x)
                  (keyword-alist-p (pairlis$ x y)))))

(local (defthm keyword-list-p-alist-keys-when-keyword-alist-p
         (implies (keyword-alist-p x)
                  (keywordlist-p (acl2::alist-keys x)))
         :hints(("Goal" :in-theory (enable acl2::alist-keys)))))

(local (defthm len-equal-0
         (equal (equal (len x) 0)
                (atom x))))

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

(local (defthm alistp-when-keyword-alist-p-rw
         (implies (keyword-alist-p x)
                  (alistp x))))



(local (defthm len-of-alist-vals
         (equal (len (acl2::alist-vals x))
                (len (acl2::alist-keys x)))
         :hints(("Goal" :in-theory (enable acl2::alist-keys
                                           acl2::alist-vals)))))




(defines ty-fix-native
  :flag-local nil
  (define ty-fix-native (x (ty ty-p))
    :guard (ty-resolved-p ty)
    :verify-guards nil
    :measure (acl2::two-nats-measure (ty-count ty) 0)
    :returns (new-x (implies (ty-satisfying-val ty)
                             (ty-satisfied-native new-x ty))
                    :hints ('(:expand ((ty-satisfying-val ty)
                                       (:free (x) (ty-satisfied-native x ty))
                                       (:free (ty) (array-type-satisfied-native nil ty))))))
    (b* ((ty (ty->val ty)))
      (type_desc-case ty
        (:t_int (constraint_kind-value-fix (ifix x) ty.constraint))
        (:t_bits (loghead* (int-literal-expr->val ty.expr) (ifix x)))
        (:t_real (rfix x))
        (:t_string (acl2::str-fix x))
        (:t_bool (acl2::bool-fix x))
        (:t_enum (if (and (keywordp x)
                          (member-equal (symbol-name x) ty.elts))
                     x
                   (if (consp ty.elts)
                       (id-to-native (car ty.elts))
                     (id-to-native "EMPTY_ENUM"))))
        (:t_tuple (tuple-type-fix-native x ty.types))
        (:t_array (array_index-case ty.index
                    :arraylength_expr
                    (array-type-fix-native (nfix (int-literal-expr->val ty.index.length)) x ty.type)
                    :arraylength_enum
                    (pairlis$ (idlist-to-native ty.index.elts)
                              (array-type-fix-native
                               (len ty.index.elts)
                               (acl2::alist-vals x)
                               ty.type))))
        (:t_record (record-type-fix-native x ty.fields))
        (:t_exception (record-type-fix-native x ty.fields))
        (:t_collection (record-type-fix-native x ty.fields))
        (otherwise nil))))

  (define tuple-type-fix-native (x (types tylist-p))
    :guard (tylist-resolved-p types)
    :measure (acl2::two-nats-measure (tylist-count types) 0)
    :returns (new-x (implies (mv-nth 0 (tylist-satisfying-val types))
                             (tuple-type-satisfied-native new-x types))
                    :hints ('(:expand ((tylist-satisfying-val types)
                                       (:free (x) (tuple-type-satisfied-native x types))))))
    (if (atom types)
        nil
      (cons (ty-fix-native (and (consp x) (car x)) (car types))
            (tuple-type-fix-native (and (consp x) (cdr x)) (Cdr types)))))

  (define array-type-fix-native ((len natp) x (ty ty-p))
    :guard (ty-resolved-p ty)
    :measure (acl2::two-nats-measure (ty-count ty) len)
    :returns (new-x (and (true-listp new-x)
                         (equal (len new-x) (nfix len))
                         (implies (ty-satisfying-val ty)
                                  (array-type-satisfied-native new-x ty)))
                    :hints ((and stable-under-simplificationp
                                 '(:expand ((array-type-satisfied-native nil ty)
                                            (:free (a b) (array-type-satisfied-native (cons a b) ty)))))))
    (if (zp len)
        nil
      (cons (ty-fix-native (and (consp x) (car x)) ty)
            (array-type-fix-native (1- len) (and (consp x) (cdr x)) ty))))

  (define record-type-fix-native (x (fields typed_identifierlist-p))
    :guard (typed_identifierlist-resolved-p fields)
    :measure (acl2::two-nats-measure (typed_identifierlist-count fields) 0)
    :returns (new-x (and (keyword-alist-p new-x)
                         (implies (mv-nth 0 (typed_identifierlist-satisfying-val fields))
                                  (record-type-satisfied-native new-x fields)))
                    :hints ('(:expand ((typed_identifierlist-satisfying-val fields)
                                       (:free (x) (record-type-satisfied-native x fields))))))
    (b* (((when (atom fields)) nil)
         ((typed_identifier f1) (car fields))
         (val (ty-fix-native (and (consp x) (consp (car x)) (cdar x)) f1.type)))
      (cons (cons (id-to-native f1.name) val)
            (record-type-fix-native (and (consp x) (cdr x)) (cdr fields)))))
  ///
  (fty::deffixequiv-mutual ty-fix-native)

  (verify-guards ty-fix-native)

  (std::defret-mutual ty-fix-native-when-satisfied
    (defret <fn>-when-satisfied
      (implies (ty-satisfied-native x ty)
               (equal new-x x))
      :hints ('(:expand ((ty-satisfied-native x ty)
                         <call>)))
      :fn ty-fix-native)
    (defret <fn>-when-satisfied
      (implies (tuple-type-satisfied-native x types)
               (equal new-x x))
      :hints ('(:expand ((tuple-type-satisfied-native x types)
                         <call>)))
      :fn tuple-type-fix-native)
    (defret <fn>-when-satisfied
      (implies (and (array-type-satisfied-native x ty)
                    (equal (len x) (nfix len))
                    (true-listp x))
               (equal new-x x))
      :hints ('(:expand ((array-type-satisfied-native x ty)
                         <call>
                         (:free (x ty) (array-type-fix-native 0 x ty)))))
      :fn array-type-fix-native)
    (defret <fn>-when-satisfied
      (implies (and (record-type-satisfied-native x fields)
                    (keyword-alist-p x))
               (equal new-x x))
      :hints ('(:expand ((record-type-satisfied-native x fields)
                         <call>)))
      :fn record-type-fix-native))

  (defopener open-ty-fix-native ty-fix-native
    :hyp (syntaxp (or (quotep ty)
                      (case-match ty
                        (('ty (ctor . &))
                         (member-eq ctor
                                    '(t_int t_bits t_real t_string t_bool t_enum
                                            t_tuple t_array t_record t_exception
                                            t_collection t_named)))
                        (& nil))))))


(defines ty-names-resolved-p
  (define ty-names-resolved-p ((x ty-p))
    :measure (ty-count x)
    (b* ((ty (ty->val x)))
      (type_desc-case ty
        :t_named nil
        :t_tuple (tylist-names-resolved-p ty.types)
        :t_array (ty-names-resolved-p ty.type)
        :t_record (typed_identifierlist-names-resolved-p ty.fields)
        :t_exception (typed_identifierlist-names-resolved-p ty.fields)
        :t_collection (typed_identifierlist-names-resolved-p ty.fields)
        :otherwise t)))
  (define tylist-names-resolved-p ((x tylist-p))
    :measure (tylist-count x)
    (if (atom x)
        t
      (and (ty-names-resolved-p (car x))
           (tylist-names-resolved-p (cdr x)))))
  (define typed_identifierlist-names-resolved-p ((x typed_identifierlist-p))
    :measure (typed_identifierlist-count x)
    (if (atom x)
        t
      (and (ty-names-resolved-p (typed_identifier->type (car x)))
           (typed_identifierlist-names-resolved-p (cdr x))))))

(defines ty-no-empty-enums-p
  (define ty-no-empty-enums-p ((x ty-p))
    :measure (ty-count x)
    (b* ((ty (ty->val x)))
      (type_desc-case ty
        :t_enum (consp ty.elts)
        :t_tuple (tylist-no-empty-enums-p ty.types)
        :t_array (ty-no-empty-enums-p ty.type)
        :t_record (typed_identifierlist-no-empty-enums-p ty.fields)
        :t_exception (typed_identifierlist-no-empty-enums-p ty.fields)
        :t_collection (typed_identifierlist-no-empty-enums-p ty.fields)
        :otherwise t)))
  (define tylist-no-empty-enums-p ((x tylist-p))
    :measure (tylist-count x)
    (if (atom x)
        t
      (and (ty-no-empty-enums-p (car x))
           (tylist-no-empty-enums-p (cdr x)))))
  (define typed_identifierlist-no-empty-enums-p ((x typed_identifierlist-p))
    :measure (typed_identifierlist-count x)
    (if (atom x)
        t
      (and (ty-no-empty-enums-p (typed_identifier->type (car x)))
           (typed_identifierlist-no-empty-enums-p (cdr x))))))


;; Checks that names are resolved and there are no empty enums -- therefore,
;; there exist values that weakly satisfy the type.
(defines ty-weakly-satisfiable-p
  (define ty-weakly-satisfiable-p ((x ty-p))
    :measure (ty-count x)
    (b* ((ty (ty->val x)))
      (type_desc-case ty
        :t_named nil
        :t_enum (consp ty.elts)
        :t_tuple (tylist-weakly-satisfiable-p ty.types)
        :t_array (ty-weakly-satisfiable-p ty.type)
        :t_record (typed_identifierlist-weakly-satisfiable-p ty.fields)
        :t_exception (typed_identifierlist-weakly-satisfiable-p ty.fields)
        :t_collection (typed_identifierlist-weakly-satisfiable-p ty.fields)
        :otherwise t)))
  (define tylist-weakly-satisfiable-p ((x tylist-p))
    :measure (tylist-count x)
    (if (atom x)
        t
      (and (ty-weakly-satisfiable-p (car x))
           (tylist-weakly-satisfiable-p (cdr x)))))
  (define typed_identifierlist-weakly-satisfiable-p ((x typed_identifierlist-p))
    :measure (typed_identifierlist-count x)
    (if (atom x)
        t
      (and (ty-weakly-satisfiable-p (typed_identifier->type (car x)))
           (typed_identifierlist-weakly-satisfiable-p (cdr x))))))


(defines ty-bitwidths-nonneg-p
  (define ty-bitwidths-nonneg-p ((x ty-p))
    :measure (ty-count x)
    :guard (ty-resolved-p x)
    (b* ((ty (ty->val x)))
      (type_desc-case ty
        :t_bits (<= 0 (int-literal-expr->val ty.expr))
        :t_tuple (tylist-bitwidths-nonneg-p ty.types)
        :t_array (ty-bitwidths-nonneg-p ty.type)
        :t_record (typed_identifierlist-bitwidths-nonneg-p ty.fields)
        :t_exception (typed_identifierlist-bitwidths-nonneg-p ty.fields)
        :t_collection (typed_identifierlist-bitwidths-nonneg-p ty.fields)
        :otherwise t)))
  (define tylist-bitwidths-nonneg-p ((x tylist-p))
    :measure (tylist-count x)
    :guard (tylist-resolved-p x)
    (if (atom x)
        t
      (and (ty-bitwidths-nonneg-p (car x))
           (tylist-bitwidths-nonneg-p (cdr x)))))
  (define typed_identifierlist-bitwidths-nonneg-p ((x typed_identifierlist-p))
    :measure (typed_identifierlist-count x)
    :guard (typed_identifierlist-resolved-p x)
    (if (atom x)
        t
      (and (ty-bitwidths-nonneg-p (typed_identifier->type (car x)))
           (typed_identifierlist-bitwidths-nonneg-p (cdr x))))))
  

(defines weak-ty-fix-native
  :flag-local nil
  (define weak-ty-fix-native (x (ty ty-p))
    :verify-guards nil
    :measure (acl2::two-nats-measure (ty-count ty) 0)
    :returns (new-x (implies (ty-weakly-satisfiable-p ty)
                             (weak-ty-satisfied-native new-x ty))
                    :hints ('(:expand ((ty-weakly-satisfiable-p ty)
                                       (:free (x) (weak-ty-satisfied-native x ty))
                                       (:free (ty) (weak-array-type-satisfied-native nil ty))))))
    (b* ((ty (ty->val ty)))
      (type_desc-case ty
        (:t_int (ifix x))
        (:t_bits (ifix x))
        (:t_real (rfix x))
        (:t_string (acl2::str-fix x))
        (:t_bool (acl2::bool-fix x))
        (:t_enum (if (and (keywordp x)
                          (member-equal (symbol-name x) ty.elts))
                     x
                   (if (consp ty.elts)
                       (id-to-native (car ty.elts))
                     (id-to-native "EMPTY_ENUM"))))
        (:t_tuple (weak-tuple-type-fix-native x ty.types))
        (:t_array (array_index-case ty.index
                    :arraylength_expr
                    (weak-array-type-fix-native (len x) x ty.type)
                    :arraylength_enum
                    (pairlis$ (idlist-to-native ty.index.elts)
                              (weak-array-type-fix-native
                               (len ty.index.elts)
                               (acl2::alist-vals x)
                               ty.type))))
        (:t_record (weak-record-type-fix-native x ty.fields))
        (:t_exception (weak-record-type-fix-native x ty.fields))
        (:t_collection (weak-record-type-fix-native x ty.fields))
        (otherwise nil))))

  (define weak-tuple-type-fix-native (x (types tylist-p))
    :measure (acl2::two-nats-measure (tylist-count types) 0)
    :returns (new-x (implies (tylist-weakly-satisfiable-p types)
                             (weak-tuple-type-satisfied-native new-x types))
                    :hints ('(:expand ((:free (x) (weak-tuple-type-satisfied-native x types))
                                       (tylist-weakly-satisfiable-p types)))))
    (if (atom types)
        nil
      (cons (weak-ty-fix-native (and (consp x) (car x)) (car types))
            (weak-tuple-type-fix-native (and (consp x) (cdr x)) (Cdr types)))))

  (define weak-array-type-fix-native ((len natp) x (ty ty-p))
    :measure (acl2::two-nats-measure (ty-count ty) len)
    :returns (new-x (implies (ty-weakly-satisfiable-p ty)
                             (and (true-listp new-x)
                                  (equal (len new-x) (nfix len))
                                  (weak-array-type-satisfied-native new-x ty)))
                    :hints ((and stable-under-simplificationp
                                 '(:expand ((weak-array-type-satisfied-native nil ty)
                                            (:free (a b) (weak-array-type-satisfied-native (cons a b) ty)))))))
    (if (zp len)
        nil
      (cons (weak-ty-fix-native (and (consp x) (car x)) ty)
            (weak-array-type-fix-native (1- len) (and (consp x) (cdr x)) ty))))

  (define weak-record-type-fix-native (x (fields typed_identifierlist-p))
    :measure (acl2::two-nats-measure (typed_identifierlist-count fields) 0)
    :returns (new-x (implies (typed_identifierlist-weakly-satisfiable-p fields)
                             (and (keyword-alist-p new-x)
                                  (weak-record-type-satisfied-native new-x fields)))
                    :hints ('(:expand ((:free (x) (weak-record-type-satisfied-native x fields))
                                       (typed_identifierlist-weakly-satisfiable-p fields)))))
    (b* (((when (atom fields)) nil)
         ((typed_identifier f1) (car fields))
         (val (weak-ty-fix-native (and (consp x) (consp (car x)) (cdar x)) f1.type)))
      (cons (cons (id-to-native f1.name) val)
            (weak-record-type-fix-native (and (consp x) (cdr x)) (cdr fields)))))
  ///
  (fty::deffixequiv-mutual weak-ty-fix-native)

  (verify-guards weak-ty-fix-native)

  (std::defret-mutual weak-ty-fix-native-when-satisfied
    (defret <fn>-when-satisfied
      (implies (weak-ty-satisfied-native x ty)
               (equal new-x x))
      :hints ('(:expand ((weak-ty-satisfied-native x ty)
                         <call>)))
      :fn weak-ty-fix-native)
    (defret <fn>-when-satisfied
      (implies (weak-tuple-type-satisfied-native x types)
               (equal new-x x))
      :hints ('(:expand ((weak-tuple-type-satisfied-native x types)
                         <call>)))
      :fn weak-tuple-type-fix-native)
    (defret <fn>-when-satisfied
      (implies (and (weak-array-type-satisfied-native x ty)
                    (equal (len x) (nfix len))
                    (true-listp x))
               (equal new-x x))
      :hints ('(:expand ((weak-array-type-satisfied-native x ty)
                         <call>
                         (:free (x ty) (weak-array-type-fix-native 0 x ty)))))
      :fn weak-array-type-fix-native)
    (defret <fn>-when-satisfied
      (implies (and (weak-record-type-satisfied-native x fields)
                    (keyword-alist-p x))
               (equal new-x x))
      :hints ('(:expand ((weak-record-type-satisfied-native x fields)
                         <call>)))
      :fn weak-record-type-fix-native))

  (defopener open-weak-ty-fix-native weak-ty-fix-native
    :hyp (syntaxp (or (quotep ty)
                      (case-match ty
                        (('ty (ctor . &))
                         (member-eq ctor
                                    '(t_int t_bits t_real t_string t_bool t_enum
                                            t_tuple t_array t_record t_exception
                                            t_collection t_named)))
                        (& nil))))))


(define symbol-name-list ((x symbol-listp))
  :returns (new-x string-listp)
  (if (atom x)
      nil
    (cons (symbol-name (car x))
          (symbol-name-list (cdr x)))))

(local (defthm symbol-listp-when-keyword-list-p
         (implies (keywordlist-p x)
                  (symbol-listp x))))


(local (defthm unsigned-byte-p-implies-natp-width
         (implies (unsigned-byte-p n x)
                  (natp n))
         :hints(("Goal" :in-theory (enable unsigned-byte-p)))
         :rule-classes :forward-chaining))


(defines native-to-typed-val
  (define native-to-typed-val (x (ty ty-p))
    :guard (and (ty-resolved-p ty)
                (ty-satisfied-native x ty))
    :verify-guards nil
    :returns (val val-p)
    :measure (acl2::two-nats-measure (ty-count ty) 0)
    (b* ((ty (ty->val ty)))
      (type_desc-case ty
        (:t_int (v_int x))
        (:t_bits (v_bitvector (int-literal-expr->val ty.expr) x))
        (:t_real (v_real x))
        (:t_string (v_string x))
        (:t_bool (v_bool x))
        (:t_enum (v_label (identifier (symbol-name x))))
        (:t_tuple (v_array (native-tuple-to-typed-val x ty.types)))
        (:t_array (array_index-case ty.index
                    :arraylength_expr
                    (v_array (native-array-to-typed-val x ty.type))
                    :arraylength_enum
                    (v_record (pairlis$ (arraylength_enum->elts ty.index)
                                        (native-array-to-typed-val (acl2::alist-vals x) ty.type)))))
        (:t_record     (v_record (native-record-to-typed-val x ty.fields)))
        (:t_exception  (v_record (native-record-to-typed-val x ty.fields)))
        (:t_collection (v_record (native-record-to-typed-val x ty.fields)))
        (otherwise (v_int 0)))))

  (define native-tuple-to-typed-val ((x true-listp)
                                     (types tylist-p))
    :guard (and (tylist-resolved-p types)
                (tuple-type-satisfied-native x types))
    :measure (acl2::two-nats-measure (tylist-count types) 0)
    :returns (vals vallist-p)
    (if (atom types)
        nil
      (cons (native-to-typed-val (car x) (car types))
            (native-tuple-to-typed-val (cdr x) (cdr types)))))

  (define native-array-to-typed-val ((x true-listp)
                                     (ty ty-p))
    :guard (and (ty-resolved-p ty)
                (array-type-satisfied-native x ty))
    :measure (acl2::two-nats-measure (ty-count ty) (len x))
    :returns (vals vallist-p)
    (if (atom x)
        nil
      (cons (native-to-typed-val (car x) ty)
            (native-array-to-typed-val (cdr x) ty))))

  (define native-record-to-typed-val ((x keyword-alist-p)
                                      (fields typed_identifierlist-p))
    :guard (and (typed_identifierlist-resolved-p fields)
                (record-type-satisfied-native x fields))
    :measure (acl2::two-nats-measure (typed_identifierlist-count fields) 0)
    :returns (vals val-imap-p)
    (b* ((x (keyword-alist-fix x))
         ((when (atom fields)) nil)
         ((cons & val) (car x))
         ((typed_identifier f1) (car fields)))
      (cons (cons f1.name
                  (native-to-typed-val val f1.type))
            (native-record-to-typed-val (cdr x) (cdr fields)))))
  ///

  (std::defret-mutual len-of-native-array-to-typed-val
    (defret len-of-native-array-to-typed-val
      (equal (len vals) (len x))
      :hints ('(:expand (<call>)))
      :fn native-array-to-typed-val)
    :skip-others t)
  
  (local (defthm equal-lens-when-when-equal-idlist-to-native
           (implies (equal x (idlist-to-native y))
                    (equal (equal (len y) (len x)) t))))

  
  
  (verify-guards native-to-typed-val
    :hints (("goal" :expand ((ty-satisfied-native x ty)
                             (tuple-type-satisfied-native x types)
                             (array-type-satisfied-native x ty)
                             (record-type-satisfied-native x fields)))))

  (std::defret-mutual ty-satisfied-of-native-to-typed-val
    (defret ty-satisfied-of-native-to-typed-val
      (implies (and (ty-satisfied-native x ty)
                    (ty-resolved-p ty))
               (ty-satisfied val ty))
      :hints ('(:expand (<call>
                         (:free (x) (ty-satisfied x ty))
                         (:free (x) (ty-satisfied-native x ty)))
                :in-theory (enable identifier identifier-fix)))
      :fn native-to-typed-val)
    (defret tuple-type-satisfied-of-<fn>
      (implies (and (tuple-type-satisfied-native x types)
                    (tylist-resolved-p types))
               (tuple-type-satisfied vals types))
      :hints ('(:expand (<call>
                         (:free (x) (tuple-type-satisfied x types))
                         (:free (x) (tuple-type-satisfied-native x types)))))
      :fn native-tuple-to-typed-val)

    (defret array-type-satisfied-of-<fn>
      (implies (and (array-type-satisfied-native x ty)
                    (ty-resolved-p ty))
               (array-type-satisfied vals ty))
      :hints ('(:expand (<call>
                         (array-type-satisfied-native x ty)
                         (array-type-satisfied nil ty)
                         (:free (a b) (array-type-satisfied (cons a b) ty)))))
      :fn native-array-to-typed-val)

    (defret record-type-satisfied-of-<fn>
      (implies (and (record-type-satisfied-native x fields)
                    (typed_identifierlist-resolved-p fields))
               (record-type-satisfied vals fields))
      :hints ('(:expand (<call>
                         (:free (x) (record-type-satisfied x fields))
                         (:free (x) (record-type-satisfied-native x fields)))))
      :fn native-record-to-typed-val)))

(defopener open-native-to-typed-val native-to-typed-val
  :hyp (syntaxp (or (quotep ty)
                    (case-match ty
                      (('ty (ctor . &))
                       (member-eq ctor
                                  '(t_int t_bits t_real t_string t_bool t_enum
                                          t_tuple t_array t_record t_exception
                                          t_collection t_named)))
                      (& nil)))))

(defopener open-ty-satisfied-native ty-satisfied-native
  :hyp (syntaxp (or (quotep ty)
                    (case-match ty
                      (('ty (ctor . &))
                       (member-eq ctor
                                  '(t_int t_bits t_real t_string t_bool t_enum
                                          t_tuple t_array t_record t_exception
                                          t_collection t_named)))
                      (& nil)))))

(defopener open-weak-ty-satisfied-native weak-ty-satisfied-native
  :hyp (syntaxp (or (quotep ty)
                    (case-match ty
                      (('ty (ctor . &))
                       (member-eq ctor
                                  '(t_int t_bits t_real t_string t_bool t_enum
                                          t_tuple t_array t_record t_exception
                                          t_collection t_named)))
                      (& nil)))))

(defopener open-ty-satisfying-val ty-satisfying-val
  :hyp (syntaxp (or (quotep x)
                    (case-match x
                      (('ty (ctor . &))
                       (member-eq ctor
                                  '(t_int t_bits t_real t_string t_bool t_enum
                                          t_tuple t_array t_record t_exception
                                          t_collection t_named)))
                      (& nil)))))

(defopener open-ty-bitwidths-nonneg-p ty-bitwidths-nonneg-p
  :hyp (syntaxp (or (quotep x)
                    (case-match x
                      (('ty (ctor . &))
                       (member-eq ctor
                                  '(t_int t_bits t_real t_string t_bool t_enum
                                          t_tuple t_array t_record t_exception
                                          t_collection t_named)))
                      (& nil)))))

(defopener open-constraint_kind-satisfying-val constraint_kind-satisfying-val
  :hyp (syntaxp (or (quotep x)
                    (and (consp x)
                         (member-eq (car x)
                                    '(unconstrained wellconstrained pendingconstrained parametrized))))))
(defopener open-int_constraintlist-satisfying-val int_constraintlist-satisfying-val
  :hyp (syntaxp (or (quotep x)
                    (case-match x
                      (('cons . &) t) (& nil)))))
(defopener open-int_constraint-satisfying-val int_constraint-satisfying-val
  :hyp (syntaxp (or (quotep x)
                    (and (consp x)
                         (member-eq (car x) '(constraint_exact constraint_range))))))



(deftagsum shallow_result
  (:sh_normal (res))
  (:sh_throwing ((throwdata maybe-throwdata)))
  (:sh_error ((desc stringp)
              (data)))
  :short-names t)

(program)

(define simplify-for-shallow (term hyp equiv state &key expand)
  ;; NOTE: always simplifies under EQUAL
  (acl2::easy-simplify-term-fn term hyp
                               `(:expand (:lambdas . ,expand)
                                 :in-theory (acl2::e/d* (asl-code-proof-enables)
                                                        (asl-code-proof-disables))) ;; hints
                               equiv   ;; equiv
                               t      ;; normalize
                               t      ;; rewrite
                               1000   ;; repeat
                               1000   ;; backchain-limit
                               t      ;; untrans-result
                               state))

;; =================================================================================
;; RESOLVING PARAMETER/ARGUMENT/RETURN TYPES.
;; =================================================================================
;; A function signature can have argument and return types that depend on
;; integer parameter values.
;; We need to derive various bits of info about these types:
;;  - guards for the shallow function
;;  - return type of the shallow function
;;  - input fixers for the shallow function
;;  - how to form interpreter parameters/args from the shallow function's parameters/args
;;    (we rewrite this form in order to produce the shallow function's body)
;;  - how to extract the shallow function's return value from the interpreter's return values
;;    (same reason)
;;  - how to extract the shallow function's parameters/args from the interpreter parameters/args
;;    (we want to prove a rule that expresses an interpreter call of the function in terms of the
;;    shallow function)
;;  - how to express the interpreter's return value in terms of the shallow function's return value
;;    (same reason)
;; An example:
;;   func foo {M, N} (x : bits(N)) => (bits(M-1), integer{M..2^N-1})

;; A subtlety of ASL typechecking is that the typechecker ensures (I think)
;; that if the function is called, it is called with correctly typed inputs,
;; but it does not ensure that the output types are necessarily well formed.
;; In fact, if we've defined a function with the signature above then we can
;; call it with foo{0,2}(Zeros{2}) or foo{5,2}(Zeros{2}) and this will
;; typecheck -- but since the return types are empty, somewhere along the way
;; we'll get an error.

;; I think we don't want to assume that such an error can't happen -- e.g., we
;; don't want to require guards m >= 1 or m <= 2^n-1 on the shallowing of the
;; above function. But we do need the guard n >= 0 because otherwise the input
;; can't exist.  We also do need those assumptions on the return types -- we
;; can't say our first return value is an unsigned-byte M-1 if M-1 is less than
;; 0.
;; =================================================================================


;; ---------------------------------------------------------------------------------
;; Resolving types:
;; A type may have variables in it, e.g., bits(N), represented as (t_bits (e_var "N"))
;; We want to resolve this to an ACL2 term representing an ASL type in which
;; all expressions are integer literals, such as (t_bits (e_literal (l_int n))),
;; where n is the variable that we're going to use for the parameter n.
;; Resolve-ty should have sufficient openers installed & enabled; the type here is a quoted value,
;; so only quotep openers are needd.

;; The local-storage argument should be a term nesting PUT-ASSOC-EQUAL
;; applications binding all of the ASL variables (such as "N" above) appearing
;; in the types. For the types inside a function signature that has passed
;; typechecking, bindings for the function's parameters should be sufficient,
;; and it should be possible to add these in order as the parameter types are
;; resolved.

;; (Obsolete:)
;; We compute two terms: the conditions under which the type successfully resolves (ty-resolves-term),
;; and the resolved term when we assume that it did resolve (resolved-ty-term).
;; The former should generally be true unless some variable referenced in the
;; type is missing from local-storage or is not known to be a v_int.
(define shallow-resolve-type (partialp
                              (type ty-p)
                              hyp
                              env-term
                              state)
  (b* (((er ty-resolves-term)
        (simplify-for-shallow `(mv-let (res orac) (,(if partialp 'partial-resolve-ty 'resolve-ty)
                                                   ,env-term ',type :clk 1000)
                                 (declare (ignore orac))
                                 (eval_result-case res :ev_normal))
                              hyp 'iff state))
       (hyp (list 'and ty-resolves-term hyp))
       ((er resolved-ty-term)
        (simplify-for-shallow `(mv-let (res orac) (,(if partialp 'partial-resolve-ty 'resolve-ty)
                                                   ,env-term ',type :clk 1000)
                                 (declare (ignore orac))
                                 (ev_normal->res res))
                              hyp 'equal state))
       ((when (or (equal resolved-ty-term ''nil)
                  (and (consp resolved-ty-term)
                       (eq (car resolved-ty-term) 'EV_NORMAL->RES$INLINE))
                  (not (or (quotep resolved-ty-term)
                           (case-match resolved-ty-term
                             (('ty (ctor . &))
                              (member-eq ctor
                                         '(t_int t_bits t_real t_string t_bool t_enum
                                                 t_tuple t_array t_record t_exception
                                                 t_collection t_named))))))))
        (er soft 'shallow-arg-req "Seems like type didn't resolve: ~x0 -- result ~x1" type resolved-ty-term)))
    (value resolved-ty-term)))

(define shallow-resolve-type-list (partialp (types tylist-p)
                                   hyp
                                   env-term
                                   state)
  (b* (((when (atom types)) (value nil))
       ((er type1) (shallow-resolve-type partialp (car types) hyp env-term state))
       ((er rest) (shallow-resolve-type-list partialp (cdr types) hyp env-term state)))
    (value (cons type1 rest))))


(define shallow-type->native-satisfies-expr (weakp
                                             type-expr ;; expr for resolved type
                                             x-expr    ;; value expression
                                             hyp state)
  ;; E.g., if type-expr is (t_bits n) and x-expr is v, result will be (unsigned-byte-p n v).
  (simplify-for-shallow
   (if weakp
       `(weak-ty-satisfied-native ,x-expr ,type-expr)
     `(ty-satisfied-native ,x-expr ,type-expr))
   hyp 'iff state))

(defines shallow-returntype-rewrites
  ;; returns list of expressions, implicitly conjoined
  (define shallow-returntype-rewrites (expr)
    (case-match expr
      (('and . exprs) (shallow-returntype-rewrites-lst exprs))
      (('unsigned-byte-p idx val)
       `((integerp ,val)
         ,(if (symbolp idx)
              `(unsigned-byte-p ,idx ,val)
            `(implies (equal __width ,idx)
                      (unsigned-byte-p __width ,val)))))
      (& (list expr))))
  (define shallow-returntype-rewrites-lst (exprs)
    (if (atom exprs)
        nil
      (append (shallow-returntype-rewrites (car exprs))
              (shallow-returntype-rewrites-lst (cdr exprs))))))

(defines shallow-returntype-forwards
  ;; returns list of expressions, implicitly conjoined
  ;; NOTE: Below, where this is used, we assume that the return value isn't mentioned, only the formals --
  ;; so we don't bind returnval to the result.
  (define shallow-returntype-forwards (expr)
    (case-match expr
      (('and . exprs) (shallow-returntype-forwards-lst exprs))
      (('unsigned-byte-p ('ifix idx) &)
       `((not (negp ,idx))))
      (& nil)))
  (define shallow-returntype-forwards-lst (exprs)
    (if (atom exprs)
        nil
      (append (shallow-returntype-forwards (car exprs))
              (shallow-returntype-forwards-lst (cdr exprs))))))

(defines shallow-returntype-type-prescriptions
  ;; returns list of expressions, implicitly conjoined
  (define shallow-returntype-type-prescriptions (expr)
    (case-match expr
      (('and . exprs) (shallow-returntype-type-prescriptions-lst exprs))
      (('unsigned-byte-p & val)
       `((integerp ,val)))
      (('rationalp &) (list expr))
      (('integerp &) (list expr))
      (('stringp &) (list expr))
      (('keywordp val) `((symbolp ,val)))
      (('consp &) (list expr))
      (& nil)))
  (define shallow-returntype-type-prescriptions-lst (exprs)
    (if (atom exprs)
        nil
      (append (shallow-returntype-type-prescriptions (car exprs))
              (shallow-returntype-type-prescriptions-lst (cdr exprs))))))

            
  

(define shallow-type->asl-satisfies-expr (weakp
                                          type-expr ;; expr for resolved type
                                          x-expr    ;; value expression
                                          hyp state)
  ;; E.g., if type-expr is (t_bits n) and x-expr is v, result will be (unsigned-byte-p n v).
  (simplify-for-shallow
   (if weakp
       `(weak-ty-satisfied ,x-expr ,type-expr)
     `(ty-satisfied ,x-expr ,type-expr))
   hyp 'iff state))

(define shallow-type->native-fix-expr (weakp
                                       type-expr ;; expr for resolved type
                                       x-expr    ;; value expression
                                       hyp state)
  ;; E.g., if type-expr is (t_bits n) and x-expr is v, result will be (loghead n v).
  (simplify-for-shallow
   (if weakp
       `(weak-ty-fix-native ,x-expr ,type-expr)
     `(ty-fix-native ,x-expr ,type-expr))
   hyp 'equal state))

(define shallow-type->native-wrap-expr (type-expr ;; expr for resolved type
                                       x-expr    ;; value expression
                                       hyp state)
  ;; E.g., if type-expr is (t_bits n) and x-expr is v, result will be (v_bitvector n v).
  (simplify-for-shallow `(native-to-typed-val ,x-expr ,type-expr) hyp 'equal state))

(define shallow-type->native-wrap-exprlist (type-exprs ;; expr for resolved types
                                            x-exprs    ;; value expressions
                                            hyp state)
  (b* (((when (atom type-exprs)) (value nil))
       ((er first) (shallow-type->native-wrap-expr (car type-exprs) (car x-exprs) hyp state))
       ((er rest) (shallow-type->native-wrap-exprlist (cdr type-exprs) (cdr x-exprs) hyp state)))
    (Value (cons first rest))))

(define shallow-type->native-extract-expr (type-expr ;; expr for resolved type
                                           x-expr    ;; value expression
                                           hyp state)
  ;; E.g., if type-expr is (t_bits n) and x-expr is v, result will be (v_bitvector->val v).
  (simplify-for-shallow `(typed-val-to-native ,x-expr ,type-expr)
                        hyp 'equal state))



(define params-to-typed_identifierlist ((x maybe-typed_identifierlist-p))
  :returns (new-x typed_identifierlist-p)
  (if (atom x)
      nil
    (b* (((maybe-typed_identifier x1) (car x)))
      (cons (typed_identifier x1.name (or x1.type (ty (t_int (unconstrained)))))
            (params-to-typed_identifierlist (cdr x))))))

;; ---------------------------------------------------------------------------------
;; Fills in a local-storage-term with parameter bindings, for use with shallow-resolve-type.
(define shallow-local-storage-param-bindings ((params symbol-listp)
                                              (fn-params typed_identifierlist-p)
                                              local-storage-term)
  ;; Forms a term representing a val-imap in which each ASL parameter name is bound to (v_int v)
  ;; where v is the corresponding ACL2 variable.
  (b* (((when (atom params)) local-storage-term)
       ((typed_identifier p1) (car fn-params))
       (new-term `(put-assoc-equal ,p1.name (v_int ,(car params)) ,local-storage-term)))
    (shallow-local-storage-param-bindings (cdr params) (cdr fn-params) new-term)))


(define shallow-param-env ((params symbol-listp) (fn-params typed_identifierlist-p))
  (let ((storage-term (shallow-local-storage-param-bindings params fn-params 'env.local.storage)))
    `(change-env env :local (change-local-env (empty-local-env) :storage ,storage-term))))

(define shallow-param-integerp-hyps ((params symbol-listp))
  ;; Assumes each parameter variable to be integerp.
  (if (atom params)
      nil
    (cons `(integerp ,(car params))
          (shallow-param-integerp-hyps (cdr params)))))


;; ---------------------------------------------------------------------------------
;; Fills in a local-storage-term with parameter bindings, but leaves out the v_int constructors --
;; i.e. we're assuming the param variables are all bound to ASL values (v_int-p).
(define shallow-local-storage-asl-param-bindings ((params symbol-listp)
                                                  (fn-params typed_identifierlist-p)
                                                  local-storage-term)
  ;; Forms a term representing a val-imap in which each ASL parameter name is bound to (v_int v)
  ;; where v is the corresponding ACL2 variable.
  (b* (((when (atom params)) local-storage-term)
       ((typed_identifier p1) (car fn-params))
       (new-term `(put-assoc-equal ,p1.name ,(car params) ,local-storage-term)))
    (shallow-local-storage-asl-param-bindings (cdr params) (cdr fn-params) new-term)))


(define shallow-asl-param-env ((params symbol-listp) (fn-params typed_identifierlist-p))
  (let ((storage-term (shallow-local-storage-asl-param-bindings params fn-params 'env.local.storage)))
    `(change-env env :local (change-local-env (empty-local-env) :storage ,storage-term))))

(define shallow-asl-param-v_int-hyps ((params symbol-listp))
  ;; Assumes each parameter variable to be integerp.
  (if (atom params)
      nil
    (cons `(val-p ,(car params))
          (cons `(val-case ,(car params) :v_int)
                (shallow-asl-param-v_int-hyps (cdr params))))))
       



;; ---------------------------------------------------------------------------------
;; Computes the resolved types of the function's arguments.
;; Hyp here just needs to assume all parameters are integerp.
;; Just returns the ordered list of resolved types.
(define shallow-resolve-arg-types (partialp
                                   (fn-args typed_identifierlist-p)
                                   param-env-term
                                   hyp
                                   state)
  (b* (((when (atom fn-args)) (value nil))
       ((typed_identifier p1) (car fn-args))
       ((er type-term) (shallow-resolve-type partialp p1.type hyp param-env-term state))
       ((er rest) (shallow-resolve-arg-types partialp (cdr fn-args) param-env-term hyp state)))
    (value (cons type-term rest))))


(define shallow-resolve-weak-arg-types ((fn-args typed_identifierlist-p)
                                        (static-env static_env_global-p))
  (b* (((when (atom fn-args)) nil)
       ((typed_identifier p1) (car fn-args))
       (type-res (name-resolve-ty static-env p1.type :clk 1000))
       ((unless (eval_result-case type-res :ev_normal))
        (er hard? 'shallow-resolve-weak-arg-types
            "Couldn't resolve type for ~x0: type ~x1, error ~x2"
            p1.name p1.type type-res))
       (type-term (kwote (ev_normal->res type-res))))
    (cons type-term
          (shallow-resolve-weak-arg-types (cdr fn-args) static-env))))
   
    
;; ---------------------------------------------------------------------------------
;; Shallow function's DEFINE formals:
;; Given resolved types for the parameters and args, pair (varname type-satisfied-term).
;; The full set of guard assumptions can be derived from this using strip-cadrs.
(define shallow-define-formals (weakp
                                (args symbol-listp)
                                resolved-types ;; term list, one per arg
                                hyp ;; params integer-listp
                                state)
  (b* (((when (atom args)) (value nil))
       ((er satisfies-term)
        (shallow-type->native-satisfies-expr weakp (car resolved-types) (car args)
                                             hyp state))
       ((er rest) (shallow-define-formals weakp (cdr args) (cdr resolved-types) hyp state)))
    (value (cons `(,(car args) ,satisfies-term) rest))))

;; ---------------------------------------------------------------------------------
;; Additional guard for weak-types method to ensure bitvector width expressions are nonnegative.
;; When we're not using weak-types, we have an unsigned-byte-p guard that implies this.
;; Given resolved types for the parameters and args, pair (varname type-satisfied-term).
;; The full set of guard assumptions can be derived from this using strip-cadrs.
(define shallow-bitwidth-nonneg-exprs (resolved-types ;; term list, one per arg
                                       hyp ;; params integer-listp
                                       state)
  (b* (((when (atom resolved-types)) (value nil))
       ((er first)
        (simplify-for-shallow `(ty-bitwidths-nonneg-p ,(car resolved-types))
                              hyp 'iff state))
       ((er rest) (shallow-bitwidth-nonneg-exprs (cdr resolved-types) hyp state)))
    (value (if (eq first t)
               rest
             (cons first rest)))))


;; ---------------------------------------------------------------------------------
;; LET* bindings for use at the start of the shallow function to fix the
;; arguments to the required types.
(define shallow-formal-fixer-alist (weakp
                                    (args symbol-listp)
                                    resolved-types ;; term list, one per arg
                                    hyp            ;; params integer-listp
                                    state)
  (b* (((when (atom args)) (value nil))
       ((er fix-expr)
        (shallow-type->native-fix-expr weakp (car resolved-types) (car args) hyp state))
       ((er rest) (shallow-formal-fixer-alist weakp (cdr args) (cdr resolved-types) hyp state)))
    (value (cons (cons (car args) fix-expr) rest))))

(define shallow-formal-fixer-bindings (alist)
  (if (atom alist)
      nil
    (b* (((cons var fix-expr) (car alist)))
      (cons `(,var (mbe :logic ,fix-expr :exec ,var))
            (shallow-formal-fixer-bindings (cdr alist))))))

;; ---------------------------------------------------------------------------------
;; For determining the function body, we rewrite a call of eval_subprogram
;; where we've constructed the arg and parameter lists from the shallow function's formals.
;; This constructs either the parameters or the args.
(define shallow-native-to-asl-args ((args symbol-listp)
                                    resolved-types ;; term list, one per arg
                                    hyp ;; params integer-listp ?
                                    state)
  (b* (((when (atom args)) (value nil))
       ((er arg-term) (shallow-type->native-wrap-expr (car resolved-types) (car args) hyp state))
       ((er rest) (shallow-native-to-asl-args (cdr args) (cdr resolved-types) hyp state)))
    (value (cons arg-term rest))))


;; ---------------------------------------------------------------------------------
;; The correctness proof of the shallow embedding shows that an arbitrary call
;; of eval_subprogram of the function with correct arity can be expressed in terms of the shallow function.
;; The shallow function needs to be called with args derived from the ASL value args given to eval_subprogram.
;; E.g., for foo {n} (v : bits(N)), we need (v_int->val n) (v_bits->val v).
;; 
(define shallow-asl-to-native-args ((args symbol-listp)
                                    asl-resolved-types
                                    hyp ;; params v_int
                                    state)
  (b* (((when (atom args)) (value nil))
       ((er first) (shallow-type->native-extract-expr (car asl-resolved-types) (car args) hyp state))
       ((er rest) (shallow-asl-to-native-args (cdr args) (cdr asl-resolved-types) hyp state)))
    (value (cons first rest))))

;; ---------------------------------------------------------------------------------
;; We install fixers at the beginning of the body so that the hypotheses for the return type theorems will be minimal.
;; But these fixers can't help when the input type isn't satisfiable, i.e., (t_bits n) when n < 0.
;; So we need to more broadly assume that the input types are all satisfiable even when we don't want to assume that the
;; inputs satisfy their types. Returns a list of assumptions.
(define shallow-types-satisfiable (weakp
                                   resolved-types
                                   hyp ;; params integer-listp
                                   state)
  (b* (((when (atom resolved-types)) (value nil))
       ((er first) (simplify-for-shallow
                    (if weakp
                        `(ty-weakly-satisfiable-p ,(car resolved-types))
                      `(ty-satisfying-val ,(car resolved-types))) hyp 'iff state))
       ((when (eq first nil))
        (er soft 'shallow-types-satisfiable "Type cannot be satisfied: ~x0" (car resolved-types)))
       ((er rest) (shallow-types-satisfiable weakp (cdr resolved-types) hyp state)))
    (value (if (eq first t)
               rest
             (cons first rest)))))
      


       

;; (define shallow-return-exprs-aux (returnvars returntypes hyps state)
;;   (b* (((When (atom returnvars)) (value nil))
;;        ((er (cons type type-resolves)) (shallow-resolve-type (car returntypes) hyps local-storage state))
;;        (term `(native-to-typed-val ,(car returnvars) ,type))
;;        (hyps (cons type-resolves hyps))
;;        ((er rw-term) (simplify-for-shallow term `(and . ,hyps) 'equal state))
;;        ((er rest) (shallow-return-exprs-aux (cdr returnvars) (cdr returntypes) hyps local-storage state)))
;;     (value (cons rw-term rest))))
    

;; (define shallow-return-exprs (returnvars returntype hyps local-storage state)
;;   (b* (((unless returntype)
;;         (value nil))
;;        (ty (ty->val returntype)))
;;     (type_desc-case ty
;;       :t_tuple (b* (((unless (eql (len returnvars) (len ty.types)))
;;                      (er soft 'shallow-return-exprs "Returns length mismatch")))
;;                  (shallow-return-exprs-aux returnvars ty.types hyps local-storage state))
;;       :otherwise (b* (((unless (symbolp returnvars))
;;                        (er soft 'shallow-return-exprs "Returns length mismatch")))
;;                    (shallow-return-exprs-aux
;;                     (list returnvars) (list returntype) hyps local-storage state)))))

    



;; (define shallow-return-type (returntype hyps local-storage state)
;;   (b* (((unless returntype)
;;         (value nil))
;;        ((er type) (shallow-resolve-type returntype hyps local-storage state))
;;        (term `(ty-satisfied-native returnval ,type))
;;        ((er rw-term) (simplify-for-shallow term `(and . ,hyps) 'iff state)))
;;     (value rw-term)))





;; (define remove-duplicated-keys-aux (x keys)
;;   (if (atom x)
;;       nil
;;     (if (and (consp (car x))
;;              (not (member-equal (caar x) keys)))
;;         (cons (car x)
;;               (remove-duplicated-keys-aux (cdr x) (cons (caar x) keys)))
;;       (remove-duplicated-keys-aux (cdr x) keys))))

;; (define remove-duplicated-keys (x)
;;   (remove-duplicated-keys-aux x nil))
                                     
             
(define remove-corresponding-elements (keys vals rem)
  (if (atom keys)
      nil
    (if (member-equal (car keys) rem)
        (remove-corresponding-elements (cdr keys) (cdr vals) rem)
      (cons (car vals)
            (remove-corresponding-elements (cdr keys) (cdr vals) rem)))))
                    



(define shallow-let-abstract (x state)
  (b* (((er x-trans) (acl2::translate x t nil t 'shallow-let-abstract (w state) state))
       ((mv letabs &) (cmr::let-abstract-term-preserving-ifs x-trans 'tmp- 0)))
    (value (untranslate letabs nil (w state)))))


(define shallow-translate-alist (x state)
  (b* (((when (atom x)) (value nil))
       ((cons var term) (car x))
       ((er trans-term) (acl2::translate term t nil t 'shallow-translate-alist (w state) state))
       ((er rest) (shallow-translate-alist (cdr x) state)))
    (value (cons (cons var trans-term) rest))))

(define shallow-substitute (x al state)
  (b* (((er x-trans) (acl2::translate x t nil t 'shallow-let-abstract (w state) state))
       ((er al-trans) (shallow-translate-alist al state))
       (subst (cmr::term-subst x-trans al-trans)))
    (value (untranslate subst nil (w state)))))

(define shallow-translate-list (x state)
  (b* (((when (atom x)) (value nil))
       ((er first) (acl2::translate (car x) t nil t 'shallow-let-abstract (w state) state))
       ((er rest) (shallow-translate-list (cdr x) state)))
    (value (cons first rest))))


(define shallow-substitute-list (x al state)
  (b* (((er x-trans) (shallow-translate-list x state))
       ((er al-trans) (shallow-translate-alist al state))
       (subst (cmr::termlist-subst x-trans al-trans)))
    (value (acl2::untranslate-lst subst nil (w state)))))


(define def-asl-shallow-fn (name args state)
  (b* (((std::extract-keyword-args
         :other-args bad-args
         :allowed-keys '(:prepwork)
         ;; :kwd-alist kwd-alist
         function
         params
         args
         returns ;; name or list of names, not specs like in def-asl-subprogram
         ;; measure
         safe-clock
         
         ;; enable
         ;; disable
         ;; hints
         ;; prepwork

         (static-env '(stdlib-static-env)))
        args)

       (weak-types t)
       (partialp t)
       
       ((when bad-args)
        (er soft 'def-asl-shallow "Bad arguments: ~x0" bad-args))
       ((unless (stringp function))
        (er soft 'def-asl-shallow "Function should be a string: ~x0" function))
       ((acl2::er (cons & static-env-val))
        (acl2::simple-translate-and-eval static-env nil nil
                                         (msg "static env ~x0" static-env)
                                         'def-asl-shallow (w state) state t))
       ((unless (static_env_global-p static-env-val))
        (er soft 'def-asl-shallow "Bad static env (evaluation of ~x0): doesn't satisfy static_env_global-p" static-env))
       (fn-struct (cdr (hons-assoc-equal function
                                         (static_env_global->subprograms static-env-val))))
       ((unless fn-struct)
        (er soft 'def-asl-shallow "Bad function ~x0: not found in static env" function))
       ((func-ses fn-struct))
       ((func f) fn-struct.fn)

       ((unless (symbol-listp params))
        (er soft 'def-asl-shallow "Params should be a symbol-list"))
       ((unless (eql (len params) (len f.parameters)))
        (er soft 'def-asl-shallow "~x0 params were given but ~s1 has ~x2 parameters" (len params) function (len f.parameters)))
       ((unless (symbol-listp args))
        (er soft 'def-asl-shallow "Args should be a symbol-list"))
       ((unless (eql (len args) (len f.args)))
        (er soft 'def-asl-shallow "~x0 args were given but ~s1 has ~x2 args" (len args) function (len f.args)))

       (returntype-is-tuple
        (type_desc-case (ty->val f.return_type) :t_tuple))

       ;; Returntype can be a tuple, in which case returns is a list of the
       ;; same length.  If not, we accept either a symbol or list containing
       ;; one symbol, but normalize to just a symbol.
       ((unless
            (b* ((returntype (ty->val f.return_type)))
              (type_desc-case returntype
                :t_tuple (and (symbol-listp returns)
                              (equal (len returns) (len returntype.types)))
                :otherwise (or (symbolp returns)
                               (symbol-listp returns)
                               (eql (len returns) 1)))))
        (er soft 'def-asl-shallow "Bad number of return values"))
       (returns (if (and (not returntype-is-tuple)
                         (not (symbolp returns)))
                    (car returns)
                  returns))

       
       (direct-subprograms (collect-direct-subprograms f.body nil))
       (table  (table-alist 'asl-subprogram-table (w state)))
       (subprograms (cons function (collect-transitive-subprograms direct-subprograms table nil)))
       (clk-val
        (or safe-clock
            (+ 1 (maximize-const-clocks direct-subprograms table -1))))


       ;; (orig-args args)

       (param-env-term (shallow-param-env params f.parameters))
       (params-integerp-hyps (shallow-param-integerp-hyps params))
       (params-integerp-hyp `(and . ,params-integerp-hyps))
       (f.parameters (params-to-typed_identifierlist f.parameters))
       ((er param-types)
        (shallow-resolve-arg-types partialp f.parameters param-env-term params-integerp-hyp state))
       (weak-param-types
        (if weak-types
            (shallow-resolve-weak-arg-types f.parameters static-env-val)
          param-types))
       ((er arg-types)
        (shallow-resolve-arg-types partialp f.args param-env-term params-integerp-hyp state))
       (weak-arg-types
        (if weak-types
            (shallow-resolve-weak-arg-types f.args static-env-val)
          arg-types))

       ((er types-sat) (shallow-types-satisfiable weak-types
                                                  (append weak-param-types weak-arg-types)
                                                  params-integerp-hyp state))
       
       (nondup-args (set-difference-eq args params))
       (nondup-arg-types (remove-corresponding-elements args weak-arg-types params))

       
       ((er param-formals)
        (shallow-define-formals weak-types params weak-param-types t state))
       ((er arg-formals)
        (shallow-define-formals weak-types nondup-args nondup-arg-types params-integerp-hyp state))
       (formals (append param-formals arg-formals))
       ((er additional-guards)
        (if weak-types
            (shallow-bitwidth-nonneg-exprs arg-types params-integerp-hyp state)
          (value nil)))
       
       (guard-hyps (append additional-guards (acl2::strip-cadrs formals)))
       
       ((er param-fixer-al)
        (shallow-formal-fixer-alist weak-types params weak-param-types t state))
       ((er arg-fixer-al)
        (shallow-formal-fixer-alist weak-types nondup-args nondup-arg-types params-integerp-hyp state))
       (fixer-al (append param-fixer-al arg-fixer-al))
       (fixers (shallow-formal-fixer-bindings fixer-al))

       ((er asl-params)
        (shallow-native-to-asl-args params param-types params-integerp-hyp state))
       ((er asl-args)
        (shallow-native-to-asl-args args arg-types params-integerp-hyp state))
       
       (match-hyp `(subprograms-match ',subprograms
                                      (global-env->static (env->global env))
                                      ,static-env))
       (measure-reqs (if (eql clk-val 0)
                         t
                       `(<= ,clk-val (ifix clk))))

       
       (body-hyp (list* 'and match-hyp measure-reqs (append params-integerp-hyps guard-hyps)))
       (body-term `(mv-nth 0 (eval_subprogram env ,function (list . ,asl-params) (list . ,asl-args))))
       ((er body-simp) (simplify-for-shallow body-term body-hyp 'equal state
                                             :expand `((:free (params args)
                                                        (eval_subprogram env ,function params args)))))
       (normalp-term `(equal (eval_result-kind ,body-simp) :ev_normal))
       ((er normalp-simp) (simplify-for-shallow normalp-term body-hyp 'iff state))
       (always-normal (eq normalp-simp t))

       ((er return-type)
        (shallow-resolve-type partialp f.return_type t param-env-term state))
       (weak-return-type
        (if weak-types
            (b* ((res (name-resolve-ty static-env-val f.return_type :clk 1000))
                 ((unless (eval_result-case res :ev_normal))
                  (er hard? 'def-asl-shallow "Couldn't resolve return type")))
              (kwote (ev_normal->res res)))
          return-type))
       ((er return-type/s)
        (b* ((returntype (ty->val f.return_type)))
          (type_desc-case returntype
            :t_tuple (shallow-resolve-type-list partialp returntype.types t param-env-term state)
            :otherwise (value return-type))))

       ;; ((er errorp-simp) (if always-normal
       ;;                       (value nil)
       ;;                     (simplify-for-shallow `(equal (eval_result-kind ,body-simp) :ev_error)
       ;;                                           `(and . ,hyps) 'iff state)))
       ;; ((er throwingp-simp) (if always-normal
       ;;                          (value nil)
       ;;                        (simplify-for-shallow `(equal (eval_result-kind ,body-simp) :ev_throwing)
       ;;                                              `(and . ,hyps) 'iff state)))
       
       (return-expr
        (if returntype-is-tuple
            `(tuple-vallist-to-native (func_result->vals res.res) ,return-type/s)
          `(typed-val-to-native (car (func_result->vals res.res)) ,return-type/s)))
       
       ((er body) (simplify-for-shallow
                   (if always-normal
                       `(let ((res.res (ev_normal->res ,body-simp)))
                          ,return-expr)
                     `(let ((res ,body-simp))
                        (eval_result-case res
                          :ev_normal (sh_normal ,return-expr)
                          :ev_error (sh_error res.desc res.data)
                          :ev_throwing (sh_throwing res.throwdata))))
                   body-hyp 'equal state))
       ((er body) (shallow-let-abstract body state))

       ((er return-type-term) (shallow-type->native-satisfies-expr
                               weak-types weak-return-type 'returnval t state))
       ;; (return-concl (if always-normal
       ;;                   return-type-term
       ;;                 (and (shallow_result-p returnval)
       ;;                      (implies (shallow_result-case returnval :sh_normal)
       ;;                               (let ((returnval (sh_normal->res returnval)))
       ;;                                 ,return-type-term)))))
       ((er return-hyp) (shallow-substitute `(and . ,types-sat) fixer-al state))
       (return-sig-thmname (intern-in-package-of-symbol
                            (concatenate 'string (symbol-name name) "-RETURN-SIGNATURE")
                            'asl-pkg))
       (return-rewrites
        (if always-normal
            `(implies ,return-hyp
                      (and . ,(shallow-returntype-rewrites return-type-term)))
          `(and (shallow_result-p returnval)
                (implies (and (shallow_result-case returnval :sh_normal)
                              ,return-hyp)
                         (let ((returnval (sh_normal->res returnval)))
                           (and . ,(shallow-returntype-rewrites return-type-term)))))))
       (return-rewrite-thmname (intern-in-package-of-symbol
                            (concatenate 'string (symbol-name name) "-RETURN-REWRITE")
                            'asl-pkg))
       (return-type-prescrips (shallow-returntype-type-prescriptions return-type-term))
       (return-type-prescrip
        (and return-type-prescrips
             (if always-normal
                 `(and . ,return-type-prescrips)
               `(implies (shallow_result-case returnval :sh_normal)
                         (let ((returnval (sh_normal->res returnval)))
                           (and . ,return-type-prescrips))))))
       (return-type-prescrip-thmname (intern-in-package-of-symbol
                            (concatenate 'string (symbol-name name) "-RETURN-TYPE-PRESCRIP")
                            'asl-pkg))

       (return-forwards (shallow-returntype-forwards return-type-term))
       ;; (return-forward
       ;;  (and return-forwards
       ;;       (not always-normal)
       ;;       `(implies (shallow_result-case returnval :sh_normal)
       ;;                 (and . ,return-forwards))))
       ;; (return-forward-thmname (intern-in-package-of-symbol
       ;;                          (concatenate 'string (symbol-name name) "-RETURN-FORWARD")
       ;;                          'asl-pkg))

       (return-reverse
        (and return-forwards
             (not always-normal)
             `(implies (and ,return-hyp
                            (not (and . ,return-forwards)))
                       (not (equal (shallow_result-kind returnval) :sh_normal)))))
       (return-reverse-thmname (intern-in-package-of-symbol
                                (concatenate 'string (symbol-name name) "-RETURN-REVERSE")
                                'asl-pkg))

       (return-concl `(and ,@(and return-type-prescrips `(,return-type-prescrip))
                           ,@(and return-reverse `(,return-reverse))
                           ,return-rewrites))
                               

       (asl-param-env-term (shallow-asl-param-env params f.parameters))
       (asl-params-v_int-hyps (shallow-asl-param-v_int-hyps params))
       (asl-params-v_int-hyp `(and . ,asl-params-v_int-hyps))
       ((er asl-param-types)
        (shallow-resolve-arg-types partialp f.parameters asl-param-env-term asl-params-v_int-hyp state))
       ((er asl-arg-types)
        (shallow-resolve-arg-types partialp f.args asl-param-env-term asl-params-v_int-hyp state))

       (nondup-asl-arg-types (remove-corresponding-elements args asl-arg-types params))

       ((er asl-to-native-args) (shallow-asl-to-native-args
                                 (append params nondup-args)
                                 (append asl-param-types nondup-asl-arg-types)
                                 asl-params-v_int-hyp state))
       
       (return-binder (if (symbolp returns) returns `(list . ,returns)))

       ((er asl-return-type)
        (shallow-resolve-type partialp f.return_type asl-params-v_int-hyp asl-param-env-term state))
       ((er asl-return-type/s)
        (b* ((returntype (ty->val f.return_type)))
          (type_desc-case returntype
            :t_tuple (shallow-resolve-type-list partialp returntype.types asl-params-v_int-hyp asl-param-env-term state)
            :otherwise (value asl-return-type))))

       ((er asl-return-exprs)
        (if returntype-is-tuple
            (shallow-type->native-wrap-exprlist asl-return-type/s returns asl-params-v_int-hyp state)
          (shallow-type->native-wrap-exprlist (list asl-return-type) (list returns) asl-params-v_int-hyp state))))
       
    (value `(define ,name ,formals
              :returns (returnval ,return-concl
                                  :rule-classes nil :name ,return-sig-thmname)
              :guard (and . ,additional-guards)
              (let* ,fixers
                ,body)
              ///

              (defret ,return-rewrite-thmname
                ,return-rewrites
                :hints (("goal" :use ,return-sig-thmname
                         :in-theory (disable ,name)))
                :rule-classes :rewrite)
              
              ,@(and return-type-prescrip
                     `((defret ,return-type-prescrip-thmname
                         ,return-type-prescrip
                         :hints (("goal" :use ,return-sig-thmname
                                  :in-theory (disable ,name
                                                      ,return-rewrite-thmname)))
                         :rule-classes :type-prescription)))

              ,@(and return-reverse
                     `(;; (defret ,return-forward-thmname
                       ;;   ,return-forward
                       ;;   :hints (("goal" :use ,return-sig-thmname
                       ;;            :in-theory (disable ,name
                       ;;                                ,return-rewrite-thmname)))
                       ;;   :rule-classes ((:forward-chaining :trigger-terms ((equal (shallow_result-kind <call>)
                       ;;                                                            :sh_normal)))))
                       (defret ,return-reverse-thmname
                         ,return-reverse
                         :hints (("goal" :use ,return-sig-thmname
                                  :in-theory (disable ,name
                                                      ,return-rewrite-thmname))))))
              
              (def-asl-subprogram ,(intern-in-package-of-symbol
                                    (concatenate 'string (symbol-name name) "-SHALLOW-CORRECT")
                                    'asl-pkg)
                :function ,function
                :params ,params
                :args ,args
                :safe-clock ,safe-clock
                ;; need list of formals wrapped in val type constructors
                :bindings ((impl (,name  . ,asl-to-native-args))
                           ,@(if always-normal
                                 `((,return-binder impl))
                               `(((sh_normal impl))
                                 (,return-binder impl.res))))
                ,@(and (not always-normal)
                       `(:normal-cond (shallow_result-case impl :sh_normal)
                         :nonnormal-res (shallow_result-case impl
                                          :sh_error (ev_error impl.desc impl.data)
                                          :otherwise (b* (((sh_throwing impl)))
                                                       (ev_throwing impl.throwdata
                                                                    (change-env env :local (empty-local-env)))))))
                :return-values ,asl-return-exprs)))))
            



(defmacro def-asl-shallow (name &rest args)
  (let* ((prepwork (cadr (assoc-keyword :prepwork args))))
    `(defsection ,name
       ,@prepwork
       (make-event (def-asl-shallow-fn ',name ',args state)))))






(logic)

(local (defthm maybe-stmt-count-measure
         (implies x
                  (< (stmt-count x) (maybe-stmt-count x)))
         :hints(("Goal" :expand ((maybe-stmt-count x))
                 :in-theory (enable maybe-stmt-some->val)))))

(define pair-typed-identifiers ((x identifierlist-p)
                                (types tylist-p))
  :guard (equal (len x) (len types))
  :returns (lst typed_identifierlist-p)
  (if (atom x)
      nil
    (cons (typed_identifier (car x) (car types))
          (pair-typed-identifiers (cdr x) (cdr types)))))

(defthm typed_identifierlist-p-of-append
  (implies (and (typed_identifierlist-p x)
                (typed_identifierlist-p y))
           (typed_identifierlist-p (append x y))))

(defines shallow-loop-and-decls
  (define shallow-loop-and-decls ((n natp)
                                  (stmttype symbolp)
                                  (x stmt-p))
    :returns (mv (res maybe-stmt-p)
                 (decls typed_identifierlist-p)
                 (next-n (implies (not res) (natp next-n)) :rule-classes :type-prescription))
    :measure (stmt-count x)
    :verify-guards nil
    (b* ((orig-x x)
         (x (stmt->val x))
         (decr (eq stmttype (stmt_desc-kind x)))
         ((when (and decr (zp n)))
          (mv (stmt-fix orig-x) nil nil))
         (n (if decr (1- n) (lnfix n))))
      (stmt_desc-case x
        :s_seq (b* (((mv res decls1 n) (shallow-loop-and-decls n stmttype x.first))
                    ((when res) (mv res decls1 n))
                    ((mv res decls2 n) 
                     (shallow-loop-and-decls n stmttype x.second)))
                 (mv res (append decls1 decls2) n))
        :s_decl (b* (((unless x.ty) (mv nil nil n))
                     (ty (ty->val x.ty)))
                  (fty::multicase
                    ((local_decl_item-case x.item)
                     (type_desc-case ty))
                    ((:ldi_tuple :t_tuple)
                     :when (eql (len x.item.names) (len ty.types))
                     (mv nil (pair-typed-identifiers x.item.names ty.types) n))
                    ((:ldi_var &) (mv nil (list (typed_identifier x.item.name x.ty)) n))
                    (& (mv nil nil n))))
        :s_cond (b* (((mv res decls n)
                      (shallow-loop-and-decls n stmttype x.then))
                     ((when res)
                      (mv res decls n))
                     ((mv res decls n)
                      (shallow-loop-and-decls n stmttype x.else))
                     ((when res)
                      (mv res decls n)))
                  (mv nil nil n))
        :s_for (b* (((mv res decls n) (shallow-loop-and-decls n stmttype x.body))
                    ((when res)
                     (mv res (cons (typed_identifier x.index_name (t_int (unconstrained))) decls) n)))
                 (mv nil nil n))
        :s_while (shallow-loop-and-decls n stmttype x.body)
        :s_repeat (shallow-loop-and-decls n stmttype x.body)
        :s_try (b* (((mv res decls n) (shallow-loop-and-decls n stmttype x.body))
                    ((when res) (mv res decls n))
                    ((mv res decls n) (shallow-loop-and-decls-catcherlist n stmttype x.catchers))
                    ((when res) (mv res decls n)))
                 (shallow-loop-and-decls-maybe n stmttype x.otherwise))
        :otherwise (mv nil nil n))))

  (define shallow-loop-and-decls-maybe ((n natp)
                                        (stmttype symbolp)
                                        (x maybe-stmt-p))
    :returns (mv (res maybe-stmt-p)
                 (decls typed_identifierlist-p)
                 (next-n (implies (not res) (natp next-n)) :rule-classes :type-prescription))
    :measure (maybe-stmt-count x)
    (if x
        (shallow-loop-and-decls n stmttype x)
      (mv nil nil (lnfix n))))

  (define shallow-loop-and-decls-catcherlist ((n natp)
                                              (stmttype symbolp)
                                              (x catcherlist-p))
    :returns (mv (res maybe-stmt-p)
                 (decls typed_identifierlist-p)
                 (next-n (implies (not res) (natp next-n)) :rule-classes :type-prescription))
    :measure (catcherlist-count x)
    (b* (((when (atom x))
          (mv nil nil (lnfix n)))
         ((mv res decls n)
          (shallow-loop-and-decls-catcher n stmttype (car x)))
         ((when res) (mv res decls n)))
      (shallow-loop-and-decls-catcherlist n stmttype (cdr x))))

  (define shallow-loop-and-decls-catcher ((n natp)
                                          (stmttype symbolp)
                                          (x catcher-p))
    :returns (mv (res maybe-stmt-p)
                 (decls typed_identifierlist-p)
                 (next-n (implies (not res) (natp next-n)) :rule-classes :type-prescription))
    :measure (catcher-count x)
    (b* (((catcher x))
         ((mv res decls n)
          (shallow-loop-and-decls n stmttype x.stmt)))
      (mv res
          (if (and res x.name)
              (cons (typed_identifier x.name x.ty) decls)
            decls)
          n)))
  ///
  (verify-guards shallow-loop-and-decls))


(local
 (progn
   (include-book "std/osets/element-list" :dir :system)
   (deflist identifierlist :elt-type identifier :true-listp t)))

(defthm setp-of-singleton
  (setp (list x))
  :hints(("Goal" :in-theory (enable setp))))

(defines shallow-lexpr-written-vars
  (define shallow-lexpr-written-vars ((x lexpr-p)
                                      (declared-vars identifierlist-p))
    :returns (written-vars (and (set::setp written-vars)
                                (identifierlist-p written-vars)))
    :measure (lexpr-count x)
    :verify-guards nil
    (b* ((x (lexpr->val x)))
      (lexpr_desc-case x
        :le_var (and (member-equal x.name (identifierlist-fix declared-vars))
                     (list x.name))
        :le_slice (shallow-lexpr-written-vars x.base declared-vars)
        :le_setarray (shallow-lexpr-written-vars x.base declared-vars)
        :le_setenumarray (shallow-lexpr-written-vars x.base declared-vars)
        :le_setfield (shallow-lexpr-written-vars x.base declared-vars)
        :le_setfields (shallow-lexpr-written-vars x.base declared-vars)
        :le_setcollectionfields
        (and (member-equal x.base (identifierlist-fix declared-vars))
             (list x.base))
        :le_destructuring (shallow-lexprlist-written-vars x.elts declared-vars)
        :otherwise nil)))
  (define shallow-lexprlist-written-vars ((x lexprlist-p)
                                      (declared-vars identifierlist-p))
    :returns (written-vars (and (set::setp written-vars)
                                (identifierlist-p written-vars)))
    :measure (lexprlist-count x)
    (if (atom x)
        nil
      (set::union (shallow-lexpr-written-vars (car x) declared-vars)
                  (shallow-lexprlist-written-vars (cdr x) declared-vars))))
  ///
  (verify-guards shallow-lexpr-written-vars))

    

(defines shallow-written-vars
  (define shallow-written-vars ((x stmt-p)
                                (declared-vars identifierlist-p))
    :returns (written-vars (and (setp written-vars)
                                (identifierlist-p written-vars)))
    :measure (stmt-count x)
    :verify-guards nil
    (b* ((x (stmt->val x)))
      (stmt_desc-case x
        :s_seq (union (shallow-written-vars x.first declared-vars)
                           (shallow-written-vars x.second declared-vars))
        :s_assign (shallow-lexpr-written-vars x.lexpr declared-vars)
        :s_cond (union (shallow-written-vars x.then declared-vars)
                            (shallow-written-vars x.else declared-vars))
        :s_for (shallow-written-vars x.body declared-vars)
        :s_while (shallow-written-vars x.body declared-vars)
        :s_repeat (shallow-written-vars x.body declared-vars)
        :s_try (union (shallow-written-vars x.body declared-vars)
                      (union
                       (shallow-written-vars-catcherlist x.catchers declared-vars)
                       (shallow-written-vars-maybe x.otherwise declared-vars)))
        :otherwise nil)))

  (define shallow-written-vars-maybe ((x maybe-stmt-p)
                                      (declared-vars identifierlist-p))
    :returns (written-vars (and (setp written-vars)
                                (identifierlist-p written-vars)))
    :measure (maybe-stmt-count x)
    (and x
         (shallow-written-vars x declared-vars)))

  (define shallow-written-vars-catcherlist ((x catcherlist-p)
                                            (declared-vars identifierlist-p))
    :returns (written-vars (and (setp written-vars)
                                (identifierlist-p written-vars)))
    :measure (catcherlist-count x)
    (if (atom x)
        nil
      (union (shallow-written-vars-catcher (car x) declared-vars)
             (shallow-written-vars-catcherlist (cdr x) declared-vars))))

  (define shallow-written-vars-catcher ((x catcher-p)
                                        (declared-vars identifierlist-p))
    :returns (written-vars (and (setp written-vars)
                                (identifierlist-p written-vars)))
    :measure (catcher-count x)
    (b* (((catcher x)))
      (shallow-written-vars x.stmt declared-vars)))
  ///
  (verify-guards shallow-written-vars))





(define typed_identifierlist-lookup ((name identifier-p)
                                     (x typed_identifierlist-p))
  :returns (ty maybe-ty-p)
  (if (atom x)
      nil
    (if (equal (typed_identifier->name (car x)) (identifier-fix name))
        (typed_identifier->type (car x))
      (typed_identifierlist-lookup name (cdr x)))))



(local (defthm >=-len-elim
         (equal (acl2::>=-len x n)
                (and (natp n)
                     (<= n (len x))))))

(local (defthm doublet-listp-implies-all->=-len-2
         (implies (doublet-listp x)
                  (acl2::all->=-len x 2))
         :hints(("Goal" :in-theory (enable acl2::>=-len)))))

(define shallow-loop-reorder-local-vars ((decls typed_identifierlist-p)
                                         (local-vars doublet-listp)) ;; pairs such as ((x "__stdlib_local_x") ...)
  :hooks nil
  (b* ((acl2-vars (strip-cars local-vars))
       (asl-vars (acl2::strip-cadrs local-vars))
       (alist (pairlis$ asl-vars acl2-vars))
       (reord (acl2::fal-extract (typed_identifierlist->names decls) alist))
       (- (let ((diff (set-difference-equal asl-vars (acl2::alist-keys reord))))
            (and diff
                 (er hard? 'shallow-loop-reorder-local-vars "Local variables not found: ~x0" diff)))))
    (pairlis$ (acl2::alist-vals reord)
              (pairlis$ (acl2::alist-keys reord) nil))))


(defines expr-vars
  (define expr-vars ((x expr-p))
    :measure (expr-count x)
    :returns (vars (and (identifierlist-p vars)
                        (setp vars)))
    :verify-guards nil
    (b* ((x (expr->desc x)))
      (expr_desc-case x
        :e_var (list x.name)
        :e_atc (union (expr-vars x.expr)
                      (ty-vars x.type))
        :e_binop (union (expr-vars x.arg1)
                        (expr-vars x.arg2))
        :e_unop (expr-vars x.arg)
        :e_call (b* (((call x.call)))
                  (union (exprlist-vars x.call.params)
                         (exprlist-vars x.call.args)))
        :e_slice (union (expr-vars x.expr)
                        (slicelist-vars x.slices))
        :e_cond (union (expr-vars x.test)
                       (union (expr-vars x.then)
                              (expr-vars x.else)))
        :e_getarray (union (expr-vars x.base)
                           (expr-vars x.index))
        :e_getenumarray (union (expr-vars x.base)
                               (expr-vars x.index))
        :e_getfield (expr-vars x.base)
        :e_getfields (expr-vars x.base)
        :e_getcollectionfields (list x.base)
        :e_getitem (expr-vars x.base)
        :e_record (union (ty-vars x.type)
                         (named_exprlist-vars x.fields))
        :e_tuple (exprlist-vars x.exprs)
        :e_array (union (expr-vars x.length)
                        (expr-vars x.value))
        :e_enumarray (expr-vars x.value)
        :e_arbitrary (ty-vars x.type)
        :e_pattern (union (expr-vars x.expr)
                          (pattern-vars x.pattern))
        :otherwise nil)))

  (define exprlist-vars ((x exprlist-p))
    :measure (exprlist-count x)
    :returns (vars (and (identifierlist-p vars)
                        (setp vars)))
    (if (atom x)
        nil
      (union (expr-vars (car x))
             (exprlist-vars (cdr x)))))

  (define pattern-vars ((x pattern-p))
    :measure (pattern-count x)
    :returns (vars (and (identifierlist-p vars)
                        (setp vars)))
    (b* ((x (pattern->val x)))
      (pattern_desc-case x
        :pattern_any (patternlist-vars x.patterns)
        :pattern_geq (expr-vars x.expr)
        :pattern_leq (expr-vars x.expr)
        :pattern_not (pattern-vars x.pattern)
        :pattern_range (union (expr-vars x.upper) (expr-vars x.lower))
        :pattern_single (expr-vars x.expr)
        :pattern_tuple (patternlist-vars x.patterns)
        :otherwise nil)))

  (define patternlist-vars ((x patternlist-p))
    :measure (patternlist-count x)
    :returns (vars (and (identifierlist-p vars)
                        (setp vars)))
    (if (atom x)
        nil
      (union (pattern-vars (car x))
             (patternlist-vars (cdr x)))))

  (define slice-vars ((x slice-p))
    :measure (slice-count x)
    :returns (vars (and (identifierlist-p vars)
                        (setp vars)))
    (slice-case x
      :slice_single (expr-vars x.index)
      :slice_range (union (expr-vars x.end) (expr-vars x.start))
      :slice_length (union (expr-vars x.start) (expr-vars x.length))
      :slice_star (union (expr-vars x.factor) (expr-vars x.length))))

  (define slicelist-vars ((x slicelist-p))
    :measure (slicelist-count x)
    :returns (vars (and (identifierlist-p vars)
                        (setp vars)))
    (if (atom x)
        nil
      (union (slice-vars (car x))
             (slicelist-vars (cdr x)))))

  (define ty-vars ((x ty-p))
    :measure (ty-count x)
    :returns (vars (and (identifierlist-p vars)
                        (setp vars)))
    (b* ((x (ty->val x)))
      (type_desc-case x
        :t_int (constraint_kind-vars x.constraint)
        :t_bits (expr-vars x.expr)
        :t_tuple (tylist-vars x.types)
        :t_array (union (array_index-vars x.index)
                        (ty-vars x.type))
        :t_record (typed_identifierlist-vars x.fields)
        :t_exception (typed_identifierlist-vars x.fields)
        :t_collection (typed_identifierlist-vars x.fields)
        :otherwise nil)))

  (define tylist-vars ((x tylist-p))
    :measure (tylist-count x)
    :returns (vars (and (identifierlist-p vars)
                        (setp vars)))
    (if (atom x)
        nil
      (union (ty-vars (car x))
             (tylist-vars (cdr x)))))

  (define constraint_kind-vars ((x constraint_kind-p))
    :measure (constraint_kind-count x)
    :returns (vars (and (identifierlist-p vars)
                        (setp vars)))
    (constraint_kind-case x
      :wellconstrained (int_constraintlist-vars x.constraints)
      :otherwise nil))

  (define int_constraint-vars ((x int_constraint-p))
    :measure (int_constraint-count x)
    :returns (vars (and (identifierlist-p vars)
                        (setp vars)))
    (int_constraint-case x
      :constraint_exact (expr-vars x.val)
      :constraint_range (union (expr-vars x.from)
                               (expr-vars x.to))))

  (define int_constraintlist-vars ((x int_constraintlist-p))
    :measure (int_constraintlist-count x)
    :returns (vars (and (identifierlist-p vars)
                        (setp vars)))
    (if (atom x)
        nil
      (union (int_constraint-vars (car x))
             (int_constraintlist-vars (cdr x)))))

  (define array_index-vars ((x array_index-p))
    :measure (array_index-count x)
    :returns (vars (and (identifierlist-p vars)
                        (setp vars)))
    (array_index-case x
      :arraylength_expr (expr-vars x.length)
      :otherwise nil))

  (define typed_identifier-vars ((x typed_identifier-p))
    :measure (typed_identifier-count x)
    :returns (vars (and (identifierlist-p vars)
                        (setp vars)))
    (ty-vars (typed_identifier->type x)))

  (define typed_identifierlist-vars ((x typed_identifierlist-p))
    :measure (typed_identifierlist-count x)
    :returns (vars (and (identifierlist-p vars)
                        (setp vars)))
    (if (atom x)
        nil
      (union (typed_identifier-vars (car x))
             (typed_identifierlist-vars (cdr x)))))

  (define named_expr-vars ((x named_expr-p))
    :measure (named_expr-count x)
    :returns (vars (and (identifierlist-p vars)
                        (setp vars)))
    (expr-vars (named_expr->expr x)))

  (define named_exprlist-vars ((x named_exprlist-p))
    :measure (named_exprlist-count x)
    :returns (vars (and (identifierlist-p vars)
                        (setp vars)))
    (if (atom x)
        nil
      (union (named_expr-vars (car x))
             (named_exprlist-vars (cdr x)))))
  ///
  (verify-guards expr-vars))
      
    
        
    



(program)


(define shallow-loop-var-types (asl-vars decls)
  (b* (((when (atom asl-vars)) nil)
       (type (typed_identifierlist-lookup (car asl-vars) decls))
       (- (and (not type)
               (er hard? 'shallow-loop-formals
                   "type not found for variable: ~x0" (car asl-vars)))))
    (cons type (shallow-loop-var-types (cdr asl-vars) decls))))



;; ---------------------------------------------------------------------------------
;; Computes the resolved types of the variables, which should be in topological order.
;; At the same time, accumulates an env storage expression (nesting of put-assoc-equal terms)
;; and a list of hyps accumulating the type assumptions.
;; Env expressions associate ASL vars with value expressions in terms of ACL2 vars,
;; by simplifying native-to-typed-val.
;; Hyps are derived using (weak-?)ty-satisfied-native
;; Returns (list resolved-type-exprs env-term hyps).

;; (define shallow-resolve-var-types (partialp
;;                                    weakp
;;                                    (asl-vars identifierlist-p)
;;                                    (acl2-vars symbol-listp)
;;                                    (types tylist-p)
;;                                    env-term ;; accumulator
;;                                    hyps
;;                                    state)
;;   (b* (((when (atom fn-args)) (value (list nil env-term hyps)))
;;        (hyp `(and . ,hyps))
;;        ((er type-term) (shallow-resolve-type partialp (car types) hyp env-term state))
;;        ((er type-hyp) (simplify-for-shallow `(,(if weakp 'weak-ty-satisfied-native 'ty-satisfied-native)
;;                                               ,(car acl2-vars) ,type-term)
;;                                              hyp 'iff state))
;;        (new-hyps (cons type-hyp hyps))
;;        ((er val-term) (simplify-for-shallow `(native-to-typed-val ,(car acl2-vars) ,type-term)
;;                                             `(and . ,new-hyps) 'equal state))
;;        (new-env-term `(put-assoc-equal ,(car asl-vars) ,val-term ,env-term))
;;        ((er (list rest env-term hyps))
;;         (shallow-resolve-var-types partialp weakp (cdr asl-vars) (cdr acl2-vars) (cdr types)
;;                                    new-env-term new-hyps state)))
;;     (value (list (cons type-term rest) env-term hyps))))



;; (define def-asl-shallow-loop-fn (name args state)
;;   (b* (((std::extract-keyword-args
;;          :other-args bad-args
;;          :allowed-keys '(:prepwork)
;;          ;; :kwd-alist kwd-alist
;;          function
;;          (looptype :while)
;;          (nth 0)
;;          local-vars ;; pairs such as ((x "__stdlib_local_x") ...)
;;          index-var
;;          (start-var 'start)
;;          (end-var 'end)
;;          measure
;;          ;; enable
;;          ;; disable
;;          ;; hints
;;          ;; prepwork

;;          (static-env '(stdlib-static-env)))
;;         args)
       
;;        ((when bad-args)
;;         (er soft 'def-asl-shallow-loop "Bad arguments: ~x0" bad-args))
;;        ((unless (stringp function))
;;         (er soft 'def-asl-shallow-loop "Function should be a string: ~x0" function))
;;        ((unless (and (doublet-listp local-vars)
;;                      (symbol-listp (strip-cars local-vars))
;;                      (identifierlist-p (acl2::strip-cadrs local-vars))))
;;         (er soft 'def-asl-shallow-loop "Local-vars should be a list of pairs such as ~x0" '(acl2var "asl_var")))

;;        (orig-looptype looptype)
;;        (looptype (case orig-looptype
;;                    ((:while :s_while) :s_while)
;;                    ((:for :s_for) :s_for)
;;                    ((:repeat :s_repeat) :s_repeat)
;;                    (t nil)))
;;        ((unless looptype)
;;         (er soft 'defloop "Bad looptype: ~x0" orig-looptype))
;;        ((unless (natp nth))
;;         (er soft 'defloop "Bad nth: ~x0" nth))
;;        ((acl2::er (cons & static-env-val))
;;         (acl2::simple-translate-and-eval static-env nil nil
;;                                          (msg "static env ~x0" static-env)
;;                                          'def-asl-shallow-loop-fn (w state) state t))
;;        ((unless (static_env_global-p static-env-val))
;;         (er soft 'def-asl-shallow-loop "Bad static env (evaluation of ~x0): doesn't satisfy static_env_global-p" static-env))
;;        (fn-struct (cdr (hons-assoc-equal function
;;                                          (static_env_global->subprograms static-env-val))))
;;        ((unless fn-struct)
;;         (er soft 'def-asl-shallow-loop "Bad function ~x0: not found in static env" function))
;;        ((func-ses fn-struct))
;;        ((func f) fn-struct.fn)
;;        ((unless (subprogram_body-case f.body :sb_asl))
;;         (er soft 'def-asl-shallow-loop "Function is a primitive rather than an ASL function"))
;;        (fn-body (sb_asl->stmt f.body))
       
;;        ((mv loopstmt decls &)
;;         (shallow-loop-and-decls nth looptype fn-body))

;;        (decls (append (params-to-typed_identifierlist f.parameters)
;;                       f.args
;;                       decls))

;;        ((unless form)
;;         (er soft 'def-asl-shallow-loop "Loop not found: nth ~x0 in function ~x1"
;;             nth function))

;;        ((when (and (eq looptype :s_for)
;;                    (not index-var)))
;;         (er soft 'defloop "Index var must be specified for for loops"))
;;        ((when (and index-var (not (eq looptype :s_for))))
;;         (er soft 'defloop "Index var specified for non-for loop"))

;;        (local-vars (if (eq looptype :s_for)
;;                        (cons (list index-var (s_for->index_name form))
;;                              local-vars)
;;                      local-vars))

;;        (local-vars (shallow-loop-reorder-local-vars decls local-vars))
    
;;        (varnames (strip-cars local-vars))
;;        (asl-vars (acl2::strip-cadrs local-vars))
       
;;        (decl-names (typed_identifierlist->names decls))
;;        (diff (difference (mergesort asl-vars) (mergesort decl-names)))
;;        ((when diff)
;;         (er soft 'def-asl-shallow-loop "Variables in local-vars (not declared in function): ~x0" diff))

;;        (written-vars (shallow-written-vars form decl-names))
;;        (written-vars (if (eq looptype :s_for) (set::insert index-var written-vars) written-vars))
;;        (diff (difference (mergesort written-vars) (mergesort asl-vars)))
;;        ((when diff)
;;         (er soft 'def-asl-shallow-loop "Loop writes variables missing from local-vars: ~x0" diff))
            
;;        ;; It seems complicated to determine automatically exactly what type
;;        ;; theorems are needed for a loop.  We can get the types of the
;;        ;; variables read and written in the loop, but those types might depend
;;        ;; on other variables not otherwise involved in the loop, perhaps
;;        ;; intervals with complicated expressions in them.  So for now we'll
;;        ;; punt on this and require the user to write theorems.  Then maybe
;;        ;; we'll add support for a simplified syntax for such theorems, and
;;        ;; maybe automatically generate some of them.

;;        ;; For now, we'll just get the basic argument types from the types,
;;        ;; without anything requiring parameters. This is implemented by the
;;        ;; weak-*-satisfied* functions.
;;        (partialp t) (weakp t)
;;        (var-types (shallow-loop-var-types asl-vars decl-names))
;;        ((er (list resolved-var-types env-term type-assums))
;;         (shallow-resolve-var-types partialp weakp
;;                                    asl-vars varnames var-types
;;                                    '(local-env->storage (env->local env))
;;                                    nil state))
                                                  
;;        (resolved-var-types-res (name-resolve-tylist static-env-val var-types :clk 1000))
;;        ((unless (eval_result-case resolved-var-types-res :ev_normal))
;;         (er soft 'def-asl-shallow-loop "Couldn't resolve some named types in local vars: ~x0" resolved-var-types-res))
;;        (weak-resolved-var-types (ev_normal->res resolved-var-types-res))
;;        (weak-var-type-terms
;;         (if weakp
;;             (kwote-lst resolved-var-types)
;;           resolved-var-types)

;;        ((er formals) (shallow-define-formals weakp varnames weak-var-type-terms t state))

;;        (guard-hyps (acl2::strip-cadrs formals))

;;        ((er fixer-al)
;;         (shallow-formal-fixer-alist weakp varnames weak-var-type-terms t state))
;;        (fixers (shallow-formal-fixer-bindings fixer-al))

;;        (direct-subprograms (collect-direct-subprograms form nil))
;;        (subprog-table  (table-alist 'asl-subprogram-table (w state)))
;;        (subprograms (collect-transitive-subprograms direct-subprograms subprog-table nil))
;;        (clk-val
;;         (or safe-clock
;;             (+ 1 (maximize-const-clocks direct-subprograms table -1))))

;;        (match-hyp `(subprograms-match ',subprograms
;;                                       (global-env->static (env->global env))
;;                                       ,static-env))
;;        (measure-reqs (if (eql clk-val 0)
;;                          t
;;                        `(<= ,clk-val (ifix clk))))

       
;;        (body-hyp (list* 'and match-hyp measure-reqs
;;                         '(no-duplicatesp-equal (acl2::alist-keys env.local.storage))
;;                         guard-hyps))

;;        ;; To get the body of the function, we want to rewrite the test
;;        ;; expression and evaluation of the body under an environment where, when we look up an ASL variable, we get a value expressed in terms of
;;        ;; the associated formal of the function.
;;        ;; E.g., if the function input signature is foo {N} (x : integer, y : bits(N)) and the ACL2 formals are (n x y),
;;        ;; then we need (cdr (hons-assoc-equal "N" env.local.storage)) to be a v_int whose value is n,
;;        ;; similarly for x, and for y we need a v_bitvector object whose length is n and value is y.
;;        ;; A complication is that while we'd like bitvector widths to be associated with known values when possible (as in the case above), we might have
;;        ;; some that are expressed in terms of other variables that don't need to be inputs to our function.
;;        ;; So this leads to a complicated compromise: When a bitvector's bitwidth is expressed in terms of variables that we know about, we'll assume that it equals that expression's evaluation,
;;        ;; otherwise we won't assume anything about it.
       
;;        ;; As a hack for doing this, we'll collect these environment conditions
;;        ;; for each variable in the order they were declared, under the
;;        ;; environment conditions produced by the previous variables, plus the
;;        ;; assumption that no other variables are set in the environment. We
;;        ;; rewrite the evaluations of all such bitwidth expressions and
;;        ;; determine whether they always evaluate to a normal, integer value; if
;;        ;; so, we include the assumption that the bitvector's width is that value.
;;        ((er env-assums)
;;         (shallow-loop-variable-constraints varnames asl-vars resolved-var-types nil
;;                                            ;; Add a way to add user specified assumptions?
;;                                            (list match-hyp)
;;                                            state))

;;        ;; test: eval result of boolean where if true, loop is done
;;        (test-term (case looptype
;;                     (:s_for `(ev_normal (for_loop-test ,start-var ,end-var ,(s_for->dir form))))
;;                     (otherwise
;;                      `(b* (((mv (evo (expr_result cres)) orac) (eval_expr env e_cond))
;;                            ((evo cbool) (v_to_bool cres.val)))
;;                         (ev_normal ,(if (eq looptype :s_while)
;;                                         '(not cbool)
;;                                       'cbool))))))

;;        ((er test-term-simp) (simplify-for-shallow test-term body-hyp

       
       
;;        (body-term `(eval_block env ,(case looptype
;;                                       (:s_for (s_for->body form))
;;                                       (:s_while (s_while->body form))
;;                                       (:s_repeat (s_repeat->body form)))))
       
;;        )
       
;;     (value `(define ,name ,formals
;;               :returns (returnval ,return-concl
;;                                   :rule-classes nil :name ,return-sig-thmname)
;;               (let* ,fixers
;;                 ,body)
;;               ///

;;               (defret ,return-rewrite-thmname
;;                 ,return-rewrites
;;                 :hints (("goal" :use ,return-sig-thmname
;;                          :in-theory (disable ,name)))
;;                 :rule-classes :rewrite)
              
;;               ,@(and return-type-prescrip
;;                      `((defret ,return-type-prescrip-thmname
;;                          ,return-type-prescrip
;;                          :hints (("goal" :use ,return-sig-thmname
;;                                   :in-theory (disable ,name
;;                                                       ,return-rewrite-thmname)))
;;                          :rule-classes :type-prescription)))

;;               ,@(and return-reverse
;;                      `(;; (defret ,return-forward-thmname
;;                        ;;   ,return-forward
;;                        ;;   :hints (("goal" :use ,return-sig-thmname
;;                        ;;            :in-theory (disable ,name
;;                        ;;                                ,return-rewrite-thmname)))
;;                        ;;   :rule-classes ((:forward-chaining :trigger-terms ((equal (shallow_result-kind <call>)
;;                        ;;                                                            :sh_normal)))))
;;                        (defret ,return-reverse-thmname
;;                          ,return-reverse
;;                          :hints (("goal" :use ,return-sig-thmname
;;                                   :in-theory (disable ,name
;;                                                       ,return-rewrite-thmname))))))
              
;;               (def-asl-subprogram ,(intern-in-package-of-symbol
;;                                     (concatenate 'string (symbol-name name) "-SHALLOW-CORRECT")
;;                                     'asl-pkg)
;;                 :function ,function
;;                 :params ,params
;;                 :args ,args
;;                 :safe-clock ,safe-clock
;;                 ;; need list of formals wrapped in val type constructors
;;                 :bindings ((impl (,name  . ,asl-to-native-args))
;;                            ,@(if always-normal
;;                                  `((,return-binder impl))
;;                                `(((sh_normal impl))
;;                                  (,return-binder impl.res))))
;;                 ,@(and (not always-normal)
;;                        `(:normal-cond (shallow_result-case impl :sh_normal)
;;                          :nonnormal-res (shallow_result-case impl
;;                                           :sh_error (ev_error impl.desc impl.data)
;;                                           :otherwise (b* (((sh_throwing impl)))
;;                                                        (ev_throwing impl.throwdata
;;                                                                     (change-env env :local (empty-local-env)))))))
;;                 :return-values ,asl-return-exprs)))))




;; (defmacro def-asl-shallow-loop (name &rest args)
;;   (let* ((prepwork (cadr (assoc-keyword :prepwork args))))
;;     `(defsection ,name
;;        ,@prepwork
;;        (make-event (def-asl-shallow-loop-fn ',name ',args state)))))

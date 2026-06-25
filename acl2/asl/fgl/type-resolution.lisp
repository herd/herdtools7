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
(include-book "centaur/fgl/fgl-object" :dir :system)
(include-book "centaur/fgl/ctrex-utils" :dir :system)
(include-book "defs")
(include-book "enums")
(include-book "centaur/fgl/helper-utils" :dir :system)
(local (include-book "std/lists/sets" :dir :System))
(local (include-book "centaur/bitops/ihsext-basics" :dir :system))
(local (std::add-default-post-define-hook :fix))

;; ----------------------------------------------------------------------------------
;; Overall representation of symbolic ASL values.
;;
;; For non-atomic ASL value types -- tuples, arrays, and
;; records/exceptions/collections -- we want to represent symbolic values
;; "lazily".  For example, if we look up the initial value of some variable of
;; record type in the global storage, we don't want to generate symbolic
;; representations of all the fields of those records; instead, we want to
;; represent it just as the lookup of that variable in the state, and work out
;; the shapes of the fields if and when they are accessed/updated. Same if that
;; variable has a record-typed field and we look up that field. Same if the
;; variable is an array or tuple rather than a record, etc. Same if instead of
;; a global lookup we have a top-level ACL2 theorem variable that is asserted
;; to be an ASL typed value. Same if we generate a value from the oracle via
;; ARBITRARY.

;; To simplify the representation, we'll deal in symbolic ASL values of the
;; following base forms:
;;  - concrete values;
;;  - explicit ASL value constructors: e.g., v_int of a symbolic integer,
;;    v_record of a symbolic val-imap (which see below), v_array of a list
;;    of symbolic values;
;;  - ty-fix-val of some variable or access.

;; and the following conditional representations:
;;  - if-of-ty-fix-vals, a nesting of IFs where the leaf branches are ty-fix-val forms
;;  - if-of-other-with-ty-fix-vals, an IF where the then branch is some
;;    explicit or concrete ASL value and the else branch is a ty-fix-val or
;;    if-of-ty-fix-vals.

;; The ty-fix-val case encompasses all the lazy value representations.

;; To get all the various kinds of lazy values into this representation we need
;; a few rules:

;; - Global variable type resolution: a val-imap-lookup checks whether the imap
;;   has a val-imap-satisfies-types assumption (see below), looks up the
;;   appropriate type and wraps itself in a ty-fix of that type (rule
;;   ty-fix-of-global-lookup).
;; - Record field type resolution: a val-imap-lookup of a key in a ty-fix-val
;;   checks the type for whether the key is present and the type it's bound to,
;;   and produces a ty-fix-val of the lookup with the field type (rule
;;   val-imap-lookup-rec-of-ty-fix-val).
;; - Array entry type resolution: a v_array-nth of an index in a ty-fix-val
;;   checks that the type is an array or tuple and that the index is in bounds,
;;   and produces the ty-fix-val of the lookup with the field / entry type
;;   (rule v_array-nth-of-ty-fix-val).
;; - Oracle value type resolution: an oracle lookup always produces a
;;   ty-fix-val of the appropriate type (rule ty-oracle-val-fgl).
;; - ACL2 variables: If a theorem variable is supposed to be an ASL typed value,
;;   we need to have (val-p x) and (ty-satisfied x type) hypotheses (in that order).
;;   Then we rewrite the (ty-satisfied x type) hyp to an assumption
;;   (equal var (ty-fix-val var type)), which will cause the variable to be replaced
;;   with the ty-fix-val form whenever it is used (rule ty-satisfied-of-variable-hyp).

;; ----------------------------------------------------------------------------------
;; Global variable type resolution (historical -- we don't do it this way anymore, see above)

;; To get type information about global variables (and stuff nested inside
;; global variables) we first assume that the global storage of the env is
;; well-typed according to a constant taken from the static env -- e.g.
;; (val-imap-satisfies-types storage (resolve-storage-types (my-static-env)))
;; The resolve-storage-types call just resolves named types everywhere so we
;; don't have to deal with those.

;; Once we've assumed this then we need to know how to figure out the type of a
;; given global variable or component. We do this using a binder function
;; bind-ty-satisfied, which is supposed to find a type that the object
;; satisfies. Rules such as bind-ty-satisfied-of-global-lookup,
;; bind-ty-satisfied-of-v_array-nth, etc. say how to find a type for various
;; forms of terms.  Ultimately, we'll end up looking for a
;; val-imap-satisfies-types assumption (via the rule
;; bind-val-imap-satisfies-types-by-hyp) and looking up the variable's type in
;; the storage types alist, then getting the subcomponent's type out of that
;; type.

(fgl::remove-fgl-rewrites array-type-fix-val)


(fgl::def-fgl-rewrite vallist-p-of-array-type-fix-val
  (vallist-p (array-type-fix-val n x ty)))


(fgl::add-fgl-rewrite vallist-fix-when-vallist-p)



(define val-imap-satisfies-types ((x val-imap-p)
                                  (types ty-global_decl_keyword-imap-p))
  :guard (ty-global_decl_keyword-imap-resolved-p types)
  :guard-hints (("goal" :in-theory (enable ty-global_decl_keyword-imap-resolved-p)))
  :verify-guards nil
  (if (atom types)
      t
    (and (or (not (mbt (and (consp (Car types))
                            (identifier-p (caar types)))))
             (let ((look (omap::assoc (caar types) (val-imap-fix x))))
               (and look
                    (ty-satisfied
                     (cdr look)
                     (ty-global_decl_keyword->ty (cdar types))))))
         (val-imap-satisfies-types x (cdr types))))
  ///
  (defthmd val-imap-satisfies-types-implies
    (implies (and (val-imap-satisfies-types x types)
                  (identifier-p key)
                  (hons-assoc-equal key types))
             (and (val-imap-has-key key x)
                  (ty-satisfied (val-imap-lookup key x)
                                (ty-global_decl_keyword->ty (cdr (hons-assoc-equal key types))))))
    :hints(("Goal" :in-theory (enable hons-assoc-equal
                                      omap::lookup
                                      val-imap-lookup
                                      val-imap-has-key))))

  (local (in-theory (enable ty-global_decl_keyword-imap-fix))))




(define bind-val-imap-satisfies-types (types x)
  :verify-guards nil
  (if (val-imap-satisfies-types x types)
      types
    nil))

(fgl::remove-fgl-rewrite bind-val-imap-satisfies-types)

;; (fgl::def-fgl-brewrite bind-ty-satisfied-of-global-lookup
;;   (implies (and (bind-val-imap-satisfies-types types x)
;;                 (syntaxp (fgl::fgl-object-case types :g-concrete))
;;                 (identifier-p key)
;;                 (hons-assoc-equal key types)
;;                 (equal type (ty-global_decl_keyword->ty (cdr (hons-assoc-equal key types)))))
;;            (equal (bind-ty-satisfied type (val-imap-lookup key x)) type))
;;   :hints(("Goal" :in-theory (enable bind-val-imap-satisfies-types
;;                                     val-imap-lookup))))

(fgl::def-fgl-rewrite ty-fix-of-global-lookup
  (implies (and (bind-val-imap-satisfies-types types x)
                (syntaxp (fgl::fgl-object-case types :g-concrete))
                (identifier-p key)
                (hons-assoc-equal key types)
                (equal type (ty-global_decl_keyword->ty (cdr (hons-assoc-equal key types)))))
           (equal (val-imap-lookup (fgl::concrete key) x)
                  (ty-fix-val (fgl::fgl-hide (val-imap-lookup key x)) type)))
  :hints(("Goal" :in-theory (enable bind-val-imap-satisfies-types
                                    val-imap-satisfies-types-implies))))


(fgl::def-fgl-rewrite lookup-exists-by-bind-val-imap-satisfies-types
  (implies (and (bind-val-imap-satisfies-types types x)
                (syntaxp (fgl::fgl-object-case types :g-concrete))
                (identifier-p key)
                (hons-assoc-equal key types))
           (equal (val-imap-has-key key x) t))
  :hints(("Goal" :in-theory (enable val-imap-satisfies-types-implies
                                    bind-val-imap-satisfies-types))))

;; (fgl::def-fgl-rewrite val-p-of-lookup-in-val-imap
;;   (implies (val-imap-p x)
;;            (iff (val-p (cdr (hons-assoc-equal key x)))
;;                 (hons-assoc-equal key x))))



(define hide-var (x) x)

(fgl::def-ctrex-rule hide-var-ctrex-rule
  :match ((val (hide-var x)))
  :assigned-var x
  :assign val
  :ruletype :elim)

(fgl::def-fgl-rewrite ty-satisfied-of-variable-hyp
  (implies (and (fgl::syntax-bind
                 ok (and (fgl::fgl-object-case val :g-var)
                         (b* ((frames (fgl::interp-st-stack-frames 'interp-st)))
                           ;; note: we want the rule to be applied to something
                           ;; in the top-level theorem (not backchaining or
                           ;; rewriting something's rhs), which means this rule
                           ;; will be in frame 2.
                           (eql 2 frames))))
                (val-p val)
                (ty-satisfiable ty))
           (iff (ty-satisfied val ty)
                (let ((fix (ty-fix-val val ty)))
                  (and (fgl::fgl-hide (ty-satisfied val ty))
                       (fgl::fgl-hide (equal val fix)) t)))))

(fgl::def-ctrex-rule ty-satisfied-ctrex-rule
    :match ((pred (ty-satisfied x ty)))
    :assign-cond pred
    :assigned-var x
    :assign (ty-fix-val x ty)
    :ruletype :fixup)



;; (local (defthm ty-satisfied-of-nth-when-array-type-satisfied
;;          (implies (and (array-type-satisfied arr ty)
;;                        (< (nfix n) (len arr)))
;;                   (ty-satisfied (nth n arr) ty))
;;          :hints(("Goal" :in-theory (enable nth array-type-satisfied)))))





;; (fgl::def-fgl-rewrite v_bitvector->val-when-bind-ty-satisfied
;;   (b* (((t_bits tb) (ty->desc ty)))
;;     (implies (and (bind-ty-satisfied ty x)
;;                   (syntaxp (fgl::fgl-object-case ty :g-concrete))
;;                   (type_desc-case (ty->desc ty) :t_bits)
;;                   (int-literal-expr-p tb.expr))
;;              (and (equal (v_bitvector->val x)
;;                          (loghead (nfix (int-literal-expr->val tb.expr))
;;                                   (fgl::fgl-hide (v_bitvector->val x))))
;;                   (equal (v_bitvector->len x)
;;                          (nfix (int-literal-expr->val tb.expr))))))
;;   :hints(("Goal" :in-theory (enable bind-ty-satisfied)
;;           :expand ((ty-satisfied x ty)))))

;; (fgl::def-fgl-rewrite v_int->val-when-bind-ty-satisfied
;;   (b* (((t_int tb) (ty->desc ty)))
;;     (implies (and (bind-ty-satisfied ty x)
;;                   (syntaxp (fgl::fgl-object-case ty :g-concrete))
;;                   (type_desc-case (ty->desc ty) :t_int)
;;                   (constraint_kind-case tb.constraint :wellconstrained)
;;                   (equal cstrs (wellconstrained->constraints tb.constraint))
;;                   (int_constraintlist-resolved-p cstrs))
;;              (equal (v_int->val x)
;;                     (int_constraintlist-value-fix
;;                      (fgl::fgl-hide (v_int->val x))
;;                      cstrs))))
;;   :hints(("Goal" :in-theory (enable bind-ty-satisfied
;;                                     constraint_kind-satisfied)
;;           :expand ((ty-satisfied x ty)))))

;; (fgl::def-fgl-rewrite  val-kind-when-bind-ty-satisfied
;;   (implies (and (bind-ty-satisfied ty x)
;;                 (syntaxp (fgl::fgl-object-case ty :g-concrete)))
;;            (equal (val-kind x)
;;                   (b* ((ty (ty->desc ty)))
;;                     (type_desc-case ty
;;                       (:t_int :v_int)
;;                       (:t_bits :v_bitvector)
;;                       (:t_real :v_real)
;;                       (:t_string :v_string)
;;                       (:t_bool :v_bool)
;;                       (:t_enum :v_label)
;;                       (:t_tuple :v_array)
;;                       (:t_array :v_array)
;;                       (otherwise :v_record)))))
;;   :hints (("goal" :in-theory (enable bind-ty-satisfied)
;;            :expand ((ty-satisfied x ty)))))



(fgl::def-fgl-brewrite bind-val-imap-satisfies-types-by-hyp
  (implies (and (fgl::match-assums (val-imap-satisfies-types storage types1))
                (equal types types1))
           (equal (bind-val-imap-satisfies-types types storage)
                  types))
  :hints(("Goal" :in-theory (enable bind-val-imap-satisfies-types))))



(fgl::remove-fgl-rewrite val-imap-satisfies-types)





(local (defthm car-last-of-append
         (equal (car (last (append x y)))
                (if (consp y)
                    (car (last y))
                  (car (last x))))))

(local (defthm car-last-of-rev
         (equal (car (last (acl2::rev x)))
                (car x))
         :hints(("Goal" :in-theory (enable acl2::Rev)))))


(defsection ty-fix-val-rules
  (fgl::remove-fgl-rewrite ty-fix-val)

  ;; do we want to do this?
  (fgl::def-fgl-rewrite ty-fix-val-of-atomic
    (equal (ty-fix-val x ty)
           (b* ((ty.desc (ty->desc ty)))
             (type_desc-case ty.desc
               (:t_int (v_int (constraint_kind-value-fix (v_int->val x)
                                                         ty.desc.constraint)))
               (:t_bits (v_bitvector (nfix (int-literal-expr->val ty.desc.expr))
                                     (loghead* (int-literal-expr->val ty.desc.expr)
                                               (v_bitvector->val x))))
               (:t_real (v_real (v_real->val x)))
               (:t_string (v_string (v_string->val x)))
               (:t_bool (v_bool (and (v_bool->val x) t)))
               (:T_ENUM (let* ((elts ty.desc.elts)
                               (rev-elts (acl2::rev elts))
                               (val (v_label->val x))
                               (ignore (fgl::trigger-constraints
                                        ;; Why do we skip the first element? see above.
                                        (trigger-enum-value-constraint val (cdr elts)))))
                          (declare (ignore ignore))
                          (v_label (choose-value (some-value-chooser val rev-elts)))))
               (:otherwise (fgl::abort-rewrite (ty-fix-val x ty))))))
    :hints (("goal" :Expand ((ty-fix-val x ty)))))

  ;; Note: not sure if the following two rules are really desirable
  (fgl::def-fgl-rewrite ty-fix-val-of-v_record
    (equal (ty-fix-val (v_record rec) ty)
           (b* ((ty.desc (ty->desc ty)))
             (type_desc-case ty.desc
               (:t_record (v_record (record-type-fix-val rec ty.desc.fields)))
               (:t_exception (v_record (record-type-fix-val rec ty.desc.fields)))
               (:t_collection (v_record (record-type-fix-val rec ty.desc.fields)))
               (:otherwise (fgl::abort-rewrite (ty-fix-val (v_record rec) ty))))))
    :hints (("goal" :expand ((ty-fix-val (v_record rec) ty)))))


  (fgl::def-fgl-rewrite ty-fix-val-of-v_array
    (equal (ty-fix-val (v_array arr) ty)
           (b* ((ty.desc (ty->desc ty)))
             (type_desc-case ty.desc
               (:t_tuple (v_array (tuple-type-fix-val arr ty.desc.types)))
               (:t_array
                (v_array
                 (array-type-fix-val (nfix (int-literal-expr->val ty.desc.index))
                                     arr ty.desc.type)))
               (:otherwise (fgl::abort-rewrite (ty-fix-val (v_array arr) ty))))))
    :hints (("goal" :expand ((ty-fix-val (v_array arr) ty)))))


  ;; Actions of various accessors applied to ty-fix-val objects.  Note the
  ;; accessors for atomic values are unlikely to be used unless we change or
  ;; disable ty-fix-val-of-atomic (which should make it so any ty-fix-val we
  ;; see is of a non-atomic type).
  (fgl::def-fgl-rewrite kind-of-ty-fix-val
    (equal (val-kind (ty-fix-val x ty))
           (b* ((desc (ty->desc ty)))
             (type_desc-case desc
               (:t_int :v_int)
               (:t_bits :v_bitvector)
               (:t_real :v_real)
               (:t_string :v_string)
               (:t_bool :v_bool)
               (:t_enum :v_label)
               (:t_tuple :v_array)
               (:t_array :v_array)
               (:t_record :v_record)
               (:t_exception :v_record)
               (:t_collection :v_record)
               (otherwise (fgl::abort-rewrite (val-kind (ty-fix-val x ty)))))))
    :hints(("Goal"
            :expand ((ty-satisfiable ty)
                     (ty-fix-val x ty)))))

  ;; likely unused
  (fgl::def-fgl-rewrite v_int->val-of-ty-fix-val
    (equal (v_int->val (ty-fix-val x ty))
           (b* ((desc (ty->desc ty)))
             (type_desc-case desc
               :t_int (constraint_kind-value-fix
                       (v_int->val x) desc.constraint)
               :otherwise (fgl::abort-rewrite (v_int->val (ty-fix-val x ty))))))
    :hints(("Goal"
            :expand ((ty-satisfiable ty)
                     (ty-fix-val x ty)))))

  (fgl::def-fgl-rewrite v_bitvector->len-of-ty-fix-val
    (equal (v_bitvector->len (ty-fix-val x ty))
           (b* ((desc (ty->desc ty)))
             (type_desc-case desc
               :t_bits (nfix (int-literal-expr->val desc.expr))
               :otherwise (fgl::abort-rewrite (v_bitvector->len (ty-fix-val x ty))))))
    :hints(("Goal"
            :expand ((ty-satisfiable ty)
                     (ty-fix-val x ty)))))

  (fgl::def-fgl-rewrite v_bitvector->val-of-ty-fix-val
    (equal (v_bitvector->val (ty-fix-val x ty))
           (b* ((desc (ty->desc ty)))
             (type_desc-case desc
               :t_bits (LOGHEAD* (INT-LITERAL-EXPR->VAL desc.EXPR)
                                 (V_BITVECTOR->VAL X))
               :otherwise (fgl::abort-rewrite (v_bitvector->val (ty-fix-val x ty))))))
    :hints(("Goal" :in-theory (enable loghead*)
            :expand ((ty-satisfiable ty)
                     (ty-fix-val x ty)))))

  (fgl::def-fgl-rewrite v_real->val-of-ty-fix-val
    (equal (v_real->val (ty-fix-val x ty))
           (b* ((desc (ty->desc ty)))
             (type_desc-case desc
               :t_real (v_real->val x)
               :otherwise (fgl::abort-rewrite (v_real->val (ty-fix-val x ty))))))
    :hints(("Goal"
            :expand ((ty-satisfiable ty)
                     (ty-fix-val x ty)))))

  (fgl::def-fgl-rewrite v_string->val-of-ty-fix-val
    (equal (v_string->val (ty-fix-val x ty))
           (b* ((desc (ty->desc ty)))
             (type_desc-case desc
               :t_string (v_string->val x)
               :otherwise (fgl::abort-rewrite (v_string->val (ty-fix-val x ty))))))
    :hints(("Goal"
            :expand ((ty-satisfiable ty)
                     (ty-fix-val x ty)))))

  (fgl::def-fgl-rewrite v_bool->val-of-ty-fix-val
    (equal (v_bool->val (ty-fix-val x ty))
           (b* ((desc (ty->desc ty)))
             (type_desc-case desc
               :t_bool (and (v_bool->val x) t)
               :otherwise (fgl::abort-rewrite (v_bool->val (ty-fix-val x ty))))))
    :hints(("Goal"
            :expand ((ty-satisfiable ty)
                     (ty-fix-val x ty)))))

  (local (defun ind (x types)
           (if (atom types)
               x
             (ind (cdr x) (cdr types)))))

  (local (defthm len-of-tuple-type-fix-val
           (equal (len (tuple-type-fix-val x types))
                  (len types))
           :hints (("goal" :induct (ind x types)
                    :expand ((tuple-type-fix-val x types)
                             (tuple-type-fix-val nil types))))))

  (local (defun n-ind (n x types)
           (if (zp n)
               (list x types)
             (n-ind (1- n) (cdr x) (cdr types)))))
  (local (defthm nth-of-tuple-type-fix-val
           (implies (< (nfix n) (len types))
                    (equal (nth n (tuple-type-fix-val x types))
                           (ty-fix-val (nth n x) (nth n types))))
           :hints(("Goal"
                   :induct (n-ind n x types)
                   :expand ((tuple-type-fix-val x types))))))



  (local (defun n-len-ind (n len x)
           (if (zp n)
               (list len x)
             (n-len-ind (1- n) (1- len) (cdr x)))))

  (local (defthm nth-of-array-type-fix-val
           (implies (< (nfix n) (nfix len))
                    (equal (nth n (array-type-fix-val len x type))
                           (ty-fix-val (nth n x) type)))
           :hints(("Goal" :induct (n-len-ind n len x)
                   :expand ((array-type-fix-val len x type))))))


  ;; Array accessors: v_array-len, v_array-nth

  (fgl::def-fgl-rewrite v_array-len-of-ty-fix-val
    (equal (v_array-len (ty-fix-val x ty))
           (b* ((desc (ty->desc ty)))
             (type_desc-case desc
               :t_tuple (len desc.types)
               :t_array (nfix (int-literal-expr->val desc.index))
               :otherwise (fgl::abort-rewrite (v_array-len (ty-fix-val x ty))))))
    :hints(("Goal"
            :in-theory (enable v_array-len)
            :expand ((ty-satisfiable ty)
                     (ty-fix-val x ty)))))

  (fgl::def-fgl-rewrite v_array-nth-of-ty-fix-val
    (equal (v_array-nth n (ty-fix-val x ty))
           (b* ((desc (ty->desc ty)))
             (type_desc-case desc
               :t_tuple (if (< (nfix n) (len desc.types))
                            (ty-fix-val (v_array-nth n x) (nth n desc.types))
                          (fgl::abort-rewrite (v_array-nth n (ty-fix-val x ty))))
               :t_array
               (b* ((len (int-literal-expr->val desc.index))
                    (in-bounds
                     (fgl-mark 'v_array-nth-in-bounds
                               (fgl::fgl-validity-check
                                (fgl::make-fgl-ipasir-config)
                                (and (< (nfix n) len) t))))
                    ((unless in-bounds)
                     (fgl::abort-rewrite (v_array-nth n (ty-fix-val x ty))))
                    (?ign (not (fgl::trigger-constraints
                                (v_array-nth-resolve-equal-indices
                                 n x desc.type)))))
                 (ty-fix-val (v_array-nth n x) desc.type))
               :otherwise (fgl::abort-rewrite (v_array-nth n (ty-fix-val x ty))))))
    :hints(("Goal"
            :do-not-induct t
            :in-theory (enable v_array-nth)
            :expand ((ty-satisfiable ty)
                     (ty-fix-val x ty)))))

  ;; (fgl::def-fgl-rewrite v_record->rec-of-ty-fix-val
  ;;   (equal (v_record->rec (ty-fix-val x ty))
  ;;          (b* ((desc (ty->desc ty)))
  ;;            (type_desc-case desc
  ;;              :t_record (record-type-fix-val (v_record->rec x) desc.fields)
  ;;              :t_exception (record-type-fix-val (v_record->rec x) desc.fields)
  ;;              :t_collection (record-type-fix-val (v_record->rec x) desc.fields)
  ;;              :t_array (fgl::abort-rewrite (v_record->rec (ty-fix-val x ty)))
  ;;              :otherwise (fgl::abort-rewrite (v_record->rec (ty-fix-val x ty))))))
  ;;   :hints(("Goal"
  ;;           :expand ((ty-satisfiable ty)
  ;;                    (ty-fix-val x ty)))))

  (defthm val-imap-lookup-of-record-type-fix-val
    (equal (val-imap-lookup key (record-type-fix-val rec desc.fields))
           (let* ((type-look (typed_identifierlist-lookup key desc.fields))
                  (rec-look (val-imap-lookup key rec)))
             (if type-look
                 (ty-fix-val rec-look type-look)
               (val-fix nil))))
    :hints(("Goal" :in-theory (enable val-imap-lookup
                                      typed_identifierlist-lookup
                                      omap::lookup-of-update)
            :expand ((record-type-fix-val rec desc.fields))
            :induct (typed_identifierlist-lookup key desc.fields))))

  (local (in-theory (disable typed_identifierlist-lookup-under-iff)))

  (defthm val-imap-has-key-of-record-type-fix-val
    (iff (val-imap-has-key key (record-type-fix-val rec desc.fields))
         (typed_identifierlist-lookup key desc.fields))
    :hints(("Goal" :in-theory (e/d (val-imap-has-key
                                    typed_identifierlist-lookup
                                    omap::lookup-of-update)
                                   ())
            :expand ((record-type-fix-val rec desc.fields))
            :induct (typed_identifierlist-lookup key desc.fields))))

  (defthm val-imap-has-key-of-record-type-fix-val
    (iff (val-imap-has-key key (record-type-fix-val rec desc.fields))
         (typed_identifierlist-lookup key desc.fields))
    :hints(("Goal" :in-theory (enable val-imap-has-key
                                      typed_identifierlist-lookup
                                      omap::lookup-of-update)
            :expand ((record-type-fix-val rec desc.fields))
            :induct (typed_identifierlist-lookup key desc.fields))))

  (local (defthm member-of-typed_identifierlist->names
           (iff (member-equal k (typed_identifierlist->names x))
                (and (identifier-p k)
                     (typed_identifierlist-lookup k x)))
           :hints(("Goal" :in-theory (enable typed_identifierlist-lookup
                                             typed_identifierlist->names)
                   :induct (typed_identifierlist->names x)))))


  (defthm val-imap-keys-of-record-type-fix-val
    (equal (val-imap-keys (record-type-fix-val rec desc.fields))
           (mergesort (typed_identifierlist->names desc.fields)))
    :hints(("goal" :in-theory (enable pick-a-point-subset-strategy
                                      set::double-containment-no-backchain-limit))))

  (local (defthm lookup-of-from-lists
           (implies (member-equal key keys)
                    (equal (omap::lookup key (omap::from-lists keys vals))
                           (nth (acl2::index-of key keys) vals)))
           :hints(("Goal" :in-theory (enable omap::lookup omap::from-lists acl2::index-of)))))

  (local (defthm val-imap-lookup-of-from-lists
           (implies (and (identifierlist-p keys)
                         (vallist-p vals)
                         (equal (len keys) (len vals))
                         (double-rewrite (member-equal (identifier-fix key) keys)))
                    (equal (val-imap-lookup key (omap::from-lists keys vals))
                           (nth (acl2::index-of (identifier-fix key) keys) vals)))
           :hints(("Goal" :in-theory (enable val-imap-lookup)))))

  (local (defthm index-of-bound
           (implies (double-rewrite (member-equal k x))
                    (< (acl2::index-of k x) (len x)))
           :hints(("Goal" :in-theory (enable acl2::index-of)))
           :rule-classes :linear))

  (local (defthm index-of-type
           (implies (double-rewrite (member-equal k x))
                    (natp (acl2::index-of k x)))
           :hints(("Goal" :in-theory (enable acl2::index-of)))
           :rule-classes :rewrite))

  (local (defthm member-of-mergesort
           (iff (member-equal k (mergesort x))
                (member-equal k x))
           :hints (("goal" :use ((:instance set::in-mergesort-under-iff
                                  (a k) (x x)))
                    :in-theory (e/d (set::in-to-member)
                                    (set::in-mergesort-under-iff))))))

  ;; Record accessors: val-imap-lookup, val-imap-has-key, val-imap-keys
  (fgl::def-fgl-rewrite val-imap-lookup-rec-of-ty-fix-val
    (equal (val-imap-lookup key (v_record->rec (ty-fix-val x ty)))
           (b* ((desc (ty->desc ty))
                ((mv recp fields type)
                 (type_desc-case desc
                   :t_record (mv t desc.fields nil)
                   :t_exception (mv t desc.fields nil)
                   :t_collection (mv t desc.fields nil)
                   :t_array (mv nil nil nil)
                   :otherwise (mv nil nil nil)))
                ((unless fields)
                 (fgl::fgl-progn
                  (fgl::fgl-error :msg "Bad type in val-imap-lookup-rec-of-ty-fix-val")
                  (fgl::abort-rewrite (val-imap-lookup key (v_record->rec (ty-fix-val x ty))))))
                ((unless recp)
                 (if (member-equal (identifier-fix key) fields)
                     (ty-fix-val (val-imap-lookup key (v_record->rec (fgl::fgl-hide x))) type)
                   (fgl::fgl-progn
                   (fgl::fgl-error :msg "Val-imap-lookup-rec-of-ty-fix-val bad key")
                    (fgl::abort-rewrite (val-imap-lookup key (v_record->rec (ty-fix-val x ty)))))))
                (type (typed_identifierlist-lookup key fields))
                ((unless type)
                 (fgl::fgl-progn
                  (fgl::fgl-error :msg "Bad key in val-imap-lookup-rec-of-ty-fix-val")
                  (fgl::abort-rewrite (val-imap-lookup key (v_record->rec (ty-fix-val x ty)))))))
             ;; note: this hide is here so that if x is a variable with a ty-satisfied hyp,
             ;; its replacement with its ty-fix-val won't loop with this rule.
             (ty-fix-val (val-imap-lookup key (v_record->rec (fgl::fgl-hide x))) type)))
    :hints (("goal" :expand ((ty-fix-val x ty)))))

  (fgl::def-fgl-rewrite val-imap-has-key-rec-of-ty-fix-val
    (equal (val-imap-has-key key (v_record->rec (ty-fix-val x ty)))
           (b* ((desc (ty->desc ty))
                ((mv recp fields ?type)
                 (type_desc-case desc
                   :t_record (mv t desc.fields nil)
                   :t_exception (mv t desc.fields nil)
                   :t_collection (mv t desc.fields nil)
                   :t_array (mv nil nil nil)
                   :otherwise (mv nil nil nil)))
                ((unless fields)
                 (fgl::fgl-progn
                  (fgl::fgl-error :msg "Bad type in val-imap-has-key-rec-of-ty-fix-val")
                  (fgl::abort-rewrite (val-imap-has-key key (v_record->rec (ty-fix-val x ty))))))
                ((unless recp)
                 (and (member-equal (identifier-fix key) fields) t))
                (type (typed_identifierlist-lookup key fields)))
             (and type t)))
    :hints (("goal" :expand ((ty-fix-val x ty)))))

  (fgl::def-fgl-rewrite val-imap-keys-rec-of-ty-fix-val
    (equal (val-imap-keys (v_record->rec (ty-fix-val x ty)))
           (b* ((desc (ty->desc ty))
                ((mv recp fields ?type)
                 (type_desc-case desc
                   :t_record (mv t desc.fields nil)
                   :t_exception (mv t desc.fields nil)
                   :t_collection (mv t desc.fields nil)
                   :t_array (mv nil nil nil)
                   :otherwise (mv nil nil nil)))
                ((unless fields)
                 (fgl::fgl-progn
                  (fgl::fgl-error :msg "Bad type in val-imap-keys-rec-of-ty-fix-val")
                  (fgl::abort-rewrite (val-imap-keys (v_record->rec (ty-fix-val x ty))))))
                ((unless recp)
                 (mergesort fields)))
             (mergesort (typed_identifierlist->names fields))))
    :hints (("goal" :expand ((ty-fix-val x ty)))))


  ;; Conditional objects: if-of-ty-fix-vals, if-of-other-with-ty-fix-vals

  (define if-of-ty-fix-vals (test then else)
    (if test then else)
    ///
    (fgl::remove-fgl-rewrite if-of-ty-fix-vals))
  (local (in-theory (enable if-of-ty-fix-vals)))

  (fgl::Def-fgl-branch-merge merge-ty-fix-val
    (equal (if test (ty-fix-val v1 ty1) (ty-fix-val v2 ty2))
           ;; (if (and (equal ty1 ty2)
           ;;          (not (type_desc-case (ty->desc ty1) '(:t_record :t_exception :t_collection :t_array :t_tuple))))
           ;;     (ty-fix-val (if test v1 v2) ty1)
           (if-of-ty-fix-vals test
                              (fgl::fgl-hide (ty-fix-val v1 ty1))
                              (fgl::fgl-hide (ty-fix-val v2 ty2))))
    :hints(("Goal" :in-theory (enable fgl::if!
                                      if-of-ty-fix-vals))))

  (fgl::Def-fgl-branch-merge merge-ty-fix-val-with-if-of-ty-fix-vals
    (equal (if test (ty-fix-val v1 ty1) (if-of-ty-fix-vals test2 v2 v3))
           (if-of-ty-fix-vals test (fgl::fgl-hide  (ty-fix-val v1 ty1))
                              (fgl::fgl-hide (if-of-ty-fix-vals test2 v2 v3))))
    :hints(("Goal" :in-theory (enable fgl::if!))))

  (fgl::Def-fgl-branch-merge merge-if-of-ty-fix-vals
    (equal (if test (if-of-ty-fix-vals test1 then1 else1)
             (if-of-ty-fix-vals test2 then2 else2))
           (if-of-ty-fix-vals
            test
            (fgl::fgl-hide (if-of-ty-fix-vals test1 then1 else1))
            (fgl::fgl-hide (if-of-ty-fix-vals test2 then2 else2))))
    :hints(("Goal" :in-theory (enable fgl::if!))))

  (defmacro enable-split-if-of-ty-fix-vals (fn &key add-args)
    `(fgl::def-fgl-rewrite ,(intern-in-package-of-symbol
                             (concatenate 'string (symbol-name fn)
                                          "-OF-IF-OF-TY-FIX-VALS")
                             fn)
       (equal (,fn ,@add-args (if-of-ty-fix-vals test then else))
              (if test (,fn ,@add-args then) (,fn ,@add-args else)))))

  (enable-split-if-of-ty-fix-vals val-kind)
  (enable-split-if-of-ty-fix-vals val-p)
  (enable-split-if-of-ty-fix-vals v_int->val)
  (enable-split-if-of-ty-fix-vals v_bitvector->val)
  (enable-split-if-of-ty-fix-vals v_bitvector->len)
  (enable-split-if-of-ty-fix-vals v_real->val)
  (enable-split-if-of-ty-fix-vals v_string->val)
  (enable-split-if-of-ty-fix-vals v_bool->val)
  (enable-split-if-of-ty-fix-vals v_label->val)
  (enable-split-if-of-ty-fix-vals v_record->rec)
  (fgl::remove-fgl-rewrite v_record->rec-of-if-of-ty-fix-vals)
  (fgl::def-fgl-rewrite val-imap-lookup-of-if-of-ty-fix-vals
    (equal (val-imap-lookup k (v_record->rec (if-of-ty-fix-vals
                                              test then else)))
           (if test
               (val-imap-lookup k (v_record->rec then))
             (val-imap-lookup k (v_record->rec else))))
    :hints(("Goal" :in-theory (enable if-of-ty-fix-vals))))
  (fgl::def-fgl-rewrite val-imap-has-key-of-if-of-ty-fix-vals
    (equal (val-imap-has-key k (v_record->rec (if-of-ty-fix-vals
                                               test then else)))
           (if test
               (val-imap-has-key k (v_record->rec then))
             (val-imap-has-key k (v_record->rec else))))
    :hints(("Goal" :in-theory (enable if-of-ty-fix-vals))))
  (fgl::def-fgl-rewrite val-imap-keys-of-if-of-ty-fix-vals
    (equal (val-imap-keys (v_record->rec (if-of-ty-fix-vals
                                          test then else)))
           (if test
               (val-imap-keys (v_record->rec then))
             (val-imap-keys (v_record->rec else))))
    :hints(("Goal" :in-theory (enable if-of-ty-fix-vals))))
  (enable-split-if-of-ty-fix-vals v_array-len)
  (enable-split-if-of-ty-fix-vals v_array-nth :add-args (n))
  (enable-split-if-of-ty-fix-vals val-fix$inline)

  (fgl::def-fgl-rewrite val-fix-of-ty-fix-val
    (equal (val-fix (ty-fix-val v ty))
           (ty-fix-val v ty)))

  (fgl::add-fgl-rewrite return-type-of-ty-fix-val.new-x)

  (define if-of-other-with-ty-fix-vals (test then else)
    (if test then else)
    ///
    (fgl::remove-fgl-rewrite if-of-other-with-ty-fix-vals))

  (local (in-theory (enable if-of-other-with-ty-fix-vals)))

  (defmacro enable-split-if-of-other-with-ty-fix-vals (fn &key add-args)
    `(fgl::def-fgl-rewrite ,(intern-in-package-of-symbol
                             (concatenate 'string (symbol-name fn)
                                          "-OF-IF-OF-OTHER-WITH-TY-FIX-VALS")
                             fn)
       (equal (,fn ,@add-args (if-of-other-with-ty-fix-vals test then else))
              (if test (,fn ,@add-args then) (,fn ,@add-args else)))))

  (enable-split-if-of-other-with-ty-fix-vals val-kind)
  (enable-split-if-of-other-with-ty-fix-vals val-p)
  (enable-split-if-of-other-with-ty-fix-vals v_int->val)
  (enable-split-if-of-other-with-ty-fix-vals v_bitvector->val)
  (enable-split-if-of-other-with-ty-fix-vals v_bitvector->len)
  (enable-split-if-of-other-with-ty-fix-vals v_real->val)
  (enable-split-if-of-other-with-ty-fix-vals v_string->val)
  (enable-split-if-of-other-with-ty-fix-vals v_bool->val)
  (enable-split-if-of-other-with-ty-fix-vals v_label->val)
  (enable-split-if-of-other-with-ty-fix-vals v_record->rec)
  (enable-split-if-of-other-with-ty-fix-vals v_array-update-nths
                                             :add-args (lst))
  (fgl::remove-fgl-rewrite v_record->rec-of-if-of-other-with-ty-fix-vals)
  (fgl::def-fgl-rewrite val-imap-lookup-of-if-of-other-with-ty-fix-vals
    (equal (val-imap-lookup k (v_record->rec (if-of-other-with-ty-fix-vals
                                              test then else)))
           (if test
               (val-imap-lookup k (v_record->rec then))
             (val-imap-lookup k (v_record->rec else))))
    :hints(("Goal" :in-theory (enable if-of-other-with-ty-fix-vals))))
  (fgl::def-fgl-rewrite val-imap-has-key-of-if-of-other-with-ty-fix-vals
    (equal (val-imap-has-key k (v_record->rec (if-of-other-with-ty-fix-vals
                                               test then else)))
           (if test
               (val-imap-has-key k (v_record->rec then))
             (val-imap-has-key k (v_record->rec else))))
    :hints(("Goal" :in-theory (enable if-of-other-with-ty-fix-vals))))
  (fgl::def-fgl-rewrite val-imap-keys-of-if-of-other-with-ty-fix-vals
    (equal (val-imap-keys (v_record->rec (if-of-other-with-ty-fix-vals
                                          test then else)))
           (if test
               (val-imap-keys (v_record->rec then))
             (val-imap-keys (v_record->rec else))))
    :hints(("Goal" :in-theory (enable if-of-other-with-ty-fix-vals))))
  (enable-split-if-of-other-with-ty-fix-vals v_array-len)
  (enable-split-if-of-other-with-ty-fix-vals v_array-nth :add-args (n))
  (enable-split-if-of-other-with-ty-fix-vals val-fix$inline)



  (fgl::def-fgl-branch-merge if-of-ty-fix-val-with-other
    (implies (syntaxp (fgl::fgl-object-case y
                        :g-apply (and (not (eq y.fn 'ty-fix-val))
                                      (not (eq y.fn 'if-of-ty-fix-vals))
                                      (not (eq y.fn 'if-of-other-with-ty-fix-vals)))
                        :otherwise t))
             (equal (if test (ty-fix-val v ty) y)
                    (if-of-other-with-ty-fix-vals (not test) y (ty-fix-val v ty))))
    :hints (("Goal" :in-theory (enable fgl::if!))))

  (fgl::def-fgl-branch-merge if-of-if-of-ty-fix-val-with-other
    (implies (syntaxp (fgl::fgl-object-case
                        y
                        :g-apply (and (not (eq y.fn 'ty-fix-val))
                                      (not (eq y.fn 'if-of-ty-fix-vals))
                                      (not (eq y.fn 'if-of-other-with-ty-fix-vals)))
                        :otherwise t))
             (equal (if test (if-of-ty-fix-vals test2 then else) y)
                    (if-of-other-with-ty-fix-vals
                     (not test)
                     y
                     (fgl::conditionalize
                      free-var test (if-of-ty-fix-vals test2 then else))))))

  (fgl::def-fgl-branch-merge if-of-if-of-other-and-ty-fix-val-with-other
    (implies (syntaxp (fgl::fgl-object-case
                        y
                        :g-apply (and (not (eq y.fn 'ty-fix-val))
                                      (not (eq y.fn 'if-of-ty-fix-vals))
                                      (not (eq y.fn 'if-of-other-with-ty-fix-vals)))
                        :otherwise t))
             (equal (if test (if-of-other-with-ty-fix-vals test1 then else) y)
                    (if-of-other-with-ty-fix-vals
                     (or (not test) test1)
                     (fgl::conditionalize free-var
                                          (or (not test) test1)
                                          (if test then y))
                     else))))

  (fgl::def-fgl-branch-merge if-of-if-of-other-with-ty-fix-vals-and-ty-fix-val
    (equal (if test (if-of-other-with-ty-fix-vals test1 then1 else1)
             (ty-fix-val v ty))
           (if-of-other-with-ty-fix-vals
            (and test test1) then1 (if test else1 (ty-fix-val v ty)))))

  (fgl::def-fgl-branch-merge if-of-if-of-other-with-ty-fix-vals-and-if-of-ty-fix-vals
    (equal (if test (if-of-other-with-ty-fix-vals test1 then1 else1)
             (if-of-ty-fix-vals test2 then2 else2))
           (if-of-other-with-ty-fix-vals
            (and test test1) then1 (if test else1 (if-of-ty-fix-vals test2 then2 else2)))))

  ;; (fgl::def-fgl-branch-merge if-of-v_record->rec-of-ty-fix-val
  ;;   (implies (syntaxp (fgl::fgl-object-case y
  ;;                       :g-apply (not (eq y.fn 'v_record->rec$inline))))
  ;;            (equal (if test (v_record->rec (ty-fix-val x ty)) y)
  ;;                   (v_record->rec
  ;;                    (if-of-other-with-ty-fix-vals (not test) y (ty-fix-val x ty))))))


  (defun ty-fix-val-expand (x ty)
    (ty-fix-val x ty))
  (fgl::remove-fgl-rewrite ty-fix-val-expand)

  ;; Note: assumes ty-fix-val-of-atomic, i.e. there are no ty-fix-vals of atomic types
  (fgl::def-fgl-rewrite ty-fix-val-do-expand
    (equal (ty-fix-val-expand x ty)
           (b* ((desc (ty->desc ty)))
             (type_desc-case desc
               (:t_record (v_record (record-type-fix-val (v_record->rec x) desc.fields)))
               (:t_exception (v_record (record-type-fix-val (v_record->rec x) desc.fields)))
               (:t_collection (v_record (record-type-fix-val (v_record->rec x) desc.fields)))
               (:t_tuple (v_array (tuple-type-fix-val (v_array->arr x) desc.types)))
               (:t_array
                (v_array (array-type-fix-val (nfix (int-literal-expr->val desc.index))
                                             (v_array->arr x)
                                             desc.type)))
               (:otherwise (fgl::abort-rewrite (ty-fix-val-expand x ty))))))
    :hints (("goal" :expand ((ty-fix-val x ty)))))


  (fgl::def-fgl-rewrite equal-of-ty-fix-val
    (implies (val-p y)
             (equal (equal (ty-fix-val x ty) y)
                    (equal (ty-fix-val-expand x ty) y))))



  (fgl::def-fgl-rewrite ty-fix-val-of-if-of-other-with-ty-fix-vals
    (equal (ty-fix-val (if-of-other-with-ty-fix-vals test then else) ty)
           (if test (ty-fix-val then ty) (ty-fix-val else ty)))
    :hints(("Goal" :in-theory (enable if-of-other-with-ty-fix-vals))))

  (fgl::def-fgl-rewrite ty-fix-val-of-if-of-ty-fix-vals
    (equal (ty-fix-val (if-of-ty-fix-vals test then else) ty)
           (if test (ty-fix-val then ty) (ty-fix-val else ty)))
    :hints(("Goal" :in-theory (enable if-of-ty-fix-vals))))

  (fgl::def-fgl-rewrite equal-of-if-of-ty-fix-vals
    (equal (equal (if-of-ty-fix-vals test then else) x)
           (if test (equal then x) (equal else x)))
    :hints(("Goal" :in-theory (enable if-of-ty-fix-vals))))

  (fgl::def-fgl-rewrite equal-of-if-of-other-with-ty-fix-vals
    (equal (equal (if-of-other-with-ty-fix-vals test then else) x)
           (if test (equal then x) (equal else x)))
    :hints(("Goal" :in-theory (enable if-of-other-with-ty-fix-vals))))

  (fgl::def-fgl-rewrite ty-fix-val-of-ty-fix-val
    (implies (ty-satisfiable ty)
             (equal (ty-fix-val (ty-fix-val x ty) ty)
                    (fgl::fgl-hide (ty-fix-val x ty)))))

  (define ty-fix-array-writes (lst ty)
    :verify-guards nil
    (if (atom lst)
        nil
      (if (atom (car lst))
          (ty-fix-array-writes (cdr lst) ty)
        (cons (cons (caar lst) (ty-fix-val (cdar lst) ty))
              (ty-fix-array-writes (cdr lst) ty)))))


  (local
   (encapsulate nil
     (local (defun ind2 (n len x)
              (if (zp len)
                  (list n x)
                (ind2 (1- n) (1- len) (cdr x)))))
     (defthm array-type-fix-val-of-update-nth
       (implies (< (nfix n) (nfix len))
                (equal (array-type-fix-val len (update-nth n v x) ty)
                       (update-nth n (ty-fix-val v ty)
                                   (array-type-fix-val len x ty))))
       :hints (("goal" :induct (ind2 n len x)
                :expand ((:free (x) (array-type-fix-val len x ty))
                         (:free (v x) (update-nth n v x))))))))


  (local
   (defthm ty-fix-val-of-v_array-update-nth
     (implies (and (equal desc (ty->desc ty))
                   (type_desc-case desc :t_array)
                   (equal idx (t_array->index desc))
                   (equal lenx idx)
                   (equal len (int-literal-expr->val lenx)) ;;(l_int->val (e_literal->val (expr->desc lenx))))
                   (< (nfix n) len))
              (equal (ty-fix-val (v_array-update-nth n v x) ty)
                     (v_array-update-nth n (ty-fix-val v (t_array->type desc))
                                         (ty-fix-val x ty))))
     :hints(("Goal" :in-theory (e/d (ty-fix-val v_array-update-nth))
             :expand ((:free (arr) (ty-fix-val (v_array arr) ty))
                      (ty-fix-val x ty))))))

  (local
   (defthm v_array-len-of-ty-fix-val-acl2
     (implies (and (equal desc (ty->desc ty))
                   (type_desc-case desc :t_array)
                   (equal idx (t_array->index desc)))
              (equal (v_array-len (ty-fix-val x ty))
                     (nfix (int-literal-expr->val idx))))
     :hints (("goal" :expand ((ty-fix-val x ty))
              :in-theory (e/d (v_array-len))))))



  (fgl::def-fgl-rewrite ty-fix-val-of-v_array-update-nths
    (implies (and (equal desc (ty->desc ty))
                  (type_desc-case desc :t_array)
                  (equal idx (t_array->index desc))
                  (ty-satisfied x ty))
             (equal (ty-fix-val (v_array-update-nths lst x) ty)
                    (v_array-update-nths (ty-fix-array-writes lst (t_array->type desc))
                                         x)))
    :hints(("Goal" :in-theory (e/d (v_array-update-nths
                                    ty-fix-array-writes
                                    v_array-len))
            :expand ((ty-satisfied x ty))))))

(fgl::def-ctrex-rule ty-fix-val-default
  :match ((fix (ty-fix-val val ty)))
  :assigned-var val
  :assign (ty-satisfying-val ty)
  :ruletype nil)


(defsection expand-val
  ;; Note: This was motivated by a case where an equality of a record's imap
  ;; failed to resolve because one side rewrote to the explicit normal form
  ;; (val-imap-add-pairs pairs nil) and the other side rewrite to a lazier
  ;; normal form involving ty-fix-val, if-of-ty-fix-vals, val-imap-put-pairs,
  ;; etc. We resolve this by adding a rewrite on equality with
  ;; val-imap-add-pairs and some other kind of object, which tries to force the
  ;; other object to expand to the more explicit val-imap-add-pairs form.

  ;; We may need more base cases, e.g. expand-val of something other than a
  ;; ty-fix-val, if-of-ty-fix-vals, if-of-other-with-ty-fix-vals should disappear.

  ;; We may also be able to use this in other contexts that come up as "lazy"
  ;; forms get compared with explicit forms.

  (define expand-val (x) x
    :enabled t
    ///
    (fgl::remove-fgl-rewrite expand-val))

  (define expand-imap (x) x
    :enabled t
    ///
    (fgl::remove-fgl-rewrite expand-imap))

  (fgl::def-fgl-rewrite expand-imap-of-val-imap-put-pairs
    (equal (expand-imap (val-imap-put-pairs pairs base))
           (val-imap-put-pairs pairs (expand-imap base))))

  (fgl::def-fgl-rewrite expand-imap-of-val-imap-add-pairs
    (equal (expand-imap (val-imap-add-pairs pairs nil))
           (val-imap-add-pairs pairs nil)))

  (fgl::def-fgl-rewrite expand-imap-of-var
    (implies (syntaxp (fgl::fgl-object-case x :g-var))
             (equal (expand-imap x) x)))

  (fgl::def-fgl-rewrite expand-imap-of-nil
    (equal (expand-imap nil) nil))

  (fgl::def-fgl-rewrite expand-imap-of-storage
    (equal (expand-imap (global-env->storage env))
           (global-env->storage env)))

  (fgl::def-fgl-rewrite expand-imap-of-v_record->rec
    (equal (expand-imap (v_record->rec x))
           (v_record->rec (expand-val x))))

  (fgl::def-fgl-rewrite expand-val-of-v_record
    (equal (expand-val (v_record x))
           (v_record (expand-imap x))))

  (fgl::def-fgl-rewrite expand-val-of-ty-fix-val
    (equal (expand-val (ty-fix-val x ty))
           (ty-fix-val-expand x ty)))

  (fgl::def-fgl-rewrite expand-val-of-if-of-other-with-ty-fix-val
    (equal (expand-val (if-of-other-with-ty-fix-vals test then else))
           (if test (expand-val then) (expand-val else)))
    :hints(("Goal" :in-theory (enable if-of-other-with-ty-fix-vals))))

  (fgl::def-fgl-rewrite expand-val-of-if-of-ty-fix-vals
    (equal (expand-val (if-of-ty-fix-vals test then else))
           (if test (expand-val then) (expand-val else)))
    :hints(("Goal" :in-theory (enable if-of-ty-fix-vals))))


  (fgl::def-fgl-rewrite equal-of-val-imap-add-pairs-with-other
    (implies (syntaxp (fgl::fgl-object-case x
                        :g-apply (not (equal x.fn 'val-imap-add-pairs))
                        :otherwise t))
             (equal (equal (val-imap-add-pairs pairs nil) x)
                    (b* ((new-x (expand-imap x))
                         (changedp (fgl::syntax-bind
                                    changedp
                                    (and (not (equal new-x x))
                                         (not (equal new-x (fgl::g-apply 'expand-imap (list x))))))))
                      (if changedp
                          (equal (val-imap-add-pairs pairs nil) new-x)
                        (fgl::abort-rewrite
                         (equal (val-imap-add-pairs pairs nil) x)))))))

  (fgl::def-fgl-branch-merge if-of-val-imap-add-pairs-with-other
    (implies (syntaxp (fgl::fgl-object-case x
                                            :g-apply (not (equal x.fn 'val-imap-add-pairs))
                                            :otherwise t))
             (equal (if test (val-imap-add-pairs pairs nil) x)
                    (b* ((new-x (expand-imap x))
                         (changedp (fgl::syntax-bind
                                    changedp
                                    (and (not (equal new-x x))
                                         (not (equal new-x (fgl::g-apply 'expand-imap (list x))))))))
                      (if changedp
                          (if test
                              (fgl::fgl-hide (val-imap-add-pairs pairs nil))
                              new-x)
                          (fgl::abort-rewrite
                           (if test (val-imap-add-pairs pairs nil) x)))))))
  )


(fgl::def-fgl-rewrite simplify-equal-of-ty-fix-val
  (implies (and (val-p x)
                (ty-satisfied x ty))
           (equal (Equal (ty-fix-val x ty) x) t)))



(define val-imap-fixup-storage-types ((x val-imap-p)
                                      (types ty-global_decl_keyword-imap-p))
  :verify-guards nil
  :hooks nil
  (b* (((when (atom x)) nil)
       ((unless (mbt (and (consp (car x))
                          (identifier-p (caar x)))))
        (val-imap-fixup-storage-types (cdr x) types))
       ((cons id val) (car x))
       (lookup (hons-assoc-equal id types))
       ((unless lookup) (omap::update id (val-fix val)
                                      (val-imap-fixup-storage-types (cdr x) types)))
       (ty (ty-global_decl_keyword->ty (cdr lookup))))
    (omap::update id (ty-fix-val val ty)
                  (val-imap-fixup-storage-types (cdr x) types))))

(fgl::def-ctrex-rule val-imap-satisfies-types-ctrex-rule
  :match ((pred (val-imap-satisfies-types storage types)))
  :assign-cond pred
  :assigned-var storage
  :assign (val-imap-fixup-storage-types storage types)
  :ruletype :fixup)



(fgl::def-ctrex-rule ty-satisfied-ctrex-rule
  :match ((pred (ty-satisfied x ty)))
  :assign-cond pred
  :assigned-var x
  :assign (ty-fix-val x ty)
  :ruletype :fixup)


(table fgl::magitastic-ev-definitions
       'val-imap-satisfies-types
       (list '(x types)
             ''t))



(fgl::def-fgl-rewrite equal-of-two-vars-when-typed-asl-val
  (implies (and (syntaxp (and (fgl::fgl-object-case x :g-var)
                              (fgl::fgl-object-case y :g-var)
                              (not (equal x y))))
                (val-p x)
                (bind-ty-satisfied ty x))
           (equal (equal x y)
                  (equal (ty-fix-val x ty) y)))
  :hints(("Goal" :in-theory (enable bind-ty-satisfied))))

(fgl::def-fgl-brewrite bind-ty-satisfied-by-match
  (implies (and (fgl::match-assums (ty-satisfied x ty1))
                (equal ty ty1))
           (equal (bind-ty-satisfied ty x)
                  ty))
  :hints (("Goal" :in-theory (enable bind-ty-satisfied))))

(define non-mergeable-if (test then else msg)
  :enabled t
  (declare (ignore msg))
  (if test then else)
  ///
  (fgl::remove-fgl-rewrite non-mergeable-if)
  (fgl::def-fgl-rewrite non-mergeable-if-fgl
    (equal (non-mergeable-if test then else msg)
           (b* ((config (fgl::make-fgl-ipasir-config))
                (?ign (fgl::fgl-sat-check config t))
                (which-branch
                 (if test
                     (b* ((?ign (fgl::fgl-sat-check config t))) t)
                     (b* ((?ign (fgl::fgl-sat-check config t))) nil)))
                (which-branch-concrete
                 (fgl::syntax-bind which-branch-concrete
                                   (fgl::fgl-object-case which-branch
                                                         :g-concrete)))
                ((unless which-branch-concrete)
                 (fgl::fgl-progn
                  (fgl::fgl-error :msg (msg "Merging non-mergeable if: ~@0"
                                            (fgl::g-concrete->val msg)))
                  (non-mergeable-if test then else msg))))
             (if which-branch then else)))))

(defmacro def-merge-with-bad (call)
  (b* ((ctor (car call))
       (kind (intern-in-package-of-symbol (symbol-name ctor) :kwd-pkg)))
    (acl2::template-subst
     '(fgl::def-fgl-branch-merge merge-<kind>-with-bad
       (implies (and (equal kind (val-kind other))
                 (syntaxp (fgl::fgl-object-case kind :g-concrete))
                 (not (equal kind <kind>)))
        (equal (if test <call> other)
         (non-mergeable-if test <call> other
                           (msg "~x0 merge with ~x1" <kind> kind)))))
     :atom-alist `((<call> . ,call)
                   (<kind> . ,kind))
     :str-alist `(("<KIND>" . ,(symbol-name kind)))
     :pkg-sym 'asl-pkg)))

(def-merge-with-bad (v_int val))
(def-merge-with-bad (v_bitvector len val))
(def-merge-with-bad (v_bool val))
(def-merge-with-bad (v_real val))
(def-merge-with-bad (v_string val))
(def-merge-with-bad (v_label val))
(def-merge-with-bad (v_array arr))
(def-merge-with-bad (v_record rec))

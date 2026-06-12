;;****************************************************************************;;
;;                              ACL2 ASL Library                              ;;
;;****************************************************************************;;
;;
;; SPDX-FileCopyrightText: Copyright 2026 Arm Limited and/or its affiliates <open-source-office@arm.com>
;; SPDX-License-Identifier: BSD-3-Clause
;; 
;; ****************************************************************************;;






(in-package "ASL")
(include-book "centaur/fgl/def-fgl-rewrite" :dir :System)
(include-book "centaur/fgl/constraint-db" :dir :System)
(include-book "centaur/fgl/member-equal" :dir :System)
(include-book "defs")
(include-book "trace-interp" :dir :acl2asl)
(local (include-book "std/lists/sets" :Dir :System))

;; ----------------------------------------------------------------------------------
;; Resolving enum values
;; When we access an enum-valued object, we want it to be obvious that it's one
;; of the possible enum values. We encode these as (e.g., for an enum with
;; possible values "a", "b", "c")
;;   (choose-value (list (cons a-cond "a") (cons b-cond "b") c))
;; This, by expanding the definition of choose-value, is just:
;;   (if a-cond "a" (if b-cond "b" "c"))

;; Need this function for merging v_label values

;; Takes an object like ((cond1 . val1) .. (condN-1 . valN-1) valN)
;; and returns (if cond1 val1 (if ... (if condN-1 valN-1 valN)...))
(define choose-value (val-conds)
  :verify-guards nil
  (if (atom (cdr val-conds))
      (car val-conds)
    (if (caar val-conds)
        (cdar val-conds)
      (choose-value (cdr val-conds))))
  ///
  (fgl::remove-fgl-rewrite choose-value)

  (fgl::def-fgl-rewrite equal-of-choose-value
    (equal (equal (choose-value val-conds) x)
           (if (atom (cdr val-conds))
               (equal (car val-conds) x)
             (if (caar val-conds)
                 (equal (cdar val-conds) x)
               (equal (choose-value (cdr val-conds)) x)))))

  (fgl::def-fgl-rewrite stringp-of-choose-value
    (equal (stringp (choose-value val-conds))
           (if (atom (cdr val-conds))
               (stringp (car val-conds))
             (if (caar val-conds)
                 (stringp (cdar val-conds))
               (stringp (choose-value (cdr val-conds)))))))

  (fgl::def-fgl-rewrite memberp-equal-of-choose-value
    (equal (fgl::memberp-equal (choose-value val-conds) lst)
           (if (atom (cdr val-conds))
               (fgl::memberp-equal (car val-conds) lst)
             (if (caar val-conds)
                 (fgl::memberp-equal (cdar val-conds) lst)
               (fgl::memberp-equal (choose-value (cdr val-conds)) lst))))
    :hints(("Goal" :in-theory (enable choose-value)))))


;; This checks whether val is a possible value of val-conds (wrt choose-value).
;; This is useful for merging IFs etc, since it's better not to confuse things
;; by reordering the val-conds if val isn't one of the possibilities anyway.
(define member-val-conds (val val-conds)
  :verify-guards nil
  (if (atom (cdr val-conds))
      (and (consp val-conds)
           (equal val (car val-conds)))
    (or (equal val (cdar val-conds))
        (member-val-conds val (cdr val-conds)))))



;; Try to compute an ordering containing all elements of both x and y (assumed to both be duplicate-free)
;; such that the orders of x and y are preserved if possible.
(define val-cond-merge-ordering-aux ((x true-listp) (y true-listp))
  :measure (+ (len x) (len y))
  :returns (mv (conflicts natp :rule-classes :type-prescription)
               (order true-listp :rule-classes :type-prescription))
  :verify-guards nil
  :prepwork ((local (defthm len-of-remove-equal-weak
                      (<= (len (Remove-equal x y)) (len y))
                      :rule-classes :linear)))
  (cond ((atom x) (mv 0 (acl2::llist-fix y)))
        ((atom y) (mv 0 (acl2::llist-fix x)))
        ((equal (car x) (car y))
         (b* (((mv confs rest) (val-cond-merge-ordering-aux (cdr x) (cdr y))))
           (mv confs (cons (car x) rest))))
        ((not (member-equal (car x) y))
         (b* (((mv confs rest) (val-cond-merge-ordering-aux (cdr x) y)))
           (mv confs (cons (car x) rest))))
        ((not (member-equal (car y) x))
         (B* (((mv confs rest) (val-cond-merge-ordering-aux x (cdr y))))
           (mv confs (cons (car y) rest))))
        ;; the harder case: (car x) is in y and unequal to (car y) which is in x. Which do we put first?
        ;; Try both (memoized) and pick the better one.
        (t (b* (((mv confs1 rest1) (val-cond-merge-ordering-aux (cdr x) (remove-equal (car x) y)))
                ((mv confs2 rest2) (val-cond-merge-ordering-aux (remove-equal (car y) x) (cdr y))))
             (cond ((< confs1 confs2)
                    (mv (+ 1 confs1) (cons (car x) rest1)))
                   (t (mv (+ 1 confs2) (cons (car y) rest2)))))))
  ///
  (verify-guards val-cond-merge-ordering-aux)
  (memoize 'val-cond-merge-ordering-aux)

  (defret member-of-<fn>
    (iff (member-equal k order)
         (or (member-equal k x)
             (member-equal k y))))

  (defret <fn>-under-set-equiv
    (acl2::set-equiv order (append x y))
    :hints(("Goal" :in-theory (enable acl2::set-unequal-witness-rw)))))


(define val-cond-merge-ordering ((x true-listp) (y true-listp))
  :returns (order)
  (b* (((mv ?conflicts order) (val-cond-merge-ordering-aux x y)))
    (clear-memoize-table 'val-cond-merge-ordering-aux)
    order)
  ///
  (defret <fn>-under-set-equiv
    (acl2::set-equiv order (append x y)))

  (defcong acl2::set-equiv acl2::set-equiv (val-cond-merge-ordering x y) 1)
  (defcong acl2::set-equiv acl2::set-equiv (val-cond-merge-ordering x y) 2))

(define val-cond-vals (val-conds)
  :verify-guards nil
  (if (atom (cdr val-conds))
      (list (car val-conds))
    (cons (cdar val-conds)
          (val-cond-vals (cdr val-conds))))
  ///
  (defthm choose-value-is-member-val-cond-vals
    (member-equal (choose-value val-conds) (val-cond-vals val-conds))
    :hints(("Goal" :in-theory (enable choose-value)))))

(define val-conds-factor-out-val-with-dups (val x)
  :verify-guards nil
  :returns (mv (val-cond) (rest) (empty))
  (if (atom (cdr x))
      (if (equal val (car x))
          (mv t nil t)
        (mv nil (list (car x)) nil))
    (b* (((mv cond rest empty) (val-conds-factor-out-val-with-dups val (cdr x))))
      (if (equal val (cdar x))
          (mv (or (caar x) cond) rest empty)
        (mv (and (not (caar x)) cond)
            (if empty (list (cdar x)) (cons (car x) rest))
            nil))))
  ///
  (defret consp-of-<fn>
    (iff (consp rest)
         (not empty)))
  
  (defun factors-to-val-conds (val val-cond rest empty)
    (if empty
        (list val)
      (cons (cons val-cond val) rest)))
  
  (local (defret <fn>-val-cond-when-empty
           (implies empty val-cond)))

  (defretd <fn>-correct-gen
    (equal (choose-value (factors-to-val-conds val val-cond rest empty))
           (choose-value x))
    :hints(("Goal" :in-theory (enable choose-value))))

  (defret <fn>-under-iff
    (iff val-cond
         (equal (choose-value x) val))
    :hints(("Goal" :in-theory (enable choose-value)
            :induct t)))
  
  (defretd <fn>-implies-equal-to-val
    (implies empty
             (equal (equal (choose-value x) val) t))
    :hints(("Goal" :in-theory (enable choose-value))))

  (defretd <fn>-choose-value-preserved
    (implies (and (not empty)
                  (not val-cond))
             (equal (choose-value rest)
                    (choose-value x)))
    :hints(("Goal" :in-theory (enable choose-value
                                      <fn>-implies-equal-to-val)
            :induct t)
           (and stable-under-simplificationp
                '(:use ((:instance <fn>-implies-equal-to-val
                         (x (cdr x))))
                  :in-theory (disable <fn>-implies-equal-to-val)))))
  
  (defretd val-cond-vals-when-<fn>-empty
    (implies empty
             (acl2::set-equiv (val-cond-vals x) (list val)))
    :hints(("Goal" :in-theory (enable val-cond-vals))))
  
  (defret val-cond-vals-of-<fn>
    (implies (not empty)
             (acl2::Set-equiv (val-cond-vals rest)
                              (remove-equal val (val-cond-vals x))))
    :hints(("Goal" :in-theory (enable val-cond-vals
                                      val-cond-vals-when-<fn>-empty))))

  (defret len-of-<fn>-weak
    (implies (consp x)
             (<= (len rest) (len x)))
    :rule-classes :linear)

  (defret len-of-<fn>-strong
    (implies (and (member-equal val (val-cond-vals x))
                  (consp x))
             (< (len rest) (len x)))
    :hints(("Goal" :in-theory (enable val-cond-vals)))
    :rule-classes :linear))


(define val-conds-deduplicate (x)
  :verify-guards nil
  :returns (new-x)
  :measure (len x)
  :hints(("Goal" :in-theory (enable val-cond-vals)))
  (b* (((When (atom (cdr x))) (list (car x)))
       ((mv cond rest empty)
        (val-conds-factor-out-val-with-dups (cdar x) x))
       ((when empty)
        (list (cdar x)))
       (rest (val-conds-deduplicate rest)))
    (cons (cons cond (cdar x)) rest))
  ///
  (defret val-cond-vals-of-<fn>
    (acl2::set-equiv (val-cond-vals new-x)
                     (val-cond-vals x))
    :hints(("Goal" :in-theory (enable val-cond-vals
                                      val-cond-vals-when-val-conds-factor-out-val-with-dups-empty))))

  (defret no-duplicate-vals-of-<fn>
    (no-duplicatesp-equal (val-cond-vals new-x))
    :hints(("Goal" :in-theory (enable val-cond-vals))))

  (defret choose-value-of-<fn>
    (equal (choose-value new-x) (choose-value x))
    :hints(("Goal" :in-theory (enable choose-value
                                      val-conds-factor-out-val-with-dups-choose-value-preserved)
            :induct t)
           (and stable-under-simplificationp
                '(:use ((:instance val-conds-factor-out-val-with-dups-implies-equal-to-val
                         (val (cdar x)))))))))
    


(define val-conds-factor-out-val (val x)
  :verify-guards nil
  :returns (mv (val-cond) (rest) (empty))
  (if (atom (cdr x))
      (if (equal val (car x))
          (mv t nil t)
        (mv nil (list (car x)) nil))
    (if (equal val (cdar x))
        (mv (caar x) (cdr x) nil)
      (b* (((mv cond rest empty) (val-conds-factor-out-val val (cdr x)))
           (cond (and (not (caar x)) cond)))
        (if empty
            (mv cond (list (cdar x)) nil)
          (mv cond (cons (car x) rest) nil)))))
  ///
  (defret consp-of-<fn>
    (iff (consp rest)
         (not empty)))

  (local (defret not-empty-when-consp-cdr
           (implies (consp (Cdr x))
                    (not empty))))
  
  (defun factors-to-val-conds (val val-cond rest empty)
    (if empty
        (list val)
      (cons (cons val-cond val) rest)))
  
  (local (defret <fn>-val-cond-when-empty
           (implies empty val-cond)))

  (defretd <fn>-correct-gen
    (equal (choose-value (factors-to-val-conds val val-cond rest empty))
           (choose-value x))
    :hints(("Goal" :in-theory (enable choose-value))))

  (defret <fn>-under-iff-when-no-duplicates
    (implies (no-duplicatesp-equal (val-cond-vals x))
             (iff val-cond
                  (equal (choose-value x) val)))
    :hints(("Goal" :in-theory (enable val-cond-vals
                                      choose-value)
            :induct t)
           (and stable-under-simplificationp
                '(:use ((:instance choose-value-is-member-val-cond-vals
                         (val-conds (cdr x))))
                  :in-theory (disable choose-value-is-member-val-cond-vals)))))
  
  (defretd <fn>-implies-equal-to-val
    (implies (or empty val-cond)
             (equal (equal (choose-value x) val) t))
    :hints(("Goal" :in-theory (enable choose-value))))

  (defretd <fn>-choose-value-preserved
    (implies (and (not empty)
                  (not val-cond))
             (equal (choose-value rest)
                    (choose-value x)))
    :hints(("Goal" :in-theory (enable choose-value
                                      <fn>-implies-equal-to-val)
            :induct t)
           (and stable-under-simplificationp
                '(:use ((:instance <fn>-implies-equal-to-val
                         (x (cdr x))))
                  :in-theory (disable <fn>-implies-equal-to-val)))))

  (local (defretd val-cond-vals-when-empty
           (implies empty
                    (equal (val-cond-vals x) (list val)))
           :hints(("Goal" :in-theory (enable val-cond-vals)))))
  
  (defret val-cond-vals-of-<fn>
    (implies (and (not empty)
                  (no-duplicatesp-equal (val-cond-vals x)))
             (equal (val-cond-vals rest)
                    (remove-equal val (val-cond-vals x))))
    :hints(("Goal" :in-theory (enable val-cond-vals
                                      val-cond-vals-when-empty)))))


(local (defthm no-duplicatesp-equal-of-remove
         (implies (no-duplicatesp-equal x)
                  (no-duplicatesp-equal (remove-equal k x)))))

(define val-cond-merge-rec ((keys true-listp) test x y)
  :returns (merge)
  :verify-guards nil
  (b* (((when (atom (cdr keys))) (list (car keys)))
       ((mv xvalcond xrest xempty) (val-conds-factor-out-val (car keys) x))
       ((mv yvalcond yrest yempty) (val-conds-factor-out-val (car keys) y))
       ((when xempty)
        (if yempty
            (list (car keys))
          (cons (cons (or test yvalcond) (car keys)) yrest)))
       ((when yempty)
        (cons (cons (if test xvalcond t) (car keys)) xrest))
       (rest (val-cond-merge-rec (cdr keys) test xrest yrest)))
    (cons (cons (if test xvalcond yvalcond) (car keys)) rest))
  ///
  (defret <fn>-correct
    (let ((val (if test (choose-value x) (choose-value y))))
      (implies (and (member-equal val (double-rewrite keys))
                    (no-duplicatesp-equal (val-cond-vals x))
                    (no-duplicatesp-equal (val-cond-vals y)))
               (equal (choose-value merge) val)))
    :hints(("Goal" :in-theory (enable choose-value
                                      val-conds-factor-out-val-implies-equal-to-val
                                      val-conds-factor-out-val-choose-value-preserved)
            :induct <call>))))

(define val-cond-merge (test x y)
  :returns (merge)
  :verify-guards nil
  (b* ((xkeys (val-cond-vals x))
       (ykeys (val-cond-vals y))
       ((mv xkeys x) (if (no-duplicatesp-equal xkeys)
                         (mv xkeys x)
                       (b* ((x (val-conds-deduplicate x)))
                         (mv (val-cond-vals x) x))))
       ((mv ykeys y) (if (no-duplicatesp-equal ykeys)
                         (mv ykeys y)
                       (b* ((y (val-conds-deduplicate y)))
                         (mv (val-cond-vals y) y))))
       (keys (val-cond-merge-ordering xkeys ykeys)))
    (val-cond-merge-rec keys test x y))
  ///
  (defret <fn>-correct
    (equal (choose-value merge)
           (if test (choose-value x) (choose-value y)))))




;; This function is for the if-of-choose-value-and-val (where val is concrete)
;; branch merge rule. It looks for an entry in val-conds matching val and
;; merges the test with the tests (cars) of the val-conds list such that
;; choose-value of this result equals the if.
(define merge-val-conds-with-val-aux (test val-conds val)
  :verify-guards nil
  (if (atom (cdr val-conds))
      (if (equal val (car val-conds))
          (list val)
        (list (cons test (car val-conds)) val))
    (if (equal val (cdar val-conds))
        (cons (cons (or (caar val-conds) (not test)) val)
              (cdr val-conds))
      (cons (cons (and test (caar val-conds)) (cdar val-conds))
            (merge-val-conds-with-val-aux test (cdr val-conds) val))))
  ///
  (defthm merge-val-conds-with-val-aux-correct
    (equal (choose-value (merge-val-conds-with-val-aux test val-conds val))
           (if test (choose-value val-conds) val))
    :hints(("Goal" :in-theory (enable choose-value)
            :cases (test)))
    :otf-flg t))

(define merge-val-conds-with-val (test val-conds val)
  :verify-guards nil
  (if (member-val-conds val val-conds)
      (merge-val-conds-with-val-aux test val-conds val)
    (cons (cons (not test) val) (if (consp val-conds) val-conds (list nil))))
  ///
  (defthm merge-val-conds-with-val-correct
    (equal (choose-value (merge-val-conds-with-val test val-conds val))
           (if test (choose-value val-conds) val))
    :hints(("Goal" :in-theory (enable choose-value member-val-conds))))
  
  (fgl::def-fgl-branch-merge if-of-choose-value-and-val
    (equal (if test (choose-value val-conds) (fgl::concrete val))
           (choose-value
            (merge-val-conds-with-val test val-conds val)))
    :hints(("Goal" :in-theory (disable merge-val-conds-with-val)))))

;; Note: we used to do IF-merging of two choose-value expressions by iterating
;; over one list doing merge-val-conds-with-val with the other.  But this was a
;; fairly major expense in certain proofs so now we organize things so that we
;; can merge linearly when the keys of the two val-conds are in about the same
;; order, i.e. with val-cond-merge.



(fgl::def-fgl-branch-merge if-of-choose-value-with-choose-value
  (equal (if test (choose-value val-conds1) (choose-value val-conds2))
         (choose-value (val-cond-merge test val-conds1 val-conds2))))


(fgl::def-fgl-branch-merge if-of-choose-value-with-if
  (equal (if test (choose-value val-conds) (if test2 then else))
         (let ((choose (choose-value val-conds)))
           (if test2 (if test choose then)
             (if test choose else)))))

(define-continue choose-value-if
  (fgl::def-fgl-rewrite choose-value-if-of-constants
    (equal (choose-value-if test (fgl::concrete then) (fgl::concrete else))
           (choose-value (list (cons test then) else)))
    :hints(("Goal" :in-theory (enable choose-value))))

  (fgl::def-fgl-rewrite choose-value-if-of-choose-value-and-const
    (equal (choose-value-if test (choose-value then) (fgl::concrete else))
           (choose-value (merge-val-conds-with-val test then else))))

  (fgl::def-fgl-rewrite choose-value-if-of-choose-values
    (equal (choose-value-if test (choose-value then) (choose-value else))
           (choose-value (val-cond-merge test then else)))))



;; This function is opened by FGL and produces a choose-value object
;; such that choose-value of that object equals x if x is one of the given elements.
;; E.g., for elts ("a" "b" "c") this produces
;; (list (cons (equal x "a") "a") (cons (equal x "b") "b") "c").
(define some-value-chooser (x elts)
  :guard (consp elts)
  (if (atom (cdr elts))
      (list (car elts))
    (cons (cons (and (equal x (car elts)) t)
                (car elts))
          (some-value-chooser x (cdr elts))))
  ///
  (defthm choose-value-of-some-value-chooser
    (equal (choose-value (some-value-chooser x elts))
           (if (member-equal x elts)
               x
             (car (last elts))))
    :hints(("Goal" :in-theory (enable choose-value)))))




;; Constrain the equality tests produced by some-value-chooser such that
;; we can't set more than one to true, i.e. we can't have both Boolean variables
;; (equal x "a") and (equal x "b") true.
(define equals-another (x val elts)
  (if (atom elts)
      nil
    (or (and (equal x (car elts))
             (not (equal val (car elts))))
        (equals-another x val (cdr elts))))
  ///
  (defthmd not-equals-another
    (not (equals-another x x elts))))

(define equals-more-than-one (x elts)
  (if (or (atom elts)
          (atom (cdr elts)))
      nil
    (or (acl2::and* (equal x (car elts))
                    (equals-another x (car elts) (cdr elts)))
        (equals-more-than-one x (cdr elts))))
  ///
  (defthmd not-equals-more-than-one
    (not (equals-more-than-one x elts))
    :hints(("Goal" :in-theory (enable not-equals-another acl2::and*)))))


(define trigger-enum-value-constraint (x elts)
  (declare (ignore x elts))
  t)

(encapsulate nil
  (set-ignore-ok t)
  (fgl::def-fgl-boolean-constraint enum-value-constraint
    :bindings ((trigger    (trigger-enum-value-constraint x elts)))
    :body (and trigger
               (not (equals-more-than-one x elts)))
    :hints(("Goal" :in-theory (enable not-equals-more-than-one)))))

(local (defthm member-of-rev
         (iff (member-equal k (acl2::rev x))
              (member-equal k x))))

(local (defthm car-last-of-append
         (equal (car (last (append x y)))
                (if (consp y)
                    (car (last y))
                  (car (last x))))))

(local (defthm car-last-of-rev
         (equal (car (last (acl2::rev x)))
                (car x))
         :hints(("Goal" :in-theory (enable acl2::Rev)))))

(fgl::def-fgl-rewrite ty-fix-val-of-enum
  (implies (and (syntaxp (fgl::fgl-object-case ty :g-concrete))
                (type_desc-case (ty->desc ty) :t_enum)
                (consp (t_enum->elts (ty->desc ty))))
           (equal (ty-fix-val x ty)
                  (let* ((elts (t_enum->elts (ty->desc ty)))
                         (rev-elts (acl2::rev elts))
                         (val (fgl::fgl-hide (v_label->val x)))
                         (ignore (fgl::trigger-constraints
                                  ;; Why do we skip the first element when
                                  ;; adding the constraint?  This is the
                                  ;; default value, and as such we don't want
                                  ;; to add a Boolean variable for whether val
                                  ;; equals that element -- it will be more or
                                  ;; less assumed to be that element if it
                                  ;; isn't any of the other ones.
                                  (trigger-enum-value-constraint val (cdr elts)))))
                    (declare (ignore ignore))
                    (v_label (choose-value (some-value-chooser val rev-elts))))))
  :hints (("goal" :expand ((ty-fix-val x ty))
           :in-theory (enable fgl::fgl-hide))))

(fgl::remove-fgl-rewrite ty-fix-val-of-enum)


(fgl::def-fgl-rewrite enum-val-cases-when-bind-ty-satisfied
  (implies (and (bind-ty-satisfied ty x)
                (syntaxp (fgl::fgl-object-case ty :g-concrete))
                (type_desc-case (ty->desc ty) :t_enum))
           (equal (v_label->val x)
                  (let* ((elts (t_enum->elts (ty->desc ty)))
                         (rev-elts (acl2::rev elts))
                         (val (fgl::fgl-hide (v_label->val x)))
                         (ignore (fgl::trigger-constraints
                                  ;; Why do we skip the first element? see above.
                                  (trigger-enum-value-constraint val (cdr elts)))))
                    (declare (ignore ignore))
                    (choose-value (some-value-chooser val rev-elts)))))
  :hints(("Goal" :in-theory (enable bind-ty-satisfied
                                    fgl::fgl-hide)
          :expand ((ty-satisfied x ty)))))




(fgl::def-fgl-rewrite v_label->val-of-ty-fix-val
  (implies (ty-satisfiable ty)
           (equal (v_label->val (ty-fix-val x (fgl::concrete ty)))
                  (b* ((desc (ty->desc ty)))
                    (type_desc-case desc
                      :t_enum (let* ((rev-elts (acl2::rev desc.elts))
                                     (val (fgl::fgl-hide (v_label->val x)))
                                     (ignore (fgl::trigger-constraints
                                              ;; Why do we skip the first element when
                                              ;; adding the constraint?  This is the
                                              ;; default value, and as such we don't want
                                              ;; to add a Boolean variable for whether val
                                              ;; equals that element -- it will be more or
                                              ;; less assumed to be that element if it
                                              ;; isn't any of the other ones.
                                              (trigger-enum-value-constraint val (cdr desc.elts)))))
                                (declare (ignore ignore))
                                (choose-value (some-value-chooser val rev-elts)))
                      :otherwise (fgl::abort-rewrite (v_label->val (ty-fix-val x (fgl::concrete ty))))))))
  :hints(("Goal"
          :expand ((ty-satisfiable ty)
                   (ty-fix-val x ty)))))

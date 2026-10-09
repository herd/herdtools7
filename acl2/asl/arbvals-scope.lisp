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

(include-book "arbvals")
(include-book "clause-processors/pseudo-term-fty" :dir :system)

(local (std::add-default-post-define-hook :fix))




;; Comparison of two arbaddr-offsets: weak partial order, an arbaddr-offset is
;; less than or equal to another if each of its indices are <= the
;; corresponding index of the other.

(define call-count-map-lte-aux ((keys identifierset-p)
                                (x call-count-map-p)
                                (y call-count-map-p))
  :returns (lte)
  :measure (acl2-count (identifierset-fix keys))
  :hints (("goal" :in-theory (enable identifierset-fix)))
  (b* ((keys (identifierset-fix keys)))
    (if (set::emptyp keys)
        t
      (let ((key (set::head keys)))
        (and (<= (call-count-map-entry key x)
                 (call-count-map-entry key y))
             (call-count-map-lte-aux (set::tail keys) x y)))))
  ///
  (defretd call-count-map-entry-bound-by-<fn>
    (implies (and lte
                  (set::in (identifier-fix key) (identifierset-fix keys)))
             (<= (call-count-map-entry key x)
                 (call-count-map-entry key y)))))

(define call-count-map-lte ((x call-count-map-p)
                            (y call-count-map-p))
  :returns (lte)
  (call-count-map-lte-aux (omap::keys (call-count-map-fix x)) x y)
  ///
  (defretd call-count-map-entry-bound-by-<fn>
    (implies lte
             (<= (call-count-map-entry key x)
                 (call-count-map-entry key y)))
    :hints (("goal" :use ((:instance call-count-map-entry-bound-by-call-count-map-lte-aux
                           (keys (omap::keys (call-count-map-fix x)))))
             :in-theory (e/d (call-count-map-entry
                              omap::in-of-keys-to-assoc-under-iff)))))

  (defthm call-count-map-lte-of-min
    (call-count-map-lte nil y)
    :hints(("Goal" :in-theory (enable call-count-map-lte-aux)))))


(define call-count-map-lte-badguy-aux ((keys identifierset-p)
                                (x call-count-map-p)
                                (y call-count-map-p))
  :returns (badguy)
  :measure (acl2-count (identifierset-fix keys))
  :hints (("goal" :in-theory (enable identifierset-fix)))
  (b* ((keys (identifierset-fix keys)))
    (if (set::emptyp keys)
        nil
      (let ((key (set::head keys)))
        (if (<= (call-count-map-entry key x)
                (call-count-map-entry key y))
            (call-count-map-lte-badguy-aux (set::tail keys) x y)
          key))))
  ///
  (defretd call-count-map-lte-aux-by-<fn>
    (implies (<= (call-count-map-entry badguy x)
                 (call-count-map-entry badguy y))
             (call-count-map-lte-aux keys x y))
    :hints(("Goal" :in-theory (enable call-count-map-lte-aux)
            :induct t))))

(define call-count-map-lte-badguy ((x call-count-map-p)
                                   (y call-count-map-p))
  :returns (badguy)
  (call-count-map-lte-badguy-aux (omap::keys (call-count-map-fix x)) x y)
  ///
  (defretd call-count-map-lte-by-badguy
    (implies (<= (call-count-map-entry badguy x)
                 (call-count-map-entry badguy y))
             (call-count-map-lte x y))
    :hints (("goal" :use ((:instance call-count-map-lte-aux-by-call-count-map-lte-badguy-aux
                           (keys (omap::keys (call-count-map-fix x)))))
             :in-theory (enable call-count-map-lte)))))




(defthm call-count-map-lte-of-sum-1
  (call-count-map-lte x (call-count-map-sum x y))
  :hints (("goal" :in-theory (enable call-count-map-lte-by-badguy))))

(defthm call-count-map-lte-of-sum-2
  (call-count-map-lte y (call-count-map-sum x y))
  :hints (("goal" :in-theory (enable call-count-map-lte-by-badguy))))


(defthm call-count-map-lte-preserved-by-sum
  (iff (call-count-map-lte (call-count-map-sum x y) (call-count-map-sum x z))
       (call-count-map-lte y z))
  :hints ((and stable-under-simplificationp
               (b* ((lit (assoc 'call-count-map-lte clause))
                    ((list arg1 arg2) (cdr lit))
                    (other-arg1 (if (eq arg1 'y) '(call-count-map-sum x y) 'y))
                    (other-arg2 (if (eq arg2 'z) '(call-count-map-sum x z) 'z)))
                 `(:use ((:instance call-count-map-lte-by-badguy
                          (x ,arg1) (y ,arg2))
                         (:instance call-count-map-entry-bound-by-call-count-map-lte
                          (key (call-count-map-lte-badguy ,arg1 ,arg2))
                          (x ,other-arg1) (y ,other-arg2))))))))

(defthm call-count-map-lte-reflexive
  (call-count-map-lte x x)
  :hints(("Goal" :in-theory (enable call-count-map-lte-by-badguy))))

(defthm call-count-map-lte-transitive
  (implies (and (call-count-map-lte x y)
                (call-count-map-lte y z))
           (call-count-map-lte x z))
  :hints(("Goal" :use ((:instance call-count-map-lte-by-badguy
                        (y z))
                       (:instance call-count-map-entry-bound-by-call-count-map-lte
                        (key (call-count-map-lte-badguy x z)))
                       (:instance call-count-map-entry-bound-by-call-count-map-lte
                        (key (call-count-map-lte-badguy x z))
                        (x y) (y z))))))


(local (defthm equal-assoc-when-equal-cdr-assoc
         (implies (And (omap::assoc key x)
                       (omap::assoc key y))
                  (equal (equal (omap::assoc key x) (omap::assoc key y))
                         (Equal (cdr (omap::assoc key x))
                                (cdr (omap::assoc key y)))))
         :hints(("Goal" :in-theory (enable omap::assoc)))))

(defthmd call-count-map-lte-asymm
  (implies (call-count-map-lte y x)
           (iff (call-count-map-lte x y)
                (equal (call-count-map-fix x)
                       (call-count-map-fix y))))
  :hints ((and stable-under-simplificationp
               '(:use ((:instance omap::diff-key-when-unequal
                        (x (call-count-map-fix x))
                        (y (call-count-map-fix y)))
                       (:instance call-count-map-entry-bound-by-call-count-map-lte
                        (key (omap::diff-key (call-count-map-fix x)
                                             (call-count-map-fix y))))
                       (:instance call-count-map-entry-bound-by-call-count-map-lte
                        (key (omap::diff-key (call-count-map-fix x)
                                             (call-count-map-fix y)))
                        (x y) (y x))
                       (:instance identifier-p-when-assoc-call-count-map-p-binds-free-x
                        (k (omap::diff-key (call-count-map-fix x)
                                           (call-count-map-fix y)))
                        (x (call-count-map-fix x)))
                       (:instance identifier-p-when-assoc-call-count-map-p-binds-free-x
                        (k (omap::diff-key (call-count-map-fix x)
                                           (call-count-map-fix y)))
                        (x (call-count-map-fix y))))
                 :in-theory (e/d (call-count-map-entry)
                                 (identifier-p-when-assoc-call-count-map-p-binds-free-x))))))

(define arbaddr-offset-lte ((x arbaddr-offset-p) (y arbaddr-offset-p))
  :returns (lte)
  (b* (((arbaddr-offset x))
       ((arbaddr-offset y)))
    (and (call-count-map-lte x.callmap y.callmap)
         (<= x.arboffset y.arboffset)
         (<= x.foroffset y.foroffset)
         (<= x.whileoffset y.whileoffset)
         (<= x.repeatoffset y.repeatoffset)))
  ///
  (defthm arbaddr-offset-lte-of-sum-1
    (arbaddr-offset-lte x (arbaddr-offset-sum x y))
    :hints(("Goal" :in-theory (enable arbaddr-offset-sum))))
  (defthm arbaddr-offset-lte-of-sum-2
    (arbaddr-offset-lte y (arbaddr-offset-sum x y))
    :hints(("Goal" :in-theory (enable arbaddr-offset-sum))))
  (defthm arbaddr-offset-lte-preserved-by-sum
    (iff (arbaddr-offset-lte (arbaddr-offset-sum x y) (arbaddr-offset-sum x z))
         (arbaddr-offset-lte y z))
    :hints(("Goal" :in-theory (enable arbaddr-offset-sum))))
  
  (defthm arbaddr-offset-lte-reflexive
    (arbaddr-offset-lte x x))
  (defthm arbaddr-offset-lte-transitive
    (implies (and (arbaddr-offset-lte x y)
                  (arbaddr-offset-lte y z))
             (arbaddr-offset-lte x z)))
  (defthmd arbaddr-offset-lte-asymm
    (implies (arbaddr-offset-lte y x)
             (iff (arbaddr-offset-lte x y)
                  (equal (arbaddr-offset-fix x)
                         (arbaddr-offset-fix y))))
    :hints(("Goal" :in-theory (enable call-count-map-lte-asymm
                                      arbaddr-offset-fix-redef))))

  (make-event
   `(defthm arbaddr-offset-lte-of-min
      (arbaddr-offset-lte ',(make-arbaddr-offset) x))))


(define arbaddr-offset-lte-component ((offset arbaddr-offset-p)
                                      (x arbaddr-component-p))
  (b* (((arbaddr-offset offset)))
    (arbaddr-component-case x
      :call (<= (call-count-map-entry x.fn offset.callmap) x.idx)
      :arb (<= offset.arboffset x.idx)
      :for (<= offset.foroffset x.idx)
      :while (<= offset.whileoffset x.idx)
      :repeat (<= offset.repeatoffset x.idx)))
  ///
  (defthm arbaddr-offset-lte-component-transitive
    (implies (and (arbaddr-offset-lte x y)
                  (arbaddr-offset-lte-component y z))
             (arbaddr-offset-lte-component x z))
    :hints(("Goal" :in-theory (enable arbaddr-offset-lte)
            :use ((:instance call-count-map-entry-bound-by-call-count-map-lte
                   (x (arbaddr-offset->callmap x))
                   (y (arbaddr-offset->callmap y))
                   (key (arbaddr-component-call->fn z)))))))

  (defthm not-arbaddr-offset-lte-component-transitive
    (implies (and (arbaddr-offset-lte y x)
                  (not (arbaddr-offset-lte-component y z)))
             (not (arbaddr-offset-lte-component x z)))
    :hints(("Goal" :in-theory (enable arbaddr-offset-lte)
            :use ((:instance call-count-map-entry-bound-by-call-count-map-lte
                   (y (arbaddr-offset->callmap x))
                   (x (arbaddr-offset->callmap y))
                   (key (arbaddr-component-call->fn z)))))))

  (defthm not-arbaddr-offset-lte-component-transitive-2
    (implies (and (not (arbaddr-offset-lte-component y z))
                  (arbaddr-offset-lte y x))
             (not (arbaddr-offset-lte-component x z)))
    :hints (("goal" :use not-arbaddr-offset-lte-component-transitive
             :in-theory (disable not-arbaddr-offset-lte-component-transitive
                                 arbaddr-offset-lte-component)))))



                        
;; Prove that writes to arbvals outside of the scope involved in a particular
;; interpreter call don't affect the results of that call.

;; For most functions, that scope consists of written addresses satisfying
;;   - the input arbaddr is a (proper) suffix of the written address
;;   - the last component of the written address before the input arbaddr is within the
;;     arboffset scope, namely:
;;        -- the input arboffset is lte that component (using arbaddr-offset-lte-component)
;;        -- the arboffset sum of the input arboffset and the interpreter's input form(s)
;;           (given by *arboffset-for-form-table*) is not lte that component.

(define arbaddr-suffixp ((base arbaddr-p) (x arbaddr-p))
  (cond ((arbaddr-equiv x base) t)
        ((atom x) nil)
        (t (arbaddr-suffixp base (cdr x)))))

(define arbaddr-last-nonbase-component ((x arbaddr-p)
                                        (base arbaddr-p))
  :returns (last (iff (arbaddr-component-p last) last))
  ;; Returns the portion of x before base.
  (cond ((atom x) nil)
        ((arbaddr-equiv (cdr x) base) (arbaddr-component-fix (car x)))
        (t (arbaddr-last-nonbase-component (cdr x) base))))

(define arbaddr-in-scope ((x arbaddr-p)
                          (base arbaddr-p)
                          (offset arbaddr-offset-p)
                          (scope arbaddr-offset-p))
  (let ((last (arbaddr-last-nonbase-component x base)))
    (and last
         (arbaddr-offset-lte-component offset last)
         (not (arbaddr-offset-lte-component scope last)))))

(include-book "std/util/defret-mutual-generate" :dir :system)

(defun-sk arbvals-equiv-in-scope (arbmap1 arbmap2 arbaddr arboffset scope)
  (forall (key ty)
          (implies (arbaddr-in-scope key arbaddr arboffset scope)
                   (equal (arbmap-lookup key ty arbmap1)
                          (arbmap-lookup key ty arbmap2))))
  :rewrite :direct)

(defun-sk arbvals-equiv-in-arbaddr-scope (arbmap1 arbmap2 arbaddr)
  (forall (key ty)
          (implies (arbaddr-suffixp arbaddr key)
                   (equal (arbmap-lookup key ty arbmap1)
                          (arbmap-lookup key ty arbmap2))))
  :rewrite :direct)

(define arbaddr-in-loop-scope ((x arbaddr-p)
                               (base arbaddr-p)
                               (is_while)
                               (loop-iter natp)
                               (loop-idx natp))
  (let ((last (arbaddr-last-nonbase-component x base)))
    (and last
         (if is_while
             (arbaddr-component-case last
               :while (and (eql last.idx (lnfix loop-idx))
                           (<= (lnfix loop-iter) last.iter))
               :otherwise nil)
           (arbaddr-component-case last
             :repeat (and (eql last.idx (lnfix loop-idx))
                         (<= (lnfix loop-iter) last.iter))
             :otherwise nil)))))

(define arbaddr-in-for-scope ((x arbaddr-p)
                              (base arbaddr-p)
                              (loop-iter natp)
                              (loop-idx natp))
  (let ((last (arbaddr-last-nonbase-component x base)))
    (and last
         (arbaddr-component-case last
           :for (and (eql last.idx (lnfix loop-idx))
                     (<= (lnfix loop-iter) last.iter))
           :otherwise nil))))


(defun-sk arbvals-equiv-in-loop-scope (arbmap1 arbmap2 arbaddr is_while loop-iter loop-idx)
  (forall (key ty)
          (implies (arbaddr-in-loop-scope key arbaddr is_while loop-iter loop-idx)
                   (equal (arbmap-lookup key ty arbmap1)
                          (arbmap-lookup key ty arbmap2))))
  :rewrite :direct)

(defun-sk arbvals-equiv-in-for-scope (arbmap1 arbmap2 arbaddr loop-iter loop-idx)
  (forall (key ty)
          (implies (arbaddr-in-for-scope key arbaddr loop-iter loop-idx)
                   (equal (arbmap-lookup key ty arbmap1)
                          (arbmap-lookup key ty arbmap2))))
  :rewrite :direct)

(in-theory (disable arbvals-equiv-in-scope
                    arbvals-equiv-in-arbaddr-scope
                    arbvals-equiv-in-loop-scope
                    arbvals-equiv-in-for-scope))








(local (include-book "centaur/vl/util/default-hints" :dir :system))



;; Metatheory to resolve LTE of arbaddr-offsets.
(defevaluator arboff-ev arboff-ev-list
  ((arbaddr-offset-sum x y)
   (arbaddr-offset-lte x y))
  :namedp t)

(acl2::def-ev-pseudo-term-fty-support arboff-ev arboff-ev-list)

(defthm arboff-ev-list-of-append
  (equal (arboff-ev-list (append x y) a)
         (append (arboff-ev-list x a)
                 (arboff-ev-list y a))))

(fty::deflist arbaddr-offsetlist :elt-type arbaddr-offset :true-listp t)

(define arbaddr-offset-sum-list ((x arbaddr-offsetlist-p))
  :returns (sum arbaddr-offset-p)
  (if (atom x)
      (make-arbaddr-offset)
    (arbaddr-offset-sum (car x) (arbaddr-offset-sum-list (cdr x))))
  ///
  (defthm arbaddr-offset-sum-list-of-append
    (equal (arbaddr-offset-sum-list (append x y))
           (arbaddr-offset-sum (arbaddr-offset-sum-list x)
                               (arbaddr-offset-sum-list y)))
    :hints(("Goal" :in-theory (enable arbaddr-offset-sum-associative)))))

(define collect-arbaddr-summands ((x pseudo-termp))
  :returns (summands pseudo-term-listp)
  :measure (acl2::pseudo-term-count x)
  (acl2::pseudo-term-case x
    :fncall (if (and (eq x.fn 'arbaddr-offset-sum)
                     (eql (len x.args) 2))
                (append (collect-arbaddr-summands (first x.args))
                        (collect-arbaddr-summands (second x.args)))
              (list (acl2::pseudo-term-fix x)))
    :otherwise (list (acl2::pseudo-term-fix x)))
  ///
  (defret <fn>-correct
    (equal (arbaddr-offset-sum-list (arboff-ev-list summands a))
           (arbaddr-offset-fix (arboff-ev x a)))
    :hints(("Goal" :in-theory (disable append)
            :induct <call>
            :expand ((:free (x y) (arbaddr-offset-sum-list (cons x y))))))))

(define reduce-arbaddr-summand-comparison ((trys pseudo-term-listp)
                                           (x pseudo-term-listp)
                                           (y pseudo-term-listp))
  ;; Trys is really a tail of x -- we'll assume subsetp-equal
  :returns (mv (xrem pseudo-term-listp)
               (yrem pseudo-term-listp))
  (b* (((when (atom trys)) (mv (acl2::pseudo-term-list-fix x)
                               (acl2::pseudo-term-list-fix y)))
       (try1 (acl2::pseudo-term-fix (car trys)))
       (x (acl2::pseudo-term-list-fix x))
       (y (acl2::pseudo-term-list-fix y))
       ((unless (and (member-equal try1 x)
                     (member-equal try1 y)))
        (reduce-arbaddr-summand-comparison (cdr trys) x y))
       (x (remove1-equal try1 x))
       (y (remove1-equal try1 y)))
    (reduce-arbaddr-summand-comparison (cdr trys) x y))
  ///
  (local
   (defthm arbaddr-sum-with-removed-equals-orig
     (implies (member-equal e x)
              (equal (arbaddr-offset-sum e (arbaddr-offset-sum-list (remove1-equal e x)))
                     (arbaddr-offset-sum-list x)))
     :hints(("Goal" :in-theory (enable arbaddr-offset-sum-list
                                       remove1-equal
                                       arbaddr-offset-sum-commutative-2
                                       member-equal)))))
  
  (local (defthm remove1-equal-both-preserves-arbaddr-lte
           (implies (and (member-equal e x)
                         (member-equal e y))
                    (iff (arbaddr-offset-lte (arbaddr-offset-sum-list (remove1-equal e x))
                                             (arbaddr-offset-sum-list (remove1-equal e y)))
                         (arbaddr-offset-lte (arbaddr-offset-sum-list x)
                                             (arbaddr-offset-sum-list y))))
           :hints (("goal" :use ((:instance arbaddr-offset-lte-preserved-by-sum
                                  (x e)
                                  (y (arbaddr-offset-sum-list (remove1-equal e x)))
                                  (z (arbaddr-offset-sum-list (remove1-equal e y)))))
                    :in-theory (disable arbaddr-offset-lte-preserved-by-sum)))))

  (local (defthm member-terms-implies-member-evals
           (implies (member-equal e x)
                    (member-equal (arboff-ev e a) (arboff-ev-list x a)))))
  
  (local (defthm remove1-equal-commutes-over-arboff-ev-list-under-sum-list
           (implies (member-equal e x)
                    (equal (arbaddr-offset-sum-list (arboff-ev-list (remove1-equal e x) a))
                           (arbaddr-offset-sum-list (remove1-equal (arboff-ev e a)
                                                                   (arboff-ev-list x a)))))
           :hints(("Goal" :induct (remove1-equal e x)
                   :in-theory (enable (:i remove1-equal))
                   :expand ((:free (a b) (arbaddr-offset-sum-list (cons a b)))))
                  (and stable-under-simplificationp
                       '(:use ((:instance arbaddr-sum-with-removed-equals-orig
                                (e (arboff-ev e a))
                                (x (arboff-ev-list (cdr x) a))))
                         :in-theory (disable arbaddr-sum-with-removed-equals-orig))))))

  (local (defthm member-terms-implies-member-evals-fix
           (implies (member-equal (acl2::pseudo-term-fix e) (acl2::pseudo-term-list-fix x))
                    (member-equal (arboff-ev e a) (arboff-ev-list x a)))
           :hints(("Goal" :in-theory (enable acl2::pseudo-term-list-fix)
                   :induct (len x)))))
  
  (defret <fn>-correct
    (iff (arbaddr-offset-lte (arbaddr-offset-sum-list (arboff-ev-list xrem a))
                             (arbaddr-offset-sum-list (arboff-ev-list yrem a)))
         (arbaddr-offset-lte (arbaddr-offset-sum-list (arboff-ev-list x a))
                             (arbaddr-offset-sum-list (arboff-ev-list y a))))
    :hints (("goal" :induct <call>
             :in-theory (enable subsetp-equal))
            (and stable-under-simplificationp
                 '(:use ((:instance remove1-equal-both-preserves-arbaddr-lte
                          (e (arboff-ev (car trys) a))
                          (x (arboff-ev-list x a))
                          (y (arboff-ev-list y a))))
                   :do-not-induct t))))

  (defret <fn>-not-consp-special-case
    (implies (and (bind-free '((a . a)) (a))
                  (not (arbaddr-offset-lte (arbaddr-offset-sum-list (arboff-ev-list x a))
                                           (arbaddr-offset-sum-list (arboff-ev-list y a)))))
             (consp xrem))
    :hints (("goal" :use ((:instance <fn>-correct))
             :in-theory (disable <fn>-correct <fn>)))))

(define arbaddr-offset-sum-nest ((x pseudo-term-listp))
  :returns (sum pseudo-termp)
  (cond ((atom x) (acl2::pseudo-term-quote (make-arbaddr-offset)))
        ((atom (cdr x)) (acl2::pseudo-term-fix (car x)))
        (t (acl2::pseudo-term-fncall 'arbaddr-offset-sum
                                     (list (car x) (arbaddr-offset-sum-nest (cdr x))))))
  ///
  (defret <fn>-correct
    (arbaddr-offset-equiv (arboff-ev sum a)
                          (arbaddr-offset-sum-list (arboff-ev-list x a)))
    :hints(("Goal" :in-theory (enable arbaddr-offset-sum-list)))))
      
(define arbaddr-offset-lte-reduce-summands ((x pseudo-termp))
  :Returns (new-x pseudo-termp)
  (acl2::pseudo-term-case x
    :fncall (if (and (eq x.fn 'arbaddr-offset-lte)
                     (eql (len x.args) 2))
                (b* ((summands1 (collect-arbaddr-summands (first x.args)))
                     (summands2 (collect-arbaddr-summands (second x.args)))
                     ((mv reduced1 reduced2)
                      (reduce-arbaddr-summand-comparison (if (< (len summands1) (len summands2))
                                                             summands1
                                                           summands2)
                                                         summands1 summands2)))
                  (if (atom reduced1)
                      ''t
                    (acl2::pseudo-term-fncall
                     'arbaddr-offset-lte
                     (list (arbaddr-offset-sum-nest reduced1)
                           (arbaddr-offset-sum-nest reduced2)))))
              (acl2::pseudo-term-fix x))
    :otherwise (acl2::pseudo-term-fix x))
  ///
  (local (in-theory (disable arboff-ev-list-of-cons)))
  (defthm arbaddr-offset-lte-reduce-summands-correct
    (equal (arboff-ev x a)
           (arboff-ev (arbaddr-offset-lte-reduce-summands x) a))
    :rule-classes ((:meta :trigger-fns (arbaddr-offset-lte)))))
                                                                      
  








(defthm arbaddr-in-scope-of-sum-1
  (implies (arbaddr-in-scope key arbaddr arboffset scope)
           (arbaddr-in-scope key arbaddr arboffset (arbaddr-offset-sum scope2 scope)))
  :hints(("Goal" :in-theory (enable arbaddr-in-scope))))

(defthm arbaddr-in-scope-of-sum-2
  (implies (arbaddr-in-scope key arbaddr arboffset scope)
           (arbaddr-in-scope key arbaddr arboffset (arbaddr-offset-sum scope scope2)))
  :hints(("Goal" :in-theory (enable arbaddr-in-scope))))

(defthm arbaddr-in-scope-of-sum-3
  (implies (arbaddr-in-scope key arbaddr (arbaddr-offset-sum scope arboffset) scope2)
           (arbaddr-in-scope key arbaddr arboffset (arbaddr-offset-sum scope2 scope)))
  :hints(("Goal" :in-theory (enable arbaddr-in-scope
                                    arbaddr-offset-sum-commutative
                                    arbaddr-offset-sum-commutative-2
                                    arbaddr-offset-sum-associative))))

;; (defthm arbvals-equiv-in-scope-of-sum-1
;;   (implies (arbvals-equiv-in-scope arbmap1 arbmap2 arbaddr arboffset
;;                                    (arbaddr-offset-sum scope2 scope))
;;            (arbvals-equiv-in-scope arbmap1 arbmap2 arbaddr arboffset scope))
;;   :hints (("goal" :expand ((arbvals-equiv-in-scope arbmap1 arbmap2 arbaddr arboffset scope)))))

;; (defthm arbvals-equiv-in-scope-of-sum-2
;;   (implies (arbvals-equiv-in-scope arbmap1 arbmap2 arbaddr arboffset
;;                                    (arbaddr-offset-sum scope3 (arbaddr-offset-sum scope2 scope)))
;;            (arbvals-equiv-in-scope arbmap1 arbmap2 arbaddr arboffset scope))
;;   :hints (("goal" :expand ((arbvals-equiv-in-scope arbmap1 arbmap2 arbaddr arboffset scope)))))

;; (defthm arbvals-equiv-in-scope-of-sum-3
;;   (implies (arbvals-equiv-in-scope arbmap1 arbmap2 arbaddr arboffset
;;                                    (arbaddr-offset-sum scope scope2))
;;            (arbvals-equiv-in-scope arbmap1 arbmap2 arbaddr arboffset scope))
;;   :hints (("goal" :expand ((arbvals-equiv-in-scope arbmap1 arbmap2 arbaddr arboffset scope)))))

;; (defthm arbvals-equiv-in-scope-of-sum-4
;;   (implies (arbvals-equiv-in-scope arbmap1 arbmap2 arbaddr arboffset
;;                                    (arbaddr-offset-sum scope3 (arbaddr-offset-sum scope2 scope)))
;;            (arbvals-equiv-in-scope arbmap1 arbmap2 arbaddr
;;                                    (arbaddr-offset-sum scope arboffset)
;;                                    scope2))
;;   :hints (("goal" :expand ((arbvals-equiv-in-scope arbmap1 arbmap2 arbaddr
;;                                    (arbaddr-offset-sum scope arboffset)
;;                                    scope2)))))

;; (defthm arbvals-equiv-in-scope-of-sum-5
;;   (implies (arbvals-equiv-in-scope arbmap1 arbmap2 arbaddr arboffset
;;                                    (arbaddr-offset-sum scope3 (arbaddr-offset-sum scope2 scope)))
;;            (arbvals-equiv-in-scope arbmap1 arbmap2 arbaddr
;;                                    (arbaddr-offset-sum scope arboffset)
;;                                    scope2))
;;   :hints (("goal" :expand ((arbvals-equiv-in-scope arbmap1 arbmap2 arbaddr
;;                                    (arbaddr-offset-sum scope arboffset)
;;                                    scope2)))))

(defthm arbaddr-in-scope-when-scope-lte
  (implies (and (arbaddr-in-scope key arbaddr arboffset scope1)
                (arbaddr-offset-lte scope1 scope))
           (arbaddr-in-scope key arbaddr arboffset scope))
  :hints(("Goal" :in-theory (enable arbaddr-in-scope))))

(defthm arbaddr-in-scope-when-arboffset-lte
  (implies (and (arbaddr-in-scope key arbaddr arboffset1 scope)
                (arbaddr-offset-lte arboffset arboffset1))
           (arbaddr-in-scope key arbaddr arboffset scope))
  :hints(("Goal" :in-theory (enable arbaddr-in-scope))))

(defthm arbaddr-in-scope-when-in-narrower-scope
  (implies (and (arbaddr-in-scope key arbaddr arboffset1 scope1)
                (arbaddr-offset-lte arboffset arboffset1)
                (arbaddr-offset-lte scope1 scope))
           (arbaddr-in-scope key arbaddr arboffset scope))
  :hints (("goal" :use ((:instance arbaddr-in-scope-when-scope-lte))
           :in-theory (disable arbaddr-in-scope-when-scope-lte))))

(defthm arbvals-equiv-in-scope-left
  (implies (and (arbvals-equiv-in-scope arbmap1 arbmap2 arbaddr arboffset scope)
                (arbaddr-offset-lte scope1 scope))
           (arbvals-equiv-in-scope arbmap1 arbmap2 arbaddr arboffset scope1))
  :hints (("goal" :expand ((arbvals-equiv-in-scope arbmap1 arbmap2 arbaddr arboffset scope1)))))

(defthm arbvals-equiv-in-scope-right
  (implies (and (arbvals-equiv-in-scope arbmap1 arbmap2 arbaddr arboffset scope)
                (arbaddr-offset-lte arboffset arboffset1))
           (arbvals-equiv-in-scope arbmap1 arbmap2 arbaddr arboffset1 scope))
  :hints (("goal" :expand ((arbvals-equiv-in-scope arbmap1 arbmap2 arbaddr arboffset1 scope)))))

(defthm arbvals-equiv-in-scope-both
  (implies (and (arbvals-equiv-in-scope arbmap1 arbmap2 arbaddr arboffset scope)
                (arbaddr-offset-lte arboffset arboffset1)
                (arbaddr-offset-lte scope1 scope))
           (arbvals-equiv-in-scope arbmap1 arbmap2 arbaddr arboffset1 scope1))
  :hints (("goal" :use ((:instance arbvals-equiv-in-scope-left))
           :in-theory (disable arbvals-equiv-in-scope-left))))

(defthm arbaddr-last-nonbase-component-implies-suffixp
  (implies (not (arbaddr-suffixp base x))
           (not (arbaddr-last-nonbase-component x base)))
  :hints(("Goal" :in-theory (enable arbaddr-suffixp
                                    arbaddr-last-nonbase-component))))

(defthm arbaddr-suffixp-when-suffixp-cons
  (implies (not (arbaddr-suffixp x y))
           (not (arbaddr-suffixp (cons e x) y)))
  :hints(("Goal" :in-theory (enable arbaddr-suffixp arbaddr-fix))))

(defthm arbaddr-suffixp-when-arbaddr-in-scope
  (implies (arbaddr-in-scope x (cons component arbaddr) arboffset scope)
           (arbaddr-suffixp arbaddr x))
  :hints(("Goal" :in-theory (enable arbaddr-in-scope))))

(defthm arbaddr-suffixp-when-arbaddr-in-scope2
  (implies (arbaddr-in-scope x arbaddr arboffset scope)
           (arbaddr-suffixp arbaddr x))
  :hints(("Goal" :in-theory (enable arbaddr-in-scope))))

(defthm arbvals-equiv-in-scope-of-add-scope-when-equiv-in-arbaddr-scope
  (implies (arbvals-equiv-in-arbaddr-scope arbmap1 arbmap2 arbaddr)
           (arbvals-equiv-in-scope
            arbmap1 arbmap2 (cons component arbaddr) arboffset scope))
  :hints (("goal" :expand ((arbvals-equiv-in-scope
                            arbmap1 arbmap2 (cons component arbaddr) arboffset scope)))))

(defthm arbvals-equiv-in-scope-when-equiv-in-arbaddr-scope
  (implies (arbvals-equiv-in-arbaddr-scope arbmap1 arbmap2 arbaddr)
           (arbvals-equiv-in-scope
            arbmap1 arbmap2 arbaddr arboffset scope))
  :hints (("goal" :expand ((arbvals-equiv-in-scope
                            arbmap1 arbmap2 arbaddr arboffset scope)))))


(encapsulate nil

  (local (defthm arbaddr-last-nonbase-component-when-len
           (implies (<= (len x) (len y))
                    (not (arbaddr-last-nonbase-component x y)))
           :hints(("Goal" :in-theory (enable arbaddr-last-nonbase-component)
                   :induct t)
                  (and stable-under-simplificationp
                       '(:use ((:instance len-of-arbaddr-fix (x (cdr x)))
                               (:instance len-of-arbaddr-fix (x y)))
                         :in-theory (disable len-of-arbaddr-fix))))))

  (defthm arbaddr-last-nonbase-component-when-last-component-of-cons
    (implies (arbaddr-last-nonbase-component addr (cons component arbaddr))
             (equal (arbaddr-last-nonbase-component addr arbaddr)
                    (arbaddr-component-fix component)))
    :hints(("Goal" :in-theory (enable (:i arbaddr-last-nonbase-component)
                                      arbaddr-fix)
            :induct (arbaddr-last-nonbase-component addr arbaddr)
            :expand ((:free (arbaddr) (arbaddr-last-nonbase-component addr arbaddr))
                     ;; (:free (arbaddr2) (arbaddr-last-nonbase-component arbaddr arbaddr2))
                     ))
           (and stable-under-simplificationp
                '(:expand ((ARBADDR-LAST-NONBASE-COMPONENT (CDR ADDR) ARBADDR)))))))

(defthm arbaddr-in-scope-when-in-deeper-scope
  (implies (and (arbaddr-in-scope addr (cons component arbaddr) arboffset1 scope1)
                (arbaddr-offset-lte-component arboffset component)
                (not (arbaddr-offset-lte-component scope component)))
           (arbaddr-in-scope addr arbaddr arboffset scope))
  :hints (("goal" :in-theory (enable arbaddr-in-scope))))

(defthm arbvals-equiv-in-scope-of-add-scope-when-equiv-in-scope
  (implies (and (arbvals-equiv-in-scope arbmap1 arbmap2 arbaddr arboffset scope)
                (arbaddr-offset-lte-component arboffset component)
                (not (arbaddr-offset-lte-component scope component)))
           (arbvals-equiv-in-scope
            arbmap1 arbmap2 (cons component arbaddr) arboffset1 scope1))
  :hints (("goal" :expand ((arbvals-equiv-in-scope
            arbmap1 arbmap2 (cons component arbaddr) arboffset1 scope1)))))


(defthm arbaddr-offset-sum-of-find_catcher-arbaddr-offset-plus-find-catcher-stmt
  (implies (find_catcher tenv throwdata catchers)
           (arbaddr-offset-lte
            (arbaddr-offset-sum
             (find_catcher-arbaddr-offset tenv throwdata catchers)
             (stmt-arbaddr-offset
              (catcher->stmt (find_catcher tenv throwdata catchers))))
            (catcherlist-arbaddr-offset catchers)))
  :hints (("goal" :in-theory (enable find_catcher-arbaddr-offset
                                     find_catcher)
           :induct (find_catcher tenv throwdata catchers)
           :expand ((catcherlist-arbaddr-offset catchers)
                    (catcher-arbaddr-offset (car catchers))))))

(defthm arbaddr-offset-sum-of-find_catcher-arbaddr-offset-plus-find-catcher-stmt2
  (implies (find_catcher tenv throwdata catchers)
           (arbaddr-offset-lte
            (arbaddr-offset-sum
             (find_catcher-arbaddr-offset tenv throwdata catchers)
             (stmt-arbaddr-offset
              (catcher->stmt (find_catcher tenv throwdata catchers))))
            (arbaddr-offset-sum (catcherlist-arbaddr-offset catchers)
                                otherwise)))
  :hints (("goal" :use arbaddr-offset-sum-of-find_catcher-arbaddr-offset-plus-find-catcher-stmt
           :in-theory (disable arbaddr-offset-sum-of-find_catcher-arbaddr-offset-plus-find-catcher-stmt))))


(defthm arbaddr-in-loop-scope-when-in-scope-while
  (implies (and (arbaddr-in-scope addr (cons (arbaddr-component-while loop-idx loop-iter)
                                             arbaddr) offset scope)
                is_while)
           (arbaddr-in-loop-scope addr arbaddr is_while loop-iter loop-idx))
  :hints(("Goal" :in-theory (enable arbaddr-in-scope arbaddr-in-loop-scope))))

(defthm arbvals-equiv-in-scope-when-equiv-in-loop-scope-while
  (implies (and (arbvals-equiv-in-loop-scope arbmap arbmap1 arbaddr is_while loop-iter loop-idx)
                is_while)
           (arbvals-equiv-in-scope arbmap arbmap1
                                   (cons (arbaddr-component-while loop-idx loop-iter)
                                         arbaddr) offset scope))
  :hints(("Goal" :in-theory (enable arbvals-equiv-in-scope))))


(defthm arbaddr-in-loop-scope-when-in-scope-repeat
  (implies (arbaddr-in-scope addr (cons (arbaddr-component-repeat loop-idx loop-iter)
                                        arbaddr) offset scope)
           (arbaddr-in-loop-scope addr arbaddr nil loop-iter loop-idx))
  :hints(("Goal" :in-theory (enable arbaddr-in-scope arbaddr-in-loop-scope))))

(defthm arbvals-equiv-in-scope-when-equiv-in-loop-scope-repeat
  (implies (arbvals-equiv-in-loop-scope arbmap arbmap1 arbaddr nil loop-iter loop-idx)
           (arbvals-equiv-in-scope arbmap arbmap1
                                   (cons (arbaddr-component-repeat loop-idx loop-iter)
                                         arbaddr) offset scope))
  :hints(("Goal" :in-theory (enable arbvals-equiv-in-scope))))


(defthm arbaddr-in-loop-scope-of-incr
  (implies (arbaddr-in-loop-scope addr arbaddr is_while (+ 1 (nfix loop-iter)) loop-idx)
           (arbaddr-in-loop-scope addr arbaddr is_while loop-iter loop-idx))
  :hints(("Goal" :in-theory (enable arbaddr-in-loop-scope))))

(defthm arbvals-equiv-in-loop-scope-of-incr
  (implies (arbvals-equiv-in-loop-scope arbmap arbmap1 arbaddr is_while loop-iter loop-idx)
           (arbvals-equiv-in-loop-scope arbmap arbmap1 arbaddr is_while
                                        (+ 1 (nfix loop-iter))
                                        loop-idx))
  :hints(("Goal" :expand ((arbvals-equiv-in-loop-scope arbmap arbmap1 arbaddr is_while
                                        (+ 1 (nfix loop-iter))
                                        loop-idx)))))



(defthm arbaddr-in-for-scope-when-in-scope
  (implies (and (arbaddr-in-scope addr (cons (arbaddr-component-for loop-idx loop-iter)
                                             arbaddr) offset scope)
                is_while)
           (arbaddr-in-for-scope addr arbaddr loop-iter loop-idx))
  :hints(("Goal" :in-theory (enable arbaddr-in-scope arbaddr-in-for-scope))))

(defthm arbvals-equiv-in-scope-when-equiv-in-for-scope
  (implies (and (arbvals-equiv-in-for-scope arbmap arbmap1 arbaddr loop-iter loop-idx)
                is_while)
           (arbvals-equiv-in-scope arbmap arbmap1
                                   (cons (arbaddr-component-for loop-idx loop-iter)
                                         arbaddr) offset scope))
  :hints(("Goal" :in-theory (enable arbvals-equiv-in-scope))))

(defthm arbaddr-in-for-scope-of-incr
  (implies (arbaddr-in-for-scope addr arbaddr (+ 1 (nfix loop-iter)) loop-idx)
           (arbaddr-in-for-scope addr arbaddr loop-iter loop-idx))
  :hints(("Goal" :in-theory (enable arbaddr-in-for-scope))))

(defthm arbvals-equiv-in-for-scope-of-incr
  (implies (arbvals-equiv-in-for-scope arbmap arbmap1 arbaddr loop-iter loop-idx)
           (arbvals-equiv-in-for-scope arbmap arbmap1 arbaddr
                                       (+ 1 (nfix loop-iter))
                                       loop-idx))
  :hints(("Goal" :expand ((arbvals-equiv-in-for-scope arbmap arbmap1 arbaddr
                                                      (+ 1 (nfix loop-iter))
                                                      loop-idx)))))


(defthm arbaddr-in-scope-when-arbaddr-in-loop-scope-of-repeat
  (implies (arbaddr-in-loop-scope addr arbaddr nil iter (arbaddr-offset->repeatoffset arboffset))
           (arbaddr-in-scope addr arbaddr arboffset (arbaddr-offset-sum arboffset
                                                                        '(nil 0 0 0 1))))
  :hints(("Goal" :in-theory (enable arbaddr-in-scope
                                    arbaddr-in-loop-scope
                                    arbaddr-offset-lte-component
                                    arbaddr-offset-sum))))

(defthm arbvals-equiv-in-loop-scope-of-repeat-when-equiv-in-scope
  (implies (arbvals-equiv-in-scope arbmap arbmap1 arbaddr
                                   arboffset
                                   (arbaddr-offset-sum
                                    arboffset (make-arbaddr-offset :repeatoffset 1)))
           (arbvals-equiv-in-loop-scope
            arbmap arbmap1 arbaddr nil iter (arbaddr-offset->repeatoffset arboffset)))
  :hints (("goal" :expand ((arbvals-equiv-in-loop-scope
                            arbmap arbmap1 arbaddr nil iter (arbaddr-offset->repeatoffset arboffset))))))

(defthm arbaddr-in-scope-when-arbaddr-in-loop-scope-of-while
  (implies (arbaddr-in-loop-scope addr arbaddr t iter (arbaddr-offset->whileoffset arboffset))
           (arbaddr-in-scope addr arbaddr arboffset (arbaddr-offset-sum arboffset
                                                                        '(nil 0 0 1 0))))
  :hints(("Goal" :in-theory (enable arbaddr-in-scope
                                    arbaddr-in-loop-scope
                                    arbaddr-offset-lte-component
                                    arbaddr-offset-sum))))

(defthm arbvals-equiv-in-loop-scope-of-while-when-equiv-in-scope
  (implies (arbvals-equiv-in-scope arbmap arbmap1 arbaddr
                                   arboffset
                                   (arbaddr-offset-sum
                                    arboffset (make-arbaddr-offset :whileoffset 1)))
           (arbvals-equiv-in-loop-scope
            arbmap arbmap1 arbaddr t iter (arbaddr-offset->whileoffset arboffset)))
  :hints (("goal" :expand ((arbvals-equiv-in-loop-scope
                            arbmap arbmap1 arbaddr t iter (arbaddr-offset->whileoffset arboffset))))))


(defthm arbaddr-in-scope-when-arbaddr-in-for-scope
  (implies (arbaddr-in-for-scope addr arbaddr iter (arbaddr-offset->foroffset arboffset))
           (arbaddr-in-scope addr arbaddr arboffset (arbaddr-offset-sum arboffset
                                                                        '(nil 0 1 0 0))))
  :hints(("Goal" :in-theory (enable arbaddr-in-scope
                                    arbaddr-in-for-scope
                                    arbaddr-offset-lte-component
                                    arbaddr-offset-sum))))

(defthm arbvals-equiv-in-for-scope-when-equiv-in-scope
  (implies (arbvals-equiv-in-scope arbmap arbmap1 arbaddr
                                   arboffset
                                   (arbaddr-offset-sum
                                    arboffset (make-arbaddr-offset :foroffset 1)))
           (arbvals-equiv-in-for-scope
            arbmap arbmap1 arbaddr iter (arbaddr-offset->foroffset arboffset)))
  :hints (("goal" :expand ((arbvals-equiv-in-for-scope
                            arbmap arbmap1 arbaddr iter (arbaddr-offset->foroffset arboffset))))))

(encapsulate nil
  (local (defthm arbaddr-not-suffixp-by-len
           (implies (< (len x) (len y))
                    (not (arbaddr-suffixp y x)))
           :hints(("Goal" :in-theory (enable arbaddr-suffixp)
                   :induct t)
                  (and stable-under-simplificationp
                       '(:use ((:instance len-of-arbaddr-fix
                                (x x))
                               (:instance len-of-arbaddr-fix
                                (x y)))
                         :in-theory (disable len-of-arbaddr-fix))))))

  (defthm arbaddr-last-nonbase-component-when-suffixp-cons
    (implies (arbaddr-suffixp (cons component arbaddr) addr)
             (equal (arbaddr-last-nonbase-component addr arbaddr)
                    (arbaddr-component-fix component)))
    :hints(("Goal" :in-theory (enable arbaddr-suffixp
                                      arbaddr-last-nonbase-component)))))

(encapsulate nil
  (local (defthm call-count-map-entry-of-fnname-arbaddr-offset
           (equal (call-count-map-entry
                   name
                   (arbaddr-offset->callmap
                    (fnname-arbaddr-offset name)))
                  1)
           :hints(("Goal" :in-theory (enable fnname-arbaddr-offset
                                             call-count-map-entry)))))
  (defthm arbaddr-in-scope-when-call-suffixp
    (implies (and (arbaddr-suffixp (cons (arbaddr-component-call
                                          name
                                          (call-count-map-entry name
                                                                (arbaddr-offset->callmap calloffset)))
                                         arbaddr)
                                   addr)
                  (arbaddr-offset-lte arboffset calloffset)
                  (arbaddr-offset-lte (arbaddr-offset-sum
                                       (fnname-arbaddr-offset name)
                                       calloffset)
                                      scope))
             (arbaddr-in-scope addr arbaddr arboffset scope))
    :hints(("Goal" :in-theory (enable arbaddr-in-scope
                                      arbaddr-offset-lte-component
                                      arbaddr-offset-lte
                                      arbaddr-offset-sum
                                      call-count-map-entry-bound-by-call-count-map-lte)
            :use ((:instance call-count-map-entry-bound-by-call-count-map-lte
                   (key name)
                   (x (call-count-map-sum
                       (arbaddr-offset->callmap (fnname-arbaddr-offset name))
                       (arbaddr-offset->callmap calloffset)))
                   (y (arbaddr-offset->callmap scope))))))))


(defthm arbvals-equiv-in-arbaddr-scope-of-call
  (implies (and (arbvals-equiv-in-scope
                 arbmap arbmap1 arbaddr arboffset scope)
                (arbaddr-offset-lte arboffset calloffset)
                (arbaddr-offset-lte (arbaddr-offset-sum
                                     (fnname-arbaddr-offset name)
                                     calloffset)
                                    scope))
           (arbvals-equiv-in-arbaddr-scope
            arbmap arbmap1
            (cons (arbaddr-component-call
                   name (call-count-map-entry
                         name (arbaddr-offset->callmap calloffset)))
                  arbaddr)))
  :hints ((and stable-under-simplificationp
               `(:expand (,(car (last clause)))))))
           
                                                  





(defthm arbaddr-offset-lte-component-repeat
  (implies (arbaddr-offset-lte arboffset next)
           (arbaddr-offset-lte-component
            arboffset
            (Arbaddr-component-repeat (arbaddr-offset->repeatoffset next) n)))
  :hints(("Goal" :in-theory (enable arbaddr-offset-lte-component
                                    arbaddr-offset-lte))))

(defthm not-arbaddr-offset-lte-component-repeat
  (implies (arbaddr-offset-lte (arbaddr-offset-sum
                                (make-arbaddr-offset :repeatoffset 1) next)
                               arboffset)
           (not (arbaddr-offset-lte-component
                 arboffset
                 (Arbaddr-component-repeat (arbaddr-offset->repeatoffset next) n))))
  :hints(("Goal" :in-theory (enable arbaddr-offset-lte-component
                                    arbaddr-offset-lte
                                    arbaddr-offset-sum))))


(local (in-theory (disable (tau-system))))

(defthm maybe-stmt-arbaddr-offset-when-exists
  (implies x
           (equal (maybe-stmt-arbaddr-offset x)
                  (stmt-arbaddr-offset x)))
  :hints(("Goal" :expand ((maybe-stmt-arbaddr-offset x))
          :in-theory (enable maybe-stmt-some->val))))

(defthm maybe-expr-arbaddr-offset-when-exists
  (implies x
           (equal (maybe-expr-arbaddr-offset x)
                  (expr-arbaddr-offset x)))
  :hints(("Goal" :expand ((maybe-expr-arbaddr-offset x))
          :in-theory (enable maybe-expr-some->val))))


(defmacro expand-hint-for-arboffset (form)
  (let ((offset-form (arboffset-for-form-fn form)))
    (if (eq (car form) 'arbaddr-offset-sum)
        nil
      `'(:expand (,offset-form)))))

(defthm constraint_kind-arbaddr-offset-special
  (equal (CONSTRAINT_KIND-ARBADDR-OFFSET
          (WELLCONSTRAINED
           (LIST (CONSTRAINT_EXACT (EXPR (E_VAR (PARAMETRIZED->NAME X))
                                         '((FNAME . "<none>")
                                           (LNUM . 0)
                                           (BOL . 0)
                                           (CNUM . 0)))))
           '(:PRECISION_FULL)))
         (make-arbaddr-offset))
  :hints(("Goal" :in-theory (enable constraint_kind-arbaddr-offset
                                    int_constraintlist-arbaddr-offset
                                    int_constraint-arbaddr-offset
                                    expr-arbaddr-offset
                                    expr-arbaddr-offset-aux
                                    expr_desc-arbaddr-offset))))

(defthm arbaddr-in-scope-of-arb
  (implies (and (arbaddr-offset-lte offset0 offset1)
                (arbaddr-offset-lte (arbaddr-offset-sum
                                     (make-arbaddr-offset
                                      :arboffset 1)
                                     offset1)
                                    offset2))
           (arbaddr-in-scope (cons (arbaddr-component-arb
                                    (arbaddr-offset->arboffset
                                     offset1))
                                   arbaddr)
                             arbaddr offset0 offset2))
  :hints(("Goal" :in-theory (enable arbaddr-in-scope
                                    arbaddr-last-nonbase-component
                                    arbaddr-offset-lte
                                    arbaddr-offset-sum
                                    arbaddr-offset-lte-component))))

(defthm exprlist-arbaddr-offset-of-named_exprlist->exprs
  (equal (exprlist-arbaddr-offset (named_exprlist->exprs x))
         (named_exprlist-arbaddr-offset x))
  :hints(("Goal" :in-theory (enable exprlist-arbaddr-offset
                                    named_expr-arbaddr-offset
                                    named_exprlist-arbaddr-offset
                                    named_exprlist->exprs))))

(defun ota-call-syntax-from-*a-form (call)
  (b* (((cons a-fn args) call)
       (ota-fn (intern-in-package-of-symbol
                (concatenate 'string
                             (subseq (symbol-name a-fn)
                                     0 (- (length (symbol-name a-fn)) 5))
                             "*OTA-FN")
                a-fn))
       (args-without (remove-equal 'arbmap args)))
    (xxxjoin 'cons (cons `',ota-fn (append args-without '('nil))))))

(defun ota-bind-free-look-for-appended-match (arg pattern)
  (case-match arg
    (('binary-append sub1 sub2)
     (or (ota-bind-free-look-for-appended-match sub1 pattern)
         (ota-bind-free-look-for-appended-match sub2 pattern)))
    (('cons & sub)
     (ota-bind-free-look-for-appended-match sub pattern))
    (('mv-nth ''2 sub)
     (and (acl2::prefixp pattern sub) `(mv-nth '2 ,sub)))
    (& nil)))

(defmacro ota-call-bind-free (call)
  (let ((form (ota-call-syntax-from-*a-form call)))
    `(bind-free (let ((match (ota-bind-free-look-for-appended-match arbmap ,form)))
                  (and match
                       (not (equal match arbmap))
                       `((arbmap1 . ,match))))
                (arbmap1))))


(encapsulate nil
  (local (in-theory (disable equal-of-ev_normal
                            equal-of-ev_throwing
                            equal-of-ev_error
                            equal-of-continuing
                            equal-of-returning
                            ;; floor mod ??
                            (tau-system))))
  (with-output
    :evisc (:gag-mode (evisc-tuple 3 4 nil nil))
    (std::defret-mutual-generate <fn>-when-arbvals-equiv-in-scope
      :rules (((not (or (:fnname eval_subprogram-*a)
                        (:fnname eval_loop-*a)
                        (:fnname eval_for-*a)))
               
               (:add-hyp (arbvals-equiv-in-scope arbmap arbmap1 arbaddr arboffset
                                                 (arbaddr-offset-sum arboffset (arboffset-for-form <call>)))))
              ((:fnname eval_subprogram-*a)
               (:add-hyp (arbvals-equiv-in-arbaddr-scope arbmap arbmap1 arbaddr)))
              ((:fnname eval_loop-*a)
               (:add-hyp (arbvals-equiv-in-loop-scope arbmap arbmap1 arbaddr
                                                      is_while
                                                      loop-iter loop-idx)))
              ((:fnname eval_for-*a)
               (:add-hyp (arbvals-equiv-in-for-scope arbmap arbmap1 arbaddr
                                                     loop-iter loop-idx)))
              (t (:add-hyp (ota-call-bind-free <call>)))
              (t
               (:add-concl (equal res
                                  (let ((arbmap arbmap1)) <call>))))
              ((:fnname is_val_of_type-*a)
               (:add-keyword
                :hints ((and stable-under-simplificationp
                             '(:expand ((ty-arbaddr-offset ty)
                                        (ty-arbaddr-offset-aux ty)
                                        (type_desc-arbaddr-offset (ty->desc ty))
                                        (CONSTRAINT_KIND-ARBADDR-OFFSET (T_INT->CONSTRAINT (TY->DESC TY)))))))))
              ((:fnname resolve-ty-*a)
               (:add-keyword
                :hints ((and stable-under-simplificationp
                             '(:expand ((ty-arbaddr-offset x)
                                        (ty-arbaddr-offset-aux x)
                                        (type_desc-arbaddr-offset (ty->desc x))
                                        ;; (CONSTRAINT_KIND-ARBADDR-OFFSET (T_INT->CONSTRAINT (TY->DESC TY)))
                                        ))))))
              ((:fnname check_int_constraints-*a)
               (:add-keyword
                :hints ((and stable-under-simplificationp
                             '(:expand ((int_constraintlist-arbaddr-offset constrs)
                                        (INT_CONSTRAINT-ARBADDR-OFFSET (CAR CONSTRS))))))))
              ((:fnname eval_stmt-*a)
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
              ((:fnname eval_lexpr-*a)
               (:add-keyword
                :hints ((and stable-under-simplificationp
                             '(:expand ((lexpr-arbaddr-offset lx)
                                        (lexpr-arbaddr-offset-aux lx)
                                        (lexpr_desc-arbaddr-offset (lexpr->desc lx))))))))
              ((:fnname eval_pattern-*a)
               (:add-keyword
                :hints ((and stable-under-simplificationp
                             '(:expand ((pattern-arbaddr-offset p)
                                        (pattern_desc-arbaddr-offset (pattern->desc p))))))))
              ((:fnname resolve-typed_identifierlist-*a)
               (:add-keyword
                :hints ((and stable-under-simplificationp
                             '(:expand ((typed_identifierlist-arbaddr-offset x)
                                        (typed_identifier-arbaddr-offset (car x))))))))
              ((:fnname resolve-int_constraints-*a)
               (:add-keyword
                :hints ((and stable-under-simplificationp
                             '(:expand ((int_constraintlist-arbaddr-offset x)
                                        (int_constraint-arbaddr-offset (car x))))))))
              ((:fnname eval_expr-*a)
               (:add-keyword
                :hints ((and stable-under-simplificationp
                             '(:expand ((expr-arbaddr-offset e)
                                        (expr-arbaddr-offset-aux e)
                                        (expr_desc-arbaddr-offset (expr->desc e))
                                        (call-arbaddr-offset-aux
                                         (e_call->call (expr->desc e)))
                                        (call-arbaddr-offset
                                         (e_call->call (expr->desc e)))))))))
              ((not (or (:fnname is_val_of_type-*a)
                        (:fnname check_int_constraints-*a)
                        (:fnname eval_catchers-*a)
                        (:fnname eval_stmt-*a)
                        (:fnname eval_lexpr-*a)
                        (:fnname eval_call-*a)
                        (:fnname eval_pattern-*a)
                        (:fnname resolve-ty-*a)
                        (:fnname resolve-typed_identifierlist-*a)
                        (:fnname resolve-int_constraints-*a)
                        (:fnname eval_expr-*a)))
               (:add-keyword
                :hints ((and stable-under-simplificationp
                             (expand-hint-for-arboffset <call>))))))
      :hints ((vl::big-mutrec-default-hint 'eval_expr-*a-fn id t (w state)))
      :mutual-recursion asl-interpreter-mutual-recursion-*a)))

  


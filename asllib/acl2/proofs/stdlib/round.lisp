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

(include-book "../../openers")
(include-book "stdlib")
(include-book "ilog2")
(include-book "misc")
(include-book "centaur/bitops/rational-exponent" :dir :system)
(local (include-book "ast-theory"))

(local (in-theory (disable (tau-system))))

(local (in-theory (disable floor mod expt
                           put-assoc-equal
                           hons-assoc-equal)))





(local
 (encapsulate nil
   (local (include-book "centaur/misc/multiply-out" :dir :system))
   (defthmd floor-in-terms-of-*-2
     (implies (and (rationalp x)
                   (posp y))
              (equal (floor x y)
                     (+ (if (<= (+ y (* 2 y (floor x (* 2 y)))) x) 1 0)
                        (* 2 (floor x (* 2 y))))))
     :hints (("goal" :use ((:instance acl2::floor-bounded-by-/ (x x) (y y))
                           (:instance acl2::floor-bounded-by-/ (x x) (y (* 2 y))))
              :in-theory (e/d (acl2::multiply-out-<
                               acl2::multiply-out-equal)
                              (acl2::floor-bounded-by-/))
              :do-not '(eliminate-destructors
                        fertilize))
             (and stable-under-simplificationp
                  '(:nonlinearp t)))
     :rule-classes ((:definition :controller-alist ((floor nil t))
                     :install-body nil))
     :otf-flg t)))




(defloop roundtowardszero-loop
  :function "RoundTowardsZero"
  :looptype :s_for
  :index-var i
  :local-vars (((v_int acc) "__stdlib_local_acc" (v_int (floor x_pos.val 1)))
               ((v_real x_pos) "__stdlib_local_x_pos"))
  :invariants (and (subprograms-match '("Real")
                                      (global-env->static (env->global env))
                                      (stdlib-static-env))
                   (<= 1 x_pos.val)
                   (not (hons-assoc-equal "__stdlib_local_next" env.local.storage))
                   (equal acc.val (* (expt 2 (+ 1 start))
                                     (floor x_pos.val (expt 2 (+ 1 start)))))
                   (equal end 0)
                   (posp clk))
  :hints ((and stable-under-simplificationp
               '(:expand ((:with floor-in-terms-of-*-2
                           (:free (x i) (floor (v_real->val x) (expt 2 (v_int->val i)))))))))
  
  :prepwork
  ((local (defund same (x y)
            (equal x y)))
  
   (local (defthm acc-equal-to-same
            (b* ((storage (local-env->storage
                           (env->local env)))
                 (acc-look (hons-assoc-equal "__stdlib_local_acc" storage))
                 (acc (cdr acc-look))
                 ((v_int acc)))
              (equal (equal acc.val (* 2 (expt 2 x) y))
                     (same  acc.val (* 2 (expt 2 x) y))))
            :hints(("Goal" :in-theory (enable same)))))

   (local (defthm acc-when-same
            (b* ((storage (local-env->storage
                           (env->local env)))
                 (acc-look (hons-assoc-equal "__stdlib_local_acc" storage))
                 (acc (cdr acc-look))
                 ((v_int acc)))
              (implies (same  acc.val (* 2 (expt 2 x) y))
                       (equal acc.val (* 2 (expt 2 x) y))))
            :hints(("Goal" :in-theory (e/d (same)
                                           (acc-equal-to-same))))))
           

   (local (in-theory (disable default-*-2 default-*-1 default-+-2 default-+-1
                              acl2::hons-assoc-equal-when-atom
                              acl2::expt-with-violated-guards
                              acl2::floor-=-x/y
                              acl2::floor-type-3
                              acl2::mod-minus
                              default-cdr
                              acl2::floor-type-4
                              acl2::floor-type-1
                              not)))

   (local (defthm v_int->val-equal-quote
            (implies (and (syntaxp (quotep v))
                          (val-case x :v_int)
                          (val-p x))
                     (equal (equal (v_int->val x) v)
                            (and (integerp v)
                                 (equal x (v_int v)))))))))




(def-asl-subprogram roundtowardszero-correct
  :function "RoundTowardsZero"
  :args (val)
  :safe-clock (+ 1 (ilog2-safe-clock (abs val.val)))
  :hyps (<= (ilog2-safe-clock (abs val.val)) (expt 2 128))
  :return-values ((v_int (truncate val.val 1)))
  :enable (abs)
  :disable (truncate)
  :prepwork
  (
   (local (in-theory (disable acl2::ilog2-is-ilog2-spec)))
   (local (defthm ilog2-is-rational-exponent
            (implies (and (rationalp x)
                          (< 0 x))
                     (equal (acl2::ilog2 x)
                            (acl2::rational-exponent x)))
            :hints (("goal" :use ((:instance acl2::rational-exponent-unique
                                   (n (acl2::ilog2 x)))
                                  (:instance acl2::ilog2-correct (value x)))
                     :in-theory (disable acl2::exponents-add expt)))))

   (local (defthm rational-exponent-nonneg
            (implies (and (rationalp val)
                          (<= 1 (abs val)))
                     (<= 0 (acl2::rational-exponent val)))
            :hints (("goal" :use ((:instance acl2::rational-exponent-gte-power-of-2
                                   (n 0) (x val)))))
            :rule-classes :linear))

   (local (defthm floor-of-rational-exponent
            (implies (and (rationalp val)
                          (< 0 val))
                     (equal (floor val (expt 2 (acl2::rational-exponent val)))
                            1))
            :hints (("goal" 
                     :in-theory (enable acl2::rational-exponent-in-terms-of-rational-significand-abs))
                    )))

   (local (in-theory (disable acl2::floor-minus)))
  
   (local (defthm floor-neg-of-rational-exponent
            (implies (and (rationalp val)
                          (< val 0))
                     (equal (floor (- val) (expt 2 (acl2::rational-exponent val)))
                            1))
            :hints (("goal" 
                     :in-theory (enable acl2::rational-exponent-in-terms-of-rational-significand-abs))
                    )))
   (local (in-theory (disable abs)))))


(def-asl-subprogram roundup-correct
  :function "RoundUp"
  :args (val)
  :safe-clock (+ 2 (ilog2-safe-clock (abs val.val)))
  :hyps (<= (ilog2-safe-clock (abs val.val)) (expt 2 128))
  :return-values ((v_int (ceiling val.val 1)))
  :enable (abs)
  :prepwork ((local (in-theory (disable abs)))))


(def-asl-subprogram rounddown-correct
  :function "RoundDown"
  :args (val)
  :safe-clock (+ 2 (ilog2-safe-clock (abs val.val)))
  :hyps (<= (ilog2-safe-clock (abs val.val)) (expt 2 128))
  :return-values ((v_int (floor val.val 1)))
  :enable (abs)
  :prepwork ((local (in-theory (disable abs)))))



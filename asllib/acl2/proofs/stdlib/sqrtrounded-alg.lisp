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


(in-package "ACL2")

(include-book "ilog2-alg")
(local (include-book "arithmetic/top-with-meta" :dir :system))
(local (include-book "ihs/quotient-remainder-lemmas" :dir :system))
(local (include-book "centaur/misc/multiply-out" :dir :System))
(local (include-book "std/util/termhints" :dir :system))
(local (in-theory (disable floor mod
                           numerator-/x)))

(define sqrt-rounded-correctness-condition ((x rationalp)
                                            (fracbits posp)
                                            sqrt)
  ;; Our definition of the correctly odd-rounded square root for a nonnegative rational:
  ;; If the input is 0, then the square root is 0. Otherwise, the square root is greater than 0.
  ;; Since it is greater than 0 it can be expressed (uniquely) as 2^exp * mant, where exp is an integer and 1 <= mant < 2. Then:
  ;; a) Mant has at most fracbits+1 significant bits, i.e. mant modulo 2^(- fracbits) = 0
  ;; b) If the square of the sqrt does not exactly equal the input, then
  ;;    the LSB of the mantissa in the given precision is 1, i.e. mant modulo 2^(1-fracbits) = 2^(-fracbits)
  ;; c) Decrementing the mantissa by 2^(-fracbits) yields a value less than the exact square root, and
  ;;    incrementing the mantissa by 2^(-fracbits) yields a value greater than the exact square root.
  ;;    That is, (2^exp * (mant - 2^(-fracbits)))^2 < x < (2^exp * (mant + 2^(-fracbits)))^2.

  ;; We'll first show that this value is unique if it exists.
  ;; We'll then show that the proposed algorithm finds a value that satisfies it.
  :guard (<= 0 x)
  (if (equal x 0)
      (equal sqrt 0)
    (and (rationalp sqrt)
         (< 0 sqrt)
         (let ((exp (rational-exponent sqrt))
               (mant (rational-significand sqrt)))
           (and (equal (mod mant (expt 2 (- fracbits))) 0) ;; a) above
                (or (equal (* sqrt sqrt) x)                ;; b) above
                    (equal (mod mant (expt 2 (- 1 fracbits)))
                           (expt 2 (- fracbits))))
                (let ((sqrt-down (* (expt 2 exp)           ;; c) above
                                    (- mant (expt 2 (- fracbits)))))
                      (sqrt-up (* (expt 2 exp)
                                  (+ mant (expt 2 (- fracbits))))))
                  (and (< (* sqrt-down sqrt-down) x)
                       (< x (* sqrt-up sqrt-up))))))))
  
  ;; Uniqueness argument:

  ;; Lemma 1: If x is an N-bit float, and x+delta / x-delta is the inc/decrement
  ;; to the next/previous N-bit float, then there's no N-bit float strictly
  ;; between x and x+delta / x-delta.

  ;; Lemma 2: If x has its LSB set, then x+delta and x-delta have LSB unset.

  ;; Suppose sqrt1 and sqrt2 satisfy the conditions.  WLOG suppose sqrt1 < sqrt2.
  ;; Cases: x <= sqrt1^2 < sqrt2^2
  ;;        sqrt1^2 < x < sqrt2^2
  ;;        sqrt1^2 < sqrt2^2 <= x
  ;; x <= sqrt1^2 < sqrt2^2:
  ;;        Conditions of sqrt2 imply (sqrt2-delta)^2 < x, so (sqrt2-delta) < sqrt1 < sqrt2.
  ;;        Contradicted by lemma 1.
  ;; sqrt1^2 < x < sqrt2^2:
  ;;        We have (sqrt2-delta)^2 < x < (sqrt1+delta)^2. The only configuration not contradicted by Lemma
  ;;        is (sqrt2-delta) = sqrt1 < (sqrt1+delta) = sqrt2.  But sqrt1 and sqrt2 must have LSBs set,
  ;;        hence this is contradicted by Lemma 2.
  ;; sqrt1^2 < sqrt2^2 <= x:
  ;;        Conditions of sqrt1 imply (sqrt1+delta)^2 > x, so sqrt1 < sqrt2 < sqrt1+delta, contradicted
  ;;        by lemma 1.
  )


(local (defthmd mod-divide-out
         (implies (and (not (equal (rfix y) 0))
                       (syntaxp (not (equal y ''1)))
                       (rationalp x))
                  (equal (mod x y)
                         (* y (mod (/ x y) 1))))
         :hints(("Goal" :in-theory (enable mod rfix)))))

(local
 (define incr-mant ((x rationalp)
                    (fracbits posp)
                    (incr integerp))
   :returns (new-mant
             rationalp :rule-classes :type-prescription
             :hyp (integerp incr))
   (let ((exp (rational-exponent x))
         (mant (rational-significand x)))
     (* (expt 2 exp) (+ mant (* incr (expt 2 (- fracbits))))))
   ///

   (defthm incr-mant-positive
     (implies (and (rationalp x)
                   (< 0 x)
                   (posp fracbits)
                   (integerp incr)
                   (< (- (expt 2 fracbits)) incr))
              (< 0 (incr-mant x fracbits incr)))
     :hints (("goal" :do-not-induct t)
             (and stable-under-simplificationp
                  '(:nonlinearp t)))
     :rule-classes :linear)
                   
   
   (defthmd sqrt-rounded-correctness-condition-in-terms-of-incr-mant
     (equal (sqrt-rounded-correctness-condition x fracbits sqrt)
            (if (equal x 0)
                (equal sqrt 0)
              (and (rationalp sqrt)
                   (< 0 sqrt)
                   (let ((mant (rational-significand sqrt)))
                     (and (equal (mod mant (expt 2 (- fracbits))) 0)
                          (or (equal (* sqrt sqrt) x)
                              (equal (mod mant (expt 2 (- 1 fracbits)))
                                     (expt 2 (- fracbits))))
                          (let ((sqrt-down (incr-mant sqrt fracbits -1)))
                            (< (* sqrt-down sqrt-down) x))
                          (let ((sqrt-up (incr-mant sqrt fracbits 1)))
                            (< x (* sqrt-up sqrt-up))))))))
     :hints(("Goal" :in-theory (enable incr-mant
                                       sqrt-rounded-correctness-condition))))


   (local (defund rational-significand-int (x fracbits)
            (ifix (* (rational-significand x) (expt 2 fracbits)))))

   (local (defthmd rational-significand-in-terms-of-int
            (implies (integerp (* (rational-significand x)
                                  (expt 2 fracbits)))
                     (equal (rational-significand x)
                            (* (expt 2 (- fracbits))
                               (rational-significand-int x fracbits))))
            :hints(("Goal" :in-theory (enable rational-significand-int)))))

   (local (defthm rational-significand-int-bounds
            (implies (and (integerp (* (rational-significand x)
                                       (expt 2 fracbits)))
                          (rationalp x)
                          (< 0 x))
                     (and (<= (expt 2 fracbits) (rational-significand-int x fracbits))
                          (< (rational-significand-int x fracbits) (* 2 (expt 2 fracbits)))))
            :hints(("Goal" :in-theory (enable rational-significand-int)))
            :rule-classes :linear))
   
   (local (defthm increment-significand-not-greater-than-next-exponent
            (implies (and (integerp (* (rational-significand x)
                                       (expt 2 fracbits)))
                          (rationalp x)
                          (< 0 x)
                          (posp fracbits))
                     (<= (+ 1 (* (rational-significand x)
                                 (expt 2 fracbits)))
                         (* 2 (expt 2 fracbits))))
            :hints(("Goal" :in-theory (enable rational-significand-in-terms-of-int)
                    :do-not-induct t))
            :rule-classes :linear))

   (defthm incr-mant-plus1-sized
     (implies (and (rationalp x)
                   (< 0 x)
                   (posp fracbits)
                   (integerp (* (rational-significand x)
                                (expt 2 fracbits))))
              (integerp (* (rational-significand (incr-mant x fracbits 1))
                           (expt 2 fracbits))))
     :hints(("Goal" :in-theory (enable rational-sign
                                       exponents-add-unrestricted)
             :use ((:instance rational-sign-significand-exponent-unique
                    (exponent (rational-exponent x))
                    (sign 1)
                    (significand (+ (rational-significand x) (expt 2 (- fracbits)))))
                   (:instance rational-sign-significand-exponent-unique
                    (exponent (+ 1 (rational-exponent x)))
                    (sign 1)
                    (significand (/ (+ (rational-significand x) (expt 2 (- fracbits))) 2))))
             :do-not-induct t)
            (and stable-under-simplificationp
                 '(:nonlinearp t)))
     :otf-flg t)

   (defthm incr-mant-plus1-sized2
     (implies (and (rationalp x)
                   (< 0 x)
                   (posp fracbits)
                   (integerp (* (rational-significand x)
                                (expt 2 fracbits))))
              (integerp (* (expt 2 fracbits)
                           (rational-significand (incr-mant x fracbits 1)))))
     :hints (("goal" :use incr-mant-plus1-sized
              :in-theory (disable incr-mant-plus1-sized
                                  incr-mant))))

   (defthm incr-mant-minus1-sized
     (implies (and (rationalp x)
                   (< 0 x)
                   (posp fracbits)
                   (integerp (* (rational-significand x)
                                (expt 2 fracbits))))
              (integerp (* (rational-significand (incr-mant x fracbits -1))
                           (expt 2 fracbits))))
     :hints(("Goal" :in-theory (enable rational-sign
                                       exponents-add-unrestricted)
             :use ((:instance rational-sign-significand-exponent-unique
                    (exponent (rational-exponent x))
                    (sign 1)
                    (significand (- (rational-significand x) (expt 2 (- fracbits)))))
                   (:instance rational-sign-significand-exponent-unique
                    (exponent (+ -1 (rational-exponent x)))
                    (sign 1)
                    (significand (* 2 (- (rational-significand x) (expt 2 (- fracbits)))))))
             :do-not-induct t)
            (and stable-under-simplificationp
                 '(:nonlinearp t)))
     :otf-flg t)

   (defthm incr-mant-minus1-sized2
     (implies (and (rationalp x)
                   (< 0 x)
                   (posp fracbits)
                   (integerp (* (rational-significand x)
                                (expt 2 fracbits))))
              (integerp (* (expt 2 fracbits)
                           (rational-significand (incr-mant x fracbits -1)))))
     :hints (("goal" :use incr-mant-minus1-sized
              :in-theory (disable incr-mant-minus1-sized
                                  incr-mant))))

   (defthm incr-mant-minus1-same-exponent
     (implies (and (rationalp x)
                   (< 0 x)
                   (posp fracbits)
                   (integerp (* (rational-significand x)
                                (expt 2 fracbits)))
                   (not (equal (rational-significand x) 1)))
              (equal (rational-exponent (incr-mant x fracbits -1))
                     (rational-exponent x)))
     :hints(("Goal" :in-theory (enable rational-sign
                                       exponents-add-unrestricted)
             :use ((:instance rational-sign-significand-exponent-unique
                    (exponent (rational-exponent x))
                    (sign 1)
                    (significand (- (rational-significand x) (expt 2 (- fracbits))))))
             :do-not-induct t)
            (and stable-under-simplificationp
                 '(:nonlinearp t)))
     :otf-flg t)

   (defthmd incr-mant-minus1-rational-significand
     (implies (and (rationalp x)
                   (< 0 x)
                   (posp fracbits)
                   (integerp (* (rational-significand x)
                                (expt 2 fracbits)))
                   (not (equal (rational-significand x) 1)))
              (equal (rational-significand (incr-mant x fracbits -1))
                     (- (rational-significand x)
                        (expt 2 (- fracbits)))))
     :hints(("Goal" :in-theory (enable rational-sign
                                       exponents-add-unrestricted)
             :use ((:instance rational-sign-significand-exponent-unique
                    (exponent (rational-exponent x))
                    (sign 1)
                    (significand (- (rational-significand x) (expt 2 (- fracbits))))))
             :do-not-induct t)
            (and stable-under-simplificationp
                 '(:nonlinearp t)))
     :otf-flg t)

   (defthm incr-mant-plus1-same-exponent
     (implies (and (rationalp x)
                   (< 0 x)
                   (posp fracbits)
                   (integerp (* (rational-significand x)
                                (expt 2 fracbits)))
                   (not (equal (rational-significand x)
                               (- 2 (expt 2 (- fracbits))))))
              (equal (rational-exponent (incr-mant x fracbits 1))
                     (rational-exponent x)))
     :hints(("Goal" :in-theory (enable rational-sign
                                       exponents-add-unrestricted)
             :use ((:instance rational-sign-significand-exponent-unique
                    (exponent (rational-exponent x))
                    (sign 1)
                    (significand (+ (rational-significand x) (expt 2 (- fracbits))))))
             :do-not-induct t)
            (and stable-under-simplificationp
                 '(:nonlinearp t)))
     :otf-flg t)

   (local
    (defthm incr-mant-plus1-same-exponent-internal
      (implies (and (rationalp x)
                    (< 0 x)
                    (posp fracbits)
                    (integerp (* (rational-significand x)
                                 (expt 2 fracbits)))
                    (not (equal (rational-significand-int x fracbits)
                                (1- (* 2 (expt 2 fracbits))))))
               (equal (rational-exponent (incr-mant x fracbits 1))
                      (rational-exponent x)))
      :hints(("Goal" :use incr-mant-plus1-same-exponent
              :in-theory (e/d (rational-significand-int)
                              (incr-mant-plus1-same-exponent
                               incr-mant))
              :do-not-induct t))
      :otf-flg t))

   ;; (local (defstub y () nil))
   ;; (local (defund yy () (if (and (rationalp (y)) (< 0 (y)))
   ;;                          (y)
   ;;                        1)))
   
   (local (defthmd y-in-terms-of-rational-significand
            (implies (and (syntaxp (and (or (equal y 'y)
                                            (equal y 'x))
                                        (not (quotep z))))
                          (rationalp y)
                          (< 0 y))
                     (and (equal (< z y)
                                 (< z (* (expt 2 (rational-exponent y))
                                         (rational-significand y))))
                          (equal (< y z)
                                 (< (* (expt 2 (rational-exponent y))
                                       (rational-significand y))
                                    z))))
            :hints(("Goal" :in-theory (enable rational-significand-in-terms-of-rational-exponent
                                              rational-sign)))))
   ;; (local (in-theory (disable y)))


   (local (in-theory (disable incr-mant)))
   
   (defthm incr-mant-minus1-none-between
     (implies (and (rationalp x)
                   ;; (< 0 x)
                   (integerp (* (rational-significand x)
                                (expt 2 fracbits)))
                   (posp fracbits)
                   (rationalp y)
                   (< 0 y)
                   (integerp (* (rational-significand y)
                                (expt 2 fracbits)))
                   (< y x)
                   ;; Special case: if x is an exact power of 2 then the decrement
                   ;; is the next-to-highest significand of the previous exponent
                   (< 1 (rational-significand x)))
              (<= y (incr-mant x fracbits -1)))
     :hints (("goal" :in-theory (e/d ()
                                     (incr-mant
                                      incr-mant-positive))
              :do-not-induct t)
             (and stable-under-simplificationp
                  '(:in-theory (e/d ()
                                    (incr-mant))
                    :use ((:instance rational-exponent-monotonic
                           (x (incr-mant x fracbits -1))
                           (y y))
                          (:instance rational-exponent-monotonic
                           (x y)
                           (y x)))))
             (and stable-under-simplificationp
                  '(:in-theory '(;; x-in-terms-of-rational-significand
                                 y-in-terms-of-rational-significand)
                    :cases ((< 0 x))))
             (and stable-under-simplificationp
                  '(:in-theory (enable rational-significand-in-terms-of-int
                                       divide-out-common-factors-<
                                       incr-mant)))
             )
     :otf-flg t)

   (local (in-theory (disable incr-mant-plus1-same-exponent
                              incr-mant-plus1-same-exponent-internal)))


   (local (defthmd rewrite-expt-of-rational-exponent-whenplus1
            (implies (equal (rational-exponent y) (+ 1 (rational-exponent x)))
                     (Equal (expt 2 (rational-exponent y))
                            (* 2 (expt 2 (rational-exponent x)))))
            :hints (("goal" :use ((:instance exponents-add (r 2) (i 1) (j (rational-exponent x))))
                     :in-theory (disable exponents-add)))))
                              
   (defthm incr-mant-plus1-none-between
     (implies (and (rationalp x)
                   (< 0 x)
                   (integerp (* (rational-significand x)
                                (expt 2 fracbits)))
                   (posp fracbits)
                   (rationalp y)
                   (integerp (* (rational-significand y)
                                (expt 2 fracbits)))
                   (< x y)
                   ;; Special case: if x is an exact power of 2 then the decrement
                   ;; is the next-to-highest significand of the previous exponent
                   (< 1 (rational-significand x)))
              (<= (incr-mant x fracbits 1) y))
     :hints (("goal" :in-theory (e/d ()
                                     (incr-mant
                                      incr-mant-positive))
              :do-not-induct t)
             (and stable-under-simplificationp
                  '(:in-theory (e/d ()
                                    (incr-mant))
                    :use ((:instance rational-exponent-monotonic
                           (x x)
                           (y y))
                          (:instance rational-exponent-monotonic
                           (x y)
                           (y (incr-mant x fracbits 1)))
                          incr-mant-plus1-same-exponent-internal)
                    :cases ((equal (rational-exponent y) (rational-exponent x)))))
             (and stable-under-simplificationp
                  '(:in-theory '(;; x-in-terms-of-rational-significand
                                 y-in-terms-of-rational-significand)
                    :cases ((< 0 y))))
             (and stable-under-simplificationp
                  '(:in-theory (enable rational-significand-in-terms-of-int
                                       divide-out-common-factors-<
                                       incr-mant
                                       rewrite-expt-of-rational-exponent-whenplus1)))
             )
     :otf-flg t)


   

   (local (defthmd mod-1-of-nonneg-exponent
            (implies (natp n)
                     (equal (mod (expt 2 n) 1) 0))))

   (local (defthm mod-1-of-half-positive-exponent
            (implies (posp n)
                     (equal (mod (* 1/2 (expt 2 n)) 1) 0))
            :hints (("goal" :use ((:instance mod-1-of-nonneg-exponent
                                   (n (+ -1 n))))
                     :in-theory (enable exponents-add-unrestricted)))))

   (local (defthm equal-times-2-mod
            (implies (equal (* 2 (mod x 1)) 1)
                     (equal (mod x 1) 1/2))))
   
   (defthm low-bit-of-incr-mant-minus1
     (implies (and (rationalp x)
                   (< 0 x)
                   (posp fracbits)
                   (integerp (* (rational-significand x)
                                (expt 2 fracbits)))
                   (equal (* (expt 2 fracbits)
                             (mod (rational-significand x) (expt 2 (- 1 fracbits))))
                          1))
              (equal (mod (rational-significand (incr-mant x fracbits -1))
                          (expt 2 (+ 1 (- fracbits))))
                     0))
     :hints (("goal" :in-theory (e/d (mod-divide-out
                                      exponents-add-unrestricted)
                                     (mod-=-0))
              :use (incr-mant-minus1-rational-significand)
              :do-not-induct t)
             (and stable-under-simplificationp
                  '(:use ((:instance mod-+
                           (z 1)
                           (x 1/2)
                           (y (* 1/2 (rational-significand x) (expt 2 fracbits)))))
                    :in-theory (disable SIMPLIFY-MOD-+-MOD)))))))


(defsection sqrt-rounded-unique
  
  
  (local (defthm exact-square-root-unique
           (implies (and (rationalp sqrt1)
                         (<= 0 sqrt1)
                         (rationalp sqrt2)
                         (<= 0 sqrt2)
                         (not (equal sqrt1 sqrt2)))
                    (not (equal (* sqrt1 sqrt1)
                                (* sqrt2 sqrt2))))
           :hints (("goal" :cases ((< sqrt1 sqrt2)))
                   (and stable-under-simplificationp
                        '(:nonlinearp t)))))

  (local (defthmd mod-divide-out
           (implies (and (not (equal (rfix y) 0))
                         (syntaxp (not (equal y ''1)))
                         (rationalp x))
                    (equal (mod x y)
                           (* y (mod (/ x y) 1))))
           :hints(("Goal" :in-theory (enable mod rfix)))))
  
  (local (defthm mod-1-of-nonpos-exp
           (implies (and (integerp e)
                         (<= e 0))
                    (equal (mod 1 (expt 2 e)) 0))
           :hints(("Goal" :in-theory (enable mod-divide-out)))))
  
  (local (defthm mod-1-of-expt-fracbits-minus-1
           (implies (posp fracbits)
                    (equal (mod 1 (expt 2 (+ 1 (- fracbits))))
                           0))))
  
  (local (in-theory (disable incr-mant-plus1-none-between
                             incr-mant-minus1-none-between)))

  (local (defstub foo () nil))
  
  (local
   (defthm sqrt-rounded-correctness-condition-unique-one-direction
     (implies (and (rationalp x)
                   (<= 0 x)
                   (posp fracbits)
                   (sqrt-rounded-correctness-condition x fracbits sqrt1)
                   (sqrt-rounded-correctness-condition x fracbits sqrt2))
              (not (< sqrt1 sqrt2)))
     :hints (("goal" :in-theory (enable sqrt-rounded-correctness-condition-in-terms-of-incr-mant)
              :do-not-induct t)
             (acl2::use-termhint
              (cond ((<= x (* sqrt1 sqrt1))
                     '(:computed-hint-replacement
                       ((and stable-under-simplificationp
                             '(:nonlinearp t)))
                       :use ((:instance mark-clause-is-true (x '(<= sqrt1^2 x)))
                             (:instance incr-mant-minus1-none-between
                              (x sqrt2) (y sqrt1)))
                       :in-theory (disable incr-mant-minus1-none-between)))
                    ((<= (* sqrt2 sqrt2) x)
                     '(:computed-hint-replacement
                       ((and stable-under-simplificationp
                             '(:nonlinearp t)))
                       :use ((:instance mark-clause-is-true (x '(sqrt2^2 <= x)))
                             (:instance incr-mant-plus1-none-between
                              (x sqrt1) (y sqrt2)))
                       :in-theory (disable incr-mant-plus1-none-between)))
                    (t (let ((sqrt2-delta (incr-mant sqrt2 fracbits -1))
                             (sqrt1+delta (incr-mant sqrt1 fracbits 1)))
                         (cond ((< sqrt2-delta sqrt1)
                                '(:use ((:instance mark-clause-is-true (x '(sqrt1^2 < x < sqrt2^2 sqrt2-delta < sqrt1)))
                                        (:instance incr-mant-minus1-none-between
                                         (x sqrt2) (y sqrt1)))))
                               ((< sqrt1+delta sqrt2)
                                `(:computed-hint-replacement
                                  ((and stable-under-simplificationp
                                        '(:nonlinearp t)))
                                  :use ((:instance mark-clause-is-true (x '(sqrt1^2 < x < sqrt2^2 sqrt1+delta < sqrt2)))
                                        (:instance incr-mant-minus1-none-between
                                         (x sqrt2) (y ,(hq sqrt1+delta))))))
                               ((< sqrt2 sqrt1+delta)
                                '(:use ((:instance mark-clause-is-true (x '(sqrt1^2 < x < sqrt2^2 sqrt2 < sqrt1+delta)))
                                        (:instance incr-mant-plus1-none-between
                                         (x sqrt1) (y sqrt2)))))
                               ((< sqrt1 sqrt2-delta)
                                `(:computed-hint-replacement
                                  ((and stable-under-simplificationp
                                        '(:nonlinearp t)))
                                  :use ((:instance mark-clause-is-true (x '(sqrt1^2 < x < sqrt2^2 sqrt1 < sqrt2-delta)))
                                        (:instance incr-mant-plus1-none-between
                                         (x sqrt1) (y ,(hq sqrt2-delta))))))
                               (t
                                '(:use ((:instance mark-clause-is-true (x '(sqrt1^2 < x < sqrt2^2))))))))))))
     :rule-classes nil
     :otf-flg t))

  (local (defthm sqrt-rounded-correctness-condition-implies-rational-sqrt
           (implies (sqrt-rounded-correctness-condition x fracbits sqrt)
                    (rationalp sqrt))
           :hints(("Goal" :in-theory (enable sqrt-rounded-correctness-condition)))
           :rule-classes :forward-chaining))
  
  (defthm sqrt-rounded-correctness-condition-unique
    (implies (and (rationalp x)
                  (<= 0 x)
                  (posp fracbits)
                  (sqrt-rounded-correctness-condition x fracbits sqrt1)
                  (sqrt-rounded-correctness-condition x fracbits sqrt2))
             (equal sqrt1 sqrt2))
    :hints (("goal" :use (sqrt-rounded-correctness-condition-unique-one-direction
                          (:instance
                           sqrt-rounded-correctness-condition-unique-one-direction
                           (sqrt1 sqrt2) (sqrt2 sqrt1)))))
    :rule-classes nil))






(local (in-theory (disable ilog2-is-ilog2-spec)))


;;     for n = 1 to fracbits - 1 do
;;         prec = prec / 2.0;
;;         if (root + prec) ^ 2 <= frac then
;;             root = root + prec;
;;         end;
;;     end;          
(define sqrtrounded-loop ((frac rationalp)
                          (root rationalp)
                          (prec rationalp)
                          (n natp)
                          (fracbits posp))
  :measure (nfix (- (ifix fracbits) (ifix n)))
  :returns (mv (new-root rationalp :rule-classes :type-prescription
                         :hyp (and (rationalp root) (rationalp prec)))
               (new-prec rationalp :rule-classes :type-prescription
                         :hyp (and (rationalp root) (rationalp prec))))
  :hints(("Goal" :in-theory (enable nfix)))
  (if (mbt (and (integerp n) (integerp fracbits))) ;; termination
      (if (<= n (- fracbits 1))
          (let* ((prec (/ prec 2))
                 (root (if (<= (expt (+ root prec) 2) frac)
                           (+ root prec)
                         root))
                 (n (+ 1 n)))
            (sqrtrounded-loop frac root prec n fracbits))
        (mv root prec))
    (mv 1 1))
  ///

  (defret <fn>-root-positive
    (implies (and (< 0 root)
                  (< 0 prec))
             (< 0 new-root))
    :rule-classes :type-prescription)
  
  (defret <fn>-prec-nonzero
    (implies (and (rationalp prec)
                  (not (equal prec 0)))
             (not (equal new-prec 0)))
    :rule-classes :type-prescription)
  
  ;; (defret <fn>-prec-result
  ;;   (implies (and (integerp n) (integerp fracbits))
  ;;            (equal new-prec
  ;;                   (* prec (expt 2 (nfix (- fracbits n)))))))
    
  (defret sqrtrounded-loop-in-range
    (implies (and (rationalp frac)
                  (< 0 frac)
                  (integerp n)
                  (integerp fracbits)
                  (rationalp prec)
                  (< 0 prec)
                  (rationalp root)
                  (<= 0 root)
                  (<= (* root root) frac)
                  (< frac (* (+ root prec)
                             (+ root prec))))
             (and (<= (* new-root new-root) frac)
                  (< frac (* (+ new-root new-prec)
                             (+ new-root new-prec)))))
    :rule-classes :linear)
  
  (defret <fn>-precision
    (implies (and (integerp n)
                  (integerp fracbits)
                  (<= n fracbits)
                  (equal prec (expt 2 (- 1 n))))
             (equal new-prec (expt 2 (- 1 fracbits))))
    :hints (("goal" :induct t
             :expand ((:free (x) (expt 2 (+ 1 x)))))))



  (local (in-theory (disable (force))))
  
  (local
   (defret <fn>-root-mod-prec-lemma
     (implies (and (rationalp root)
                   (rationalp prec)
                   (< 0 prec)
                   (equal (mod root prec) 0))
              (equal (mod new-root new-prec) 0))))

  (defret <fn>-root-mod-prec
    (implies (and (rationalp root)
                  (rationalp prec)
                  (equal new-prec1 new-prec)
                  (< 0 prec)
                  (equal (mod root prec) 0))
             (equal (mod new-root new-prec1) 0)))


  (defret <fn>-significant-bits
    (implies (and (rationalp root)
                  (rationalp prec)
                  (< 0 prec)
                  (integerp n)
                  (integerp fracbits)
                  (<= n fracbits)
                  (equal prec (expt 2 (- 1 n)))
                  (equal (mod root prec) 0))
             (and
              (integerp (* (expt 2 fracbits) new-root))
              (integerp (* 1/2 (expt 2 fracbits) new-root))))
    :hints (("goal" :use <fn>-root-mod-prec-lemma
             :in-theory (e/d (exponents-add-unrestricted)
                             (<fn>-root-mod-prec
                              <fn>-root-mod-prec-lemma))
             :do-not-induct t)))

  (defret <fn>-root-range
    (implies (and (rationalp root)
                  (rationalp prec)
                  (< 0 prec)
                  (integerp n)
                  (integerp fracbits))
             (and (<= root new-root)
                  (<= new-root (+ root prec (- new-prec)))))
    :rule-classes
    ((:linear :trigger-terms ((mv-nth 0 (sqrtrounded-loop frac root prec n fracbits))))))

  )


(local (defthm integerp-of-half-2
         (implies (and (integerp x)
                       (not (integerp (* 1/2 x))))
                  (integerp (+ -1/2 (* 1/2 x))))
         :hints (("goal" :use ((:instance mod-=-0
                                (x x) (y 2))
                               (:instance mod-=-0
                                (x (+ 1 x)) (y 2)))
                  :in-theory (disable mod-=-0)))))


(local (defthm ilog2-is-rational-exponent
         (implies (and (rationalp x)
                       (< 0 x))
                  (equal (ilog2 x)
                         (rational-exponent x)))
         :hints (("goal" :use ((:instance rational-exponent-unique
                                (n (ilog2 x)))
                               (:instance ilog2-correct (value x)))
                  :expand ((expt 2 (+ 1 (ilog2 x))))
                  :in-theory (disable exponents-add)))))
  


(define sqrtrounded-normalize ((value rationalp))
  :guard (< 0 value)
  :returns (mv (frac rationalp
                     :hyp (rationalp value)
                     :rule-classes :type-prescription)
               (exp integerp :rule-classes :type-prescription))
  (let* ((exp (ilog2 value))
         (frac (/ value (expt 2 exp))))
    (if (eql (mod exp 2) 0)
        (mv frac exp)
      (mv (* 2 frac)
          (+ exp -1))))
  ///
  (defret <fn>-frac-range
    (implies (and (rationalp value)
                  (< 0 value))
             (and (<= 1 frac)
                  (< frac 4)))
    :hints (("goal" :use ((:instance rational-exponent-correct-positive
                           (x value)))
             :in-theory (disable rational-exponent-correct-positive)))
    :rule-classes :linear)

  (defret <fn>-frac-positive
    (implies (and (rationalp value)
                  (< 0 value))
             (< 0 frac))
    :hints(("Goal" :in-theory (disable <fn>)))
    :rule-classes :type-prescription)

  (defret <fn>-exp-even
    (integerp (* 1/2 exp)))

  (defret <fn>-correct
    (implies (and (rationalp value)
                  (< 0 value))
             (equal (* frac (expt 2 exp)) value))
    :hints (("goal" :in-theory (enable exponents-add-unrestricted))))

  (std::defretd <fn>-correct2
    (implies (and (rationalp value)
                  (< 0 value))
             (equal (expt 2 exp) (/ value frac)))
    :hints(("Goal" :use <fn>-correct
            :in-theory (disable <fn> <fn>-correct)))))

(local (defthm rational-significand-of-in-range 
         (implies (and (rationalp x)
                       (<= 1 x)
                       (< x 2))
                  (equal (rational-significand x) x))))

(local (defthm rational-exponent-of-in-range
         (implies (and (rationalp x)
                       (<= 1 x)
                       (< x 2))
                  (equal (rational-exponent x) 0))))


(define sqrtrounded-root ((frac rationalp)
                           (fracbits posp))
  :guard (and (<= 1 frac)
              (< frac 4))
  :returns (root rationalp :rule-classes :type-prescription)
  (mv-let (root ign)
    (sqrtrounded-loop frac 1 1 1 fracbits)
    (declare (ignore ign))
    (let ((prec (expt 2 (- fracbits))))
      (if (< (expt root 2) frac)
          (+ root prec)
        root)))
  ///
  (defret <fn>-type
    (implies (and (rationalp frac)
                  (< 0 frac))
             (< 0 root))
    :rule-classes :type-prescription)

  (defret <fn>-lower-bound
    (implies (and (rationalp frac)
                  (posp fracbits)
                  (<= 1 frac))
             (<= 1 root))
    :rule-classes :linear)

  (defret <fn>-upper-bound
    (implies (and (rationalp frac)
                  (posp fracbits)
                  (< frac 4))
             (< root 2))
    :hints (("goal" :do-not-induct t
             :expand ((expt 2 (- fracbits)))
             :in-theory (disable expt-minus)))
    :rule-classes :linear)

  (local (defthm integerp-+
           (implies (and (integerp x)
                         (integerp y))
                    (integerp (+ x y)))))
  
  (defret <fn>-significant-bits
    (implies (and (rationalp frac)
                  (posp fracbits))
             (integerp (* (expt 2 fracbits) root)))
    :hints(("Goal" :in-theory (enable exponents-add-unrestricted)
            :do-not-induct t)))
  
  (defret <fn>-rounded-to-odd
    (implies (and (rationalp frac)
                  (<= 1 frac)
                  (< frac 4)
                  (posp fracbits)
                  (not (equal (* root root) frac)))
             (equal (mod root (expt 2 (+ 1 (- fracbits))))
                    (expt 2 (- fracbits))))
    :hints (("goal" :use ((:instance sqrtrounded-loop-in-range
                           (n 1) (prec 1) (root 1)))
             :in-theory (e/d (exponents-add-unrestricted)
                             (sqrtrounded-loop-in-range)))))

  (defret <fn>-is-in-range
    (implies (and (rationalp frac)
                  (<= 1 frac)
                  (< frac 4)
                  (posp fracbits))
             (and (< (* (- root (expt 2 (- fracbits)))
                        (- root (expt 2 (- fracbits))))
                     frac)
                  (< frac
                     (* (+ root (expt 2 (- fracbits)))
                        (+ root (expt 2 (- fracbits)))))))
    :hints (("goal" :use ((:instance sqrtrounded-loop-in-range
                           (n 1) (prec 1) (root 1)))
             :in-theory (e/d (exponents-add-unrestricted)
                             (sqrtrounded-loop-in-range
                              multiply-out-<)))
            (and stable-under-simplificationp
                 '(:nonlinearp t)))
    :rule-classes nil))




;; // SqrtRounded()
;; // Compute square root of VALUE with FRACBITS of precision, rounding inexact values to Odd

;; func SqrtRounded(value : real, fracbits : integer) => real
;; begin
;;     assert value >= 0.0 && fracbits > 0;
;;     if value == 0.0 then return 0.0; end;

;;     // Normalize value to the form 1.nnnn... x 2^exp
;;     var exp : integer = ILog2(value);
;;     var frac : real = value / (2.0 ^ exp);

;;     // Require value = 2.0^exp * frac, where exp is even and 1 <= frac < 4
;;     if exp MOD 2 != 0 then
;;         frac = 2.0 * frac;
;;         exp = exp - 1;
;;     end;

;;     // Set root to sqrt(frac) truncated to fracbits-1 bits
;;     var root = 1.0;
;;     var prec = 1.0;
;;     for n = 1 to fracbits - 1 do
;;         prec = prec / 2.0;
;;         if (root + prec) ^ 2 <= frac then
;;             root = root + prec;
;;         end;
;;     end;
;;     // prec == 2^(1-fracbits)

;;     // Final value of root is odd-rounded to fracbits bits
;;     if root ^ 2 < frac then
;;         root = root + (prec / 2.0);
;;     end;

;;     // Return sqrt(value) odd-rounded to fracbits bits
;;     return (2.0 ^ (exp DIV 2)) * root;
;; end;


(define sqrtrounded ((value rationalp)
                     (fracbits posp))
  :guard (<= 0 value)
  :guard-debug t
  ;;     assert value >= 0.0 && fracbits > 0;
  ;;     if value == 0.0 then return 0.0; end;
  (if (eql value 0)
      0
    ;;     // Normalize value to the form 1.nnnn... x 2^exp
    (let* ((exp (ilog2 value)) ;;     var exp : integer = ILog2(value);
           (frac (/ value (expt 2 exp)))) ;;     var frac : real = value / (2.0 ^ exp);
      (mv-let (frac exp)
        ;;     // Require value = 2.0^exp * frac, where exp is even and 1 <= frac < 4
        ;;     if exp MOD 2 != 0 then
        ;;         frac = 2.0 * frac;
        ;;         exp = exp - 1;
        ;;     end;
        (if (eql (mod exp 2) 0)
            (mv frac exp)
          (mv (* 2 frac)
              (- exp 1)))
        ;;     // Set root to sqrt(frac) truncated to fracbits-1 bits
        (let ((root 1) ;;     var root = 1.0;
              (prec 1) ;;     var prec = 1.0;
              (n 1))
          ;;     for n = 1 to fracbits - 1 do
          ;;         prec = prec / 2.0;
          ;;         if (root + prec) ^ 2 <= frac then
          ;;             root = root + prec;
          ;;         end;
          ;;     end;          
          (mv-let (root prec)
            (sqrtrounded-loop frac root prec n fracbits)
            ;;     // prec == 2^(1-fracbits)

            ;;     // Final value of root is odd-rounded to fracbits bits
            ;;     if root ^ 2 < frac then
            ;;         root = root + (prec / 2.0);
            ;;     end;
            (let ((root (if (< (expt root 2) frac)
                            (+ root (/ prec 2))
                          root)))
              ;;     // Return sqrt(value) odd-rounded to fracbits bits
              ;;     return (2.0 ^ (exp DIV 2)) * root;
              (* (expt 2 (/ exp 2)) root)))))))
  ///

  (local (defthm sqrtrounded-redef
           (implies (posp fracbits)
                    (equal (sqrtrounded value fracbits)
                           (if (eql value 0)
                               0
                             (mv-let (frac exp)
                               (sqrtrounded-normalize value)
                               
                               (let* ((root (sqrtrounded-root frac fracbits)))
                                 (* (expt 2 (/ exp 2)) root))))))
           :hints(("Goal" :in-theory (enable sqrtrounded-normalize
                                             sqrtrounded-root
                                             exponents-add-unrestricted)))))

  (local (in-theory (disable sqrtrounded)))
  

  ;; (local (defthm rational-significand-factor-out-expt
  ;;          (implies (and (rationalp x)
  ;;                        (rationalp y)
  ;;                        (rationalp z)
  ;;                        (not (equal 0 (+ (* x y) z))))
  ;;                   (equal (rational-significand
  ;;                           (+ (* x y (expt 2 n))
  ;;                              (* (expt 2 n) z)))
  ;;                          (rational-significand (+ (* x y) z))))
  ;;          :hints (("goal" :use ((:instance rational-significand-of-expt-2-prod
  ;;                                 (x (+ (* x y) z))))
  ;;                   :in-theory (disable rational-significand-of-expt-2-prod)))))

  ;; (local (defthm rational-significand-factor-out-expt-2
  ;;          (implies (and (rationalp x)
  ;;                        (rationalp z)
  ;;                        (not (equal 0 (+ x z))))
  ;;                   (equal (rational-significand
  ;;                           (+ (* x (expt 2 n))
  ;;                              (* (expt 2 n) z)))
  ;;                          (rational-significand (+ x z))))
  ;;          :hints (("goal" :use ((:instance rational-significand-of-expt-2-prod
  ;;                                 (x (+ x z))))
  ;;                   :in-theory (disable rational-significand-of-expt-2-prod)))))

  ;;   (local (defthm rational-exponent-factor-out-expt
  ;;          (implies (and (rationalp x)
  ;;                        (rationalp y)
  ;;                        (rationalp z)
  ;;                        (not (equal 0 (+ (* x y) z))))
  ;;                   (equal (rational-exponent
  ;;                           (+ (* x y (expt 2 n))
  ;;                              (* (expt 2 n) z)))
  ;;                          (+ (ifix n) (rational-exponent (+ (* x y) z)))))
  ;;          :hints (("goal" :use ((:instance rational-exponent-of-expt-2-prod
  ;;                                 (x (+ (* x y) z))))
  ;;                   :in-theory (disable rational-exponent-of-expt-2-prod)))))

  ;; (local (defthm rational-exponent-factor-out-expt-2
  ;;          (implies (and (rationalp x)
  ;;                        (rationalp z)
  ;;                        (not (equal 0 (+ x z))))
  ;;                   (equal (rational-exponent
  ;;                           (+ (* x (expt 2 n))
  ;;                              (* (expt 2 n) z)))
  ;;                          (+ (ifix n) (rational-exponent (+ x z)))))
  ;;          :hints (("goal" :use ((:instance rational-exponent-of-expt-2-prod
  ;;                                 (x (+ x z))))
  ;;                   :in-theory (disable rational-exponent-of-expt-2-prod)))))

  (local (include-book "centaur/misc/collect-like-terms" :dir :system))
  
  (local (defthm expt-2-half-squared
           (implies (integerp (* 1/2 n))
                    (equal (* (expt 2 (* 1/2 n))
                              (expt 2 (* 1/2 n)))
                           (expt 2 n)))
           :hints (("goal" :use ((:instance exponents-add
                                  (r 2) (i (* 1/2 n)) (j (* 1/2 n))))
                    :in-theory (e/d (collect-like-terms)
                                    (exponents-add))
                    :do-not-induct t))))

  (local (defthm expt-2-half-squared-2
           (implies (integerp (* 1/2 n))
                    (equal (* (expt 2 (* 1/2 n))
                              (expt 2 (* 1/2 n))
                              z)
                           (* (expt 2 n) z)))
           :hints (("goal" :use expt-2-half-squared
                    :in-theory (disable expt-2-half-squared)))))
  

  ;; (local (defthm linear-lemma
  ;;          (implies (and (rationalp x)
  ;;                        (< 0 x))
  ;;                   (< 0 (+ (expt 2 a)
  ;;                           (* (expt 2 b)
  ;;                              (expt 2 c)
  ;;                              x))))
  ;;          :hints (("goal" :nonlinearp t))))

  ;; (local (in-theory (disable (force))))

  (local (defthm linear-lemma
           (implies (and (rationalp x)
                         (rationalp y)
                         (<= 1 x)
                         (<= 1 y))
                    (< 1 (* 2 x y)))
           :hints (("goal" :nonlinearp t))
           :rule-classes :linear))
  
  (defthm sqrtrounded-correct
    (implies (and (rationalp value)
                  (<= 0 value)
                  (posp fracbits))
             (sqrt-rounded-correctness-condition
              value fracbits (sqrtrounded value fracbits)))
    :hints(("Goal" :in-theory (enable sqrt-rounded-correctness-condition
                                      sqrtrounded-normalize-correct2))
           (and stable-under-simplificationp
                '(:in-theory (enable divide-out-common-factors-<
                                     collect-like-terms)
                  :use ((:instance sqrtrounded-root-is-in-range
                         (frac (mv-nth 0 (sqrtrounded-normalize value))))))))))


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



(include-book "arithmetic/top" :dir :system)
(include-book "std/util/define" :dir :system)


;; Definition of an integer power of 2 -- x is an integer, and exists n such that (expt 2 n) equals x
(defsection is-power-of-2

  (defun-sk is-power-of-2 (x)
    (exists n
            (and (integerp x)
                 (equal x (expt 2 n)))))

  (in-theory (disable is-power-of-2
                      is-power-of-2-suff))

  (defthm one-is-power-of-2
    (is-power-of-2 1)
    :hints (("goal" :use ((:instance is-power-of-2-suff
                           (x 1) (n 0))))))

  (defthm two-is-power-of-2
    (is-power-of-2 2)
    :hints (("goal" :use ((:instance is-power-of-2-suff
                           (x 2) (n 1))))))

  (defthm double-is-power-of-2
    (implies (is-power-of-2 x)
             (is-power-of-2 (* 2 x)))
    :hints (("goal" :expand ((is-power-of-2 x)
                             (expt 2 (+ 1 (is-power-of-2-witness x))))
             :use ((:instance is-power-of-2-suff
                    (x (* 2 x)) (n (+ 1 (is-power-of-2-witness x))))))))

  (defund is-power-of-2-nat-expt (x)
    (nfix (is-power-of-2-witness x)))
  
  (defthmd is-power-of-2-in-terms-of-nat-expt
    (equal (is-power-of-2 x)
           (and (integerp x)
                (equal x (expt 2 (is-power-of-2-nat-expt x)))))
    :hints(("Goal" :in-theory (enable is-power-of-2
                                      is-power-of-2-nat-expt)
            :use ((:instance is-power-of-2-suff (n (is-power-of-2-nat-expt x))))
            :cases ((integerp (is-power-of-2-witness x)))))
    :rule-classes :definition)
  
  (defthm half-is-power-of-2
    (implies (and (is-power-of-2 x)
                  (not (equal x 1)))
             (is-power-of-2 (* 1/2 x)))
    :hints (("goal" :expand ((is-power-of-2 x)
                             (expt 2 (+ -1 (is-power-of-2-nat-expt x)))
                             (expt 2 (is-power-of-2-nat-expt x)))
             :use ((:instance is-power-of-2-suff
                    (x (* 1/2 x)) (n (+ -1 (is-power-of-2-nat-expt x))))))))
  

  (defthm expt-is-power-of-2
    (implies (natp n)
             (is-power-of-2 (expt 2 n)))
    :hints (("goal" :use ((:instance is-power-of-2-suff (x (expt 2 n)))))))


  (local (defthmd plus-1-less-rewrite
           (implies (and (integerp x) (integerp y)
                         (<= x y))
                    (iff (< (+ 1 x) y)
                         (not (or (equal x y)
                                  (equal (+ 1 x) y)))))))
                             
  
  (defthmd greater-power-of-2-minimum
    (implies (and (is-power-of-2 x)
                  (is-power-of-2 y)
                  (< x y))
             (<= (* 2 x) y))
    :hints (("goal" :use ((:instance expt-is-increasing-for-base>1
                           (r 2) (i (+ 1 (is-power-of-2-nat-expt x)))
                           (j (is-power-of-2-nat-expt y)))
                          (:instance expt-is-increasing-for-base>1
                           (r 2) (i (is-power-of-2-nat-expt y))
                           (j (is-power-of-2-nat-expt x))))
             :in-theory (enable is-power-of-2-in-terms-of-nat-expt plus-1-less-rewrite))))

  (defthm not-power-of-2-when-less-than-1
    (implies (< x 1)
             (not (is-power-of-2 x)))
    :hints(("Goal" :in-theory (enable is-power-of-2-in-terms-of-nat-expt))))

  (defthm is-power-of-2-implies-posp
    (implies (is-power-of-2 x)
             (posp x))
    :hints(("Goal" :in-theory (enable is-power-of-2-in-terms-of-nat-expt)))
    :rule-classes :forward-chaining)
  )



;; while x >= p2 looplimit 2^128 do // i.e. unbounded
;;     p2 = p2 * 2;
;; end;
;; return p2 DIV 2;

;; Ignoring the looplimit for now
(define floor-pow-2-loop ((x posp)
                          (p2 posp))
  :measure (nfix (+ 1 (- (nfix x) (nfix p2))))
  :returns (final-p2)
  (if (mbt (and (posp p2) (posp x))) ;; check types to ensure termination
      (if (>= x p2)
          (let ((p2 (* p2 2)))
            (floor-pow-2-loop x p2))
        p2)
    2) ;; Value when inputs are ill-typed -- doesn't really matter what's here
  ///
  (defret <fn>-is-power-of-2
    (implies (is-power-of-2 p2)
             (is-power-of-2 final-p2)))

  (defret <fn>-lte-x*2
    (implies (and (posp x) (posp p2)
                  (<= p2 (* 2 x)))
             (<= final-p2 (* 2 x)))
    :rule-classes :linear)

  (defret <fn>-greater-than-x
    (implies (and (integerp x) (posp p2))
             (< x final-p2))
    :rule-classes :linear)

  (defret <fn>-posp
    (implies (posp p2)
             (posp final-p2))
    :rule-classes :type-prescription)

    (defret <fn>-divisible-by-2
      (implies (posp (/ p2 2))
               (posp (/ final-p2 2)))
      :rule-classes :type-prescription))


;; // FloorPow2()
;; // ===========
;; // For a strictly positive integer x, returns the largest power of 2 that is
;; // less than or equal to x
;; 
;; func FloorPow2(x : integer) => integer
;; begin
;;     assert x > 0;
;;     var p2 : integer = 2;
;;     while ...   // floor-pow-2-loop

(define floor-pow-2 ((x posp))
  :returns (pow-2 posp :rule-classes :type-prescription)
  (let ((p2 2))
    (/ (floor-pow-2-loop x p2) 2))
  ///

  ;; Returns the largest power of 2 that is less than or equal to x
  ;; is the same as
  ;; 1) returns a power of 2 less than or equal to x
  ;; 2) there does not exist a larger power of 2 that is less than or equal to x

  (defthm floor-pow-2-is-power-of-2
    (is-power-of-2 (floor-pow-2 x)))

  (defthm floor-pow-2-lte-x
    (implies (posp x)
             (<= (floor-pow-2 x) x))
    :rule-classes :linear)

  (defthm floor-pow-2-no-larger-power-of-2
    (implies (and (posp x)
                  (< (floor-pow-2 x) other)
                  (<= other x))
             (not (is-power-of-2 other)))
    :hints (("goal" :use ((:instance greater-power-of-2-minimum
                           (x (floor-pow-2 x))
                           (y other))))))

  ;; Helper lemma for ceil-pow-2 correctness
  (defthm floor-pow-2*2-greater-than-x
    (implies (integerp x)
             (< x (* 2 (floor-pow-2 x))))
    :rule-classes :linear))



;; // CeilPow2()
;; // ==========
;; // For an integer x, returns the smallest power of 2 that is greater or equal
;; // to x.

;; func CeilPow2(x : integer) => integer
;; begin
;;     if x <= 1 then return 1; end;
;;     return FloorPow2(x - 1) * 2;
;; end;

(define ceil-pow-2 ((x integerp))
  :returns (pow-2 posp :rule-classes :type-prescription)
  (if (<= x 1)
      1
    (* 2 (floor-pow-2 (- x 1))))
  ///
  ;; Returns a power of 2 greater than or equal to x
  (defthm ceil-pow-2-is-power-of-2
    (is-power-of-2 (ceil-pow-2 x)))

  (defthm ceil-pow-2-gte-x
    (implies (integerp x)
             (<= x (ceil-pow-2 x)))
    :rule-classes :linear)

  ;; There does not exist a lesser power of 2 that is greater than or equal to x
  (defthm ceil-pow-2-no-smaller-power-of-2
    (implies (and (integerp x)
                  (< other (ceil-pow-2 x))
                  (<= x other))
             (not (is-power-of-2 other)))
    :hints (("goal" :use ((:instance greater-power-of-2-minimum
                           (x (floor-pow-2 (+ -1 x)))
                           (y other)))))))




;; // IsPow2()
;; // ========
;; // Return TRUE if integer X is positive and a power of 2. Otherwise,
;; // return FALSE.

;; func IsPow2(x : integer) => boolean
;; begin
;;     if x <= 0 then return FALSE; end;
;;     return FloorPow2(x) == CeilPow2(x);
;; end;


(define is-pow-2 ((x integerp))
  :returns (is-pow-2)
  (if (<= x 0)
      nil
    (equal (floor-pow-2 x)
           (ceil-pow-2 x)))
  ///
  (local
   (defthm is-pow-2-implies
     (implies (and (integerp x)
                   (is-pow-2 x))
              (is-power-of-2 x))
     :hints (("goal" :use (ceil-pow-2-gte-x
                           floor-pow-2-lte-x)
              :in-theory (disable ceil-pow-2-gte-x
                                  floor-pow-2-lte-x)))))
  
  (local
   (defthm is-pow-2-when-power-of-2
     (implies (and (integerp x)
                   (is-power-of-2 x))
              (is-pow-2 x))
     :hints (("goal" :use ((:instance ceil-pow-2-no-smaller-power-of-2
                            (other x))
                           (:instance floor-pow-2-no-larger-power-of-2
                            (other x)))
              :in-theory (disable ceil-pow-2-no-smaller-power-of-2
                                  floor-pow-2-no-larger-power-of-2)))))

  (local (in-theory (disable is-pow-2)))
  
  (defthm is-pow-2-correct
    (implies (integerp x)
             (iff (is-pow-2 x)
                  (is-power-of-2 x)))))

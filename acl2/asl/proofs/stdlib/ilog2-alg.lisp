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



(include-book "std/basic/arith-equivs" :dir :System)
(include-book "centaur/bitops/rational-exponent" :dir :System)
(local (include-book "arithmetic/top" :dir :system))
(local (include-book "ihs/quotient-remainder-lemmas" :dir :system))
(local (include-book "centaur/misc/multiply-out" :dir :system))

(defsection ilog2-spec

  ;; This uses the Axiom of Choice to find N satisfying the body, given X, if
  ;; one exists (which as we'll show is the case whenever x is positive).
  (defchoose ilog2-spec (n) (x)
    (and (integerp n)
         (<= (expt 2 n) (abs x))
         (< (abs x) (expt 2 (+ 1 n)))))

  ;; A witness for ilog2-spec for positive rationals is rational-exponent
  ;; (defined in the book included above). This means that ilog2-spec always
  ;; returns n that satisfies the conditions (when x is positive).
  (defthm ilog2-spec-satisfied
    (implies (and (rationalp x)
                  (not (equal 0 x)))
             (let ((n (ilog2-spec x)))
               (and (integerp n)
                    (implies (<= 0 x)
                             (<= (expt 2 n) x))
                    (implies (<= x 0)
                             (<= (expt 2 n) (- x)))
                    (< x (expt 2 (+ 1 n)))
                    (< (- x) (expt 2 (+ 1 n))))))
    :hints (("goal" :use ((:instance ilog2-spec (n (rational-exponent x)))
                          rational-exponent-correct-positive
                          rational-exponent-correct-negative)
             :expand ((expt 2 (+ 1 (rational-exponent x))))
             :in-theory (disable rational-exponent-correct-positive
                                 rational-exponent-correct-negative
                                 exponents-add))))

  (defthm ilog2-spec-unique
    (implies (and (rationalp x)
                  (not (equal 0 x))
                  (integerp n)
                  (<= (expt 2 n) (abs x))
                  (< (abs x) (expt 2 (+ 1 n))))
             (equal (ilog2-spec x) n))
    :hints (("goal" :use (ilog2-spec
                          (:instance expt-is-weakly-increasing-for-base>1
                           (r 2) (i (+ 1 (ilog2-spec x))) (j n))
                          (:instance expt-is-weakly-increasing-for-base>1
                           (r 2) (i (+ 1 n)) (j (ilog2-spec x))))
             :in-theory (disable ilog2-spec-satisfied
                                 exponents-add
                                 expt
                                 exponents-add-for-nonneg-exponents
                                 expt-is-weakly-increasing-for-base>1))))

  (defthmd ilog2-spec-is-rational-exponent
    (implies (and (rationalp x)
                  (not (equal 0 x)))
             (equal (ilog2-spec x)
                    (rational-exponent x)))
    :hints (("goal" :use ((:instance rational-exponent-unique
                           (n (ilog2-spec x)))
                          (:instance rational-exponent-unique
                           (n (ilog2-spec x)) (x (- x)))
                          (:instance ilog2-spec-satisfied))
             :in-theory (e/d (exponents-add-unrestricted)
                             (exponents-add expt
                                            ilog2-spec-satisfied))))))



;;         while 2.0 ^ high <= val looplimit 2^128 do
;;             low = high;
;;             high = high * 2;
;;         end;
(define ilog2-search-up ((val rationalp) (low integerp) (high posp))
  :guard (<= 1 val)
  :measure (nfix (- (+ 1 (rational-exponent val)) (nfix high)))
  :returns (mv (new-low integerp :rule-classes :type-prescription
                        :hyp (integerp low))
               (new-high integerp :rule-classes :type-prescription))
  :hints ;; for termination
  (("goal" :use ((:instance rational-exponent-gte-power-of-2
                  (n high) (x val)))
    :in-theory (disable rational-exponent-gte-power-of-2)
    :do-not-induct t))
  
  (if (mbt (and (rationalp val) (<= 1 val) (posp high))) ;; constrain for termination
      (if (<= (expt 2 high) val)
          (let ((low high)
                (high (* 2 high)))
            (ilog2-search-up val low high))
        (mv low high))
    (mv 0 1))
  ///
  (defret ilog2-search-up-correct
    (implies (and (rationalp val)
                  (<= 1 val)
                  (<= (expt 2 low) val)
                  (posp high))
             (and (<= (expt 2 new-low) val)
                  (< val (expt 2 new-high))))
    :rule-classes :linear)

  (defret ilog2-search-up-low<high
    (implies (< low high)
             (< new-low new-high))
    :rule-classes :linear))


;;         while 2.0 ^ low > val looplimit 2^128 do
;;             high = low;
;;             low = low * 2;
;;         end;
(define ilog2-search-down ((val rationalp) (low negp) (high integerp))
  :guard (and (< 0 val) (< val 1))
  :measure (nfix (- (+ 1 (nfix (- (rational-exponent val))))
                    (if (negp low) (- low) 1)))
  :returns (mv (new-low integerp :rule-classes :type-prescription)
               (new-high integerp :rule-classes :type-prescription
                         :hyp (integerp high)))
  :hints ;; for termination
  (("goal" :use ((:instance rational-exponent-less-than-power-of-2
                  (n low) (x val)))
    :in-theory (disable rational-exponent-less-than-power-of-2)
    :do-not-induct t))
  
  (if (mbt (and (rationalp val) (< 0 val) (< val 1) (negp low))) ;; constrain for termination
      (if (> (expt 2 low) val)
          (let ((high low)
                (low (* 2 low)))
            (ilog2-search-down val low high))
        (mv low high))
    (mv -1 0))
  ///
  (defret ilog2-search-down-correct
    (implies (and (rationalp val)
                  (< 0 val)
                  (< val 1)
                  (< val (expt 2 high))
                  (negp low))
             (and (<= (expt 2 new-low) val)
                  (< val (expt 2 new-high))))
    :rule-classes :linear)

  (defret ilog2-search-down-low<high
    (implies (< low high)
             (< new-low new-high))
    :rule-classes :linear))
                  

(local (in-theory (disable floor mod)))




;;    // Binary search between low and high
;;    while low+1 < high looplimit 2^128 do
;;        var mid = (low + high) DIVRM 2;
;;        if 2.0 ^ mid > val then
;;            high = mid;
;;        else
;;            low = mid;
;;        end;
;;    end;

;;    return low;
(define ilog2-binary-search ((val rationalp) (low integerp) (high integerp))
  :guard (and (< 0 val)
              (< low high))
  :measure (nfix (- (ifix high) (ifix low)))
  :returns (mv (new-low integerp :rule-classes :type-prescription)
               (new-high integerp :rule-classes :type-prescription))
  (if (mbt (and (integerp low) (integerp high)))
      (if (< (+ 1 low) high)
          (let ((mid (floor (+ low high) 2)))
            (if (< val (expt 2 mid))
                (let ((high mid))
                  (ilog2-binary-search val low high))
              (let ((low mid))
                (ilog2-binary-search val low high))))
        (mv low high))
    (mv 0 0))
  ///
  (defret ilog2-binary-search-correct
    (implies (and (rationalp val)
                  (< 0 val)
                  (integerp low)
                  (integerp high)
                  (<= (expt 2 low) val)
                  (< val (expt 2 high)))
             (and (<= (expt 2 new-low) val)
                  (< val (* 2 (expt 2 new-low)))))
    :hints(("Goal" :in-theory (enable exponents-add-unrestricted)
            :induct t)
           (and stable-under-simplificationp
                '(:use ((:instance expt-is-weakly-increasing-for-base>1
                         (r 2) (i high) (j (+ 1 low)))))))
    :rule-classes :linear))


;; // ILog2()
;; // Return floor(log2(VALUE))

;; func ILog2(value : real) => integer
;; begin
;;     assert value > 0.0;
;;     var val : real = Abs(value);
;;     var low : integer;
;;     var high : integer;

;;     // Exponential search to find upper/lower power-of-2 exponent range
;;     if val >= 1.0 then
;;         low = 0; high = 1;
;;         while 2.0 ^ high <= val looplimit 2^128 do
;;             low = high;
;;             high = high * 2;
;;         end;
;;     else
;;         low = -1; high = 0;
;;         while 2.0 ^ low > val looplimit 2^128 do
;;             high = low;
;;             low = low * 2;
;;         end;
;;     end;

;;    // Binary search between low and high
;;    while low+1 < high looplimit 2^128 do
;;        var mid = (low + high) DIVRM 2;
;;        if 2.0 ^ mid > val then
;;            high = mid;
;;        else
;;            low = mid;
;;        end;
;;    end;

;;    return low;
;; end;
(define ilog2 ((value rationalp))
  :guard (< 0 value)
  :returns (ilog2 integerp :rule-classes :type-prescription)
  (let ((val (abs value))) ;; no-op given guard
    (mv-let (low high)
      (if (>= val 1)
          (let ((low 0) (high 1))
            (ilog2-search-up val low high))
        (let ((low -1) (high 0))
          (ilog2-search-down val low high)))
      (mv-let (low high)
        (ilog2-binary-search val low high)
        (declare (ignore high))
        low)))
  ///
  (defthm ilog2-correct
    (implies (and (rationalp value)
                  (not (equal 0 value)))
             (and (<= (expt 2 (ilog2 value)) (abs value))
                  (< (abs value) (expt 2 (+ 1 (ilog2 value))))))
    :hints(("Goal" :in-theory (enable exponents-add-unrestricted)))
    :rule-classes :linear)

  (local (in-theory (disable ilog2)))
  
  (defthm ilog2-is-ilog2-spec
    (implies (and (rationalp value)
                  (not (equal 0 value)))
             (equal (ilog2 value)
                    (ilog2-spec value)))
    :hints (("goal" :use ((:instance ilog2-spec-unique
                           (n (ilog2 value))
                           (x value)))))))

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
(include-book "misc")
(include-book "centaur/bitops/logrepeat" :dir :system) ;; for replicate
(include-book "centaur/bitops/trailing-0-count" :dir :system) ;; for lowestsetbit
(local (include-book "ast-theory"))
(local (include-book "centaur/bitops/ihsext-basics" :dir :System))

(local (in-theory (disable (tau-system))))

(local (in-theory (disable floor mod expt
                           put-assoc-equal
                           hons-assoc-equal
                           unsigned-byte-p
                           logmask)))




;; Replicate
;; BitCount
;; LowestSetBit
;; LowestSetBitNZ
;; HighestSetBit
;; HighestSetBitNZ
;; SignExtend
;; Extend
;; CountLeadingZeroBits
;; CountLeadingSignBits



(local (defthm nfix-when-not-negp
         (implies (not (negp x))
                  (equal (nfix x) (ifix x)))
         :hints(("Goal" :in-theory (enable nfix ifix)))))


  
(local (defthmd equal-when-v_int
         (implies (and (val-case x :v_int)
                       (val-case y :v_int)
                       (val-p x) (val-p y))
                  (equal (equal x y)
                         (equal (v_int->val x)
                                (v_int->val y))))
         :hints (("goal" :use ((:instance val-fix-when-v_int (x x))
                               (:instance val-fix-when-v_int (x y)))
                  :in-theory (disable v_int-of-fields
                                      equal-of-v_int)))))

(defloop replicate-1-loop
  :function "Replicate-1"
  :looptype :s_for
  :index-var i
  :local-vars (((v_int m)            "__stdlib_local_M")
               ((v_int items)        "__stdlib_local_items")
               ((v_bitvector result) "__stdlib_local_result"
                (v_bitvector (* items.val m.val)
                             (logrepeat items.val m.val x.val)))
               ((v_bitvector x)      "__stdlib_local_x"))
  :invariants (and (<= 0 start)
                   (equal end (+ -1 items.val))
                   (equal x.len m.val)
                   (equal result.len (* items.val m.val))
                   (equal result.val (logrepeat start m.val x.val)))
  :hints ((and stable-under-simplificationp
               '(:in-theory (enable equal-of-v_int))))

  :prepwork
  ((local (defthm cons-under-iff
            (iff (cons x y) t)))


   (local (defthm bound-lemma
            (implies (and (equal r (* x items))
                          (rationalp x)
                          (integerp i)
                          (integerp items)
                          (< i items)
                          (<= 0 x))
                     (>= r (+ x (* x i))))
            :hints ((and stable-under-simplificationp
                         '(:nonlinearp t)))))

   (local (defthm bound-lemma2
            (implies (and (equal r (* x items))
                          (rationalp x)
                          (integerp i)
                          (integerp items)
                          (< i items)
                          (<= 0 x))
                     (>= r (* x i)))
            :hints ((and stable-under-simplificationp
                         '(:nonlinearp t)))))

   (local (defthm bound-lemma3
            (implies (and (equal r (* x items))
                          (rationalp x)
                          (integerp i)
                          (integerp items)
                          (< i items)
                          (<= 0 x))
                     (>= (+ r (- (* x i))) x))
            :hints ((and stable-under-simplificationp
                         '(:nonlinearp t)))))

   (local (defthm expt-minus-1-to-logmask
            (implies (not (negp n))
                     (equal (+ -1 (expt 2 n))
                            (bitops::logmask n)))
            :hints(("Goal" :in-theory (enable bitops::logmask)))))


   (local (defun bits-ind1 (rl xl rv xv)
            (if (zp xl)
                (list rl rv xv)
              (bits-ind1 (1- rl) (1- xl) (logcdr rv) (logcdr xv)))))

   (local (defthm logand-loghead-neg1
            (equal (logand x (loghead n -1))
                   (loghead n x))
            :hints(("Goal" :in-theory (acl2::enable* bitops::ihsext-inductions
                                                     bitops::ihsext-recursive-redefs)))))
   (local (defthm bits-lemma-lemma
            (implies (unsigned-byte-p xl xv)
                     (EQUAL (LOGIOR (LOGHEAD RL XV)
                                    (LOGAND RV (LOGHEAD RL (LOGNOT (LOGMASK XL)))))
                            (LOGHEAD RL (LOGAPP XL XV (LOGTAIL XL RV)))))
            :hints (("goal" :in-theory (acl2::e/d* (bitops::ihsext-recursive-redefs))
                     :induct (bits-ind1 rl xl rv xv)))))
   (local (defun bits-ind (rl sh rv xv)
            (if (zp sh)
                (list rl rv xv)
              (bits-ind (1- rl) (1- sh) (logcdr rv) xv))))
   (local (defthm bits-lemma
            (implies (and (not (negp sh))
                          (unsigned-byte-p xl xv))
                     (equal (logior (loghead rl (ash xv sh))
                                    (logand rv (loghead rl (lognot (ash (logmask xl) sh)))))
                            (loghead rl (logapp sh rv (logapp xl xv (logtail (+ (nfix sh) (nfix xl)) rv))))))
            :hints(("Goal" :in-theory (acl2::e/d* (bitops::ihsext-recursive-redefs))
                    :induct (bits-ind rl sh rv xv)))))

  
   (local (defthm logtail-of-logrepeat
            (implies (>= (nfix m) (* (nfix n) (nfix w)))
                     (equal (logtail m (logrepeat n w x))
                            0))
            :hints(("Goal" :in-theory (enable logrepeat
                                              bitops::logtail-of-logapp-split))
                   (and stable-under-simplificationp
                        '(:nonlinearp t)))))

   (local (defthm logtail-of-equal-logrepeat
            (implies (and (equal y (logrepeat n w x))
                          (>= (nfix m) (* (nfix n) (nfix w))))
                     (equal (logtail m y)
                            0))))

   (local (defthm logapp-of-logrepeat
            (implies (and (natp n) (natp w))
                     (equal (logapp (* n w) (logrepeat n w x) (logrepeat m w x))
                            (logrepeat (+ n (nfix m)) w x)))
            :hints (("goal" :induct (logrepeat n w x)
                     :in-theory (enable logrepeat bitops::logapp-right-assoc)))))
  
   (local (defthm logapp-of-equal-to-logrepeat
            (implies (and (natp n) (natp w)
                          (equal y (logrepeat n w x))
                          (unsigned-byte-p w x))
                     (equal (logapp (* w n) y x)
                            (logrepeat (+ 1 n) w x)))
            :hints(("Goal" :use ((:instance logapp-of-logrepeat
                                  (n n) (m 1)))
                    :expand ((logrepeat 1 w x))))))))




(def-asl-subprogram replicate-1-correct
  :function "Replicate-1"
  :params (n m)
  :args (x)
  :hyps (and (<= 0 n.val)
             (< 0 m.val)
             (integerp (/ n.val m.val)))
  :return-values ((v_bitvector n.val
                               (logrepeat (/ n.val m.val) m.val x.val)))
  :prepwork
  ((local (defthm logrepeat-is-logmask
            (implies (equal (loghead 1 x) 1)
                     (equal (logrepeat n 1 x) (loghead n -1)))
            :hints(("Goal" :in-theory (enable bitops::loghead**
                                              logrepeat
                                              bitops::logapp**
                                              bitops::logmask**)))))

   (local (defthm logrepeat-is-zero
            (implies (equal (loghead 1 x) 0)
                     (equal (logrepeat n 1 x) 0))
            :hints(("Goal" :in-theory (enable bitops::loghead**
                                              logrepeat
                                              bitops::logapp**)))))

   (local (defthm logrepeat-0
            (equal (logrepeat 0 m x) 0)
            :hints(("Goal" :in-theory (enable logrepeat)))))))


(def-asl-subprogram replicate-correct
  :function "Replicate"
  :params (m n)
  :args (x n)
  :hyps (and (<= 0 n.val)
            (< 0 m.val))
  :return-values ((v_bitvector (* n.val m.val)
                               (logrepeat n.val m.val x.val))))




(local (defthm loghead-1-of-bit
         (implies (bitp x)
                  (equal (loghead 1 x) x))
         :hints(("Goal" :in-theory (enable bitops::loghead**)))))

(defloop BitCount-loop
  :function "BitCount"
  :looptype :s_for
  :index-var i
  :local-vars (((v_int result) "__stdlib_local_result"
                (v_int (logcount (loghead x.len x.val))))
               ((v_bitvector x) "__stdlib_local_x"))
  :invariants (and (equal result.val (logcount (loghead start x.val)))
                   (<= 0 start)
                   (equal end (+ -1 x.len)))
  :hints ((and stable-under-simplificationp
               '(:in-theory (enable equal-when-v_int))))
  :prepwork
  ((local (defthm logcount-loghead-when-logbitp
            (implies (and (logbitp n x)
                          (natp n))
                     (equal (logcount (loghead (+ 1 n) x))
                            (+ 1 (logcount (loghead n x)))))
            :hints(("Goal" :in-theory (acl2::enable* bitops::ihsext-inductions
                                                     bitops::ihsext-recursive-redefs)))))

   (local (defthm logcount-loghead-when-not-logbitp
            (implies (and (not (logbitp n x))
                          (natp n))
                     (equal (logcount (loghead (+ 1 n) x))
                            (logcount (loghead n x))))
            :hints(("Goal" :in-theory (acl2::enable* bitops::ihsext-inductions
                                                     bitops::ihsext-recursive-redefs)))))))


(def-asl-subprogram BitCount-correct
  :function "BitCount"
  :params (n)
  :args (x)
  :return-values ((v_int (logcount x.val)))
  :prepwork
  ((local (defthm logcount-loghead-bound
            (<= (logcount (loghead n x)) (nfix n))
            :hints(("Goal" :in-theory (acl2::enable* bitops::ihsext-inductions
                                                     bitops::ihsext-recursive-redefs)))
            :rule-classes :linear))
   (local (defthm v_bitvector->val-logcount-bound
            (<= (logcount (v_bitvector->val x)) (v_bitvector->len x))
            :hints (("goal" :use ((:instance logcount-loghead-bound
                                   (n (v_bitvector->len x))
                                   (x (v_bitvector->val x))))
                     :in-theory (disable logcount-loghead-bound)))
            :rule-classes :linear))))



(defloop LowestSetBit-loop
  :function "LowestSetBit"
  :looptype :s_for
  :index-var i
  :local-vars (((v_bitvector x) "__stdlib_local_x")
               ((v_int n) "__stdlib_local_N"))
  :invariants (and (equal (loghead start x.val) 0)
                   (equal n.val x.len)
                   (<= 0 start)
                   (equal end (+ -1 x.len)))
  :return-cond (not (equal x.val 0))
  :return-values (list (v_int (bitops::trailing-0-count x.val)))
  :hints ((and stable-under-simplificationp
               '(:in-theory (enable equal-when-v_int))))
  :prepwork
  ((local (defthm loghead-equal-0-when-not-logbitp
            (implies (and (not (logbitp n x))
                          (not (negp n)))
                     (equal (equal (loghead (+ 1 n) x) 0)
                            (equal (loghead n x) 0)))
            :hints(("Goal" :in-theory (acl2::enable* bitops::ihsext-recursive-redefs
                                                     bitops::ihsext-inductions)))))

   (local (defthm trailing-0-count-when-logbitp
            (implies (and (logbitp n x)
                          (equal (loghead n x) 0))
                     (equal (bitops::trailing-0-count x) (nfix n)))
            :hints(("Goal" :in-theory (acl2::enable* bitops::ihsext-recursive-redefs
                                                     bitops::trailing-0-count
                                                     bitops::ihsext-inductions)))))))




(def-asl-subprogram LowestSetBit-correct
  :function "LowestSetBit"
  :params (n)
  :args (x)
  :return-values ((v_int (if (equal x.val 0)
                             n.val
                           (bitops::trailing-0-count x.val))))
  :hints ((and stable-under-simplificationp
               '(:cases ((eql (v_bitvector->val x) 0)))))
  :prepwork
  ((local (defthm loghead-of-ifix-n
            (equal (loghead (ifix n) x)
                   (loghead n x))
            :hints(("Goal" :in-theory (enable ifix)))))

   (local (defthm trailing-0-count-of-loghead
            (implies (not (equal (loghead n x) 0))
                     (Equal (bitops::trailing-0-count (loghead n x))
                            (bitops::trailing-0-count x)))
            :hints(("Goal" :in-theory (acl2::enable* bitops::ihsext-recursive-redefs
                                                     bitops::trailing-0-count
                                                     bitops::ihsext-inductions)))))))

(def-asl-subprogram LowestSetBitNZ-correct
  :function "LowestSetBitNZ"
  :params (n)
  :args (x)
  :error-cond (equal x.val 0)
  :return-values ((v_int (bitops::trailing-0-count x.val)))
  :prepwork
  ((local (defthm trailing-0-count-bound
            (implies (not (equal (loghead n x) 0))
                     (< (bitops::trailing-0-count x) (nfix n)))
            :hints(("Goal" :in-theory (acl2::enable* bitops::ihsext-recursive-redefs
                                                     bitops::trailing-0-count
                                                     bitops::ihsext-inductions)))
            :rule-classes :linear))
   (local (defthm v_bitvector->val-trailing-0-count-bound
            (implies (not (equal (v_bitvector->val x) 0))
                     (< (bitops::trailing-0-count (v_bitvector->val x)) (v_bitvector->len x)))
            :hints(("Goal" :use ((:instance trailing-0-count-bound
                                  (x (v_bitvector->val x)) (n (v_bitvector->len x))))))
            :rule-classes :linear))))



(defloop HighestSetBit-loop
  :function "HighestSetBit"
  :looptype :s_for
  :index-var i
  :local-vars (((v_bitvector x) "__stdlib_local_x")
               ((v_int n) "__stdlib_local_N"))
  :invariants (and (equal (logtail (+ 1 start) x.val) 0)
                   (equal n.val x.len)
                   (< start n.val)
                   (equal end 0))
  :return-cond (not (equal x.val 0))
  :return-values (list (v_int (1- (integer-length x.val))))
  :hints ((and stable-under-simplificationp
               '(:in-theory (enable equal-when-v_int))))
  :prepwork
  ((local (defthm logtail-n-in-terms-of-logbitp
            (implies (equal (logtail (+ 1 n) x) 0)
                     (equal (equal (logtail n x) 0)
                            (not (logbitp n x))))
            :hints(("Goal" :in-theory (acl2::enable* bitops::ihsext-inductions
                                                     bitops::ihsext-recursive-redefs)))))

   (local (defthm integer-length-when-logtail-logbitp
            (implies (and (equal (logtail (+ 1 n) x) 0)
                          (logbitp n x)
                          (natp n))
                     (equal (integer-length x) (+ 1 n)))
            :hints(("Goal" :in-theory (acl2::enable* bitops::ihsext-inductions
                                                     bitops::ihsext-recursive-redefs)))))))

(def-asl-subprogram HighestSetBit-correct
  :function "HighestSetBit"
  :params (n)
  :args (x)
  :return-values ((v_int (+ -1 (integer-length x.val))))
  :hints ((and stable-under-simplificationp
               '(:cases ((equal (v_bitvector->val x) 0))))))


(def-asl-subprogram HighestSetBitNZ-correct
  :function "HighestSetBitNZ"
  :params (n)
  :args (x)
  :error-cond (equal x.val 0)
  :return-values ((v_int (+ -1 (integer-length x.val))))
  :prepwork
  ((local (defthm v_bitvector-integer-length-bound
            (<= (integer-length (v_bitvector->val x))
                (v_bitvector->len x))
            :hints (("goal" :use ((:instance bitops::integer-length-of-loghead-bound
                                   (n (v_bitvector->len x)) (i (v_bitvector->val x))))
                     :in-theory (disable bitops::integer-length-of-loghead-bound)))
            :rule-classes :linear))
  (local (defthm integer-length-equal-0
           (implies (natp x)
                    (equal (equal (integer-length x) 0)
                           (equal x 0)))
           :hints(("Goal" :in-theory (acl2::enable* bitops::ihsext-recursive-redefs
                                                    bitops::ihsext-inductions)))))))


(def-asl-subprogram SignExtend-1-correct
  :function "SignExtend-1"
  :params (n m)
  :args (x)
  :hyps (and (< 0 m.val)
             (<= m.val n.val))
  :return-values ((v_bitvector n.val (logext m.val x.val)))
  :enable (logext)
  :prepwork
  ((local (defthm integer-length-equal-0
            (implies (natp x)
                     (equal (equal (integer-length x) 0)
                            (equal x 0)))
            :hints(("Goal" :in-theory (acl2::enable* bitops::ihsext-recursive-redefs
                                                     bitops::ihsext-inductions)))))

   (local (defthm logrepeat-1
            (equal (logrepeat n 1 1)
                   (loghead n -1))
            :hints(("Goal" :in-theory (acl2::enable* bitops::ihsext-recursive-redefs
                                                     bitops::ihsext-inductions
                                                     logrepeat)))))

   (local (defthm logapp-when-logbitp
            (implies (and (logbitp m x)
                          (natp m)
                          (posp n))
                     (equal (logapp m x (loghead n -1))
                            (logapp (+ 1 m) x (loghead (+ -1 n) -1))))
            :hints(("Goal" :in-theory (acl2::enable* bitops::ihsext-recursive-redefs
                                                     bitops::ihsext-inductions)))))

   (local (defthm logrepeat-0
            (equal (logrepeat m n 0) 0)
            :hints(("Goal" :in-theory (acl2::enable* bitops::ihsext-recursive-redefs
                                                     bitops::ihsext-inductions
                                                     logrepeat)))))

   (local (defthm loghead-when-not-logbitp
            (implies (and (not (logbitp (+ -1 m) x))
                          (natp m))
                     (equal (loghead (+ -1 m) x)
                            (loghead m x)))
            :hints(("Goal" :in-theory (acl2::enable* bitops::ihsext-recursive-redefs
                                                     bitops::ihsext-inductions)))))

   (local (defthm loghead-of-v_bitvector->val
            (implies (>= (nfix n) (v_bitvector->len x))
                     (equal (loghead n (v_bitvector->val x))
                            (v_bitvector->val x)))
            :hints (("goal" :use ((:instance v_bitvector-requirements))
                     :in-theory (e/d (nfix)
                                     (v_bitvector-requirements))))))))


(def-asl-subprogram SignExtend-correct
  :function "SignExtend"
  :params (n m)
  :args (x n)
  :hyps (and (< 0 m.val)
             (<= m.val n.val))
  :return-values ((v_bitvector n.val (logext m.val x.val)))
  :enable (logext))


(def-asl-subprogram Extend-1-correct
  :function "Extend-1"
  :params (n m)
  :args (x unsigned)
  :hyps (and (or unsigned.val
                 (< 0 m.val))
             (<= m.val n.val))
  :return-values ((v_bitvector n.val
                               (if unsigned.val
                                   x.val
                                 (logext m.val x.val)))))


(def-asl-subprogram Extend-correct
  :function "Extend"
  :params (n m)
  :args (x n unsigned)
  :hyps (and (or unsigned.val
                 (< 0 m.val))
             (<= m.val n.val))
  :return-values ((v_bitvector n.val
                               (if unsigned.val
                                   x.val
                                 (logext m.val x.val)))))





(def-asl-subprogram CountLeadingZeroBits-correct
  :function "CountLeadingZeroBits"
  :params (n)
  :args (x)
  :return-values ((v_int (- n.val (integer-length x.val)))))


;; NOTE: This seems intentional, but note the difference between the specs of
;; CountLeadingZeroBits and CountLeadingSignBits: the former counts all the
;; zeros leading up to the most significant 1 bit, whereas the latter counts
;; the leading 0s/1s except for the sign bit.
(def-asl-subprogram CountLeadingSignBits-correct
  :function "CountLeadingSignBits"
  :params (n)
  :args (x)
  :hyps (< 0 n.val)
  :return-values ((v_int (- n.val (+ 1 (integer-length (logext n.val x.val))))))
  :prepwork
  ((local (defthm integer-length-equal-0
            (implies (natp x)
                     (equal (equal (integer-length x) 0)
                            (equal x 0)))
            :hints(("Goal" :in-theory (acl2::enable* bitops::ihsext-recursive-redefs
                                                     bitops::ihsext-inductions)))))


   (local (defthm integer-length-logxor
            (implies (and (posp n)
                          (unsigned-byte-p n x))
                     (equal (integer-length (logxor (logtail 1 x)
                                                    (loghead (+ -1 n) x)))
                            (integer-length (logext n x))))
            :hints(("Goal" :in-theory (acl2::enable* bitops::ihsext-inductions
                                                     bitops::ihsext-recursive-redefs)))))

   (local (defthm logext-when-loghead-0
            (implies (and (equal (loghead n x) 0)
                          (posp n))
                     (Equal (logext n x) 0))
            :hints(("Goal" :in-theory (acl2::enable* bitops::ihsext-inductions
                                                     bitops::ihsext-recursive-redefs)))))))



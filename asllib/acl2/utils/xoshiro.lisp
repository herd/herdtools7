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

(include-book "std/util/define" :dir :system)
(include-book "ihs/logops-definitions" :Dir :system)
(include-book "centaur/misc/tailrec" :dir :system)
(include-book "std/basic/pos-fix" :dir :system)
(include-book "std/stobjs/nicestobj" :dir :system)
(local (include-book "centaur/bitops/ihsext-basics" :Dir :system))
(local (include-book "arithmetic/top" :dir :system))
(local (include-book "system/random" :dir :system))
(local (include-book "ihs/quotient-remainder-lemmas" :dir :system))
(local (include-book "std/lists/update-nth" :dir :system))


(local (in-theory (disable unsigned-byte-p
                           acl2::natp-when-integerp)))



;; static inline uint32_t rotl(const uint32_t x, int k) {
;; 	return (x << k) | (x >> (32 - k));
;; }

(define xoshiro-rotl ((x :type (unsigned-byte 32))
                      (k :type (unsigned-byte 4)))
  :parents (xoshiro)
  :short "Utility function for xoshiro pseudorandom number generation"
  :inline t
  :returns (new-x natp :rule-classes :type-prescription)
  :prepwork ((local (defthm unsigned-byte-4-forward
                      (implies (unsigned-byte-p 4 x)
                               (and (<= 0 x)
                                    (< x 16)))
                      :hints(("Goal" :in-theory (enable unsigned-byte-p)))
                      :rule-classes :forward-chaining))
             (local (defthm loghead-4-bounds
                      (and (<= 0 (loghead 4 x))
                           (< (loghead 4 x) 16))
                      :hints (("goal" :use ((:instance unsigned-byte-p-of-loghead
                                             (i x) (size 4) (size1 4)))
                               :in-theory (e/d (unsigned-byte-p) (unsigned-byte-p-of-loghead))))
                      :rule-classes :linear)))
  (b* ((x (mbe :logic (loghead 32 x)
               :exec x))
       (k (mbe :logic (loghead 4 k)
               :exec k)))
    (the (unsigned-byte 32)
         (logior (the (unsigned-byte 32)
                      (logand #xffffffff
                              (the (unsigned-byte 48)
                                   (ash (the (unsigned-byte 32) x)
                                        (the (integer 0 15) k)))))
                 (the (unsigned-byte 32)
                      (ash (the (unsigned-byte 32) x)
                           (the (integer -15 0)
                                (- (the (integer 0 16) k))))))))
  ///
  (defret width-of-<fn>
    (unsigned-byte-p 32 new-x)))


(defxdoc xoshiro
  :parents (stobjs::std/stobjs)
  :short "Pseudorandom number generating stobj"
  :long "<p>
 Implementation of the xoshiro128++ PRNG, from <a href=\"https://prng.di.unimi.it/\">this webpage</a>.
</p>

<p>Some utility functions built around it:</p>
<ul>
<li>@(see xoshiro-next): generate the next pseudorandom 32-bit unsigned integer</li>
<li>@(see xoshiro-gen-bits): generate a pseudorandom N-bit unsigned integer</li>
<li>@(see xoshiro-gen-range): generate a pseudorandom integer in the [0, N) range.</li>
<li>@(see xoshiro-init): initialize the xoshiro state pseudo-randomly, from the system PRNG -- takes and returns state as well.</li>
</ul>")


;; static uint32_t s[4];
(stobjs::defnicestobj xoshiro
  (s :type (array (unsigned-byte 32) (4)) :initially #x789abcde
     :fix (lambda (x) (loghead 32 x))))

(in-theory (enable xoshiro->s-length))

;; (local (in-theory (disable nth update-nth)))

;; (local (defthm xoshiro-sp-implies
;;          (implies (and (xoshiro-sp x)
;;                        (< (nfix n) (len x)))
;;                   (and (integerp (nth n x))
;;                        (unsigned-byte-p 32 (nth n x))
;;                        (natp (nth n x))
;;                        (acl2-numberp (nth n x))))
;;          :hints(("Goal" :in-theory (enable nth)))
;;          :rule-classes (:rewrite :type-prescription)))

;; (local (defthm xoshiro-sp-of-update-nth
;;          (implies (and (xoshiro-sp x)
;;                        (< (nfix n) (len x))
;;                        (unsigned-byte-p 32 v))
;;                   (xoshiro-sp (update-nth n v x)))
;;          :hints(("Goal" :in-theory (enable update-nth)))))

;; (local (defthm xoshirop-implies-si-type
;;          (implies (and (xoshirop x)
;;                        (< (nfix n) 4))
;;                   (natp (xoshiro-si n x)))
;;          :rule-classes :type-prescription))

;; (local (defthm xoshirop-implies-si-width
;;          (implies (and (xoshirop x)
;;                        (< (nfix n) 4))
;;                   (unsigned-byte-p 32 (xoshiro-si n x)))))

;; (local (defthm xoshirop-of-update-si
;;          (implies (and (xoshirop x)
;;                        (< (nfix n) 4)
;;                        (unsigned-byte-p 32 v))
;;                   (xoshirop (update-xoshiro-si n v x)))
;;          :hints (("goal" :do-not-induct t))))

;; (in-theory (disable xoshirop xoshiro-si update-xoshiro-si))

(define xoshiro-next (xoshiro)
  :returns (mv (val natp :rule-classes :type-prescription)
               (new-xoshiro))
  :guard-hints (("goal" :do-not-induct t))
  :parents (xoshiro)
  :short "Generate the next pseudorandom 32-bit integer and update the xoshiro state."
  :long "<p>The algorithm implemented is from <a href=\"https://prng.di.unimi.it/xoshiro128plusplus.c\">here</a>:
@({
  uint32_t next(void) {
        const uint32_t result = rotl(s[0] + s[3], 7) + s[0];
  
        const uint32_t t = s[1] \<< 9;
  
        s[2] ^= s[0];
        s[3] ^= s[1];
        s[1] ^= s[2];
        s[0] ^= s[3];
  
        s[2] ^= t;
  
        s[3] = rotl(s[3], 11);
  
        return result;
  }
})
</p>"
  (let* ((result (the (unsigned-byte 32)
                      (logand #xffffffff
                              (+ (the (unsigned-byte 32)
                                      (xoshiro-rotl (the (unsigned-byte 32)
                                                         (logand #xffffffff
                                                                 (+ (the (unsigned-byte 32)
                                                                         (xoshiro->si 0 xoshiro))
                                                                    (the (unsigned-byte 32)
                                                                         (xoshiro->si 3 xoshiro)))))
                                                    7))
                                 (the (unsigned-byte 32)
                                      (xoshiro->si 0 xoshiro))))))
         (tee (the (unsigned-byte 32)
                   (logand #xffffffff
                           (ash (the (unsigned-byte 32)
                                     (xoshiro->si 1 xoshiro))
                                9))))
         (xoshiro (update-xoshiro->si 2 (the (unsigned-byte 32)
                                             (logxor (the (unsigned-byte 32) (xoshiro->si 2 xoshiro))
                                                     (the (unsigned-byte 32) (xoshiro->si 0 xoshiro))))
                                      xoshiro))
         (xoshiro (update-xoshiro->si 3 (the (unsigned-byte 32)
                                             (logxor (the (unsigned-byte 32) (xoshiro->si 3 xoshiro))
                                                     (the (unsigned-byte 32) (xoshiro->si 1 xoshiro))))
                                      xoshiro))
         (xoshiro (update-xoshiro->si 1 (the (unsigned-byte 32)
                                             (logxor (the (unsigned-byte 32) (xoshiro->si 1 xoshiro))
                                                     (the (unsigned-byte 32) (xoshiro->si 2 xoshiro))))
                                      xoshiro))
         (xoshiro (update-xoshiro->si 0 (the (unsigned-byte 32)
                                             (logxor (the (unsigned-byte 32) (xoshiro->si 0 xoshiro))
                                                     (the (unsigned-byte 32) (xoshiro->si 3 xoshiro))))
                                      xoshiro))
         (xoshiro (update-xoshiro->si 2 (the (unsigned-byte 32)
                                             (logxor (the (unsigned-byte 32) (xoshiro->si 2 xoshiro))
                                                     (the (unsigned-byte 32) tee)))
                                      xoshiro))
         (xoshiro (update-xoshiro->si 3 (the (unsigned-byte 32)
                                             (xoshiro-rotl (the (unsigned-byte 32) (xoshiro->si 3 xoshiro))
                                                           11))
                                      xoshiro)))
    (mv (the (unsigned-byte 32) result)
        xoshiro))
  ///
  (defret width-of-<fn>
    (unsigned-byte-p 32 val)))


;; Initialize the xoshiro RNG from the system PRNG.
(define xoshiro-init (xoshiro state)
  :parents (xoshiro)
  :short "Initialize the xoshiro PRNG state with pseudorandom numbers from the system random number generator."
  :guard-hints (("goal" :in-theory (enable unsigned-byte-p)))
  (b* (((mv s0 state) (random$ (ash 1 32) state))
       ((mv s1 state) (random$ (ash 1 32) state))
       ((mv s2 state) (random$ (ash 1 32) state))
       ((mv s3 state) (random$ (ash 1 32) state))
       (xoshiro (update-xoshiro->si 0 s0 xoshiro))
       (xoshiro (update-xoshiro->si 1 s1 xoshiro))
       (xoshiro (update-xoshiro->si 2 s2 xoshiro))
       (xoshiro (update-xoshiro->si 3 s3 xoshiro)))
    (mv xoshiro state)))


(define xoshiro-gen-aux ((nbits natp)
                         (acc natp)
                         (xoshiro))
  :returns (mv (result natp :rule-classes :type-prescription)
               (new-xoshiro))
  :measure (nfix nbits)
  (b* (((mv first xoshiro) (xoshiro-next xoshiro))
       (nbits (lnfix nbits)))
    (if (<= nbits 32)
        (mv (logapp nbits first (lnfix acc)) xoshiro)
      (xoshiro-gen-aux (- nbits 32) (logapp 32 first (lnfix acc)) xoshiro)))
  ///
  (defret width-of-<fn>
    (implies (and (natp n)
                  (natp nbits)
                  (<= nbits n)
                  (unsigned-byte-p (- n nbits) acc))
             (unsigned-byte-p n result))))


(define xoshiro-gen-bits ((nbits natp) xoshiro)
  :parents (xoshiro)
  :short "Generate a pseudorandom N-bit unsigned integer from the xoshiro PRNG."
  :returns (mv (result natp :rule-classes :type-prescription)
               (new-xoshiro))
  (xoshiro-gen-aux (lnfix nbits) 0 xoshiro)
  ///
  (defret width-of-<fn>
    (implies (natp nbits)
             (unsigned-byte-p nbits result))))



(encapsulate nil
  (local (in-theory (enable nfix)))
  (acl2::def-tr xoshiro-gen-range-aux (top nbits xoshiro)
    (declare (xargs :guard (and (posp top) (natp nbits))
                    :stobjs xoshiro))
    (b* (((mv res xoshiro) (xoshiro-gen-bits nbits xoshiro))
         ((when (< res (mbe :logic (acl2::pos-fix top) :exec top)))
          (mv res xoshiro)))
      (xoshiro-gen-range-aux top nbits xoshiro))
    :diverge (let* ((xoshiro (non-exec (create-xoshiro))))
               (mv 0 xoshiro)))

  (defthm xoshiro-gen-range-aux-type
    (natp (mv-nth 0 (xoshiro-gen-range-aux top nbits xoshiro)))
    :hints(("Goal" :in-theory (enable xoshiro-gen-range-aux))))
  
  (defthm xoshiro-gen-range-aux-correct
    (implies (posp top)
             (<= (mv-nth 0 (xoshiro-gen-range-aux top nbits xoshiro)) top))
    :hints(("Goal" :in-theory (enable xoshiro-gen-range-aux)))
    :rule-classes :linear))


(define xoshiro-gen-range ((limit posp) xoshiro)
  :parents (xoshiro)
  :short "Generate a pseudorandom unsigned integer less than limit, from the xoshiro PRNG."
  :returns (mv (res natp :rule-classes :type-prescription)
               new-xoshiro)
  :prepwork ((local (defthm ash-of-integer-length
                      (implies (natp x)
                               (< x (ash 1 (integer-length x))))
                      :hints(("Goal" :in-theory (acl2::enable* bitops::ihsext-inductions
                                                               bitops::ihsext-recursive-redefs)))
                      :rule-classes :linear))
             (local (defthm ash-monotonic
                      (implies (and (natp x) (natp y)
                                    (<= x y))
                               (<= (ash 1 x) (ash 1 y)))
                      :hints(("Goal" :in-theory (enable bitops::ash-is-expt-*-x)))))
             (local (defthm ash-range-lemma
                      (implies (and (natp limit)
                                    (integerp i)
                                    (<= (integer-length limit) i))
                               (<= limit (ash 1 i)))
                      :hints (("goal" :use ((:instance ash-monotonic
                                             (x (integer-length limit)) (y i)))
                               :in-theory (disable ash-monotonic))))))
  (b* ((limit (mbe :logic (acl2::pos-fix limit) :exec limit))
       (limit-nbits (integer-length limit))
       (limit-dwords-fl (floor limit-nbits 32))
       (limit-extrabits (mod limit-nbits 32))
       (bits (* 32 (if (< limit-extrabits 16)
                       (+ 1 limit-dwords-fl)
                     (+ 2 limit-dwords-fl))))
       (top-val (ash 1 bits))
       (max-val (* limit (floor top-val limit)))
       ((mv gen-val xoshiro)
        (xoshiro-gen-range-aux max-val bits xoshiro)))
    (mv (mod gen-val limit) xoshiro))
  ///
  (defret <fn>-correct
    (implies (posp limit)
             (< res limit))
    :rule-classes :linear))





;; Dumb little microbenchmark: shows we can generate (and xor together) ~25,000,000 dwords/sec
#|
(define xor-randoms ((n natp)
                     (xor :type (unsigned-byte 32))
                     xoshiro)
  :returns (mv (xor natp)
               xoshiro)
  (if (zp n)
      (mv (lnfix xor) xoshiro)
    (b* (((mv val xoshiro) (xoshiro-next xoshiro)))
      (xor-randoms (1- n)
                   (the (unsigned-byte 32)
                        (logxor (the (unsigned-byte 32) xor)
                                (the (unsigned-byte 32) val)))
                   xoshiro))))
|#


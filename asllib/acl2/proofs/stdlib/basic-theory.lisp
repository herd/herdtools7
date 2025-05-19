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

;; Include this book locally!
(include-book "centaur/meta/variable-free" :dir :system)
(include-book "std/alists/hons-assoc-equal" :dir :system)
(include-book "std/alists/alist-keys" :dir :system)
(include-book "std/alists/fal-extract" :dir :system)
(include-book "std/lists/sets" :dir :system)
(include-book "std/lists/remove-duplicates" :dir :system)
(include-book "arithmetic/top" :dir :system)
(include-book "ihs/quotient-remainder-lemmas" :dir :system)
(include-book "std/alists/put-assoc-equal" :dir :system)
(include-book "centaur/bitops/rational-exponent" :Dir :system)



(defthm assoc-equal-is-hons-assoc-equal
  (implies (alistp x)
           (equal (assoc-equal k x)
                  (hons-assoc-equal k x))))



(defthm fal-extract-of-acons-non-mem
  (implies (not (member-equal k keys))
           (equal (acl2::fal-extract keys (cons (cons k v) x))
                  (acl2::fal-extract keys x)))
  :hints(("Goal" :in-theory (enable acl2::fal-extract))))

(defthm fal-extract-identity-when-no-duplicates
  (implies (and (no-duplicatesp-equal (acl2::alist-keys x))
                (alistp x))
           (equal (acl2::fal-extract (acl2::alist-keys x) x) x))
  :hints(("Goal" :in-theory (enable acl2::fal-extract acl2::alist-keys))))

(defthm remove-duplicates-when-no-duplicates
  (implies (and (no-duplicatesp-equal x)
                (True-listp x))
           (equal (remove-duplicates-equal x) x))
  :hints(("Goal" :in-theory (enable no-duplicatesp-equal remove-duplicates-equal))))


(defthm rational-exponent-when-expt-less
  (implies (and (rationalp x)
                (<= (expt 2 i) x)
                (integerp i))
           (<= i (acl2::rational-exponent x)))
  :hints (("goal" :use ((:instance acl2::rational-exponent-gte-power-of-2
                         (n i) (x x)))
           :in-theory (disable acl2::rational-exponent-gte-power-of-2)))
  :rule-classes :linear)

(defthm rational-exponent-when-expt-greater
  (implies (and (rationalp x)
                (< 0 x)
                (< x (expt 2 i))
                (integerp i))
           (> i (acl2::rational-exponent x)))
  :hints (("goal" :use ((:instance acl2::rational-exponent-less-than-power-of-2
                         (n i) (x x)))
           :in-theory (disable acl2::rational-exponent-less-than-power-of-2)))
  :rule-classes :linear)

(defthm alist-keys-of-fal-extract
  (equal (acl2::alist-keys (acl2::fal-extract keys x))
         (intersection-equal keys (acl2::alist-keys x)))
  :hints(("Goal" :in-theory (enable acl2::alist-keys acl2::fal-extract))))

(defthm no-duplicatesp-equal-of-intersection
  (implies (no-duplicatesp-equal x)
           (no-duplicatesp-equal (intersection-equal x y)))
  :hints(("Goal" :in-theory (enable intersection-equal))))

(defthm put-assoc-equal-normalize
  (implies (and ;; (syntaxp (and (not (equal k k1))
                ;;               (member-equal k (put-assoc-equal-term-keys x))))
                (hons-assoc-equal k x)
                (not (equal k k1)))
           (equal (put-assoc-equal k v (put-assoc-equal k1 v1 x))
                  (put-assoc-equal k1 v1 (put-assoc-equal k v x))))
  :hints(("Goal" :in-theory (enable put-assoc-equal))))


(defthm hons-assoc-equal-of-remove-assoc-equal
  (implies (syntaxp (and (quotep k) (quotep k1)))
           (equal (hons-assoc-equal k (remove-assoc-equal k1 x))
                  (if (equal k k1)
                      nil
                    (hons-assoc-equal k x)))))


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
(include-book "../../proof-utils")
(local (include-book "ast-theory"))
(local (include-book "centaur/bitops/ihsext-basics" :dir :System))


(local (in-theory (disable (tau-system)
                           hons-assoc-equal
                           put-assoc-equal
                           floor mod
                           bitops::part-select)))


(defloop uint-loop
  :function "UInt"
  :looptype :s_for
  :local-vars (((v_bitvector x) "__stdlib_local_x")
               ((v_int n) "__stdlib_local_N")
               ((v_int result) "__stdlib_local_result"
                (v_int (+ result.val (ash (bitops::part-select x.val :low start :high end) start)))))
  :index-var i
  :invariants (and (<= 0 start)
                   (<= (+ 1 end) x.len))
  :prepwork
  ((local (defthm ash-of-partselect-plus-one
            (implies (natp start)
                     (equal (ash (bitops::part-select-width-low val width (+ 1 start)) (+ 1 start))
                            (- (ash (bitops::part-select-width-low val (+ 1 (nfix width)) start) start)
                               (* (logbit start val) (expt 2 start)))))
            :hints(("Goal" :in-theory (enable bitops::ash-is-expt-*-x
                                              bitops::part-select-width-low
                                              logcons)
                    :expand ((logtail (+ 1 start) val)
                             (:free (x) (loghead (+ 1 (nfix width)) x)))))))
  
   (local (defthm loghead-of-part-select
            (implies (<= (nfix n) (nfix width))
                     (equal (loghead n (bitops::part-select-width-low val width low))
                            (loghead n (logtail low val))))
            :hints(("Goal" :in-theory (enable bitops::part-select-width-low)))))


   (local (defthm part-select-0-width
            (equal (bitops::part-select-width-low val 0 low) 0)
            :hints(("Goal" :in-theory (enable bitops::part-select-width-low)))))))


(def-asl-subprogram uint-correct
  :function "UInt"
  :params (n)
  :args (val)
  :return-values ((v_int val.val))
  :enable (bitops::part-select-width-low)
  :prepwork
  ((local (defthm nfix-when-not-negp
            (implies (not (negp n))
                     (equal (nfix n) (ifix n)))
            :hints(("Goal" :in-theory (enable nfix)))))

   (local (defthm expt-when-not-negp
            (implies (not (negp n))
                     (posp (expt 2 n)))
            :hints(("Goal" :in-theory (enable expt)))
            :rule-classes :type-prescription))

   (local (defthm v_bitvector-val-upper-bound
            (<= (v_bitvector->val x) (+ -1 (expt 2 (v_bitvector->len x))))
            :hints (("goal" :use ((:instance v_bitvector-requirements))
                     :in-theory (e/d (unsigned-byte-p)
                                     (v_bitvector-requirements))))
            :rule-classes :linear))))



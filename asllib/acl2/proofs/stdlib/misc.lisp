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

(local (in-theory (disable (tau-system))))

(def-asl-subprogram abs-real-correct
  :function "Abs"
  :args (val)
  :return-values ((v_real (abs val.val))))

(def-asl-subprogram abs-int-correct
  :function "Abs-1"
  :args (val)
  :return-values ((v_int (abs val.val))))

(local (in-theory (disable rfix)))

(def-asl-subprogram min-real-correct
  :function "Min"
  :args (a b)
  :return-values ((v_real (min a.val b.val))))

(def-asl-subprogram min-int-correct
  :function "Min-1"
  :args (a b)
  :return-values ((v_int (min a.val b.val))))

(def-asl-subprogram max-real-correct
  :function "Max"
  :args (a b)
  :return-values ((v_real (max a.val b.val))))

(def-asl-subprogram max-int-correct
  :function "Max-1"
  :args (a b)
  :return-values ((v_int (max a.val b.val))))


(def-asl-subprogram iseven-correct
  :function "IsEven"
  :args (a)
  :return-values ((v_bool (evenp a.val))))


(def-asl-subprogram isodd-correct
  :function "IsOdd"
  :args (a)
  :return-values ((v_bool (oddp a.val))))

(def-asl-subprogram real-correct
  :function "Real"
  :args (a)
  :return-values ((v_real a.val)))


(local (include-book "centaur/bitops/ihsext-basics" :dir :system))

(local (defthm nfix-when-not-negp
         (implies (not (negp x))
                  (equal (nfix x) (ifix x)))
         :hints(("Goal" :in-theory (enable nfix ifix)))))

(def-asl-subprogram zeros-1-correct
  :function "Zeros-1"
  :params (n)
  :hyps (<= 0 n.val)
  :return-values ((v_bitvector n.val 0)))

(def-asl-subprogram zeros-correct
  :function "Zeros"
  :params (n)
  :args (n)
  :hyps (<= 0 n.val)
  :return-values ((v_bitvector n.val 0)))




(local (in-theory (disable logmask)))

(local (defthm loghead-neg1
         (equal (loghead n -1)
                (logmask n))
         :hints(("Goal" :in-theory (acl2::enable* bitops::ihsext-inductions
                                                  bitops::ihsext-recursive-redefs)))))

(def-asl-subprogram ones-1-correct
  :function "Ones-1"
  :params (n)
  :hyps (<= 0 n.val)
  :return-values ((v_bitvector n.val -1)))

(def-asl-subprogram ones-correct
  :function "Ones"
  :params (n)
  :args (n)
  :hyps (<= 0 n.val)
  :return-values ((v_bitvector n.val -1)))

(def-asl-subprogram replicatebit-1-correct
  :function "ReplicateBit-1"
  :params (n)
  :args (b)
  :hyps (<= 0 n.val)
  :return-values ((v_bitvector n.val (if b.val 0 -1))))

(def-asl-subprogram replicatebit-correct
  :function "ReplicateBit"
  :params (n)
  :args (b n)
  :hyps (<= 0 n.val)
  :return-values ((v_bitvector n.val (if b.val 0 -1))))

(def-asl-subprogram len-correct
  :function "Len"
  :params (n)
  :args (v)
  :hyps (<= 0 n.val)
  :return-values ((v_int n.val)))

(def-asl-subprogram iszero-correct
  :function "IsZero"
  :params (n)
  :args (v)
  :return-values ((v_bool (equal v.val 0))))


(def-asl-subprogram isones-correct
  :function "IsOnes"
  :params (n)
  :args (v)
  :return-values ((v_bool (equal v.val (logmask n.val)))))


(def-asl-subprogram zeroextend-1-correct
  :function "ZeroExtend-1"
  :params (n m)
  :args (v)
  :hyps (<= m.val n.val)
  :return-values ((v_bitvector n.val v.val)))

(def-asl-subprogram zeroextend-correct
  :function "ZeroExtend"
  :params (n m)
  :args (v n)
  :hyps (<= m.val n.val)
  :return-values ((v_bitvector n.val v.val)))


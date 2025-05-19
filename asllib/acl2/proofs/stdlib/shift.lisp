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
(include-book "bits")
(include-book "centaur/bitops/rotate" :dir :system)
(local (include-book "ast-theory"))
(local (include-book "centaur/bitops/ihsext-basics" :dir :System))

(local (in-theory (disable (tau-system))))

(local (in-theory (disable floor mod expt
                           put-assoc-equal
                           hons-assoc-equal
                           unsigned-byte-p
                           logmask)))





;; LSL
;; LSL_C
;; LSR
;; LSR_C
;; ASR
;; ASR_C
;; ROR
;; ROR_C

(local (defthm nfix-when-not-negp
         (implies (not (negp x))
                  (equal (nfix x) (ifix x)))
         :hints(("Goal" :in-theory (enable nfix ifix)))))



(local (defthm loghead-when-zp
         (implies (zp n)
                  (equal (loghead n x) 0))
         :hints(("Goal" :in-theory (enable bitops::loghead**)))))

(def-asl-subprogram LSL-correct
  :function "LSL"
  :params (n)
  :args (x shift)
  :hyps (<= 0 shift.val)
  :return-values ((v_bitvector n.val (ash x.val shift.val)))
  :enable (bitops::loghead-of-ash))

(def-asl-subprogram LSL_C-correct
  :function "LSL_C"
  :params (n)
  :args (x shift)
  :hyps (< 0 shift.val)
  :return-values ((v_bitvector n.val (ash x.val shift.val))
                  (v_bitvector 1 (logbit n.val (ash x.val shift.val))))
  :enable (bitops::loghead-of-ash))

(local (defthm bitvector-logtail-0
         (implies (<= (v_bitvector->len x) (nfix n))
                  (equal (logtail n (v_bitvector->val x)) 0))
         :hints (("goal" :use ((:instance v_bitvector-requirements)
                               (:instance acl2::logtail-identity
                                (size (nfix n)) (i (v_bitvector->val x))))
                  :in-theory (disable v_bitvector-requirements
                                      acl2::logtail-identity)))))

(def-asl-subprogram LSR-correct
  :function "LSR"
  :params (n)
  :args (x shift)
  :hyps (<= 0 shift.val)
  :return-values ((v_bitvector n.val (ash x.val (- shift.val))))
  :enable (bitops::loghead-of-loghead-split))


(local (defthm loghead-1-of-bit
         (implies (bitp x)
                  (equal (loghead 1 x) x))
         :hints(("Goal" :in-theory (enable bitops::loghead**)))))

(local (defthm bitvector-logbitp-out-of-range
         (implies (<= (v_bitvector->len x) (nfix n))
                  (not (logbitp n (v_bitvector->val x))))
         :hints (("goal" :use ((:instance v_bitvector-requirements)
                               (:instance bitops::logbitp-of-loghead-out-of-bounds
                                (size (v_bitvector->len x)) (i (v_bitvector->val x))
                                (pos n)))
                  :in-theory (disable v_bitvector-requirements
                                      bitops::logbitp-of-loghead-out-of-bounds)))))

(def-asl-subprogram LSR_C-correct
  :function "LSR_C"
  :params (n)
  :args (x shift)
  :hyps (< 0 shift.val)
  :return-values ((v_bitvector n.val (ash x.val (- shift.val)))
                  (v_bitvector 1 (logbit (1- shift.val) x.val)))
  :enable (bitops::loghead-of-ash))



(def-asl-subprogram ASR-correct
  :function "ASR"
  :params (n)
  :args (x shift)
  :hyps (and (< 0 n.val)
             (<= 0 shift.val))
  :return-values ((v_bitvector n.val (logtail shift.val (logext n.val x.val))))
  :enable (bitops::loghead-of-loghead-split
           logext
           bitops::logtail-of-logapp-split))

(def-asl-subprogram ASR_C-correct
  :function "ASR_C"
  :params (n)
  :args (x shift)
  :hyps (and (< 0 n.val)
             (< 0 shift.val))
  :return-values ((v_bitvector n.val (logtail shift.val (logext n.val x.val)))
                  (v_bitvector 1 (logbit (1- shift.val) (logext n.val x.val))))
  :enable (bitops::loghead-of-ash))



(def-asl-subprogram ROR-correct
  :function "ROR"
  :params (n)
  :args (x shift)
  :hyps (and (< 0 n.val)
             (<= 0 shift.val))
  :return-values ((v_bitvector n.val (bitops::rotate-right x.val n.val shift.val)))
  :prepwork
  (

   (local (defthm logand-with-ash-minus-1
            (implies (not (negp n))
                     (equal (logand x (+ -1 (ash 1 n)))
                            (loghead n x)))
            :hints(("Goal" :in-theory (acl2::enable* bitops::ihsext-inductions
                                                     bitops::ihsext-recursive-redefs
                                                     bitops::equal-logcons-strong)))))

    
  
   (local (defthm logior-loghead-ash
            (implies (not (negp w))
                     (equal (logior (ash x w)
                                    (loghead w y))
                            (logapp w y x)))
            :hints(("Goal" :in-theory (acl2::enable* bitops::ihsext-inductions
                                                     bitops::ihsext-recursive-redefs
                                                     bitops::equal-logcons-strong)))))
   (local (defthm rotate-right-in-terms-of-logapp
            (implies (posp n)
                     (equal (rotate-right x n shift)
                            (b* ((shift-mod-n (mod (nfix shift) n)))
                              (logapp (- n shift-mod-n)
                                      (logtail shift-mod-n x)
                                      (loghead shift-mod-n x)))))
            :hints(("Goal" :in-theory (enable rotate-right)))))))

(def-asl-subprogram ROR_C-correct
  :function "ROR_C"
  :params (n)
  :args (x shift)
  :hyps (and (< 0 n.val)
             (< 0 shift.val))
  :return-values ((v_bitvector n.val (bitops::rotate-right x.val n.val shift.val))
                  (v_bitvector 1 (logbit (mod (1- shift.val) n.val) x.val)))
  :enable (bitops::loghead-of-ash))



(local (include-book "centaur/bitops/equal-by-logbitp" :dir :system))

(local (in-theory (enable bitops::logbitp-of-rotate-left-split
                          bitops::logbitp-of-rotate-right-split)))
(local (defthm rotate-left-in-terms-of-rotate-right
         (implies (and (posp width) (natp places))
                  (equal (rotate-left x width places)
                         (rotate-right x width (mod (- places) width))))
         :hints((bitops::logbitp-reasoning))))


(def-asl-subprogram ROL-correct
  :function "ROL"
  :params (n)
  :args (x shift)
  :hyps (and (< 0 n.val)
             (<= 0 shift.val))
  :return-values ((v_bitvector n.val (bitops::rotate-left x.val n.val shift.val))))

(def-asl-subprogram ROL_C-correct
  :function "ROL_C"
  :params (n)
  :args (x shift)
  :hyps (and (< 0 n.val)
             (< 0 shift.val))
  :return-values ((v_bitvector n.val (bitops::rotate-left x.val n.val shift.val))
                  (v_bitvector 1 (logbit 0 (bitops::rotate-left x.val n.val shift.val))))
  :enable (bitops::loghead-of-ash
           bitops::loghead** bitops::logbitp**))


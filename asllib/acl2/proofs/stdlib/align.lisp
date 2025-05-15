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
(include-book "uint")
(local (include-book "ast-theory"))
(local (include-book "centaur/bitops/ihsext-basics" :dir :System))

(local (in-theory (disable (tau-system))))

(local (in-theory (disable floor mod expt ceiling
                           put-assoc-equal
                           hons-assoc-equal)))



(def-asl-subprogram aligndownsize-1-correct
  :function "AlignDownSize-1"
  :args (x size)
  :hyps (and (<= 0 x.val)
             (< 0 size.val))
  :return-values ((v_int (* size.val (floor x.val size.val)))))

(local (defthm ceiling-in-terms-of-floor
         (implies (and (<= 0 x)
                       (< 0 size)
                       (rationalp x)
                       (rationalp size))
                  (equal (ceiling x size)
                         (if (integerp (/ x size))
                             (floor x size)
                           (+ 1 (floor x size)))))
         :hints(("Goal" :in-theory (enable floor ceiling)))))


(def-asl-subprogram alignupsize-1-correct
  :function "AlignUpSize-1"
  :args (x size)
  :hyps (and (<= 0 x.val) (< 0 size.val))
  :return-values ((v_int (* size.val (ceiling x.val size.val))))
  :prepwork ((local (defthm integerp-recip-when-posp
                      (implies (posp x)
                               (equal (integerp (/ x))
                                      (equal x 1)))))

             (local (in-theory (disable ceiling)))
  
  
  

             (local (defthm floor-of-x+size-1
                      (implies (and (<= 0 x)
                                    (< 0 size)
                                    (integerp x)
                                    (integerp size)
                                    (not (integerp (/ x size))))
                               (equal (floor (+ -1 size x) size)
                                      (+ 1 (floor x size))))))))


(local (defthm posp-expt
         (implies (not (negp x))
                  (posp (expt 2 x)))
         :hints(("Goal" :in-theory (enable expt)))
         :rule-classes :type-prescription))

(def-asl-subprogram aligndownp2-1-correct
  :function "AlignDownP2-1"
  :args (x p2)
  :hyps (and (<= 0 x.val) (<= 0 p2.val))
  :return-values ((v_int (* (expt 2 p2.val)
                            (floor x.val (expt 2 p2.val))))))


(def-asl-subprogram alignupp2-1-correct
  :function "AlignUpP2-1"
  :args (x p2)
  :hyps (and (<= 0 x.val) (<= 0 p2.val))
  :return-values ((v_int (* (expt 2 p2.val)
                            (ceiling x.val (expt 2 p2.val))))))






(local (defthm nfix-when-not-negp
         (implies (not (negp x))
                  (equal (nfix x) (ifix x)))
         :hints(("Goal" :in-theory (enable nfix ifix)))))

(local (defthm logtail-natp
         (implies (not (negp x))
                  (natp (logtail n x)))
         :hints(("Goal" :use ((:instance bitops::logtail-natp
                               (n n)
                               (x (ifix x))))))
         :rule-classes :type-prescription))

(def-asl-subprogram aligndown-correct
  :function "AlignDown"
  :params (n)
  :args (x y)
  ;; :hyps (and (< 0 y.val)
  ;;            (<= y.val n.val))
  :return-values ((v_bitvector n.val (ash (logtail y.val x.val) y.val)))
  :enable (bitops::loghead-of-ash))

(local
 (defsection loghead-of-plus-loghead
   

  (local (defun loghead-of-plus-loghead-ind (n a b c)
           (if (zp n)
               (list a b c)
             (loghead-of-plus-loghead-ind
              (1- n) (logcdr a) (logcdr b)
              (b-ior (b-and (logcar a) (logcar b))
                     (b-ior (b-and (logcar a) c)
                            (b-and (logcar b) c)))))))
  
  (local (defthm loghead-of-plus-loghead-lemma
           (implies (and (integerp a)
                         (integerp b)
                         (bitp c))
                    (equal (loghead n (+ c a (loghead n b)))
                           (loghead n (+ c a b))))
           :hints (("goal" :in-theory (acl2::enable* bitops::ihsext-recursive-redefs)
                    :induct (loghead-of-plus-loghead-ind n a b c)))))
  
  (defthm loghead-of-plus-loghead
    (implies (and (integerp a)
                  (integerp b))
             (equal (loghead n (+ a (loghead n b)))
                    (loghead n (+ a b))))
    :hints (("goal" :use ((:instance loghead-of-plus-loghead-lemma (c 0))))))))


(def-asl-subprogram alignup-correct
  :function "AlignUp"
  :params (n)
  :args (x y)
  :return-values ((v_bitvector n.val
                               (if (equal (loghead y.val x.val) 0)
                                   x.val
                                 (ash (+ 1 (logtail y.val x.val)) y.val))))
  :enable (bitops::loghead-of-ash))

(def-asl-subprogram aligndownsize-correct
  :function "AlignDownSize"
  :params (n)
  :args (x size)
  :return-values ((v_bitvector n.val
                               (* size.val
                                  (floor x.val size.val)))))


(def-asl-subprogram alignupsize-correct
  :function "AlignUpSize"
  :params (n)
  :args (x size)
  :return-values ((v_bitvector n.val
                               (* size.val
                                  (ceiling x.val size.val)))))






(local (defthmd ash-is-expt
         (implies (not (negp n))
                  (equal (ash x n)
                         (* (ifix x) (expt 2 (ifix n)))))
         :hints (("goal" :use ((:instance bitops::ash-is-expt-*-x
                                (n (ifix n)) (x x)))))))



(local (defthm floor-to-logtail
         (implies (and (not (negp n)) (integerp x))
                  (equal (floor x (expt 2 n))
                         (logtail n x)))
         :hints(("Goal" :in-theory (enable logtail)))))

(def-asl-subprogram aligndownp2-correct
  :function "AlignDownP2"
  :params (n)
  :args (x p2)
  :hyps (< 0 n.val)
  :return-values ((v_bitvector n.val
                               (* (expt 2 p2.val)
                                  (floor x.val (expt 2 p2.val)))))
  :prepwork ((local (defthm loghead-of-expt-product
                      (implies (and (integerp i)
                                    (not (negp p2))
                                    (<= (ifix p2) (nfix n)))
                               (equal (loghead n (* (expt 2 p2) i))
                                      (ash (loghead (- (nfix n) (ifix p2)) i) p2)))
                      :hints (("goal" :use ((:instance bitops::loghead-of-ash
                                             (n n) (m p2) (x i)))
                               :in-theory (enable ash-is-expt)))))))



(def-asl-subprogram alignupp2-correct
  :function "AlignUpP2"
  :params (n)
  :args (x p2)
  :hyps (< 0 n.val)
  :return-values ((v_bitvector n.val
                               (* (expt 2 p2.val)
                                  (ceiling x.val (expt 2 p2.val)))))

  :prepwork
  ((defthm loghead-of-plus-loghead3
     (implies (and (integerp a)
                   (integerp b)
                   (integerp c))
              (equal (loghead n (+ a b (loghead n c)))
                     (loghead n (+ a b c))))
     :hints (("goal" :use ((:instance loghead-of-plus-loghead
                            (a (+ a b)) (b c))))))

   (local (defun ind (n x y)
            (if (zp n)
                (list x y)
              (ind (1- n) (logcdr x) (logcdr y)))))
  
   (local (defthm logtail-small-when-loghead-0
            (implies (and (equal (loghead n x) 0)
                          (integerp x)
                          (unsigned-byte-p n y))
                     (equal (logtail n (+ x y))
                            (logtail n x)))
            :hints (("goal" :in-theory (bitops::e/d* (bitops::ihsext-recursive-redefs))
                     :induct (ind n x y)))))

   (local (defthm loghead-0-when-integerp-div
            (implies (and (integerp x)
                          (not (negp n)))
                     (equal (integerp (* x (/ (expt 2 n))))
                            (equal 0 (loghead n x))))
            :hints(("Goal" :in-theory (enable loghead)))))

   (local (defthm logtail-lemma
            (implies (and (equal (loghead n x) 0)
                          (integerp x)
                          (not (negp n)))
                     (equal (logtail n (+ -1 x (expt 2 n)))
                            (logtail n x)))
            :hints (("goal" :use ((:instance logtail-small-when-loghead-0
                                   (y (+ -1 (expt 2 n)))))))))

   (local (defthm logtail-of-plus-ash
            (implies (and (integerp x)
                          (not (negp n)))
                     (equal (logtail n (+ x (ash 1 n)))
                            (+ 1 (logtail n x))))
            :hints (("goal" :in-theory (bitops::e/d* (bitops::ihsext-inductions
                                                      bitops::ihsext-recursive-redefs))
                     :induct (logtail n x)
                     :expand ((:free (x y) (logtail n (+ x y)))))
                    (and stable-under-simplificationp
                         '(:in-theory (enable ash-is-expt))))))
  
   (local (defthm logtail-lemma2-aux
            (implies (and (not (equal (loghead n x) 0))
                          (integerp x)
                          (not (negp n)))
                     (equal (logtail n (+ -1 x (ash 1 n)))
                            (+ 1 (logtail n x))))
            :hints (("goal" :in-theory (bitops::e/d* (bitops::ihsext-inductions
                                                      bitops::ihsext-recursive-redefs))
                     :induct (logtail n x)
                     :expand ((:free (x y) (logtail n (+ x y))))))))

   (local (defthm logtail-lemma2
            (implies (and (not (equal (loghead n x) 0))
                          (integerp x)
                          (not (negp n)))
                     (equal (logtail n (+ -1 x (expt 2 n)))
                            (+ 1 (logtail n x))))
            :hints (("goal" :use ((:instance logtail-lemma2-aux))
                     :in-theory (e/d (ash-is-expt) (logtail-lemma2-aux))))))
  

   (local (defthm loghead-of-plus-mult-expt
            (implies (and (integerp x)
                          (natp n)
                          (not (negp m))
                          (<= (ifix m) n))
                     (equal (loghead n (+ x (* (expt 2 m) (loghead (+ n (- (ifix m))) y))))
                            (loghead n (+ x (* (expt 2 m) (ifix y))))))
            :hints (("goal" :use ((:instance loghead-of-plus-loghead
                                   (a x)
                                   (b (ash y m))))
                     :in-theory (e/d (bitops::loghead-of-ash)
                                     (loghead-of-plus-loghead)))
                    (and stable-under-simplificationp
                         '(:in-theory (enable ash-is-expt))))))

   (local (defthm loghead-of-mult-expt
            (implies (and (integerp x)
                          (natp n)
                          (natp m)
                          (<= m n))
                     (equal (loghead n (* (expt 2 m) (loghead (+ n (- m)) y)))
                            (loghead n (* (expt 2 m) (ifix y)))))
            :hints (("goal" :use ((:instance loghead-of-plus-mult-expt
                                   (x 0)))))))
  

   (local (in-theory (disable ceiling)))))





(def-asl-subprogram isalignedsize-1-correct
  :function "IsAlignedSize-1"
  :args (x size)
  :hyps (< 0 size.val)
  :return-values ((v_bool (eql (mod x.val size.val) 0))))

(def-asl-subprogram isalignedsize-correct
  :function "IsAlignedSize"
  :params (n)
  :args (x size)
  :hyps (< 0 size.val)
  :return-values ((v_bool (eql (mod x.val size.val) 0))))

(def-asl-subprogram isalignedp2-1-correct
  :function "IsAlignedP2-1"
  :args (x p2)
  :hyps (<= 0 p2.val)
  :return-values ((v_bool (eql (mod x.val (expt 2 p2.val)) 0))))

(def-asl-subprogram isalignedp2-correct
  :function "IsAlignedP2"
  :params (n)
  :args (x p2)
  :hyps (<= 0 p2.val)
  :return-values ((v_bool (eql (mod x.val (expt 2 p2.val)) 0)))
  :hints('(:in-theory (enable loghead))))


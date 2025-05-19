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
(include-book "centaur/bitops/rational-exponent" :dir :system)
(local (include-book "ast-theory"))

(local (in-theory (disable (tau-system))))

(local (in-theory (disable floor mod
                           put-assoc-equal
                           hons-assoc-equal)))


;; (let ((env (env (make-global-env :static (stdlib-static-env))
;;                 (change-local-env (empty-local-env)
;;                                   :storage
;;                                   (put-assoc-equal
;;                                    "__stdlib_local_a" (v_int 1)
;;                                    (put-assoc-equal "__stdlib_local_current"
;;                                                     (v_int 2)
;;                                                     (put-assoc-equal "__stdlib_local_result"
;;                                                                      (v_int 0) nil))))))
;;       (clk 100) (limit 100))
;;   (EVAL_LOOP ENV T LIMIT *FLOORLOG2-LOOP-TEST*
;;              *FLOORLOG2-LOOP-BODY*))


(local (defthm integer-length-in-terms-of-rational-exponent
         (implies (force (posp x))
                  (equal (integer-length x)
                         (+ 1 (acl2::rational-exponent x))))
         :hints(("Goal" :in-theory (enable acl2::rational-exponent)))))

(local (in-theory (disable integer-length
                           expt)))


(defloop floorlog2-loop
  :function "FloorLog2"
  :looptype :s_while
  :local-vars (((v_int a)       "__stdlib_local_a")
               ((v_int current) "__stdlib_local_current" (v_int current-spec))
               ((v_int result)  "__stdlib_local_result"  (v_int result-spec)))
  :bindings ((result-spec (+ -1 (integer-length a.val)))
             (current-spec (expt 2 (+ 1 result-spec))))
  :invariants (and (<= 0 result.val)
                   (equal current.val (expt 2 (+ 1 result.val)))
                   (<= current.val (* 2 a.val))
                   (< (- (integer-length a.val) result.val) (ifix clk))
                   (integerp limit)
                   (< (- (integer-length a.val) result.val) limit))
  :hints ((and stable-under-simplificationp
               '(:use ((:instance acl2::rational-exponent-unique
                        (x (V_INT->VAL
                            (CDR (HONS-ASSOC-EQUAL "__stdlib_local_a"
                                                   (LOCAL-ENV->STORAGE (ENV->LOCAL ENV))))))
                        (n (V_INT->VAL
                            (CDR (HONS-ASSOC-EQUAL "__stdlib_local_result"
                                                   (LOCAL-ENV->STORAGE (ENV->LOCAL ENV)))))))
                       (:instance rational-exponent-when-expt-less
                        (i (+ 1 (V_INT->VAL
                                 (CDR (HONS-ASSOC-EQUAL "__stdlib_local_result"
                                                        (LOCAL-ENV->STORAGE (ENV->LOCAL ENV)))))))
                        (x (V_INT->VAL
                            (CDR (HONS-ASSOC-EQUAL "__stdlib_local_a"
                                                   (LOCAL-ENV->STORAGE (ENV->LOCAL ENV)))))))))))
  :prepwork
  (;; (local (Defthmd rational-exponent-unique-of-nonneg
   ;;          (implies (and (<= (expt 2 r) a)
   ;;                        (< a (* 2 (expt 2 r)))
   ;;                        (integerp a)
   ;;                        (integerp r))
   ;;                   (equal (acl2::rational-exponent a) r))
   ;;          :hints (("goal" :use ((:instance acl2::rational-sign-significand-exponent-unique
   ;;                                 (sign 1) (significand (/ a (expt 2 r))) (exponent r)))))))
                         

   (local (defthm put-assoc-equal-identity-free
            (implies (and (equal v (cdr (hons-assoc-equal k x)))
                          (hons-assoc-equal k x)
                          (alistp x))
                     (equal (put-assoc-equal k v x) x))))

   ;; (local (defthmd powers-of-2-not-between-lemma1
   ;;          (implies (and (< (ifix ye) (ifix xe))
   ;;                        (<= (ifix ye) 0))
   ;;                   (<= (* 2 (expt 2 ye)) (expt 2 xe)))
   ;;          :hints (("goal" :induct (expt 2 ye)))
   ;;          :otf-flg t))
   ;; (local (defthmd powers-of-2-not-between-lemma
   ;;          (implies (< (ifix ye) (ifix xe))
   ;;                   (<= (* 2 (expt 2 ye)) (expt 2 xe)))
   ;;          :hints (("goal" :cases ((<= 0 (ifix ye)))
   ;;                   :in-theory (enable powers-of-2-not-between-lemma1)))
   ;;          :otf-flg t))  

   ;; (local (defthm powers-of-2-not-between
   ;;          (implies (< (expt 2 xe) (* 2 (expt 2 ye)))
   ;;                   (<= (expt 2 xe) (expt 2 ye)))
   ;;          :hints (("goal" :use ((:instance acl2::expt-is-increasing-for-base>1
   ;;                                 (r 2) (i (ifix xe)) (j (ifix ye)))
   ;;                                (:instance acl2::expt-is-increasing-for-base>1
   ;;                                 (r 2) (i (ifix ye)) (j (ifix xe)))
   ;;                                powers-of-2-not-between-lemma)
   ;;                   :in-theory (acl2::e/d* (acl2::arith-equiv-forwarding)
   ;;                                          (acl2::expt-is-increasing-for-base>1))))
   ;;          :rule-classes nil))


   (local (defthm v_int->val-rewrite-to-exponent
            (implies (equal (v_int->val x) (expt 2 y))
                     (equal (v_int->val x) (expt 2 y)))))

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
                                         equal-of-v_int)))))))
         


(def-asl-subprogram floorlog2-correct
  :function "FloorLog2"
  :args (x)
  :hyps (and (< 0 x.val)
             (<= (+ 1 (integer-length x.val)) (expt 2 128)))
  :safe-clock (+ 1 (integer-length x.val))
  :return-values ((v_int (1- (integer-length x.val)))))



;; (let ((env (env (make-global-env :static (stdlib-static-env))
;;                 (change-local-env (empty-local-env)
;;                                   :storage
;;                                   (put-assoc-equal
;;                                    "__stdlib_local_a" (v_int 5)
;;                                    (put-assoc-equal "__stdlib_local_current"
;;                                                     (v_int 1)
;;                                                     (put-assoc-equal "__stdlib_local_result"
;;                                                                      (v_int 0) nil))))))
;;       (clk 100) (limit 100))
;;   (EVAL_LOOP ENV T LIMIT *CEILLOG2-LOOP-TEST*
;;              *CEILLOG2-LOOP-BODY*))




(defloop ceillog2-loop
  :function "CeilLog2"
  :looptype :s_while
  :local-vars (((v_int a)       "__stdlib_local_a")
               ((v_int current) "__stdlib_local_current" (v_int current-spec))
               ((v_int result)  "__stdlib_local_result"  (v_int result-spec)))
  :bindings ((result-spec (let ((exp (1- (integer-length a.val))))
                            (if (equal a.val (expt 2 exp))
                                exp
                              (+ 1 exp))))
             (current-spec (expt 2 result-spec)))
  :invariants (and (<= 0 result.val)
                   (equal current.val (expt 2 result.val))
                   (< current.val (* 2 a.val))
                   (< (- (integer-length a.val) result.val) (ifix clk))
                   (integerp limit)
                   (< (- (integer-length a.val) result.val) limit))
  :hints ((and stable-under-simplificationp
               '(:use ((:instance rational-exponent-hack
                        (r (V_INT->VAL
                            (CDR (HONS-ASSOC-EQUAL "__stdlib_local_result"
                                                   (LOCAL-ENV->STORAGE (ENV->LOCAL ENV))))))
                        (a (V_INT->VAL
                            (CDR (HONS-ASSOC-EQUAL "__stdlib_local_a"
                                                   (LOCAL-ENV->STORAGE (ENV->LOCAL ENV)))))))
                       (:instance powers-of-2-not-between
                        (xe (V_INT->VAL
                             (CDR (HONS-ASSOC-EQUAL "__stdlib_local_result"
                                                    (LOCAL-ENV->STORAGE (ENV->LOCAL ENV))))))
                        (ye (acl2::rational-exponent
                             (V_INT->VAL
                              (CDR (HONS-ASSOC-EQUAL "__stdlib_local_a"
                                                     (LOCAL-ENV->STORAGE (ENV->LOCAL ENV))))))))
                       (:instance rational-exponent-when-expt-less
                        (i (V_INT->VAL
                            (CDR (HONS-ASSOC-EQUAL "__stdlib_local_result"
                                                   (LOCAL-ENV->STORAGE (ENV->LOCAL ENV))))))
                        (x (V_INT->VAL
                            (CDR (HONS-ASSOC-EQUAL "__stdlib_local_a"
                                                   (LOCAL-ENV->STORAGE (ENV->LOCAL ENV))))))))
                 :in-theory (enable equal-when-v_int))))
  :prepwork
  ((local (Defthmd rational-exponent-unique-of-nonneg
            (implies (and (<= (expt 2 r) a)
                          (< a (* 2 (expt 2 r)))
                          (integerp a)
                          (integerp r))
                     (equal (acl2::rational-exponent a) r))
            :hints (("goal" :use ((:instance acl2::rational-sign-significand-exponent-unique
                                   (sign 1) (significand (/ a (expt 2 r))) (exponent r)))))))
  
   (local (defthm rational-exponent-hack
            (implies (and (<= a (expt 2 r))
                          (< (expt 2 r) (* 2 a))
                          (integerp a)
                          (integerp r))
                     (equal r (if (equal a (expt 2 r))
                                  (acl2::rational-exponent a)
                                (+ 1 (acl2::rational-exponent a)))))
            :hints (("goal" :do-not-induct t
                     :in-theory (enable expt)
                     :expand ((:free (x) (hide x))))
                    (and stable-under-simplificationp
                         '(:use ((:instance rational-exponent-unique-of-nonneg
                                  (r (+ -1 r))))
                           :expand ((expt 2 r)))))
            :otf-flg t
            :rule-classes nil))
                         

   (local (defthm put-assoc-equal-identity-free
            (implies (and (equal v (cdr (hons-assoc-equal k x)))
                          (hons-assoc-equal k x)
                          (alistp x))
                     (equal (put-assoc-equal k v x) x))))

   (local (defthmd powers-of-2-not-between-lemma1
            (implies (and (< (ifix ye) (ifix xe))
                          (<= (ifix ye) 0))
                     (<= (* 2 (expt 2 ye)) (expt 2 xe)))
            :hints (("goal" :induct (expt 2 ye)
                     :in-theory (enable expt)))
            :otf-flg t))
   (local (defthmd powers-of-2-not-between-lemma
            (implies (< (ifix ye) (ifix xe))
                     (<= (* 2 (expt 2 ye)) (expt 2 xe)))
            :hints (("goal" :cases ((<= 0 (ifix ye)))
                     :in-theory (enable powers-of-2-not-between-lemma1
                                        expt)))
            :otf-flg t))  

   (local (defthm powers-of-2-not-between
            (implies (< (expt 2 xe) (* 2 (expt 2 ye)))
                     (<= (expt 2 xe) (expt 2 ye)))
            :hints (("goal" :use ((:instance acl2::expt-is-increasing-for-base>1
                                   (r 2) (i (ifix xe)) (j (ifix ye)))
                                  (:instance acl2::expt-is-increasing-for-base>1
                                   (r 2) (i (ifix ye)) (j (ifix xe)))
                                  powers-of-2-not-between-lemma)
                     :in-theory (acl2::e/d* (acl2::arith-equiv-forwarding)
                                            (acl2::expt-is-increasing-for-base>1))))
            :rule-classes nil))


   (local (defthm v_int->val-rewrite-to-exponent
            (implies (equal (v_int->val x) (expt 2 y))
                     (equal (v_int->val x) (expt 2 y)))))

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
                                         equal-of-v_int)))))))


(define ceil-log2-spec ((x integerp))
  :guard (< 0 x)
  (b* ((exp (1- (integer-length x))))
    (if (equal x (expt 2 exp))
        exp
      (+ 1 exp)))
  ///
  (defthm ceil-log2-spec-correct
    (implies (and (integerp x)
                  (< 0 x))
             (let ((exp (ceil-log2-spec x)))
               (and (<= x (expt 2 exp))
                    (< (expt 2 (+ -1 exp)) x))))
    :hints (("goal" :use ((:instance acl2::rational-exponent-correct-positive (x x)))
             :in-theory (e/d (acl2::exponents-add-unrestricted
                              integer-length-in-terms-of-rational-exponent)
                             (acl2::rational-exponent-correct-positive))))))
      


(def-asl-subprogram ceillog2-correct
  :function "CeilLog2"
  :args (x)
  :hyps (and (< 0 x.val)
             (<= (+ 1 (integer-length x.val)) (expt 2 128)))
  :safe-clock (+ 1 (integer-length x.val))
  :return-values ((v_int (ceil-log2-spec x.val)))
  :enable (ceil-log2-spec))








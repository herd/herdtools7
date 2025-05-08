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

(include-book "misc")
(include-book "ilog2-alg")
(local (include-book "ast-theory"))
(local (in-theory (disable (tau-system)
                           hons-assoc-equal
                           put-assoc-equal
                           floor mod)))




(defloop ilog2-loop-1
  :function "ILog2"
  :looptype :s_while
  :nth 0
  :local-vars (((v_real val) "__stdlib_local_val")
               ((v_int high) "__stdlib_local_high" (v_int high-spec))
               ((v_int low)  "__stdlib_local_low"  (v_int low-spec)))
  :bindings (((mv low-spec high-spec) (acl2::ilog2-search-up val.val low.val high.val)))
  :invariants (and (<= 1 val.val)
                   (<= 1 high.val)
                   (< (- (+ 1 (acl2::rational-exponent val.val)) high.val) (nfix clk))
                   (integerp limit)
                   (< (- (+ 1 (acl2::rational-exponent val.val)) high.val) limit))
  :hints ((and stable-under-simplificationp
               '(:expand ((ACL2::ILOG2-SEARCH-UP
                           (V_REAL->VAL (CDR (HONS-ASSOC-EQUAL "__stdlib_local_val"
                                                               (LOCAL-ENV->STORAGE (ENV->LOCAL ENV)))))
                           (V_INT->VAL (CDR (HONS-ASSOC-EQUAL "__stdlib_local_low"
                                                              (LOCAL-ENV->STORAGE (ENV->LOCAL ENV)))))
                           (V_INT->VAL
                            (CDR (HONS-ASSOC-EQUAL "__stdlib_local_high"
                                                   (LOCAL-ENV->STORAGE (ENV->LOCAL ENV)))))))))))


(defloop ilog2-loop-2
  :function "ILog2"
  :looptype :s_while
  :nth 1
  :local-vars (((v_real val) "__stdlib_local_val")
               ((v_int high) "__stdlib_local_high" (v_int high-spec))
               ((v_int low)  "__stdlib_local_low"  (v_int low-spec)))
  :bindings (((mv low-spec high-spec) (acl2::ilog2-search-down val.val low.val high.val)))
  :invariants (and (< 0 val.val)
                   (< val.val 1)
                   (<= low.val -1)
                   (< (- (+ 1 (- (acl2::rational-exponent val.val))) (- low.val)) (nfix clk))
                   (integerp limit)
                   (< (- (+ 1 (- (acl2::rational-exponent val.val))) (- low.val)) limit))
  :hints ((and stable-under-simplificationp
               '(:expand ((ACL2::ILOG2-SEARCH-DOWN
                           (V_REAL->VAL (CDR (HONS-ASSOC-EQUAL "__stdlib_local_val"
                                                               (LOCAL-ENV->STORAGE (ENV->LOCAL ENV)))))
                           (V_INT->VAL (CDR (HONS-ASSOC-EQUAL "__stdlib_local_low"
                                                              (LOCAL-ENV->STORAGE (ENV->LOCAL ENV)))))
                           (V_INT->VAL
                            (CDR (HONS-ASSOC-EQUAL "__stdlib_local_high"
                                                   (LOCAL-ENV->STORAGE (ENV->LOCAL ENV)))))))))))


(defloop ilog2-loop-3
  :function "ILog2"
  :looptype :s_while
  :nth 2
  :local-vars (((v_real val) "__stdlib_local_val")
               ((v_int high) "__stdlib_local_high" (v_int high-spec))
               ((v_int low)  "__stdlib_local_low"  (v_int low-spec)))
  :bindings (((mv low-spec high-spec) (acl2::ilog2-binary-search val.val low.val high.val)))
  :invariants (and (not (hons-assoc-equal "__stdlib_local_mid" env.local.storage))
                   (< 0 val.val)
                   (< (- high.val low.val) (nfix clk))
                   (integerp limit)
                   (< (- high.val low.val) limit))
  :hints ((and stable-under-simplificationp
               '(:expand ((acl2::ilog2-binary-search
                           (V_REAL->VAL (CDR (HONS-ASSOC-EQUAL "__stdlib_local_val"
                                                               (LOCAL-ENV->STORAGE (ENV->LOCAL ENV)))))
                           (V_INT->VAL (CDR (HONS-ASSOC-EQUAL "__stdlib_local_low"
                                                              (LOCAL-ENV->STORAGE (ENV->LOCAL ENV)))))
                           (V_INT->VAL
                            (CDR (HONS-ASSOC-EQUAL "__stdlib_local_high"
                                                   (LOCAL-ENV->STORAGE (ENV->LOCAL ENV)))))))))))



(define ilog2-safe-clock ((val rationalp))
  :guard (not (equal 0 val))
  :returns (clock posp :rule-classes :type-prescription)
  (let ((Val (abs val)))
    (+ 1 (max (max (acl2::rational-exponent val)
                   (- (acl2::rational-exponent val)))
              (if (<= 1 val)
                  (b* (((mv low high) (acl2::ilog2-search-up val 0 1)))
                    (- high low))
                (b* (((mv low high) (acl2::ilog2-search-down val -1 0)))
                  (- high low))))))
  ///
  (defthm ilog2-safe-clock-implies
    (implies (<= (ilog2-safe-clock val) clk)
             (and (< (acl2::Rational-exponent val) clk)
                  (< (- (acl2::rational-exponent val)) clk)
                  (implies (<= 1 (abs val))
                           (and (implies (<= 0 val)
                                         (< (b* (((mv low high) (acl2::ilog2-search-up val 0 1)))
                                              (+ (- low) high))
                                            clk))
                                (implies (<= val 0)
                                         (< (b* (((mv low high) (acl2::ilog2-search-up (- val) 0 1)))
                                              (+ (- low) high))
                                            clk))))
                  (implies (< (abs val) 1)
                           (and (implies (<= 0 val)
                                         (< (b* (((mv low high) (acl2::ilog2-search-down val -1 0)))
                                              (+ (- low) high))
                                            clk))
                                (implies (<= val 0)
                                         (< (b* (((mv low high) (acl2::ilog2-search-down (- val) -1 0)))
                                              (+ (- low) high))
                                            clk))))))))

(local
 (def-asl-subprogram ilog2-correct-lemma
   :function "ILog2"
   :args (val)
   :safe-clock (ilog2-safe-clock val.val)
   :hyps (and (not (eql 0 val.val))
              (<= (ilog2-safe-clock val.val) (expt 2 128)))
   :return-values ((v_int (acl2::ilog2 val.val)))
   :enable (acl2::ilog2)))



(local (defthmd rational-exponent-is-ilog2
         (implies (and (rationalp x)
                       (not (equal 0 x)))
                  (equal (acl2::rational-exponent x)
                         (acl2::ilog2 x)))
         :hints (("goal" :in-theory (enable acl2::ilog2-spec-is-rational-exponent)))))

(def-asl-subprogram ilog2-correct
  :function "ILog2"
  :args (val)
  :safe-clock (ilog2-safe-clock val.val)
  :hyps (and (not (eql 0 val.val))
             (<= (ilog2-safe-clock val.val) (expt 2 128)))
  :return-values ((v_int (acl2::rational-exponent val.val)))
  :no-expand-hint t
  :enable (rational-exponent-is-ilog2))

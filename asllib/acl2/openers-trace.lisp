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


(include-book "openers")
(include-book "trace-interp")




(local (acl2::def-ruleset openers-back (acl2::get-ruleset 'openers (w state))))

(defopener open-eval_stmt-*t eval_stmt-*t :hyp (syntaxp (quotep s)))
(defopener open-eval_stmt-*t1 eval_stmt-*t1 :hyp (syntaxp (quotep s)))
(defopener open-eval_block-*t eval_block-*t :hyp (syntaxp (quotep x)))

(defopener open-eval_expr-*t eval_expr-*t :hyp (syntaxp (quotep e)))
(defopener open-eval_call-*t eval_call-*t :hyp (syntaxp (quotep name)))
(defopener open-eval_expr_list-*t eval_expr_list-*t :hyp (syntaxp (quotep e)))
(defopener open-eval_lexpr-*t eval_lexpr-*t :hyp (syntaxp (quotep lx)))
(defopener open-eval_limit-*t eval_limit-*t :hyp (syntaxp (quotep x)))

(defopener open-eval_slice_list-*t eval_slice_list-*t :hyp (syntaxp (quotep sl)))
(defopener open-eval_slice-*t eval_slice-*t :hyp (syntaxp (quotep s)))

(defopener open-is_val_of_type-*t is_val_of_type-*t :hyp (syntaxp (quotep ty)))
(defopener open-check_int_constraints-*t check_int_constraints-*t :hyp (syntaxp (quotep constrs)))


(defopener open-resolve-ty-*t resolve-ty-*t :hyp (syntaxp (quotep x)))
(defopener open-resolve-tylist-*t resolve-tylist-*t :hyp (syntaxp (quotep x)))
(defopener open-resolve-typed_identifierlist-*t resolve-typed_identifierlist-*t :hyp (syntaxp (quotep x)))
(defopener open-resolve-constraint_kind-*t resolve-constraint_kind-*t :hyp (syntaxp (quotep x)))
(defopener open-resolve-int_constraints-*t resolve-int_constraints-*t :hyp (syntaxp (quotep x)))

(defopener open-eval_subprogram-*t1 eval_subprogram-*t1 :hyp (syntaxp (quotep name)))


(acl2::def-ruleset trace-openers (set-difference-equal
                                  (acl2::get-ruleset 'openers (w state))
                                  (acl2::get-ruleset 'openers-back (w state))))

(make-event
 (let ((rules (acl2::get-ruleset 'openers-back (w state))))
   `(table acl2::ruleset-table 'openers ',rules)))

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

(include-book "trace-interp")
(local (include-book "centaur/vl/util/default-hints" :dir :system))


(encapsulate nil
  (local (defthm xor-of-bool-fix
           (equal (xor (acl2::bool-fix x) y)
                  (xor x y))))
  (local (in-theory (acl2::e/d*
                     ()
                     (asl-*t-equals-original-rules
                      maybe-expr-some-of-fields
                      cons-equal
                      env-replace-static-with-self
                      (tau-system)
                      tracespec-p-when-maybe-tracespec-p
                      acl2::nfix-when-not-natp
                      acl2::append-to-nil
                      acl2::natp-when-gte-0
                      set::sets-are-true-lists-cheap
                      default-<-1 default-<-2
                      (:rules-of-class :type-prescription :here)
                      (:rules-of-class :definition :here))
                     ((:t stmt-fix)
                      (:t expr-fix)
                      atom lifix
                      maybe-expr-fix-when-some
                      maybe-expr-some
                      maybe-expr-some->val))))

  (with-output
    :off (event)
    :evisc (:gag-mode '(nil 5 8 nil))
    (fty::deffixequiv-mutual asl-interpreter-mutual-recursion-*t)))





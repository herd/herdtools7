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

(include-book "interp")
(local (std::add-default-post-define-hook :fix))





(define termination-error-p ((x eval_result-p))
  :returns (errp)
  (eval_result-case x
    :ev_error
    (and (member-equal x.desc
                       '("DE_LE: Recursion limit ran out"
                         "DE_LE: Loop limit ran out"
                         "DE_LE: Recursion limit ran out"
                         "Clock ran out resolving named type"))
         t)
    :otherwise nil)
  ///
  (defthm termination-error-p-of-ev_error
    (implies (syntaxp (quotep desc))
             (iff (termination-error-p (ev_error desc data backtrace))
                  (member-equal (acl2::str-fix desc)
                                '("DE_LE: Recursion limit ran out"
                                  "DE_LE: Loop limit ran out"
                                  "DE_LE: Recursion limit ran out"
                                  "Clock ran out resolving named type")))))

  (defthm termination-error-p-when-not-ev_error
    (implies (not (equal (eval_result-kind x) :ev_error))
             (not (termination-error-p x))))

  (defthm termination-error-p-of-init-backtrace
    (iff (termination-error-p (init-backtrace x storage pos))
         (termination-error-p x))
    :hints(("Goal" :in-theory (enable init-backtrace))))

  (defthm termination-error-p-of-change
    (implies (eval_result-case x :ev_error)
             (iff (termination-error-p (ev_error (ev_error->desc x) data backtrace))
                  (termination-error-p x)))))

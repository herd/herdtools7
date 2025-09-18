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

(include-book "../../interp")
(include-book "basic-theory")
(include-book "clause-processors/just-expand" :dir :system)

(defthm alistp-when-val-imap-p-rw
  (implies (val-imap-p x)
           (alistp x)))

(defthm alistp-when-func-ses-imap-p-rw
  (implies (func-ses-imap-p x)
           (alistp x)))

;; (defthm val-imap-p-of-fal-extract
;;   (implies (val-imap-p x)
;;            (val-imap-p (acl2::fal-extract keys x)))
;;   :hints(("Goal" :in-theory (enable acl2::fal-extract))))

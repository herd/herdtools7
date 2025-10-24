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

;; This *non-locally* includes many theorems needed for admitting and
;; guard-verifying an interpreter. Should be included locally only.

(include-book "ast")
(include-book "std/lists/repeat" :dir :system)
(include-book "centaur/vl/util/default-hints" :dir :system)

(defthm len-equal-0
  (equal (equal 0 (len x))
         (not (consp x)))
  :hints(("Goal" :in-theory (enable len))))

(defthm len-of-cons
  (Equal (len (cons x y))
         (+ 1 (len y))))


(defthm rationalp-when-integerp-rw
  (implies (integerp x)
           (rationalp x)))

(defthm assoc-equal-is-hons-assoc-equal
  (implies k
           (equal (assoc-equal k x)
                  (hons-assoc-equal k x)))
  :hints(("Goal" :in-theory (enable assoc-equal hons-assoc-equal))))

(defthm alistp-when-func-ses-imap-p-rw
  (implies (func-ses-imap-p x)
           (alistp x))
  :hints(("Goal" :in-theory (enable func-ses-imap-p))))

(defthm identifier-p-compound-recognizer
  (implies (identifier-p x)
           (stringp x))
  :hints(("Goal" :in-theory (enable identifier-p)))
  :rule-classes :compound-recognizer)


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

(in-package "CAT")

(include-book "ast")

(defsection varset-theory
   (defthm var-p-of-head
     (implies (and (varlist-p x)
                   (not (emptyp x)))
              (var-p (head x)))
     :hints(("Goal" :in-theory (enable head sfix))))

   (defthm varlist-p-of-tail
     (implies (varlist-p x)
              (varlist-p (tail x)))
     :hints(("Goal" :in-theory (enable tail sfix))))

   (defthm varlist-p-of-sfix
     (implies (varlist-p x)
              (varlist-p (sfix x)))
     :hints(("Goal" :in-theory (enable sfix))))

   (defthm varlist-p-of-insert
     (implies (and (var-p x)
                   (varlist-p y))
              (varlist-p (insert x y)))
     :hints(("Goal" :in-theory (enable insert))))

   (defthm varlist-p-of-union
     (implies (and (varlist-p x)
                   (varlist-p y))
              (varlist-p (union x y)))
     :hints(("Goal" :in-theory (enable union))))

   (defthm varlist-p-of-difference
     (implies (varlist-p x)
              (varlist-p (difference x y)))
     :hints(("Goal" :in-theory (enable difference))))

   (defthm varlist-p-of-mergesort
     (implies (varlist-p x)
              (varlist-p (mergesort x)))
     :hints(("Goal" :in-theory (enable mergesort))))

   (defthm varlist-p-of-intersect-1
     (implies (varlist-p x)
              (varlist-p (intersect x y)))
     :hints(("Goal" :in-theory (enable intersect))))

   (defthmd var-p-when-in
     (implies (and (varlist-p x)
                   (not (var-p v)))
              (not (in v x)))
     :hints(("Goal" :in-theory (enable in))))
   
   (defthm varlist-p-of-intersect-2
     (implies (varlist-p y)
              (varlist-p (intersect x y)))
     :hints(("Goal" :in-theory (enable intersect)
             :induct t)
            (and stable-under-simplificationp
                 '(:cases ((var-p (head x)))
                   :in-theory (enable var-p-when-in)))))

   (defthm varlist-p-of-delete
     (implies (varlist-p x)
              (varlist-p (delete v x)))
     :hints(("Goal" :in-theory (enable delete)))))

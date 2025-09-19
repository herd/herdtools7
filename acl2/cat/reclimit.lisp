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

(include-book "eval-ins")
(local (include-book "std/basic/arith-equivs" :dir :system))
(local (in-theory (disable acl2::zp-open
                           ACL2::ZP-FORWARD-TO-NAT-EQUIV-0)))

(local
 (progn
   (defun-sk eval-exp-of-greater-reclimit-cond (x ex env reclimit)
     (forall reclimit2
             (b* (((mv err res) (eval-exp x))
                  ((mv err2 res2) (eval-exp x :reclimit reclimit2)))
               (implies (and (syntaxp (not (equal reclimit reclimit2)))
                             (<= (nfix reclimit) (nfix reclimit2))
                             (not (equal (errmsg->name err) "Hit recursion limit")))
                        (and (equal err2 err)
                             (equal res2 res)))))
     :rewrite :direct)
   (in-theory (disable eval-exp-of-greater-reclimit-cond))

   (defun-sk eval-explist-of-greater-reclimit-cond (x ex env reclimit)
     (forall reclimit2
             (b* (((mv err res) (eval-explist x))
                  ((mv err2 res2) (eval-explist x :reclimit reclimit2)))
               (implies (and (syntaxp (not (equal reclimit reclimit2)))
                             (<= (nfix reclimit) (nfix reclimit2))
                             (not (equal (errmsg->name err) "Hit recursion limit")))
                        (and (equal err2 err)
                             (equal res2 res)))))
     :rewrite :direct)
   (in-theory (disable eval-explist-of-greater-reclimit-cond))

   (defun-sk eval-bindings-of-greater-reclimit-cond (x ex env reclimit)
     (forall reclimit2
             (b* (((mv err res) (eval-bindings x))
                  ((mv err2 res2) (eval-bindings x :reclimit reclimit2)))
               (implies (and (syntaxp (not (equal reclimit reclimit2)))
                             (<= (nfix reclimit) (nfix reclimit2))
                             (not (equal (errmsg->name err) "Hit recursion limit")))
                        (and (equal err2 err)
                             (equal res2 res)))))
     :rewrite :direct)
   (in-theory (disable eval-bindings-of-greater-reclimit-cond))

   (defun-sk eval-binding-of-greater-reclimit-cond (x ex env reclimit)
     (forall reclimit2
             (b* (((mv err res) (eval-binding x))
                  ((mv err2 res2) (eval-binding x :reclimit reclimit2)))
               (implies (and (syntaxp (not (equal reclimit reclimit2)))
                             (<= (nfix reclimit) (nfix reclimit2))
                             (not (equal (errmsg->name err) "Hit recursion limit")))
                        (and (equal err2 err)
                             (equal res2 res)))))
     :rewrite :direct)
   (in-theory (disable eval-binding-of-greater-reclimit-cond))

   (defun-sk eval-fixpoint-of-greater-reclimit-cond (x ex env reclimit)
     (forall reclimit2
             (b* (((mv err res) (eval-fixpoint x))
                  ((mv err2 res2) (eval-fixpoint x :reclimit reclimit2)))
               (implies (and (syntaxp (not (equal reclimit reclimit2)))
                             (<= (nfix reclimit) (nfix reclimit2))
                             (not (equal (errmsg->name err) "Hit recursion limit")))
                        (and (equal err2 err)
                             (equal res2 res)))))
     :rewrite :direct)
   (in-theory (disable eval-fixpoint-of-greater-reclimit-cond))

   (defun-sk eval-fixpoint1-of-greater-reclimit-cond (x ex env reclimit)
     (forall reclimit2
             (b* (((mv err res changedp) (eval-fixpoint1 x))
                  ((mv err2 res2 changedp2) (eval-fixpoint1 x :reclimit reclimit2)))
               (implies (and (syntaxp (not (equal reclimit reclimit2)))
                             (<= (nfix reclimit) (nfix reclimit2))
                             (not (equal (errmsg->name err) "Hit recursion limit")))
                        (and (equal err2 err)
                             (equal res2 res)
                             (equal changedp2 changedp)))))
     :rewrite :direct)
   (in-theory (disable eval-fixpoint1-of-greater-reclimit-cond))

   (defun-sk eval-fixpoint-binding-of-greater-reclimit-cond (x ex env reclimit)
     (forall reclimit2
             (b* (((mv err res changedp) (eval-fixpoint-binding x))
                  ((mv err2 res2 changedp2) (eval-fixpoint-binding x :reclimit reclimit2)))
               (implies (and (syntaxp (not (equal reclimit reclimit2)))
                             (<= (nfix reclimit) (nfix reclimit2))
                             (not (equal (errmsg->name err) "Hit recursion limit")))
                        (and (equal err2 err)
                             (equal res2 res)
                             (equal changedp2 changedp)))))
     :rewrite :direct)
   (in-theory (disable eval-fixpoint-binding-of-greater-reclimit-cond))

   (defun-sk eval-condition-of-greater-reclimit-cond (x ex env reclimit)
     (forall reclimit2
             (b* (((mv err res) (eval-condition x))
                  ((mv err2 res2) (eval-condition x :reclimit reclimit2)))
               (implies (and (syntaxp (not (equal reclimit reclimit2)))
                             (<= (nfix reclimit) (nfix reclimit2))
                             (not (equal (errmsg->name err) "Hit recursion limit")))
                        (and (equal err2 err)
                             (equal res2 res)))))
     :rewrite :direct)
   (in-theory (disable eval-condition-of-greater-reclimit-cond))))


(local
 (defthm-eval-exp-flag
   (defthm eval-exp-of-greater-reclimit-lemma
     (eval-exp-of-greater-reclimit-cond x ex env reclimit)
     :hints ((and stable-under-simplificationp
                  `(:expand (,(car (last clause))
                             (:free (reclimit) (eval-exp x))))))
     :flag eval-exp)

   (defthm eval-explist-of-greater-reclimit-lemma
     (eval-explist-of-greater-reclimit-cond x ex env reclimit)
     :hints ((and stable-under-simplificationp
                  `(:expand (,(car (last clause))
                             (:free (reclimit) (eval-explist x))))))
     :flag eval-explist)

   (defthm eval-bindings-of-greater-reclimit-lemma
     (eval-bindings-of-greater-reclimit-cond x ex env reclimit)
     :hints ((and stable-under-simplificationp
                  `(:expand (,(car (last clause))
                             (:free (reclimit) (eval-bindings x))))))
     :flag eval-bindings)

   (defthm eval-binding-of-greater-reclimit-lemma
     (eval-binding-of-greater-reclimit-cond x ex env reclimit)
     :hints ((and stable-under-simplificationp
                  `(:expand (,(car (last clause))
                             (:free (reclimit) (eval-binding x))))))
     :flag eval-binding)

   (defthm eval-fixpoint-of-greater-reclimit-lemma
     (eval-fixpoint-of-greater-reclimit-cond x ex env reclimit)
     :hints ((and stable-under-simplificationp
                  `(:expand (,(car (last clause))
                             (:free (reclimit) (eval-fixpoint x))))))
     :flag eval-fixpoint)

   (defthm eval-fixpoint1-of-greater-reclimit-lemma
     (eval-fixpoint1-of-greater-reclimit-cond x ex env reclimit)
     :hints ((and stable-under-simplificationp
                  `(:expand (,(car (last clause))
                             (:free (reclimit) (eval-fixpoint1 x))))))
     :flag eval-fixpoint1)

   (defthm eval-fixpoint-binding-of-greater-reclimit-lemma
     (eval-fixpoint-binding-of-greater-reclimit-cond x ex env reclimit)
     :hints ((and stable-under-simplificationp
                  `(:expand (,(car (last clause))
                             (:free (reclimit) (eval-fixpoint-binding x))))))
     :flag eval-fixpoint-binding)

   (defthm eval-condition-of-greater-reclimit-lemma
     (eval-condition-of-greater-reclimit-cond x ex env reclimit)
     :hints ((and stable-under-simplificationp
                  `(:expand (,(car (last clause))
                             (:free (reclimit) (eval-condition x))))))
     :flag eval-condition)
   :hints ((and stable-under-simplificationp
                '(:do-not-induct t)))))





(progn
  (defthm eval-exp-of-greater-reclimit
    (b* (((mv err res) (eval-exp x))
         ((mv err2 res2) (eval-exp x :reclimit reclimit2)))
      (implies (and (not (equal (errmsg->name err) "Hit recursion limit"))
                    (syntaxp (not (equal reclimit reclimit2)))
                    (<= (nfix reclimit) (nfix reclimit2)))
               (and (equal err2 err)
                    (equal res2 res))))
    :hints (("goal" :use eval-exp-of-greater-reclimit-lemma
             :in-theory (disable eval-exp-of-greater-reclimit-lemma))))

  (defthm eval-explist-of-greater-reclimit
    (b* (((mv err res) (eval-explist x))
         ((mv err2 res2) (eval-explist x :reclimit reclimit2)))
      (implies (and (not (equal (errmsg->name err) "Hit recursion limit"))
                    (syntaxp (not (equal reclimit reclimit2)))
                    (<= (nfix reclimit) (nfix reclimit2)))
               (and (equal err2 err)
                    (equal res2 res))))
    :hints (("goal" :use eval-explist-of-greater-reclimit-lemma
             :in-theory (disable eval-explist-of-greater-reclimit-lemma))))

  (defthm eval-bindings-of-greater-reclimit
    (b* (((mv err res) (eval-bindings x))
         ((mv err2 res2) (eval-bindings x :reclimit reclimit2)))
      (implies (and (not (equal (errmsg->name err) "Hit recursion limit"))
                    (syntaxp (not (equal reclimit reclimit2)))
                    (<= (nfix reclimit) (nfix reclimit2)))
               (and (equal err2 err)
                    (equal res2 res))))
    :hints (("goal" :use eval-bindings-of-greater-reclimit-lemma
             :in-theory (disable eval-bindings-of-greater-reclimit-lemma))))

  (defthm eval-binding-of-greater-reclimit
    (b* (((mv err res) (eval-binding x))
         ((mv err2 res2) (eval-binding x :reclimit reclimit2)))
      (implies (and (not (equal (errmsg->name err) "Hit recursion limit"))
                    (syntaxp (not (equal reclimit reclimit2)))
                    (<= (nfix reclimit) (nfix reclimit2)))
               (and (equal err2 err)
                    (equal res2 res))))
    :hints (("goal" :use eval-binding-of-greater-reclimit-lemma
             :in-theory (disable eval-binding-of-greater-reclimit-lemma))))

  (defthm eval-fixpoint-of-greater-reclimit
    (b* (((mv err res) (eval-fixpoint x))
         ((mv err2 res2) (eval-fixpoint x :reclimit reclimit2)))
      (implies (and (not (equal (errmsg->name err) "Hit recursion limit"))
                    (syntaxp (not (equal reclimit reclimit2)))
                    (<= (nfix reclimit) (nfix reclimit2)))
               (and (equal err2 err)
                    (equal res2 res))))
    :hints (("goal" :use eval-fixpoint-of-greater-reclimit-lemma
             :in-theory (disable eval-fixpoint-of-greater-reclimit-lemma))))

  (defthm eval-fixpoint1-of-greater-reclimit
    (b* (((mv err res changedp) (eval-fixpoint1 x))
         ((mv err2 res2 changedp2) (eval-fixpoint1 x :reclimit reclimit2)))
      (implies (and (not (equal (errmsg->name err) "Hit recursion limit"))
                    (syntaxp (not (equal reclimit reclimit2)))
                    (<= (nfix reclimit) (nfix reclimit2)))
               (and (equal err2 err)
                    (equal res2 res)
                    (equal changedp2 changedp))))
    :hints (("goal" :use eval-fixpoint1-of-greater-reclimit-lemma
             :in-theory (disable eval-fixpoint1-of-greater-reclimit-lemma))))

  (defthm eval-fixpoint-binding-of-greater-reclimit
    (b* (((mv err res changedp) (eval-fixpoint-binding x))
         ((mv err2 res2 changedp2) (eval-fixpoint-binding x :reclimit reclimit2)))
      (implies (and (not (equal (errmsg->name err) "Hit recursion limit"))
                    (syntaxp (not (equal reclimit reclimit2)))
                    (<= (nfix reclimit) (nfix reclimit2)))
               (and (equal err2 err)
                    (equal res2 res)
                    (equal changedp2 changedp))))
    :hints (("goal" :use eval-fixpoint-binding-of-greater-reclimit-lemma
             :in-theory (disable eval-fixpoint-binding-of-greater-reclimit-lemma))))

  (defthm eval-condition-of-greater-reclimit
    (b* (((mv err res) (eval-condition x))
         ((mv err2 res2) (eval-condition x :reclimit reclimit2)))
      (implies (and (not (equal (errmsg->name err) "Hit recursion limit"))
                    (syntaxp (not (equal reclimit reclimit2)))
                    (<= (nfix reclimit) (nfix reclimit2)))
               (and (equal err2 err)
                    (equal res2 res))))
    :hints (("goal" :use eval-condition-of-greater-reclimit-lemma
             :in-theory (disable eval-condition-of-greater-reclimit-lemma)))))

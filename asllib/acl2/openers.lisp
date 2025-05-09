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
(include-book "tools/easy-simplify" :dir :system)

(acl2::def-ruleset openers nil)

(defmacro defopener (name fn &key (hyp 't) (hint 'nil))
  `(encapsulate nil
     (set-ignore-ok t)
     (make-event
      (b* ((fn (acl2::deref-macro-name ',fn (acl2::macro-aliases (w state))))
           (formals (acl2::formals fn (w state)))
           (body (acl2::body fn nil (w state)))
           (hyp ',hyp)
           ;; ((er simp-body)
           ;;  (if ,simp
           ;;      (b* (((er new-hyp-term)
           ;;            (acl2::easy-simplify-term-fn hyp t ',hint 'iff t t 1000 1000 t state)))
           ;;        (acl2::easy-simplify-term-fn
           ;;         body new-hyp-term ',hint 'equal t t 1000 1000 t state))
           ;;    (value body)))
           (name ',name)
           (hint ',hint)
           (call `(,fn . ,formals))
           (concl `(equal ,call ,body))
           (thmbody (if (eq hyp t)
                        concl
                      `(implies ,hyp ,concl))))
        (value `(defthm ,name ,thmbody
                  :hints (("goal" :expand (,call)
                           . ,hint))))))
     (acl2::add-to-ruleset openers ,name)))


(defopener open-eval_stmt eval_stmt :hyp (syntaxp (quotep s)))
(defopener open-eval_block eval_block :hyp (syntaxp (quotep x)))

(defopener open-eval_expr eval_expr :hyp (syntaxp (quotep e)))
(defopener open-eval_binop eval_binop :hyp (syntaxp (quotep op)))
(defopener open-eval_unop eval_unop :hyp (syntaxp (quotep op)))
(defopener open-eval_call eval_call :hyp (syntaxp (quotep name)))
(defopener open-eval_expr_list eval_expr_list :hyp (syntaxp (quotep e)))
(defopener open-eval_lexpr eval_lexpr :hyp (syntaxp (quotep lx)))
(defopener open-eval_limit eval_limit :hyp (syntaxp (quotep x)))

(defopener open-eval_slice_list eval_slice_list :hyp (syntaxp (quotep sl)))
(defopener open-eval_slice eval_slice :hyp (syntaxp (quotep s)))

(defopener open-is_val_of_type is_val_of_type :hyp (syntaxp (quotep ty)))
(defopener open-check_int_constraints check_int_constraints :hyp (syntaxp (quotep constrs)))


(defopener open-resolve-ty resolve-ty :hyp (syntaxp (quotep x)))
(defopener open-resolve-tylist resolve-tylist :hyp (syntaxp (quotep x)))
(defopener open-resolve-typed_identifierlist resolve-typed_identifierlist :hyp (syntaxp (quotep x)))
(defopener open-resolve-constraint_kind resolve-constraint_kind :hyp (syntaxp (quotep x)))
(defopener open-resolve-int_constraints resolve-int_constraints :hyp (syntaxp (quotep x)))




(defopener open-ty-satisfied ty-satisfied
  :hyp (syntaxp (or (quotep ty)
                    (case-match ty
                      (('ty (ctor . &))
                       (member-eq ctor
                                  '(t_int t_bits t_real t_string t_bool t_enum
                                          t_tuple t_array t_record t_exception
                                          t_collection t_named)))
                      (& nil)))))
(defopener open-tuple-type-satisfied tuple-type-satisfied :hyp (syntaxp (or (quotep types)
                                                                            (case-match types
                                                                              (('cons . &) t) (& nil)))))
(defopener open-array-type-satisfied array-type-satisfied)
(defopener open-record-type-satisfied record-type-satisfied :hyp (syntaxp (or (quotep fields)
                                                                              (case-match fields
                                                                                (('cons . &) t) (& nil)))))
(defopener open-constraint_kind-satisfied constraint_kind-satisfied
  :hyp (syntaxp (or (quotep c)
                    (and (consp c)
                         (member-eq (car c)
                                    '(unconstrained wellconstrained pendingconstrained parametrized))))))
(defopener open-int_constraintlist-satisfied int_constraintlist-satisfied
  :hyp (syntaxp (or (quotep c)
                    (case-match c
                      (('cons . &) t) (& nil)))))
(defopener open-int_constraint-satisfied int_constraint-satisfied
  :hyp (syntaxp (or (quotep c)
                    (and (consp c)
                         (member-eq (car c) '(constraint_exact constraint_range))))))

(defopener open-int-literal-expr->val int-literal-expr->val)

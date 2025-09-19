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

(include-book "ast")
(include-book "centaur/fty/visitor" :dir :System)
(include-book "centaur/misc/starlogic" :dir :system)

(acl2::def-b*-binder readonly-return
  :decls ((declare (ignore acl2::args)))
  :body `(and ,@acl2::forms
              ,acl2::rest-expr))

(fty::defvisitor-template readonly ((x :object))
  :returns (readonlyp ;; (:join readonlyp1
                      ;;  :tmp-var readonlyp1
                      ;;  :initial t)
            (:ignore :return t))
  :binder readonly-return
  :renames ((expr_desc readonly-expr_desc-p-aux))
  :type-fns ((expr_desc readonly-expr_desc-p))
  :fnname-template readonly-<type>-p)

(fty::defvisitor-multi readonly
  (define readonly-expr_desc-p ((x expr_desc-p))
    :measure (acl2::nat-list-measure (list (expr_desc-count x) 1))
    (expr_desc-case x
      :e_call nil
      :e_arbitrary nil
      :otherwise (readonly-expr_desc-p-aux x)))

  (fty::defvisitor :template readonly
    :type expr
    :measure (acl2::nat-list-measure (list :count 0))))
    
  

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

(include-book "../../ast")
(include-book "std/util/defconsts" :dir :System)

; (depends-on "stdlib-ast.lsp")
(defconsts (*stdlib-static-env* *stdlib-ast* state)
  (b* (((mv err obj state) (acl2::read-file "stdlib-ast.lsp" state)))
    (and err (er hard? 'static-env err))
    (and (not (and (consp obj)
                   (consp (car obj))
                   (not (cdr obj))))
         (er hard? 'static-env "Object read should have been a singleton list containing a cons"))
    (mv (caar obj) (cdar obj) state)))

(make-event
`(define stdlib-static-env ()
   :returns (static-env static_env_global-p)
   *stdlib-static-env*
   ///
   (in-theory (disable (stdlib-static-env)))))

(make-event
`(define stdlib-ast ()
   :returns (ast ast-p)
   *stdlib-ast*
   ///
   (in-theory (disable (stdlib-ast)))))


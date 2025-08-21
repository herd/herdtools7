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
(include-book "static-env-preserved")
(local (include-book "centaur/vl/util/default-hints" :dir :system))

(local
 (with-output
   ;; makes it so it won't take forever to print the induction scheme
   :evisc (:gag-mode (evisc-tuple 3 4 nil nil))
   :off (event)
   (make-event
    (b* (((std::defines-guts guts)
          (cdr (assoc 'asl-interpreter-mutual-recursion-*t (std::get-defines-alist (w state))))))
      `(flag::make-flag asl-interpreter-mutual-recursion-*t-flag
                        eval_expr-*t-fn
                        :flag-mapping ,guts.flag-mapping)))))


(local (in-theory (acl2::disable* (:ruleset asl-*t-equals-original-rules))))

(local (defun replace-static-env (x)
           (if (atom x)
               x
             (if (equal x '(global-env->static (env->global env)))
                 '(static_env_global-fix static-env)
               (cons (replace-static-env (car x))
                     (replace-static-env (cdr x)))))))

(with-output
  ;; makes it so it won't take forever to print the induction scheme
  :evisc (:gag-mode (evisc-tuple 3 4 nil nil))
  :off (event)
  (make-event (append (replace-static-env *static-env-preserved-form*)
                      '(:hints ((vl::big-mutrec-default-hint 'eval_expr-*t-fn id nil world))
                        :mutual-recursion asl-interpreter-mutual-recursion-*t))))

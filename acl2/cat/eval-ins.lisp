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

(include-book "eval-exp")
(local (include-book "std/basic/arith-equivs" :dir :System))
(local (std::add-default-post-define-hook :fix))

;; (acl2::def-b*-binder insval
;;   :parents (cat-interpreter-functions)
;;   :short "Binds an error and a @(see result) structure, returning immediately if it is an
;; error or the result judgement is forbidden, otherwise continuing the
;; computation."
;;   :body
;;   `(b* (((mv err result . ,(cdr acl2::args)) . ,acl2::forms)
;;         ((when (or err (eq (result->judgement result) :forbidden)))
;;          (mv err (and (not err) result))))
;;      (b* ((,(car acl2::args) result))
;;        ,acl2::rest-expr)))


(define withfrom-results ((var var-p)
                          (rels vallist-p)
                          (result result-p))
  :returns (results resultlist-p)
  (if (atom rels)
      nil
    (cons (change-result result :env
                         (env-update var
                                     (car rels)
                                     (result->env result)))
          (withfrom-results var (cdr rels) result))))


(define eval-do_test ((val val-p)
                      (test do_test-p))
  :returns (mv err satisfied)
  (case (do_test-fix test)
    (:acyclic (val-case val
                :v_rel (norm (test-acyclic val.rel))
                :v_empty (norm t)
                :otherwise (err "Bad test expression" (msg "~x0 value for ~x1 test"
                                                           (val-kind val) :acyclic))))
    (:irreflexive (val-case val
                    :v_rel (norm (test-irreflexive val.rel))
                    :v_empty (norm t)
                    :otherwise (err "Bad test expression" (msg "~x0 value for ~x1 test"
                                                               (val-kind val) :irreflexive))))
    (t (val-case val
         :v_rel (norm (atom val.rel))
         :v_empty (norm t)
         :v_set (norm (atom val.evts))
         :v_valset (norm (atom val.elts))
         :otherwise (err "Bad test expression" (msg "~x0 value for ~x1 test"
                                                    (val-kind val) :testempty))))))

(define eval-test ((val val-p)
                   (test test-p))
  :returns (mv err satisfied)
  (test-case test
    :t_yes (eval-do_test val test.test)
    :t_no (b* (((expval satisfied) (eval-do_test val test.test)))
            (norm (not satisfied)))))

(define resultlist-replace-env ((x resultlist-p)
                                (env env-p))
  :returns (new-x resultlist-p)
  (if (Atom x)
      nil
    (cons (change-result (car x) :env env)
          (resultlist-replace-env (cdr x) env))))

(defines eval-ins
  :flag-local nil
  (define eval-ins ((x ins-p)
                    &key
                    ((ex execgraph-p) 'ex)
                    ((result result-p) 'result)
                    ((reclimit natp) 'reclimit))
    :returns (mv err (res resultlist-p))
    :measure (acl2::nat-list-measure (list reclimit (ins-count x) 0))
    :verify-guards nil
    (b* (((result result) (result-fix result))
         ((when (eq result.judgement :forbidden))
          (norm (list result))))
      (ins-case x
        :i_let (b* (((expval env) (eval-bindings x.bindings :env result.env)))
                 (norm (list (change-result result :env env))))
        :i_rec (b* ((env result.env)
                    ((expval env)
                     (if (function-bindings-p x.bindings)
                         (b* ((fn-env (recursive-function-bindings-to-closures x.bindings env)))
                           (norm (env-combine fn-env env)))
                       (b* ((env (add-empty-bindings-to-env x.bindings env))
                            ((expval env) (eval-fixpoint x.bindings)))
                         (norm env))))
                    ((when x.test) (err "Unimplemented" "Recursive definition with test")))
                 (norm (list (change-result result :env env))))
        :i_test (b* (((app_test x.test))
                     ((expval val) (eval-exp x.test.exp :env result.env))
                     ((expval ok) (eval-test val x.test.test))
                     ((when ok) (norm (list result)))
                     ((when x.test.name)
                      (norm (list (change-result result :flags (cons x.test.name result.flags))))))
                  (norm (list (change-result result :judgement :forbidden))))
        :i_procedure (b* ((proc (v_proc x.formals x.body result.env))
                          (env (env-update x.name proc result.env)))
                       (norm (list (change-result result :env env))))
        :i_call      (b* (((expval proc) (b* ((look (env-lookup x.proc result.env))
                                              ((unless look)
                                               (err "Unbound var" x.proc)))
                                           (norm look))))
                       (val-case proc
                         :v_proc
                         (b* (((expval arg) (eval-exp x.arg :env result.env))
                              ((expval argenv) (match-pat proc.formals arg))
                              ((when (zp reclimit))
                               (err "Hit recursion limit"
                                    (msg "in procedure call of" x.proc)))
                              ((expval rslts)
                               (eval-inslist proc.body
                                             :result (change-result result :env (env-combine argenv result.env))
                                             :reclimit (1- reclimit))))
                           (norm (resultlist-replace-env rslts result.env)))
                         :otherwise (err "Bad procedure call"
                                         (msg "unexpected procedure value ~x0" proc))))
        :i_withfrom (b* (((expval rels) (eval-exp x.rels :env result.env)))
                      (val-case rels
                        :v_valset (norm (withfrom-results x.name rels.elts result))
                        :otherwise (err "Type mismatch" (msg "~x0 expected ~x1 but got ~x2"
                                                             :i_withfrom :v_valset (val-kind rels)))))
        :otherwise (err "Unimplemented" (msg "~x0 instruction" (ins-kind x))))))

  (define eval-inslist ((x inslist-p)
                        &key
                        ((ex execgraph-p) 'ex)
                        ((result result-p) 'result)
                        ((reclimit natp) 'reclimit))
    :returns (mv err (res resultlist-p))
    :measure (acl2::nat-list-measure (list reclimit (inslist-count x) 0))
    (b* (((when (atom x)) (norm (list (result-fix result))))
         ((expval results) (eval-ins (car x))))
      (eval-inslist* (cdr x))))

  (define eval-inslist* ((x inslist-p)
                         &key
                         ((ex execgraph-p) 'ex)
                         ((results resultlist-p) 'results)
                         ((reclimit natp) 'reclimit))
    :returns (mv err (res resultlist-p))
    :measure (acl2::nat-list-measure (list reclimit (inslist-count x) (len results)))
    (b* (((when (atom results)) (norm nil))
         ((expval first) (eval-inslist x :result (car results)))
         ((expval rest) (eval-inslist* x :results (cdr results))))
      (norm (append first rest))))
  ///
  (verify-guards eval-ins-fn)

  (fty::deffixequiv-mutual eval-ins))




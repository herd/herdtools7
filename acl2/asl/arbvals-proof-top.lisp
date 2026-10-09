;;****************************************************************************;;
;;                                ASLRef                                      ;;
;;****************************************************************************;;
;; SPDX-FileCopyrightText: Copyright 2025 Arm Limited and/or its affiliates <open-source-office@arm.com>
;; SPDX-License-Identifier: BSD-3-Clause

(in-package "ASL")

(include-book "arbvals")
(include-book "arbvals-to-orac")
(include-book "orac-to-arbvals")
(local (include-book "arbvals-to-orac-proof"))
(local (include-book "orac-to-arbvals-proof"))

(define eval_subprogram-orac-to-arbmap ((env env-p)
                                        (name identifier-p)
                                        (vparams vallist-p)
                                        (vargs vallist-p)
                                        &key
                                        ((clk natp) 'clk)
                                        ((arbaddr arbaddr-p) 'arbaddr)
                                        (orac 'orac))
  :returns (arbmap arbmap-p)
  :verify-guards nil
  (b* (((mv & & arbmap)
        (non-exec (eval_subprogram-*ota env name vparams vargs))))
    arbmap)
  ///
  (defretd eval_subprogram-*a-implements-eval_subprogram
    (equal (eval_subprogram-*a env name vparams vargs)
           (mv-nth 0 (eval_subprogram env name vparams vargs)))))

(define eval_subprogram-arbmap-to-orac ((env env-p)
                                        (name identifier-p)
                                        (vparams vallist-p)
                                        (vargs vallist-p)
                                        &key
                                        ((clk natp) 'clk)
                                        ((arbaddr arbaddr-p) 'arbaddr)
                                        ((arbmap arbmap-p) 'arbmap)
                                        (orac 'orac))
  :returns (new-orac)
  :verify-guards nil
  (b* (((mv & types vals)
        (eval_subprogram-*ato env name vparams vargs))
       (orac (acl2::update-oracle-mode 3 orac))
       (orac (acl2::update-oracle-st (typed-vallist-to-oracle types vals) orac)))
    orac)
  ///
  (defretd eval_subprogram-implements-eval_subprogram-*a
    (equal (mv-nth 0 (eval_subprogram env name vparams vargs :orac new-orac))
           (eval_subprogram-*a env name vparams vargs))
    :hints (("goal" :use ((:instance original-replicates-eval_subprogram-*ato
                           (orac (eval_subprogram-arbmap-to-orac env name vparams vargs))))
             :in-theory (disable original-replicates-eval_subprogram-*ato)))))


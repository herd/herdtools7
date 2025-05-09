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
(include-book "ihs/logops-definitions" :dir :system)
(local (include-book "std/lists/nth" :dir :system))
(local (include-book "std/lists/repeat" :dir :system))
(local (include-book "std/lists/take" :dir :system))
(local (include-book "centaur/bitops/ihsext-basics" :dir :system))

(local (table fty::deftagsum-defaults :short-names t))
(local (in-theory (disable (tau-system))))


(deftypes val
  (deftagsum val
    (:v_int ((val integerp :rule-classes :type-prescription)))
    (:v_bool ((val booleanp)))
    (:v_real ((val rationalp :rule-classes :type-prescription)))
    (:v_string ((val stringp :rule-classes :type-prescription)))
    (:v_bitvector ((len natp :rule-classes :type-prescription)
                   (val integerp :reqfix (loghead len val)))
     :require (unsigned-byte-p len val))
    (:v_label ((val identifier-p)))
    (:v_record ((rec val-imap)))
    (:v_array  ((arr vallist))))
  (fty::deflist vallist :elt-type val :true-listp t)
  (fty::defmap val-imap :key-type identifier :val-type val :true-listp t)
  ///
  
  (defthm val-imap-p-of-pairlis$
    (implies (and (identifierlist-p keys)
                  (vallist-p vals)
                  (equal (len keys) (len vals)))
             (val-imap-p (pairlis$ keys vals))))

  (defthm vallist-p-of-update-nth
    (implies (and (vallist-p x)
                  (val-p v)
                  (<= (nfix n) (len x)))
             (vallist-p (update-nth n v x)))
    :hints(("Goal" :in-theory (enable update-nth vallist-p))))

  (defthm val-imap-p-of-put-assoc-equal
    (implies (and (val-imap-p x)
                  (identifier-p k)
                  (val-p v))
             (val-imap-p (put-assoc-equal k v x)))
    :hints(("Goal" :in-theory (enable put-assoc-equal)))))

(fty::defoption maybe-val val)


(fty::defmap pos-imap :key-type identifier :val-type posp :true-listp t)

(defprod global-env
  ((static static_env_global)
   (storage val-imap)
   (stack_size pos-imap))
  :layout :list)

(defprod unit () :layout :list
  ///
  (defthm unit-p-compound-recognizer
    (implies (unit-p x)
             (equal x nil))
    :hints(("Goal" :in-theory (enable unit-p)))
    :rule-classes :compound-recognizer))

(in-theory (enable (:t unit)
                   (:t unit-fix)))

(fty::deflist integer-list :pred integer-listp :elt-type integerp :true-listp t
  :elementp-of-nil nil)


(defprod local-env
  ((storage val-imap)
   (scope unit)
   (unroll integer-list)
   (declared identifierlist))
  :layout :list)

(define empty-local-env ()
  :returns (empty local-env-p)
  (make-local-env))


(defprod env
  ((global global-env)
   (local local-env))
  :layout :list)


(defprod val_read_from
  ((val val)
   (name identifier)
   (scope unit))
  :layout :list)

(fty::deflist val_read_from-list :elt-type val_read_from :true-listp t)

(defprod throwdata
  ((val val-p)
   (ty ty))
  :layout :list)




(defoption maybe-throwdata throwdata)

(deftagsum eval_result
  (:ev_normal (res))
  (:ev_throwing ((throwdata maybe-throwdata)
                 (env env)))
  (:ev_error    ((desc stringp)
                 (data))))


(defmacro def-eval_result (pred res-pred)
  `(define ,pred (x)
     (and (eval_result-p x)
          (eval_result-case x
            :ev_normal (,res-pred x.res)
            :otherwise t))
     ///
     (defthm ,(intern-in-package-of-symbol
               (concatenate 'string (symbol-name pred) "-IMPLIES") pred)
       (implies (,pred x)
                (and (eval_result-p x)
                     (implies (eval_result-case x :ev_normal)
                              (,res-pred (ev_normal->res x))))))

     (defthm ,(intern-in-package-of-symbol
               (concatenate 'string (symbol-name pred) "-WHEN-EVAL_RESULT-P") pred)
       (implies (and (eval_result-p x)
                     (or (not (eval_result-case x :ev_normal))
                         (,res-pred (ev_normal->res x))))
                (,pred x)))))


(defprod intpair/env
  ((pair intpair-p)
   (env  env-p))
  :layout :fulltree)

(def-eval_result slice_eval_result-p intpair/env-p)

(defprod intpairlist/env
  ((pairlist intpairlist-p)
   (env  env-p))
  :layout :fulltree)

(def-eval_result slices_eval_result-p intpairlist/env-p)



(acl2::def-b*-binder ev
  :body
  `(b* ((evresult ,(car acl2::forms)))
     (eval_result-case evresult
       :ev_normal (b* ,(and (not (eq (car acl2::args) '&))
                            `((,(car acl2::args) evresult.res)))
                    ,acl2::rest-expr)
       :otherwise evresult)))


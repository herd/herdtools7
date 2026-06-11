;;****************************************************************************;;
;;                              ACL2 ASL Library                              ;;
;;****************************************************************************;;
;;
;; SPDX-FileCopyrightText: Copyright 2026 Arm Limited and/or its affiliates <open-source-office@arm.com>
;; SPDX-License-Identifier: BSD-3-Clause
;; 
;; ****************************************************************************;;



(in-package "ASL")
(include-book "centaur/fgl/def-fgl-rewrite" :dir :system)
(include-book "centaur/fgl/constraint-db" :dir :system)
(include-book "defs")
(local (std::add-default-post-define-hook :fix))




;; ----------------------------------------------------------------------------------
;; Non-vars-match-init-env:
;; This is intended as a hyp that constrains the values in storage
;; to the initial values computed by eval-globals, for any identifier not declared with 'var',
;; that is, constants, configs, and lets.


(define non-vars-match-init-env-aux ((stypes ty-global_decl_keyword-imap-p)
                                    (storage val-imap-p)
                                    (init-env env-p))
  :verify-guards nil
  (b* (((when (atom stypes)) t)
       ((unless (mbt (and (consp (car stypes))
                          (identifier-p (caar stypes)))))
        (non-vars-match-init-env-aux (cdr stypes) storage init-env))
       (name (caar stypes))
       (kw (ty-global_decl_keyword->kw (cdar stypes)))
       ((when (eq kw :gdk_var))
        (non-vars-match-init-env-aux (cdr stypes) storage init-env))
       (pair (omap::assoc name (global-env->storage (env->global init-env))))
       ((unless pair)
        (non-vars-match-init-env-aux (cdr stypes) storage init-env))
       (storage-val (val-imap-lookup name storage))
       (init-val (cdr pair)))
    (and (equal storage-val init-val)
         (non-vars-match-init-env-aux (cdr stypes) storage init-env)))
  ///
  (defthmd non-vars-match-init-env-aux-implies
    (implies (and (non-vars-match-init-env-aux stypes storage init-env)
                  (not (equal (ty-global_decl_keyword->kw (cdr (hons-assoc-equal (identifier-fix name) stypes))) :gdk_var))
                  (val-imap-has-key name (global-env->storage (env->global init-env))))
             (equal (val-imap-lookup name storage)
                    (val-imap-lookup name (global-env->storage (env->global init-env)))))
    :hints(("Goal" :in-theory (enable hons-assoc-equal))
           (and stable-under-simplificationp
                '(:in-theory (enable val-imap-lookup
                                     val-imap-has-key
                                     omap::lookup)))))

  (local (in-theory (enable ty-global_decl_keyword-imap-fix))))


(define trigger-non-var-match-constraint (name storage init-env)
  :ignore-ok t
  :irrelevant-formals-ok t
  nil
  ///
  (fgl::remove-fgl-rewrite trigger-non-var-match-constraint))


;; NOTE: In order to create counterexamples that contain the right config/constant/let
;; values, we used to just add all the configs in the init-env
;; into the storage using a fixup routine.  The scheme here using FGL boolean
;; constraints avoids the need for this, because it stores these equalities as
;; always-true Boolean variables which are then visible to counterexample
;; generation.

;; The way it works: the non-vars-match-init-env-implies rewrite rule triggers
;; whenever we look up a variable from storage, and if we have a
;; non-vars-match-init-env assumption with an init-env in which the variable is
;; a config, we rewrite the lookup to the lookup from the init-env. However, as
;; a side effect during the rewriting of the RHS, we also trigger constraint
;; generation based on this trigger-non-var-match-constraint stub function. This
;; triggers the constraint rule non-var-match-constraint, which adds that same
;; equivalence under the non-vars-match-init-env assumption.

(define non-vars-match-init-env ((storage val-imap-p)
                                (init-env env-p))
  :verify-guards nil
  (non-vars-match-init-env-aux (static_env_global->storage_types
                               (global-env->static (env->global init-env)))
                              storage
                              init-env)
  ///
  (fgl::def-fgl-rewrite non-vars-match-init-env-implies
    (implies (and (fgl::match-assums (non-vars-match-init-env storage init-env))
                  (not (equal (ty-global_decl_keyword->kw
                               (cdr (hons-assoc-equal (identifier-fix name)
                                                      (static_env_global->storage_types
                                                       (global-env->static (env->global init-env))))))
                              :gdk_var)))
             (equal (val-imap-lookup (fgl::concrete name) storage)
                    (let* ((ignore (fgl::trigger-constraints
                                    ;; see note about trigger-non-var-match-constraint above
                                    (trigger-non-var-match-constraint name storage init-env)))
                           (pair (omap::assoc name (global-env->storage (env->global init-env)))))
                      (declare (ignore ignore))
                      (if pair
                          (cdr pair)
                        (fgl::abort-rewrite (val-imap-lookup (fgl::concrete name) storage))))))
    :hints(("Goal" :use ((:instance non-vars-match-init-env-aux-implies
                          (stypes (static_env_global->storage_types
                                                       (global-env->static (env->global init-env)))))))
           (and stable-under-simplificationp
                '(:in-theory (enable val-imap-lookup
                                     val-imap-has-key
                                     omap::lookup)))))

  (set-ignore-ok t) ;; trigger isn't used
  (fgl::def-fgl-boolean-constraint non-var-match-constraint
    :bindings ((trigger    (trigger-non-var-match-constraint name storage init-env)))
    :body (let ((pair (omap::assoc name (global-env->storage (env->global init-env)))))
            (implies (and (non-vars-match-init-env storage init-env)
                          (not (equal (ty-global_decl_keyword->kw
                                       (cdr (hons-assoc-equal (identifier-fix name)
                                                              (static_env_global->storage_types
                                                               (global-env->static (env->global init-env))))))
                                      :gdk_var))
                          pair)
                   ;; we use fgl-hide here because we don't want to trigger the
                   ;; non-vars-match-init-env-implies rule (or any others).
                   (equal (fgl::fgl-hide (val-imap-lookup name storage))
                          (cdr pair))))
    :hints(("Goal" :use non-vars-match-init-env-implies)))

  (fgl::remove-fgl-rewrites non-vars-match-init-env))

(table fgl::magitastic-ev-definitions
       'non-vars-match-init-env
       (list '(storage init-env)
             ''t))


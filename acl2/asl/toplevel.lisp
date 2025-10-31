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
(local (std::add-default-post-define-hook :fix))

(defxdoc asl
  :parents (acl2::top)
  :short "ASL interpreter and proofs in ACL2 (umbrella topic)")

(defxdoc asl-interpreter-functions
  :parents (asl)
  :short "ASL interpreter implementation functions (umbrella topic)")

(defxdoc asl-interpreter-main-functions
  :parents (asl)
  :short "Main ASL interpreter functions (umbrella topic)")



(local (xdoc::set-default-parents asl-interpreter-functions))

(defconst *eval_global-initial-clock* 1000000)

(defconst *main-initial-clock* 1000000)



(define declare_global ((env env-p)
                        (name identifier-p)
                        (val val-p))
  :short "Declare a new global variable and assign it the given value, adding it to the
env's global storage alist"
  :returns (new-env env-p)
  (b* (((env env))
       ((global-env g) env.global)
       (new-storage (omap::update (identifier-fix name) (val-fix val)
                                  g.storage)))
    (change-env env :global (change-global-env g :storage new-storage))))

(define eval_global ((env env-p)
                     (x decl-p)
                     &key (orac 'orac))
  :returns (mv (res env_eval_result-p) new-orac)
  :short "If the given declaration is a global storage declaration, evaluate the
declaration, adding the new global variable with its computed initial value."
  (b* ((x (decl->desc x)))
    (decl_desc-case x
      :d_globalstorage
      (b* (((global_decl d) x.decl)
           ((unless d.initial_value)
            (evo_error "TypeInferenceNeeded" x nil))
           ((mv (evo (expr_result iv)) orac) (eval_expr env d.initial_value :clk *eval_global-initial-clock*)))
        (evo_normal (declare_global  iv.env d.name iv.val)))
      :otherwise (evo_normal (env-fix env)))))

(define eval_globals ((env env-p)
                      (x ast-p)
                      &key (orac 'orac))
  :short "Evaluate all the global storage declarations in the given AST, initializing
global variables and producing a new environment."
  :measure (len x)
  :returns (mv (res env_eval_result-p) new-orac)
  (b* (((when (atom x)) (evo_normal (env-fix env)))
       ((mv (evo env2) orac) (eval_global env (car x))))
    (eval_globals env2 (cdr x))))


(define find_main-aux ((names identifierlist-p)
                       (static-env static_env_global-p))
  :returns (mains (and (eval_result-p mains)
                      (implies (eval_result-case mains :ev_normal)
                               (identifierlist-p (ev_normal->res mains)))))
  (b* (((when (atom names)) (ev_normal nil))
       (name1 (identifier-fix (car names)))
       (look (hons-assoc-equal name1 (static_env_global->subprograms static-env)))
       ((unless look)
        (make-ev_error :desc "Couldn't find function" :data name1))
       ((func f) (func-ses->fn (cdr look)))
       ((ev rest) (find_main-aux (cdr names) static-env))
       ((when (and (not f.parameters) (not f.args)))
        (ev_normal (cons name1 rest))))
    (ev_normal rest)))
    

(define find_main ((static-env static_env_global-p))
  :returns (main (and (eval_result-p main)
                      (implies (eval_result-case main :ev_normal)
                               (identifier-p (ev_normal->res main)))))
  (b* ((names (cdr (hons-assoc-equal "main" (static_env_global->overloaded_subprograms static-env))))
       ((ev mains) (find_main-aux names static-env))
       ((when (atom mains))
        (make-ev_error :desc "No main function found"))
       ((when (consp (cdr mains)))
        (make-ev_error :desc "Multiple main functions found")))
    (ev_normal (car mains))))




(local (defthm equal-len
         (implies (syntaxp (quotep n))
                  (Equal (equal (len x) n)
                         (if (zp n)
                             (and (equal n 0) (atom x))
                           (and (consp x)
                                (equal (len (cdr x)) (1- n))))))))

(define run ((tenv static_env_global-p) (ast ast-p) &key (orac 'orac))
  :short "Run a toplevel ASL program, first initializing all declared global variables
and then evaluating the \"main\" subprogram."
  :parents (asl-interpreter-functions asl-interpreter-main-functions)
  :returns (mv (val val_result-p) new-orac)
  (b* ((env0 (make-env :local (empty-local-env)
                       :global (make-global-env :static tenv)))
       ((mv (evo env1) orac) (eval_globals env0 ast))
       ((evo main) (find_main tenv))
       ((mv (evo (func_result res)) orac)
        (eval_subprogram env1 main nil nil :clk *main-initial-clock*))
       ((when (eql (len res.vals) 1))
        (evo_normal (car res.vals))))
    (evo_error "malformed return values from main" res.vals nil)))

    

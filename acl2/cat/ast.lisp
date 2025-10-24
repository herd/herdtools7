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


(include-book "centaur/fty/deftypes" :dir :system)
(include-book "centaur/fty/basetypes" :dir :system)
(include-book "std/util/defenum" :dir :system)
(include-book "std/basic/two-nats-measure" :dir :system)
(local (in-theory (disable (tau-system))))
(local (table fty::deftagsum-defaults :short-names t))

(defxdoc cat-ast
  :parents (cat)
  :short "Format of the CAT AST")

(local (xdoc::set-default-parents cat-ast))


(defmacro def-type-alias (new-type existing-type &rest args)
  `(defprod ,new-type
     ((val ,existing-type))
     :layout :fulltree
     . ,args))

(def-type-alias var string
  :short "Alias for string, but used for variables in CAT ASTs.")

(deflist varlist :elt-type var :true-listp t)

(def-type-alias tag string
  :short "Alias for string, but used for enum tags in CAT ASTs.")

(deflist taglist :elt-type tag :true-listp t)


(defenum set_or_rln-p (:set :rln))

(defenum op2-p
  (:union
   :inter
   :diff
   :seq
   :cartesian
   :add
   :tuple)
  :short "CAT binary (and higher) operators")

(defenum op1-p
  (:plus
   :star
   :opt
   :comp
   :inv
   :toid)
  :short "CAT unary operators")

(deftagsum konst
  (:empty ((setrel set_or_rln-p)))
  (:universe ((setrel set_or_rln-p))))


;; ??
(defenum scope-p
  (:device :kernel :work_group :sub_group :work_item))


(defoption pat0 var)

(deflist pat0-list :elt-type pat0 :true-listp t)

(deftagsum pat
  (:pvar ((var pat0)))
  (:ptuple ((tuple pat0-list))))

(deftagsum variant_cond
  (:variant ((variant string)))
  (:var_opnot ((arg variant_cond)))
  (:var_opand ((arg1 variant_cond)
               (arg2 variant_cond)))
  (:var_opor  ((arg1 variant_cond)
               (arg2 variant_cond))))

(deftypes exp
  (deftagsum exp
    (:e_konst       ((loc)
                     (val konst)))
    (:e_tag         ((loc)
                     (val tag)))
    (:e_var         ((loc)
                     (var var)))
    (:e_op1         ((loc)
                     (op op1-p)
                     (arg exp)))
    (:e_op          ((loc)
                     (op op2-p)
                     (args explist)))
    (:e_app         ((loc)
                     (fn exp)
                     (arg exp)))
    (:e_bind        ((loc)
                     (bindings bindinglist)
                     (body exp)))
    (:e_bindrec     ((loc)
                     (bindings bindinglist)
                     (body exp)))
    (:e_fun         ((loc)
                     (formals pat)
                     (body exp)
                     (name var)
                     (free varlist)))
    (:e_explicitset ((loc)
                     (elems explist)))
    (:e_match       ((loc)
                     (arg exp)
                     (clauses matchclauselist)
                     (otherwise maybe-exp)))
    (:e_matchset    ((loc)
                     (arg exp)
                     (empty exp)
                     (nonempty set_clause)))
    (:e_try         ((loc)
                     (arg1 exp)
                     (arg2 exp))) ;; ??
    (:e_if          ((loc)
                     (test condition)
                     (then exp)
                     (else exp)))
    :measure (acl2::two-nats-measure (acl2-count x) 10))

  (deflist explist :elt-type exp :true-listp t
    :measure (acl2::two-nats-measure (acl2-count x) 10))

  (defoption maybe-exp exp
    :measure (acl2::two-nats-measure (acl2-count x) 20))

  (deftagsum set_clause
    (:eltrem ((elt pat0)
              (rem pat0)
              (exp exp)))
    (:preeltpost ((pre pat0)
                  (elt pat0)
                  (post pat0)
                  (exp exp)))
    :base-case-override :eltrem
    :measure (acl2::two-nats-measure (acl2-count x) 20))
  

  (deftagsum condition
    (:cond_eq     ((arg1 exp)
                   (arg2 exp)))
    (:cond_subset ((arg1 exp)
                   (arg2 exp)))
    (:cond_in     ((arg1 exp)
                   (arg2 exp)))
    (:cond_variant ((c variant_cond)))
    :measure (acl2::two-nats-measure (acl2-count x) 20))

  (defprod matchclause
    ((tag string)
     (exp exp))
    :layout :alist
    :measure (acl2::two-nats-measure (acl2-count x) 20))

  (deflist matchclauselist :elt-type matchclause :true-listp t
    :measure (acl2::two-nats-measure (acl2-count x) 20))

  (defprod binding
    ((loc)
     (pat pat)
     (exp exp))
    :layout :alist
    :measure (acl2::two-nats-measure (acl2-count x) 20))

  (deflist bindinglist :elt-type binding :true-listp t
    :measure (acl2::two-nats-measure (acl2-count x) 20)))
           
   


(defenum do_test-p (:acyclic :irreflexive :testempty))

(deftagsum test
  (:t_yes ((test do_test-p)))
  (:t_no  ((test do_test-p))))


(defenum test_type-p (:flagged :undefinedunless :check :assert))

(defoption maybe-var var
  ///
  (defthm maybe-var-compound-recognizer
    (equal (maybe-var-p x)
           (or (stringp x) (not x)))
    :hints(("Goal" :in-theory (enable maybe-var-p var-p)))
    :rule-classes :compound-recognizer))

(defprod app_test
  ((loc)
   (pos) ;; ??
   (test test)
   (exp exp)
   (name maybe-var))
  :layout :alist)

(defoption maybe-app_test app_test)

(defenum is_rec-p (:isrec :isnotrec))

(deftypes ins
  (deftagsum ins
    (:i_let ((loc)
             (bindings bindinglist)))
    (:i_rec ((loc)
             (bindings bindinglist)
             (test maybe-app_test)))
    (:i_insmatch ((loc)
                  (arg exp)
                  (clauses insclauselist)
                  ;; FIXME -- this is an ins list option in the OCaml AST
                  ;;  But of course our option types don't distinguish between a None and an empty list.
                  ;;  Is a None here different than an empty list?
                  (otherwise inslist)))
    (:i_test ((test app_test)
              (type test_type-p)))
    (:i_unshow ((loc)
                (lst string-listp)))
    (:i_show   ((loc)
                (lst string-listp)))
    (:i_showas ((loc)
                (exp exp)
                (name string)))
    (:i_include ((loc)
                 (fname string))) ;; hope we can get rid of these!
    (:i_procedure ((loc)
                   (name var)
                   (formals pat)
                   (body inslist)
                   (rec is_rec-p)))
    (:i_call      ((loc)
                   (proc var)
                   (arg exp)
                   (name maybe-var)))
    (:i_enum      ((loc)
                   (name var)
                   (elts taglist)))
    (:i_forall    ((loc)
                   (var var)
                   (exp exp)
                   (body inslist)))
    (:i_debug     ((loc)
                   (exp exp)))
    (:i_withfrom  ((loc)
                   (name var)
                   (rels exp)))
    (:i_events    ((loc)
                   (var var)
                   (exps explist)
                   (flag booleanp)))
    (:i_ifvariant ((loc)
                   (test variant_cond)
                   (then inslist)
                   (else inslist)))
    :measure (acl2::two-nats-measure (acl2-count x) 10))

  (deflist inslist :elt-type ins :true-listp t
    :measure (acl2::two-nats-measure (acl2-count x) 10))

  (defprod insclause
    ((tag string)
     (body inslist))
    :measure (acl2::two-nats-measure (acl2-count x) 20))

  (deflist insclauselist :elt-type insclause :true-listp t
    :measure (acl2::two-nats-measure (acl2-count x) 20)))
                   

  
(define read-ast-file ((fname stringp) &key (state 'state))
  :short "Reads the file produced by @('cat2lisp'), returning the AST."
  :returns (mv err
               (ast "should be an @(see inslist)")
               state)
  :mode :program
  (b* (((mv err contents state) (acl2::read-file fname state))
       ((when err) (mv err nil state))
       ((unless (and (consp contents) (not (cdr contents))))
        (mv "Unexpected file contents" nil state)))
    (mv nil (car contents) state)))



(define read-ast-file-into-global ((fname stringp)
                                   &key (state 'state))
  :short "Reads the file produced by @('cat2lisp'), storing the AST in state global @('(@
:ast)')."
  ;; Reads a Lisp AST file as dumped by aslref, and stores its static env and AST in state globals :static-env and :ast.
  :returns (mv err ok state)
  :mode :program
  (b* (((mv err ast state) (read-ast-file fname))
       ((when err)
        (er soft 'read-ast-file-into-global "~@0" err))
       (state (f-put-global ':ast ast state)))
    (value :ok)))


(define find-first-non-ins (x)
  (if (atom x)
      nil
    (if (ins-p (car x))
        (let ((rest (find-first-non-ins (cdr x))))
          (and rest (+ 1 rest)))
      0)))


(define test-ast-file ((fname stringp)
                       &key (state 'state))
  :mode :program
  (b* (((mv err ast state) (read-ast-file fname))
       ((when err)
        (cw "@#@#@ Error reading AST file: ~@0~%" err)
        (exit 2)
        state)
       ((unless (consp ast))
        (cw "@#@#@ Empty AST~%")
        (exit 2)
        state)
       ((unless (inslist-p ast))
        (cw "@#@#@ Not well-typed. First non-ins: ~x0~%" (find-first-non-ins ast))
        (exit 1)
        state))
    (cw "Well-typed AST, length ~x0~%" (len ast))
    (exit 43) ;; success
    state))


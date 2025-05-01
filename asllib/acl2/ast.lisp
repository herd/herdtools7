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

(include-book "centaur/fty/deftypes" :dir :system)
(include-book "centaur/fty/basetypes" :dir :system)
(include-book "std/util/defenum" :dir :System)
(include-book "std/basic/two-nats-measure" :dir :system)
(local (in-theory (disable (tau-system))))

;; how should we deal with type aliases?  E.g., an identifier is a string, an uid is an int.

(local (table fty::deftagsum-defaults :short-names t))


(defmacro def-type-alias (new-type existing-type)
  `(defprod ,new-type
     ((val ,existing-type))
     :layout :fulltree))

  ;; `(table fty::fixtypes ',new-type
  ;;         (cdr (assoc ',existing-type (table-alist 'fty::fixtypes world)))))

(def-type-alias identifier string)
(in-theory (enable (:t identifier)
                   (:t identifier-fix)))
(deflist identifierlist :elt-type identifier :true-listp t)

(def-type-alias uid acl2::int)


(defenum unop-p (:bnot :neg :not))

(defenum binop-p
  (:and
   :band
   :beq 
   :bor 
   :div 
   :divrm
   :xor 
   :eq_op
   :gt  
   :geq 
   :impl
   :lt  
   :leq 
   :mod 
   :minus
   :mul 
   :neq 
   :or  
   :plus
   :pow 
   :rdiv
   :shl 
   :shr 
   :concat))

(deftagsum literal
  (:l_int ((val integerp :rule-classes :type-prescription)))
  (:l_bool ((val booleanp :rule-classes :type-prescription)))
  (:l_real ((val rationalp :rule-classes :type-prescription)))
  (:l_bitvector ((len natp :rule-classes :type-prescription)
                 (val natp :rule-classes :type-prescription)))
  (:l_string ((val stringp :rule-classes :type-prescription)))
  (:l_label ((val identifier-p :rule-classes :type-prescription))))

(deftagsum precision_loss_flag
  (:precision_full ())
  (:precision_lost ()))


(defenum subprogram_type-p
  (:st_procedure :st_function :st_getter :st_emptygetter :st_setter :st_emptysetter))

(defprod bitvector_mask
  ((length natp)
   (set natp)
   (unset natp))
  :layout :alist)

(deftypes expr
  (deftagsum expr_desc
    (:e_literal ((val literal)))
    (:e_var ((name identifier)))
    (:e_atc ((expr expr)
             (type ty)))
    (:e_binop ((op binop-p)
               (arg1 expr)
               (arg2 expr)))
    (:e_unop ((op unop-p)
              (arg expr)))
    (:e_call ((call call)))
    (:e_slice ((expr expr)
               (slices slicelist)))
    (:e_cond ((test expr)
              (then expr)
              (else expr)))
    (:e_getarray ((base expr)
                  (index expr)))
    (:e_getenumarray ((base expr)
                      (index expr)))
    (:e_getfield ((base expr)
                  (field identifier)))
    (:e_getfields ((base expr)
                   (fields identifierlist)))
    (:e_getcollectionfields
     ((base identifier)
      (fields identifierlist)))
    (:e_getitem ((base expr)
                 (index acl2::int)))
    (:e_record ((type ty)
                (fields named_exprlist)))
    (:e_tuple ((exprs exprlist)))
    (:e_array ((length expr)
               (value expr)))
    (:e_enumarray ((enum identifier)
                   (labels identifierlist)
                   (value expr)))
    (:e_arbitrary ((type ty)))
    (:e_pattern ((expr expr)
                 (pattern pattern)))
    :base-case-override :e_literal
    :measure (acl2::two-nats-measure (acl2-count x) 10))

  (defprod expr ((desc expr_desc)
                 ;; (ty maybe-ty)
                 )
    :layout :fulltree ;; :alist
    :measure (acl2::two-nats-measure (acl2-count x) 20))

  (deflist exprlist :elt-type expr :true-listp t
    :measure (acl2::two-nats-measure (acl2-count x) 10))
  
  (deftagsum pattern_desc
    (:pattern_all ())
    (:pattern_any ((patterns patternlist)))
    (:pattern_geq ((expr expr)))
    (:pattern_leq ((expr expr)))
    (:pattern_mask ((mask bitvector_mask)))
    (:pattern_not ((pattern pattern)))
    (:pattern_range ((lower expr)
                     (upper expr)))
    (:pattern_single ((expr expr)))
    (:pattern_tuple ((patterns patternlist)))
    :measure (acl2::two-nats-measure (acl2-count x) 10))

  (defprod pattern ((val pattern_desc)) :layout :fulltree
    :measure (acl2::two-nats-measure (acl2-count x) 20))

  (deflist patternlist :elt-type pattern :true-listp t
    :measure (acl2::two-nats-measure (acl2-count x) 10))
  
  (deftagsum slice
    (:slice_single ((index expr)))
    (:slice_range ((end expr)
                   (start expr)))
    (:slice_length ((start expr)
                    (length expr)))
    (:slice_star ((factor expr)
                  (length expr)))
    :base-case-override :slice_single
    :measure (acl2::two-nats-measure (acl2-count x) 30))

  (deflist slicelist :elt-type slice :true-listp t
    :measure (acl2::two-nats-measure (acl2-count x) 10))
  
  (defprod call
    ((name identifier)
     (params exprlist)
     (args exprlist)
     (call_type subprogram_type-p))
    :measure (acl2::two-nats-measure (acl2-count x) 20)
    :layout :alist)

  (deftagsum type_desc
    (:t_int ((constraint constraint_kind)))
    (:t_bits ((expr expr)
              (fields bitfieldlist)))
    (:t_real ())
    (:t_string ())
    (:t_bool ())
    (:t_enum ((elts identifierlist)))
    (:t_tuple ((types tylist)))
    (:t_array ((index array_index)
               (type ty)))
    (:t_record ((fields typed_identifierlist)))
    (:t_exception ((fields typed_identifierlist)))
    (:t_collection ((fields typed_identifierlist)))
    (:t_named ((name identifier)))
    :measure (acl2::two-nats-measure (acl2-count x) 10))

  (defprod ty ((val type_desc)) :layout :fulltree
    :measure (acl2::two-nats-measure (acl2-count x) 20))

  (deflist tylist :elt-type ty :true-listp t
    :measure (acl2::two-nats-measure (acl2-count x) 10))

  (deftagsum int_constraint
    (:constraint_exact ((val expr)))
    (:constraint_range ((from expr)
                        (to expr)))
    :base-case-override :constraint_exact
    :measure (acl2::two-nats-measure (acl2-count x) 30))

  (deflist int_constraintlist :elt-type int_constraint :true-listp t
    :measure (acl2::two-nats-measure (acl2-count x) 10))
  
  (deftagsum constraint_kind
    (:unconstrained ())
    (:wellconstrained ((constraints int_constraintlist)
                       (flag precision_loss_flag)))
    (:pendingconstrained ())
    (:parametrized ((id uid)
                    (name identifier))
     ;; NOTE: In some cases we're going to transform a :parametrized
     ;; constraint to one of the form (wellconstrained (list (constraint_exact (expr (e_var name)))) flag).
     ;; In order to be able to recur on this without increasing the constraint_kind-count,
     ;; we add an extra value to the count for the parametrized case.
     :count-incr 20)
    :measure (acl2::two-nats-measure (acl2-count x) 10))

  (deftagsum bitfield
    (:bitfield_simple ((name identifier)
                       (slices slicelist)))
    (:bitfield_nested ((name identifier)
                       (slices slicelist)
                       (nested bitfieldlist)))
    (:bitfield_type ((name identifier)
                     (slices slicelist)
                     (type ty)))
    :base-case-override :bitfield_simple
    :measure (acl2::two-nats-measure (acl2-count x) 20))

  (deflist bitfieldlist :elt-type bitfield :true-listp t
    :measure (acl2::two-nats-measure (acl2-count x) 10))

  (deftagsum array_index
    (:arraylength_expr ((length expr)))
    (:arraylength_enum ((name identifier)
                        (elts identifierlist)))
    :measure (acl2::two-nats-measure (acl2-count x) 10))

  (defprod named_expr
    ((name identifier)
     (expr expr))
    :measure (acl2::two-nats-measure (acl2-count x) 30)
    :layout :fulltree)

  (deflist named_exprlist :elt-type named_expr :true-listp t
    :measure (acl2::two-nats-measure (acl2-count x) 10)

    :measure-debug t)

  (defoption maybe-ty ty
    :measure (acl2::two-nats-measure (acl2-count x) 30))

  (defprod typed_identifier
    ((name identifier)
     (type ty))
    :measure (acl2::two-nats-measure (acl2-count x) 30)
    :layout :fulltree)

  (deflist typed_identifierlist :elt-type typed_identifier :true-listp t
    :measure (acl2::two-nats-measure (acl2-count x) 10)

    :measure-debug t))


(defprod intpair
  ((first integerp)
   (second integerp))
  :layout :fulltree)

(deflist intpairlist :elt-type intpair :true-listp t)


(deftypes lexpr
  (deftagsum lexpr_desc
    (:le_discard ())
    (:le_var ((name identifier)))
    (:le_slice ((base lexpr)
                (slices slicelist)))
    (:le_setarray ((base lexpr)
                   (index expr)))
    (:le_setenumarray ((base lexpr)
                       (index expr)))
    (:le_setfield ((base lexpr)
                   (field identifier)))
    (:le_setfields ((base lexpr)
                    (fields identifierlist)
                    (pairs intpairlist)))
    (:le_setcollectionfields
     ((base identifier)
      (fields identifierlist)
      (pairs intpairlist)))
    (:le_destructuring ((elts lexprlist)))
    :base-case-override :le_discard
    :measure (acl2::two-nats-measure (acl2-count x) 10))


  (defprod lexpr ((val lexpr_desc)) :layout :fulltree
    :measure (acl2::two-nats-measure (acl2-count x) 11))
  
  (deflist lexprlist :elt-type lexpr :true-listp t
    :measure (acl2::two-nats-measure (acl2-count x) 12)))

(defenum local_decl_keyword-p (:ldk_var :ldk_constant :ldk_let))

(deftagsum local_decl_item
  (:ldi_var ((name identifier)))
  (:ldi_tuple ((names identifierlist))))


(defenum for_direction-p (:up :down))
(defoption maybe-expr expr)

(defprod expr*maybe-ty
  ((expr expr)
   (ty maybe-ty))
  :layout :list
  :measure (acl2::two-nats-measure (acl2-count x) 10))

(defoption maybe-[expr*maybe-ty] expr*maybe-ty)

(defoption maybe-identifier identifier)

(deftypes stmt
  (deftagsum stmt_desc
    (:s_pass ())
    (:s_seq ((first stmt)
             (second stmt)))
    (:s_decl ((key local_decl_keyword-p)
              (item local_decl_item)
              (ty maybe-ty)
              (expr maybe-expr)))
    (:s_assign ((lexpr lexpr)
                (expr expr)))
    (:s_call ((call call)))
    (:s_return ((expr maybe-expr)))
    (:s_cond ((test expr)
              (then stmt)
              (else stmt)))
    (:s_assert ((expr expr)))
    (:s_for ((index_name identifier)
             (start_e expr)
             (dir for_direction-p)
             (end_e expr)
             (body stmt)
             (limit maybe-expr)))
    (:s_while ((test expr)
               (limit maybe-expr)
               (body stmt)))
    (:s_repeat ((body stmt)
                (test expr)
                (limit maybe-expr)))
    (:s_throw ((val maybe-[expr*maybe-ty])))
    (:s_try ((body stmt)
             (catchers catcherlist)
             (otherwise maybe-stmt)))
    (:s_print ((args exprlist)
               (newline booleanp)
               (debug booleanp)))
    (:s_unreachable ())
    (:s_pragma ((name identifier)
                (exprs exprlist)))
    :base-case-override :s_pass
    :measure (acl2::two-nats-measure (acl2-count x) 10))

  (defprod stmt ((val stmt_desc)) :layout :fulltree
    :measure (acl2::two-nats-measure (acl2-count x) 11))

  (defoption maybe-stmt stmt
    :measure (acl2::two-nats-measure (acl2-count x) 12))

  (defprod catcher ((name maybe-identifier)
                    (ty ty)
                    (stmt stmt))
    :layout :list
    :measure (acl2::two-nats-measure (acl2-count x) 12))

  (deflist catcherlist :elt-type catcher :true-listp t
    :measure (acl2::two-nats-measure (acl2-count x) 13)))



(deftagsum subprogram_body
  (:sb_asl ((stmt stmt)))
  (:sb_primitive ((side-effecting booleanp))))


;; (deflist [expr*maybe-ty]list :elt-type expr*maybe-ty :true-listp t)


(defprod maybe-typed_identifier
  ((name identifier)
   (type maybe-ty))
  :layout :fulltree)

(deflist maybe-typed_identifierlist :elt-type maybe-typed_identifier :true-listp t)


(defprod func
  ((name identifier)
   (parameters maybe-typed_identifierlist)
   (args typed_identifierlist)
   (body subprogram_body)
   (return_type maybe-ty)
   (subprogram_type subprogram_type-p)
   (recurse_limit maybe-expr)
   (builtin booleanp))
  :layout :alist)

(defenum global_decl_keyword-p (:gdk_constant :gdk_config :gdk_let :gdk_var))

(defprod global_decl
  ((keyword global_decl_keyword-p)
   (name identifier)
   (ty maybe-ty)
   (initial_value maybe-expr))
  :layout :alist)

(defprod supertype ((name identifier)
                    (fields typed_identifierlist))
  :layout :fulltree)

(defoption maybe-supertype supertype)


(deftagsum decl_desc
  (:d_func ((func func)))
  (:d_globalstorage ((decl global_decl)))
  (:d_typedecl ((name identifier)
                (ty ty)
                (supertype maybe-supertype)))
  (:d_pragma ((name identifier)
              (exprs exprlist))))

(defprod decl ((val decl_desc)) :layout :fulltree)

(deflist ast :elt-type decl :true-listp t)



(defenum timeframe-p (:constant :execution))

(defprod read ((name identifier)
               (time_frame timeframe-p)
               (immutable booleanp))
  :layout :alist)

(deftagsum side_effect
  (:readslocal ((read read)))
  (:writeslocal ((name identifier)))
  (:readsglobal ((read read)))
  (:writesglobal ((name identifier)))
  (:throwsexception ((name identifier)))
  (:callsrecursive ((name identifier)))
  (:performsassertions ())
  (:nondeterministic ()))

(deflist ses :elt-type side_effect :true-listp t)

(fty::defalist literal-storage :key-type string :val-type literal :true-listp t)

(defprod ty-timeframe ((ty ty) (tf timeframe-p)) :layout :fulltree)
(defprod ty-global_decl_keyword ((ty ty) (kw global_decl_keyword-p)) :layout :fulltree)
(defprod ty-local_decl_keyword ((ty ty) (kw local_decl_keyword-p)) :layout :fulltree)
(defprod func-ses ((fn func) (ses ses)) :layout :fulltree)

(fty::defmap ty-timeframe-imap :key-type identifier :val-type ty-timeframe :true-listp t)
(fty::defmap ty-global_decl_keyword-imap :key-type identifier :val-type ty-global_decl_keyword :true-listp t)
(fty::defmap ty-local_decl_keyword-imap :key-type identifier :val-type ty-local_decl_keyword :true-listp t)
(fty::defmap func-ses-imap :key-type identifier :val-type func-ses :true-listp t)

(fty::defmap identifier-imap :key-type identifier :val-type identifier :true-listp t)

(fty::defmap identifierlist-imap :key-type identifier :val-type identifierlist :true-listp t)
(fty::defmap expr-imap :key-type identifier :val-type expr :true-listp t)

(defprod static_env_global
  ((declared_types ty-timeframe-imap)
   (constant_values literal-storage)
   (storage_types ty-global_decl_keyword-imap)
   (subtypes identifier-imap)
   (subprograms func-ses-imap)
   (overloaded_subprograms identifierlist-imap)
   (expr_equiv expr-imap))
  :layout :alist)

(defprod static_env_local
  ((constant_values literal-storage)
   (storage_types ty-local_decl_keyword-imap)
   (expr_equiv expr-imap)
   (return_type maybe-ty))
  :layout :alist)

(defprod static_env
  ((global static_env_global)
   (local static_env_local))
  :layout :alist)


#|
(include-book
 "std/util/defconsts" :dir :System)

(acl2::defconsts (& *test* state)
  (acl2::read-file "~/work/asltest/test5.lsp" state))

(and (static_env_global-p (caar *test*))
     (ast-p (cdar *test*))
     (endp (cdr *test*)))

|#


(define read-ast-file ((fname stringp) &key (state 'state))
  :returns (mv err static-env ast state)
  :mode :program
  (b* (((mv err contents state) (acl2::read-file fname state))
       ((when err) (mv err nil nil state)))
    (case-match contents
      (((static-env . ast))
       (mv nil static-env ast state))
      (& (mv "Malformed Lisp AST file." nil nil state)))))

(define read-ast-file-into-globals ((fname stringp)
                                    &key (state 'state))
  ;; Reads a Lisp AST file as dumped by aslref, and stores its static env and AST in state globals :static-env and :ast.
  :returns (mv err ok state)
  :mode :program
  (b* (((mv err static-env ast state) (read-ast-file fname))
       ((when err)
        (er soft 'read-ast-file-into-globals "~@0" err))
       (state (f-put-global ':static-env static-env state))
       (state (f-put-global ':ast ast state)))
    (value :ok)))


       

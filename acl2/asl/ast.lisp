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
(include-book "std/util/defenum" :dir :system)
(include-book "std/basic/two-nats-measure" :dir :system)
(local (in-theory (disable (tau-system))))

;; how should we deal with type aliases?  E.g., an identifier is a string, an uid is an int.

(local (table fty::deftagsum-defaults :short-names t))


(defxdoc asl-ast
  :parents (asl)
  :short "Format of the ASL AST")

(local (xdoc::set-default-parents asl-ast))


(defmacro def-type-alias (new-type existing-type &rest args)
  `(defprod ,new-type
     ((val ,existing-type))
     :layout :fulltree
     . ,args))

  ;; `(table fty::fixtypes ',new-type
  ;;         (cdr (assoc ',existing-type (table-alist 'fty::fixtypes world)))))

(def-type-alias identifier string
  :short "Alias for string, but used for identifiers in ASL AST constructs
 as well as enum values.")

(in-theory (enable (:t identifier)
                   (:t identifier-fix)))
(deflist identifierlist :elt-type identifier :true-listp t)

(def-type-alias uid acl2::int
  :short "Alias for integer, used for the parameterized integer constraint type which is
never used in the interpreter")


(defenum unop-p (:bnot :neg :not)
  :short "Type of ASL unary operators")

(defenum binop-p
  (:and
   :band
   :beq
   :bor
   :div
   :divrm
   :xor
   :eq
   :gt
   :ge
   :impl
   :lt
   :le
   :mod
   :sub
   :mul
   :ne
   :or
   :add
   :pow
   :rdiv
   :shl
   :shr
   :bv_concat
   :str_concat)
  :short "Type of ASL binary operators")

(deftagsum literal
  :short "Type of ASL literals, used in literal expressions (see @(see expr_desc))."
  :long "<p>See also @(see val), the type of ASL values; not all such values
 can be expressed as literals.</p>"
  (:l_int ((val integerp :rule-classes :type-prescription)))
  (:l_bool ((val booleanp :rule-classes :type-prescription)))
  (:l_real ((val rationalp :rule-classes :type-prescription)))
  (:l_bitvector ((len natp :rule-classes :type-prescription)
                 (val natp :rule-classes :type-prescription)))
  (:l_string ((val stringp :rule-classes :type-prescription)))
  (:l_label ((val identifier-p :rule-classes :type-prescription))))

(deftagsum precision_loss_flag
  :short "Signifies whether any precision was lost in determining the constraints on an integer"
  (:precision_full ())
  (:precision_lost ()))


(defenum subprogram_type-p
  (:st_procedure :st_function :st_getter :st_emptygetter :st_setter :st_emptysetter))

(defprod bitvector_mask
  :short "Bitvector mask; for a bitvector value to satisfy the mask, all bits
corresponding to 1-bits of @('set') must be 1 whereas all bits corresponding to
1-bits of @('unset') must be 0. Used in patterns; see @(see pattern_desc)."
  ((length natp)
   (set natp)
   (unset natp))
  :layout :alist)

(defprod posn
  :short "File position marker for ASL code"
  :long "<p>Note @('cnum') denotes character number (in the whole file), not column
number. The column number would be @('cnum') minus @('bol').</p>"
  ((fname stringp :rule-classes :type-prescription)
   (lnum natp :rule-classes :type-prescription
         "Line number")
   (bol natp :rule-classes :type-prescription
        "Beginning of line character index")
   (cnum integerp :rule-classes :type-prescription
         "Character index from the beginning of the file"))
  :layout :alist)

(deftypes expr
  (deftagsum expr_desc
    :short "Body of an ASL expression (see @(see expr), which pairs this with a @(see
posn))."
    (:e_literal ((val literal))
     :short "Literal expression")
    (:e_var ((name identifier))
     :short "Variable expression")
    (:e_atc ((expr expr)
             (type ty))
     :short "Asserting type conversion expression"
     :long "<p>Semantics: evaluates @('expr'), checks whether it satisfies type @('type');
if so, return the expression, else produce an error.</p>")
    (:e_binop ((op binop-p)
               (arg1 expr)
               (arg2 expr))
     :short "Binary operator expression")
    (:e_unop ((op unop-p)
              (arg expr))
     :short "Unary operator expression")
    (:e_call ((call call))
     :short "Function call expression")
    (:e_slice ((expr expr)
               (slices slicelist))
     :short "Bitvector slice expression"
     :long "<p>Accepts an integer base expression as well as a bitvector. The slices are
concatenated together, MSBs first, to form the result.</p>")
    (:e_cond ((test expr)
              (then expr)
              (else expr))
     :short "Conditional expression")
    (:e_getarray ((base expr)
                  (index expr))
     :short "Array lookup expression")
    (:e_getenumarray ((base expr)
                      (index expr))
     :short "Enumarray lookup expression")
    (:e_getfield ((base expr)
                  (field identifier))
     :short "Record field lookup expression")
    (:e_getfields ((base expr)
                   (fields identifierlist))
     :short "Record multiple-field concatenation expression"
     :long "<p>All the fields looked up must be bitvectors</p>")
    (:e_getcollectionfields
     ((base identifier)
      (fields identifierlist))
     :short "Collection multiple-field concatenation expression (same semantics as @(see
e_getfields))")
    (:e_getitem ((base expr)
                 (index acl2::int))
     :short "Tuple item accessor expression")
    (:e_record ((type ty)
                (fields named_exprlist))
     :short "Record construction expression")
    (:e_tuple ((exprs exprlist))
     :short "Tuple construction expression")
    (:e_array ((length expr)
               (value expr))
     :short "Array construction expression. (No syntax for this; only produced for initial
values.)")
    (:e_enumarray ((enum identifier)
                   (labels identifierlist)
                   (value expr))
     :short "Enumarray construction expression. (No syntax for this; only produced for
initial values.)")
    (:e_arbitrary ((type ty))
     :short "Arbitrary value expression. Semantics: reads the oracle to construct a value of
the given type.")
    (:e_pattern ((expr expr)
                 (pattern pattern))
     :short "Pattern match expression")
    :base-case-override :e_literal
    :measure (acl2::two-nats-measure (acl2-count x) 10))

  (defprod expr ((desc expr_desc "Main expression")
                 (pos_start posn "Source code position of expression"))
    :short "ASL expression (wrapper for an @(see expr_desc))"
    :layout :alist
    :measure (acl2::two-nats-measure (acl2-count x) 20))

  (deflist exprlist :elt-type expr :true-listp t
    :short "List of ASL expressions"
    :measure (acl2::two-nats-measure (acl2-count x) 10))

  (deftagsum pattern_desc
    :short "ASL pattern expression (main body)"
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

  (defprod pattern ((desc pattern_desc "Main pattern")
                    (pos_start posn "Source code position"))
    :short "ASL pattern expression (wrapper for @(see pattern_desc)))"
    :layout :alist
    :measure (acl2::two-nats-measure (acl2-count x) 20))

  (deflist patternlist :elt-type pattern :true-listp t
    :short "List of ASL patterns"
    :measure (acl2::two-nats-measure (acl2-count x) 10))

  (deftagsum slice
    :short "ASL bitvector slice expression"
    (:slice_single ((index expr))
     :short "Bitvector slice of width 1 at index @('index').")
    (:slice_range ((end expr)
                   (start expr))
     :short "Bitvector slice of width @('end - start + 1') starting at LSB @('start').")
    (:slice_length ((start expr)
                    (length expr))
     :short "Bitvector slice of width @('length') starting at LSB @('start').")
    (:slice_star ((factor expr)
                  (length expr))
     :short "Bitvector slice of width @('length') starting at LSB @('factor * length').")
    :base-case-override :slice_single
    :measure (acl2::two-nats-measure (acl2-count x) 30))

  (deflist slicelist :elt-type slice :true-listp t
    :short "List of ASL bitvector slices"
    :measure (acl2::two-nats-measure (acl2-count x) 10))

  (defprod call
    :short "ASL function or procedure call"
    ((name identifier)
     (params exprlist)
     (args exprlist)
     (call_type subprogram_type-p))
    :measure (acl2::two-nats-measure (acl2-count x) 20)
    :layout :alist)

  (deftagsum type_desc
    :short "Body of an ASL type (see @(see ty), which pairs this with a
@(see posn))."
    (:t_int ((constraint constraint_kind))
     :short "Integer type, constrained by the given constraint expression")
    (:t_bits ((expr expr)
              (fields bitfieldlist))
     :short "Bitvector type, width given by @('expr'). The @('fields') give names to
subfields of the bitvector but are semantically irrelevant because references
to them are removed by the typechecker.")
    (:t_real ())
    (:t_string ())
    (:t_bool ())
    (:t_enum ((elts identifierlist)))
    (:t_tuple ((types tylist)))
    (:t_array ((index array_index)
               (type ty))
     :short "Array type, either an integer-indexed array or an enumarray depending on
@('index'). All elements are of type @('type').")
    (:t_record ((fields typed_identifierlist)))
    (:t_exception ((fields typed_identifierlist)))
    (:t_collection ((fields typed_identifierlist)))
    (:t_named ((name identifier)))
    :measure (acl2::two-nats-measure (acl2-count x) 10))

  (defprod ty ((desc type_desc "Main body")
               (pos_start posn "Source code position"))
    :short "ASL type (wrapper for @(see type_desc))"
    :layout :alist
    :measure (acl2::two-nats-measure (acl2-count x) 20))

  (deflist tylist :elt-type ty :true-listp t
    :short "List of ASL types"
    :measure (acl2::two-nats-measure (acl2-count x) 10))

  (deftagsum int_constraint
    :short "Building block of integer constraints used in ASL types (see @(see constraint_kind))"
    (:constraint_exact ((val expr)))
    (:constraint_range ((from expr)
                        (to expr)))
    :base-case-override :constraint_exact
    :measure (acl2::two-nats-measure (acl2-count x) 30))

  (deflist int_constraintlist :elt-type int_constraint :true-listp t
    :short "List of integer constraints used in ASL types (see @(see constraint_kind))"
    :measure (acl2::two-nats-measure (acl2-count x) 10))

  (deftagsum constraint_kind
    :short "Integer constraint used in ASL types (see @(see type_desc))"
    (:unconstrained ())
    (:wellconstrained ((constraints int_constraintlist)
                       (flag precision_loss_flag)))
    (:pendingconstrained ()
     :short "Shouldn't see this in type-checked ASTs")
    (:parametrized (;; (id uid)
                    (name identifier))
     :short "Shouldn't see this while interpreting type-checked ASTs"
     :long "<p>This can occur in the types of parameters of function declarations but
shouldn't occur in @(see e_arbitrary) or @(see e_atc) expressions where they
are semantically relevant.</p>"
     ;; NOTE: In some cases we're going to transform a :parametrized
     ;; constraint to one of the form (wellconstrained (list (constraint_exact (expr (e_var name)))) flag).
     ;; In order to be able to recur on this without increasing the constraint_kind-count,
     ;; we add an extra value to the count for the parametrized case.
     :count-incr 20)
    :measure (acl2::two-nats-measure (acl2-count x) 10))

  (deftagsum bitfield
    :short "ASL bitfields -- not semantically relevant (see @(see t_bits))"
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
    :short "List of ASL bitfields"
    :measure (acl2::two-nats-measure (acl2-count x) 10))

  (deftagsum array_index
    :short "Array indexing specification -- either a length (indicating a regular array) or
an enum type (indicating an enumarray)."
    (:arraylength_expr ((length expr)))
    (:arraylength_enum ((name identifier)
                        (elts identifierlist)))
    :measure (acl2::two-nats-measure (acl2-count x) 10))

  (defprod named_expr
    :short "Pairing of a name (identifier) with an expression, as in @(see e_record)"
    ((name identifier)
     (expr expr))
    :measure (acl2::two-nats-measure (acl2-count x) 30)
    :layout :fulltree)

  (deflist named_exprlist :elt-type named_expr :true-listp t
    :short "List of name/expression pairs"
    :measure (acl2::two-nats-measure (acl2-count x) 10)

    :measure-debug t)

  (defoption maybe-ty ty
    :measure (acl2::two-nats-measure (acl2-count x) 30))

  (defprod typed_identifier
    :short "Pairing of a name (identifier) with a type, as in @(see t_record)"
    ((name identifier)
     (type ty))
    :measure (acl2::two-nats-measure (acl2-count x) 30)
    :layout :fulltree)

  (deflist typed_identifierlist :elt-type typed_identifier :true-listp t
    :short "List of name/type pairs"
    :measure (acl2::two-nats-measure (acl2-count x) 10)

    :measure-debug t))


(defprod intpair
  :short "Pair of integers, typically used to give the LSB (first) and width (second) of
a bitvector slice"
  ((first integerp)
   (second integerp))
  :layout :fulltree)

(deflist intpairlist :elt-type intpair :true-listp t)


(deftypes lexpr
  (deftagsum lexpr_desc
    :short "Body of an ASL left-hand-side expression, i.e. an object to be assigned to. See
@(see lexpr), which pairs this with a @(see posn)."
    (:le_discard ())
    (:le_var ((name identifier)))
    (:le_slice ((base lexpr)
                (slices slicelist))
     :short "Assignment to a list of bitvector slices: see @(see write_to_bitvector)")
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
    (:le_destructuring ((elts lexprlist))
     :short "Assignment to to a tuple of LHS expressions")
    :base-case-override :le_discard
    :measure (acl2::two-nats-measure (acl2-count x) 10))


  (defprod lexpr ((desc lexpr_desc "Main left-hand-side expression")
                  (pos_start posn "Source code position"))
    :short "ASL left-hand-side expression (wrapper for @(see lexpr_desc))"
    :layout :alist
    :measure (acl2::two-nats-measure (acl2-count x) 11))

  (deflist lexprlist :elt-type lexpr :true-listp t
    :short "List of left-hand-side expressions"
    :measure (acl2::two-nats-measure (acl2-count x) 12)))

(defenum local_decl_keyword-p
  (:ldk_var :ldk_constant :ldk_let)
  :short "Kinds of local variables: regular/mutable, constant at typechecking time, or immutable.")

(deftagsum local_decl_item
  :short "Variable or variables declared in a local variable declaration statement"
  (:ldi_var ((name identifier)))
  (:ldi_tuple ((names identifierlist))))


(defenum for_direction-p (:up :down))
(defoption maybe-expr expr)

(defoption maybe-identifier identifier)

(deftypes stmt
  (deftagsum stmt_desc
    :short "Body of an ASL statement. See @(see stmt), which pairs this with a @(see posn)."
    (:s_pass () :short "Do nothing")
    (:s_seq ((first stmt)
             (second stmt))
     :short "Run the first statement, then the second")
    (:s_decl ((key local_decl_keyword-p)
              (item local_decl_item)
              (ty maybe-ty)
              (expr maybe-expr))
     :short "Declare one or multiple local variables or constants and set them to the value of @('expr')")
    (:s_assign ((lexpr lexpr)
                (expr expr))
     :short "Assign the value of @('expr') to the LHS expression @('lexpr')")
    (:s_call ((call call))
     :short "Procedure call statement")
    (:s_return ((expr maybe-expr))
     :short "Return the value of the given expression (or nothing)")
    (:s_cond ((test expr)
              (then stmt)
              (else stmt)))
    (:s_assert ((expr expr)))
    (:s_for ((index_name identifier)
             (start_e expr)
             (dir for_direction-p)
             (end_e expr)
             (body stmt)
             (limit maybe-expr))
     :short "For loop statement.")
    (:s_while ((test expr)
               (limit maybe-expr)
               (body stmt))
     :short "While loop statement. Body is evaluated until the test is false.")
    (:s_repeat ((body stmt)
                (test expr)
                (limit maybe-expr))
     :short "Repeat-until loop statement. Body is evaluated once, then
 repeatedly until the test is true.")
    (:s_throw ((val expr)
               (ty maybe-ty))
     :short "Throw statement. @('val') gives the exception data to be thrown. If not
present, this is a rethrow -- i.e. throw the same exception that was previously
thrown within a catcher block.")
    (:s_try ((body stmt)
             (catchers catcherlist)
             (otherwise maybe-stmt))
     :short "Try/catch statement -- run @('body'), then if it results in an uncaught
exception try to catch it, either with one of @('catchers') or otherwise with
the optional @('otherwise') block.")
    (:s_print ((args exprlist)
               (newline booleanp)
               (debug booleanp "Currently ignored"))
     :short "Print the list of expressions, optionally with a newline.")
    (:s_unreachable ()
     :short "Indicates that this statement shouldn't have been reached, so we produce an error")
    (:s_pragma ((name identifier)
                (exprs exprlist))
     :short "Should not occur in typechecked ASTs")
    :base-case-override :s_pass
    :measure (acl2::two-nats-measure (acl2-count x) 10))

  (defprod stmt ((desc stmt_desc "Statement body")
                 (pos_start posn "Source code position"))
    :short "ASL statement (wrapper for @(see stmt_desc))"
    :layout :alist
    :measure (acl2::two-nats-measure (acl2-count x) 11))

  (defoption maybe-stmt stmt
    :measure (acl2::two-nats-measure (acl2-count x) 12))

  (defprod catcher ((name maybe-identifier)
                    (ty ty)
                    (stmt stmt))
    :short "Catch block for a @(see s_try) statement"
    :layout :list
    :measure (acl2::two-nats-measure (acl2-count x) 12))

  (deflist catcherlist :elt-type catcher :true-listp t
    :measure (acl2::two-nats-measure (acl2-count x) 13)))



(deftagsum subprogram_body
  :short "Body of a subprogram. Primitives are supported by @(see eval_primitive) and can
be omitted (since they should all be implemented in the standard library) using
@('aslref --no-primitives')."
  (:sb_asl ((stmt stmt))
   :short "A regular ASL function/procedure body: run the @('stmt') to run the subprogram")
  (:sb_primitive ((side-effecting booleanp))
   :short "An ASL primitive, run using @(see eval_primitive)"))


;; (deflist [expr*maybe-ty]list :elt-type expr*maybe-ty :true-listp t)


(defprod maybe-typed_identifier
  ((name identifier)
   (type maybe-ty))
  :layout :fulltree)

(deflist maybe-typed_identifierlist :elt-type maybe-typed_identifier :true-listp t)


(defprod func
  :short "ASL function declaration, giving the name, parameter/argument names and types,
body, return type, and recursion limit."
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
  :short "ASL global data declaration"
  ((keyword global_decl_keyword-p)
   (name identifier)
   (ty maybe-ty)
   (initial_value maybe-expr))
  :layout :alist)

(defprod supertype ((name identifier)
                    (fields typed_identifierlist))
  :short "Part of an ASL type declaration -- identifies a supertype"
  :layout :fulltree)

(defoption maybe-supertype supertype)


(deftagsum decl_desc
  :short "Body of an ASL top-level declaration (see @(see decl), which pairs this with a @(see posn))"
  (:d_func ((func func))
   :short "Type declaration")
  (:d_globalstorage ((decl global_decl))
   :short "Global data declaration")
  (:d_typedecl ((name identifier)
                (ty ty)
                (supertype maybe-supertype))
   :short "Type declaration")
  (:d_pragma ((name identifier)
              (exprs exprlist))
   :short "Ignored"))

(defprod decl ((desc decl_desc)
               (pos_start posn))
  :short "ASL top-level declaration -- wrapper for @(see decl_desc)"
  :layout :alist)

(deflist ast :elt-type decl :true-listp t
  :short "ASL abstract syntax tree -- list of @(see decl) objects."
  :long "<p>Note that the AST file we read, produced by @('aslref'), contains
both an AST and, importantly, a @(see static_env_global). The static env is
important for the interpreter, primarily because it lets us look up functions
by name.</p>")



(defenum timeframe-p (:constant :execution))

(defprod read ((name identifier)
               (time_frame timeframe-p)
               (immutable booleanp))
  :layout :alist)

(deftagsum side_effect
  :short "Relevant to typechecking but not semantics"
  (:readslocal ((read read)))
  (:writeslocal ((name identifier)))
  (:readsglobal ((read read)))
  (:writesglobal ((name identifier)))
  (:throwsexception ((name identifier)))
  (:callsrecursive ((name identifier)))
  (:performsassertions ())
  (:nondeterministic ())
  (:prints ()))

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
  :short "Static environment for ASL"
  :long "<p>This contains basically everything important about the ASL program that
doesn't change over the course of evaluation: function and type definitions,
global variable types, constant values, etc. Most of it is not semantically
relevant after type-checking. The interpreter uses the @('subprograms') table
to look up function definitions when they are called, @('declared_types') to
resolve named types (for asserting type conversions and arbitrary typed
values), and @('subtypes') to determine whether a catcher can accept a given
exception.</p>"
  ((declared_types ty-timeframe-imap "Maps type names to their definitions -- used in @(see resolve-ty)")
   (constant_values literal-storage "Maps constant names to their (literal) values")
   (storage_types ty-global_decl_keyword-imap "Maps global variable names to their types and declaration keywords")
   (subtypes identifier-imap "Maps named types to their declared supertypes -- used in @(see find_catcher)")
   (subprograms func-ses-imap "Maps function names to their definitions -- used in @(see eval_subprogram)")
   (overloaded_subprograms identifierlist-imap)
   (expr_equiv expr-imap))
  :layout :alist)

(defprod static_env_local
  :short "Local static env -- not used"
  ((constant_values literal-storage)
   (storage_types ty-local_decl_keyword-imap)
   (expr_equiv expr-imap)
   (return_type maybe-ty))
  :layout :alist)

(defprod static_env
  :short "Static env, including global and local -- not used (see @(see static_env_global))."
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
  :short "Reads the file produced by @('aslref --print-lisp'), returning the static env and AST."
  :returns (mv err
               (static-env "should be a @(see static_env_global)")
               (ast "should be an @(see ast)")
               state)
  :mode :program
  (b* (((mv err contents state) (acl2::read-file fname state))
       ((when err) (mv err nil nil state)))
    (case-match contents
      (((static-env . ast))
       (mv nil static-env ast state))
      (& (mv "Malformed Lisp AST file." nil nil state)))))

(define read-ast-file-into-globals ((fname stringp)
                                    &key (state 'state))
  :short "Reads the file produced by @('aslref --print-lisp'), storign the static env and
AST in state globals (respectively) @('(@ :static-env)') and @('(@ :ast)')."
  ;; Reads a Lisp AST file as dumped by aslref, and stores its static env and AST in state globals :static-env and :ast.
  :returns (mv err ok state)
  :mode :program
  (b* (((mv err static-env ast state) (read-ast-file fname))
       ((when err)
        (er soft 'read-ast-file-into-globals "~@0" err))
       (state (f-put-global ':static-env static-env state))
       (state (f-put-global ':ast ast state)))
    (value :ok)))

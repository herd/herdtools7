open Asllib
open ASTUtils
open AST
open! Helpers
open Infix
open Asllib.Types

let empty_env = StaticEnv.empty

let env_with_n =
  let open StaticEnv in
  add_local "N" integer LDK_Let empty

let builtin_examples () =
  let assert_is_builtin_singular t =
    assert (is_builtin_singular !!t);
    assert (not (is_builtin_aggregate !!t));
    assert (is_builtin !!t);
    assert (is_anonymous !!t);
    ()
  in
  let assert_is_builtin_aggregate t =
    assert (is_builtin_aggregate !!t);
    assert (not (is_builtin_singular !!t));
    assert (is_builtin !!t);
    assert (is_anonymous !!t)
  in

  (* Builtin singulars *)
  List.iter assert_is_builtin_singular
    [
      integer';
      integer_exact' !$3;
      T_Real;
      T_String;
      T_Bool;
      T_Enum [];
      T_Enum [ "Something"; "Something Else" ];
      T_Bits (!$0, []);
      T_Bits (!$3, [ BitField_Simple ("Something", [ Slice_Single !$0 ]) ]);
    ];

  (* Builtin aggregate *)
  List.iter assert_is_builtin_aggregate
    [
      T_Tuple [];
      T_Tuple [ !!T_Real; !!T_String ];
      T_Record [];
      T_Record [ ("a", !!T_Real); ("B", integer) ];
      T_Exception [];
      T_Exception [ ("a", !!T_Real); ("B", integer) ];
    ];

  (* Not builtin *)
  assert (is_named !!(T_Named "type_x"));
  assert (not (is_builtin !!(T_Named "type_x")));
  assert (not (is_anonymous !!(T_Named "type_x")));

  ()

let structure_example () =
  (* type T1 of integer; *)
  let t1 = !!(T_Named "T1") in
  (* type T2 of (integer, T1); *)
  let t2_def = !!(T_Tuple [ integer; t1 ]) in
  let t2 = !!(T_Named "T2") in
  let env =
    let open StaticEnv in
    add_type "T1" integer empty |> add_type "T2" t2_def
  in
  (* the named type `T1` whose structure is integer *)
  assert (is_named t1);
  assert ((get_structure env t1).desc = integer.desc);
  (* the named type `T2` whose structure is (integer, integer) *)
  assert (is_named t2);
  assert ((get_structure env t2).desc = T_Tuple [ integer; integer ]);
  (* Note that (integer, T1) is non-primitive since it uses T1 *)
  assert (is_non_primitive t2_def);
  (* the named (hence non-primitive) type `T1` *)
  assert (is_non_primitive t1);

  (* anonymous primitive type `integer` *)
  assert (is_primitive integer);
  assert (is_anonymous integer);

  (* the anonymous non-primitive type `(integer, T1)` whose structure is `(integer, integer)` *)
  assert (is_anonymous t2_def);
  assert (is_non_primitive t2_def);
  assert ((get_structure env t2).desc = T_Tuple [ integer; integer ]);

  ()

let subtype_examples () =
  (*
  let bits_4 = !!(T_Bits (!$4, [])) in
  let bits_2_4 =
    !!(T_Bits
         ( BitWidth_Constraints [ Constraint_Exact !$2; Constraint_Exact !$4 ],
           [] ))
  in

  assert (not (subtype_satisfies empty_env bits_2_4 bits_4));
   *)
  let bits_btifields =
    !!(T_Bits (!$4, [ BitField_Simple ("a", [ Slice_Single !$3 ]) ]))
  in

  assert (domain_subtype_satisfies empty_env bits_btifields bits_btifields);

  let bits_n = !!(T_Bits (!%"N", [])) in
  let bits_n_1 = !!(T_Bits (binop MUL !%"N" !$1, [])) in

  assert (domain_subtype_satisfies env_with_n bits_n bits_n_1);
  assert (structural_subtype_satisfies env_with_n bits_n bits_n_1);
  assert (subtype_satisfies env_with_n bits_n bits_n_1);

  ()

let type_examples () =
  let bits_4 = !!(T_Bits (!$4, [])) in
  let bits_n = !!(T_Bits (!%"N", [])) in
  let bits_n' = !!(T_Bits (!%"N", [])) in

  assert (type_satisfies env_with_n bits_n bits_n');

  assert (not (type_satisfies empty_env !!T_Bool integer));
  assert (not (type_satisfies empty_env bits_4 integer));
  assert (type_satisfies empty_env integer integer);
  assert (type_satisfies empty_env bits_4 bits_4);
  assert (type_satisfies empty_env !!T_Bool !!T_Bool);

  ()

let lca_examples () =
  let bits_4 = !!(T_Bits (!$4, [])) in
  let bits_2 = !!(T_Bits (!$2, [])) in

  assert (lowest_common_ancestor empty_env bits_4 bits_2 = None);

  let integer_4 = integer_exact !$4 in
  let integer_2 = integer_exact !$2 in

  let lca = lowest_common_ancestor empty_env integer_4 integer_2 in
  assert (Option.is_some lca);
  let lca = Option.get lca in
  let domain = Asllib.Types.Domain.of_type empty_env lca in

  assert (Asllib.Types.Domain.mem ~$2 domain);
  assert (Asllib.Types.Domain.mem ~$4 domain);

  ()

let type_clashes () =
  let bits_4 = !!(T_Bits (!$4, [])) in
  let bits_2 = !!(T_Bits (!$2, [])) in
  let bits_m = !!(T_Bits (!%"M", [])) in

  let integer_4 = integer_exact !$4 in
  let integer_2 = integer_exact !$2 in

  assert (not (type_clashes empty_env bits_4 integer_4));
  assert (not (type_clashes empty_env integer bits_2));
  assert (type_clashes empty_env integer integer_4);
  assert (type_clashes empty_env integer_2 integer_4);
  assert (type_clashes empty_env bits_m bits_m);

  assert (type_clashes empty_env integer integer);
  assert (type_clashes empty_env boolean boolean);

  ()

let enum_example () =
  let variants = [ "A"; "B" ] in
  let variants' = [ "A"; "B" ] in
  let t1 = !!(T_Enum variants) in
  let t2 = !!(T_Enum variants') in

  assert (subtype_satisfies empty_env t1 t2);

  ()

let () =
  exec_tests
    [
      ("types.builtin_examples", builtin_examples);
      ("types.structure_example", structure_example);
      ("types.subtype_example", subtype_examples);
      ("types.lca_example", lca_examples);
      ("types.types_examples", type_examples);
      ("types.type_clashes", type_clashes);
      ("types.enum_example", enum_example);
    ]

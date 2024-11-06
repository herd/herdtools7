open Asllib
open AST

let eval_binop x op y =
  let pos = ASTUtils.dummy_annotated and t = Error.Static in
  match Operations.binop_values pos t op (L_Int x) (L_Int y) with
  | L_Int z -> z
  | _ -> assert false

let eval_expr e =
  match StaticInterpreter.static_eval StaticEnv.empty e with
  | L_Int z -> z
  | _ -> assert false

let try_eval_expr e = try eval_expr e |> ASTUtils.expr_of_z with _ -> e

let z_in_constraint z c =
  match
    StaticOperations.filter_reduce_constraint_div c |> Option.value ~default:c
  with
  | Constraint_Exact e -> ( try Z.equal z (eval_expr e) with _ -> false)
  | Constraint_Range (e1, e2) -> (
      try
        let z1 = eval_expr e1 and z2 = eval_expr e2 in
        Z.leq z1 z && Z.leq z z2
      with _ -> false)

let z_in_constraints z cs = List.exists (z_in_constraint z) cs

let property op (x, y, cs1, cs2) =
  assert (z_in_constraints x cs1);
  assert (z_in_constraints y cs2);
  (match op with
  | SHR | SHL | POW -> assert (Z.sign y >= 0)
  | DIV | DIVRM | MOD -> assert (Z.sign y > 0)
  | _ -> ());
  z_in_constraints (eval_binop x op y)
    (StaticOperations.constraint_binop op cs1 cs2)

let gen_xy op =
  let open QCheck2.Gen in
  let base_nat = small_nat in
  let strict_positive_nat = base_nat >|= ( + ) 1 in
  let signed_nat = oneof [ base_nat; base_nat >|= ( ~- ) ] in
  match op with
  | SHR | SHL | POW -> pair signed_nat base_nat
  | DIV ->
      let* y = strict_positive_nat in
      let+ x = signed_nat >|= ( * ) y in
      (x, y)
  | DIVRM | MOD -> pair signed_nat strict_positive_nat
  | _ -> pair signed_nat signed_nat

let gen_cs x =
  let open QCheck2.Gen in
  let+ a = small_nat >|= ( - ) x and+ b = small_nat >|= ( + ) x in
  assert (a <= x);
  assert (x <= b);
  (ASTUtils.expr_of_int a, ASTUtils.expr_of_int b)

let gen_test_abcd op =
  let open QCheck2.Gen in
  let* x, y = gen_xy op in
  let+ a, b = gen_cs x and+ c, d = gen_cs y in
  ( Z.of_int x,
    Z.of_int y,
    [ Constraint_Range (a, b) ],
    [ Constraint_Range (c, d) ] )

let gen_test_abc op =
  let open QCheck2.Gen in
  let* x, y = gen_xy op in
  let+ a, b = gen_cs x in
  let c = ASTUtils.expr_of_int y in
  (Z.of_int x, Z.of_int y, [ Constraint_Range (a, b) ], [ Constraint_Exact c ])

let gen_test_acd op =
  let open QCheck2.Gen in
  let* x, y = gen_xy op in
  let+ c, d = gen_cs y in
  let a = ASTUtils.expr_of_int x in
  (Z.of_int x, Z.of_int y, [ Constraint_Exact a ], [ Constraint_Range (c, d) ])

let print_test op (x, y, cs1, cs2) =
  try
    let res = eval_binop x op y in
    let cs = StaticOperations.constraint_binop op cs1 cs2 in
    let reduced_cs =
      List.filter_map StaticOperations.filter_reduce_constraint_div cs
      |> List.map (function
           | Constraint_Exact e -> Constraint_Exact (try_eval_expr e)
           | Constraint_Range (e1, e2) ->
               Constraint_Range (try_eval_expr e1, try_eval_expr e2))
    in
    Format.asprintf
      "@[<2>@[%a %s %a = %a@]@ is@ not@ in@ @[<2>[%a] %s [%a]@ = [%a]@ = [%a]@]"
      Z.pp_print x (PP.binop_to_string op) Z.pp_print y Z.pp_print res
      PP.pp_int_constraints cs1 (PP.binop_to_string op) PP.pp_int_constraints
      cs2 PP.pp_int_constraints cs PP.pp_int_constraints reduced_cs
  with _ ->
    Format.asprintf
      "(x=%a, y=%a, cs1=%a, cs2=%a) with op %s resulted in an error" Z.pp_print
      x Z.pp_print y PP.pp_int_constraints cs1 PP.pp_int_constraints cs2
      (PP.binop_to_string op)

let long_factor = 100
let base_count = 10000

let test_abcd op =
  let count = base_count * 10
  and name =
    Printf.sprintf "constraint_binop [a..b] %s [c..d] is sound"
      (PP.binop_to_string op)
  in
  QCheck2.Test.make ~count ~long_factor ~print:(print_test op) ~name
    (gen_test_abcd op) (property op)

let test_abc op =
  let count = base_count
  and name =
    Printf.sprintf "constraint_binop [a..b] %s [c] is sound"
      (PP.binop_to_string op)
  in
  QCheck2.Test.make ~count ~long_factor ~print:(print_test op) ~name
    (gen_test_abc op) (property op)

let test_acd op =
  let count = base_count
  and name =
    Printf.sprintf "constraint_binop [a] %s [c..d] is sound"
      (PP.binop_to_string op)
  in
  QCheck2.Test.make ~count ~long_factor ~print:(print_test op) ~name
    (gen_test_acd op) (property op)

let () =
  QCheck_runner.run_tests_main
    [
      test_abcd PLUS;
      test_acd PLUS;
      test_abc PLUS;
      test_abcd MINUS;
      test_acd MINUS;
      test_abc MINUS;
      test_abcd MUL;
      test_acd MUL;
      test_abc MUL;
      test_abcd DIV;
      test_acd DIV;
      test_abc DIV;
      test_abcd DIVRM;
      test_acd DIVRM;
      test_abc DIVRM;
      test_abcd MOD;
      test_acd MOD;
      test_abc MOD;
      test_abcd SHR;
      test_acd SHR;
      test_abc SHR;
      test_abcd SHL;
      test_acd SHL;
      test_abc SHL;
      test_abcd POW;
      test_acd POW;
      test_abc POW;
    ]

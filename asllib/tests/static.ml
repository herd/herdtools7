open Asllib
open AST
open ASTUtils
open! Helpers
open Infix

let _dbg = false

let env_with_N =
  let open StaticEnv in
  add_local "N" integer LDK_Let empty

let build_consts () =
  let values =
    [ ("c1", !$3); ("c2", !!(E_Slice (!%"c1", [ Slice_Range (!$3, !$0) ]))) ]
  in
  let consts =
    List.map
      (fun (name, e) ->
        D_GlobalStorage
          { name; keyword = GDK_Let; ty = None; initial_value = Some e }
        |> __POS_OF__ |> add_pos_from_pos_of)
      values
  in
  let main =
    D_Func
      {
        name = "main";
        body = SB_ASL !!(S_Return (Some !$0));
        args = [];
        recurse_limit = None;
        parameters = [];
        return_type = Some integer;
        subprogram_type = ST_Function;
        builtin = false;
      }
    |> __POS_OF__ |> add_pos_from_pos_of
  in
  let ast = main :: consts in
  let _ = Typing.type_and_run ast in
  ()

let normalize () =
  let do_one (e1, e2, env) =
    let e1' = StaticModel.normalize env e1 in
    let e2' = StaticModel.normalize env e2 in
    let () =
      if _dbg then Format.eprintf "%a ---> %a@." PP.pp_expr e1 PP.pp_expr e1'
    in
    let () =
      if _dbg then Format.eprintf "%a ---> %a@." PP.pp_expr e2 PP.pp_expr e2'
    in
    assert (StaticModel.equal_in_env env e1 e2)
  in

  List.iter do_one
    [
      (binop MINUS !$4 !$2, !$2, StaticEnv.empty);
      ( binop PLUS (binop MINUS !%"N" !%"m") (binop MINUS !%"m" !$1),
        binop MINUS !%"N" !$1,
        StaticEnv.add_local "m" integer LDK_Let env_with_N );
      (unop NEG !$3, !$(-3), StaticEnv.empty);
    ]

let fpzero_example () =
  let ( -~ ) = binop MINUS and ( ==~ ) = binop EQ_OP and ( +~ ) = binop PLUS in
  let e =
    !!(E_Cond (!%"N" ==~ !$16, !$5, !!(E_Cond (!%"N" ==~ !$32, !$8, !$11))))
  in
  let f = !%"N" -~ e -~ !$1 in
  let res = !$1 +~ e +~ f in

  let env = env_with_N in

  let () =
    if _dbg then
      let e' = StaticModel.normalize env e in
      Format.eprintf "%a ---> %a@." PP.pp_expr e PP.pp_expr e'
  in
  let () =
    if _dbg then
      let f' = StaticModel.normalize env f in
      Format.eprintf "%a ---> %a@." PP.pp_expr f PP.pp_expr f'
  in
  let () =
    if _dbg then
      let res' = StaticModel.normalize env res in
      Format.eprintf "%a ---> %a@." PP.pp_expr res PP.pp_expr res'
  in

  assert (StaticModel.equal_in_env env !%"N" res)

let () =
  exec_tests
    [
      ("build_consts", build_consts);
      ("static.normalize", normalize);
      ("static.fpzero_example", fpzero_example);
    ]

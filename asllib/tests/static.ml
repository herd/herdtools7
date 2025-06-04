open Asllib
open AST
open ASTUtils
open! Testhelpers.Helpers
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
        qualifier = None;
        override = None;
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
      (binop `SUB !$4 !$2, !$2, StaticEnv.empty);
      ( binop `ADD (binop `SUB !%"N" !%"m") (binop `SUB !%"m" !$1),
        binop `SUB !%"N" !$1,
        StaticEnv.add_local "m" integer LDK_Let env_with_N );
      (unop NEG !$3, !$(-3), StaticEnv.empty);
    ]

let () =
  exec_tests [ ("build_consts", build_consts); ("static.normalize", normalize) ]

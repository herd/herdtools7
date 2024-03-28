open Asllib
open AST
open ASTUtils
open Helpers
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
        parameters = [];
        return_type = Some integer;
        subprogram_type = ST_Function;
      }
    |> __POS_OF__ |> add_pos_from_pos_of
  in
  let ast = main :: consts in
  let _ = Native.interprete `TypeCheck ast in
  ()

let normalize () =
  let do_one (e1, e2, env) =
    let e1' = StaticInterpreter.Normalize.normalize env e1 in
    let e2' = StaticInterpreter.Normalize.normalize env e2 in
    let () =
      if _dbg then Format.eprintf "%a ---> %a@." PP.pp_expr e1 PP.pp_expr e1'
    in
    let () =
      if _dbg then Format.eprintf "%a ---> %a@." PP.pp_expr e2 PP.pp_expr e2'
    in
    assert (StaticInterpreter.equal_in_env env e1 e2)
  in

  List.iter do_one
    [
      (binop MINUS !$4 !$2, !$2, StaticEnv.empty);
      ( binop PLUS (binop MINUS !%"N" !%"m") (binop MINUS !%"m" !$1),
        binop MINUS !%"N" !$1,
        StaticEnv.add_local "m" integer LDK_Let env_with_N );
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
      let e' = StaticInterpreter.Normalize.normalize env e in
      Format.eprintf "%a ---> %a@." PP.pp_expr e PP.pp_expr e'
  in
  let () =
    if _dbg then
      let f' = StaticInterpreter.Normalize.normalize env f in
      Format.eprintf "%a ---> %a@." PP.pp_expr f PP.pp_expr f'
  in
  let () =
    if _dbg then
      let res' = StaticInterpreter.Normalize.normalize env res in
      Format.eprintf "%a ---> %a@." PP.pp_expr res PP.pp_expr res'
  in

  assert (StaticInterpreter.equal_in_env env !%"N" res)

let[@warning "-44"] normalize_affectations () =
  let open StaticInterpreter.Normalize in
  let affectations = [ ("x", Z.of_int 3, None); ("y", Z.of_int 7, None) ] in
  let m_1 = Prod AMap.empty in
  let m_1', f_1' = subst_mono affectations m_1 (Z.of_int 1) in
  assert (mono_compare m_1' m_1 = 0 && f_1' = Z.of_int 1);

  let m_x = Prod (AMap.singleton "x" 1) in
  let m_x', f_x' = subst_mono affectations m_x (Z.of_int 1) in
  assert (mono_compare m_x' m_1 = 0 && f_x' = Z.of_int 3);

  let m_xy = Prod (AMap.singleton "x" 1 |> AMap.add "y" 1) in
  let m_xy', f_xy' = subst_mono affectations m_xy (Z.of_int 1) in
  assert (mono_compare m_xy' m_1 = 0 && f_xy' = Z.of_int 21);

  let p_1 = Sum (MMap.singleton m_1 (Z.of_int 1)) in
  let p_x = Sum (MMap.singleton m_x (Z.of_int 1)) in
  let p_x_1 = add_polys p_1 p_x in
  let p_xy_1 =
    Sum (MMap.singleton m_xy (Z.of_int 1) |> MMap.add m_1 (Z.of_int 1))
  in

  let p_1' = subst_poly affectations p_1 in
  let p_x' = subst_poly affectations p_x in
  let p_x_1' = subst_poly affectations p_x_1 in
  let p_xy_1' = subst_poly affectations p_xy_1 in

  assert (poly_compare p_1 p_1 = 0);
  assert (poly_compare p_1' p_1 = 0);
  assert (poly_compare p_x' (poly_of_int 3) = 0);
  assert (poly_compare p_x_1' (poly_of_int 4) = 0);
  assert (poly_compare p_xy_1' (poly_of_int 22) = 0);

  ()

let () =
  exec_tests
    [
      ("build_consts", build_consts);
      ("static.normalize", normalize);
      ("static.fpzero_example", fpzero_example);
      ("static.normalize_affectations", normalize_affectations);
    ]

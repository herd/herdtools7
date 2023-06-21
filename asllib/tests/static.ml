open Asllib
open AST
open ASTUtils
open Test_helpers.Helpers
open Test_helpers.Helpers.Infix

let _dbg = false

let build_consts () =
  let values =
    [ ("c1", !$3); ("c2", !!(E_Slice (!%"c1", [ Slice_Range (!$3, !$0) ]))) ]
  in
  let consts =
    List.map
      (fun (name, e) ->
        D_GlobalStorage
          { name; keyword = GDK_Let; ty = None; initial_value = Some e })
      values
  in
  let main =
    D_Func
      {
        name = "main";
        body = !!S_Pass;
        args = [];
        parameters = [];
        return_type = None;
      }
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
      (binop MINUS !$4 !$2, !$2, Env.Static.empty);
      ( binop PLUS (binop MINUS !%"N" !%"m") (binop MINUS !%"m" !$1),
        binop MINUS !%"N" !$1,
        Env.Static.empty );
    ]

let fpzero_example () =
  let ( -~ ) = binop MINUS and ( ==~ ) = binop EQ_OP and ( +~ ) = binop PLUS in
  let e =
    !!(E_Cond (!%"N" ==~ !$16, !$5, !!(E_Cond (!%"N" ==~ !$32, !$8, !$11))))
  in
  let f = !%"N" -~ e -~ !$1 in
  let res = !$1 +~ e +~ f in

  let env =
    let open Env.Static in
    add_local "N" integer LDK_Let empty
  in

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
  let affectations = [ ("x", 3, None); ("y", 7, None) ] in
  let m_1 = Prod AMap.empty in
  let m_1', f_1' = subst_mono affectations m_1 1 in
  assert (mono_compare m_1' m_1 = 0 && f_1' = 1);

  let m_x = Prod (AMap.singleton "x" 1) in
  let m_x', f_x' = subst_mono affectations m_x 1 in
  assert (mono_compare m_x' m_1 = 0 && f_x' = 3);

  let m_xy = Prod (AMap.singleton "x" 1 |> AMap.add "y" 1) in
  let m_xy', f_xy' = subst_mono affectations m_xy 1 in
  assert (mono_compare m_xy' m_1 = 0 && f_xy' = 21);

  let p_1 = Sum (MMap.singleton m_1 1) in
  let p_x = Sum (MMap.singleton m_x 1) in
  let p_x_1 = add_polys p_1 p_x in
  let p_xy_1 = Sum (MMap.singleton m_xy 1 |> MMap.add m_1 1) in

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

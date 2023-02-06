open Asllib
open AST
open Test_helpers.Helpers

let build_consts () =
  let ( !! ) e = ASTUtils.add_dummy_pos e in
  let ( !$ ) i = !!(E_Literal (V_Int i)) in
  let ( !% ) x = !!(E_Var x) in
  let values =
    [ ("c1", !$3); ("c2", !!(E_Slice (!%"c1", [ Slice_Range (!$3, !$0) ]))) ]
  in
  let consts =
    List.map (fun (name, e) -> D_GlobalConst (name, !!(T_Int None), e)) values
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
  let _ = Native.NativeInterpreter.run ast [] () in
  ()

let () = exec_tests [ ("build_consts", build_consts) ]

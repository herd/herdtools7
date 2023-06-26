open Asllib

module Infix = struct
  open AST

  let ( !! ) e = ASTUtils.add_dummy_pos e
  let ( !$ ) i = !!(E_Literal (V_Int i))
  let ( !% ) x = !!(E_Var x)
  let integer = !!(T_Int None)
  let boolean = !!T_Bool
end

let exec_tests =
  let exec_one_test any_failed (name, f) =
    let on_fail e =
      Printf.printf "failed 𐄂\n";
      any_failed := Some e
    in
    Printf.printf "[asl-test] Running %s ... %!" name;
    try
      f ();
      Printf.printf "ok ✔︎\n"
    with
    | Error.ASLException err as e ->
        Format.eprintf "%a@." Error.pp_error err;
        on_fail e
    | e -> on_fail e
  in
  fun li ->
    let any_failed = ref None in
    List.iter (exec_one_test any_failed) li;
    match !any_failed with Some e -> raise e | None -> ()

let build_ast_from_file filename =
  match Builder.from_file_result `ASLv1 filename with
  | Error e ->
      Format.eprintf "%a@." Error.pp_error e;
      Error.fatal e
  | Ok ast -> ast

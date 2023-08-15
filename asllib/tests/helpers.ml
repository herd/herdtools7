open Asllib

module Infix = struct
  open AST
  include ASTUtils.Infix

  let ( !! ) e = ASTUtils.add_dummy_pos e
  let ( !% ) x = !!(E_Var x)
end

let exec_tests =
  let exec_one_test any_failed (name, f) =
    let on_fail e =
      let () =
        match e with
        | Error.ASLException e ->
            Format.eprintf
              "@[<hov 2>Test@ %s@ failed@ with@ the@ following@ error:@ %a.@."
              name Error.pp_error e
        | _ -> ()
      in
      any_failed := Some e
    in
    if false then f () else try f () with e -> on_fail e
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

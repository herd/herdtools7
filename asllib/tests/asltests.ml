open Asllib
open Test_helpers.Helpers

let _dbg = false

let process_test path () =
  let () = if _dbg then Format.eprintf "Processing %s: @." path in
  let ast, ver = build_ast_from_file path in
  (* First interprete it. *)
  let () = if _dbg then Format.eprintf "@[AST: %a@]@." PP.pp_t ast in
  let tc_strictness =
    match ver with `ASLv0 -> `Silence | `ASLv1 -> `TypeCheck
  in
  let i, _ = Native.interprete tc_strictness ast in
  let () = assert (i = 0) in
  let () = if _dbg then Format.eprintf "Ran successfully.@.@." in

  (* Then ensure that printed version is understandable by the parser. *)
  match ver with
  | `ASLv1 ->
      (* V1 only *)
      let printed = PP.t_to_string ast in
      let () = if false then Printf.eprintf "Printed:\n%s\n%!" printed in
      let lexbuf = Lexing.from_string printed in
      let ast = Parser.ast Lexer.token lexbuf in
      let i, _ = Native.interprete `TypeCheck ast in
      let () = assert (i = 0) in
      ()
  | _ -> ()

let tests testdir =
  let process_filename filename =
    if Filename.check_suffix filename ".asl" then
      let path = Filename.concat testdir filename in
      Some (path, process_test path)
    else None
  in
  Sys.readdir testdir |> Array.to_list |> List.filter_map process_filename

let () = exec_tests @@ tests Sys.argv.(1)

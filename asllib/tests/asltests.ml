open Asllib
open Test_helpers.Helpers

let process_test path () =
  let ast = build_ast_from_file path in

  (* First interprete it. *)
  let _ = Native.interprete `TypeCheck ast in

  (* Then ensure that printed version is understandable by the parser. *)
  let buffer = Buffer.create 1024 in
  let formatter = Format.formatter_of_buffer buffer in
  let () = PP.pp_t formatter ast in
  let () = Format.pp_print_flush formatter () in
  let printed = Buffer.contents buffer in
  let () = if false then Printf.eprintf "Printed:\n%s\n%!" printed in
  let lexbuf = Lexing.from_string printed in
  let ast = Parser.ast Lexer.token lexbuf in
  let _ = Native.interprete `TypeCheck ast in
  ()

let tests testdir =
  let process_filename filename =
    if Filename.check_suffix filename ".asl" then
      let path = Filename.concat testdir filename in
      Some (path, process_test path)
    else None
  in
  Sys.readdir testdir |> Array.to_list |> List.filter_map process_filename

let () = exec_tests @@ tests Sys.argv.(1)

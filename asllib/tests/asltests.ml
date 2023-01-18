open Asllib
open Test_helpers.Helpers

let process_test path () =
  let ast = build_ast_from_file path in
  let _ = Native.NativeInterpreter.run ast [] [] () in
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

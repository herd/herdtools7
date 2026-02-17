open Uoaim

module Config = struct
  let includes = []
  let libdir = "./libdir"
  let cat = "aarch64.cat"
end

module S = Structure
module Def = Definition.MakeInterpreter (NameParser.Make (Config))

let read_file filename =
  let chan = open_in filename in
  let rec read_lines lines_rev =
    try
      let line = input_line chan in
      read_lines (line :: lines_rev)
    with End_of_file ->
      close_in chan;
      List.rev lines_rev
  in
  let lines = read_lines [] in
  String.concat "\n" lines

let () =
  let dir = "data" in
  let filenames =
    Sys.readdir dir |> Array.to_list
    |> List.filter (Util.String.ends_with ~suffix:".txt")
    |> List.sort String.compare
  in
  let defs =
    filenames
    |> List.filter_map (fun filename ->
        let path = Filename.concat dir filename in
        if Sys.is_directory path then None
        else
          let str = read_file path in
          if String.equal str "" then None else Some (filename, str))
  in
  defs
  |> List.iter (fun (filename, str) ->
      print_endline "-------------";
      print_endline filename;
      try Def.top ~infer_rec:true str |> List.iter print_endline
      with Util.Interpret_error { msg; context } ->
        Format.printf "Interpret error: %s (%S)@." msg context)

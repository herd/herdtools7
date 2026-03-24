open Uoaim

module Config = struct
  let includes = []
  let libdir = "./libdir"
  let cat = "aarch64.cat"
end

module S = Structure
module HR = HwReqs.MakeInterpreter (NameParser.Make (Config))

let () =
  let dir = "data/miaouified-hw-reqs" in
  let filenames =
    Sys.readdir dir |> Array.to_list |> List.sort String.compare
  in
  let reqs =
    filenames
    |> List.filter_map (fun filename ->
        let path = Filename.concat dir filename in
        if Sys.is_directory path then None
        else
          let str = Misc.input_protect Util.read_all path in
          if String.equal str "" then None else Some (filename, str))
  in
  reqs
  |> List.iter (fun (filename, str) ->
      print_endline filename;
      HR.top ~irrefl:false str |> List.iter print_endline;
      HR.top ~irrefl:true str |> List.iter print_endline)

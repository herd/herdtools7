let find_compliance_files dir =
  let dir_contents =
    let rec loop result = function
      | f :: fs when Sys.is_directory (Filename.concat dir f) ->
          Sys.readdir (Filename.concat dir f)
          |> Array.to_list
          |> List.map (Filename.concat f)
          |> List.append fs |> loop result
      | f :: fs -> loop (f :: result) fs
      | [] -> result
    in
    let start = Sys.readdir dir |> Array.to_list in
    loop [] start
  in
  let asl_suffix = ".asl" and yaml_suffix = ".yaml" in
  let chop_suffix fname =
    if Filename.check_suffix fname asl_suffix then
      Filename.chop_suffix_opt ~suffix:asl_suffix fname
    else Filename.chop_suffix_opt ~suffix:yaml_suffix fname
  in
  let all = List.filter_map chop_suffix dir_contents in
  List.sort_uniq String.compare all

module StringMap = Map.Make (String)

let generate_rules actual_stems base =
  (* Dune doesn't support targets in other directories (including directories
     nested in the current one) until 3.24. Therefore, though the test directory
     may have nested structure, we have to flatten it into a single directory
     when generating the temporary "actual" tests outputs that we will diff with
     the "expected" ones. The use of a map below ensures that we do not
     accidentally clash names of "actual" files. *)
  let actual, actual_stems =
    let name = Filename.basename base in
    match StringMap.find_opt name actual_stems with
    | None -> (name, StringMap.add name 1 actual_stems)
    | Some count ->
        let name = Printf.sprintf "%s.%d" name count in
        (name, StringMap.add name (count + 1) actual_stems)
  in
  Printf.printf
    {|
(rule
  (deps ../tests/%s.asl ../tests/%s.yaml ../schema.yaml)
  (action
  (with-stdout-to ./%s.yaml.actual
    (run ../asltest.exe --base ../tests/%s))))

(rule
  (alias runtest)
  (action
  (diff ../tests/%s.yaml %s.yaml.actual)))
|}
    base base actual base base actual;
  actual_stems

let () =
  let files = find_compliance_files "../tests" in
  let _ = List.fold_left generate_rules StringMap.empty files in
  ()

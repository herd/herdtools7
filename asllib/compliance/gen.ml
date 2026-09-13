let find_compliance_files dir =
  let dir_contents =
    let rec loop result = function
      | f :: fs when Sys.is_directory f ->
          Sys.readdir f
          |> Array.fold_left (fun fs f' -> Filename.concat f f' :: fs) fs
          |> loop result
      | f :: fs -> loop (f :: result) fs
      | [] -> result
    in
    loop [] [ dir ]
  in
  let asl_suffix = ".asl" and yaml_suffix = ".yaml" in
  let chop_suffix fname =
    match Filename.chop_suffix_opt ~suffix:asl_suffix fname with
    | Some _ as o -> o
    | None -> Filename.chop_suffix_opt ~suffix:yaml_suffix fname
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
  (* Keep the generated fragment to rules and aliases. Dune's [dynamic_include]
     supports generated rule-like stanzas, but deliberately excludes stanzas
     such as libraries. *)
  Printf.printf
    {|
(rule
  (deps %s.asl %s.yaml)
  (enabled_if %%{lib-available:yaml})
  (action
  (with-stdout-to ./%s.yaml.actual
    (run ../asltest.exe %s))))

(rule
  (alias runtest)
  (enabled_if %%{lib-available:yaml})
  (action
  (diff %s.yaml %s.yaml.actual)))
|}
    base base actual base base actual;
  actual_stems

let () =
  let files = find_compliance_files "../tests" in
  let _ = List.fold_left generate_rules StringMap.empty files in
  ()

(** This program takes a list of files that need to be run, and checks that they
    are present in the [run.t] file.

    It runs in 2 steps: 1. Extract all the words in the [run.t] file that look
    like asl filenames. More specifically, extrat the matches of
    [asl_filename_regexp]. 2. For each file that need to be run, check that they
    are in the list of filenames extracted from the [run.t] file.

    Assumption: it is enough for the filename to be mentioned in the run.t file.
    This allows the user to disable the running of a file by commenting it
    execution in the run.t file. *)

(* Step 1: Extraction of the *.asl filenames *)

let asl_filename_regexp = Str.regexp {|[^ ]+\.asl|}
let is_asl_file s = Str.string_match asl_filename_regexp s 0

(** [process_line acc start line] process the string [line] starting at index
    [start] and adds to [acc] all the occurences of [asl_filename_regexp]. *)
let rec process_line acc start line : string list =
  try
    let start' : int = Str.search_forward asl_filename_regexp line start in
    let filename = Str.matched_string line in
    let end' = start' + String.length filename in
    process_line (filename :: acc) end' line
  with Not_found -> acc

(** [run_files dir] extracts from the file [dir ^ "/run.t"] all the matches of
    [asl_filename_regexp]. *)
let run_files dir : string list =
  let run_t_file = Filename.concat dir "run.t" in
  let rec loop o acc =
    try input_line o |> process_line acc 0 |> loop o with End_of_file -> acc
  in
  let o = open_in run_t_file in
  Fun.protect ~finally:(fun () -> close_in_noerr o) @@ fun () -> loop o []

(* Step 2: Check that all the files are run. *)

(** [check_one dir committed_file] checks that the files [committed_file] is
    present in the file [dir ^ "/run.t"]. If it isn't, we log it to stderr and
    exit with code 1. *)
let check_one dir run_files committed_file : unit =
  let committed_file = Filename.basename committed_file in
  if is_asl_file committed_file && not (List.mem committed_file run_files) then
    let whole_name = Filename.concat dir committed_file in
    let () =
      Printf.eprintf "File %s is committed but not run.\n%!" whole_name
    in
    exit 1

(** [main dir committed_files] checks that all the files in [committed_files]
    are present in the file [dir ^ "/run.t"].

    If one file is not present in the [run.t] file, we log it to stderr and exit
    with code 1. *)
let main dir run_files committed_files : unit =
  List.iter (check_one dir run_files) committed_files

(** [show_usage_and_exit ()] prints the usage of this executable and exit with
    exit code 2. *)
let show_usage_and_exit () =
  Format.eprintf "%s %a" Sys.executable_name Format.pp_print_text
    "checks that no committed file in DIR is not run by its run.t";
  Format.eprintf "\n\nUSAGE:\n  %s DIR COMMITTED_FILES" Sys.executable_name;
  exit 2

(* Main entry point: parse command line arguments and calls [run_files] then
   [main]. *)
let () =
  let run_t_dir, committed_files =
    match Array.to_list Sys.argv with
    | _ :: first :: remaining -> (first, remaining)
    | [] | _ :: [] -> show_usage_and_exit ()
  in
  let run_files = run_files run_t_dir in
  let () =
    if false then
      let open Format in
      eprintf "@[<v 2>Run files:@ %a@]@."
        (pp_print_list ~pp_sep:pp_print_space pp_print_string)
        run_files
  in
  let () =
    if false then
      let open Format in
      eprintf "@[<v 2>Committed files:@ %a@]@."
        (pp_print_list ~pp_sep:pp_print_space pp_print_string)
        committed_files
  in
  main run_t_dir run_files committed_files

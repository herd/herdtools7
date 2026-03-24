(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Utilities for running Herd binaries in tests. *)

let _dbg = false

type path = string

type stdout_lines = string list
type stderr_lines = string list
type speedcheck = [`True | `False | `Fast]

let outname l = l ^ ".out"
and errname l = l ^ ".err"

let read_file name =
  if Sys.file_exists name then
    Filesystem.read_file name Channel.read_lines
  else []

(*****************************)
(* Log "complete" comparison *)
(*****************************)

let time_re = Str.regexp "^Time "

let is_stable line = not (Str.string_match time_re line 0)

let without_unstable_lines lines = List.filter is_stable lines

let hash_re = Str.regexp "^Hash="

(* If both string contain the "unrolling limit exceeded",
  then droping the file names and temperate directory,
  and matching the following characters. *)
let match_unrolling_limit l1 l2 =
  let re = Str.regexp "unrolling limit exceeded.*$" in
  try
    let _ = Str.search_forward re l1 0 in
    let matched_l1 = Str.matched_string l1 in
    let _ = Str.search_forward re l2 0 in
    let matched_l2 = Str.matched_string l2 in
    matched_l1 = matched_l2
  with Not_found -> false

let compare_lines nohash l1 l2 =
  match_unrolling_limit l1 l2
  || (nohash
   && Str.string_match hash_re l1 0
   && Str.string_match hash_re l2 0)
  || String.equal l1 l2

let rec log_equal nohash xs ys =
  match xs,ys with
  | [],[] -> true
  | x::xs,y::ys ->
      compare_lines nohash x y && log_equal nohash xs ys
  | ([],_::_)|(_::_,[]) -> false

let log_diff  nohash xs ys = not (log_equal nohash xs ys)


(**************************)
(* Observation comparison *)
(**************************)

let rec extract_obs = function
  | [] -> "NoObs"
  | x::xs ->
      let ys = String.split_on_char ' ' x in
      match ys with
      | "Observation"::_::_::"0"::"0"::_ -> "Empty"
      | "Observation"::_::key::_ -> key
      | _ -> extract_obs xs

let obs_equal litmus xs ys =
  let ox = extract_obs xs and oy = extract_obs ys in
  let r = String.equal ox oy in
  if not r then begin
    Printf.eprintf "%s: Observation failed, %s vs. %s\n%!" litmus  ox oy
  end ;
  r

let obs_diff litmus _ xs ys = not (obs_equal litmus xs ys)

(**************************)
(* Final state comparison *)
(**************************)

let pp_norm_states chan =
  List.iter
    (fun xs ->
       let xs = List.map (Printf.sprintf "%s") xs in
       Printf.fprintf chan "%s\n" (String.concat " " xs))

let norm_states xs =
  List.fold_left
    (fun k x -> match LexState.as_state x with
       | [] -> k
       | ws -> List.sort String.compare ws::k)
    [] xs
  |> List.sort (Misc.list_compare String.compare)

let states_equal litmus xs ys =
  let xs0 = norm_states xs and ys0 = norm_states ys in
  let eq = Misc.list_eq (Misc.list_eq String.equal) xs0 ys0 in
  if _dbg || not eq then begin
    Printf.eprintf "%s on %s\n" (if eq then "Ok" else "Fail") litmus ;
    Printf.eprintf "** Expected:\n" ; pp_norm_states stderr ys0 ;
    Printf.eprintf "** Out     :\n" ; pp_norm_states stderr xs0 ;
    prerr_endline "" ;
    ()
  end ;
  eq

let states_diff litmus _ xs ys = not (states_equal litmus xs ys)

(** Type of comparison for stdout logs *)

type check =
  | All (** Complete, except non stable item such as time *)
  | Obs (** Observation, _i.e._ Newver/Sometimes/Always *)
  | Sta (** Final states *)

let pp_check = function
  | All -> "All"
  | Obs -> "Obs"
  | Sta -> "States"

let checklog litmus  = function
  | All -> log_diff
  | Obs -> obs_diff litmus
  | Sta -> states_diff litmus

let checkerrlog = function
  | All -> log_diff
  | Obs|Sta ->
  fun _ xs ys ->
    match xs,ys with
    | ([],[])|(_::_,_::_) -> false
    | ([],_::_)|(_::_,[]) -> true

let herd_args ~bell ~cat ~conf ~variants ~libdir ~timeout ~speedcheck
    ~checkfilter =
  let timeout =
    match timeout with
    | None -> []
    | Some t -> ["-timeout"; Printf.sprintf "%.2f" t;] in
  let bells =
    match bell with
    | None -> []
    | Some bell -> ["-bell"; bell]
  and cats =
    match cat with
    | None -> []
    | Some cat -> ["-cat"; cat; "-I"; Filename.dirname cat]
  and confs =
    match conf with
    | None -> []
    | Some conf -> ["-conf"; conf]
  and variants =
    List.concat (List.map (fun v -> ["-variant"; v]) variants)
  and speedchecks =
    match speedcheck with
    | None -> []
    | Some `True -> ["-speedcheck"; "true"]
    | Some `Fast -> ["-speedcheck"; "fast"]
    | Some `False -> ["-speedcheck"; "false"]
  and checkfilters =
    match checkfilter with
    | None -> []
    | Some b -> ["-checkfilter"; string_of_bool b]
  and libdirs = ["-set-libdir"; libdir]
  and exits = ["-exit"; "true"]
  in
  List.concat
    [
      exits; libdirs; timeout; bells; cats; confs; variants; speedchecks;
      checkfilters
    ]

let apply_args herd j herd_args =
  let herd_args = String.concat "," herd_args in
  ["-com"; herd; "-j" ; Printf.sprintf "%i" j; "-comargs"; herd_args;]

let apply_redirect_args ?(verbose=false) herd j herd_args =
  let herd_args = herd::herd_args in
  let herd_args =
    if verbose then "-verbose"::herd_args
    else herd_args in
  let redirect_args = String.concat "," herd_args in
  let redirect =
    Filename.concat (Filename.dirname Sys.argv.(0))
      "herd_redirect.exe" in
  ["-com"; redirect; "-j"; Printf.sprintf "%i" j; "-comargs"; redirect_args;]

let herd_command ~bell ~cat ~conf ~variants ~libdir herd ?j ?timeout
    ?speedcheck ?checkfilter litmuses =
  let args =
    herd_args ~bell ~cat ~conf ~variants ~libdir ~timeout ~speedcheck
      ~checkfilter
  in
  match j with
  | None ->
     Command.command herd (args @ litmuses)
  | Some j ->
     let mapply = Filename.concat (Filename.dirname herd) "mapply7" in
     let args = apply_args  herd j args in
     Command.command mapply  (args @ litmuses)

let check_tags s =
  try
    let i = String.index s ' ' in
    let q = String.sub s 0 i in
    match q with
    | "Observation"
    | "Time"
      -> true
    | _ -> false
  with Not_found -> false

let check line = if check_tags line then prerr_endline line

let do_run_herd_args verbose herd args ?j litmuses =
  let litmuses = Base.Iter.of_list litmuses in
  (*
   * Record stdout and stderr to two sources if we need
   * to reason about them separately.
   *)
  let lines = ref [] in
  let err_lines = ref [] in
  let read_line =
    if verbose then
      fun line -> check line; lines := line :: !lines
    else
      fun line -> lines := line :: !lines in
  let read_err_line line = err_lines := line :: !err_lines in
  let r =
    match j with
    | None ->
       Command.NonBlock.run_status
         ~stdin:litmuses ~stdout:read_line ~stderr:read_err_line herd args
    | Some j ->
       let j = max 2 j in
       let mapply = Filename.concat (Filename.dirname herd) "mapply7" in
       let args = apply_args herd j args in
       Command.NonBlock.run_status
         ~stdin:litmuses ~stdout:read_line ~stderr:read_err_line mapply args in
  (r,without_unstable_lines (List.rev !lines), (List.rev !err_lines))

let run_herd_args ?(verbose=false) herd args litmus =
  do_run_herd_args verbose herd args [litmus]

let run_herd ?(verbose=false) ~bell ~cat ~conf ~variants ~libdir herd ?j
    ?timeout ?speedcheck ?checkfilter litmuses =
  let args =
    herd_args ~bell ~cat ~conf ~variants ~libdir ~timeout ~speedcheck
      ~checkfilter
  in
  do_run_herd_args verbose herd args ?j litmuses

let run_herd_concurrent ?verbose ~bell ~cat ~conf ~variants ~libdir herd ~j litmuses =
  let args =
    herd_args ~bell:bell ~cat:cat ~conf:conf ~variants:variants ~libdir:libdir
      ~timeout:None ~checkfilter:None ~speedcheck:None
  in
  let litmuses = Base.Iter.of_list litmuses in
  let j = max 2 j in
  let mapply = Filename.concat (Filename.dirname herd) "mapply7" in
  let args = apply_redirect_args ?verbose herd j args in
  let r = Command.NonBlock.run_status ~stdin:litmuses  mapply args in
  r

let read_some_file litmus name =
  if name = "" then None
  else
    try Some (Filesystem.read_file name Channel.read_lines)
    with _ ->
      begin
        Printf.printf "Failed %s : Missing file '%s'\n" litmus name ;
        None
      end

let do_check_output
    check nohash litmus expected  expected_failure expected_warn t =
  let () =
    let _,lines,_ = t in
    if false && lines <> [] then begin
      Printf.eprintf "Expected (%s) %s, Out of test:\n"
        (pp_check check)
        expected ;
      List.iter prerr_endline lines ;
      ()
    end in

  match t with
    | 0,[],[] -> true (* Can occur in case of controlled timeout *)
    | _,[],[] ->
       Printf.printf
         "Failed %s : Herd finished but returned no output or errors\n" litmus ;
       false
    | 0,(_::_ as stdout), [] -> (* Herd finished without errors - normal *)
       begin
         match read_some_file litmus expected with
         | None -> false
         | Some expected_output ->
             if
               checklog litmus check nohash stdout expected_output
             then begin
               Printf.printf "Failed %s : Logs do not match\n%!" litmus ;
               false
            end else true
       end

    | r,[], (_::_ as stderr) when r <> 0 -> (* Herd finished with errors - check expected failure *)
       begin
         match read_some_file litmus expected_failure with
         | None -> false
         | Some expected_failure_output ->
            if checkerrlog check nohash stderr expected_failure_output then begin
              Printf.printf
                  "Failed %s : Expected Failure Logs do not match\n" litmus ;
              false
            end else true
       end
    | 0,(_::_ as stdout),(_::_ as stderr) ->
       (* Herd returned both output and errors *)
        begin
         match read_some_file litmus expected with
         | None -> false
         | Some expected_output ->
             if
               checklog litmus check nohash stdout expected_output
             then begin
               Printf.printf "Failed %s : Logs do not match\n" litmus ;
               false
             end else
               match read_some_file litmus expected_warn with
               | None ->
                   if _dbg then begin
                     Printf.eprintf
                       "** Unexpected warning stderr for %s\n"
                       (Filename.basename litmus) ;
                     List.iter prerr_endline stderr
                   end ;
                   false
              | Some expected_warn ->
                  if log_diff nohash stderr expected_warn then begin
                    Printf.printf
                      "Failed %s : Warning logs do not match\n" litmus ;
                    false
                  end else true
        end
    | r,stdout,stderr ->
       let some f =
         match f with
         | [] -> "no"
         | _::_ -> "some" in
       Printf.printf
         "Failed %s : unexpected exit code %i, %s output %s error.\n"
         litmus r (some stdout) (some stderr) ;
       if _dbg then begin
         let display tag = function
           | [] -> ()
           | _::_ as lines ->
             Printf.printf "** %s %%\n" tag ;
             List.iter print_endline lines in
         display "stdout" stdout ;
         display "stderr" stderr
       end ;
       false

let read_output_files litmus =
  let o = read_file (outname litmus)
  and e = read_file (errname litmus) in
  o,e

let output_matches_expected ?(check=All) ?(nohash=false) litmus expected =
  try
    let o,e = read_output_files litmus in
    do_check_output check nohash litmus expected "" "" (0,o,e)
  with Command.Error e ->
     Printf.printf "Failed %s : %s \n" litmus
       (Command.string_of_error e) ; false

let do_herd_output_matches_expected
    (check:check) nohash
    do_run litmus expected expected_failure expected_warn =
  try
    let t = do_run litmus in
    do_check_output
      check nohash litmus expected expected_failure expected_warn t
  with
  | Command.Error e ->
     Printf.printf "Failed %s : %s \n" litmus
       (Command.string_of_error e) ; false

let herd_output_matches_expected
    ?(verbose=false) ?(check=All) ?(nohash=false)
    ~bell ~cat ~conf ~variants ~libdir
    herd litmus expected expected_failure expected_warn =
  do_herd_output_matches_expected
    check nohash
    (fun litmus ->
       run_herd
        ~verbose:verbose
        ~bell:bell ~cat:cat ~conf:conf
        ~variants:variants ~libdir:libdir herd [litmus])
    litmus expected expected_failure expected_warn

let herd_args_output_matches_expected
    ?(verbose=false) ?(check=All) ?(nohash=false)
    herd args  litmus expected expected_failure expected_warn =
  do_herd_output_matches_expected
    check nohash
    (run_herd_args ~verbose:verbose herd args)
    litmus expected expected_failure expected_warn

let is_litmus path = Filename.check_suffix path ".litmus"
let is_expected path = Filename.check_suffix path ".litmus.expected"

let expected_of_litmus litmus = litmus ^ ".expected"
let litmus_of_expected expected = Filename.chop_suffix expected ".expected"

let expected_failure_of_litmus litmus = litmus ^ ".expected-failure"
let litmus_of_expected_failure expected = Filename.chop_suffix expected ".expected-failure"

let expected_warn_of_litmus litmus = litmus ^ ".expected-warn"

let remove_if_exists path =
  if Sys.file_exists path then
    Sys.remove path

let write_file path lines =
  Filesystem.write_file path (fun o -> Channel.write_lines o lines)


let promote litmus t =
  let expected = expected_of_litmus litmus in
  let expected_failure = expected_failure_of_litmus litmus in
  match t with
  | 0, [], [] ->
     Printf.printf "Failed %s : Returned neither stdout nor stderr\n" litmus ;
     false

  | 0, out, [] ->
     remove_if_exists expected_failure ;
     write_file expected out ;
     true

  | r, [], err when r <> 0 ->
     remove_if_exists expected ;
     write_file expected_failure err ;
     true

  | 0, out, err ->
     write_file expected out ;
     let expected_warn = expected_warn_of_litmus litmus in
     write_file expected_warn err ;
     true

  | r, _, _  ->
     Printf.printf "Failed %s : unexpected exit code %i\n" litmus r ;
     false

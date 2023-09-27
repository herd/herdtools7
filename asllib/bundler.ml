(******************************************************************************)
(*                                ASLRef                                      *)
(*                                                                            *)
(* Copyright (c) 2022-present, Arm Limited or its affiliates.                 *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(* SPDX-License-Identifier: Apache-2.0                                        *)
(******************************************************************************)
(* Authors:                                                                   *)
(* Hadrien Renaud, University College London, UK.                             *)
(******************************************************************************)
(* Disclaimer:                                                                *)
(* This material covers both ASLv0 (viz, the existing ASL pseudocode language *)
(* which appears in the Arm Architecture Reference Manual) and ASLv1, a new,  *)
(* experimental, and as yet unreleased version of ASL.                        *)
(* This material is work in progress, more precisely at pre-Alpha quality as  *)
(* per Arm’s quality standards.                                               *)
(* In particular, this means that it would be premature to base any           *)
(* production tool development on this material.                              *)
(* However, any feedback, question, query and feature request would be most   *)
(* welcome; those can be sent to Arm’s Architecture Formal Team Lead          *)
(* Jade Alglave <jade.alglave@arm.com>, or by raising issues or PRs to the    *)
(* herdtools7 github repository.                                              *)
(******************************************************************************)

open Asllib
module IMap = ASTUtils.IMap
module ISet = ASTUtils.ISet

type identifier_state =
  | NotYetHandled
  | NotFound
  | Parsed of unit AST.t
  | BlackListed

type bundler_state = identifier_state IMap.t

let to_look_dirs = ref []
let instr_dir = ref ""
let outdir = ref @@ Sys.getcwd ()
let ( // ) = Filename.concat
let whitelist = ref []
let blacklist = ref ISet.empty

let is_interesting name filename =
  String.equal name @@ Filename.remove_extension filename

let build_ast_from_file ?(is_opn = false) f =
  (* Taken from herdtools7/lib/Pos.ml *)
  let pp_pos chan pos =
    let open Lexing in
    Printf.fprintf chan "File \"%s\", line %i, character %i" pos.pos_fname
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol)
  in
  let parse = if is_opn then Parser.opn else Parser.ast in
  let chan = open_in f in
  let lexbuf = Lexing.from_channel chan in
  let () =
    lexbuf.Lexing.lex_curr_p <-
      { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = f }
  in
  let res =
    try Some (parse Lexer.token lexbuf) with
    | Parser.Error ->
        Printf.eprintf "%s:%a: Cannot parse. Ignoring file.\n" f pp_pos
          lexbuf.Lexing.lex_curr_p;
        None
    | Lexer.LexerError ->
        Printf.eprintf "%s:%a: unknown token. Ignoring file.\n" f pp_pos
          lexbuf.Lexing.lex_curr_p;
        None
  in
  close_in chan;
  res

let add_if_not_there = function None -> Some NotYetHandled | Some t -> Some t

let has_been_handled state name =
  match IMap.find_opt name state with
  | None | Some NotYetHandled -> false
  | Some (Parsed _) | Some NotFound | Some BlackListed -> true

let update_state_and_queue_from_ast ast (state, queue) =
  let uses = ASTUtils.used_identifiers ast in
  let state =
    ISet.fold (fun name -> IMap.update name add_if_not_there) uses state
  and queue =
    ISet.fold
      (fun name queue ->
        if has_been_handled state name then queue else name :: queue)
      uses queue
  in
  (state, queue)

let handle_one (state, queue) name =
  let interesting_filenames_in dir =
    Sys.readdir dir |> Array.to_seq
    |> Seq.filter (is_interesting name)
    |> Seq.map (Filename.concat dir)
  in
  let ast =
    List.to_seq !to_look_dirs
    |> Seq.flat_map interesting_filenames_in
    |> Seq.filter_map (build_ast_from_file ~is_opn:false)
    |> Seq.flat_map List.to_seq |> List.of_seq
  in
  match ast with
  | _ :: _ ->
      let () = Printf.eprintf "Adding ast from %s.\n" name in
      update_state_and_queue_from_ast ast
        (IMap.add name (Parsed ast) state, queue)
  | [] ->
      let () = Printf.eprintf "No AST found for identifier %s.\n" name in
      (IMap.add name NotFound state, queue)

let rec handle_max (state, queue) =
  match queue with
  | [] -> state
  | name :: queue ->
      if has_been_handled state name then handle_max (state, queue)
      else if ISet.mem name !blacklist then
        handle_max (IMap.add name BlackListed state, queue)
      else handle_max @@ handle_one (state, queue) name

let init instr_name =
  let filename = !instr_dir // instr_name // (instr_name ^ ".opn") in
  let ast =
    match build_ast_from_file filename ~is_opn:true with
    | None -> failwith "Cannot find instruction %s"
    | Some ast -> ast
  in
  update_state_and_queue_from_ast ast
    (IMap.singleton "main" (Parsed ast), !whitelist)

let check_all_dirs_exists () =
  let dir_exists s = Sys.file_exists s && Sys.is_directory s in
  to_look_dirs := List.filter dir_exists !to_look_dirs;
  if not (dir_exists !outdir) then
    failwith ("Cannot find output directory: " ^ !outdir);
  if String.equal !instr_dir "" then instr_dir := Sys.getcwd ()
  else if not (dir_exists !instr_dir) then
    failwith ("Cannot find instruction directory: %s." ^ !instr_dir)

let parse_args () =
  let add_to_ref r s = r := s :: !r in
  let add_to_ref_set r s = r := ISet.add s !r in
  let instrs = ref [] in
  let set_pseudocode_dir s =
    to_look_dirs := (s // "functions") :: (s // "defs") :: !to_look_dirs;
    if String.equal !instr_dir "" then instr_dir := s // "instrs"
  in
  let speclist =
    [
      ( "--pseudocode-dir",
        Arg.String set_pseudocode_dir,
        "If they exist, add the subdirs 'functions', 'defs' and 'instrs' to \
         the parsed dirs." );
      ( "--dirs",
        Arg.String (add_to_ref to_look_dirs),
        "Add the following dir to the parsed dirs." );
      ("--instr-dir", Arg.Set_string instr_dir, "Set the instruction directory.");
      ( "-o",
        Arg.Set_string outdir,
        "Directory in which write the files. Default to current." );
      ("-i", Arg.String (add_to_ref_set blacklist), "Ignore this identifier.");
      ( "-a",
        Arg.String (add_to_ref whitelist),
        "Add this file and all dependencies." );
    ]
  in
  let usage =
    Printf.sprintf
      "Usage: %s OPTIONS INSTRS ...\n\n\
       INSTRS is a list of instruction names that can be found under the name \
       INSTR_DIR/INSTR/INSTR.opn\n\n\
       OPTIONS"
      (Filename.basename Sys.executable_name)
  in
  let anon_fun = add_to_ref instrs in
  let () = Arg.parse speclist anon_fun usage in
  let () = check_all_dirs_exists () in
  !instrs

let write_ast_to instr_name ast =
  let chan = open_out (!outdir // (instr_name ^ ".asl")) in
  let formatter = Format.formatter_of_out_channel chan in
  Format.pp_set_margin formatter 100;
  PP.pp_t formatter ast;
  Format.pp_print_flush formatter ();
  close_out chan

let main instr_name =
  let final_state = init instr_name |> handle_max in
  let folder _key = function
    | Parsed ast -> List.rev_append ast
    | NotFound | BlackListed -> Fun.id
    | NotYetHandled -> assert false
  in
  let final_ast = IMap.fold folder final_state [] |> List.rev in
  write_ast_to instr_name final_ast

let () =
  let instrs = parse_args () in
  List.iter main instrs

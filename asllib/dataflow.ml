(*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 *)

open Asllib
module U = ASTUtils
module IMap = U.IMap
module ISet = U.ISet

let dfg_blacklist =
  [ "X"; "Mem" ] |> List.to_seq
  |> Fun.flip Seq.zip (Seq.repeat ())
  |> Hashtbl.of_seq

let is_not_blacklisted s = not (Hashtbl.mem dfg_blacklist s)

let def_directly_stmt s =
  let open AST in
  let rec def_le le acc =
    match le.desc with
    | LE_Var x -> x :: acc
    | LE_Destructuring li | LE_Concat (li, _) -> List.fold_right def_le li acc
    | LE_Discard -> acc
    | LE_SetArray _ -> failwith "Not yet implemented"
    | LE_SetField (le, _) | LE_SetFields (le, _) | LE_Slice (le, _) ->
        def_le le acc
  in
  let rec def_ldi ldi acc =
    match ldi with
    | LDI_Var (x, _) -> x :: acc
    | LDI_Discard _ -> acc
    | LDI_Tuple (ldis, _) -> List.fold_right def_ldi ldis acc
  in
  match s.desc with
  | S_Assign (le, _, _) -> def_le le []
  | S_Decl (_, ldi, _) -> def_ldi ldi []
  | _ -> []

let option_set_union us = function
  | None -> Some us
  | Some us' -> Some (ISet.union us us')

let map_set_add values map name = IMap.update name (option_set_union values) map

let rec fold_compound_stmts folder stmt acc =
  let open AST in
  match stmt.desc with
  | S_Debug _ | S_Assert _ | S_Pass | S_Decl _ | S_Assign _ | S_Call _
  | S_Return _ ->
      folder stmt acc
  | S_Cond (_, s1, s2) | S_Seq (s1, s2) ->
      fold_compound_stmts folder s1 acc |> fold_compound_stmts folder s2
  | S_For (_, _, _, _, _)
  | S_While (_, _)
  | S_Repeat (_, _)
  | S_Throw _
  | S_Case (_, _)
  | S_Try (_, _, _) ->
      failwith "not yet implemented: loops and exceptions"

let get_dataflow_graph s =
  let on_one stmt acc =
    let ds = def_directly_stmt stmt |> List.filter is_not_blacklisted in
    if List.is_empty ds then acc
    else
      let us = U.used_identifiers_stmt stmt |> ISet.filter is_not_blacklisted in
      List.fold_left (map_set_add us) acc ds
  in
  fold_compound_stmts on_one s IMap.empty

let pp_dataflow_graph = IMap.pp_print ISet.pp_print

let print_dataflow_graph filename name_to_analyze dfg =
  Format.printf "@[<v 2>Dataflow graph for %s / %s :@ %a@]@." filename
    name_to_analyze pp_dataflow_graph dfg

let get_interesting_things =
  let open AST in
  let get_args args = List.fold_left U.use_e ISet.empty args in
  let get_args_slices args = List.map U.slice_as_single args |> get_args in
  let rec get_e e acc =
    match e.desc with
    | E_Slice ({ desc = E_Var ("X" as name); _ }, [ x; _datasize ]) ->
        (`Input, name, get_args_slices [ x ]) :: acc
    | E_Slice ({ desc = E_Var (("Mem" | "MemU" | "X" | "R") as name); _ }, args)
      ->
        (`Input, name, get_args_slices args) :: acc
    | E_Call (("ShiftReg" as name), args, _) ->
        (`Input, name, get_args args) :: acc
    | E_Binop (_op, e1, e2) -> get_e e1 acc |> get_e e2
    | E_Unop (_op, e) -> get_e e acc
    | E_Concat es | E_Call (_, es, _) | E_Tuple es ->
        List.fold_right get_e es acc
    | _ -> acc
  and get_le le acc =
    match le.desc with
    | LE_Slice ({ desc = LE_Var ("X" as name); _ }, [ x; _datasize ]) ->
        (`Output, name, get_args_slices [ x ]) :: acc
    | LE_Slice
        ({ desc = LE_Var (("Mem" | "MemU" | "X" | "R") as name); _ }, args) ->
        (`Output, name, get_args_slices args) :: acc
    | _ -> acc
  and get_s s acc =
    match s.desc with
    | S_Decl (_ldk, _ldi, Some e) -> get_e e acc
    | S_Assign (le, e, _) -> get_le le acc |> get_e e
    | _ -> acc
  in
  fun s -> fold_compound_stmts get_s s []

let get_leafs =
  let rec all_children dfg start (seen, leafs) =
    if ISet.mem start seen then (seen, leafs)
    else
      let seen = ISet.add start seen in
      match IMap.find_opt start dfg with
      | None -> (seen, ISet.add start leafs)
      | Some set ->
          if ISet.is_empty set then (seen, ISet.add start leafs)
          else ISet.fold (all_children dfg) set (seen, leafs)
  in
  let make_one dfg (t, name, set) =
    let seen = ISet.empty and leafs = ISet.empty in
    let _seen, leafs = ISet.fold (all_children dfg) set (seen, leafs) in
    (t, name, leafs)
  in
  fun dfg interesting_things -> List.map (make_one dfg) interesting_things

let print_interesting interesting_things =
  let open Format in
  let t_to_s = function `Input -> "Input" | `Output -> "Output" in
  let print_one fmt (t, name, used) =
    fprintf fmt "@[<hov 2>%s at %S:@ %a@]" (t_to_s t) name ISet.pp_print used
  in
  printf "@[<v 2>Interesting stuff:@ %a@]@."
    (pp_print_list ~pp_sep:pp_print_space print_one)
    interesting_things

type args = {
  target_file : string;
  asl_version : Builder.version;
  name_to_analyze : string;
  ignore : string list;
  ignore_globals_from : string list;
  show_ignored : bool;
  raw : bool;
  opn : bool;
}

let append_to_string_list_ref r s = r := s :: !r

let parse_args () : args =
  let target_file = ref None in
  let asl_version = ref `ASLv1 in
  let opn = ref false in
  let raw = ref false in
  let set_v0 () = asl_version := `ASLv0 in
  let set_v1 () = asl_version := `ASLv1 in
  let name_to_analyze = ref "main" in
  let ignore_globals_from : string list ref = ref [] in
  let ignore : string list ref = ref [] in
  let show_ignored = ref false in

  let speclist =
    [
      ("-0", Arg.Unit set_v0, " Use ASLv0 parser.");
      ("-1", Arg.Unit set_v1, " Use ASLv1 parser. (default)");
      ("--subprogram", Arg.Set_string name_to_analyze, " Analyse this function.");
      ("--opn", Arg.Set opn, " Parse the file as an opn file.");
      ( "--raw",
        Arg.Set raw,
        " Don't perform dataflow analysis, only pattern matching." );
      ( "--ignore",
        Arg.String (append_to_string_list_ref ignore),
        " Ignore this identifier." );
      ( "--ignore-from",
        Arg.String (append_to_string_list_ref ignore_globals_from),
        " Ignore in dataflow all globals defined in this file." );
      ( "--show-ignored",
        Arg.Set show_ignored,
        " Print the ignored identifiers for dataflow analysis." );
    ]
    |> Arg.align ?limit:None
  in

  let prog =
    if Array.length Sys.argv > 0 then Filename.basename Sys.argv.(0)
    else "aslref"
  in

  let anon_fun s =
    match !target_file with
    | None -> target_file := Some s
    | Some _ -> raise Arg.(Bad "Only one file can be specified.")
  in

  let usage_msg =
    Printf.sprintf
      {|ASL dataflow analysis tool.

USAGE:
  %s [OPTIONS] FILE

EXAMPLE:
  $ %s --ignored-from herd/lidbir/asl-pseudocode/shared_pseudocode.asl --ignore MemOp_LOAD -0 --opn LDR.asl
  Interesting stuff:
    Output at "X": {Rd}
    Input at "ShiftReg": {Rm, imm6, sf, shift}
    Input at "X": {Rn}

OPTIONS:|}
      prog prog
  in

  let () = Arg.parse speclist anon_fun usage_msg in

  let target_file =
    match !target_file with
    | Some f -> f
    | None ->
        Printf.printf "an argument is expected.\n";
        Arg.usage speclist usage_msg;
        exit 1
  in

  {
    target_file;
    asl_version = !asl_version;
    name_to_analyze = !name_to_analyze;
    ignore = !ignore;
    ignore_globals_from = !ignore_globals_from;
    show_ignored = !show_ignored;
    opn = !opn;
    raw = !raw;
  }

let () =
  let args = parse_args () in

  let () = List.iter (fun s -> Hashtbl.add dfg_blacklist s ()) args.ignore in

  let () =
    let add_ignored_from_file filename =
      match Builder.from_file_multi_version ~ast_type:`Ast `Any filename with
      | Ok ast ->
          List.iter (fun d -> Hashtbl.add dfg_blacklist (U.def_decl d) ()) ast
      | Error e ->
          Format.eprintf
            "@[<hov 2>Error@ will@ processing@ file@ with@ identifiers@ to@ \
             be@ ignored:@ @[<hov>%a@]@."
            Error.pp_error e
    in
    List.iter add_ignored_from_file args.ignore_globals_from
  in

  let () =
    if args.show_ignored then
      let open Format in
      printf "@[<hov 2>Ignored identifiers during dataflow analysis:@ %a@]@."
        (pp_print_seq ~pp_sep:pp_print_space pp_print_string)
        (Hashtbl.to_seq_keys dfg_blacklist)
  in

  let ast =
    let ast_type = if args.opn then `Opn else `Ast in
    try Builder.from_file ~ast_type args.asl_version args.target_file
    with Error.ASLException e ->
      Format.eprintf "@[<hv 2>Parsing failed with error:@ %a@]" Error.pp_error e;
      exit 1
  in

  let decl_to_analyze = U.find_decl_by_name args.name_to_analyze ast in

  let stmt_to_analyze =
    match decl_to_analyze.desc with
    | D_Func { body = SB_ASL stmt; _ } -> stmt
    | _ -> failwith "Not yet implemented."
  in

  let raw_results = get_interesting_things stmt_to_analyze in

  let results =
    if args.raw then raw_results
    else
      let dfg = get_dataflow_graph stmt_to_analyze in
      get_leafs dfg raw_results
  in

  let () = print_interesting results in

  ()

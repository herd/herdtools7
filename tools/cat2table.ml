(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2025-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

let prog =
  if Array.length Sys.argv > 0 then
    Filename.basename Sys.argv.(0)
  else "cat2table"

module Make
  (O: sig
    val verbose: bool
    val includes: string list
    val libdir: string
  end) =
struct

  let libfind =
    let module ML =
      MyLib.Make
        (struct
          let includes = O.includes
          let env = Some "HERDLIB"
          let libdir = O.libdir
          let debug = O.verbose
        end) in
    ML.find

  module ParserConfig =
    struct
      let debug = false
      let libfind = libfind
    end

  module P = ParseModel.Make(ParserConfig)

  open AST

  module Cache = TxtLoc.Extract()
  
  (* A character that we know can't be part of a variable name *)
  let sep = '$'

  let to_alias s n =
    s ^ String.make 1 sep ^ string_of_int n

  let from_alias s =
    match String.index_opt s sep with
    | Some idx -> String.sub s 0 idx
    | None -> s

  let make_var id = Var (TxtLoc.none, id)

  let pp_expr_with_loc e = ASTUtils.exp2loc e |> Cache.extract

  let pp_expr =
    let rec pp_with_sep paran rec_paran sep es =
      let str = String.concat sep (List.map (do_pp_expr rec_paran) es) in
      if paran then Printf.sprintf "(%s)" str else str
    and do_pp_expr paran = function
    | Konst (_, (Empty _)) -> "empty"
    | Konst (_, (Universe _)) -> "universe"
    | Var (_, id) -> id
    | Op1 (_, Comp, e) -> Printf.sprintf "~%s" (do_pp_expr true e)
    | Op (_, Inter, es) -> pp_with_sep paran true " & " es
    | Op (_, Union, es) -> pp_with_sep paran true " | " es
    | Op (_, Diff, es) -> pp_with_sep paran true " \\ " es
    | Op (_, Seq, es) -> pp_with_sep paran false "; " es
    | Try (_, e1, e2) ->
      Printf.sprintf "try %s with %s" (do_pp_expr false e1) (do_pp_expr false e2)
    | If (_, _, e1, e2) ->
      Printf.sprintf "if _some_cond then %s else %s" (do_pp_expr false e1) (do_pp_expr false e2)
    | e -> pp_expr_with_loc e in
    do_pp_expr false

  (* Adds special character to distinguish between non-terminals and primitives
    and replaces shadowed variables with their definitions *)
  let convert_defs num_defs crt_nums def_id is_rec expr =
    let rec run = function
    | Var (_, "emptyset") as e -> e
    | Var (_, id) as e ->
      let crt_num = StringMap.safe_find 0 id crt_nums in
      let total_num = StringMap.safe_find 0 id num_defs in
      if is_rec && id = def_id then
        if total_num = crt_num + 1 then e
        else make_var (to_alias id (crt_num + 1))
      else if total_num = crt_num then e
      else make_var (to_alias id crt_num)
    | Konst _ | Tag _ as e -> e
    | Op1 (loc, op, e) -> Op1 (loc, op, run e)
    | Op (loc, op, es) -> Op (loc, op, List.map run es)
    | App (loc, e1, e2) -> App (loc, run e1, run e2)
    | Bind (loc, bds, e) -> Bind (loc, run_bds bds, run e)
    | BindRec (loc, bds, e) -> BindRec (loc, run_bds bds, run e)
    | Fun (loc, pat, e, f, xs) -> Fun (loc, pat, run e, f, xs)
    | ExplicitSet (loc, es) -> ExplicitSet (loc, List.map run es)
    | Match (loc, e, cls, eo) ->
        let cls = List.map (fun (x, e) -> x, run e) cls in
        let eo = Option.map (fun e -> run e) eo in
        Match (loc, run e, cls, eo)
    | MatchSet (loc, e1, e2, cl) ->
      let cl = match cl with
      | EltRem (p1, p2, e) -> EltRem (p1, p2, run e)
      | PreEltPost (p1, p2, p3, e) -> PreEltPost (p1, p2, p3, run e) in
      MatchSet (loc, run e1, run e2, cl)
    | Try (loc, e1, e2) -> Try (loc, run e1, run e2)
    | If (loc, cond, e1, e2) -> If (loc, run_cond cond, run e1, run e2)
    and run_bds bds = List.map (fun (loc, pat, e) -> (loc, pat, run e)) bds
    and run_cond = function
    | Eq (e1, e2) -> Eq (run e1, run e2)
    | Subset (e1, e2) -> Subset (run e1, run e2)
    | In (e1, e2) -> In (run e1, run e2)
    | VariantCond _ as c -> c
    in
    run expr

  let tr_bds defs kont bds =
    List.fold_left (fun defs (_, p, expr) ->
      match p with
      | Pvar (Some id) -> kont defs id expr
      | _ -> defs
    ) defs bds

  let rec tr_ast defs kont parsed_files fname =
    let parsed_files = StringSet.add fname parsed_files in
    let (_, _, ast) = P.parse fname in
    List.fold_left (fun (defs, parsed_files) ins ->
      tr_ins defs kont parsed_files ins
    ) (defs, parsed_files) ast

  and tr_ins defs kont parsed_files = function
  | Let (_, bds) ->
    let defs = tr_bds defs (kont false) bds in
    (defs, parsed_files)
  | Rec (_, bds, _) ->
    let defs = tr_bds defs (kont true) bds in
    (defs, parsed_files)
  | Include (_,fname) when not (StringSet.mem fname parsed_files) ->
      tr_ast defs kont parsed_files fname
  | _ -> (defs, parsed_files)

  let count_defs fname existing_num_defs =
    let num_defs, _ = tr_ast existing_num_defs (fun _ num_defs id _ ->
      let num = StringMap.safe_find 0 id num_defs in
      StringMap.add id (num + 1) num_defs
    ) StringSet.empty fname in
    num_defs

  let get_defs fname ~existing_defs ~num_defs ~crt_nums =
    let (defs, crt_nums), _= tr_ast (existing_defs, crt_nums)
      (fun is_rec (defs, crt_nums) id expr ->
        let converted_def = convert_defs num_defs crt_nums id is_rec expr in
        let num = StringMap.safe_find 0 id crt_nums + 1 in
        let total_num = StringMap.find id num_defs in
        let crt_nums = StringMap.add id num crt_nums in
        let id = if num = total_num then id else to_alias id num in
        let defs = StringMap.add id converted_def defs in
        defs, crt_nums
      ) StringSet.empty fname in
    defs, crt_nums

  let execute fname =
    let stdlib_fname = Filename.concat O.libdir "stdlib.cat" in
    let stdlib_num_defs = count_defs stdlib_fname StringMap.empty in
    let num_defs = count_defs fname stdlib_num_defs in
    let stdlib_defs, crt_nums = get_defs stdlib_fname
      ~existing_defs:StringMap.empty 
      ~num_defs
      ~crt_nums:StringMap.empty in
    let defs, _ = get_defs fname ~existing_defs:stdlib_defs
      ~num_defs ~crt_nums in
    let _ = StringMap.map ASTUtils.flatten defs in
    ()

end

let verbose = ref false
let libdir = ref (Filename.concat Version.libdir "herd")
let includes = ref []
let model = ref (Filename.concat !libdir "aarch64.cat")

let options = [
  ("-version", Arg.Unit
    (fun () -> Printf.printf "%s, Rev: %s\n" Version.version Version.rev;
      exit 0), " show version number and exit");
  ("-libdir", Arg.Unit (fun () -> print_endline !libdir; exit 0),
    " show installation directory and exit");
  ("-set-libdir", Arg.String (fun s -> libdir := s),
    "<path> set installation directory to <path>");
  (ArgUtils.parse_string "-model" model "path to cat model");
  (ArgUtils.parse_bool "-v" verbose "show various diagnostics");
]

let arg_handler s =
  raise (Arg.Bad (Printf.sprintf "Unexpected argument: %s" s))

let () =
  try
    Arg.parse options
      arg_handler
      (Printf.sprintf "Usage %s [options], output all chains of relations \
        between e1 and e2." prog)
  with
  | Misc.Fatal msg -> Printf.eprintf "%s: %s\n" prog msg; exit 2

let () =
  let module Run =
    Make
      (struct
        let verbose = !verbose
        let includes = !includes
        let libdir = !libdir
      end) in
  ignore (Run.execute !model)

(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
(* Authors:                                                                 *)
(* Hadrien Renaud, University College London, UK.                           *)
(****************************************************************************)

module A64B = AArch64Base
module AST = Asllib.AST

(*****************************************************************************)
(*                                                                           *)
(*                               ASL preambule                               *)
(*                                                                           *)
(*****************************************************************************)

type scope = AST.identifier * int

let pp_scope (enclosure, call_nb) = Printf.sprintf "%s.%d" enclosure call_nb

let arch_reg_to_int =
  let rec index_of elt pos li =
    match li with
    | [] -> raise Not_found
    | h :: _ when compare elt h == 0 -> pos
    | _ :: t -> index_of elt (pos + 1) t
  in
  fun r -> index_of r 1 AArch64Base.gprs

(*****************************************************************************)
(*                                                                           *)
(*                             Implem of archBase                            *)
(*                                                                           *)
(*****************************************************************************)

(* Who am I ? *)
let arch = Archs.asl
let base_type = A64B.base_type
let endian = A64B.endian

(***********************************************)
(* Basic arch types and their basic operations *)
(***********************************************)

type reg = ASLLocalId of scope * AST.identifier | ArchReg of A64B.reg

let is_local = function ASLLocalId _ -> true | _ -> false
let to_arch_reg = function ASLLocalId _ -> assert false | ArchReg r -> r
let to_reg r = ArchReg r
let default_scope = ("main", 0)

let parse_local_id =
  let ( let* ) = Option.bind in
  let ( and* ) o1 o2 =
    match (o1, o2) with Some x1, Some x2 -> Some (x1, x2) | _ -> None
  in
  let find_opt n s =
    try Some (Str.matched_group n s) with Not_found -> None
  in
  let regexp =
    Str.regexp {|\([A-Za-z0-9_-]+\)\.\([0-9]+\)\.\([A-Za-z0-9_-]+\)|}
  in
  fun s ->
    if Str.string_match regexp s 0 then
      let* x1 = find_opt 1 s and* x2 = find_opt 2 s and* x3 = find_opt 3 s in
      Some (ASLLocalId ((x1, int_of_string x2), x3))
    else Some (ASLLocalId (default_scope, s))

let parse_reg s =
  match (A64B.parse_reg s, s) with
  | Some r, _ -> Some (ArchReg r)
  | None, "NZCV" -> Some (ArchReg AArch64Base.NZCV)
  | None, _ -> parse_local_id s

(** A list of supported AArch64 registers. *)
let arch_regs = AArch64Base.NZCV :: List.map fst AArch64Base.xregs

let pp_reg = function
  | ASLLocalId (scope, x) -> pp_scope scope ^ "." ^ x
  | ArchReg r -> A64B.pp_reg r

let reg_compare r1 r2 =
  match (r1, r2) with
  | ArchReg r1, ArchReg r2 -> A64B.reg_compare r1 r2
  | ASLLocalId ((c1, n1), x1), ASLLocalId ((c2, n2), x2) ->
      if String.equal x1 x2 then
        if String.equal c1 c2 then Int.compare n1 n2 else String.compare c1 c2
      else String.compare x1 x2
  | ArchReg _, ASLLocalId _ -> 1
  | ASLLocalId _, ArchReg _ -> -1

let symb_reg_name = function
  | ASLLocalId _ -> None
  | ArchReg r -> A64B.symb_reg_name r

let symb_reg x = ArchReg (A64B.symb_reg x)
let type_reg _ = assert false

type barrier = A64B.barrier

let pp_barrier = A64B.pp_barrier
let barrier_compare = A64B.barrier_compare

type parsedInstruction = AST.t
type instruction = AST.t

let pp_instruction _ppmode ast = Asllib.PP.t_to_string ast
let dump_instruction a = pp_instruction PPMode.Ascii a
let dump_instruction_hash = Asllib.Serialize.t_to_string
let allowed_for_symb = List.map to_reg A64B.allowed_for_symb
let fold_regs _ ab _ = ab
let map_regs _ _ i = i
let fold_addrs _ a _ = a
let map_addrs _ i = i
let norm_ins i = i
let get_next _ = assert false

include Pseudo.Make (struct
  type ins = instruction
  type pins = parsedInstruction
  type reg_arg = reg

  let parsed_tr ast = ast
  let get_naccesses _ = -1
  let fold_labels k _f _i = k
  let map_labels _f i = i
end)

let get_macro _ = assert false
let hash_pteval _ = assert false

(*****************************************************************************)
(*                                                                           *)
(*                                 ASL Utils                                 *)
(*                                                                           *)
(*****************************************************************************)

let memoize f =
  let table = Hashtbl.create 17 in
  fun s ->
    match Hashtbl.find_opt table s with
    | Some r -> r
    | None ->
        let r = f s in
        let () = Hashtbl.add table s r in
        r

let do_build_ast_from_file fname chan =
  let lexbuf = Lexing.from_channel chan in
  let () =
    lexbuf.Lexing.lex_curr_p <-
      { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = fname }
  in
  try Asllib.Parser.ast Asllib.Lexer.token lexbuf with
  | Asllib.Parser.Error ->
      Warn.fatal "%s: Cannot parse." (Pos.str_pos lexbuf.Lexing.lex_curr_p)
  | Asllib.Lexer.LexerError ->
      Warn.fatal "%s: unknown token." (Pos.str_pos lexbuf.Lexing.lex_curr_p)

let build_ast_from_file =
  let protected_f s = Misc.input_protect (do_build_ast_from_file s) s in
  memoize protected_f

let asl_generic_parser lexer lexbuf =
  let ast =
    try Asllib.Parser.ast lexer lexbuf with
    | Asllib.Parser.Error -> raise Parsing.Parse_error
    | Asllib.Lexer.LexerError ->
        LexMisc.error "Unknown token in ASL program" lexbuf
  in
  ([ (0, None, MiscParser.Main) ], [ [ Instruction ast ] ], MiscParser.NoExtra)

module Instr = Instr.No (struct
  type instr = instruction
end)

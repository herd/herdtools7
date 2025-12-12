(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2026-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module SP = Splitter.Make (struct
  let debug = false
  let check_rename = fun s -> Some s
end)

module C = struct
  let is_morello = false
end

module AArch64Value = CapabilityValue.Make (C)
module AArch64 = MakeAArch64Base.Make (C)

module AArch64LexParse = struct
  type instruction = AArch64.parsedPseudo
  type token = AArch64Parser.token

  module Lexer = AArch64Lexer.Make (struct
    include C

    let debug = false
  end)

  let lexer = Lexer.token
  let parser = AArch64Parser.main
end

module P0 = GenParser.Make (GenParser.DefaultConfig) (AArch64) (AArch64LexParse)

type instr = {
  instruction : AArch64Base.instruction;
  proc : int;
  static_poi : int;
  label : Label.t option;
}

let equal_proc_poi x y =
  Misc.pair_eq Int.equal Int.equal (x.proc, x.static_poi) (y.proc, y.static_poi)

let compare_proc_poi x y =
  Misc.pair_compare Int.compare Int.compare (x.proc, x.static_poi)
    (y.proc, y.static_poi)

let make_instr ?label ~proc ~static_poi i =
  { instruction = i; label; proc; static_poi }

let collect_instructions_ ~proc (pseudos : AArch64Base.pseudo list) : instr list
    =
  let rec go current_label spoi acc = function
    | [] -> List.rev acc
    | AArch64Base.Nop :: rest -> go current_label spoi acc rest
    | Label (lbl, pseudo) :: rest -> go (Some lbl) spoi acc (pseudo :: rest)
    | Instruction raw :: rest ->
        let instr =
          make_instr ?label:current_label ~static_poi:spoi ~proc raw
        in
        let acc = instr :: acc in
        go None (spoi + 1) acc rest
    | Macro _ :: rest -> go None spoi acc rest
    | Symbolic _ :: rest -> go None spoi acc rest
    | Pagealign :: rest -> go None spoi acc rest
    | Skip _ :: rest -> go None spoi acc rest
  in
  go None 0 [] pseudos

let collect_instructions parsed =
  let blocks = parsed.MiscParser.prog in
  blocks
  |> Misc.List.concat_map (fun ((proc, _, _), instrs) ->
      collect_instructions_ ~proc instrs)

let from_string contents =
  let (splitted : Splitter.result) = SP.split_string "" contents in
  match splitted.arch with
  | `AArch64 -> P0.parse_string contents splitted
  | arch ->
      let arch_str = Archs.pp arch in
      let msg = Printf.sprintf "litmus test with architecture `%s`" arch_str in
      invalid_arg msg

let render_instr ~latex (ins : AArch64Base.instruction) =
  if latex then
    AArch64.do_pp_instruction
      { AArch64.m_int with pp_k = Format.sprintf "\\#%i" }
      ins
  else AArch64.dump_instruction ins

let render_instruction ~latex instr = render_instr ~latex instr.instruction

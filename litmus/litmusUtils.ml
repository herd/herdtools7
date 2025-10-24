(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2017-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)


(* Some utilities *)

module Hash(O:Warn.Config) =
  struct
    open Answer
    module W = Warn.Make(O)

    let mk_hash_info fname info =
      let hash = MiscParser.get_info_on_info MiscParser.hash_key info in
      match hash with
      | Some hash -> { filename=fname; hash=hash;}
      | None -> assert false

    let hash_ok env tname hash =
      try
        let ohash = StringMap.find tname env in
        if String.compare hash.hash ohash.hash <> 0 then begin
          Warn.user_error "Unconsistent hashes for test %s, previous file %s"
            tname ohash.filename
        end else begin
          if  ohash.filename <> hash.filename then
            W.warn  "Duplicate occurrence of test %s (%s,%s)"
              tname ohash.filename hash.filename
          else
            W.warn "File %s is referenced more then once"
              ohash.filename
        end ;
        false
      with Not_found ->  true

  end

module Pseudo(A:Arch_litmus.S) = struct

  type ins = A.instruction
  type code = MiscParser.proc * A.pseudo list

  let rec fmt_io io = match io with
  | A.Nop -> ""
  | A.Instruction ins -> A.dump_instruction ins
  | A.Label (lbl,io) -> lbl ^ ": " ^ fmt_io io
  | A.Symbolic _ -> assert false (*no symbolic in litmus *)
  | A.Macro (f,regs) ->
      Printf.sprintf
        "%s(%s)"
        f
        (String.concat "," (List.map A.pp_reg regs))

  let dump_prog _ (p,is) = MiscParser.pp_proc  p::List.map fmt_io is

  let dump_prog_lines prog =
    let pp = List.map (dump_prog true) prog in
    let pp = Misc.lines_of_prog pp in
    List.map (Printf.sprintf "%s;") pp

  let print_prog chan prog =
    let pp = List.map (dump_prog true) prog in
    Misc.pp_prog chan pp

  let code_exists p (_,c) = A.code_exists p c


(* Extract "exported" labels from code.
 * Those are defined as the argument of instruction
 * that store a label into a register, as does
 * ARM "ADR" instruction.
 *)

  module AU = ArchUtils.Make(A)

  let exported_labels_code prog = AU.get_exported_labels_code prog

  let from_labels lbls prog = A.from_labels lbls prog

  let all_labels = A.all_labels
end

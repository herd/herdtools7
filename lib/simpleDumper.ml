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
(** Parsable dump *)

module type I = sig
  module A : ArchDump.S

  type v
  val dump_v : v -> string

  type state
  val dump_state : state -> string list

  type prop
  val dump_prop : prop -> string
  val dump_constr : prop ConstrGen.constr -> string

  type location
  val dump_location : location -> string

  type fault_type
  val dump_fault_type : fault_type -> string
end


module Make(O:sig val compat : bool end)(I:I) :
CoreDumper.S with
  type test =
         (I.state, (MiscParser.proc * I.A.pseudo list) list, I.prop, I.location,
          I.v, I.fault_type)
           MiscParser.result
= struct

  open Printf
  open I

  let dump_instruction =
    if O.compat then A.dump_instruction_hash
    else A.dump_instruction

  let rec fmt_io io = match io with
  | A.Nop -> ""
  | A.Instruction ins -> dump_instruction ins
  | A.Label (lbl,io) -> lbl ^ ": " ^ fmt_io io
  | A.Symbolic s -> "codevar:"^s
  | A.Macro (f,regs) ->
      sprintf
        "%s(%s)"
        f
        (String.concat "," (List.map A.pp_reg regs))
  | A.Pagealign -> ".pagealign"
  | A.Skip _ -> assert false

  let fmt_col (p,is) = MiscParser.pp_proc p::List.map fmt_io is

  module Dump =
    CoreDumper.Make
      (struct

        let arch = I.A.arch

        type prog = (MiscParser.proc * I.A.pseudo list) list

        let print_prog chan prog =
          let pp = List.map fmt_col prog in
          fprintf chan "%s" (Misc.string_of_prog pp)

        let dump_prog_lines prog =
          let pp = List.map fmt_col prog in
          let pp = Misc.lines_of_prog pp in
          let pp = List.map (sprintf "%s;") pp in
          pp

        type v = I.v
        let dump_v = I.dump_v

        type state = I.state
        let dump_state = I.dump_state

        type prop = I.prop
        let dump_prop = I.dump_prop
        let dump_constr = I.dump_constr

        type location = I.location
        let dump_location = I.dump_location

        type fault_type = I.fault_type
        let dump_fault_type = I.dump_fault_type
      end)

  include Dump
end

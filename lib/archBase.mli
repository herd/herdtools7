(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Basic arch definitions made abstract *)

module type S = sig

  (* Who am I ? *)
  val arch : Archs.t


  (***********************************************)
  (* Basic arch types and their basic operations *)
  (***********************************************)

  type reg
(*  val pc : reg (* Program counter *) *)

  val parse_reg : string -> reg option
  val pp_reg : reg -> string
  val reg_compare : reg -> reg -> int
  val symb_reg_name : reg -> string option
  val symb_reg : string -> reg
    (* Use if you want to implicitly type a final register *)
  val typeof : reg -> CType.t
(*
  type reservation
  val pp_res : reservation -> string
  val res_compare : reservation -> reservation -> int
*)

  type barrier
(*  val all_kinds_of_barriers : barrier list *)
  val pp_barrier            : barrier -> string
  val barrier_compare : barrier -> barrier -> int


  type parsedInstruction
  type instruction

  val pp_instruction : PPMode.t -> instruction -> string
  (* Shorthand for parsable dump *)
  val dump_instruction : instruction -> string

  (*************************************)
  (* All this needed for symbolic regs *)
  (*************************************)

  (* Registers that can be allocated to symbolic registers *)
  val allowed_for_symb : reg list

  (* Apply fonctions to all registers  either real or symbolic *)
  val fold_regs :
        (reg -> 'a -> 'a) * (string -> 'b -> 'b) ->
          'a * 'b -> instruction -> 'a * 'b

  (* Map over all registers *)
    val map_regs :
        (reg -> reg) -> (string -> reg) -> instruction -> instruction

  (* Apply function to addresses present in  code *)
  val fold_addrs : (ParsedConstant.v -> 'a -> 'a) -> 'a -> instruction -> 'a

  (* Map over addresses *)
  val map_addrs :
      (ParsedConstant.v -> ParsedConstant.v) -> instruction -> instruction

  (* Normalize instruction (for hashes) *)
  val norm_ins : instruction -> instruction

(* LM: Was not used, delete
  (* Recognise store, data read from register r *)
  val is_data : reg -> instruction -> bool
*)

  (* Instruction continuation *)
  val get_next : instruction -> Label.next list


  include Pseudo.S
   with type ins = instruction
   and type pins = parsedInstruction
   and type reg_arg = reg

  val get_macro :
      string ->
        (reg list -> pseudo list -> pseudo list)

end

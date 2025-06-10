(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris, France.                                       *)
(*                                                                          *)
(* Copyright 2020-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Abstraction of page table entry (PTE) *)

module Attrs : sig
  type t
  val default : t
  val compare : t -> t -> int
  val eq : t -> t -> bool
  val pp : t -> string
  val as_list : t -> string list
  val of_list : string list -> t
  val mem : string -> t -> bool
end

type t = {
  oa : OutputAddress.t;
  valid : int;
  af : int;
  db : int;
  dbm : int;
  el0 : int;
  attrs : Attrs.t;
  }

(* Identity translations *)
val fromExtra : t -> t
val toExtra : t -> t

(* Basic *)
val compare : t -> t -> int
val eq : t -> t -> bool

(* Accessors, Setters *)
val is_af : t -> bool

val same_oa : t -> t -> bool
val writable : bool -> bool -> t -> bool
val get_attrs : t -> string list

(* Default value *)
val prot_default : t (* Fields only *)
val default : string -> t (* Physical address + default fields *)
val of_pte : string -> t (* Default value for pte page table entry *)

(* Flags have default values *)
val is_default : t -> bool

(* Finish parsing *)
val tr : ParsedPteVal.t -> t
val pp_norm : ParsedPteVal.t -> string


(* Pretty print pp [hexa]  *)
val pp : bool -> t -> string  (* Default field not printed *)
val pp_v : t -> string  (* Decimal *)
val pp_hash : t -> string (* Backward compatibility for test hashes *)

(* Litmus *)
val fields : string list
val default_fields : string list
val norm : string StringMap.t -> string StringMap.t
val dump_pack : (string -> string) -> t -> string
val as_physical : t -> string option
val as_flags : t -> string option

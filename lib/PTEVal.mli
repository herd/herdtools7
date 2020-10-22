(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris, France.                                       *)
(*                                                                          *)
(* Copyright 2020-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
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
end

type t = {
  oa : string;
  valid : int;
  af : int;
  db : int;
  dbm : int;
  el0 : int;
  attrs : Attrs.t;
  }

(* Default value *)
val prot_default : t (* Fields only *)
val default : string -> t (* Physical address + default fields *)

(* Value for pte argument *)
val of_pte : string -> t

(* Set oa  field *)
val set_oa : t -> string -> t

(* Flags have default values *)
val is_default : t -> bool

type pte_prop =
| KV of (string * string)
| Attrs of string list

(* Create fresh pteval *)
(* With physical adress *)
val of_list : string -> pte_prop list -> t
(* Without physcal adress *)
val of_list0 : pte_prop list -> t

(* Pretty print *)
val pp : t -> string  (* Default field not printed *)
val pp_hash : t -> string (* Backward compatibility for test hashes *)

val compare : t -> t -> int
val eq : t -> t -> bool

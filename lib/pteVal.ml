(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris, France.                                       *)
(*                                                                          *)
(* Copyright 2021-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Abstract page table entry values *)
module type S = sig
  type t

  (* Default pte for virtual addresses and pte themselves  *)
  val default : string -> t
  val of_pte : string -> t
  val is_default : t -> bool

  val pp : bool -> t -> string
  val pp_v : t -> string
  val pp_hash : t -> string
  val tr : ParsedPteVal.t -> t

  val eq : t -> t -> bool
  val compare : t -> t -> int

  (* Access flag exported, accessed in mem.ml  *)
  val is_af : t -> bool

  val same_oa : t -> t -> bool

  (* Value specifies a writable page *)
  val writable : bool -> bool -> t -> bool

  (* Attributes *)
  val get_attrs : t -> string list

  (* Litmus *)
  val fields : string list
  val default_fields : string list
  val dump_pack : (string -> string) -> t -> string
  val as_physical : t -> string option
  val as_flags : t -> string option
end

module No= struct
    type t

    let default _ = assert false
    let of_pte _ = assert false
    let is_default _ = assert false
    let pp _ _ = assert false
    let pp_v _ = assert false
    let pp_hash _ = assert false
    let tr _ = assert false

    let eq _ _ = assert false
    let compare _ _ = assert false

    let is_af _ = assert false

    let same_oa _ _ = assert false
    let writable _ _ _ = assert false
    let get_attrs _ = assert false

    let fields = []
    let default_fields = []
    let dump_pack _ _ = assert false
    let as_physical _ = None
    let as_flags _ = None
end

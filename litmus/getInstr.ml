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

module type S = sig
  type t

  val self_instrs : t list (* Mandatory for `-variant self` mode *)

  val instr_name : t -> string
  val fun_name : t -> string
  val dump_instr :
    (('a,'b,t) Constant.t -> string) -> ('a,'b,t) Constant.t -> string

  module Make : functor (O:Indent.S) -> sig val dump : t -> unit end
end

module No (I:sig type instr end) = struct

  type t = I.instr

  let self_instrs = []

  let instr_name _ = ""
  let fun_name _ = ""
  let dump_instr dump v = dump v

  module Make (O:Indent.S) = struct let dump _ = ()  end
end

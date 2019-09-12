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

module Make(O:sig val memory : Memory.t val hexa : bool end) = struct
  module V = Int32Constant

  type reg = string
  type instruction = unit

  module RegSet = StringSet
  module RegMap = StringMap

  let vToName =
    let open Constant in
    function
      | Concrete i -> "addr_" ^ V.Scalar.pp O.hexa i
      | Symbolic ((s,None),_) -> s
      | Label _|Symbolic _|Tag _ -> assert false

  module Internal = struct
    type arch_reg = reg
    let pp_reg x = x
    let reg_compare = String.compare

    type arch_global = string
    let pp_global x = x
    let global_compare = String.compare

    let arch = `C
  end

  include Location.Make(Internal)

  let parse_reg x = Some x
  let reg_compare = Internal.reg_compare

  type state = (location * V.v) list
  type fullstate = (location * (MiscParser.run_type * V.v)) list

  module Out = struct
    module V = V
    include CTarget
    include OutUtils.Make(O)(V)

    let dump_init_val = dump_v
  end

  let arch = Internal.arch

  let rec find_in_state loc = function
    | [] -> V.zero
    | (loc2,v)::rem ->
        if location_compare loc loc2 = 0 then v
        else find_in_state loc rem

  let pp_reg x = x

  let rec count_procs = function
    | CAst.Test _::xs -> 1 + count_procs xs
    | CAst.Global _::xs -> count_procs xs
    | [] -> 0

  let base_type = CType.Base "int"
  let typeof c = assert false
end

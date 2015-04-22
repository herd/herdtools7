(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

module type I = sig
  module V :
      sig
        type v = Constant.v
        include Constant.S
        val maybevToV : v -> v
      end

  type arch_reg
  val arch : Archs.t
  val forbidden_regs : arch_reg list
  val pp_reg : arch_reg -> string
  val reg_compare : arch_reg -> arch_reg -> int
  val reg_to_string  : arch_reg -> string
  val internal_init : arch_reg -> (string * string) option
  val reg_class : arch_reg -> string
  val comment : string
end

module type S = sig

  module I : I

  val comment : string (* ASM comment to use *)

  val vToName : Constant.v -> string

  module RegSet : MySet.S with type elt = I.arch_reg
  include Location.S
  with type loc_reg = I.arch_reg and type loc_global = string

  module Out : Template.S
  with type arch_reg = I.arch_reg
  module MapValue : MyMap.S with type key = Constant.v

(* A bit of state handling *)
  type state = (location * Constant.v) list
  type fullstate = (location * (MiscParser.run_type * Constant.v)) list

  val find_in_state : location -> state -> Constant.v

end

module type Config = sig
  include Template.Config
  val asmcomment : string option
end

module Make(O:Config)(I:I) : S with module I = I
= struct

  let comment = match O.asmcomment with
  | Some c -> c
  | None -> I.comment

  module I = I
  open Constant

  let vToName v = match v with
  | Concrete i -> "addr_" ^ string_of_int i
  | Symbolic s -> s

  module RegSet =
    MySet.Make
      (struct
        type t = I.arch_reg
        let compare = I.reg_compare
      end)

  include Location.Make
      (struct
        include I

        type arch_global = string
        let pp_global s = s
        let global_compare = String.compare
      end)

  module Out =
    Template.Make
      (O)
      (struct
        type arch_reg = I.arch_reg
        let arch = I.arch
        let reg_compare = I.reg_compare
        let reg_to_string = I.reg_to_string
        let comment = comment
      end)
      (I.V)

  module MapValue =
    MyMap.Make
      (struct
        type t = Constant.v
        let compare = I.V.compare
      end)

(* A bit of state handling *)
  type state = (location * Constant.v) list
  type fullstate = (location * (MiscParser.run_type * Constant.v)) list

  let rec find_in_state loc = function
    | [] -> I.V.intToV 0
    | (loc2,v)::rem ->
        if location_compare loc loc2 = 0 then v
        else find_in_state loc rem

end

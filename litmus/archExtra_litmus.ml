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

module type I = sig
  module V : Constant.S

  type arch_reg
  val arch : Archs.t
  val forbidden_regs : arch_reg list
  val pp_reg : arch_reg -> string
  val reg_compare : arch_reg -> arch_reg -> int
  val reg_to_string  : arch_reg -> string
  val internal_init : arch_reg -> (string * string) option
  val reg_class : arch_reg -> string
  val reg_class_stable : arch_reg -> string
  val comment : string
  val error : CType.t -> CType.t -> bool
  val warn : CType.t -> CType.t -> bool
end

module type S = sig

  module I : I

  val comment : string (* ASM comment to use *)

  module RegSet : MySet.S with type elt = I.arch_reg
  module RegMap : MyMap.S with type key = I.arch_reg

  include Location.S
  with type loc_reg = I.arch_reg and type loc_global = Global_litmus.t
  val location_of_addr : string -> location
  val tr_global : MiscParser.maybev -> Global_litmus.t

  module Out : Template.S with
  module V = I.V and
  type arch_reg = I.arch_reg
  and module RegSet = RegSet
  and module RegMap = RegMap

(* Normalised tag for symbols *)
  val dump_loc_tag : location -> string

  module MapValue : MyMap.S with type key = I.V.v

(* A bit of state handling *)
  type state = (location * I.V.v) list

  val debug_state : state -> string

  type fullstate = (location * (MiscParser.run_type * I.V.v)) list

  val find_in_state : location -> state -> I.V.v

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

  module RegSet =
    MySet.Make
      (struct
        type t = I.arch_reg
        let compare = I.reg_compare
      end)

  module RegMap =
    MyMap.Make
      (struct
        type t = I.arch_reg
        let compare = I.reg_compare
      end)

  include Location.Make
      (struct
        include I
        module G = Global_litmus
        type arch_global = G.t
        let pp_global = G.pp
        let global_compare = G.compare
      end)

  let location_of_addr a = Location_global (Global_litmus.Addr a)

  let tr_global (c:ParsedConstant.v) = 
    let open Constant in
    match c with
    | Symbolic sym -> Global_litmus.tr_symbol sym
    | Tag _|Concrete _|Label _|PteVal _ ->
        Warn.fatal "Constant %s cannot be translated to a litmus adress"
          (ParsedConstant.pp O.hexa c)

  module Out =
    Template.Make
      (O)
      (struct
        module V = I.V
        type arch_reg = I.arch_reg
        let arch = I.arch
        let reg_compare = I.reg_compare
        let reg_to_string = I.reg_to_string
        let comment = comment
        module RegSet = RegSet
        module RegMap = RegMap
      end)

  let dump_loc_tag loc =
    let module G = Global_litmus in
    match loc with
    | Location_reg (proc,reg) -> Out.dump_out_reg proc reg
    | Location_global (G.Addr s) -> s
    | Location_deref (G.Addr s,i) -> Printf.sprintf "%s_%i" s i
    | Location_global (G.Pte _|G.Phy _)
    | Location_deref ((G.Pte _|G.Phy _),_)
      -> assert false

  module MapValue =
    MyMap.Make
      (struct
        type t = I.V.v
        let compare = I.V.compare
      end)

(* A bit of state handling *)
  type state = (location * I.V.v) list

  let debug_state st =
    String.concat " "
      (List.map
         (fun (loc,v) -> Printf.sprintf "<%s -> %s>" (pp_location loc) (I.V.pp_v v))
         st)

  type fullstate = (location * (MiscParser.run_type * I.V.v)) list

  let rec find_in_state loc = function
    | [] -> I.V.zero
    | (loc2,v)::rem ->
        if location_compare loc loc2 = 0 then v
        else find_in_state loc rem

end

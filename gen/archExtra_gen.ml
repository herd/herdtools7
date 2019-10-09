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

open Printf

module type I = sig
 type arch_reg

  val is_symbolic : arch_reg -> bool
  val pp_reg : arch_reg -> string
  val free_registers : arch_reg list
end

module type S = sig
  type arch_reg

(* Locations *)
  type location =
    | Reg of Code.proc * arch_reg
    | Loc of string

  val of_loc : Code.loc -> location
  val of_reg : Code.proc -> arch_reg -> location

  val pp_location : location -> string
  val location_compare : location -> location -> int

  module LocMap : MyMap.S with type key = location
(* Initial states *)
  type init = (location * string option) list

(***********************)
(* Register allocation *)
(***********************)

  type st
  val st0 : st

  val alloc_reg : st -> arch_reg * st
  val alloc_trashed_reg : string -> st -> arch_reg * st

  val current_label : st -> int
  val next_label : st -> int
  
  val next_label_st : st -> st

end

module Make(I:I) : S with type arch_reg = I.arch_reg
= struct
  type arch_reg = I.arch_reg

  type location =
    | Reg of int * arch_reg
    | Loc of string

  let pp_location = function
    | Reg (i,r) ->
        if I.is_symbolic r then I.pp_reg r
        else sprintf "%i:%s" i (I.pp_reg r)
    | Loc loc -> loc

  let location_compare loc1 loc2 = match loc1,loc2 with
  | Reg _,Loc _ -> -1
  | Loc _,Reg _ -> 1
  | Reg (p1,r1),Reg (p2,r2) ->
      begin match Misc.int_compare p1 p2 with
      | 0 -> compare r1 r2
      | r -> r
      end
  | Loc loc1,Loc loc2 -> compare loc1 loc2

  module LocMap =
    MyMap.Make
      (struct
        type t = location
        let compare = location_compare
      end)

  let of_loc loc = Loc (Code.as_data loc)
  let of_reg p r = Reg (p,r)

  type init = (location * string option) list

  type st = arch_reg list * arch_reg StringMap.t * int

  let st0 = I.free_registers,StringMap.empty,0

  let alloc_reg = function
    | [],_,_ -> Warn.fatal "No more registers"
    | r::rs,m,i -> r,(rs,m,i)

  let alloc_trashed_reg k (_,m,_ as st) =
    try StringMap.find k m,st
    with Not_found ->
      let r,(rs,m,i) = alloc_reg st in
      r,(rs,StringMap.add k r m,i) 

  let current_label (_,_,i) = i
  let next_label (_,_,i) = i+1  
  let next_label_st (r,rs,i) = (r,rs,i+1)
end

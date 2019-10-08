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
  type arch_reg
  val pp_reg : arch_reg -> string
  val reg_compare : arch_reg -> arch_reg -> int

  type arch_global
  val pp_global : arch_global -> string
  val global_compare : arch_global -> arch_global -> int
end

module type S = sig

 type loc_reg
 type loc_global

 type location =
    | Location_global of loc_global
    | Location_deref of loc_global * int
    | Location_reg of int*loc_reg

  val pp_location : location -> string
  val pp_rval : location -> string
  val location_compare : location -> location -> int
  val of_proc : int -> location -> loc_reg option
  val is_global : location -> bool
  val global : location -> loc_global option

  module LocSet : MySet.S with type elt = location
  module LocMap : MyMap.S with type key = location
end

module Make(A:I) : S
with type loc_reg = A.arch_reg and type loc_global = A.arch_global =
  struct

    type loc_reg = A.arch_reg
    type loc_global = A.arch_global

    type location =
      | Location_global of loc_global
      | Location_deref of loc_global * int
      | Location_reg of int*loc_reg

    let of_proc p = function
      | Location_reg (q,r) -> if p=q then Some r else None
      | Location_global _|Location_deref _ -> None

    let is_global = function
      | Location_global _ -> true
      | Location_reg _ |Location_deref _ -> false

    let global = function
      | Location_global s  -> Some s
      | Location_reg _|Location_deref _ -> None

    let pp_location l = match l with
    | Location_reg (proc,r) -> string_of_int proc ^ ":" ^ A.pp_reg r
    | Location_global a -> A.pp_global a
    | Location_deref (a,i) -> Printf.sprintf "%s[%i]" (A.pp_global a) i

    let pp_rval l = match l with
    | Location_reg (proc,r) -> string_of_int proc ^ ":" ^ A.pp_reg r
    | Location_global a -> Printf.sprintf "*%s" (A.pp_global a)
    | Location_deref (a,i) -> Printf.sprintf "%s[%i]" (A.pp_global a) i

(*
  The following compare  comes from ancient code
  that used that order to pretty print states.
  I guess I can use it for ordering keys in maps
 *)
    let location_compare l1 l2 = match l1,l2 with
    | Location_reg (p1,r1), Location_reg (p2,r2) ->
        begin match Misc.int_compare p1 p2 with
        | 0 -> A.reg_compare r1 r2
        | r -> r
        end
    | Location_global a1, Location_global a2 -> A.global_compare a1 a2 
    | Location_deref (a1,i1), Location_deref (a2,i2) ->
        begin match A.global_compare a1 a2  with
        | 0 -> Misc.int_compare i1 i2
        | r -> r
        end
    | Location_reg _, (Location_global _|Location_deref _) -> -1
    | (Location_global _|Location_deref _), Location_reg _ -> 1
    | Location_global _,Location_deref _ -> -1
    | Location_deref _,Location_global _ -> 1

    module OL = struct
      type t = location
      let compare = location_compare
    end

    module LocSet = MySet.Make(OL)
    module LocMap = MyMap.Make(OL)
  end

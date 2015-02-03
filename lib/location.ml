(*********************************************************************)
(*                      Litmus/DIY                                   *)
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
    | Location_reg of int*loc_reg

  val pp_location : location -> string
  val pp_rval : location -> string
  val location_compare : location -> location -> int
  val of_proc : int -> location -> loc_reg option
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
      | Location_reg of int*loc_reg

    let of_proc p = function
      |  Location_reg (q,r) when p = q -> Some r
      | _ -> None
    let global = function
      | Location_global s -> Some s
      | Location_reg _ -> None

    let pp_location l = match l with
    | Location_reg (proc,r) -> string_of_int proc ^ ":" ^ A.pp_reg r
    | Location_global a -> A.pp_global a

    let pp_rval l = match l with
    | Location_reg (proc,r) -> string_of_int proc ^ ":" ^ A.pp_reg r
    | Location_global a -> Printf.sprintf "*%s" (A.pp_global a)

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
    | Location_reg _, Location_global _ -> -1
    | Location_global _, Location_reg _ -> 1
    | Location_global a1, Location_global a2 -> A.global_compare a1 a2 

    module OL = struct
      type t = location
      let compare = location_compare
    end

    module LocSet = MySet.Make(OL)
    module LocMap = MyMap.Make(OL)
  end

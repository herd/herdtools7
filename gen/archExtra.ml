(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

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
    | Loc of Code.loc

  val of_loc : Code.loc -> location
  val of_reg : Code.proc -> arch_reg -> location

  val pp_location : location -> string
  val location_compare : location -> location -> int

(* Initial states *)
  type init = (location * string) list

(***********************)
(* Register allocation *)
(***********************)

  type st
  val st0 : st

  val alloc_reg : st -> arch_reg * st

end

module Make(I:I) : S with type arch_reg = I.arch_reg
= struct
  type arch_reg = I.arch_reg

  type location =
    | Reg of int * arch_reg
    | Loc of string

  let location_compare loc1 loc2 = match loc1,loc2 with
  | Reg _,Loc _ -> -1
  | Loc _,Reg _ -> 1
  | Reg (p1,r1),Reg (p2,r2) ->
      begin match Misc.int_compare p1 p2 with
      | 0 -> Pervasives.compare r1 r2
      | r -> r
      end
  | Loc loc1,Loc loc2 -> String.compare loc1 loc2

  let pp_location = function
    | Reg (i,r) ->
        if I.is_symbolic r then I.pp_reg r
        else sprintf "%i:%s" i (I.pp_reg r)
    | Loc loc -> loc

  let of_loc loc = Loc loc
  let of_reg p r = Reg (p,r)

  type init = (location * string) list

  type st = arch_reg list

  let st0 = I.free_registers

  let alloc_reg = function
    | [] -> Warn.fatal "No more registers"
    | r::rs -> r,rs
end

(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2024-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Extend a scalar module by wide scalars *)

module
  Make
    (Narrow:Scalar.S)
    (Wide:WideScalar.S)
    (Translate:
       sig
         val promote : Narrow.t -> Wide.t
         val demote : Wide.t -> Narrow.t
       end) : Scalar.S =
  struct
    type t = Narrow of Narrow.t | Wide of Wide.t

    let choose fn fw = function
      | Narrow i -> fn i
      | Wide i -> fw i

    let machsize = Narrow.machsize

    let zero = Narrow Narrow.zero
    let one = Narrow Narrow.one

    let unique_zero = false (* Wide also have a zero *)
    let is_zero = choose Narrow.is_zero Wide.is_zero


    (*****************)
    (* One arguments *)
    (*****************)

    let to_narrow f i = Narrow (f i)

    let map fn fw = function
      | Narrow i -> Narrow (fn i)
      | Wide i -> Wide (fw i)

    let of_string = to_narrow Narrow.of_string
    let pp hexa = choose (Narrow.pp hexa)  Wide.pp
    let pp_unsigned hexa = choose (Narrow.pp_unsigned hexa)  Wide.pp

    let of_int = to_narrow Narrow.of_int
    and to_int = choose Narrow.to_int Wide.to_int

    let of_int64 = to_narrow Narrow.of_int64
    and to_int64 = choose Narrow.to_int64 Wide.to_int64

    let printable = map Narrow.printable Misc.identity

    let compare s1 s2 = match s1,s2 with
      | Narrow i1,Narrow i2 -> Narrow.compare i1 i2
      | Wide i1,Wide i2 -> Wide.compare i1 i2
      | Narrow _,Wide _ -> -1
      | Wide _,Narrow _ -> +1

    let unsigned_compare s1 s2 = match s1,s2 with
      | Narrow i1,Narrow i2 -> Narrow.unsigned_compare i1 i2
      | Wide i1,Wide i2 -> Wide.compare i1 i2
      | Narrow _,Wide _ -> -1
      | Wide _,Narrow _ -> +1

    let equal s1 s2 = match s1,s2 with
      | Narrow i1,Narrow i2 -> Narrow.equal i1 i2
      | Wide i1,Wide i2 -> Wide.equal i1 i2
      | (Narrow _,Wide _)
      | (Wide _,Narrow _)
        -> false

    (*****************)
    (* Two arguments *)
    (*****************)

    (* Notice: implicit promotion to wider scalar, if needed *)

    let map2 fn fw s1 s2 = match s1,s2 with
      | Narrow i1,Narrow i2 -> Narrow (fn i1 i2)
      | Wide i1,Wide i2 -> Wide (fw i1 i2)
      | Narrow i1,Wide i2 -> Wide (fw (Translate.promote i1) i2)
      | Wide i1,Narrow i2 -> Wide (fw i1 (Translate.promote i2))

(* Implicit demotion and promotion for operations not
 * implemented by the wider scalars.
 * IN practice, this will apply to arithmetics.
 *)

    let widePromote i = Wide (Translate.promote i)

    let map2DemotePromote fn s1 s2 = match s1,s2 with
      | Narrow i1,Narrow i2 -> Narrow (fn i1 i2)
      | Wide i1,Narrow i2 ->
         fn (Translate.demote i1) i2 |> widePromote
      | Narrow i1,Wide i2 ->
         fn i1 (Translate.demote i2) |> widePromote
      | Wide i1,Wide i2 ->
         fn (Translate.demote i1) (Translate.demote i2)
         |> widePromote


    let choose2 fn fw s1 s2 = match s1,s2 with
      | Narrow i1,Narrow i2 ->  fn  i1 i2
      | Wide i1,Wide i2 -> fw i1 i2
      | Narrow i1,Wide i2 -> fw (Translate.promote i1) i2
      | Wide i1,Narrow i2 -> fw i1 (Translate.promote i2)

    let no_tag = Warn.fatal "operation %s non-existent for wide integers"
    let no_op2 tag _ _ = no_tag tag
    let no_op tag _ = no_tag tag

    let add = map2DemotePromote Narrow.add
    and sub = map2DemotePromote Narrow.sub
    and mul = map2DemotePromote Narrow.mul
    and div = map2DemotePromote Narrow.div
    and rem = map2DemotePromote Narrow.rem
    and logor = map2 Narrow.logor Wide.logor
    and logand = map2 Narrow.logand Wide.logand
    and logxor = map2 Narrow.logxor Wide.logxor
    and lognot = map Narrow.lognot Wide.lognot
    and abs = map Narrow.abs Misc.identity


    let mapIntArg fn fw s1 k =
      map (fun i -> fn i k) (fun i -> fw i k) s1

    let shift_left =
      mapIntArg Narrow.shift_left Wide.shift_left
    and shift_right_logical =
      mapIntArg Narrow.shift_right_logical Wide.shift_right
    and shift_right_arithmetic =
      mapIntArg
        Narrow.shift_right_arithmetic
        (no_op2 "shift_right_arithmetic")

    let bit_at k = map (Narrow.bit_at k) (Wide.bit_at k)
    let addk s k = match s with
      | Narrow i -> Narrow (Narrow.addk i k)
      | Wide i ->
         Narrow.addk (Translate.demote i) k
         |> widePromote

    let lt = choose2 Narrow.lt Wide.lt
    and le = choose2 Narrow.le Wide.le

    let mask sz = map (Narrow.mask sz) (Wide.mask sz)
    and sxt sz = map (Narrow.sxt sz) (fun _ -> no_tag "sxt")

    let get_tag = choose Narrow.get_tag (no_op "get_tag")
    and set_tag tag = map (Narrow.set_tag tag) (no_op "set_tag")

    (****************)
    (* Translations *)
    (****************)

    let promote = function
      | Narrow x -> Wide (Translate.promote x)
      | Wide _ as x -> x

    and demote = function
      | Narrow _ as x -> x
      | Wide x -> Narrow (Translate.demote x)

  end

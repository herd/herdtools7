(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Signed 128-bit integer types in pure OCaml. *)

module Int128 = struct
  include BaseUint128

  let max_int = Int64.max_int,BaseUint64.max_int

  let chsgn a = add (lognot a) one

  let minus_one = chsgn one

  let is_neg (ah,_) = BaseUint64.has_top_bit_set ah

  let abs x = if is_neg x then chsgn x else x

  let compare a b =
    match is_neg a,is_neg b with
    | true,true -> compare b a
    | false,false -> compare a b
    | true,false -> -1
    | false,true -> +1

  let unsigned_compare a b =
    match Int64.unsigned_compare (fst a) (fst b) with
    | 0 -> Int64.unsigned_compare (snd a) (snd b)
    | n -> n
  
  let div_and_rem a b =
    if is_neg b then raise (Failure "division by negative number")
    else if is_neg a then
(* Signed euclidian division, with remainder in [0..b) *)
      let a = chsgn a in
      let q,r = div_and_rem a b in
      if equal r zero then chsgn q,r
      else chsgn (succ q),sub b r
    else div_and_rem a b

  let div a b =
    let q,_ = div_and_rem a b in q

  let rem a b =
    let _,r = div_and_rem a b in r

  let to_string a =
    if is_neg a then "-" ^ to_string (chsgn a)
    else to_string a

  let of_string raw =
    let len = String.length raw in
    if len > 0 && raw.[0] = '-' then
      chsgn (of_string (String.sub raw 1 (len-1)))
    else of_string raw

end

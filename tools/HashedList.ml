(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
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

module Make(I:sig type elt end)  =
  struct
    type elt = I.elt
    type elt_hashed = elt Hashcons.hash_consed

    type t = t_node Hashcons.hash_consed
    and t_node =
      | Nil
      | Cons of elt_hashed * t

    module M = Hashcons.Make
        (struct
          type t = t_node
          let equal l1 l2 = match l1,l2 with
          | Nil,Nil -> true
          | (_,Nil)|(Nil,_) -> false
          | Cons (x1,r1),Cons (x2,r2) ->
              x1 == x2 && r1 == r2
          let hash = function
            | Nil -> 0
            | Cons (x,r) ->
                abs ((19 * x.Hashcons.hkey) + r.Hashcons.hkey + 1)
        end)

    let as_hash h = h.Hashcons.hkey

    let ht = M.create 101

    let nilp nh = match nh.Hashcons.node with
    | Nil -> true
    | Cons _ -> false

    let cons x r = M.hashcons ht (Cons (x,r))
    and nil = M.hashcons ht Nil

    (* Iter hashconsed list *)
    let rec iter f xs = match xs.Hashcons.node with
    | Nil -> ()
    | Cons (x,r) -> f x ; iter f r

    (* Map to list *)
    let rec map f xs = match xs.Hashcons.node with
    | Cons (x,r) -> f x :: map f r
    | Nil        -> []

    let pp pp_elt nh =
      let pps = map pp_elt nh in
      String.concat " " pps
  end

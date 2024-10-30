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

module type Config = sig
  val naturalsize : MachSize.sz option
  val fullmixed : bool
end

open MachSize
open Endian
type offset = int
type t = sz * offset

let equal (sz1,o1) (sz2,o2) =
  MachSize.equal sz1 sz2 && Misc.int_eq o1 o2

let disjoint (l1,h1) (l2,h2) =
   l1 < l2 && h1 <= l2 || l2 < l1 && h2 <= l1

let tr (sz,o) = (o,o+MachSize.nbytes sz)

let overlap a1 a2 =
  let i1 = tr a1 and i2 = tr a2 in
  not (disjoint i1 i2)

module Make(C:Config) = struct

  open Printf

  let pp_mixed = function (sz,o) -> sprintf "%s%i" (MachSize.pp_short sz) o

  let do_fold f sz xs r = List.fold_right (fun o r -> f (sz,o) r) xs r

  let get_off = match  C.naturalsize with
  | None -> fun _ -> []
  | Some sz ->
      (if C.fullmixed then MachSize.get_off else  MachSize.get_off_reduced)
        sz

  let fold_mixed f r =
    let r = do_fold f Byte (get_off Byte) r in
    let r = do_fold f Short (get_off Short) r in
    let r = do_fold f Word (get_off Word) r in
    let r = do_fold f Quad (get_off Quad) r in
    let r = do_fold f S128 (get_off S128) r in
    r

  let rec tr_value sz v = match sz with
  | Byte -> v
  | Short -> v lsl 8 + v
  | Word ->  v lsl 24 + v lsl 16 + v lsl 8 + v
  | Quad ->
      let x = tr_value Word v in
      x lsl 32 + x
  | S128 -> assert false


end


module type ValsConfig = sig
  val naturalsize : unit -> MachSize.sz
  val endian : Endian.t
end

module Vals(C:ValsConfig) = struct

  let correct_offset = match C.endian with
  | Little -> fun _ o -> o
  | Big ->
      fun sz o ->
        let nsz = C.naturalsize () in
        let bsz = nbytes sz in
        let bo = o / bsz in
        let no = bsz * ((nbytes nsz/bsz)-bo-1) in
(*            Printf.eprintf "tr: %i -> %i\n" o no ; *)
        no

  let overwrite_value v sz o w  =
    if sz = C.naturalsize () then w
    else
      let o = correct_offset sz o in
      let sz_bits =  MachSize.nbits sz in
      let nshift =  o * 8 in
      let wshifted = w lsl nshift in
      let mask = lnot (((1 lsl sz_bits) - 1) lsl nshift) in
      (v land mask) lor wshifted

  let extract_value v sz o =
    let sz_bits =  MachSize.nbits sz in
    let o = correct_offset sz o in
    let nshift =  o * 8 in
    let mask =
      match sz with
      | S128 -> assert false
      | Quad -> -1
      | _ -> (1 lsl sz_bits) - 1 in
    let r = (v lsr nshift) land mask in
(*      Printf.eprintf "EXTRACT (%s,%i)[0x%x]: 0x%x -> 0x%x\n"
        (MachSize.pp sz) o mask v r ; *)
    r

end

(** Utilities for atoms supplemented with mixed accesses *)

module Util(I:sig type at val plain : at val is_ifetch : at -> bool end) =
  struct
    let get_access_atom = function
      | None -> None
      | Some (_,m) -> m

   let set_access_atom a acc =
     Some
       (match a with
        | Some (a,_) ->
            if I.is_ifetch a then
              Warn.fatal "mixed-size and self mode are not compatible (yet)" ;
            (a,Some acc)
        | None -> (I.plain,Some acc))             

  end

module No =
  struct
    let get_access_atom _ = None
    let set_access_atom a _ = a
  end

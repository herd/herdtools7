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
end

open MachSize
open Endian
type offset = int
type t = sz * offset

module Make(C:Config) = struct

  open Printf

  let pp_mixed = function (sz,o) -> sprintf "%s%i" (MachSize.pp_short sz) o

  let do_fold f sz xs r = List.fold_right (fun o r -> f (sz,o) r) xs r

  let get_off = match  C.naturalsize with
  | None -> fun _ -> []
  | Some sz -> MachSize.get_off sz

  let fold_mixed f r =
    let r = do_fold f Byte (get_off Byte) r in
    let r = do_fold f Short (get_off Short) r in
    let r = do_fold f Word (get_off Word) r in
    let r = do_fold f Quad (get_off Quad) r in
    r

  let rec tr_value sz v = match sz with
  | Byte -> v
  | Short -> v lsl 8 + v
  | Word ->  v lsl 24 + v lsl 16 + v lsl 8 + v
  | Quad ->
      let x = tr_value Word v in
      x lsl 32 + x


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
      | Quad -> -1
      | _ -> (1 lsl sz_bits) - 1 in
    let r = (v lsr nshift) land mask in
(*      Printf.eprintf "EXTRACT (%s,%i)[0x%x]: 0x%x -> 0x%x\n"
        (MachSize.pp sz) o mask v r ; *)
    r

end
    

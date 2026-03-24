(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2023-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Name handling for miaou and back *)


let toalpha s =
  let buff = Buffer.create 10 in
  for k=0 to String.length s-1 do
    match s.[k] with
    | 'a'..'z'|'A'..'Z' as c ->
        Buffer.add_char buff c
    | _ -> ()
  done ;
  Buffer.contents buff

let vocabulary =
  StringMap.empty
  |> StringMap.add "dmb.full" "DMBFULL"
  |> StringMap.add "dmb.st" "DMBST"
  |> StringMap.add "dmb.ld" "DMBLD"
  |> StringMap.add "dsb.full" "DSBFULL"
  |> StringMap.add "dsb.st" "DSBST"
  |> StringMap.add "dsb.ld" "DSBLD"
  |> StringMap.add "iico_order" "iicoorder"
  |> StringMap.add "iico_data" "iicodata"
  |> StringMap.add "iico_ctrl" "iicoctrl"
  |> StringMap.add "iico_control" "iicoctrl"
  |> StringMap.add "hw-reqs" "hwreqs"
  |> StringMap.add "sca-class" "sca"
  |> StringMap.add "Instr-read-ordered-before" "Instrreadob"
  |> StringMap.add "L" "REL"
  |> StringMap.add "id" "sameEffect"

let to_csname s =
  try StringMap.find s vocabulary
  with Not_found -> toalpha s

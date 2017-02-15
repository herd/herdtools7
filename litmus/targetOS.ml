(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)


type t = Linux | Mac | AIX | FreeBsd


let tags = ["linux"; "mac"; "aix"; "freebsd" ]

let parse tag = match Misc.lowercase tag with
| "linux" -> Some Linux
| "freebsd" -> Some FreeBsd
| "mac"|"macos" -> Some Mac
| "aix"|"aix5" -> Some AIX
| _ -> None

let pp = function
  | Linux -> "linux"
  | Mac -> "mac"
  | AIX -> "aix"
  | FreeBsd -> "freebsd"

let is_freebsd = function
  | FreeBsd -> true
  | Linux|Mac|AIX -> false

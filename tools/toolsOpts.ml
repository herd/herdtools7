(****************************************************************************)
(*                           The Diy Toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2026-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf

let parse_hexa hexa =
  "-hexa", Arg.Bool (fun b -> hexa := b),
  sprintf "<bool> specify hexadecimal output, default %b" !hexa

let parse_int32 int32 =
  "-int32", Arg.Bool (fun b -> int32 := b),
  sprintf "<bool> integer in logs are 32 bits wide, default %b" !int32

let parse_faulttype ft =
  "-faulttype", Arg.Bool (fun b -> ft := b),
  sprintf "<bool> consider fault types, default %b" !ft

let parse_conds conds =
  "-conds",
  Arg.String (fun s -> conds := !conds @ [s]),
  "<name> specify condition files, can be repeated"

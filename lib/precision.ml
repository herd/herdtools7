(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2022-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

type t =
  | Handled      (* Do nothing special *)
  | Fatal        (* Jump to end of code *)
  | LoadsFatal   (* Only faults on loads jump to end of code, stores do nothing *)
  | Skip         (* Skip instruction *)

let default = Handled

let tags =  ["handled"; "fatal"; "loadsfatal"; "faultToNext"; ]

let parse s = match s with
  | "imprecise"|"handled"|"asynchronous"|"async" -> Some Handled
  | "precise"|"fatal"|"synchronous"|"sync" -> Some Fatal
  | "loadsfatal"|"asymmetric"|"asym" -> Some LoadsFatal
  | "faulttonext"|"skip" -> Some Skip
  | _ -> None

let pp = function
  | Handled -> "handled"
  | Fatal -> "fatal"
  | LoadsFatal -> "loadsfatal"
  | Skip -> "faulToNext"

let is_fatal = function
  | Fatal -> true
  | Handled|LoadsFatal|Skip -> false

let is_skip = function
  | Skip -> true
  | Handled|Fatal|LoadsFatal -> false


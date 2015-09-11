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

type t = Spline | Line | Polyline | Ortho | Curved | No

let tags =
  ["spline"; "true"; "line"; "false"; "polyline"; "ortho";
   "curved";"none"]

let parse tag = match String.lowercase tag with
| "spline"|"true" -> Some Spline
| "line"|"false" -> Some Line
| "polyline" -> Some Polyline
| "ortho" -> Some Ortho
| "curved" -> Some Curved
| "none" -> Some No
| _ -> None

let pp = function
  | Spline -> "spline"
  | Line -> "line"
  | Polyline -> "polyline"
  | Ortho -> "ortho"
  | Curved -> "curved"
  | No -> "none"

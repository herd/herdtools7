(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

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

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

type t = Free | Cluster | Columns

let tags = ["free"; "cluster";"columns";]

let parse tag = match String.lowercase tag with
| "free" -> Some Free
| "cluster" -> Some Cluster
| "columns" -> Some Columns
| _ -> None

let pp = function
| Free -> "free"
| Cluster -> "cluster"
| Columns -> "columns"

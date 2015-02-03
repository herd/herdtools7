(*********************************************************************)
(*                       Litmus/Diy                                  *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*                                                                   *)
(*  Copyright 2011 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)



(*******************************************)
(* Passing hints from generators to litmus *)
(*******************************************)

type t = (string * string) list

(* Dumping info *)
type out 

val none : out
val open_out : string -> out
val close_out : out -> unit
val dump : out -> string -> t -> unit

(* Reading info *)

type table
val empty : table
val read : string -> table
val get : table -> string (* test name *) -> t

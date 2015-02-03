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

(** Edge attributes in .dot files *)

type t

val empty : t

(*  label -> attribute -> value -> oldt -> newt *)
val add : string -> string -> string -> t -> t

(* label -> attribute -> t -> value, may raise Not_found *)
val find : string -> string -> t -> string
val find_all : string -> t -> (string * string) list

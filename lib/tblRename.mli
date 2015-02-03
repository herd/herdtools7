(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Map for renaming, also provides an order *)

type 'a t 

val empty : 'a t

val add_binding : 'a t -> string -> int -> 'a -> 'a t
val find_value : 'a t -> string -> 'a
val find_value_opt : 'a t -> string -> 'a option
val find_order : 'a t -> string -> int

val fold : (string -> ('a*int) -> 'b -> 'b) -> 'a t -> 'b -> 'b

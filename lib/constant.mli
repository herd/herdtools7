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

(** Constants in code *)

type v =
  | Concrete of int
  | Symbolic of string


module type S =
  sig
    val pp : bool -> v -> string (* true -> hexa *)
    val pp_v  : v -> string
    val compare : v -> v -> int

    val intToV  : int -> v 
    val nameToV  : string -> v
  end

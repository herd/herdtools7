(*********************************************************************)
(*                         Diy/Litmus                                *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*  Copyright 2011, the authors and                                  *)
(*  Institut National de Recherche en Informatique et                *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)


type com = Rf | Fr | Ws | Hat

type t =
    int list list      (* Thread grouping *)
    * (int * int) list (* Exclusion *)

val pp : t -> string

val compute : com list -> t



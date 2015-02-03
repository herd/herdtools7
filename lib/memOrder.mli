(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Luc Maranget INRIA Paris-Rocquencourt, France.                  *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

type t =
  | Acq
  | Rel
  | Acq_Rel
  | SC
  | Rlx
  | Con
val compare : t -> t -> int
val pp_mem_order : t -> string
val pp_mem_order_short : t -> string


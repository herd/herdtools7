(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*     Jacques-Pascal Deplaix, INRIA Paris-Rocquencourt, France.     *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

module type S = sig
  type arch_reg
  type t
(* Function dump *)
  val dump_fun :
    out_channel ->
    (arch_reg * CType.t) list ->
    (string * CType.t) list ->
    string list ->
    int ->
    t ->
    unit

  val dump_call :
    out_channel ->
    string ->
    (arch_reg * CType.t) list ->
    (string * CType.t) list ->
    string list ->
    int ->
    t ->
    unit

(* Inline dump *)
  val dump :
    out_channel ->
    string ->
    (arch_reg * CType.t) list ->
    (string * CType.t) list ->
    string list ->
    int ->
    t ->
    unit
end

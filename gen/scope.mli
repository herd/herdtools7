(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*       Luc Maranget INRIA Paris-Rocquencourt, France.              *)
(*                                                                   *)
(*  Copyright 2015 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

(** Scope tags *)

type t =
  | No
  | Default
  | One of BellInfo.scopes
  | Gen of (string * int * int) list
  | All

val tags : string list

val parse : string -> t option

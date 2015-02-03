(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* Abstract signature of target *)
module type S = sig
  type arch_reg
  type t

  val get_addrs : t -> string list
  val dump_out_reg : int -> arch_reg -> string
  val addr_cpy_name : string -> int -> string
  val dump_v : Constant.v -> string

end

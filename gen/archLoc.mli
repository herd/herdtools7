(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Luc Maranget INRIA Paris-Rocquencourt, France.                  *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

(* Restricted arch, with abstract location *)
module type S = sig
  type arch_reg
  include Fence.S
  type location
  val of_loc : Code.loc -> location
  val of_reg : Code.proc -> arch_reg -> location
  val location_compare : location -> location -> int
  val pp_location : location -> string
end

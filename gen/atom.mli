(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Luc Maranget INRIA Paris-Rocquencourt, France.                  *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

module type S = sig
  type atom
  val default_atom : atom
  val applies_atom : atom -> Code.dir -> bool
  val applies_atom_rmw : atom option -> bool
  val compare_atom : atom -> atom -> int
  val pp_as_a : atom option
  val pp_atom : atom -> string
  val fold_atom : (atom -> 'a -> 'a) -> 'a -> 'a
  val worth_final : atom -> bool
end

(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Luc Maranget INRIA Paris-Rocquencourt, France.                  *)
(*                                                                   *)
(*  Copyright 2015 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

(* Do nothing for mixed values, which should not appear *)
val fold_mixed : ('a -> 'b -> 'b) -> 'b -> 'b
val tr_value : 'a option -> Code.v -> Code.v
val overwrite_value : Code. v -> 'a option -> Code.v -> Code.v
val extract_value : Code. v -> 'a option -> Code.v

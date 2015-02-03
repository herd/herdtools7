(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

val fold_outcomes :
    (MiscParser.location, SymbConstant.v) ConstrGen.prop ConstrGen.constr
  -> ((MiscParser.location *  SymbConstant.v) list
      -> 'a -> 'a) -> 'a -> 'a

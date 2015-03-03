(*********************************************************************)
(*                         DIY                                       *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*                                                                   *)
(*  Copyright 2015 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

include
  (InnerRel.Make(String) :
    InnerRel.S with type elt0 = string and module Elts = MySet.Make(String))

(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

type v = Constant.v
open Constant
        
let intToV i = Concrete i
and nameToV s = Symbolic s

  let compare c1 c2 = match c1,c2 with
  | Concrete i1, Concrete i2 -> Pervasives.compare i1 i2
  | Symbolic s1,Symbolic s2 -> String.compare s1 s2
  | Concrete _,Symbolic _ -> -1
  | Symbolic _,Concrete _ -> 1

  open Printf
  let pp hexa = function
    | Concrete i -> if hexa then sprintf "0x%x" i else sprintf "%i" i
    | Symbolic s -> s

  let pp_v = pp false

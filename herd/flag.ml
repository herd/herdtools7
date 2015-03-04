(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(*                                                                   *)
(*  Copyright 2015 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

type t = Undef | Flag of string

let pp = function
  | Undef -> "*undef*"
  | Flag name -> name


type u = t

module O = 
  struct
    type t = u
    let compare = Pervasives.compare
  end

module Set = MySet.Make(O)

module Map = MyMap.Make(O)

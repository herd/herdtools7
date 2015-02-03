(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

module type Config = sig
  include Top.Config
  val family : string option
  val canonical_only : bool
  val fmt : int
  val no : string list
  val tarfile : string option
  val addnum : bool
  val numeric : bool
  val lowercase : bool
  val overload : int option
  val cpp : bool
end

module Make(Config:Config) (T:Builder.S) : sig

  type edge = T.edge
  type check = edge list list -> bool
  type info = (string * string) list

(* Compute information *)
  type mk_info = edge list -> info * T.R.Set.t
  val no_info : mk_info

(* Compute name *)
  type mk_name =  edge list -> string option
  val no_name : mk_name

(* Remains abstract: for dumper internal usage *)
  type t

(* Type of cycle generator *)
  type generator =
      ((edge list -> mk_info -> mk_name -> t -> t) -> t -> t)


(* Combine generator and dumper: so as to dump all test
   from generated cycles, check is the "last minute check"
   that operates on cycles splitted by proc *)
  val all : ?check:T.check -> generator -> unit


end

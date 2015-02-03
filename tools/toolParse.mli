(*********************************************************************)
(*                        DIY                                        *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(***************************************)
(* Apply a function (zyva) to one test *)
(***************************************)

module Top :
    functor (T:sig type t end) -> (* Return type, must be abstracted *)
      functor (B: functor(A:ArchBase.S) ->
        (sig val zyva : Name.t -> A.pseudo MiscParser.t -> T.t end)) ->
sig
  val from_file : string -> T.t
end

module Tops :
    functor (T:sig type t end) -> (* Return type, must be abstracted *)
      functor (B: functor(A:ArchBase.S) ->
        (sig val zyva : ( Name.t * A.pseudo MiscParser.t) list -> T.t end)) ->
sig
  val from_files : string list -> T.t
end

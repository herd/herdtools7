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

(** Run a test from source file *)

module type Config = sig
  val model : Model.t option
  val through : Model.through
  val skipchecks : StringSet.t
  val strictskip : bool
  val check_name : string -> bool
  val check_rename : string -> string option
  include GenParser.Config
  include Top.Config
  include Sem.Config
end

module Top :
  functor (C : Config) ->
  sig
    val from_file : string -> TestHash.env -> TestHash.env
  end

(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Warning messages for the user *)

module type Config = sig val verbose : int end

module Make : functor(O:Config) ->
  sig
(* Formatting printer  for warning,
   use it as, for instance Printf.eprintf.
   Warning is printed iff verbose mode is activated *)
val warn : ('a, unit, string, unit) format4 -> 'a
val warn1 : ('a, unit, string, unit) format4 -> 'a
  end

(* Always print the message *)
val warn_always : ('a, out_channel, unit, unit) format4 -> 'a

(* Format a message and finish with current test, by raising
   exception Exit *)
val exit : ('a, unit, string, unit) format4 -> 'a

(* To be called when some error results from
   wrong or unhandled input, such as for instance
   a jump when jumps are not implemented, or
   an illegal operation on symbolic constants.
   raises Misc.UserError (formatted message) *)
val user_error : ('a, unit, string, 'b) format4 -> 'a

(* Idem, but the user should not be blamed,
   exception Misc.Fatal *)
    val fatal :  ('a, unit, string, 'b) format4 -> 'a

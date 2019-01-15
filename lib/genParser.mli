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

(** A 'generic' parsing module for herd/litmus/tools files *)

(* Wapper (takes care of parsing exceptions *)
val call_parser :
    string -> Lexing.lexbuf -> 'a -> ('a -> Lexing.lexbuf -> 'b) -> 'b

(* Configuration, to change kind, condition and rename *)
module type Config = sig
  val debuglexer : bool
  val verbose : int
  val check_kind : string -> ConstrGen.kind option
  val check_cond : string -> string option
end

module DefaultConfig : Config

(* input signature, a lexer and a parser for a given architecture *)
module type LexParse = sig
  type token
  type instruction

  val lexer : Lexing.lexbuf -> token
  val parser :
       (Lexing.lexbuf -> token) -> Lexing.lexbuf ->
	 int list * instruction list list *
           MiscParser.extra_data
end

module type S = sig
  type pseudo
  type init = MiscParser.state
  type prog = (int * pseudo list) list
  type locations = MiscParser.LocSet.t

(*
(* Partial access for external digest computation *)
  val parse_init : Lexing.lexbuf -> init
  val parse_prog : Lexing.lexbuf -> prog
  val parse_cond : Lexing.lexbuf -> MiscParser.constr
*)
(* Main parser for memevents and litmus *)
  val parse : in_channel -> Splitter.result ->  pseudo MiscParser.t
  val parse_string : string -> Splitter.result ->  pseudo MiscParser.t
end

(* Build a generic parser *)
module Make
    (C:Config)
    (A:ArchBase.S)
    (L:LexParse with type instruction = A.parsedPseudo) :
    S with type pseudo = A.pseudo

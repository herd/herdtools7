(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(* John Wickerson, Imperial College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** A 'generic' parsing module for memevents/litmus files *)

(* Wapper (takes care of parsing exceptions *)
val call_parser :
    string -> Lexing.lexbuf -> 'a -> ('a -> Lexing.lexbuf -> 'b) -> 'b

(* Configuration, to change kind, condition and rename *)
module type Config = sig
  val debuglexer : bool
  val check_kind : string -> ConstrGen.kind option
  val check_cond : string -> string option
end

module DefaultConfig : Config

(* input signature, a lexer and a parser for a given architecture *)
module type LexParse = sig
  type token
  type pseudo

  val deep_lexer : Lexing.lexbuf -> token
  val deep_parser :
        (Lexing.lexbuf -> token) -> Lexing.lexbuf ->
	  (int * pseudo list) list * MiscParser.gpu_data option

  val shallow_lexer : Lexing.lexbuf -> token
  val shallow_parser :
        (Lexing.lexbuf -> token) -> Lexing.lexbuf ->
	  string CAst.t list
end

module type S = sig
  type pseudo
  type init = MiscParser.state
  type prog = (int * pseudo list) list
  type locations = MiscParser.LocSet.t

(* Partial access for external digest computation *)
  val parse_init : Lexing.lexbuf -> init
(* val parse_prog : Lexing.lexbuf -> prog *)
  val parse_cond : Lexing.lexbuf -> MiscParser.constr
(* Main parser for memevents and litmus *)
  val parse : in_channel -> Splitter.result ->  pseudo MiscParser.t
end

(* Build a generic parser *)
module Make
    (C:Config)
    (A:ArchBase.S)
    (L: LexParse with type pseudo = A.pseudo) :
    S with type pseudo = A.pseudo

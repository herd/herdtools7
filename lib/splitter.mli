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

(** Split litmus files into their various components *)

(* BEWARE: This interface files is common to memevents and litmus *)

(* The splitter
   - Parses the first (significant) line of litmus files,
     which includes the critical arch information and
     a bunch of names
   - Identify the four sections of litmus files:
      1. Initial state specification.
      2. Program
      3. Condition to be checked after the run.
      4. Various command and option settings [aka "configs"] whose
         semantics is totally unclear to me (LM).
   - As apparent from the signature Config, handles test renaming.
*)


(*  Result of splitter *)
type result =
  {
   (* architecture *)
    arch : Archs.t ;
   (* All names of the test grouped *)
    name : Name.t ;
   (* Additional information as (key * value) pairs *)
    info : (string * string) list ;
    (* The four sections of a litmus file *)
    locs : Pos.pos2 * Pos.pos2 * Pos.pos2  * Pos.pos2;
    (* Initial position *)
    start : Lexing.position ;
  }

module type Config = sig
  include LexUtils.Config
  val check_rename : string -> string option
end

module Default : Config

module Make : functor (O:Config) -> sig
  val split : string -> in_channel -> result
end


(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Read a Bell file *)

module type Config = sig
  val debug_lexer : bool
  val debug_model : bool
  val verbose : int
  val libfind : string -> string
  val compat : bool
  val prog : string
  val variant : string -> bool
end

module Make(O:Config) =
  struct
    open Printf

    module ParserConfig = struct
      let debug = O.debug_lexer
      let libfind =  O.libfind
    end

    module IB =
      BellInterpreter.Make
        (struct
          let debug = O.debug_model
          let verbose = O.verbose
          let libfind = O.libfind
          let compat = O.compat
          let variant = O.variant
        end)

    let parse fname =
      let module P = ParseModel.Make(ParserConfig) in
      try
        P.parse fname
      with
      | Misc.Fatal msg -> eprintf "%s: %s\n" O.prog msg ; exit 2
      | Misc.Exit ->
          eprintf "Failure of generic model parsing for bell\n" ;
          exit 2

    let read fname =
      let m = parse fname in
      IB.interpret_bell m
  end

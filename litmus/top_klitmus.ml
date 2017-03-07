(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2017-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf

module type Config = sig
(* Names *)
  val check_name : string -> bool
  val check_rename : string -> string option
  val check_kind : string -> ConstrGen.kind option
  val check_cond : string -> string option
(* Parameters *)
  val verbose : int
  val hexa : bool
  val is_out : bool
  val size : int
  val runs : int
  val avail : int option
  val stride : Stride.t
  val tarname : string
end

module Top(O:Config)(Tar:Tar.S) = struct

  module OX = struct
    let debuglexer = O.verbose > 2
    let debug = debuglexer
    include O
    include Template.DefaultConfig
    let mode = Mode.Std
    let asmcommentaslabel = false
  end

  module H = LitmusUtils.Hash(O)
  module W = Warn.Make(O)

  module Make
      (A:Arch_litmus.S)
      (Lang:Language.S
      with type arch_reg = A.Out.arch_reg
      and type t = A.Out.t)
      (L:GenParser.LexParse with type instruction = A.parsedPseudo)
      (XXXComp : XXXCompile_litmus.S with module A = A) =
    struct
      module Pseudo = LitmusUtils.Pseudo(A)
      module P = GenParser.Make(OX)(A)(L)
      module T = Test_litmus.Make(O)(A)(Pseudo)
      module Comp = Compile.Make (Compile.Default)(A)(T)(XXXComp)

      let dump source doc compiled =
        let outname = Tar.outname source in
        try
          Misc.output_protect
            (fun chan ->
              let module Out =
                Indent.Make(struct let hexa = O.hexa let out = chan end) in
              let module S = KSkel.Make(O)(Pseudo)(A)(T)(Out)(Lang) in
              S.dump doc compiled)
            outname
        with e ->
          begin try Sys.remove outname with _ -> () end ;
          raise e

      let compile hash_env fname chan splitted =
        let parsed = P.parse chan splitted in
        close_in chan ;
        let doc = splitted.Splitter.name in
        let tname = doc.Name.name in
        let hash = H.mk_hash_info fname parsed.MiscParser.info in
        let hash_ok = H.hash_ok hash_env tname hash in
        if hash_ok then begin
          let hash_env = StringMap.add tname hash hash_env in
          let base =
            try
              Filename.chop_suffix (Filename.basename fname) ".litmus"
            with _ -> assert false in
          let src = sprintf "%s.c" base in
          src,hash_env
        end else begin
          W.warn "%s, test not compiled" (Pos.str_pos0 doc.Name.file) ;
          raise Misc.Exit
        end
    end

  module SP = Splitter.Make(OX)

  let from_chan hash_env fname chan =
    let { Splitter.arch=arch ; _ } as splitted =
      SP.split fname chan in
    let tname = splitted.Splitter.name.Name.name in
    if O.check_name tname then begin
(* Then call appropriate compiler, depending upon arch *)
      let module V = SymbConstant in
      match arch with
      | `LISA ->
          let module A = LISAArch_litmus.Make(V) in
          let module LexParse = struct
            type instruction = A.parsedPseudo
            type token = LISAParser.token
            module Lexer = BellLexer.Make(OX)
            let lexer = Lexer.token
            let parser = LISAParser.main
          end in
          let module Lang = ASMLang.Make(OX)(A.I)(A.Out) in
          let module Comp = LISACompile.Make(V) in
          let module X = Make(A)(Lang)(LexParse)(Comp) in
          X.compile hash_env fname chan splitted
      | _ ->
          W.warn "%s, cannot handle arch %s" (Pos.str_pos0 fname)
            (Archs.pp arch) ;
          raise Misc.Exit
    end else raise Misc.Exit

  let from_file fname (srcs,hash_env as k) =
      try
        let src,hash_env =
          Misc.input_protect (from_chan hash_env fname) fname in
        src::srcs,hash_env
      with
      | Misc.Exit -> k
      | Misc.Fatal msg
      | Misc.UserError msg ->
          eprintf "%a %s\n%!" Pos.pp_pos0 fname msg ;
          k
      | e ->
          let msg = sprintf "exception %s"  (Printexc.to_string e) in
          eprintf "%a %s\n%!" Pos.pp_pos0 fname msg ;
          assert false

  let from_files args =
    let sources,_ = Misc.fold_argv from_file args ([],StringMap.empty)in
    List.iter (printf "%s\n") sources
end

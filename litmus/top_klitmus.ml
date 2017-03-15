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
  val rcu : Rcu.t
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

  let exit_not_compiled fname =
    W.warn "%s Test not compiled" (Pos.str_pos0 fname) ;
    raise Misc.Exit

  module Utils(A:Arch_litmus.Base)
      (Lang:Language.S
      with type arch_reg = A.Out.arch_reg
      and type t = A.Out.t
      and module RegMap = A.RegMap)
      (Pseudo:PseudoAbstract.S) =
    struct
      module T = Test_litmus.Make(O)(A)(Pseudo)

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

      let compile parse compile allocate
          fname (hash_env,id) chan splitted =
        let parsed = parse chan splitted in
        close_in chan ;
        let doc = splitted.Splitter.name in
        let tname = doc.Name.name in
        let hash = H.mk_hash_info fname parsed.MiscParser.info in
        let hash_ok = H.hash_ok hash_env tname hash in
        if hash_ok then begin
          let hash_env = StringMap.add tname hash hash_env in
          let base = sprintf "litmus%03i" id in
          let src = sprintf "%s.c" base in
          let parsed = allocate parsed in
          let compiled =  compile doc parsed in
          dump src doc compiled ;
          (base,tname),(hash_env,id+1)
        end else begin
          exit_not_compiled fname
        end
    end

  module MakeLISA =
    struct
      module V = SymbConstant
      module A = LISAArch_litmus.Make(V)
      module LexParse = struct
        type instruction = A.parsedPseudo
        type token = LISAParser.token
        module Lexer = BellLexer.Make(OX)
        let lexer = Lexer.token
        let parser = LISAParser.main
      end
      module LISAComp = LISACompile.Make(V)
      module Pseudo = LitmusUtils.Pseudo(A)
      module Lang = LISALang.Make(V)
(*      module Lang = ASMLang.Make(OX)(A.I)(A.Out) *)
      module Utils = Utils(A)(Lang)(Pseudo)
      module P = GenParser.Make(OX)(A)(LexParse)
      module T = Test_litmus.Make(O)(A)(Pseudo)

      module Alloc =
        SymbReg.Make
          (struct
            include A
            type v = V.v
            let maybevToV = V.maybevToV
            type global = string
            let maybevToGlobal = vToName
          end)

      let allocate =
        let open MiscParser in
        let have_rcu p =
          List.exists
            (fun (_,code) ->
              List.exists
                (fun p ->
                  A.pseudo_fold
                    (fun r ins -> match ins with
                    | A.Pfence
                        (A.Fence
                           ([("rcu_read_lock"|"rcu_read_unlock"|"sync")],_))
                      -> true
                    | _ -> r)
                    false p)
                code)
            p in
        fun fname parsed ->
          let p = Alloc.allocate_regs parsed in
          let ok = match O.rcu with
          | Rcu.Yes -> true
          | Rcu.No -> not (have_rcu p.MiscParser.prog)
          | Rcu.Only -> have_rcu p.MiscParser.prog in
          if ok then p
          else exit_not_compiled fname

      module Comp = Compile.Make (Compile.Default)(A)(T)(LISAComp)

      let compile fname =
        Utils.compile P.parse Comp.compile (allocate fname)
          fname
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
          MakeLISA.compile fname hash_env chan splitted
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

  let dump_makefile srcs =
    let fname = Tar.outname "Makefile" in
    Misc.output_protect
      (fun chan ->
        let module Out =
          Indent.Make(struct let hexa = O.hexa let out = chan end) in
        Out.o "ccflags-y += -std=gnu99";
        List.iter (fun (src,_) -> Out.f "obj-m += %s.o" src) srcs ;
        Out.o "" ;
        Out.o "all:" ;
        Out.o "\tmake -C /lib/modules/$(shell uname -r)/build/ M=$(PWD) modules" ;
        Out.o "" ;
        Out.o "clean:" ;
        Out.o "\tmake -C /lib/modules/$(shell uname -r)/build/ M=$(PWD) clean" ;
        ())
      fname

  let dump_run srcs =
    let fname = Tar.outname "run.sh" in
    Misc.output_protect
      (fun chan ->
        let module Out =
          Indent.Make(struct let hexa = O.hexa let out = chan end) in
        Out.o "OPT=\"$*\"" ;
        Out.o "date" ;
        Out.f "echo Compilation command: \"%s\""
          (String.concat " " (Array.to_list Sys.argv)) ;
        Out.o "echo \"OPT=$OPT\"" ;
        Out.o "echo" ;
        Out.o "" ;
        Out.o "zyva () {" ;
        Out.oi "name=$1" ;
        Out.oi "ko=$2" ;
        Out.oi "if test -f  $ko" ;
        Out.oi "then" ;
        Out.oii "insmod $ko $OPT" ;
        Out.oii "cat /proc/litmus" ;
        Out.oii "rmmod $ko" ;
        Out.oi "fi" ;
        Out.o "}" ;
        Out.o "" ;

        List.iter
          (fun (src,name) ->
            Out.f "zyva %S %s.ko" name src)
          srcs ;
        Out.o "date" ;
        ())
      fname

  let from_files args =
    let sources,_ = Misc.fold_argv from_file args ([],(StringMap.empty,0))in
    let sources = List.rev sources in
    dump_makefile sources ;
    dump_run sources ;
    ()

end

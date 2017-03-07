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


let indent = Indent.indent
and indent2 = Indent.indent2
and indent3 = Indent.indent3
and indent4 = Indent.indent4
and indent5 = Indent.indent5

module type Config = sig
  val verbose : int
  val hexa : bool
  val size : int
  val runs : int
  val avail : int option
  val stride : Stride.t
end

module Make
    (Cfg:Config)
    (P:sig type code end)
    (A:Arch_litmus.Base)
    (T:Test_litmus.S with type P.code = P.code and module A = A)
    (O:Indent.S)
    (Lang:Language.S with type arch_reg = T.A.reg and type t = A.Out.t) :
    sig
      val dump : Name.t -> T.t -> unit
    end =
  struct
(*
    module PF = DoEmitPrintf.Make
        (struct
          let emitprintf = false
          let ctr = Fmt.I64
        end)(O)
*)
    let dump_header () =
      O.o "#include <linux/module.h>\n" ;
      O.o "#include <linux/kernel.h>\n" ;
      O.o "#include <linux/init.h>\n" ;
      O.o "#include <linux/kthread.h>\n" ;
      O.o "#include <linux/ktime.h>\n" ;
      O.o "#include <linux/atomic.h>\n" ;
      O.o "#include <linux/kobject.h>\n" ;
      O.o "#include <linux/sysfs.h>\n" ;
      O.o "#include <linux/string.h>\n" ;
      O.o "#include <linux/sched.h>\n" ;
      O.o "#include <linux/wait.h>\n" ;
      O.o "#include <linux/slab.h>\n" ;
      O.o "" ;
      ()

    let dump_params test =
      O.f "static int runs = %i\n" Cfg.runs ;
      O.f "static int size = %i\n" Cfg.size ;
      O.f "static int stride = %i\n"
        (let open Stride in
        match Cfg.stride with
        | No -> 1
        | St i -> i
        | Adapt -> List.length test.T.code) ;
      ()

    let dump name test =
      dump_header () ;
      dump_params test ;
      ()


  end

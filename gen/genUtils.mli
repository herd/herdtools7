(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2018-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

val to_full :
    ('st -> 'p -> 'init -> 'n -> 'x -> 'r) ->
      'st -> 'p -> 'init -> 'n -> 'x  -> 'init * 'r * 'st

module type Config = sig
  val hexa : bool
  val variant : Variant_gen.t -> bool
end

module type Extra = sig
  val use_symbolic : bool
  type reg
  type instruction
  val mov : reg -> int -> instruction   
  val mov_mixed : MachSize.sz -> reg -> int -> instruction
  val mov_reg : reg -> reg -> instruction
  val mov_reg_mixed : MachSize.sz -> reg -> reg -> instruction
end

module Make :
functor (Cfg:Config) ->
  functor (A:Arch_gen.S) ->
    functor(Extra : Extra with
            type reg = A.reg
            and type instruction = A.pseudo) -> 
  sig

    val next_init :
        A.st ->
          Code.proc ->
            A.init ->
              string -> A.arch_reg * A.init * A.st

    val find_init : Code.proc -> A.init -> string -> A.arch_reg

    val emit_const :
        A.st -> Code.proc -> A.init -> int -> A.reg option * A.init * A.st

    val emit_nop :
        A.st -> Code.proc -> A.init -> string -> A.reg * A.init * A.st

    val emit_mov :
        A.st ->
          Code.proc ->
            A.init ->
              int ->
                A.arch_reg * A.init * Extra.instruction list * A.st

    val emit_mov_sz :
        MachSize.sz ->
          A.st ->
            Code.proc ->
              A.init ->
                int ->
                  A.arch_reg * A.init * Extra.instruction list * A.st

    val emit_mov_fresh :
        A.st ->
          Code.proc ->
            A.init ->
              int ->
                A.arch_reg * A.init * Extra.instruction list * A.st

    val emit_mov_sz_fresh :
        MachSize.sz ->
          A.st ->
            Code.proc ->
              A.init ->
                int ->
                  A.arch_reg * A.init * Extra.instruction list * A.st
  end

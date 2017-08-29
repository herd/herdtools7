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
open Code

module type Config = sig
  include CompileCommon.Config
  val realdep : bool
end

module Make(Cfg:Config) : XXXCompile_gen.S =
  struct
    let naturalsize = TypBase.get_size Cfg.typ
    module RISCV =
      RISCVArch_gen.Make
        (struct let naturalsize = naturalsize end)
    include CompileCommon.Make(Cfg)(RISCV)
  end

(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Copyright (c) 2024 Puranjay Mohan <puranjay@kernel.org>                  *)
(*                                                                          *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module Make : functor (Conf : RunTest.Config) (ModelConfig : MemWithCav12.Config) -> sig
  val run : RunTest.runfun
end

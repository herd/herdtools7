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
(* Authors:                                                                 *)
(* Hadrien Renaud, University College London, UK.                           *)
(****************************************************************************)

type 'body primitive = {
  name : string;
  args : AST.type_desc list;
  body : 'body;
  return_type : AST.type_desc option;
}
(** Primitives are special functions that have a body as an ocaml function,
    and thus are not interpreted but executed by the interpreter. *)

module type S = sig
  module B : Backend.S

  type body = B.value list -> B.value list B.m

  val run : AST.t -> body primitive list -> B.value list -> B.value list B.m
  (** [run spec_lib ast args] runs the function main of the ast, in an
      environment build from the ast and spec_lib.
      The primitives signatures will be passed by the interpreter to the type-
      checker with [D_Primitive].
  *)
end

module Make (B : Backend.S) : S with module B = B

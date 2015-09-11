(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

type pad_ty = | Left | Right | Zeros

type padding = | No_padding | Some_padding of int * pad_ty

type int_conv =  Int_i | Int_x | Int_u

type int_ty = I | I8 | I16 | I32 | I64 | CTR

type conv =
  | Char
  | String
  | Float
  | Int of padding * int_conv * int_ty

type t =
  | Conv of conv
  | Lit of string
  | Percent

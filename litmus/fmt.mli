(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

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

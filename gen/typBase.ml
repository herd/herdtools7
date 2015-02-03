(*********************************************************************)
(*                        Diy                                        *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* Base type for produced tests *)

type t =  Long | LongLong | Int | Short | Char 

let tags =  ["longlong";"long";"int";"short";"char";]

let parse s = match s with
| "longlong" -> Some LongLong
| "long" -> Some Long
| "int" -> Some Int
| "short" -> Some Short
| "char" -> Some Char
| _ -> None

let pp = function
  | LongLong -> "longlong"
  | Long -> "long"
  | Int -> "int"
  | Short -> "short"
  | Char -> "char"

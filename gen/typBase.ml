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

open MachSize

type sgn = Signed | Unsigned
type t =   Int | Std of sgn * MachSize.sz

let tags =
  ["int";
   "int8_t"; "uint8_t";
   "int16_t"; "uint16_t";
   "int32_t"; "uint32_t";
   "int64_t"; "uint64_t";
 ]

let parse s = match s with
| "int" -> Some Int
| "int8_t" -> Some (Std (Signed,Byte))
| "uint8_t" -> Some (Std (Unsigned,Byte))
| "int16_t" -> Some (Std (Signed,Short))
| "uint16_t" -> Some (Std (Unsigned,Short))
| "int32_t" -> Some (Std (Signed,Word))
| "uint32_t" -> Some (Std (Unsigned,Word))
| "int64_t" -> Some (Std (Signed,Quad))
| "uint64_t" -> Some (Std (Unsigned,Quad))
| _ -> None

let pp = function
| Int ->  "int"
| Std (Signed,Byte) ->  "int8_t"
| Std (Unsigned,Byte) ->  "uint8_t"
| Std (Signed,Short) ->  "int16_t"
| Std (Unsigned,Short) ->  "uint16_t"
| Std (Signed,Word) ->  "int32_t"
| Std (Unsigned,Word) ->  "uint32_t"
| Std (Signed,Quad) ->  "int64_t"
| Std (Unsigned,Quad) ->  "uint64_t"


let default = Int

let is_default = function
  | Int -> true
  | _ -> false

let get_size = function
  | Int -> Word
  | Std (_,sz) -> sz


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

type sz = Byte | Short | Word | Quad | QuadWord

let pp = function
  | Byte -> "byte"
  | Short -> "short"
  | Word -> "word"
  | Quad -> "quad"
  | QuadWord -> "quadword"


let pp_short = function
  | Byte -> "b"
  | Short -> "h"
  | Word -> "w"
  | Quad -> "q"
  | QuadWord -> "x"

let debug = function
  | Byte -> "Byte"
  | Short -> "Short"
  | Word -> "Word"
  | Quad -> "Quad"
  | QuadWord -> "QuadWord"

let nbytes = function
  | Byte -> 1
  | Short -> 2
  | Word -> 4
  | Quad -> 8
  | QuadWord -> 16

let nbits sz = nbytes sz * 8

(* check is 16 bit immediate *)
let is_imm16 n = n >= 0 && n < 65535

(* Correct endianess *)
let swap16 x =
  let r = (x land 0xff) lsl 8 in
  let r = r lor (x land 0xff00) lsr 8 in
  r

(* k is total size (in bits) *)
let rec swap k x =
   if k <= 16 then swap16 x
   else
     let k2 = k / 2 in
     let mask = (1 lsl k2) - 1 in
     let r1 = swap k2 (x land mask)
     and r2 = swap k2 (x lsr k2) in
     (r1 lsl k2) lor r2


let tr_endian sz = match sz with
| Byte -> fun x -> x
| Short -> swap16
| Word|Quad|QuadWord -> swap (nbits sz)


let l0 = [0;]
let l01 = [0;1;]
let l02 = [0;2;]
let l04 = [0;4;]
let l0123 = [0;1;2;3;]
let l0246 = [0;2;4;6;]
let l01234567 = [0;1;2;3;4;5;6;7;]
let l0to15 = [0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15]

let off_byte = function
  | Byte -> l0
  | Short|Word|Quad|QuadWord -> []

let off_short = function
  | Byte -> l01
  | Short -> l0
  | Word|Quad|QuadWord -> []

let off_word = function
  | Byte -> l0123
  | Short -> l02
  | Word -> l0
  | Quad | QuadWord -> []

let off_quad = function
  | Byte -> l01234567
  | Short -> l0246
  | Word ->  l04
  | Quad -> l0
  | QuadWord -> []

let off_quadword = function
  | Byte     -> l0to15
  | Short    -> l01234567
  | Word     -> l0246
  | Quad     -> l02
  | QuadWord -> l0

let get_off sz = match sz with
| Byte -> off_byte
| Short -> off_short
| Word -> off_word
| Quad -> off_quad
| QuadWord -> off_quadword

let get_off_reduced sz = match sz with
| Byte -> off_byte
| Short -> off_short
| Word ->
    begin fun sz -> match sz with
    | Byte -> []
    | _ -> off_word sz
    end
| Quad ->
    begin fun sz -> match sz with
    | Byte|Short -> []
    | _ -> off_quad sz
    end
| QuadWord ->
    begin fun sz -> match sz with
    | Byte | Short | Word -> []
    | _ -> off_quadword sz
    end

let compare sz1 sz2 = match sz1,sz2 with
| (Byte,(Short|Word|Quad|QuadWord))
| (Short,(Word|Quad|QuadWord))
| (Word,(Quad|QuadWord))
| (Quad, QuadWord)
  -> -1
| (Byte,Byte)
| (Short,Short)
| (Word,Word)
| (Quad,Quad)
| (QuadWord, QuadWord)
  -> 0
| ((Short|Word|Quad|QuadWord),Byte)
| ((Word|Quad|QuadWord),Short)
| ((Quad|QuadWord),Word)
| (QuadWord, _)
    -> 1

module Set =
  MySet.Make
    (struct
      type t = sz
      let compare = compare
    end)

let min sz1 sz2 = if compare sz1 sz2 <= 0 then sz1 else sz2

let pred = function
  | Byte|Short -> Byte
  | Word -> Short
  | Quad -> Word
  | QuadWord -> Quad

module Tag = struct

  type t = Auto | Size of sz

  let tags = ["auto";"byte";"short";"word";"quad";]

  let parse tag = match Misc.lowercase tag with
  | "byte" -> Some (Size Byte)
  | "short" -> Some (Size Short)
  | "word" -> Some (Size Word)
  | "quad" -> Some (Size Quad)
  | "quadword" -> Some (Size QuadWord)
  | "auto" -> Some Auto
  | _      -> None

  let pp = function
    | Size sz -> pp sz
    | Auto -> "auto"

end

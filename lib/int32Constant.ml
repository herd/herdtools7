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

module Int32Scalar = struct
  include Int32

  let shift_right_arithmetic = Int32.shift_right

  let addk x k = match k with
  | 0 -> x
  | 1 -> succ x
  | _ -> add x (of_int k)

  let machsize = MachSize.Word
  let pp hexa v =
    Printf.sprintf (if hexa then "0x%lx" else "%li") v
  let lt v1 v2 = compare v1 v2 < 0
  let le v1 v2 = compare v1 v2 <= 0

  let mask sz =
    let open MachSize in
    match sz with
    | Byte -> fun v -> logand v 0xffl
    | Short -> fun v -> logand v 0xffffl
    | Word -> fun v -> v
    | Quad -> Warn.fatal "make 32 value with quad mask"
end

include SymbConstant.Make(Int32Scalar)

